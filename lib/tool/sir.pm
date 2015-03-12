package tool::sir;

use include_modules;
use strict;
use File::Copy 'cp';
use data;
use log;
use OSspecific;
use tool::modelfit;
use Math::Random;
use Moose;
use MooseX::Params::Validate;
use ext::Math::MatrixReal;# qw(all); 
use Math::Trig;	# For pi
use Math::Random;
use output;
use array qw(:all);
use linear_algebra;
use utils::file;

extends 'tool';

has 'sir_raw_results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['sirlog.csv'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'sir_results.csv' );

has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'recompute' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'with_replacement' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'samples' => ( is => 'rw', required => 1, isa => 'Int' );
has 'resamples' => ( is => 'rw', required => 1, isa => 'Int' );
has 'covmat_input' => ( is => 'rw', isa => 'Str' );
has 'rawres_input' => ( is => 'rw', isa => 'Str' );
has 'offset_rawres' => ( is => 'rw', isa => 'Int', default => 1 );
has 'in_filter' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'inflation' => ( is => 'rw', isa => 'Num', default => 1 );
has 'mceta' => ( is => 'rw', isa => 'Int', default => 0 );

has 'original_ofv' => ( is => 'rw', isa => 'Num');
has 'pdf_vector' => ( is => 'rw', isa => 'ArrayRef' );


sub BUILD
{
	my $self  = shift;

	for my $accessor ('logfile','raw_results_file','raw_nonp_file'){
		my @new_files=();
		my @old_files = @{$self->$accessor};
		for (my $i=0; $i < scalar(@old_files); $i++){
			my $name;
			my $ldir;
			( $ldir, $name ) =
				OSspecific::absolute_path( $self ->directory(), $old_files[$i] );
			push(@new_files,$ldir.$name) ;
		}
		$self->$accessor(\@new_files);
	}	

	croak("No \$PROBLEM in input model") unless 
		(defined $self ->models()->[0]->problems and scalar(@{$self ->models()->[0]->problems})>0);

	croak("No \$INPUT found") unless 
		(defined $self ->models()->[0]->problems->[0]->inputs and 
		 scalar(@{$self ->models()->[0]->problems->[0]->inputs})>0);
	croak("No \$DATA found") unless 
		(defined $self ->models()->[0]->problems->[0]->datas and 
		 scalar(@{$self ->models()->[0]->problems->[0]->datas})>0);

	#TODO support multiple $PROB
	croak("sir does not yet support more than 1 \$PROB in model ") if (scalar(@{$self ->models()->[0]->problems})>1);


	croak("Number of samples must be larger than 0") unless ($self->samples()>0);
	croak("Number of resamples must be larger than 1") unless ($self->resamples()>1);
	croak("Number of resamples cannot be larger than samples unless with_replacement is set") unless 
		(($self->resamples() <= $self->samples) or ($self->with_replacement));
	

}

sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	my $model = $self ->models() -> [$model_number-1];

	# ------------------------  Run original run if not already done  -------------------------------

	my $check_output_and_model_match=0;
	if ( $model -> is_run ) {
		#check that input model and provided lst-file with control stream copy match
		$check_output_and_model_match=1 unless (defined $self->rawres_input);
	}else{
		my %subargs = ();
		if ( defined $self -> subtool_arguments() ) {
			%subargs = %{$self -> subtool_arguments()};
		}

		if( $self -> nonparametric_etas() or
			$self -> nonparametric_marginals() ) {
			$model -> add_nonparametric_code;
		}
		my @models=();
		my $message = "Running input model";

		my $orig_fit = tool::modelfit ->
			new( %{common_options::restore_options(@common_options::tool_options)},
				 base_directory	 => $self ->directory(),
				 directory		 => $self ->directory().
				 '/orig_modelfit_dir'.$model_number,
				 models		 => [$model],
				 threads               => $self->threads,
				 nm_output => 'ext,cov,coi,cor,phi',
				 logfile	         => undef,
				 raw_results           => undef,
				 prepared_models       => undef,
				 copy_data             => $self->copy_data,
				 top_tool              => 0,
				 %subargs );

		ui -> print( category => 'sir',
			message => $message );

		$orig_fit -> run;

	}

	my $output = $model -> outputs -> [0];
	unless (defined $output){
		croak("No output object from input model");
	}

	my $original_ofv = $output->get_single_value(attribute => 'ofv');
	if (defined $original_ofv){
		$self->original_ofv($original_ofv);
	}else{
		croak("No ofv from input model result files");
	}

	my $parameter_hash = output::get_nonmem_parameters(output => $output);

	if ($check_output_and_model_match ){
		#check that the same set of estimated parameter with the same labels
		#in both input model and existing output file

		my $mod_coords = $model->problems->[0]->get_estimated_attributes(parameter=> 'all',
																		 attribute => 'coordinate_strings');
		my $mismatch=0;
		if (scalar(@{$mod_coords})==scalar(@{$parameter_hash->{'coordinate_strings'}})){
			for (my $i=0; $i< scalar(@{$mod_coords}); $i++){
				unless ($mod_coords->[$i] eq $parameter_hash->{'coordinate_strings'}->[$i]){
					$mismatch = 1;
					last;
				}
			}
		}else{
			$mismatch=1;
		}

		if ($mismatch){
			my $message = "\nsir input error:\n".
				"The input model and the control stream copy and the top of the lst-file do not match\n".
				"Estimated parameters in the input model are\n".join(' ',@{$mod_coords})."\n".
				"Estimated parameters in the lst-file model are\n".
				join(' ',@{$parameter_hash->{'coordinate_strings'}})."\n";
			croak($message);
		}

	}

	my $covmatrix;
	my @covmat_column_headers = ();

	if (defined $self->covmat_input){
		#read user matrix
		#use keep_labels_hash from input model problem
		my %keep_labels_hash;
		foreach my $coord (@{$output->problems->[0]->input_problem->get_estimated_attributes(attribute=>'coordinate_strings')}){
			$keep_labels_hash{$coord}=1;
		}

		my @lines = utils::file::slurp_file($self->covmat_input);
		my ($success,$lower_covar,$index_order_ref,$header_labels_ref) = 
			output::problem::subproblem::parse_additional_table (covariance_step_run => 1,
																 have_omegas => 1,
																 have_sigmas => 1,
																 method_string => ' ',
																 type => 'cov',
																 keep_labels_hash => \%keep_labels_hash,
																 tableref => \@lines);
		@lines=undef;
		unless ($success){
			croak("failed to parse covmat_input file ".$self->covmat_input);
		}

		#error check that column headers match parameters in nonmem output
		unless ( scalar(@{$index_order_ref}) == scalar(@{$parameter_hash->{'param'}})){
			croak("Number of parameters ".scalar(@{$index_order_ref})." in covmat_input does not match number ".
				  scalar(@{$parameter_hash->{'param'}})." of estimated parameters in input model." );
		}
		foreach my $ind (@{$index_order_ref}) {
			push (@covmat_column_headers, $header_labels_ref->[$ind]);
		}

		for (my $j=0; $j < scalar(@covmat_column_headers); $j++){
			my $covheader = $covmat_column_headers[$j];
			my $outheader;
			my $par = uc($parameter_hash->{'param'}->[$j]);
			if ($par eq 'THETA'){
				$outheader = $par.$parameter_hash->{'coords'}->[$j];
			}else{
				$outheader = $par.'('.$parameter_hash->{'coords'}->[$j].')';
			}
			unless ($covheader eq $outheader){
				croak("headers $covheader from covmat_input and $outheader from input model does not match\n");
			}
		}

		$covmatrix = make_square($lower_covar);

	}elsif (defined $self->rawres_input){
		#do not need any matrices at all, will set pdfvec to ones
		1;
	}else{
		$covmatrix = get_nonmem_covmatrix(output => $output);
	}


	my $mu_values = $parameter_hash->{'values'};
	my $mat = new Math::MatrixReal(1,1);
	my $muvector = $mat->new_from_rows( [$mu_values] );
	my $sampled_params_arr;
	my $href;
	my $user_labels;
	if (defined $self->rawres_input){
		$user_labels = 1; #we do not have generic labels in rawresults
		($sampled_params_arr,$href) = model::get_rawres_params(filename => $self->rawres_input,
															   filter => $self->in_filter,
															   offset => $self->offset_rawres,
															   model => $model);
		my @arr= (1) x scalar(@{$sampled_params_arr});
		$self->pdf_vector(\@arr);
	}else{
		ui -> print( category => 'sir',
					 message => "Sampling from the truncated multivariate normal distribution");
		
		$user_labels = 0; #using generic labels is safer
		my $vectorsamples = sample_multivariate_normal(samples=>$self->samples,
													   covmatrix => $covmatrix,
													   inflation => $self->inflation,
													   lower_bound => $parameter_hash->{'lower_bounds'},
													   upper_bound => $parameter_hash->{'upper_bounds'},
													   param => $parameter_hash->{'param'},
													   coords => $parameter_hash->{'coords'},
													   block_number => $parameter_hash->{'block_number'},
													   mu => $muvector);
		
		$sampled_params_arr = create_sampled_params_arr(samples_array => $vectorsamples,
														labels_hash => $parameter_hash,
														user_labels => $user_labels);
		#TODO document relative pdf
		my $relative = 1;
		$self->pdf_vector(linear_algebra::mvnpdf_cholesky($covmatrix,$mu_values,$vectorsamples,$self->inflation,$relative));

	}
	

	ui -> print( category => 'sir',
				 message => "Creating parameter vector evaluation models...");
	my $modelsarr = model::create_maxeval_zero_models_array(
		sampled_params_arr => $sampled_params_arr,
		mceta => $self->mceta(),
		ignore_missing_parameters => 1,
		basedirectory => $self->directory,
		subdirectory => $self->directory().'m'.$model_number.'/',
		model => $model,
		purpose => 'sir',
		match_labels => $user_labels
		);
	
	$self -> prepared_models -> [$model_number-1]{'own'} = $modelsarr;

	my @subtools = ();
	@subtools = @{$self -> subtools()} if (defined $self->subtools());
	shift( @subtools );
	my %subargs = ();
	if ( defined $self -> subtool_arguments() ) {
		%subargs = %{$self -> subtool_arguments()};
	}

	$self->tools([]) unless (defined $self->tools());

	push( @{$self -> tools()},
		tool::modelfit ->
		new( %{common_options::restore_options(@common_options::tool_options)},
			 models		 => $modelsarr,
			 directory             => undef,
			 _raw_results_callback => $self ->
			 _modelfit_raw_results_callback( model_number => $model_number ),
			 subtools              => \@subtools,
			 nmtran_skip_model => 2,
			 raw_results           => undef,
			 prepared_models       => undef,
			 top_tool              => 0,
			 copy_data             => $self->copy_data,
			 %subargs ) );
	
	trace(tool => 'sir', message => "Created a modelfit object to run all the models in ".
							$self ->directory().'m'.$model_number, level => 1);
}



sub mvnpdf{
	my %parm = validated_hash(\@_,
							  inverse_covmatrix => { isa => 'Math::MatrixReal', optional => 0 },
							  mu => { isa => 'Math::MatrixReal', optional => 0 },
							  xvec_array => { isa => 'ArrayRef[ArrayRef]', optional => 0 },
							  inflation => { isa => 'Num', optional => 0 },
							  relative => { isa => 'Bool', optional => 1, default => 1 }
	);
	my $inverse_covmatrix = $parm{'inverse_covmatrix'};
	my $mu = $parm{'mu'};
	my $xvec_array = $parm{'xvec_array'};
	my $inflation = $parm{'inflation'};
	my $relative = $parm{'relative'};

	my ($rows,$columns) = $inverse_covmatrix->dim();
	unless (($rows == $columns) and ($rows > 0)){
		croak("Input error mvnpdf: Inverse covmatrix must be square and dimension > 0, but we have rows=$rows and columns=$columns");
	}
	my $k=$rows;
	($rows,$columns) = $mu->dim();
	unless ($rows == 1 and $columns == $k){
		croak("Input error mvnpdf: mu vector should have dimension (1,$k) but has dimension ($rows,$columns)");
	}
	unless (scalar(@{$xvec_array})>0){
		croak("Input error mvnpdf: xvec_array is empty");
	}
	unless ($inflation>0){
		croak("Input error mvnpdf: inflation is not > 0");
	}

	my $det_factor = get_determinant_factor(inverse_covmatrix => $inverse_covmatrix,
											k => $k,
											inflation => $inflation		);

	my @pdf_array=();
	my $delta_right = $mu->shadow(); #zeros matrix same size as $mu
	my $delta_left = $mu->shadow(); #zeros matrix same size as $mu

	foreach my $xvec (@{$xvec_array}){
		unless (scalar(@{$xvec}) == $k){
			croak("Input error mvnpdf: xvec should have dimension $k but has dimension ".scalar(@{$xvec}));
		}
		for (my $i=0; $i< $k; $i++){
			$delta_right->assign(1,($i+1),($xvec->[$i] - $mu->element(1,($i+1))));
			$delta_left->assign(1,($i+1),($xvec->[$i] - $mu->element(1,($i+1)))*(1/$inflation));
		}
		#now $delta is $xvec - $mu
		my $product_left = $delta_left->multiply($inverse_covmatrix);
		my $product=$product_left->multiply(~$delta_right); # ~ is transpose
		if ($relative){
			push(@pdf_array,exp(-0.5 * $product->element(1,1))); #we want relative the mu here, so divide by determinant factori which vanishes
		}else{
			push(@pdf_array,$det_factor*exp(-0.5 * $product->element(1,1))); #matlab absolute mvnpdf possibly change back
		}
	}

	return \@pdf_array;
}


sub compute_weights{

	#weight is same as importance_ratio
	my %parm = validated_hash(\@_,
		pdf_array => { isa => 'ArrayRef', optional => 0 },
		dofv_array => { isa => 'ArrayRef', optional => 0 }
	);
	my $pdf_array = $parm{'pdf_array'};
	my $dofv_array = $parm{'dofv_array'};

	my $len = scalar(@{$pdf_array});
	my $bignum=1e20;
	unless ($len > 0){
		croak("pdf array is empty into compute_weights");
	}
	unless ($len == scalar(@{$dofv_array})){
		croak("compute_weights: pdf_array is length $len but dofv_array is length ".scalar(@{$dofv_array}));
	}

	my %hash;
	$hash{'weights'}=[];
	$hash{'cdf'}=[];
	my $cumsum=0;
	for (my $i=0; $i< $len; $i++){
		#we check in mvnpdf that pdf is not zero??
		my $wgt;
		if (not defined $dofv_array->[$i]){
			$wgt = 0;
		}elsif ($pdf_array->[$i] > 0){
			$wgt=exp(-0.5*($dofv_array->[$i]))/($pdf_array->[$i]);
		}else{
			$wgt=$bignum;
		}
		push(@{$hash{'weights'}},$wgt);
		$cumsum = $cumsum+$wgt;
		push(@{$hash{'cdf'}},$cumsum);
	}
	$hash{'sum_weights'}=$cumsum;
	croak("total weights is 0 in compute_weights") unless ($hash{'sum_weights'}>0);
	return \%hash;
}

sub recompute_weights{
	my %parm = validated_hash(\@_,
							  weight_hash => { isa => 'HashRef', optional => 0 },
							  reset_index => { isa => 'Int', optional => 0 }
		);
	my $weight_hash = $parm{'weight_hash'};
	my $reset_index = $parm{'reset_index'};
	
	my $len = scalar(@{$weight_hash->{'weights'}});
	if ($reset_index < 0 or $reset_index >= $len){
		croak("illlegal input to recompute_weights, reset_index is $reset_index but length of weights is $len");
	} 
	$weight_hash->{'weights'}->[$reset_index] = 0;
	my $cumsum=0;
	$cumsum = $weight_hash->{'cdf'}->[$reset_index-1] if ($reset_index > 0);
	#loop over rest and recompute cdf
	for (my $i=$reset_index; $i< $len; $i++){
		$cumsum = $cumsum+$weight_hash->{'weights'}->[$i]; #first round this wgt is 0
		$weight_hash->{'cdf'}->[$i] = $cumsum;
	}
}

sub weighted_sample{
	my %parm = validated_hash(\@_,
							  cdf => { isa => 'ArrayRef', optional => 0 }
		);
	my $cdf = $parm{'cdf'};
	my $len = scalar(@{$cdf});
	croak("empty cdf into weighted_sample") unless ($len>0);
	my $max = $cdf->[$len-1];
	my $val = Math::Random::random_uniform(1,0,$max); # n low high

	for (my $i=0; $i< $len; $i++){
		return $i if ($val <= $cdf->[$i]);
	}

}

sub empirical_statistics{
	my %parm = validated_hash(\@_,
							  sampled_params_arr => { isa => 'ArrayRef', optional => 0 },
							  labels_hash => { isa => 'HashRef', optional => 0 }
		);
	my $sampled_params_arr = $parm{'sampled_params_arr'};
	my $labels_hash = $parm{'labels_hash'};

	my $len = scalar(@{$sampled_params_arr});
	croak("empty set of samples to empirical_statistics") unless ($len >0);
	my $dim = scalar(@{$labels_hash->{'labels'}});
	croak("empty set of labels to empirical_statistics") unless ($dim >0);
	croak("column resamples is missing from sampled_params_arr in empirical_statistics") unless (defined $sampled_params_arr->[0]->{'resamples'});

	my @all_labels=();
	my @all_params=();
	my $n_resamples=0;

	for (my $i=0; $i<$dim; $i++){
		#order is theta omega sigma
		push(@all_labels,$labels_hash->{'labels'}->[$i]);
		push(@all_params,$labels_hash->{'param'}->[$i]);
	}


	#create samples matrix
	my @Amatrix=();
	my @parameter_vectors=();
	my @sums=(0) x $dim;
	for (my $j=0; $j< $dim; $j++){
		push(@parameter_vectors,[]);
	}

	for (my $i=0; $i< $len; $i++){
		my @vector=();
		for (my $j=0; $j<$dim; $j++){
			push(@vector,$sampled_params_arr->[$i]->{$all_params[$j]}->{$all_labels[$j]});
		}

		for (my $k=0; $k<$sampled_params_arr->[$i]->{'resamples'} ; $k++){
			$n_resamples++;
			push(@Amatrix,\@vector);
			for (my $j=0; $j< $dim; $j++){
				push(@{$parameter_vectors[$j]},$vector[$j]);
				$sums[$j] = $sums[$j] + $vector[$j];
			}
		}
	}
	croak("Number of resamples is 0 in empirical_statistics") if ($n_resamples < 1);

	my %resulthash;
	$resulthash{'covar'}=[];
	my $err1 = linear_algebra::row_cov(\@Amatrix,$resulthash{'covar'});
	if ($err1 == 1){
		croak ("numerical error in linear_algebra rowcov");
	}elsif ($err1 == 2){
		croak ("input error to linear_algebra rowcov");
	}

	my @pred_int = sort {$a <=> $b} 0,40,80,90,95;
	my @temp =();
	foreach my $pi (@pred_int){
		if ($pi == 0){
			push (@temp,50);
		}else {
			push (@temp,(100-$pi)/2);
			push (@temp,(100-(100-$pi)/2));
		}
	}
	my @perc_limit = sort {$a <=> $b} @temp;
	my @probs;
	foreach my $lim (@perc_limit){
		push(@probs,$lim/100);
	}

	my $no_perc_limits = scalar(@perc_limit);

	$resulthash{'mean'}=[];
	$resulthash{'percentiles_labels'}=\@perc_limit;
	$resulthash{'percentiles_values'}=[];
 	for (my $j=0; $j< $dim; $j++){
		push(@{$resulthash{'mean'}},($sums[$j]/$n_resamples));
	}
 	for (my $j=0; $j< scalar(@probs); $j++){
		push(@{$resulthash{'percentiles_values'}},[(0) x $dim]);
	}

 	for (my $j=0; $j< $dim; $j++){
		my @sorted = (sort {$a <=> $b} @{$parameter_vectors[$j]}); #sort ascending
		my $quantref = quantile(probs => \@probs, numbers=> \@sorted);
		for (my $k=0; $k< scalar(@probs); $k++){
			$resulthash{'percentiles_values'}->[$k]->[$j] = $quantref->[$k];
		}
	}

	return \%resulthash;
}


sub median
{
	my %parm = validated_hash(\@_,
							  sorted_array => { isa => 'Ref', optional => 1 }
		);
	my $sorted_array = $parm{'sorted_array'};
	my $result;

	my $len = scalar( @{$sorted_array} );
	
	if( $len  % 2 ){
		$result = $sorted_array->[($len-1)/2];
	} else {
		$result = ($sorted_array->[$len/2]+$sorted_array->[($len-2)/2])/ 2;
	}

	return $result;
}

sub round
{
	my %parm = validated_hash(\@_,
							  number => { isa => 'Num', optional => 0 }
		);
	my $number = $parm{'number'};
	my $integer_out;
	
	my $floor=int($number);
	my $rem=$number-$floor;
	if ($rem >= 0){
		$integer_out = ($rem >= 0.5)? $floor+1 : $floor;
	} else {
		$integer_out = (abs($rem) >= 0.5)? $floor-1 : $floor;
	}

	return $integer_out;
}

sub get_determinant_factor
{
	my %parm = validated_hash(\@_,
							  inverse_covmatrix => { isa => 'Math::MatrixReal', optional => 0 },
							  k => { isa => 'Int', optional => 0 },
							  inflation => { isa => 'Num', optional => 1, default => 1 },
	);
	my $inverse_covmatrix = $parm{'inverse_covmatrix'};
	my $k = $parm{'k'};
	my $inflation = $parm{'inflation'};

	unless ($inflation > 0){
		croak("Inflation less than 0 in get_determinant_factor")
	}

	my $invdeterminant = $inverse_covmatrix->det();

	unless (defined $invdeterminant and $invdeterminant > 0){
		print "\nInverse covmatrix:\n";
		print $inverse_covmatrix;
		croak("\nFailed to compute determinant of inverse covariance matrix");
	}
	#p90 Mathematics Handbook
	# det(AB)=det(A)det(B) det I = 1,  det(xI)=x^n*det(I)=x^n
	# derivation: det((A*xI)^(-1))=det((xI)^(-1)A^(-1))=det((xI)^(-1))*det(A^(-1))=(1/x)^n*det(A^(-1))
	# k is n
	$invdeterminant = $invdeterminant * ((1/$inflation)**($k));

	return sqrt($invdeterminant)/((2* pi)**($k/2));

}

sub make_square {
	#copied from output::problem::subproblem local subroutine inside another sub
	my $m_ref = shift;
	my @matrix = @{$m_ref};
	# Make the matrix square:
	my $elements = scalar @matrix; # = M*(M+1)/2
	my $M = -0.5 + sqrt( 0.25 + 2 * $elements );
	my @square;
	for ( my $m = 1; $m <= $M; $m++ ) {
	    for ( my $n = 1; $n <= $m; $n++ ) {
			push( @{$square[$m-1]}, $matrix[($m-1)*$m/2 + $n - 1] );
			unless ( $m == $n ) {
				push( @{$square[$n-1]}, $matrix[($m-1)*$m/2 + $n - 1] );
			}
	    }
	}
	return \@square;
}


sub sample_multivariate_normal
{
	my %parm = validated_hash(\@_,
							  samples => { isa => 'Int', optional => 0 },
							  covmatrix => { isa => 'ArrayRef[ArrayRef]', optional => 0 },
							  lower_bound => { isa => 'ArrayRef', optional => 0 },
							  upper_bound => { isa => 'ArrayRef', optional => 0 },
							  param => { isa => 'ArrayRef', optional => 0 },
							  block_number => { isa => 'ArrayRef', optional => 0 },
							  coords => { isa => 'ArrayRef', optional => 0 },
							  mu => { isa => 'Math::MatrixReal', optional => 0 },							  
							  inflation => { isa => 'Num', optional => 0 }							  
		);
	my $samples = $parm{'samples'};
	my $covmatrix = $parm{'covmatrix'};
	my $lower_bound = $parm{'lower_bound'};
	my $upper_bound = $parm{'upper_bound'};
	my $param = $parm{'param'};
	my $block_number = $parm{'block_number'};
	my $coords = $parm{'coords'};
	my $mu = $parm{'mu'};
	my $inflation = $parm{'inflation'};
	
	my ($rows,$columns) = $mu->dim();
	unless ($rows == 1 and $columns > 0){
		croak("Input error sample_multivariate_normal: mu vector has dimension ($rows,$columns)");
	}
	my $dim = $columns;
	unless (scalar(@{$covmatrix})==$dim){
		croak("Input error sample_multivariate_normal: mu vector has dimension $dim but covmatrix has dimension ".scalar(@{$covmatrix}));
	}
	unless (scalar(@{$covmatrix->[0]})==$dim){
		croak("Input error sample_multivariate_normal: covmatrix is not square ");
	}
	unless (scalar(@{$lower_bound})==$dim){
		croak("Input error sample_multivariate_normal: mu vector has dimension $dim but lower_bound has dimension ".scalar(@{$lower_bound}));
	}
	unless (scalar(@{$upper_bound})==$dim){
		croak("Input error sample_multivariate_normal: mu vector has dimension $dim but upper_bound has dimension ".scalar(@{$upper_bound}));
	}
	unless (scalar(@{$block_number})==$dim){
		croak("Input error sample_multivariate_normal: mu vector has dimension $dim but block_number has dimension ".scalar(@{$block_number}));
	}
	unless (scalar(@{$param})==$dim){
		croak("Input error sample_multivariate_normal: mu vector has dimension $dim but param has dimension ".scalar(@{$param}));
	}
	unless (scalar(@{$coords})==$dim){
		croak("Input error sample_multivariate_normal: mu vector has dimension $dim but coords has dimension ".scalar(@{$coords}));
	}
	unless (scalar($samples)>0){
		croak("Input error sample_multivariate_normal: samples must be larger than 0");
	}
	my @muvec=();
	for (my $i=1; $i<= $dim; $i++){
		push(@muvec,$mu->element(1,$i));
	}
	
	my $use_covmatrix;

	if ($inflation != 1){
		$use_covmatrix = inflate_covmatrix(matrix => $covmatrix,
										   inflation => $inflation);
	}else{
		$use_covmatrix = $covmatrix;
	}


	my $check_sigma_posdef=0;
	my $check_omega_posdef=0;
	my @sigma_hashes=();
	my @omega_hashes=();
	my @row_index=();
	my @col_index=();
	for (my $i=0; $i< $dim; $i++){
		if ($param->[$i] eq 'omega' or $param->[$i] eq 'sigma'){
			my ($row,$col) = split(',',$coords->[$i]);
			push(@row_index,($row-1));
			push(@col_index,($col-1));
			unless ($row == $col){
				$check_sigma_posdef=1 if ($param->[$i] eq 'sigma');
				$check_omega_posdef=1 if ($param->[$i] eq 'omega');
			}
		}else{
			push(@row_index,-1);
			push(@col_index,-1);
		} 
	}
	#handle different blocks
	#if block_number changes AND the new number is nonzero then we have the 0,0 index of a new block
	#loop here to reset indices so that start at 0 for each block
	if ($check_sigma_posdef){
		my $prev_block_number=0;
		my $offset=0;
		my $index=0;
		for (my $i=0; $i< $dim; $i++){
			if ($param->[$i] eq 'sigma'){
				if ($block_number->[$i] > 0){
					if ($block_number->[$i] == $prev_block_number){
						#add to existing block, keep same offset
						if (($row_index[$i]-$offset+1) > $sigma_hashes[$index]->{'size'}){
							$sigma_hashes[$index]->{'size'} = ($row_index[$i]-$offset+1);
						}
						if ($row_index[$i] != $col_index[$i]){
							$sigma_hashes[$index]->{'offdiag'}=1;
						}
					}else{
						push(@sigma_hashes,{});
						$index = scalar(@sigma_hashes)-1;
						#new block, change offset
						$offset = $row_index[$i];
						$sigma_hashes[$index]->{'block_number'}=$block_number->[$i];
						$sigma_hashes[$index]->{'offset'}=$offset;
						$sigma_hashes[$index]->{'offdiag'}=0;
						$sigma_hashes[$index]->{'size'}=1;
					}
					$row_index[$i] = $row_index[$i] -$offset;
					$col_index[$i] = $col_index[$i] -$offset;
				}else{
					$offset=0; #unnecessary?
				}
				$prev_block_number=$block_number->[$i];
			}
		}
	}
	if ($check_omega_posdef){
		my $prev_block_number=0;
		my $offset=0;
		my $index=0;
		for (my $i=0; $i< $dim; $i++){
			if ($param->[$i] eq 'omega'){
				if ($block_number->[$i] > 0){
					if ($block_number->[$i] == $prev_block_number){
						#add to existing block, keep same offset
						if (($row_index[$i]-$offset+1) > $omega_hashes[$index]->{'size'}){
							$omega_hashes[$index]->{'size'} = ($row_index[$i]-$offset+1);
						}
						if ($row_index[$i] != $col_index[$i]){
							$omega_hashes[$index]->{'offdiag'}=1;
						}
					}else{
						push(@omega_hashes,{});
						$index = scalar(@omega_hashes)-1;
						#new block, change offset
						$offset = $row_index[$i];
						$omega_hashes[$index]->{'block_number'}=$block_number->[$i];
						$omega_hashes[$index]->{'offset'}=$offset;
						$omega_hashes[$index]->{'offdiag'}=0;
						$omega_hashes[$index]->{'size'}=1;
					}
					$row_index[$i] = $row_index[$i] -$offset;
					$col_index[$i] = $col_index[$i] -$offset;
				}else{
					$offset=0; #unnecessary?
				}
				$prev_block_number=$block_number->[$i];
			}
		}
	}


	my @samples_array=();
	my $counter=0;
	my $max_iter=1000;

	for (my $j=0; $j<$max_iter; $j++){
		#we will probably discard some samples, generate twice needed amount to start with
		my @candidate_samples = Math::Random::random_multivariate_normal((2*$samples), @muvec, @{$use_covmatrix});

		foreach my $xvec (@candidate_samples){
			my $accept = 1;
			for (my $i=0; $i< $dim; $i++){
				if ($xvec->[$i] <= $lower_bound->[$i]){
					$accept=0;
					last;
				}elsif($xvec->[$i] >= $upper_bound->[$i]){
					$accept=0;
					last;
				}elsif($xvec->[$i] == 0){
					#unlikely, but must handle 
					$accept=0;
					last;
				}
			}
			next unless $accept;
			if ($check_sigma_posdef){
				foreach my $ref (@sigma_hashes){
					next unless ($ref->{'offdiag'}==1);
					my $size = $ref->{'size'};
					my $mat = [];
					for (my $k=0; $k< $size; $k++){
						push(@{$mat},[(0) x $size]);
					}
					my $num = $ref->{'block_number'};
					for (my $i=0; $i< $dim; $i++){
						if (($param->[$i] eq 'sigma') and ($block_number->[$i] == $num)){
							my $row=$row_index[$i];
							my $col=$col_index[$i];
							$mat->[$row]->[$col]=$xvec->[$i];
							$mat->[$col]->[$row]=$xvec->[$i];
						}
					}
					#if get numerical error on cholesky then do not accept
					my $err = linear_algebra::cholesky($mat);
					if ($err == 1){
						$accept = 0;
						last;
					}
				}
			}
			next unless $accept;
			if ($check_omega_posdef){
				foreach my $ref (@omega_hashes){
					next unless ($ref->{'offdiag'}==1);
#					foreach my $key(keys %{$ref}){
#						print "$key ".$ref->{$key}."\n";
#					}
					my $size = $ref->{'size'};
					my $mat = [];
					for (my $k=0; $k< $size; $k++){
						push(@{$mat},[(0) x $size]);
					}
					my $num = $ref->{'block_number'};
					for (my $i=0; $i< $dim; $i++){
						if (($param->[$i] eq 'omega') and ($block_number->[$i] == $num)){
							my $row=$row_index[$i];
							my $col=$col_index[$i];
							$mat->[$row]->[$col]=$xvec->[$i];
							$mat->[$col]->[$row]=$xvec->[$i];
						}
					}
#					for (my $i=0; $i< $size; $i++){
#						print join(' ',@{$mat->[$i]})."\n";
#					}
					#if get numerical error on cholesky then do not accept
					my $err = linear_algebra::cholesky($mat);
					if ($err == 1){
						$accept = 0;
#						print "cholesky skip\n";
						last;
					}else{
#						print "cholesky accept\n";
					}
				}
			}
			next unless $accept;
			push(@samples_array,$xvec);
			$counter++;
			last if ($counter == $samples);
		}
		last if ($counter == $samples);
	}
	
	unless ($counter == $samples){
		croak("Failed to generate $samples accepted parameter vectors within the boundaries even after generating ".(2*$max_iter*$samples)." candidates");
	}
	return \@samples_array;

}

sub create_sampled_params_arr{
	#to be used in update_inits (from_hash=> $sampled_params_arr->[$k],ignore_missing_parameters=>1);
	my %parm = validated_hash(\@_,
							  samples_array => { isa => 'ArrayRef[ArrayRef]', optional => 0 },
							  labels_hash => { isa => 'HashRef', optional => 0 },
							  user_labels => { isa => 'Bool', optional => 0 },
		);
	my $samples_array = $parm{'samples_array'};
	my $labels_hash = $parm{'labels_hash'};
	my $user_labels = $parm{'user_labels'};
	
	my @allparams=();

	my $labelkey='coordinate_strings';
	if ($user_labels){
		$labelkey='labels';
	}

	foreach my $sample (@{$samples_array}){
		my %allpar;
		$allpar{'theta'} = {};
		$allpar{'omega'} = {};
		$allpar{'sigma'} = {};
		for (my $i=0; $i<scalar(@{$sample}); $i++){
			my $label = $labels_hash->{$labelkey}->[$i];
			my $param = $labels_hash->{'param'}->[$i];
			my $value = $sample->[$i];
			$allpar{$param}->{$label} = $value;
		}
		push (@allparams,\%allpar);
	}

	return \@allparams;
}

sub inflate_covmatrix
{
	my %parm = validated_hash(\@_,
							  matrix => { isa => 'ArrayRef[ArrayRef]', optional => 0 },
							  inflation => { isa => 'Num', optional => 0 }
		);
	my $matrix = $parm{'matrix'};
	my $inflation = $parm{'inflation'};

	my $dim = scalar(@{$matrix});
	unless ($dim > 0){
		croak("Input error inflate_covmatrix: dimension is 0 ");
	}
	unless (scalar(@{$matrix->[0]})==$dim){
		croak("Input error inflate_covmatrix: covmatrix is not square ");
	}
	unless ($inflation > 0){
		croak("Input error inflate_covmatrix: inflation must be larger than 0");
	}

	my @copy=();
	for (my $i=0;$i< $dim; $i++){
		push(@copy,[0 x $dim]);
		for (my $j=0;$j< $dim; $j++){
			$copy[$i]->[$j] = ($matrix->[$i]->[$j])*$inflation;
		}
	}
	return \@copy;
}

sub get_nonmem_covmatrix
{
	my %parm = validated_hash(\@_,
		output => { isa => 'output', optional => 0 }
	);
	my $output = $parm{'output'};
	
	unless ($output->have_output){
		croak("Trying get_nonmem_covmatrix but output object is empty, output file\n".$output->full_name."\n");
	}
	unless( $output -> parsed_successfully ){
		croak("Trying get_nonmem_covmatrix but unable to read everything from outputfile, parser error message:\n".
			  $output -> parsing_error_message());
	}
	unless ($output-> get_single_value(attribute => 'covariance_step_run')){
		croak("Trying get_nonmem_covmatrix but the covariance step was not run");
	}
	unless ($output-> get_single_value(attribute => 'covariance_step_successful')){
		croak("Trying get_nonmem_covmatrix but the covariance step was not successful");
	}
	if ($output-> get_single_value(attribute => 'covariance_step_warnings')){
		ui -> print( category => 'sir',
					 message  => "Warning: Doing get_nonmem_covmatrix but there were covariance step warnings in the lst-file. This is ".
					 " likely to give errors in the computation of weights");
	}
	
	my $lower_covar  = $output -> get_single_value(attribute => 'covariance_matrix');

	unless (defined $lower_covar){
		croak("Trying get_nonmem_covmatrix but the covariance matrix is undefined. Parsing error? Output file is\n".$output->full_name."\n");
	}

	my $covar = make_square( $lower_covar);
	return $covar;
}

sub get_nonmem_inverse_covmatrix
{

	my %parm = validated_hash(\@_,
		output => { isa => 'output', optional => 0 }
	);
	my $output = $parm{'output'};
	
	unless ($output->have_output){
		croak("Trying get_nonmem_inverse_covmatrix but output object is empty, output file\n".$output->full_name."\n");
	}
	unless( $output -> parsed_successfully ){
		croak("Trying get_nonmem_inverse_covmatrix but unable to read everything from outputfile, parser error message:\n".
			  $output -> parsing_error_message());
	}
	unless ($output-> get_single_value(attribute => 'covariance_step_run')){
		croak("Trying get_nonmem_inverse_covmatrix but the covariance step was not run");
	}
	unless ($output-> get_single_value(attribute => 'covariance_step_successful')){
		croak("Trying get_nonmem_inverse_covmatrix but the covariance step was not successful");
	}
	if ($output-> get_single_value(attribute => 'covariance_step_warnings')){
		ui -> print( category => 'sir',
					 message  => "Warning: Doing get_nonmem_inverse_covmatrix but there were covariance step warnings in the lst-file. This is ".
					 " likely to give errors in the SIR sampling");
	}
	
	my $icm  = $output -> get_single_value(attribute => 'inverse_covariance_matrix');

	unless (defined $icm){
		croak("Trying get_nonmem_inverse_covmatrix but the matrix is undefined. Parsing error? Output file is\n".$output->full_name."\n");
	}

	return $icm;
}






sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	1;
}




sub _modelfit_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 0 }
		);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	my ($dir,$file) = 
		OSspecific::absolute_path( $self ->directory(),
								   $self -> raw_results_file()->[$model_number-1] );
	my ($dir,$nonp_file) = 
		OSspecific::absolute_path( $self ->directory(),
								   $self -> raw_nonp_file()->[$model_number-1] );
	my $original_ofv = $self ->original_ofv();
	my $pdf_vector = $self->pdf_vector();
	my $samples = $self->samples();
	my $resamples = $self->resamples();
	my $with_replacement = $self->with_replacement();

	my $orig_mod = $self ->models()->[$model_number-1];


	$subroutine = sub {
		my $modelfit = shift;
		my $mh_ref   = shift;
		my %max_hash = %{$mh_ref};
		$modelfit -> raw_results_file([$dir.$file] );
		$modelfit -> raw_nonp_file( [$dir.$nonp_file] );

		$self->raw_line_structure($modelfit -> raw_line_structure());

		my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'problem'});
		my $probindex = $start;
		my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'subproblem'});
		my $subindex = $start;
		my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'ofv'});
		my $ofvindex=$start;
		croak("could not find ofv in raw results header") unless (defined $ofvindex);

		my @delta_ofv=();
		my $index = 0;
		foreach my $row ( @{$modelfit -> raw_results()} ) {
			my $delta_ofv;
			if (defined $row->[$ofvindex]){
				$delta_ofv = $row->[$ofvindex] - $original_ofv;
			}else{
				$delta_ofv = undef;
			}
			push(@delta_ofv,$delta_ofv);
			$index++;
		}

		unless (scalar(@delta_ofv)>=$samples){
			croak("Expected $samples ofv values after running models, but found only ".scalar(@delta_ofv)."\n".
				  "NONMEM run(s) must have failed, check lst-file(s) in m1.");
		}

		my $wghash = tool::sir::compute_weights(pdf_array => $pdf_vector,
												dofv_array => \@delta_ofv);

		my @original_weights = @{$wghash->{'weights'}};
		my $total_weights = $wghash->{'sum_weights'};
		my @times_sampled = (0) x $samples;

		for (my $i=0; $i<$resamples; $i++){
			my $sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
			$times_sampled[$sample_index]++;
			unless ($with_replacement or $i==$resamples){
				tool::sir::recompute_weights(weight_hash => $wghash,
											 reset_index => $sample_index);
			}
		}

		my @extra_headers = ('deltaofv','likelihood_ratio','relPDF','importance_ratio','probability_resample','resamples');
		$index=0;
		foreach my $row ( @{$modelfit -> raw_results()} ) {
			my @oldrow =@{$row};
			$row = [($index+1),
					@oldrow[0 .. $ofvindex],
					$delta_ofv[$index],
					((defined $delta_ofv[$index]) ? exp(-0.5*$delta_ofv[$index]): undef), #likelihood_ratio
					$pdf_vector->[$index],
					$original_weights[$index], #importance_ratio
					$original_weights[$index]/$total_weights, #probability_resample
					$times_sampled[$index],
					@oldrow[$ofvindex+1 .. $#oldrow]]; 
			$index++;
		}

		# The prepare_raw_results in the modelfit will fix the
		# raw_results for each maxev0 model, we must add
		# the result for the original model.

		my %dummy;
		my ($raw_results_row, $nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
																			  model => $orig_mod,
																			  raw_line_structure => \%dummy );
		$orig_mod -> outputs -> [0] -> flush;
		$raw_results_row->[0]->[0] = 'input';
		my @oldrow =@{$raw_results_row->[0]};
		my $row = [0,@oldrow[0 .. $ofvindex],0,undef,undef,undef,undef,undef,@oldrow[$ofvindex+1 .. $#oldrow]]; 
		
		unshift( @{$modelfit -> raw_results()}, @{[$row]} );

		#fix the header
		
		my @old_header = @{$modelfit -> raw_results_header()};
		my $headerindex;
		for (my $k=0; $k<scalar(@old_header);$k++){
			$headerindex = $k if ($old_header[$k] eq 'ofv');
		}
		$modelfit -> raw_results_header(
			['sample.id',@old_header[0 .. $headerindex],@extra_headers,@old_header[$headerindex+1 .. $#old_header]]);

		my $extra_header_count = scalar(@extra_headers);
		foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure()})){
			foreach my $category (keys %{$self->raw_line_structure() -> {$mod}}){
				next if ($category eq 'line_numbers');
				my ($start,$len) = split(',',$self->raw_line_structure() -> {$mod}->{$category});
				if ($start > $ofvindex){
					$self->raw_line_structure() -> {$mod}->{$category} = ($start+$extra_header_count+1).','.$len;#+1 extra for sample.id
				}else{
					$self->raw_line_structure() -> {$mod}->{$category} = ($start+1).','.$len; #+1 for sample id
				}
			}
			my $hc = 0;
			$self->raw_line_structure() -> {$mod}->{'sample.id'} = ($hc).',1'; #sample has index 0
			$hc++;
			foreach my $head (@extra_headers){
				$hc++; #first will be 2
				$self->raw_line_structure() -> {$mod}->{$head} = ($ofvindex+$hc).',1'; #first has 2 extra relative old ofvindex
				#			$self->raw_line_structure() -> {$mod}->{'deltaofv'} = ($ofvindex+1).',1';
			}
		}
		
		$self->raw_line_structure() -> write( $dir.'raw_results_structure' );

		$self -> raw_results_header($modelfit -> raw_results_header());
		$self -> raw_results($modelfit -> raw_results());

	};
	return $subroutine;
}


sub prepare_results
{
	my $self = shift;
	ui -> print( category => 'sir',
				 message => "\nAnalyzing results");

	if ($self->recompute()){
		#change results_file so that old is not overwritten
		my $fname = $self->results_file();
		$fname =~ s/\.csv$/_recompute/ ;
		my $addnum=1;
		while (-e $self -> directory."/$fname$addnum".'.csv'){
			$addnum++;
		}
		$self->results_file("$fname$addnum".'.csv');
		$self -> raw_results_file->[0] = $self->directory().$self->recompute();
	}

#	print "prepare results model name ".$self -> models -> [0]->full_name."\n";
	my $model = $self -> models -> [0];
	my ($sampled_params_arr,$href) = model::get_rawres_params(filename => $self->raw_results_file()->[0],
															  require_numeric_ofv => 0,
															  extra_columns => ['resamples'],
															  offset => 1,
															  model => $model); 

	## Prepare general run info for output file
	my %return_section;
	$return_section{'name'} = 'SIR run info';
	my $modelname=$model ->filename();
	$return_section{'labels'} = [[],['Date','samples','resamples','model','PsN version','NONMEM version']];

	my @datearr=localtime;
	my $the_date=($datearr[5]+1900).'-'.($datearr[4]+1).'-'.($datearr[3]);
	$return_section{'values'} = [[$the_date,$self->samples,$self->resamples(),$modelname,'v'.$PsN::version,$self->nm_version]];
	#results is initialized in tool.dia
	push( @{$self -> results->[0]{'own'}},\%return_section );


	my $output = $model -> outputs -> [0];
	unless (defined $output){
		croak("No output object from input model");
	}

#	print "output is ".$output->full_name."\n";

	my $parameter_hash = output::get_nonmem_parameters(output => $output);
	my $resulthash = empirical_statistics( sampled_params_arr => $sampled_params_arr,
										   labels_hash => $parameter_hash);

	#hash

	my %mean_section;
	$mean_section{'name'}='Mean over resamples';
	$mean_section{'labels'}=[[],$parameter_hash->{'labels'}];
	$mean_section{'values'}=[$resulthash->{'mean'}];
	push( @{$self -> results->[0]{'own'}},\%mean_section );

	my %perc_section;
	my @perc_labels=();
	foreach my $lab (@{$resulthash->{'percentiles_labels'}}){
		push(@perc_labels,$lab.'%');
	}

	$perc_section{'name'}='Quantiles (R type=2)';
	$perc_section{'labels'}=[\@perc_labels,$parameter_hash->{'labels'}];
	$perc_section{'values'}=$resulthash->{'percentiles_values'};
	push( @{$self -> results->[0]{'own'}},\%perc_section );

	my %space_section;
	$space_section{'name'}= ' ';
	$space_section{'labels'}= [' '];
	$space_section{'values'}= [[]];
	push( @{$self -> results->[0]{'own'}},\%space_section );

	my %covar_section;
	$covar_section{'name'}='Empirical covariance matrix';
	$covar_section{'labels'}=[$parameter_hash->{'labels'},$parameter_hash->{'labels'}];
	$covar_section{'values'}=$resulthash->{'covar'};
	push( @{$self -> results->[0]{'own'}},\%covar_section );

	my $basename = $model->create_output_filename();
	$basename =~ s/\.lst$/_sir.cov/;
	my( $ldir, $name ) = OSspecific::absolute_path( $self ->directory(),$basename);
	my $output_covar_file = $ldir.$name;
	my @order=();
	my @coords=();
	foreach my $param ('theta','sigma','omega'){
		for (my $i=0; $i<scalar(@{$parameter_hash->{'coordinate_strings'}}); $i++){
			if ($parameter_hash->{'param'}->[$i] eq $param){
				push(@coords, $parameter_hash->{'coordinate_strings'}->[$i]) ;
				push(@order, $i);
			}
		}
	}
	my $copy = linear_algebra::copy_and_reorder_square_matrix($resulthash->{'covar'},\@order);
	my $formatted = format_covmatrix(matrix => $copy, header => \@coords, comma => 0, print_labels => 1);
	open ( RES, ">" . $output_covar_file );
	foreach my $line (@{$formatted}){
		print RES $line;
	}
	close(RES);
}

sub format_covmatrix
{
	my %parm = validated_hash(\@_,
							  matrix => { isa => 'ArrayRef', optional => 0 },
							  header => { isa => 'ArrayRef', optional => 0 },
							  print_labels => { isa => 'Bool', optional => 0 },
							  comma => { isa => 'Bool', optional => 0 },
		);
	my $matrix = $parm{'matrix'};
	my $header = $parm{'header'};
	my $print_labels = $parm{'print_labels'};
	my $comma = $parm{'comma'};
	
	my @output = ();

	if ($print_labels){
		my $line;
		if ($comma){
			$line = '"NAME"';
		}else{
			$line = ' NAME             ';
		}
		foreach my $head (@{$header}){
			if ($comma){
				$line .= ',"'.$head.'"';
			}else{
				$line .= sprintf("%-15s",$head);
			}
		}
		push (@output,$line."\n");
	}
	for (my $row=0; $row< scalar(@{$matrix}); $row++){
		my $line = '';
		if ($print_labels){
			if ($comma){
				$line = '"'.$header->[$row].'"';
			}else{
				$line = sprintf(" %-15s",$header->[$row]);
			}
		}
		for (my $col=0; $col< scalar(@{$matrix}); $col++){
			if ($comma){
				$line .= ',' if (($col > 0) or ($print_labels));
				$line .= $matrix->[$row][$col];
			}else{
				$line .= sprintf(" %14.7E",$matrix->[$row][$col]);
			}
		}
		push (@output,$line."\n");
	}
	return \@output;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
