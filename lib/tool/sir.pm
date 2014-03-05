package tool::sir;

use include_modules;
use strict;
use File::Copy 'cp';
use data;
use OSspecific;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Moose;
use MooseX::Params::Validate;
use ext::Math::MatrixReal;# qw(all); 
use Math::Trig;	# For pi
use Math::Random;
use output;
use array qw(:all);
use linear_algebra;

extends 'tool';

has 'sir_raw_results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['sirlog.csv'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'sir_results.csv' );

has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 0 );
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
has 'pdf_vector' => ( is => 'rw', isa => 'ArrayRef[Num]' );


sub BUILD
{
	my $this  = shift;

	for my $accessor ('logfile','raw_results_file','raw_nonp_file'){
		my @new_files=();
		my @old_files = @{$this->$accessor};
		for (my $i=0; $i < scalar(@old_files); $i++){
			my $name;
			my $ldir;
			( $ldir, $name ) =
				OSspecific::absolute_path( $this ->directory(), $old_files[$i] );
			push(@new_files,$ldir.$name) ;
		}
		$this->$accessor(\@new_files);
	}	

	croak("No \$PROBLEM in input model") unless 
		(defined $this ->models()->[0]->problems and scalar(@{$this ->models()->[0]->problems})>0);

	croak("No \$INPUT found") unless 
		(defined $this ->models()->[0]->problems->[0]->inputs and 
		 scalar(@{$this ->models()->[0]->problems->[0]->inputs})>0);
	croak("No \$DATA found") unless 
		(defined $this ->models()->[0]->problems->[0]->datas and 
		 scalar(@{$this ->models()->[0]->problems->[0]->datas})>0);

	#TODO support multiple $PROB
	croak("sir does not yet support more than 1 \$PROB in model ") if (scalar(@{$this ->models()->[0]->problems})>1);


	croak("Number of samples must be larger than 0") unless ($this->samples()>0);
	croak("Number of resamples must be larger than 0") unless ($this->resamples()>0);
	croak("Number of resamples cannot be larger than samples unless with_replacement is set") unless 
		(($this->resamples() <= $this->samples) or ($this->with_replacement));
	

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

	unless ( $model -> is_run ) {
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

	my $icm = get_nonmem_inverse_covmatrix(output => $output);
	my $covmatrix = get_nonmem_covmatrix(output => $output);
	my $parameter_hash = get_nonmem_parameters(output => $output);

	#$model-> get_values_to_labels(category => $param);
#			$paramnames = $model -> labels(parameter_type => $param);

	my $values = $parameter_hash->{'filtered_values'};
	my $mat = new Math::MatrixReal(1,1);
	my $muvector = $mat->new_from_rows( [$values] );

	my $vectorsamples = sample_multivariate_normal(samples=>$self->samples,
												   covmatrix => $covmatrix,
												   lower_bound => $parameter_hash->{'lower_bounds'},
												   upper_bound => $parameter_hash->{'upper_bounds'},
												   mu => $muvector);
	
	my $sampled_params_arr = create_sampled_params_arr(samples_array => $vectorsamples,
													   labels_hash => $parameter_hash);
	
	
	$self->pdf_vector(mvnpdf(inverse_covmatrix => $icm,
							 mu => $muvector,
							 xvec_array => $vectorsamples));

	my $modelsarr = model::create_maxeval_zero_models_array(
		sampled_params_arr => $sampled_params_arr,
		mceta => $self->mceta(),
		ignore_missing_parameters => 1,
		basedirectory => $self->directory,
		subdirectory => $self->directory().'m'.$model_number.'/',
		model => $model,
		purpose => 'sir'
		);
	
	$self -> prepared_models -> [$model_number-1]{'own'} = $modelsarr;

	my @subtools = ();
	@subtools = @{$self -> subtools()} if (defined $self->subtools());
	shift( @subtools );
	my %subargs = ();
	if ( defined $self -> subtool_arguments() ) {
		%subargs = %{$self -> subtool_arguments()};
	}
	if (not $self->copy_data()){
		$subargs{'data_path'}='../../m'.$model_number.'/';
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
			 %subargs ) );
	
	$self->stop_motion_call(tool=>'sir',message => "Created a modelfit object to run all the models in ".
							$self ->directory().'m'.$model_number)
		if ($self->stop_motion());
}


sub mvnpdf{
	my %parm = validated_hash(\@_,
							  inverse_covmatrix => { isa => 'Math::MatrixReal', optional => 0 },
							  mu => { isa => 'Math::MatrixReal', optional => 0 },
							  xvec_array => { isa => 'ArrayRef[ArrayRef[Num]]', optional => 0 }
	);
	my $inverse_covmatrix = $parm{'inverse_covmatrix'};
	my $mu = $parm{'mu'};
	my $xvec_array = $parm{'xvec_array'};

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

	my $det_factor = get_determinant_factor(inverse_covmatrix => $inverse_covmatrix,
											k => $k);

	my @pdf_array=();
	my $delta = $mu->shadow(); #zeros matrix same size as $mu

	foreach my $xvec (@{$xvec_array}){
		unless (scalar(@{$xvec}) == $k){
			croak("Input error mvnpdf: xvec should have dimension $k but has dimension ".scalar(@{$xvec}));
		}
		for (my $i=0; $i< $k; $i++){
			$delta->assign(1,($i+1),($xvec->[$i] - $mu->element(1,($i+1))));
		}
		#now $delta is $xvec - $mu
		my $product_left = $delta->multiply($inverse_covmatrix);
		my $product=$product_left->multiply(~$delta); # ~ is transpose
		push(@pdf_array,$det_factor*exp(-0.5 * $product->element(1,1)));

	}

	return \@pdf_array;
}


sub compute_weights{
	my %parm = validated_hash(\@_,
		pdf_array => { isa => 'ArrayRef[Num]', optional => 0 },
		dofv_array => { isa => 'ArrayRef[Num]', optional => 0 }
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
							  cdf => { isa => 'ArrayRef[Num]', optional => 0 }
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
							  samples_array => { isa => 'ArrayRef[ArrayRef[Num]]', optional => 0 },
							  sample_counts => { isa => 'ArrayRef[Num]', optional => 0 }
		);
	my $samples_array = $parm{'samples_array'};
	my $sample_counts = $parm{'sample_counts'};

	my $len = scalar(@{$samples_array});
	croak("empty set of samples to empirical_statistics") unless ($len >0);
	my $dim = scalar(@{$samples_array->[0]});
	croak("empty set of values in samples_array to empirical_statistics") unless ($dim >0);
	my $len2 = scalar(@{$sample_counts});
	croak("empty set of sample_counts to empirical_statistics") unless ($len2 >0);
	croak("number $len of sets in samples_array is not the same as number $len2 in sample_counts") unless ($len == $len2);

	#create samples matrix
	my @Amatrix=();
	my @parameter_vectors=();
	my @sums=(0) x $dim;
	for (my $j=0; $j< $dim; $j++){
		push(@parameter_vectors,[]);
	}

	for (my $i=0; $i< $len; $i++){
		for (my $k=0; $k< $sample_counts->[$i]; $k++){
			push(@Amatrix,$samples_array->[$i]);
			for (my $j=0; $j< $dim; $j++){
				push(@{$parameter_vectors[$j]},$samples_array->[$i]->[$j]);
				$sums[$j] = $sums[$j] + $samples_array->[$i]->[$j];
			}
		}
	}

	my %resulthash;
	$resulthash{'covar'}=[];
	my $err1 = linear_algebra::row_cov(\@Amatrix,$resulthash{'covar'});
	if ($err1 == 1){
		croak ("numerical error in linear_algebra rowcov");
	}elsif ($err1 == 2){
		croak ("input error to linear_algebra rowcov");
	}

	#TODO univariate percentiles or something

	my @indices=();

	my @pred_int = sort {$a <=> $b} 0,40,80,90,95;
	my @temp =();
	foreach my $pi (@pred_int){
		if ($pi == 0){
			push (@temp,50); #need to have median last of these three for order in diagnostics output
		}else {
			push (@temp,(100-$pi)/2);
			push (@temp,(100-(100-$pi)/2));
		}
	}
	my @perc_limit = sort {$a <=> $b} @temp;
	my $no_perc_limits = scalar(@perc_limit);
	my @limit_index = (0) x $no_perc_limits;

 	for (my $j=0; $j< $no_perc_limits; $j++){
		if ($perc_limit[$j] == 50){
			$limit_index[$j] = -1; #signal to use median
		}else{
			$limit_index[$j] = round(number=>$perc_limit[$j]*($len-1)/100);
		}
	}


	$resulthash{'mean'}=[];
	$resulthash{'percentiles_labels'}=\@perc_limit;
	$resulthash{'percentiles_values'}=[];
 	for (my $j=0; $j< $dim; $j++){
		push(@{$resulthash{'mean'}},($sums[$j]/$len));
		my @sorted = (sort {$a <=> $b} @{$parameter_vectors[$j]}); #sort ascending
		my @limit =();
		for (my $k=0; $k< scalar(@limit_index); $k++){
			if ($limit_index[$k] > 0){
				push(@limit,$sorted[$limit_index[$k]]);
			}else{
				#median
				push(@limit,median(sorted_array => \@sorted));
			}
			push(@{$resulthash{'percentiles_values'}},\@limit);

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
							  k => { isa => 'Int', optional => 0 }
	);
	my $inverse_covmatrix = $parm{'inverse_covmatrix'};
	my $k = $parm{'k'};

	my $invdeterminant = $inverse_covmatrix->det();
	unless (defined $invdeterminant and $invdeterminant > 0){
		print "\nInverse covmatrix:\n";
		print $inverse_covmatrix;
		croak("\nFailed to compute determinant of inverse covariance matrix");
	}
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

sub get_nonmem_parameters
{
	my %parm = validated_hash(\@_,
		output => { isa => 'output', optional => 0 }
	);
	my $output = $parm{'output'};
	
	unless ($output->have_output){
		croak("Trying get_nonmem_parameters but output object is empty, output file\n".$output->full_name."\n");
	}
	unless( $output -> parsed_successfully ){
		croak("Trying get_nonmem_parameters but unable to read everything from outputfile, parser error message:\n".
			  $output -> parsing_error_message());
	}

	my $init_problem;
	if ( not_empty($output->problems) ) {
		$init_problem = $output->problems->[0]->input_problem();
	} else {
	    croak("No problems defined in output object in get_nonmem_parameters");
	}

	my %hash;
	$hash{'filtered_labels'}=[];
	$hash{'lower_bounds'}=[];
	$hash{'upper_bounds'}=[];
	$hash{'filtered_values'}=[];
	$hash{'param'}=[];

	#TODO verify that this is the parameter order in invcov
	foreach my $param ('theta','omega','sigma'){
		my $accessor=$param.'s';
		my $coordval = $output -> get_single_value(attribute => $param.'coordval'); #ref to a hash
		croak("No $param coordval in output object") unless (defined $coordval);
		my @records;
		if (defined $init_problem -> $accessor) {
			@records = @{$init_problem -> $accessor};
		}
		next unless (scalar(@records) > 0); #no parameter in this problem

		foreach my $record (@records){
			if  ($record->same() or $record->fix() or $record->prior()) {
				next;
			}
			unless (defined $record -> options()) {
				croak("$param record has no values in get_nonmem_parameters in output object");
			}
			foreach my $option (@{$record -> options()}) {
				if ($option->fix() or $option->prior()) {
					next;
				}
				if ($param eq 'theta'){
					my $lobnd = $option ->lobnd();
					$lobnd = -1000000 unless (defined $lobnd);
					push(@{$hash{'lower_bounds'}},$lobnd);
					my $upbnd = $option ->upbnd();
					$upbnd = 1000000 unless (defined $upbnd);
					push(@{$hash{'upper_bounds'}},$upbnd);
				}else{
		  			if ($option -> on_diagonal()){
						push(@{$hash{'lower_bounds'}},0);
						push(@{$hash{'upper_bounds'}},1000000);
					}else{	 
						if ($option->init() == 0) {
							#do not check off-diagonal zeros
							next;
						}else{
							push(@{$hash{'lower_bounds'}},-1);
							push(@{$hash{'upper_bounds'}},1);
						}
		  			}
				}
				my $coord = $option -> coordinate_string();
				my $name = $coord;
				if (defined $option ->label()) {
					$name = $option ->label();
				}
				push(@{$hash{'filtered_labels'}},$name);
				push(@{$hash{'param'}},$param);
				my $value = $coordval->{$coord};
				croak("No estimate for $param $coord") unless (defined $value);
				push(@{$hash{'filtered_values'}},$value);
			}
		}
	}

	return \%hash;
}

sub sample_multivariate_normal
{
	my %parm = validated_hash(\@_,
							  samples => { isa => 'Int', optional => 0 },
							  covmatrix => { isa => 'ArrayRef[ArrayRef[Num]]', optional => 0 },
							  lower_bound => { isa => 'ArrayRef[Num]', optional => 0 },
							  upper_bound => { isa => 'ArrayRef[Num]', optional => 0 },
							  mu => { isa => 'Math::MatrixReal', optional => 0 }							  
		);
	my $samples = $parm{'samples'};
	my $covmatrix = $parm{'covmatrix'};
	my $lower_bound = $parm{'lower_bound'};
	my $upper_bound = $parm{'upper_bound'};
	my $mu = $parm{'mu'};
	
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
		croak("Input error sample_multivariate_normal: mu vector has dimension $dim but lower_bound has dimension ".scalar(@{$upper_bound}));
	}
	unless (scalar($samples)>0){
		croak("Input error sample_multivariate_normal: samples must be larger than 0");
	}
	my @muvec=();
	for (my $i=1; $i<= $dim; $i++){
		push(@muvec,$mu->element(1,$i));
	}
	
	my @samples_array=();
	my $counter=0;

	for (my $j=0; $j<10; $j++){
		#we will probably discard some samples, generate twice needed amount to start with
		my @candidate_samples = Math::Random::random_multivariate_normal((2*$samples), @muvec, @{$covmatrix});

		foreach my $xvec (@candidate_samples){
			my $accept = 1;
			for (my $i=0; $i< $dim; $i++){
				if ($xvec->[$i] <= $lower_bound->[$i]){
					$accept=0;
					last;
				}elsif($xvec->[$i] >= $upper_bound->[$i]){
					$accept=0;
					last;
				}
			}
			next unless $accept;
			#my $xvec = $mu->new_from_rows( [[0.006,1,0.5,0.4,0.1]] );
			push(@samples_array,$xvec);
			$counter++;
			last if ($counter == $samples);
		}
		last if ($counter == $samples);
	}
	
	unless ($counter == $samples){
		croak("Failed to generate $samples accepted parameter vectors within the boundaries even after generating ".(20*$samples)." candidates");
	}
	return \@samples_array;

}

sub create_sampled_params_arr{
	#to be used in update_inits (from_hash=> $sampled_params_arr->[$k],ignore_missing_parameters=>1);
	my %parm = validated_hash(\@_,
							  samples_array => { isa => 'ArrayRef[ArrayRef[Num]]', optional => 0 },
							  labels_hash => { isa => 'HashRef', optional => 0 }
		);
	my $samples_array = $parm{'samples_array'};
	my $labels_hash = $parm{'labels_hash'};
	
	my @allparams=();

	foreach my $sample (@{$samples_array}){
		my %allpar;
		$allpar{'theta'} = {};
		$allpar{'omega'} = {};
		$allpar{'sigma'} = {};
		for (my $i=0; $i<scalar(@{$sample}); $i++){
			my $label = $labels_hash->{'filtered_labels'}->[$i];
			my $param = $labels_hash->{'param'}->[$i];
			my $value = $sample->[$i];
			$allpar{$param}->{$label} = $value;
		}
		push (@allparams,\%allpar);
	}

	return \@allparams;
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
					 message  => "Warning: Doing get_nonmem_inverse_covmatrix but there were covariance step warnings in the lst-file. This is ".
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
		my $wghash = tool::sir::compute_weights(pdf_array => $pdf_vector,
												dofv_array => \@delta_ofv);

		my @original_weights = @{$wghash->{'weights'}};
		my @times_sampled = (0) x $samples;

		for (my $i=0; $i<$resamples; $i++){
			my $sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
			$times_sampled[$sample_index]++;
			unless ($with_replacement or $i==$resamples){
				tool::sir::recompute_weights(weight_hash => $wghash,
											 reset_index => $sample_index);
			}
		}

		$index=0;
		foreach my $row ( @{$modelfit -> raw_results()} ) {
			my @oldrow =@{$row};
			$row = [@oldrow[0 .. $ofvindex],$delta_ofv[$index],$pdf_vector->[$index],
					$original_weights[$index],$times_sampled[$index],@oldrow[$ofvindex+1 .. $#oldrow]]; 
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
		my $row = [@oldrow[0 .. $ofvindex],0,undef,undef,undef,@oldrow[$ofvindex+1 .. $#oldrow]]; 
		
		unshift( @{$modelfit -> raw_results()}, @{[$row]} );

		#fix the header
		
		my @old_header = @{$modelfit -> raw_results_header()};
		my $headerindex;
		for (my $k=0; $k<scalar(@old_header);$k++){
			$headerindex = $k if ($old_header[$k] eq 'ofv');
		}
		$modelfit -> raw_results_header(
			[@old_header[0 .. $headerindex],'deltaofv','PDF','weight','resamples',@old_header[$headerindex+1 .. $#old_header]]);
		
		foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure()})){
			foreach my $category (keys %{$self->raw_line_structure() -> {$mod}}){
				next if ($category eq 'line_numbers');
				my ($start,$len) = split(',',$self->raw_line_structure() -> {$mod}->{$category});
				$self->raw_line_structure() -> {$mod}->{$category} = ($start+4).','.$len
					if ($start > $ofvindex); #+4 for deltaofv PDF weight resamples
			}
			$self->raw_line_structure() -> {$mod}->{'deltaofv'} = ($ofvindex+1).',1';
			$self->raw_line_structure() -> {$mod}->{'PDF'} = ($ofvindex+2).',1';
			$self->raw_line_structure() -> {$mod}->{'weight'} = ($ofvindex+3).',1';
			$self->raw_line_structure() -> {$mod}->{'resamples'} = ($ofvindex+4).',1';
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

	1;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
