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
use math qw(usable_number round);
use linear_algebra;
use utils::file;
use boxcox;
extends 'tool';

has 'sir_raw_results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['sirlog.csv'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'sir_results.csv' );

has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'recenter' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'negative_dofv' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'recompute' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'with_replacement' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'samples' => ( is => 'rw', required => 1, isa => 'ArrayRef' );
has 'resamples' => ( is => 'rw', required => 1, isa => 'ArrayRef' );
has 'attempted_samples' => ( is => 'rw', isa => 'ArrayRef',default => sub { [] } );
has 'successful_samples' => ( is => 'rw', isa => 'ArrayRef',default => sub { [] } );
has 'actual_resamples' => ( is => 'rw', isa => 'ArrayRef',default => sub { [] } );
has 'iteration' => ( is => 'rw', isa => 'Int', default => 1 );
has 'rawres_samples' => ( is => 'rw', isa => 'Int', default => 0 );
has 'max_iteration' => ( is => 'rw', isa => 'Int', default => 2 );
has 'covmat_input' => ( is => 'rw', isa => 'Str' );
has 'auto_rawres' => ( is => 'rw', isa => 'Maybe[Num]' );
has 'rawres_input' => ( is => 'rw', isa => 'Str' );
has 'offset_rawres' => ( is => 'rw', isa => 'Int', default => 1 );
has 'in_filter' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'inflation' => ( is => 'rw', isa => 'Num', default => 1 );
has 'mceta' => ( is => 'rw', isa => 'Int', default => 0 );
has 'problems_per_file' => ( is => 'rw', isa => 'Maybe[Int]', default => 100 );
has 'full_rawres_header' => ( is => 'rw', isa => 'ArrayRef' );

has 'minimum_ofv' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] });
has 'reference_ofv' => ( is => 'rw', isa => 'Num');
has 'pdf_vector' => ( is => 'rw', isa => 'ArrayRef' );
has 'intermediate_raw_results_files' => ( is => 'rw', isa => 'ArrayRef', default => sub{[]} );

our $relativepdf = 1;

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

	$self->problems_per_file(100) unless defined ($self->problems_per_file);
	croak("problems_per_file must be larger than 0") unless ($self->problems_per_file > 0);

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

	unless (scalar(@{$self->samples}) == scalar(@{$self->resamples})){
		croak("samples and resamples arrays must have equal length");
	} 
	unless (scalar(@{$self->samples})>0){
		croak("samples arrays must have at least length 1");
	} 
	$self->max_iteration(scalar(@{$self->samples}));
	for (my $j=0; $j<scalar(@{$self->samples}); $j++){
		croak("Number of samples must be larger than 0") unless ($self->samples()->[$j]>0);
		croak("Number of resamples must be larger than 1") unless ($self->resamples()->[$j]>1);
		croak("Number of resamples cannot be larger than samples unless with_replacement is set") unless 
			(($self->resamples()->[$j] <= $self->samples->[$j]) or ($self->with_replacement));
	}

	if (defined $self->covmat_input and defined $self->rawres_input){
		croak("Not allowed to set both covmat_input and rawres_input");
	}

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

		my $orig_fit = 
			tool::modelfit->new( %{common_options::restore_options(@common_options::tool_options)},
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
		$self->reference_ofv($original_ofv);
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


	my $rawres_resampled_params_arr;
	if (defined $self->rawres_input or (defined $self->auto_rawres)){
		if (defined $self->rawres_input){
			my $resamp_href;
			($rawres_resampled_params_arr,$resamp_href) = model::get_rawres_params(filename => $self->rawres_input,
																				   filter => $self->in_filter,
																				   offset => $self->offset_rawres,
																				   model => $model);
			$self->rawres_samples(scalar(@{$rawres_resampled_params_arr}));
		}else{
			$rawres_resampled_params_arr = [];
		}

		unless (scalar(@{$rawres_resampled_params_arr}) >= scalar(@{$parameter_hash->{'values'}})){
			my $filename = 'tweak_inits_rawres.csv';
			my $covfile = 'tweak_inits.cov';
			my $message = "Have ".scalar(@{$rawres_resampled_params_arr})." parameter vectors but need at least ".(scalar(@{$parameter_hash->{'values'}})).
				"\n"."Using tweak inits to create fake raw results file tweak_inits_rawres.csv and covmat file $covfile";
			ui -> print( category => 'sir', message =>  $message);
			my $tweaksamples = tweak_inits_sampling( sampled_params_arr => $rawres_resampled_params_arr,
													 parameter_hash => $parameter_hash,
													 model => $model,
													 output => $output,
													 degree => $self->auto_rawres,
													 filename => $filename,
													 directory => $self->directory,
				);
				
			$rawres_resampled_params_arr = create_sampled_params_arr(samples_array => $tweaksamples,
																	 labels_hash => $parameter_hash,
																	 user_labels => 1); #otherwise get empty values, zero vectors

			my $resulthash = empirical_statistics( sampled_params_arr => $rawres_resampled_params_arr,
												   labels_hash => $parameter_hash);

			my( $ldir, $name ) = OSspecific::absolute_path( $self ->directory(),$covfile);
			print_empirical_covmatrix(filename=> $ldir.$name, parameter_hash => $parameter_hash, covar => $resulthash->{'covar'});

		}
	}


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

	}elsif (defined $self->rawres_input or (defined $self->auto_rawres)){
		#do not need any matrices at all, not sampling in 0th iteration
		1;
	}else{
		$covmatrix = get_nonmem_covmatrix(output => $output);
	}

	if (defined $covmatrix ){
		my $err = check_matrix_posdef(matrix => $covmatrix);
		if ($err == 1){
			croak("\nERROR: covariance matrix is numerically not positive definite\n".
				  "(as checked with Cholesky decomposition without pivoting). Cannot proceed with sir.\n");
		}
	}


	my $user_labels=0; #always use generic labels
	my ($resampled_params_arr,$resamp_href);
	my $boxcox_resulthash;
	my $have_previous_iteration = 0;

	for (my $iteration=0;$iteration<=$self->max_iteration(); $iteration++){
		next if ($iteration == 0 and (defined $covmatrix));
		$self->iteration($iteration);
		if ($iteration > 0 ) {
			my $message = "Sampling from the truncated multivariate normal distribution";
			ui -> print( category => 'sir',	 message => $message);
			my $mu_values; 
			my $lambda;
			my $delta;
			my $inflation;
			if ($have_previous_iteration){
				croak("iteration $iteration boxcox_resulthash not defined") 
					unless (defined $boxcox_resulthash and 
							defined $boxcox_resulthash->{'lambda'} and 
							defined $boxcox_resulthash->{'delta'} and
							defined $boxcox_resulthash->{'covar'});

				if ($boxcox_resulthash->{'rank_deficient'}){
					croak("The number of parameter vectors obtained in iteration ".($iteration-1)." is smaller than ".
						"the number of estimated parameters. This gives a rank deficient covariance matrix, so ".
						"sampling cannot proceed in iteration $iteration.\n");
				}

				$lambda = $boxcox_resulthash->{'lambda'};
				$delta = $boxcox_resulthash->{'delta'};
				$mu_values = boxcox::shift_and_box_cox(vector=>$parameter_hash->{'values'},
													   inverse=>0,
													   lambda=>$lambda,
													   delta=>$delta);
				if (1){
					open ( RES, ">" . $self->directory().'delta_lambda_iteration'.($iteration-1).'.csv' );
					print RES "delta,lambda,original.estimate,transformed.estimate\n";
					for (my $k=0; $k< scalar(@{$delta}); $k++){
						print RES $delta->[$k].','.$lambda->[$k].','.$parameter_hash->{'values'}->[$k].','.$mu_values->[$k]."\n";
					}
					close(RES);
				}
				$covmatrix = $boxcox_resulthash->{'covar'};
				my $err = check_matrix_posdef(matrix => $covmatrix);
				if ($err == 1){
					croak("\nERROR: Empirical covariance matrix obtained after Box-Cox transformation is numerically ".
						  "not positive definite\n(as checked with Cholesky decomposition without pivoting). Cannot proceed with sir.\n");
				}
				$inflation = 1;
			}else{
				$mu_values= $parameter_hash->{'values'};
				$lambda = [];
				$delta = [];
				$inflation = $self->inflation();
				#covmatrix already read
			}
			my $mat = new Math::MatrixReal(1,1);
			my $muvector = $mat->new_from_rows( [$mu_values] );
			my $current_samples = update_attempted_samples(samples=>$self->samples,
														   successful_samples=>$self->successful_samples,
														   attempted_samples=>$self->attempted_samples,
														   iteration=> $iteration);
			if (($current_samples > $self->samples->[($iteration-1)]) and 
				($self->problems_per_file() > 1)){
				ui->print(category=> "sir", message => "Setting problems_per_file to 1 to minimize ofv losses");
				$self->problems_per_file(1);
			}
			my $sampled_params_arr;
			my ($vectorsamples,$boxcox_samples) = sample_multivariate_normal(samples=>$current_samples,
																			 covmatrix => $covmatrix,
																			 inflation => $inflation,
																			 lower_bound => $parameter_hash->{'lower_bounds'},
																			 upper_bound => $parameter_hash->{'upper_bounds'},
																			 param => $parameter_hash->{'param'},
																			 coords => $parameter_hash->{'coords'},
																			 block_number => $parameter_hash->{'block_number'},
																			 mu => $muvector,
																			 lambda => $lambda,
																			 delta => $delta);
		
			$sampled_params_arr = create_sampled_params_arr(samples_array => $vectorsamples,
															labels_hash => $parameter_hash,
															user_labels => $user_labels);
			my $pdfvec;
			if ($have_previous_iteration ){
				$pdfvec = linear_algebra::mvnpdf_cholesky($covmatrix,$mu_values,$boxcox_samples,$inflation,$relativepdf);
			}else{
				$pdfvec= linear_algebra::mvnpdf_cholesky($covmatrix,$mu_values,$vectorsamples,$inflation,$relativepdf)
			}
			$self->pdf_vector($pdfvec);

			ui -> print( category => 'sir',
						 message => "Creating parameter vector evaluation models iteration $iteration...");

			my $modelsarr = model::create_maxeval_zero_models_array(
				problems_per_file => $self->problems_per_file,
				sampled_params_arr => $sampled_params_arr,
				mceta => $self->mceta(),
				ignore_missing_parameters => 1,
				basedirectory => $self->directory,
				subdirectory => $self->directory().'m'.$model_number.'/',
				model => $model,
				purpose => 'sir_iteration'.$iteration,
				match_labels => $user_labels
				);
		
			my %subargs = ();
			if ( defined $self -> subtool_arguments() ) {
				%subargs = %{$self -> subtool_arguments()};
			}
		
			my $message = "Running iteration ".$self->iteration()." evaluation models";

			my $iteration_evaluation = 
				tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
									  models		 => $modelsarr,
									  base_directory   => $self -> directory,
									  raw_results_file => [$self->directory.'raw_results_sir_iteration'.$iteration.'.csv'],
									  directory             => undef,
									  directory_name_prefix => 'iteration'.$iteration,
									  _raw_results_callback => $self ->
									  _modelfit_raw_results_callback( model_number => $model_number ),
#								  subtools              => \@subtools,
									  nmtran_skip_model => 2,
									  raw_results           => undef,
									  prepared_models       => undef,
									  top_tool              => 0,
									  copy_data             => $self->copy_data,
									  %subargs );



			ui -> print( category => 'sir', message => $message );

			if ($iteration < $self->max_iteration()){
				#not final round
				push(@{$self->intermediate_raw_results_files},'raw_results_sir_iteration'.$iteration.'.csv');
				$iteration_evaluation -> run;
				#here we read all successful samples even if not resampled. The filtering on resamples column
				#is done in sub empirical_statistics
				($resampled_params_arr,$resamp_href) = 
					model::get_rawres_params(filename => $iteration_evaluation->raw_results_file()->[0],
											 require_numeric_ofv => 1,
											 extra_columns => ['resamples'],
											 offset => 1, #first is original
											 model => $model); 

				if (($self->negative_dofv->[($iteration-1)] > 0) and ($self->recenter)){
					my ($errors,$new_ofv) = recenter_mu(sampled_params_arr => $resampled_params_arr,
														parameter_hash => $parameter_hash);
					if (scalar(@{$errors})>0){
						croak("Recentering mu failed, error messages are \n".join("\n",@{$errors})."\n");
					}
					$self->reference_ofv($new_ofv);
					ui->print(category => 'sir',
							  message => "Recentered mu to parameter vector\n".
							  join(' ',@{$parameter_hash->{'values'}})."\n".
							  "with lowest ofv $new_ofv");
				}

			}else{
				#final round
				my ($dir,$file)= 
					OSspecific::absolute_path( $self ->directory(),
											   $self -> raw_results_file()->[$model_number-1] );
				push(@{$self->intermediate_raw_results_files},$file);

				$self -> prepared_models -> [$model_number-1]{'own'} = $modelsarr;
				$self->tools([]) unless (defined $self->tools());
				push( @{$self -> tools()},$iteration_evaluation);
				trace(tool => 'sir', message => "Created a modelfit object to run all the models in ".
					  $self ->directory().'m'.$model_number, level => 1);
			}
			#end if iteration > 0
		}else{
			#this is iteration 0, rawres either from file or tweak_inits or mix

			$resampled_params_arr = $rawres_resampled_params_arr;

		}


		if ($iteration < $self->max_iteration()){
			#not final round
			#Now we should have resampled_params_arr, either from first iteration or rawresinput, on original scale
			#Do Box-Cox and get transformed covariance matrix
			ui -> print( category => 'sir', message => "Find optimal Box-Cox transformation of resampled vectors" );
			$boxcox_resulthash = empirical_statistics( sampled_params_arr => $resampled_params_arr,
													   labels_hash => $parameter_hash,
													   get_lambda_delta => 1,
													   estimated_vector => $parameter_hash->{'values'},
													   do_percentiles => 0);
			my( $ldir, $name ) = OSspecific::absolute_path( $self ->directory(),'boxcox_covmatrix_iteration'.
															$iteration.'.cov');
			#this does not use mu-vector
			print_empirical_covmatrix(filename=> $ldir.$name,
									  covar => $boxcox_resulthash->{'covar'},
									  parameter_hash => $parameter_hash);

		}
		$have_previous_iteration=1;
	}


}


sub recenter_mu{
	#static, no shift
	my %parm = validated_hash(\@_,
							  sampled_params_arr => { isa => 'ArrayRef', optional => 0 },
							  parameter_hash => { isa => 'HashRef', optional => 0 }
		);
	my $sampled_params_arr = $parm{'sampled_params_arr'};
	my $parameter_hash = $parm{'parameter_hash'};

	#sampled_params_arr has been created with require_numeric_ofv, so should not have NaN here
	my $index_lowest_ofv=0;
	my $lowest_ofv = $sampled_params_arr->[$index_lowest_ofv]->{'ofv'};
	for (my $i=1; $i<scalar(@{$sampled_params_arr}); $i++){
		if ($sampled_params_arr->[$i]->{'ofv'} < $lowest_ofv){
			$index_lowest_ofv=$i;
			$lowest_ofv = $sampled_params_arr->[$index_lowest_ofv]->{'ofv'};
		}
	}
#	print "\nLowest ofv $lowest_ofv at index $index_lowest_ofv\n";
	#modify $parameter_hash->{'values'}, match on $parameter_hash->{'labels'} 

	my ($new_center,$errors) = get_vector_from_sampled_params_arr(sampled_params_arr => $sampled_params_arr,
																  parameter_hash => $parameter_hash,
																  index => $index_lowest_ofv);

	$parameter_hash->{'values'} = $new_center;
	return ($errors,$lowest_ofv);
}

sub get_vector_from_sampled_params_arr
{
	#static, no shift
	my %parm = validated_hash(\@_,
							  sampled_params_arr => { isa => 'ArrayRef', optional => 0 },
							  parameter_hash => { isa => 'HashRef', optional => 0 },
							  index => {isa => 'Int', optional => 0}
		);
	my $sampled_params_arr = $parm{'sampled_params_arr'};
	my $parameter_hash = $parm{'parameter_hash'};
	my $index = $parm{'index'};

	my @vector = ();
	my @errors = ();

	for (my $j=0; $j< scalar(@{$parameter_hash->{'labels'}}); $j++) {
		my $parameter = $parameter_hash->{'param'}->[$j];
		my $label = $parameter_hash->{'labels'}->[$j];
		if (defined $sampled_params_arr->[$index]->{$parameter}->{$label}){
			push(@vector,$sampled_params_arr->[$index]->{$parameter}->{$label});
		}else{
			push(@errors,"index $index parameter $parameter label $label undefined in sampled_params_arr");
		}
	}
	return (\@vector,\@errors);

}
sub mvnpdf{
	#Note: this sub is not used in current sir procedure

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

sub check_matrix_posdef{
	my %parm = validated_hash(\@_,
							  matrix => { isa => 'ArrayRef', optional => 0 },
		);
	my $matrix = $parm{'matrix'};

	my $dim = scalar(@{$matrix});
	#copy and check it
	my @array=();
	#this gives full matrix
	for (my $row=0; $row<$dim;$row++){
		push(@array,[0 x $dim]);
		for (my $col=0;$col<$dim;$col++){
			$array[$row]->[$col]=$matrix->[$row]->[$col];
		}
	}
	return linear_algebra::cholesky(\@array);

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
							  labels_hash => { isa => 'HashRef', optional => 0 },
							  do_percentiles => {isa => 'Bool', optional => 1, default => 1},
							  get_lambda_delta => {isa => 'Bool', optional => 1, default => 0},
							  absmaxlambda => {isa => 'Num', optional => 1, default => 3},
							  resolutionlambda => {isa => 'Num', optional => 1, default => 0.2},
							  estimated_vector => { isa => 'ArrayRef', optional => 1 },
		);
	my $sampled_params_arr = $parm{'sampled_params_arr'};
	my $labels_hash = $parm{'labels_hash'};
	my $do_percentiles = $parm{'do_percentiles'};
	my $get_lambda_delta = $parm{'get_lambda_delta'};
	my $absmaxlambda = $parm{'absmaxlambda'};
	my $resolutionlambda = $parm{'resolutionlambda'};
	my $estimated_vector = $parm{'estimated_vector'};

	my $len = scalar(@{$sampled_params_arr});
	croak("empty set of samples to empirical_statistics") unless ($len >0);
	my $dim = scalar(@{$labels_hash->{'labels'}});
	croak("empty set of labels to empirical_statistics") unless ($dim >0);

	my %resulthash;

	if ($get_lambda_delta and  ($len < $dim)){
		$resulthash{'rank_deficient'}=1;
	}
	my $count_resamples=1;
	$count_resamples = 0 unless (defined $sampled_params_arr->[0]->{'resamples'}); #assume all included

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

		my $max_k=1;
		if ($count_resamples){
			$max_k=$sampled_params_arr->[$i]->{'resamples'};
		}
		for (my $k=0; $k< $max_k; $k++){
			$n_resamples++;
			push(@Amatrix,\@vector);
			for (my $j=0; $j< $dim; $j++){
				push(@{$parameter_vectors[$j]},$vector[$j]);
				$sums[$j] = $sums[$j] + $vector[$j] unless ($get_lambda_delta);
			}
		}
	}
	croak("Number of resamples is 0 in empirical_statistics") if ($n_resamples < 1);

	$resulthash{'covar'}=[];
	if ($get_lambda_delta){
		$resulthash{'lambda'}=[];
		$resulthash{'delta'}=[];
		my @Bmatrix=();
		for (my $j=0; $j< $dim; $j++){
			$sums[$j] =0;
			my ($lam,$del) = boxcox::get_lambda_delta($parameter_vectors[$j],$absmaxlambda,$estimated_vector->[$j]);
			if (abs($lam-1)<$resolutionlambda){
				push(@{$resulthash{'lambda'}},undef);
				push(@{$resulthash{'delta'}},0);
			}else{
				push(@{$resulthash{'lambda'}},$lam);
				push(@{$resulthash{'delta'}},$del);
			}
		}
		#redo sums and Bmatrix
		foreach my $vec (@Amatrix){
			my $trans = boxcox::shift_and_box_cox(vector=>$vec,
												  lambda => $resulthash{'lambda'},
												  delta=>$resulthash{'delta'},
												  inverse=>0);
			push(@Bmatrix,$trans);
			for (my $j=0; $j< $dim; $j++){
				$sums[$j] = $sums[$j] + $trans->[$j];
			}
		}

		my $err1 = linear_algebra::row_cov(\@Bmatrix,$resulthash{'covar'});
		if ($err1 == 1){
			croak ("numerical error in linear_algebra rowcov for boxcox");
		}elsif ($err1 == 2){
			croak ("input error to linear_algebra rowcov for boxcox");
		}

	}else{
		my $err1 = linear_algebra::row_cov(\@Amatrix,$resulthash{'covar'});
		if ($err1 == 1){
			croak ("numerical error in linear_algebra rowcov");
		}elsif ($err1 == 2){
			croak ("input error to linear_algebra rowcov");
		}
	}

	$resulthash{'mean'}=[];
 	for (my $j=0; $j< $dim; $j++){
		push(@{$resulthash{'mean'}},($sums[$j]/$n_resamples));
	}


	if ($do_percentiles){
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

		$resulthash{'percentiles_labels'}=\@perc_limit;
		$resulthash{'percentiles_values'}=[];
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
		#se and rse
		$resulthash{'standard_error'}=[];
		$resulthash{'relative_standard_error'}=[];
		$resulthash{'rse_sd_scale'}=[];
		for (my $j=0; $j< $dim; $j++){
			push(@{$resulthash{'standard_error'}},sem($parameter_vectors[$j]));
			push(@{$resulthash{'relative_standard_error'}},rse($parameter_vectors[$j],$labels_hash->{'values'}->[$j]));
			if ($labels_hash->{'param'}->[$j] eq 'theta'){
				push(@{$resulthash{'rse_sd_scale'}},'');
			}else{
				push(@{$resulthash{'rse_sd_scale'}},rse($parameter_vectors[$j],$labels_hash->{'values'}->[$j])/2);
			}
		}

	}
	return \%resulthash;
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
#							  multivariate_normal  => { isa => 'Bool', optional => 1, default => 1},
							  covmatrix => { isa => 'ArrayRef[ArrayRef]', optional => 0 }, #required for multnorm
#							  bootstrap_samples => { isa => 'ArrayRef', optional => 1 }, #required for uniform
							  lower_bound => { isa => 'ArrayRef', optional => 0 },
							  upper_bound => { isa => 'ArrayRef', optional => 0 },
							  param => { isa => 'ArrayRef', optional => 0 },
							  block_number => { isa => 'ArrayRef', optional => 0 },
							  coords => { isa => 'ArrayRef', optional => 0 },
							  mu => { isa => 'Math::MatrixReal', optional => 0 }, #required for multnorm							  
							  inflation => { isa => 'Num', optional => 0 }, #required for multnorm
#							  degree => { isa => 'Num', optional => 1 }, #required for uniform
							  lambda => { isa => 'ArrayRef', optional => 1 }, #not allowed for uniform?
							  delta => { isa => 'ArrayRef', optional => 1 }, #not allowed for uniform?
		);
	my $samples = $parm{'samples'};
#	my $multivariate_normal = $parm{'multivariate_normal'};
	my $covmatrix = $parm{'covmatrix'};
#	my $bootstrap_samples = $parm{'bootstrap_samples'};
	my $lower_bound = $parm{'lower_bound'};
	my $upper_bound = $parm{'upper_bound'};
	my $param = $parm{'param'};
	my $block_number = $parm{'block_number'};
	my $coords = $parm{'coords'};
	my $mu = $parm{'mu'};
	my $inflation = $parm{'inflation'};
#	my $degree = $parm{'degree'};
	my $lambda = $parm{'lambda'};
	my $delta = $parm{'delta'};

	my $dim = scalar(@{$coords});

	unless ($dim>0){
		croak("Input error sample_multivariate_normal: coords has dimension $dim");
	}

	my $transform=0;
	if (defined $lambda){
		if (scalar(@{$lambda})>0){
			unless(scalar(@{$lambda})==$dim){
				croak("Input error sample_multivariate_normal: coords vector has dimension $dim but lambda has dimension ".scalar(@{$lambda}));
			}
			unless(scalar(@{$delta})==$dim){
				croak("Input error sample_multivariate_normal: coords vector has dimension $dim but delta has dimension ".scalar(@{$delta}));
			}
			$transform=1;
		}
	} 
	unless (scalar(@{$lower_bound})==$dim){
		croak("Input error sample_multivariate_normal: coords vector has dimension $dim but lower_bound has dimension ".scalar(@{$lower_bound}));
	}
	unless (scalar(@{$upper_bound})==$dim){
		croak("Input error sample_multivariate_normal: coords vector has dimension $dim but upper_bound has dimension ".scalar(@{$upper_bound}));
	}
	unless (scalar(@{$block_number})==$dim){
		croak("Input error sample_multivariate_normal: coords vector has dimension $dim but block_number has dimension ".scalar(@{$block_number}));
	}
	unless (scalar(@{$param})==$dim){
		croak("Input error sample_multivariate_normal: coords vector has dimension $dim but param has dimension ".scalar(@{$param}));
	}
	unless (scalar($samples)>0){
		croak("Input error sample_multivariate_normal: samples must be larger than 0");
	}

	my @muvec=();
	my $use_covmatrix;
#	if ($multivariate_normal){
	unless (defined $covmatrix ){
		croak("Input error sample_multivariate_normal: covmatrix is required for multivariate normal");
	}
	unless (defined $mu ){
		croak("Input error sample_multivariate_normal: mu is required for multivariate normal");
	}
	unless (defined $inflation){
		croak("inflation parameter is required for multivariate normal");
	}

	my ($rows,$columns) = $mu->dim();
	unless ($rows == 1 and ($columns == $dim)){
		croak("Input error sample_multivariate_normal: mu vector has dimension ($rows,$columns)");
	}
	for (my $i=1; $i<= $dim; $i++){
		push(@muvec,$mu->element(1,$i));
	}
	
	unless (scalar(@{$covmatrix})==$dim){
		croak("Input error sample_multivariate_normal: coords vector has dimension $dim but covmatrix has dimension ".scalar(@{$covmatrix}));
	}
	unless (scalar(@{$covmatrix->[0]})==$dim){
		croak("Input error sample_multivariate_normal: covmatrix is not square ");
	}
	if ($inflation != 1){
		$use_covmatrix = inflate_covmatrix(matrix => $covmatrix,
										   inflation => $inflation);
	}else{
		$use_covmatrix = $covmatrix;
	}
#	}else{
		#uniform
#		unless (defined $bootstrap_samples ){
#			croak("Input error sample_multivariate_normal: bootstrap_samples is required for uniform");
#		}
#		unless (defined $degree ){
#			croak("Input error sample_multivariate_normal: degree is required for uniform");
#		}
		#TODO rawres_hash_array-> array of vectors in plain format
#	}
	



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
	my @boxcox_samples_array=();
	my $counter=0;
	my $max_iter=1000;
	my $discarded=0;

	for (my $j=0; $j<$max_iter; $j++){
		#we will probably discard some samples, generate twice needed amount to start with
		my @candidate_samples;
#		if ($multivariate_normal){
		@candidate_samples = Math::Random::random_multivariate_normal((2*$samples), @muvec, @{$use_covmatrix});
#		}

		for (my $cand=0; $cand < scalar(@candidate_samples); $cand++){
			my $xvec;
			my $accept = 1;
			if ($transform){
				#back-transform from boxcox
				$xvec = boxcox::shift_and_box_cox(vector=>$candidate_samples[$cand],
												  lambda=>$lambda,
												  delta=>$delta, 
												  inverse => 1);
				for (my $i=0; $i< $dim; $i++){
					unless (usable_number($xvec->[$i])){
						$accept=0;
						last;
					}
				}
			}else{
				$xvec= $candidate_samples[$cand];
			}
			unless ($accept){
				$discarded++;
				next ;
			}

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
			unless ($accept){
				$discarded++;
				next ;
			}
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
			unless ($accept){
				$discarded++;
				next ;
			}
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
			unless ($accept){
				$discarded++;
				next ;
			}
			push(@samples_array,$xvec);
			push(@boxcox_samples_array,$candidate_samples[$cand]) if ($transform);
			$counter++;
			last if ($counter == $samples);
		}
		last if ($counter == $samples);
	}
	
	unless ($counter == $samples){
		croak("Failed to generate $samples accepted parameter vectors within the boundaries even after generating ".(2*$max_iter*$samples)." candidates");
	}
	ui->print(category => 'sir',message=> "Resimulated $discarded samples that did not fulfill boundary conditions.\n");
	return (\@samples_array,\@boxcox_samples_array);

}

sub tweak_inits_sampling{
	#static, no shift
	my %parm = validated_hash(\@_,
							  sampled_params_arr => { isa => 'ArrayRef', optional => 0 }, #can be empty
							  parameter_hash => { isa => 'HashRef', optional => 0 },
							  model => {isa => 'model', optional => 0},
							  output => {isa => 'output', optional => 1},
							  degree => {isa => 'Maybe[Num]', optional => 1},
							  filename => {isa => 'Str', optional => 1},
							  directory => {isa => 'Str', optional => 1},
		);
	my $sampled_params_arr = $parm{'sampled_params_arr'};
	my $parameter_hash = $parm{'parameter_hash'};
	my $model = $parm{'model'};
	my $output = $parm{'output'};
	my $degree = $parm{'degree'};
	my $filename = $parm{'filename'};
	my $directory = $parm{'directory'};

	my $default_degree=0.1;

	my @Amatrix=();
	my $nparam = scalar(@{$parameter_hash->{'values'}});
	
	my $referencemodel = $model->copy( filename => 'referencefortweakinits.mod',
								  write_copy => 0,
								  copy_output => 0);
	my $tweakmodel = $model->copy( filename => 'dummyfortweakinits.mod',
								   write_copy => 0,
								   copy_output => 0);
	my $minimum_samples = $nparam;
	if (scalar(@{$sampled_params_arr}) == 0){
		$minimum_samples = $nparam+2;
		#we have nothing but estimates of intput model
		push(@Amatrix,$parameter_hash->{'values'});
		unless (defined $output and (defined $degree)){
			croak("tweak_inits_sampling: output and degree required when do not have sampled array");
		}
		$referencemodel->update_inits( from_output => $output,
								  ensure_posdef => 0,
								  ignore_missing_parameters => 0,
								  update_fix => 0);
	}else{
		my @problem_lines=();
		my $linesarray = $referencemodel->problems->[0]->_format_problem(relative_data_path => 1,
																	write_directory => $referencemodel->directory);
		#we cannot use this array directly, must make sure items do not contain line breaks
		foreach my $line (@{$linesarray}){
			my @arr = split(/\n/,$line);
			push(@problem_lines,@arr);
		}
		unless (defined $degree){
			$degree = $default_degree;
		}

		for (my $i=0; $i<scalar(@{$sampled_params_arr}); $i++){
			my ($vector,$errors) = get_vector_from_sampled_params_arr(sampled_params_arr => $sampled_params_arr,
																	  parameter_hash => $parameter_hash,
																	  index => $i);
			push(@Amatrix,$vector);
			unless ($i==0){
				#add problem and then update inits
				push(@{$referencemodel->problems()},
					 model::problem ->
					 new ( directory                   => $referencemodel->directory,
						   ignore_missing_files        => 1,
						   ignore_missing_output_files => 1,
						   prob_arr                    => \@problem_lines));
			}

			$referencemodel -> update_inits(from_hash => $sampled_params_arr->[$i],
											problem_number => ($i+1),
											ignore_missing_parameters => 0,
											match_labels => 1); 
		}
	}

	my $indexmax = (scalar(@{$referencemodel->problems})-1);

	my $done = 0;
	my $full_rank=0;
	$done = $full_rank;
	my $problem_index =0;
	while (not $done){
		my $any = $tweakmodel->problems->[0]->set_random_inits(degree => $degree,
															   basic_model => $referencemodel,
															   problem_index => $problem_index);
		$problem_index++;
		$problem_index = 0 if ($problem_index > $indexmax);

		$tweakmodel->problems->[0]->reset_estimated_parameters_hash();
		push(@Amatrix,$tweakmodel->problems->[0]->get_estimated_attributes(parameter => 'all',attribute => 'inits'));
		next if (scalar(@Amatrix)<($minimum_samples));
		$done=1 if (scalar(@Amatrix)> (100*$nparam)); #to avoid inf loop
		$full_rank = linear_algebra::full_rank(\@Amatrix);
		$done = 1 if ($full_rank);
	}

	#create  rawres
	if (defined $filename and (defined $directory)){
		open ( RES, ">" . $directory.'/'.$filename );
		print RES 'model,"'.join('","',@{$parameter_hash->{'coordinate_strings'}}).'"'."\n";
		for (my $i=0;$i < scalar(@Amatrix); $i++){
			print RES join(',',@{$Amatrix[$i]})."\n";
		}
		close RES;
	}

	return \@Amatrix;
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
		croak("Trying get_nonmem_inverse_covmatrix but the matrix is undefined. Parsing error? Output file is\n".
			  $output->full_name."\n");
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

	if (defined $self->tools and defined $self->tools()->[0]){
		$self->full_rawres_header($self->tools()->[0]->raw_results_header);
	}

}




sub _modelfit_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 0 }
		);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	my ($dir,$file,$nonp_file); 

	my $reference_ofv = $self->reference_ofv();
	my $pdf_vector = $self->pdf_vector();
	my $attempted_samples = $self->attempted_samples()->[($self->iteration-1)];
	
	my $iteration = $self->iteration;
	my $final_iteration = 0;
	$final_iteration = 1 if ($self->iteration == $self->max_iteration);
	($dir,$file)= 
		OSspecific::absolute_path( $self ->directory(),
								   $self -> raw_results_file()->[$model_number-1] );
	($dir,$nonp_file) = 
		OSspecific::absolute_path( $self ->directory(),
								   $self -> raw_nonp_file()->[$model_number-1] );

	my $with_replacement = $self->with_replacement();
	my $problems_per_file = $self->problems_per_file();

	my $orig_mod = $self ->models()->[$model_number-1];


	$subroutine = sub {
		my $modelfit = shift;
		my $mh_ref   = shift;
		my %max_hash = %{$mh_ref};
		if ($final_iteration){
			$modelfit -> raw_results_file([$dir.$file] );
			$modelfit -> raw_nonp_file( [$dir.$nonp_file] );
		}else{
			#leave as is
		}
#		$self->raw_line_structure($modelfit -> raw_line_structure());

		#make sure that we have valid raw_line_structure and not from crashed run here
		my $structure;
		for (my $i=1;$i <= scalar(keys %{$modelfit->raw_line_structure()}); $i++){
			if (defined $modelfit->raw_line_structure()->{$i} and defined $modelfit->raw_line_structure()->{$i}->{'ofv'}){
				$structure = $i; 
				last;
			}else{
				#print "rawline $i not ok!\n";
			}
		}
		croak("could not find defined ofv in raw_line_structure") unless (defined $structure);

		my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$structure}->{'model'});
		my $modelindex = $start;
		my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$structure}->{'problem'});
		my $probindex = $start;
		my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$structure}->{'subproblem'});
		my $subindex = $start;
		my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$structure}->{'ofv'});
		my $ofvindex=$start;
		croak("could not find ofv in raw results header") unless (defined $ofvindex);

		my @delta_ofv=();
		my @filtered_pdf=();
		my $index = 0;
		my $successful_count=0;
		my $negative_count = 0;
		my $this_minimum_ofv = 1000000;
		#new filtered_pdf for rows actually in raw_results_file

		foreach my $row ( @{$modelfit -> raw_results()} ) {
			#compute index in original pdf vector based on model and problem column
			#index is (model-1)*problems_per_file + (problem-1)
			#push this pdf to filtered_pdf
			my $delta_ofv;
			if (defined $row->[$ofvindex]){
				$delta_ofv = $row->[$ofvindex] - $reference_ofv;
				$successful_count++;
				if ($delta_ofv < 0){
					$negative_count++;
				}
				if ($row->[$ofvindex] < $this_minimum_ofv){
					$this_minimum_ofv = $row->[$ofvindex]; 
				}
			}else{
				$delta_ofv = undef;
			}
			my $pdf;
			if (defined $row->[$modelindex] and defined $row->[$probindex] and
				($row->[$modelindex]>0) and  ($row->[$probindex]>0)){
				my $tmp =($row->[$modelindex]-1)*$problems_per_file + ($row->[$probindex]-1);
				if ($tmp > scalar(@{$pdf_vector})){
					croak("computed index $tmp based on model ".$row->[$modelindex]." problem ".$row->[$probindex].
						  " and problems_per_file $problems_per_file, ".
						  " but pdf_vector has length ".scalar(@{$pdf_vector}));
				}
				$pdf = $pdf_vector->[$tmp];
			}else{
				$pdf = undef;
			}
			push(@filtered_pdf,$pdf);
			push(@delta_ofv,$delta_ofv);
			$index++;
		}

		push(@{$self->minimum_ofv},$this_minimum_ofv);
		push(@{$self->negative_dofv},$negative_count);
		my $current_resamples = tool::sir::update_actual_resamples(samples=>$self->samples,
																   resamples=>$self->resamples,
																   successful_samples=>$self->successful_samples,
																   actual_resamples=>$self->actual_resamples,
																   successful_count=>$successful_count,
																   iteration=> $iteration);
		
		if ($successful_count == $attempted_samples){
			#check that original pdf and filtered_pdf are the same
			my $mismatch=0;
			for (my $j=0; $j < $attempted_samples; $j++){
				unless ($filtered_pdf[$j] == $pdf_vector->[$j]){
					$mismatch++;
					ui->print(category => 'sir',
							  message => "mismatch in pdf $j, filtered is ".$filtered_pdf[$j].
							  " but original is ".$pdf_vector->[$j]);
				}
			}
#			print "mismatch count $mismatch ok $ok\n";
		}else{
			unless ($with_replacement){
				#almost always true
				my $original_resamples = $self->resamples->[$iteration-1];
				unless ($current_resamples == $original_resamples){
					my $message;
					if ($current_resamples < $original_resamples){
						$message = "\nOnly $successful_count samples gave an ofv-value.\n".
							  "Some \$PROBLEMs must have terminated with error, check lst-files in\n".
							  $self->directory."m1\n";
					}else{
						$message = "\n $successful_count samples gave an ofv-value.\n".
							"This is more than expected given previous losses\n";
					}
					$message .= "Resetting resamples from $original_resamples to $current_resamples in this iteration so ".
						"that the samples/resamples ratio set on commandline is maintained ";
					ui->print(category => 'sir', message => $message);
				}

			}
		}

#		unless (scalar(@delta_ofv) == $self->samples()->[($iteration-1)]){
#			ui->print(category => 'sir',
#					  message => "It seems some runs crashed, only have ".scalar(@delta_ofv).
#					  " sample lines in raw_results.\nContinuing anyway.\n");
#		}
		
		my $wghash = tool::sir::compute_weights(pdf_array => \@filtered_pdf,
												dofv_array => \@delta_ofv);

		my @original_weights = @{$wghash->{'weights'}};
		my $total_weights = $wghash->{'sum_weights'};
		my @times_sampled = (0) x scalar(@delta_ofv); #filtered_samples length filtered instead

		ui -> print( category => 'sir', message => "Resampling vectors based on weights" );
		for (my $i=0; $i<$current_resamples; $i++){
			my $sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
			$times_sampled[$sample_index]++;
			unless ($with_replacement or $i==$current_resamples){
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
		$raw_results_row->[0]->[0] = 'input'; #model column
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
		foreach my $mod (sort({$a <=> $b} keys %{$modelfit->raw_line_structure()})){
			foreach my $category (keys %{$modelfit->raw_line_structure() -> {$mod}}){
				next if ($category eq 'line_numbers');
				my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$mod}->{$category});
				if ($start > $ofvindex){
					$modelfit->raw_line_structure() -> {$mod}->{$category} = ($start+$extra_header_count+1).','.$len;#+1 extra for sample.id
				}else{
					$modelfit->raw_line_structure() -> {$mod}->{$category} = ($start+1).','.$len; #+1 for sample id
				}
			}
			my $hc = 0;
			$modelfit->raw_line_structure() -> {$mod}->{'sample.id'} = ($hc).',1'; #sample has index 0
			$hc++;
			foreach my $head (@extra_headers){
				$hc++; #first will be 2
				$modelfit->raw_line_structure() -> {$mod}->{$head} = ($ofvindex+$hc).',1'; #first has 2 extra relative old ofvindex
				#			$modelfit->raw_line_structure() -> {$mod}->{'deltaofv'} = ($ofvindex+1).',1';
			}
		}
		
		if ($negative_count > 0) {
			my $message ="\nFound ".$negative_count.
				" samples with negative delta-ofv, this means that ".
				"the model was not at the global minimum. Details in raw results file\n".
				$modelfit -> raw_results_file->[0];
			ui->print(category => 'sir', message => $message);
		}
		if ($final_iteration){
			$self->raw_line_structure($modelfit -> raw_line_structure());
			$self->raw_line_structure() -> write( $dir.'raw_results_structure' );
			
			$self -> raw_results_header($modelfit -> raw_results_header());
			$self -> raw_results($modelfit -> raw_results());
		}else{
			my ($dir,$file) = 	OSspecific::absolute_path( $modelfit ->directory(),
														   $modelfit -> raw_results_file()->[$model_number-1] );
			$modelfit->raw_line_structure() -> write( $dir.'raw_results_structure' );

		}
	};
	return $subroutine;
}

sub update_actual_resamples
{
	#static
	my %parm = validated_hash(\@_,
							  samples => { isa => 'ArrayRef', optional => 0 },
							  resamples => { isa => 'ArrayRef', optional => 0 },
							  successful_samples => { isa => 'ArrayRef', optional => 0 },
							  actual_resamples => { isa => 'ArrayRef', optional => 0 },
							  iteration => { isa => 'Int', optional => 0 },
							  successful_count => { isa => 'Int', optional => 0 },
		);
	my $samples = $parm{'samples'};
	my $resamples = $parm{'resamples'};
	my $successful_samples = $parm{'successful_samples'};
	my $actual_resamples = $parm{'actual_resamples'};
	my $iteration = $parm{'iteration'};
	my $successful_count = $parm{'successful_count'};

	push(@{$successful_samples},$successful_count);

	my $new_resamples = $resamples->[($iteration-1)];

	my $turnout = $successful_count/($samples->[($iteration-1)]);
	#turnout can be both larger and smaller than 1
	if (abs($turnout-1) >= 0.05){ # at least 5 % loss or gain relative original request
		$new_resamples = round($resamples->[($iteration-1)]*$turnout);
	}
	push(@{$actual_resamples},$new_resamples);

	return $new_resamples;
}

sub update_attempted_samples
{
	#static
	my %parm = validated_hash(\@_,
							  samples => { isa => 'ArrayRef', optional => 0 },
							  successful_samples => { isa => 'ArrayRef', optional => 0 },
							  attempted_samples => { isa => 'ArrayRef', optional => 0 },
							  iteration => { isa => 'Int', optional => 0 },
		);
	my $samples = $parm{'samples'};
	my $successful_samples = $parm{'successful_samples'};
	my $attempted_samples = $parm{'attempted_samples'};
	my $iteration = $parm{'iteration'};

	my $new_samples = $samples->[($iteration-1)];
	if ($iteration > 1){
		my $previous_turnout = ($successful_samples->[($iteration-2)])/($attempted_samples->[($iteration-2)]);
		#previous turnout cannot be larger than 1, then bug
		if ($previous_turnout > 1){
			croak("this is a bug, successful samples ".$successful_samples->[($iteration-2)].
				" iteration ".($iteration-1)." larger than attempted samples ".$attempted_samples->[($iteration-2)]);
		}
		if ($previous_turnout <= 0.95){ # at least 5 % loss
			$new_samples = round($samples->[($iteration-1)]/$previous_turnout);
		}
	}
	
	push(@{$attempted_samples},$new_samples);
	return $new_samples;
}

sub prepare_results
{
	my $self = shift;
	ui -> print( category => 'sir',
				 message => "\nAnalyzing results");

	my $rawresfile;
	if ($self->recompute()){
		#change results_file so that old is not overwritten
		my $fname = $self->results_file();
		$fname =~ s/\.csv$/_recompute/ ;
		my $addnum=1;
		while (-e $self -> directory."/$fname$addnum".'.csv'){
			$addnum++;
		}
		$self->results_file("$fname$addnum".'.csv');
	}

#	print "prepare results model name ".$self -> models -> [0]->full_name."\n";
#	print "prepare results rawres name ".$self->raw_results_file()->[0]."\n";

	my $iterationfile = $self -> directory."/summary_iterations.csv";
	open ( RES, ">" . $iterationfile );
	print RES "iteration,commandline.samples,attempted.samples,successful.samples,".
		"commandline.resamples,actual.resamples,requested.ratio,actual.ratio,negative.dOFV,minimum.sample.ofv\n";
	if (defined $self->rawres_input){
		print RES "0, , , , ,".$self->rawres_samples().", , , , \n";
	}

	for (my $i=0; $i<$self->max_iteration; $i++){
		print RES ($i+1).','.$self->samples->[$i].','.$self->attempted_samples->[$i].','.$self->successful_samples->[$i].','.
			$self->resamples->[$i].','.$self->actual_resamples->[$i].','.($self->samples->[$i]/$self->resamples->[$i]).
			','.($self->successful_samples->[$i]/$self->actual_resamples->[$i]).
			','.$self->negative_dofv->[$i].','.$self->minimum_ofv->[$i]."\n";
	}
	close(RES);


	my $model = $self -> models -> [0];
	my ($sampled_params_arr,$href) = model::get_rawres_params(filename => $self->raw_results_file()->[0],
															  require_numeric_ofv => 1,
															  extra_columns => ['resamples'],
															  offset => 1,
															  model => $model); 
	my $len= 0;
	$len = scalar(@{$sampled_params_arr}) if (defined $sampled_params_arr);
	## Prepare general run info for output file
	my %return_section;
	$return_section{'name'} = 'SIR run info';
	my $modelname=$model ->filename();
	$return_section{'labels'} = [[],['Date','model','PsN version','NONMEM version']];

	my @datearr=localtime;
	my $the_date=($datearr[5]+1900).'-'.($datearr[4]+1).'-'.($datearr[3]);
	$return_section{'values'} = [[$the_date,$modelname,'v'.$PsN::version,$self->nm_version]];
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

	my %se_section;
	$se_section{'name'}='Summary statistics over resamples';
	$se_section{'labels'}=[['mean','se','rse','rse_sd'],$parameter_hash->{'labels'}];
	$se_section{'values'}=[$resulthash->{'mean'},$resulthash->{'standard_error'},$resulthash->{'relative_standard_error'},$resulthash->{'rse_sd_scale'}];
	push( @{$self -> results->[0]{'own'}},\%se_section );

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
	print_empirical_covmatrix(filename=> $ldir.$name, parameter_hash => $parameter_hash, covar => $resulthash->{'covar'});

}

sub print_empirical_covmatrix
{
	my %parm = validated_hash(\@_,
							  filename => { isa => 'Str', optional => 0 },
							  parameter_hash => { isa => 'HashRef', optional => 0 },
							  covar => { isa => 'ArrayRef', optional => 0 },
		);
	my $filename = $parm{'filename'};
	my $parameter_hash = $parm{'parameter_hash'};
	my $covar = $parm{'covar'};

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
	my $copy = linear_algebra::copy_and_reorder_square_matrix($covar,\@order);
	my $formatted = format_covmatrix(matrix => $copy, header => \@coords, comma => 0, print_labels => 1);
	open ( RES, ">" . $filename );
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


sub create_R_plots_code{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  rplot => { isa => 'rplots', optional => 0 }
		);
	my $rplot = $parm{'rplot'};

	my @cols=();
	my $paramcount = 0;

	my $maxresamplestring = 'MAX.RESAMPLE <- 1    # maximum resamples for single sample. 1 if without replacement';
	if ($self->with_replacement){
		$maxresamplestring = 'MAX.RESAMPLE <- '.$self->resamples->[($self->max_iteration-1)];
	}

	my $labelref = $self->models->[0]->problems->[0]->get_estimated_attributes(parameter => 'all',
																		  attribute => 'labels');
	#TODO filter out off-diagonals like in sse? 

	if (defined $labelref){
		$paramcount = scalar(@{$labelref});
		#cannot use raw_results_header, has only placeholders for sigma etc
		my $headerref = $self->full_rawres_header; #ref of array of strings, no quotes
#		print join(' ',@{$headerref})."\n";
		@cols = @{get_array_positions(target => $headerref,keys => $labelref)};
	}


	my $colstring = 'COL.ESTIMATED.PARAMS <-  c('.join(',',@cols).')   #column numbers in raw_results for parameters';
	my $paramstring = 'N.ESTIMATED.PARAMS <- '.$paramcount;
	my $cistring = 'CI <- 95';

	$rplot->add_preamble(code => [
							 '#sir-specific preamble',
							 'SAMPLES          <-'.$self->samples->[($self->max_iteration-1)],
							 'RESAMPLES        <-'.$self->resamples->[($self->max_iteration-1)],
							 'ALL.SAMPLES      <-c('.join(',',@{$self->samples}).')',
							 'ALL.RESAMPLES    <-c('.join(',',@{$self->resamples}).')',
							 "ALL.RAWRESFILES   <-c('".join("','",@{$self->intermediate_raw_results_files})."')",
							 $maxresamplestring,
							 $colstring,
							 $paramstring,
							 $cistring,
						 ]);

}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
