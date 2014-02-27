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

extends 'tool';

has 'sir_raw_results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'samples' => ( is => 'rw', required => 1, isa => 'Int' );
has 'base_model' => ( is => 'rw', isa => 'model' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['sirlog.csv'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'sir_results.csv' );
has 'reference_column' => ( is => 'rw', isa => 'Str' );


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

	#make sure IGNORE=C is not used
	my @ignores = $this->models->[0]->get_option_value(record_name=>'data', 
		option_name=>'IGNORE',
		problem_index=>0, 
		record_index=>0,
		option_index=>'all');

	foreach my $ig (@ignores){
		croak("PsN sir cannot handle IGNORE=C. Use IGNORE=@ instead\n")
		if ($ig eq 'C');
	}



	croak("Number of samples must be larger than 0") unless ($this->samples()>0);
}

sub mvnpdf{
	my %parm = validated_hash(\@_,
							  inverse_covmatrix => { isa => 'Math::MatrixReal', optional => 0 },
							  mu => { isa => 'Math::MatrixReal', optional => 0 },
							  xvec_array => { isa => 'ArrayRef[Math::MatrixReal]', optional => 0 }
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
		($rows,$columns) = $xvec->dim();
		unless ($rows == 1 and $columns == $k){
			croak("Input error mvnpdf: xvec should have dimension (1,$k) but has dimension ($rows,$columns)");
		}
		$delta->subtract($xvec,$mu); #now $delta is $xvec - $mu
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

	my @weights=();
	for (my $i=0; $i< $len; $i++){
		#we check in mvnpdf that weight is not zero??
		if ($pdf_array->[$i] > 0){
			push(@weights,exp(-0.5*($dofv_array->[$i]))/($pdf_array->[$i]));
		}else{
			push(@weights,$bignum);
		}
	}
	return \@weights;
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
	my @records;
	if (defined $init_problem -> thetas()) {
		@records = @{$init_problem -> thetas()};
	}
	croak("No thetas in model in output file") unless (scalar(@records) > 0); #no parameter in this problem

	my @values=();
	my @lower_bounds=();
	my @upper_bounds=();
	my @names=();

	my $thetacoordval = $output -> get_single_value(attribute => 'thetacoordval'); #ref to a hash
	croak("No thetacoordval in output object") unless (defined $thetacoordval);

	foreach my $record (@records){
		if  ($record->same() or $record->fix() or $record->prior()) {
			next;
		}
		unless (defined $record -> options()) {
			croak("theta record has no values in get_nonmem_parameters in output object");
		}
		foreach my $option (@{$record -> options()}) {
			if ($option->fix() or $option->prior()) {
				next;
			}
			my $coord = $option -> coordinate_string();
			my $name = $coord;
			if (defined $option ->label()) {
				$name = $option ->label();
			}
			push(@names,$name);
			my $lobnd = $option ->lobnd();
			$lobnd = -1000000 unless (defined $lobnd);
			push(@lower_bounds,$lobnd);
			my $upbnd = $option ->upbnd();
			$upbnd = 1000000 unless (defined $upbnd);
			push(@upper_bounds,$upbnd);
			my $value = $thetacoordval->{$coord};
			croak("No estimate for theta $coord") unless (defined $value);
			push(@values,$value);
		}
	}

#$THETA  (0,0.0105) ; CL
#$THETA  (0,1.0500) ; V
#$THETA  (0,0.65)
#$THETA  (0,0.5)
#$THETA  (0,0.2)

	#TODO get also diagonal omegas here, and diagonal sigmas with bounds

	return \@values;
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
			push(@samples_array,$mu->new_from_rows( [$xvec] ));
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


sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	my $model = $self ->models() -> [$model_number-1];

	# Check which models that hasn't been run and run them 

	# ------------------------  Run original run  -------------------------------

	unless ( $model -> is_run and ((not defined $self->base_model) or $self->base_model->is_run) ) {
		my %subargs = ();
		if ( defined $self -> subtool_arguments() ) {
			%subargs = %{$self -> subtool_arguments()};
		}

		if( $self -> nonparametric_etas() or
			$self -> nonparametric_marginals() ) {
			$model -> add_nonparametric_code unless ($model->is_run);
			$self->base_model -> add_nonparametric_code if (defined $self->base_model and not $self->base_model->is_run);
		}
		my @models=();
		my $message = "Executing ";
		unless ($model->is_run){
			push(@models,$model) ;
			$message .= "input model";
		}
		if (defined $self->base_model and not $self->base_model->is_run){
			push(@models,$self->base_model) ;
			if ($model->is_run){
				$message .= "base model";
			}else{
				$message .= "and base model";
			}
		}

		my $orig_fit = tool::modelfit ->
		new( %{common_options::restore_options(@common_options::tool_options)},
			base_directory	 => $self ->directory(),
			directory		 => $self ->directory().
			'/orig_modelfit_dir'.$model_number,
			models		 => \@models,
			threads               => $self->threads,
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
	my $base_output;
	$base_output = $self->base_model -> outputs ->[0] if (defined $self->base_model);
	my $new_mod;
	my @problems   = @{$model -> problems};
	my @new_models;

	if (scalar(@{$model -> datas})>1){
		print "\nWarning: PsN sir only randomizes first data file, seems like model has more than one data file\n";
	}

	my $orig_data = $model -> datas->[0];

	my $done = ( -e $self ->directory()."/m$model_number/done" ) ? 1 : 0;
	my $new_datas;
	if ( not $done ) {
		ui -> print( category => 'sir',
			message  => "Randomizing column ".$self->randomization_column." in ".$orig_data -> filename );

		$new_datas = $orig_data -> randomize_data( directory   => $self ->directory().'/m'.$model_number,
			name_stub   => 'rand',
			samples     => $self->samples(),
			stratify_index => $self->strat_index(), 
			rand_index => $self->rand_index(), 
			equal_obs => (not $self->match_transitions()));

		$self->stop_motion_call(tool=>'sir',message => "Created randomized datasets in ".
			$self ->directory().'m'.$model_number)
		if ($self->stop_motion());

		for ( my $j = 0; $j < $self->samples(); $j++ ) {
			my @data_arr = ($new_datas->[$j]) x scalar(@{$model->problems});

			$new_mod = $model ->  copy( filename    => $self -> directory().'m'.$model_number.'/rand_'.($j+1).'.mod',
				output_same_directory => 1,
				copy_data   => 0,
				copy_output => 0);

			$new_mod->datas(\@data_arr); #sets record and data object. Number of $PROBS and length data_arr must match

			if( $self -> shrinkage() ) {
				$new_mod -> shrinkage_stats( enabled => 1 );
				$new_mod -> shrinkage_modules( $model -> shrinkage_modules );
			}

			if( $self -> nonparametric_etas() or
				$self -> nonparametric_marginals() ) {
				$new_mod -> add_nonparametric_code;
			}

			$new_mod -> update_inits( from_output => $output );
			$new_mod -> _write;

			push( @new_models, $new_mod );
		}
		$self->stop_motion_call(tool=>'sir',message => "Created one modelfile per dataset in ".
			$self ->directory().'m'.$model_number)
		if ($self->stop_motion());

		# Create a checkpoint. Log the samples and individuals.
		open( DONE, ">".$self ->directory()."/m$model_number/done" ) ;
		print DONE "Randomization of ",$orig_data -> filename, " performed\n";
		print DONE $self->samples()." samples\n";
		close( DONE );
	} else {
		ui -> print( category => 'sir',
			message  => "Recreating sir from previous run." );

		# Recreate the datasets and models from a checkpoint
		my ($stored_filename, $stored_samples);
		my ($stored_filename_found, $stored_samples_found);
		open( DONE, $self ->directory()."/m$model_number/done" );
		while( <DONE> ){
			if( /^Randomization of (.+) performed$/ ){
				$stored_filename = $1;
				$stored_filename_found = 1;
				next;
			}
			if( /^(\d+) samples$/ ){
				ui -> print( category => 'sir',
					message  => "Samples saved: $1" );
				$stored_samples = $1;
				$stored_samples_found = 1;
				next;
			}
		}
		close( DONE );
		unless( $stored_filename_found and $stored_samples_found ) {
			croak("The sir/m1/done file could not be parsed.");
		}
		if ( $stored_samples < $self->samples() ) {
			croak("The number of samples saved in previous run ($stored_samples) ".
				"is smaller than the number of samples specified for this run (".
				$self->samples().")" );
		}

		# Reinitiate the model objects
		for ( my $j = 1; $j <= $self->samples(); $j++ ) {
			my ($model_dir, $filename) = OSspecific::absolute_path( $self ->directory().'/m'.$model_number,
				'rand_'.($j+1).'.mod' );

			$new_mod = model ->
			new( directory   => $model_dir,
				filename    => $filename,
				extra_files => $model -> extra_files,
				target      => 'disk',
				ignore_missing_files => 1,
			);
			push( @new_models, $new_mod );
		}
		ui -> print( category => 'sir',
			message  => "Using $stored_samples previously randomized ".
			"data sets sets from $stored_filename" )
	}

	$self -> prepared_models -> [$model_number-1]{'own'} = \@new_models;

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
			models		 => \@new_models,
			threads               => $self->threads,
			directory             => $self ->directory().'/modelfit_dir'.$model_number,
			_raw_results_callback => $self ->
			_modelfit_raw_results_callback( model_number => $model_number ),
			subtools              => \@subtools,
			nmtran_skip_model => 2,
			logfile		 => [$self -> logfile()->[$model_number-1]],
			raw_results           => undef,
			prepared_models       => undef,
			top_tool              => 0,
			%subargs ) );

	$self->stop_motion_call(tool=>'sir',message => "Created a modelfit object to run all the models in ".
		$self ->directory().'m'.$model_number)
	if ($self->stop_motion());
}



sub calculate_delta_ofv
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
}

sub general_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 },
		class => { isa => 'Str', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $class = $parm{'class'};
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

sub modelfit_post_fork_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
}

sub _modelfit_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Use the  raw_results file.
	my ($dir,$file) = 
	OSspecific::absolute_path( $self ->directory(),
		$self -> raw_results_file()->[$model_number-1] );
	my ($dir,$nonp_file) = 
	OSspecific::absolute_path( $self ->directory(),
		$self -> raw_nonp_file()->[$model_number-1] );
	my $orig_mod = $self ->models()->[$model_number-1];
	my $base_mod_ofv;
	my $base_mod;
	if (defined $self->base_model and $self->base_model->is_run){
		$base_mod= $self->base_model;
		$base_mod_ofv=$self->base_model->outputs->[0]->ofv(); #array over problems and subprobs
	}

	$subroutine = sub {
		my $modelfit = shift;
		my $mh_ref   = shift;
		my %max_hash = %{$mh_ref};
		$modelfit -> raw_results_file([$dir.$file] );
		$modelfit -> raw_nonp_file( [$dir.$nonp_file] );

		# The prepare_raw_results in the modelfit will fix the
		# raw_results for each rand sample model, we must add
		# the result for the original model.

		my %dummy;

		my ($raw_results_row, $nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
			model => $orig_mod,
			raw_line_structure => \%dummy );

		my ($base_raw_results_row, $base_nonp_rows);
		if (defined $base_mod){
			($base_raw_results_row, $base_nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
				model => $base_mod,
				raw_line_structure => \%dummy );
		}
		$orig_mod -> outputs -> [0] -> flush;
		$raw_results_row->[0]->[0] = 'input';

		unshift( @{$modelfit -> raw_results()}, @{$raw_results_row} );
		if (defined $base_raw_results_row){
			$base_raw_results_row->[0]->[0] = 'base';
			unshift( @{$modelfit -> raw_results()}, @{$base_raw_results_row} ) ;
		}
		$self->raw_line_structure($modelfit -> raw_line_structure());

		if ( defined $base_mod_ofv ) {
			my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'problem'});
			my $probindex = $start;
			my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'subproblem'});
			my $subindex = $start;
			my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'ofv'});
			my $ofvindex=$start;
			croak("could not find ofv in raw results header") unless (defined $ofvindex);

			foreach my $row ( @{$modelfit -> raw_results()} ) {
				my $delta_ofv = $row->[$ofvindex] - $base_mod_ofv->[($row->[$probindex]-1)]->[($row->[$subindex]-1)];
				my @oldrow =@{$row};
				$row = [@oldrow[0 .. $ofvindex],$delta_ofv,@oldrow[$ofvindex+1 .. $#oldrow]]; 
			}

			my @old_header = @{$modelfit -> raw_results_header()};
			my $headerindex;
			for (my $k=0; $k<scalar(@old_header);$k++){
				$headerindex = $k if ($old_header[$k] eq 'ofv');
			}
			$modelfit -> raw_results_header(
				[@old_header[0 .. $headerindex],'deltaofv',@old_header[$headerindex+1 .. $#old_header]]);

			foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure()})){
				foreach my $category (keys %{$self->raw_line_structure() -> {$mod}}){
					next if ($category eq 'line_numbers');
					my ($start,$len) = split(',',$self->raw_line_structure() -> {$mod}->{$category});
					$self->raw_line_structure() -> {$mod}->{$category} = ($start+1).','.$len
					if ($start > $ofvindex); #+1 for deltaofv
				}
				$self->raw_line_structure() -> {$mod}->{'deltaofv'} = ($ofvindex+1).',1';
			}
		}
		$self->raw_line_structure() -> {'input'} = $self->raw_line_structure() -> {'1'}; #input model
		$self->raw_line_structure() -> {'base'} = $self->raw_line_structure() -> {'1'}; 
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
