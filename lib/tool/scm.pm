package tool::scm;

use include_modules;
use strict;
use Cwd;
use tool::modelfit;
use OSspecific;
use Data::Dumper;
use File::Copy 'cp';
use status_bar;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

use tool::scm::config_file;

has 'config_file' => ( is => 'rw', isa => 'tool::scm::config_file' );
has 'append_log' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'base_criteria_values' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'format' => ( is => 'rw', isa => 'Str' );
has 'main_data_file' => ( is => 'rw', isa => 'Str' );
has 'medians' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'means' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'initial_estimates_model' => ( is => 'rw', isa => 'model' );
has 'derivatives_base_model' => ( is => 'rw', isa => 'model' );
has 'filtered_data_model' => ( is => 'rw', isa => 'model' );
has 'derivatives_output' => ( is => 'rw', isa => 'output' );
has 'update_derivatives' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'max_data_items' => ( is => 'rw', isa => 'Str', default => 50 );
has 'error' => ( is => 'rw', isa => 'Str' );
has 'best_step' => ( is => 'rw', isa => 'Any' );
has 'bounds' => ( is => 'rw', isa => 'HashRef' );
has 'categorical_covariates' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'error_code' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'config_file_name' => ( is => 'rw', isa => 'Str' );
has 'continuous_covariates' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'final_model_directory' => ( is => 'rw', isa => 'Str' );
has 'data_items' => ( is => 'rw', isa => 'Int', default => 0 );
has 'sizes_pd' => ( is => 'rw', isa => 'Int', default => 0 );
has 'covariate_statistics' => ( is => 'rw', isa => 'HashRef' );
has 'global_covariate_statistics' => ( is => 'rw', isa => 'HashRef' );
has 'do_not_drop' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'global_init' => ( is => 'rw', isa => 'Num', default => 0.001 );
has 'gof' => ( is => 'rw', isa => 'Str', default => 'p_value' );
has 'included_relations' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['scmlog.txt'] } );
has 'ofv_change' => ( is => 'rw', isa => 'HashRef' );
has 'ofv_backward' => ( is => 'rw', isa => 'Any' );
has 'p_value' => ( is => 'rw', isa => 'Num' );
has 'p_backward' => ( is => 'rw', isa => 'Any' );
has 'parameters' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'prev_best' => ( is => 'rw', isa => 'Any' );
has 'relations' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'resulting_model' => ( is => 'rw', isa => 'Maybe[model]' );
has 'max_steps' => ( is => 'rw', isa => 'Int' );
has 'linearize' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'basename' => ( is => 'rw', isa => 'Str' );
has 'noabort' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'skip_filtering' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'xv_pred_data' => ( is => 'rw', isa => 'Str' );
has 'xv_results' => ( is => 'rw', isa => 'HashRef' );
has 'xv_results_file' => ( is => 'rw', isa => 'Str' );
has 'epsilon' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'derivatives_data' => ( is => 'rw', isa => 'Str' );
has 'have_Math_CDF' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_run_included' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'run_linearized_base' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'return_after_derivatives_done' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'only_successful' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'parallel_states' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'logit' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'sum_covariates_hash' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'time_varying' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'second_order' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'parameter_eta' => ( is => 'rw', isa => 'HashRef' );
has 'parameter_relation' => ( is => 'rw', isa => 'HashRef' );
has 'foce' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'search_direction' => ( is => 'rw', isa => 'Str' );
has 'both_directions' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'step_number' => ( is => 'rw', isa => 'Int', default => 1 );
has 'step_relations' => ( is => 'rw', isa => 'ArrayRef' );
has 'test_relations' => ( is => 'rw', isa => 'HashRef' );
has 'valid_states' => ( is => 'rw', isa => 'HashRef[ArrayRef]', default => sub { {'continuous' => [1,2,3], 'categorical' => [1,2]}  } );
has 'work_queue' => ( is => 'rw', isa => 'ArrayRef' );
has 'covariate_statistics_file' => ( is => 'rw', isa => 'Str', default => 'covariate_statistics.txt' );
has 'relations_file' => ( is => 'rw', isa => 'Str', default => 'relations.txt' );
has 'short_logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['short_scmlog.txt'] } );

sub BUILD
{
	my $self = shift;
	my %parm = %{$_[0]};

	# <I>test_relations and p_value</I> can be specified
	# as either a reference to a hash or as a reference to an
	# array (for different settings for each model).

	my $do_filtering=0;
	if( defined $self -> config_file_name or defined $self -> config_file ){
		#only true when scm is started
		$self -> read_config_file;

		croak("You need to specify \'models'\ either as argument or in the config file.")
		unless ( defined $self -> models and scalar @{$self -> models} > 0 );

		if (scalar(@{$self -> models->[0]->problems()})>1){
			ui -> print( category => 'all',
				message  => "\nWarning:\n".
				"The scm program has not been tested with models with more than one\$PROBLEM.\n".
				"Check results carefully.",
				newline => 1);
		}
		unless (defined $self->derivatives_data or $self->skip_filtering){
			my @check_list;
			my $ignorelist = $self -> models->[0]-> get_option_value(record_name=>'data',
				option_name=>'IGNORE',
				option_index => 'all');
			push (@check_list,@{$ignorelist}) if (defined $ignorelist);
			my $accept_list = $self -> models->[0]-> get_option_value(record_name=>'data',
				option_name=>'ACCEPT',
				option_index => 'all');
			push (@check_list,@{$accept_list}) if (defined $accept_list);
			foreach my $val (@check_list){
				unless (length($val)==1){
					ui -> print( category => 'scm',
						message  => "\nWarning:\n".
						"IGNORE/ACCEPT statement found in \$DATA.\n".
						"Will try to filter data automatically before computing statistics",
						newline => 1);

					$do_filtering=1;

					last;
				} 
			}
		}
	}

	# This block is a duplicate of the settign of the 'directory' attribute in tool.pm
	# It needs to be here too to make sure that the correct directory is set when
	# resuming an scm.
	if ( defined $parm{'directory'} ) {
		my $dummy;
		my $dir;
		( $dir, $dummy ) = OSspecific::absolute_path( $parm{'directory'}, '');
		$self -> directory($dir);
	}

	if ($self->epsilon == 1) {
		croak("The option -error cannot be used when option -epsilon is set. ")  if
		(defined $self->error);
	}else{
		croak("The option -error must be used when option -epsilon is not set. ")  unless
		(defined $self->error);
	}

	if (defined $self->error) {
		croak("Unknown error form ".$self->error)  unless 
		( $self->error eq 'add' or 
			$self->error eq 'prop' or
			$self->error eq 'propadd' or
			$self->error eq 'exp' or
			$self->error eq 'user');
	}

	if ($self->error eq 'user') {
		unless ( defined $self -> error_code() )  {
			croak("You need to specify \'error_code'\ either as argument or in the config file ".
				"when option -error=user is set." );
		}
	}

	if ($self->step_number == 1 and defined $self->derivatives_data) {
		ui -> print( category => 'scm',
			message  => "Warning: the program will not check the contents of the ".
			"derivatives data file ".$self->derivatives_data.". If columns are ".
			"missing NMtran will fail, and if values are incorrect the scm results ".
			"will be incorrect.",newline => 1);
	}

	$self->have_Math_CDF(1) if eval('require Statistics::Distributions'); #enough, now loaded
	if ( lc($self -> gof()) eq 'p_value' and (not $self->have_Math_CDF())) {
		$self -> gof('ofv');
		ui -> print( category => 'scm',
			message  => "Warning: gof = p_value (the default) cannot be used when ".
			"Perl module Statistics::Distributions is not installed. Changed to gof = ofv",newline => 1);
	}


	if ( not defined $self -> p_value() or
		$self -> p_value() eq '' ) {
		$self -> p_value(0.05);
	}
	croak("Option p_value (p_forward/p_backward) must be either ".
		"0.05 (default), 0.01, 0.005 or 0.001 when Perl module Statistics::Distributions is ".
		"not installed")
	unless ($self->p_value() == 0.05 or $self->p_value() == 0.01 or
		$self->p_value() == 0.005 or $self->p_value() == 0.001
			or $self->have_Math_CDF() );

	croak("Option p_value (p_forward/p_backward) must be in the range 0-1")
	unless ($self->p_value >= 0 and $self->p_value <=1);

	if (scalar(@{$self -> models}) > 1){
		if ($self->linearize){
			croak("scm object with option linearize can only be generated with a single model");
		}else{
			ui -> print( category => 'scm',
				message  =>"Warning: scm object generated with more than one model, not tested.",
				newline => 1);
		}
	}
	foreach my $model ( @{$self -> models} ) {
		foreach my $problem (@{$model->problems()}){
			if (defined $problem->nwpri_ntheta()){
				ui -> print( category => 'scm',
					message => "Warning: scm does not support \$PRIOR NWPRI.",
					newline => 1);
				last;
			}
		}
	}
	foreach my $par ( sort keys %{$self -> test_relations} ){
		$self->sum_covariates_hash->{$par}=0;
	}
	if (defined $self->logit and scalar(@{$self->logit()})>0){
		foreach my $par (@{$self->logit()}){
			croak("Cannot set logit for $par unless it is defined in test_relations")
			unless (defined $self->sum_covariates_hash->{$par});
			$self->sum_covariates_hash->{$par}=1;
		}
	}

	#skipped numbering when more than one this->models
	for my $accessor ( 'logfile', 'short_logfile', 'raw_results_file'){
		my @new_files=();
		my @old_files = @{$self->$accessor};
		for (my $i=0; $i < scalar(@old_files); $i++){
			my $name;
			my $ldir;
			#will this move files that already have global path???
			( $ldir, $name ) =
			OSspecific::absolute_path( $self ->directory(), $old_files[$i] );
			push(@new_files,$ldir.$name) ;
		}
		$self->$accessor(\@new_files);
	}	
	for my $accessor ( 'covariate_statistics_file','relations_file'){
		#will this move files that already have global path???
		my ( $ldir, $name )= OSspecific::absolute_path( $self ->directory(), $self->$accessor);
		$self->$accessor($ldir.$name);
	}	

	unless ( defined $self -> test_relations ) {
		croak("You need to specify \'test_relations'\ either as argument or in the config file." );
	}
	unless ( defined( $self -> categorical_covariates() ) 
			or defined( $self -> continuous_covariates() )) {
		croak("You must specify either " .
			"categorical and/or continuous covariates either as argument or in the config file" );
	}

	if (defined $self->time_varying() and scalar(@{$self->time_varying()})>0){
		my %tmphash;
		foreach my $par ( sort keys %{$self -> test_relations()} ){
			foreach my $cov ( @{$self -> test_relations()->{$par}} ){
				$tmphash{$cov}=0;
			}
		}

		my @continuous = defined $self -> continuous_covariates() ? @{$self -> continuous_covariates()} : ();
		foreach my $cov ( @continuous) {
			$tmphash{$cov}=1;
		}

		foreach my $cov (@{$self->time_varying()}){
			unless (defined $tmphash{$cov} and ($tmphash{$cov}==1)){
				if ($self->linearize()){
					croak("Cannot set time_varying for $cov unless it is defined in test_relations and continuous. With -linearize bivariate time-varying categorical covariates must be ".
						"defined as continuous");
				}else{
					croak("Cannot set time_varying for $cov unless it is defined in test_relations and continuous. When -linearize is not set categorical covariates do not need to be ".
						"defined as time varying, scm will work anyway.");

				}
			}
		}
	}

	# Check Errors and init
	unless ( $self -> search_direction eq 'forward' or
		$self -> search_direction eq 'backward' or
		$self -> search_direction eq 'both' ) {
		croak("You must specify the search direction ".
			"either as \"forward\" or \"backward\". Default is \"forward\"." );
	}

	# check the validity of the covariates and the relations to be tested

	if ( defined $self -> continuous_covariates() or defined $self -> categorical_covariates() ) {

		my @continuous = defined $self -> continuous_covariates() ? @{$self -> continuous_covariates()} : ();
		my @categorical = defined $self -> categorical_covariates() ? @{$self -> categorical_covariates()} : ();

		my @not_found = ();
		foreach my $cov ( @continuous, @categorical ) {
			#check if reserved words
			if (($cov eq 'PAR') or ($cov eq 'COV')){
				croak("PAR and COV are reserved words in scm and must not be ".
					"used as name for a covariate.");
			}
			push( @not_found, $cov )
			unless ( $self -> models->[0] -> is_option_set( record => 'input',
					name => $cov ) );
		}
		if ( scalar @not_found ){
			croak("Covariate(s) [ ". join(',', @not_found). " ] specified is not defined in " . 
				$self ->  models->[0] -> filename );
		}
	}

	if ( defined $self -> test_relations() ) {
		foreach my $par ( sort keys %{$self -> test_relations()} ){
			#check if reserved words
			if (($par eq 'PAR') or ($par eq 'COV')){
				croak("PAR and COV are reserved words in scm ".
					"and must not be used as name for a parameter.");
			}

			my @not_found = ();
			foreach my $cov ( @{$self -> test_relations()->{$par}} ){
				my @continuous = defined $self -> continuous_covariates() ? @{$self -> continuous_covariates()} : ();
				my @categorical = defined $self -> categorical_covariates() ? @{$self -> categorical_covariates()} : ();

				my $covariate_test = 0;
				foreach my $specified_cov ( @continuous, @categorical ) {
					$covariate_test = 1 and last if( $cov eq $specified_cov );
				}
				push( @not_found, $cov ) unless ( $covariate_test );
			}
			if ( scalar @not_found and
				( not defined $self -> models->[0] -> extra_files or 
					scalar @{$self -> models->[0] -> extra_files} == 0 ) ) {
				croak("Covariate(s) [ " . join( ',', @not_found ). " ] specified for parameter $par " . 
					"in test_relations is not defined as a covariate" );
			}
		}
	}

	# If no previous information on the statistics of the
	# covariates is available, initiate this.
	# First; the continuous covariates:

	if (-e $self->covariate_statistics_file) {
		open(STAT, '<' . $self->covariate_statistics_file);
		my $tmp;
		for (<STAT>) {
			$tmp = $tmp . $_;
		}
		close(STAT);
		my $VAR1;
		eval($tmp);
		$self->covariate_statistics($VAR1);

	} else {

		my $model;
		if ($do_filtering or (defined $self->time_varying and scalar(@{$self->time_varying}) > 0)) {
			$model = $self->preprocess_data(model => $self->models->[0],
				directory => $self->directory,
				test_relations => $self->test_relations,
				time_varying => $self->time_varying,
				filter => $do_filtering);
			croak('preprocessing data failed to return a model') unless (defined $model);
		} else {
			$model = $self->models->[0];
		}
		# Assume one $PROBLEM
		my %model_column_numbers; 

		my $data_obj;
		if (defined $self->derivatives_data) {
			$data_obj = data->new(
				filename             => $self->derivatives_data,
				ignoresign           => '@',
				missing_data_token   => $self->missing_data_token,
				ignore_missing_files => 0,
				parse_header				 => 1	); #ok parse_header, do not know idcol

			#set header from this data, must have column headers otherwise die
			if (defined $data_obj->column_head_indices and scalar(keys %{$data_obj->column_head_indices}) > 0) { 
				%model_column_numbers = %{$data_obj->column_head_indices};
			} else {
				croak("When using option derivatives_data (done implicitly in boot_scm) the given file must have a header.");
			}

		} else {
			my $filename = $model->datafiles(problem_numbers => [1],
											 absolute_path => 1)->[0];
			$data_obj = data->new(filename =>$filename,
								  idcolumn => $model->idcolumn(problem_number =>1),
								  ignoresign => $model->ignoresigns->[0],
								  missing_data_token => $self->missing_data_token);
			#use the model header when computing statistics
			my $model_col_num = 1;
			if (defined $model->problems->[0]->inputs and defined $model->problems->[0]->inputs->[0]->options) {
				foreach my $option (@{$model->problems->[0]->inputs->[0]->options}) {
					if (($option->name eq 'DROP' or $option->name eq 'SKIP') and (defined $option->value)) {
						$model_column_numbers{$option->value}= $model_col_num;
					} else {
						$model_column_numbers{$option->name}= $model_col_num;
					}
					$model_col_num++;
				}
			}
		}

		unless (defined $self->covariate_statistics and scalar(keys %{$self->covariate_statistics}) > 0) {
			$self->covariate_statistics({});
			my $statsref = $data_obj->scm_calculate_covariate_statistics( categorical_covariates => $self->categorical_covariates,
																	  continuous_covariates => $self->continuous_covariates,
																	  model_column_numbers => \%model_column_numbers,
																	  time_varying => $self->time_varying,
																	  linearize => $self->linearize,
																	  return_after_derivatives_done => $self->return_after_derivatives_done,
																	  gof => $self->gof,
																	  missing_data_token => $self->missing_data_token);
			$self->covariate_statistics($statsref) if (defined $statsref);
			$data_obj = undef;
			if (defined $self -> global_covariate_statistics and scalar(keys %{$self ->global_covariate_statistics })>0){
				#this is necessary for xv_scm
				if (defined $self->continuous_covariates) {
					foreach my $cov (@{$self -> continuous_covariates()}){
						$self -> covariate_statistics->{$cov}{'have_missing_data'} = 
							$self -> global_covariate_statistics->{$cov}{'have_missing_data'};
						$self -> covariate_statistics->{$cov}{'min'} = 
							$self -> global_covariate_statistics->{$cov}{'min'};
						$self -> covariate_statistics->{$cov}{'max'} = 
							$self -> global_covariate_statistics->{$cov}{'max'};
					}
				}
				if ( defined $self -> categorical_covariates()) {
					foreach my $cov (@{$self -> categorical_covariates()}){
						#this is necessary for xv_scm
						$self -> covariate_statistics->{$cov}{'have_missing_data'} = 
							$self -> global_covariate_statistics->{$cov}{'have_missing_data'};
						$self -> covariate_statistics->{$cov}{'min'} = 
							$self -> global_covariate_statistics->{$cov}{'min'};
						$self -> covariate_statistics->{$cov}{'max'} = 
							$self -> global_covariate_statistics->{$cov}{'max'};
					}
				}
			}
		}
		open( STAT, '>'.$self -> covariate_statistics_file );
		$Data::Dumper::Purity = 1;
		print STAT Dumper $self -> covariate_statistics;
		$Data::Dumper::Purity = 0;
		close( STAT );
	}

	# Default ofv drops at desired p-values (assuming chi-squared
	# distribution of hirerchical models)
	my %p_values;
	#For unlimited stepping: p = 100% /JR
	$p_values{'1'}     = {1=>0,
		2=>0,
		3=>0,
		4=>0,
		5=>0,
		6=>0,
		7=>0,
		8=>0,
		9=>0,
		10=>0};
	## p= 0.05
	$p_values{'0.05'}  = {1=>3.84,
		2=>5.99,
		3=>7.81,
		4=>9.49,
		5=>11.07,
		6=>12.59,	
		7=>14.07,
		8=>15.51,	
		9=>16.92,	
		10=>18.31};
	## p=0.01
	$p_values{'0.01'}  = {1=>6.63,
		2=>9.21,
		3=>11.34,
		4=>13.28,
		5=>15.09,
		6=>16.81,
		7=>18.48,
		8=>20.09,
		9=>21.67,
		10=>23.21};
	## p=0.005
	$p_values{'0.005'} = {1=>7.88,
		2=>10.60,
		3=>12.84,
		4=>14.86,
		5=>16.75,
		6=>18.55,
		7=>20.28,
		8=>21.95,
		9=>23.59,
		10=>25.19};
	## p=0.001
	$p_values{'0.001'} = {1=>10.83,
		2=>13.82,
		3=>16.27,
		4=>18.47,
		5=>20.52,
		6=>22.46,
		7=>24.32,
		8=>26.12,
		9=>27.88,
		10=>29.59};

	unless ($self -> p_value()== 0.05 or $self -> p_value() == 0.01 or
		$self -> p_value() == 0.005 or $self -> p_value() == 0.001){
		#create new table for this value using CDF
		my %phash;
		for (my $i=1; $i<11; $i++){
			if ($self -> p_value() <= 0){
				$phash{$i} = 1000000;
			}elsif ($self -> p_value() >= 1){
				$phash{$i} = 0;
			}else{
				$phash{$i} = Statistics::Distributions::chisqrdistr($i,($self -> p_value()));
			}
		}
		$p_values{$self -> p_value()}=\%phash;
	}

	# If no previous information on the relations is available,
	# create the basic parameter-covariate relation data structure
	# including the information about states (1=not included,
	# 2=linear relation, 3=hockey-stick relation).

	for ( my $i = 0; $i < scalar @{$self -> models}; $i++ ) {
		my $first = 1;
		#no existing relations files
		foreach my $par ( sort keys %{$self -> test_relations()} ) {
			foreach my $cov ( @{$self -> test_relations()->{$par}} ){
				# Here the ofv-drops should be defined
				# I've started 2004-11-09
				my $ofv_changes = $p_values{$self -> p_value()};
				if ( defined $self -> ofv_change ) {
					# If only one ofv_drop given for all models and all relations
					while ( my ( $df, $ofv ) = each %{$self -> ofv_change} ) {
						$ofv_changes -> {$df} = $ofv;
					}
					if ( $first ) {
						open( LOG, ">>".$self -> logfile -> [0] );
						print LOG "Using user-defined ofv change criteria\n";
						print LOG "Degree of freedom  |  Required ofv change\n";
						my @dfs = sort {$a <=> $b} keys %{$ofv_changes};
						foreach my $df ( @dfs ) {
							print LOG "         $df         -          ",
							$ofv_changes -> {$df},"\n";
						}
						close( LOG );
					}
				} 
				$self -> relations->{$par}{$cov}{'ofv_changes'} = $ofv_changes;
				# Is this covariate continuous or not?
				my $continuous = 1;
				if (defined $self -> categorical_covariates()){
					foreach my $cat ( @{$self -> categorical_covariates()} ) {
						$continuous = 0 if ( $cov eq $cat );
					}
				}
				$self -> relations->{$par}{$cov}{'continuous'} = $continuous;
				my @valid_states;
				if ( $continuous ) {
					@valid_states = @{$self -> valid_states->{'continuous'}};
				} else {
					#categorical
					@valid_states = @{$self -> valid_states->{'categorical'}};
				}
				croak("No valid states defined for ".
					(($continuous)? 'continuous':'categorical'))
				if (scalar(@valid_states) == 0);
				croak("The first valid state must always be 1")
				unless ($valid_states[0] == 1); #unless have included relations with this state at least

				$self -> relations->{$par}{$cov}{'state'} = 1;
				foreach my $state ( @valid_states ) {
					if ( defined $self -> relations->{$par}{$cov}{'code'}{$state} ) {
						if ( not ref $self -> relations->{$par}{$cov}{'code'}{$state} eq 'ARRAY' ) {
							croak("The code specified for $par $cov $state is not ".
								"an array\n" );
						} else {
							for ( @{$self -> relations->{$par}{$cov}{'code'}{$state}} ) {
								s/PARCOV/$par$cov/g;
								s/PAR/$par/g;
								s/COV/$cov/g;
							}
						}
					} else {
						croak("No code defined for relation $par-$cov state $state\n") 
						if ($state > 5);
						$self -> relations->{$par}{$cov}{'code'}{$state} = [];
					}
					$self -> relations->{$par}{$cov}{'inits'}{$state} = [] unless
					( defined $self -> relations->{$par}{$cov}{'inits'}{$state} );
					$self -> relations->{$par}{$cov}{'bounds'}{$state} = {} unless
					( defined $self -> relations->{$par}{$cov}{'bounds'}{$state} );
					my %local_statistics = %{$self -> covariate_statistics->{$cov}};
					if (defined $self->medians->{$par.'_'.$cov}){
						$local_statistics{'median'} = $self->medians->{$par.'_'.$cov};
						if ($local_statistics{'median'}< $local_statistics{'min'}){
							croak("computed median ".$local_statistics{'median'}.
								" of time-varying $cov on $par is smaller than min $cov ".
								$local_statistics{'min'});
						}
						if ($local_statistics{'median'}> $local_statistics{'max'}){
							croak("computed median ".$local_statistics{'median'}.
								" of time-varying $cov on $par is larger than max $cov ".
								$local_statistics{'max'});
						}
					}
					if (defined $self->means->{$par.'_'.$cov}){
						$local_statistics{'mean'} = $self->means->{$par.'_'.$cov};
						if ($local_statistics{'mean'}< $local_statistics{'min'}){
							croak("computed mean ".$local_statistics{'mean'}.
								" of time-varying $cov on $par is smaller than min $cov ".
								$local_statistics{'min'});
						}
						if ($local_statistics{'mean'}> $local_statistics{'max'}){
							croak("computed mean ".$local_statistics{'mean'}.
								" of time-varying $cov on $par is larger than max $cov ".
								$local_statistics{'max'});
						}
					}
					( $self -> relations->{$par}{$cov}{'code'}{$state},
						$self -> relations->{$par}{$cov}{'nthetas'}{$state},
						$self -> relations->{$par}{$cov}{'inits'}{$state},
						$self -> relations->{$par}{$cov}{'bounds'}{$state} ) =
					$self -> create_code( start_theta => 1,
						parameter   => $par,
						covariate   => $cov,
						continuous  => $continuous,
						state       => $state,
						code        => $self -> relations->{$par}{$cov}{'code'}{$state},
						inits       => $self -> relations->{$par}{$cov}{'inits'}{$state},
						bounds      => $self -> relations->{$par}{$cov}{'bounds'}{$state},
						statistics  => \%local_statistics,
						sum_covariates  => $self->sum_covariates_hash->{$par},
						missing_data_token => $self -> missing_data_token);
					if ( defined $self->included_relations() and 
						exists $self -> included_relations->{$par} and
						exists $self -> included_relations->{$par}{$cov} and
						$self -> included_relations->{$par}{$cov}{'state'} == $state ) {
						$self -> included_relations->{$par}{$cov}{'code'} =
						$self -> relations->{$par}{$cov}{'code'}{$state};
						$self -> included_relations->{$par}{$cov}{'nthetas'} =
						$self -> relations->{$par}{$cov}{'nthetas'}{$state};
						$self -> included_relations->{$par}{$cov}{'inits'} =
						$self -> relations->{$par}{$cov}{'inits'}{$state};
						$self -> included_relations->{$par}{$cov}{'bounds'} =
						$self -> relations->{$par}{$cov}{'bounds'}{$state};
					}
				} #end loop over valid states
				$first = 0;
				#check that no included relations for invalid states
				if ( defined $self->included_relations() and
					exists $self -> included_relations->{$par} and
					exists $self -> included_relations->{$par}{$cov}){
					my $included_state =$self -> included_relations->{$par}{$cov}{'state'};
					my $found = 0;
					foreach my $state (@valid_states){
						$found = 1 if ($state == $included_state);
					}
					croak("State $included_state is not listed in valid_states for ".
						(($continuous)? 'continuous':'categorical')." covariates and therefore cannot ".
						"be set as the state for relation $par-$cov in included_relations.")
					unless ($found);
				}
			} #end loop over cov
		} # end loop over par

		#check that no included relations for covariates not in test_relations
		foreach my $par ( sort keys %{$self -> included_relations} ) {
			foreach my $cov (sort keys %{$self -> included_relations->{$par}} ){
				my $found=0;
				if ( defined $self -> test_relations() ) {
					foreach my $testpar ( sort keys %{$self -> test_relations()} ){
						next unless ($testpar eq $par);
						foreach my $testcov ( @{$self -> test_relations()->{$par}} ){
							$found = 1 if ($testcov eq $cov);
						}
					}
				}
				croak("Relation $par-$cov is not listed in test_relations and therefore ".
					"cannot be set in included_relations.")
				unless ($found);
			}
		}
		open( RELATIONS, '>'.$self -> relations_file );
		$Data::Dumper::Purity = 1;
		print RELATIONS Dumper $self -> relations;
		$Data::Dumper::Purity = 0;
		close( RELATIONS );

	}#end loop models
}

sub add_config_file
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	push( @{$self->config_files}, config_file->new( %{$parm{'init_data'}} ) );
}

sub _raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	# The goal is to transfer the default modelfit raw_results format of
	# diagnostic_values-all_thetas-omegas-sigmas
	# to the format:
	# diagnostic_values-orig_thetas-scm_thetas-omegas-sigmas
	# where the scm_thetas are formatted to only hold values where there are active
	# relations in this step. Do a "print Dumper $self -> raw_results" before and
	# after to see the transformation

	#if we are doing linearize then none of thetas from orig_mod will be left
	#the sigmas and omegas will be the same
	my $labels_mod = $self->models->[$model_number - 1];
	my $orig_mod;

	if (defined $self->initial_estimates_model) {
		$orig_mod = $self->initial_estimates_model;
	} else {
		$orig_mod = $self->models->[$model_number - 1];
	}
	my ( %param_names, %npar_orig );
	my @params = ( 'theta', 'omega', 'sigma' );
	my $cols_orig = 0;
	foreach my $param ( @params ) {
		my $labels = $labels_mod -> labels( parameter_type => $param );
		if ( defined $labels ) {
			if ($self->linearize() and $param eq 'theta'){
				$param_names{$param} = [];
			}else{
				$param_names{$param} = $labels -> [0];
			}
			$npar_orig{$param}   = scalar @{$param_names{$param}};
			$cols_orig          += $npar_orig{$param};
		}
	}

	my ( @rel_header, @rel_flag, @step_rel_names, %npars );

	# In this loop we create a mesh of all (allowed and) possible parameter-covariate
	# relations. The active relations of each model [$i] is stored in $rel_flag[$i] as
	# a 1. All inactive relations are indicated by a 0. A header for the raw_results
	# file is stored in @rel_header. %npars is a bit superfluous since this
	# information may be reach through relations-{par}{cov}{'nthetas'}{state} later on. 
	foreach my $parameter ( sort keys %{$self -> relations()} ) {
		foreach my $covariate ( sort keys %{$self -> relations()->{$parameter}} ) {
			my ( $in_step, $in_step_state ) = ( 0, undef );
			my $type = $self -> relations()->{$parameter}{$covariate}{'continuous'} == 1 ?
			'continuous' : 'categorical';
			for ( my $j = 0; $j < scalar @{$self -> valid_states->{$type}}; $j++ ) {
				my $state = $self -> valid_states->{$type}[$j];
				my $npar =
				$self -> relations()->{$parameter}{$covariate}{'nthetas'}{$state};
				if ( defined $npar ) { # Skip states without parameters
					$npars{$parameter}{$covariate}{$state} = $npar;
					for ( my $k = 1; $k <= $npar; $k++ ) {
						push( @rel_header, $parameter.$covariate.'-'.$state.'-'.$k );
						for ( my $i = 0; $i < scalar @{$self -> step_relations}; $i++ ) {
							if ( $parameter eq $self -> step_relations -> [$i]{'parameter'} and
								$covariate eq $self -> step_relations -> [$i]{'covariate'} and
								$state     eq $self -> step_relations -> [$i]{'state'} or
								( defined $self -> included_relations->{$parameter} and
									defined $self -> included_relations->{$parameter}{$covariate} and
									$state eq $self -> included_relations->{$parameter}{$covariate}{'state'} and not
									( $parameter eq $self -> step_relations -> [$i]{'parameter'} and
										$covariate eq $self -> step_relations -> [$i]{'covariate'} ) ) ) {
								push( @{$rel_flag[$i]}, 1);
							} else {
								push( @{$rel_flag[$i]}, 0);
							}
						}
					}
				}
			}
		}
	}

	my $nmax = 0;
	for ( my $i = 0; $i <= $#rel_flag; $i++ ) {
		my $sum = 0;
		for ( my $j = 0; $j < scalar @{$rel_flag[$i]}; $j++ ) {
			$sum += $rel_flag[$i][$j];
		}
		$nmax = $nmax > $sum ? $nmax : $sum;
	}

	# Use the scm's raw_results file.
	my ($dir,$file) = 
	OSspecific::absolute_path( $self -> directory,
		$self -> raw_results_file->[$model_number-1] );
	my $step_number = $self -> step_number();
	for ( my $i = 0; $i < scalar @{$self -> step_relations}; $i++ ) {
		push( @step_rel_names, $self -> step_relations -> [$i]{'parameter'}.
			$self -> step_relations -> [$i]{'covariate'}.'-'.
			$self -> step_relations -> [$i]{'state'} );
	}
	my %included_relations = %{$self -> included_relations};
	my %relations          = %{$self -> relations()};
	my @step_relations     = @{$self -> step_relations};
	my %valid_states       = %{$self -> valid_states};
	my $action = $self -> search_direction eq 'forward' ? 'added' : 'removed';
	$subroutine = sub {
		my $modelfit = shift;
		my $mh_ref   = shift;
		my %max_hash = %{$mh_ref};
		my @new_header = ('step.number','action','relation' );
		$modelfit -> raw_results_file( [$dir.$file] );
		$modelfit -> raw_results_append( 1 ) if ( $step_number > 1 );

		my $raw_results_header = $modelfit -> raw_results_header;
		my $raw_results = $modelfit -> raw_results;
		my $cols = scalar @{$modelfit -> raw_results -> [0]}; # first non-header row
		#callback is only used when running candidate models.
		#only change raw_line_structure first iteration.
		#use rawline structure for model no 1, edit, overwrite for other models 
		#if not first iteration then read from file?

		my @diagnostic_params = @{$self -> diagnostic_parameters};
		my @diagnostic_indices;
		unshift(@diagnostic_params,('model','problem','subproblem'));
		push(@diagnostic_params,'ofv');
		foreach my $param (@diagnostic_params){
			my ($start,$len) = split(',',$modelfit->raw_line_structure->{'1'}->{$param});
			push(@diagnostic_indices,$start) unless ($len == 0);
		}
		my $len;
		my $theta_start=0;
		my $omega_start=0;
		my $sigma_start=0;
		my $setheta_start=0;
		my $seomega_start=0;
		my $sesigma_start=0;
		my $shrinkage_eta_start=0;
		my $shrinkage_iwres_start=0;
		my $len_setheta=0;
		my $len_seomega=0;
		my $len_sesigma=0;
		my $len_shrinkage_eta=0;
		my $len_shrinkage_iwres=0;

		($theta_start,$len) = split(',',$modelfit->raw_line_structure->{'1'}->{'theta'})
		if (defined $modelfit->raw_line_structure->{'1'}->{'theta'});
		($omega_start,$len) = split(',',$modelfit->raw_line_structure->{'1'}->{'omega'})
		if (defined $modelfit->raw_line_structure->{'1'}->{'omega'});
		($sigma_start,$len) = split(',',$modelfit->raw_line_structure->{'1'}->{'sigma'})
		if (defined $modelfit->raw_line_structure->{'1'}->{'sigma'});
		($setheta_start,$len_setheta) = 
		split(',',$modelfit->raw_line_structure->{'1'}->{'setheta'})
		if (defined $modelfit->raw_line_structure->{'1'}->{'setheta'});
		($seomega_start,$len_seomega) = 
		split(',',$modelfit->raw_line_structure->{'1'}->{'seomega'})
		if (defined $modelfit->raw_line_structure->{'1'}->{'seomega'});
		($sesigma_start,$len_sesigma) = 
		split(',',$modelfit->raw_line_structure->{'1'}->{'sesigma'})
		if (defined $modelfit->raw_line_structure->{'1'}->{'sesigma'});
		($shrinkage_eta_start,$len_shrinkage_eta) = 
		split(',',$modelfit->raw_line_structure->{'1'}->{'shrinkage_eta'})
		if (defined $modelfit->raw_line_structure->{'1'}->{'shrinkage_eta'});
		($shrinkage_iwres_start,$len_shrinkage_iwres) = 
		split(',',$modelfit->raw_line_structure->{'1'}->{'shrinkage_iwres'})
		if (defined $modelfit->raw_line_structure->{'1'}->{'shrinkage_iwres'});

		for ( my $i = 0; $i < scalar @{$modelfit -> raw_results}; $i++ ) {
			my @new_raw_results = ( $step_number,$action,$step_rel_names[$i] );

			my ( @diagnostics, @thetas, @omsi, @sethetas, @seomsi, @shrinkage_eta,$shrinkage_iwres );

			# {{{ Get diagnostic results:
			#every column up to first theta.

			for ( my $j = 0; $j < scalar(@diagnostic_indices); $j++ ) {
				push( @diagnostics, $modelfit -> raw_results -> [$i][$diagnostic_indices[$j]] );
			}

			# }}}

			# {{{ Get the thetas that were present in the original model 

			for ( my $j = $theta_start;
				$j < ($theta_start+$npar_orig{'theta'}); $j++ ) {
				push( @thetas, $modelfit -> raw_results -> [$i][$j] );
			}

			# }}}

			# {{{ Get the results for all par-cov-relation

			# Initiate $j as starting position for the relation thetas
			my %res;


			foreach my $kind ( 'estimate', 'se' ) {
				my ($j,$len);
				if ($kind eq 'estimate'){
					$j = $theta_start+$npar_orig{'theta'};
				}else{
					next if ($len_setheta==0);
					$j = $setheta_start+$npar_orig{'theta'};
				}
				# Important to loop over the sorted hash
				# Add all included relations estimates
				foreach my $incl_par ( sort keys %included_relations ) {
					foreach my $incl_cov ( sort keys %{$included_relations{$incl_par}} ) {
						next if ( $incl_par eq $step_relations[$i]->{'parameter'} and
							$incl_cov eq $step_relations[$i]->{'covariate'} );
						my $npar  = $included_relations{$incl_par}{$incl_cov}{'nthetas'};
						my $state = $included_relations{$incl_par}{$incl_cov}{'state'};
						for ( my $l = 1; $l <= $npar; $l++ ) {
							push( @{$res{$incl_par}{$incl_cov}{$state}{$kind}},
								$modelfit -> raw_results -> [$i][$j++] );
						}
					}
				}

				# Add the estimates of the relation unique to the model [$i]
				for ( my $l = 1; $l <= $npars{$step_relations[$i]->{'parameter'}}
					{$step_relations[$i]->{'covariate'}}
					{$step_relations[$i]->{'state'}}; $l++ ) {
					push( @{$res{$step_relations[$i]->{'parameter'}}
						{$step_relations[$i]->{'covariate'}}
						{$step_relations[$i]->{'state'}}{$kind}},
						$modelfit -> raw_results -> [$i][$j++] );
				}

				# Sort the results according to the order they appear in (a sorted) $self ->
				# {'relations'}
				foreach my $par ( sort keys %npars ) {
					foreach my $cov ( sort keys %{$npars{$par}} ) {
						my $type = $relations{$par}{$cov}{'continuous'} == 1 ?
						'continuous' : 'categorical';
						foreach my $state ( @{$valid_states{$type}} ) {
							my $val = ( defined $res{$par} and defined $res{$par}{$cov} and
								defined $res{$par}{$cov}{$state} and
								defined $res{$par}{$cov}{$state}{$kind} ) ?
							$res{$par}{$cov}{$state}{$kind} :
							[(undef) x $npars{$par}{$cov}{$state}];
							push( @thetas, @{$val} ) if( defined $val and $kind eq 'estimate' );
							push( @sethetas, @{$val} ) if( defined $val and $kind eq 'se' );
						}
					}
				}
			}

			# }}} 

			# {{{ Get all the omegas and sigmas

			for ( my $j = $omega_start; $j < ( $omega_start +$npar_orig{'omega'}); $j++ ) {
				push( @omsi, $modelfit -> raw_results() -> [$i][$j] );
			}
			for ( my $j = $sigma_start; $j < ( $sigma_start +$npar_orig{'sigma'}); $j++ ) {
				push( @omsi, $modelfit -> raw_results() -> [$i][$j] );
			}

			# }}}

			# {{{ Get all the standard errors of the omegas and sigmas
			for ( my $j = $seomega_start; $j < ( $seomega_start +$len_seomega); $j++ ) {
				push( @omsi, $modelfit -> raw_results() -> [$i][$j] );
			}
			for ( my $j = $sesigma_start; $j < ( $sesigma_start +$len_sesigma); $j++ ) {
				push( @omsi, $modelfit -> raw_results() -> [$i][$j] );
			}

			#ok if 0 lenght here since do not add anything after
			for (my $j=$shrinkage_eta_start; $j<($shrinkage_eta_start+$len_shrinkage_eta);$j++){
				push (@shrinkage_eta,$modelfit -> raw_results() -> [$i][$j]);
			}
			$shrinkage_iwres = $modelfit -> raw_results() -> [$i][($shrinkage_iwres_start)]
			if ($len_shrinkage_iwres > 0);

			# }}}

			push( @new_raw_results, ( @diagnostics, @thetas, @omsi,
					@sethetas, @seomsi,@shrinkage_eta,$shrinkage_iwres ) );

			$modelfit -> raw_results() -> [$i] = \@new_raw_results;
		}
		if ( $step_number == 1 ) {
			my %start_category_hash;
			# Loop through the unchanged header and use the header names as accessors
			# for the original model raw results.
			$self->raw_line_structure($modelfit->raw_line_structure);
			#my @new_header = ('step.number','action','relation' );
			my @orig_res = ( 0, 'none (base)','none', 1, 1, 1 );
			foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure})){
				foreach my $category (keys %{$self->raw_line_structure -> {$mod}}){
					next if ($category eq 'line_numbers');
					my ($start,$len) = split(',',$self->raw_line_structure -> {$mod}->{$category});
					$self->raw_line_structure -> {$mod}->{$category} = ($start+3).','.$len;
					$start_category_hash{$start+3}=$category if ($mod ==1);
				}
				$self->raw_line_structure -> {$mod}->{'step.number'} = '0,1';
				$self->raw_line_structure -> {$mod}->{'action'} = '1,1';
				$self->raw_line_structure -> {$mod}->{'relation'} = '2,1';
				$start_category_hash{0}='step.number' if ($mod ==1);
				$start_category_hash{1}='action' if ($mod ==1);
				$start_category_hash{2}='relation' if ($mod ==1);
			}

			$self->raw_line_structure -> {'0'} = $self->raw_line_structure -> {'1'};

			my $saem=0;
			my $bayes=0;

			foreach my $param ( @{$raw_results_header} ){
				next if( $param eq 'model' or $param eq 'problem' or $param eq 'subproblem' );
				my ( $accessor, $res );
				if ( $param eq 'npomega' or $param eq 'eigen' ) {
					$accessor = $param.'s';
					$res = $orig_mod -> outputs -> [0] -> $accessor;

				}elsif ( $param eq 'theta' or $param eq 'omega' or $param eq 'sigma' or
					$param eq 'setheta' or $param eq 'seomega' or $param eq 'sesigma' ) {
					$res = $orig_mod -> get_values_to_labels ( category => $param);

				}elsif ( $param eq 'est_methods' ) {
					#array over $PROB
					my @arr=();
					for (my $i=0;$i< scalar(@{$orig_mod ->problems()}); $i++){
						#get ref of array of methods
						my $methref = $orig_mod -> get_option_value(record_name => 'estimation', option_name => 'METHOD',
							problem_index => $i, record_index => 'all'); 

						my $eonlyref = $orig_mod -> get_option_value(record_name => 'estimation', 
							option_name => 'EONLY',
							problem_index => $i, record_index => 'all'); 
						my @string_arr;
						for (my $j=0; $j< scalar(@{$methref}); $j++){ 
							if (defined $methref->[$j]){
								if ($methref->[$j] eq '1' or $methref->[$j] eq 'COND' or 
									(index('COND', $methref->[$j]) == 0)){
									if( $orig_mod-> is_option_set( record => 'estimation', name => 'LAPLACE',
											record_number => ($j+1),fuzzy_match =>1) or 
										$orig_mod-> is_option_set( record => 'estimation', name => 'LAPLACIAN',
											record_number => ($j+1),
											fuzzy_match =>1)){
										push(@string_arr,'LAPLACE');
									}else{
										push(@string_arr,'FOCE');
									}
								}elsif ($methref->[$j] eq '0' or $methref->[$j] eq 'ZERO' or 
									(index('ZERO', $methref->[$j]) == 0)){
									push(@string_arr,'FO');
								}elsif (defined $eonlyref->[$j] and $eonlyref->[$j] == 1){
									push(@string_arr,$methref->[$j].'*');
								}else{
									push(@string_arr,$methref->[$j]);
								}
							}else{
								push(@string_arr,'FO'); #default
							}
							last unless ($PsN::nm_major_version >= 7);
						}
						push(@arr,join('-',@string_arr));
						$saem = 1 if ($string_arr[$#string_arr] eq 'SAEM' or 
							(index('SAEM',$string_arr[$#string_arr]==0)));
						$bayes = 1 if ($string_arr[$#string_arr] eq 'BAYES' or 
							(index('BAYES',$string_arr[$#string_arr]==0)));
					}
					$res = \@arr;
				}elsif ( $param eq 'nburn_set' ) {
					if ($saem or $bayes){
						my @arr=();
						for (my $i=0;$i< scalar(@{$orig_mod ->problems()}); $i++){
							my $nburnref = $orig_mod -> get_option_value(record_name => 'estimation', 
								option_name => 'NBURN',
								problem_index => $i, record_index => 'all'); 
							if (defined $nburnref){
								my $j= scalar(@{$nburnref})-1;
								if (defined $nburnref->[$j]){
									push(@arr,$nburnref->[$j]);
								}else{
									push(@arr,undef);
								}
							}
						}
						$res = \@arr;
					}else{
						$res = undef;
					}
				}elsif ( $param eq 'burn_in_iter' ) {
					if ($saem or $bayes){
						$accessor = 'burn_in_iterations';
						$res = $orig_mod -> outputs -> [0] -> $accessor;	  
					}else{
						$res = undef;
					}
				}elsif ( $param eq 'burn_in_conv' ) {
					if ($saem or $bayes){
						$accessor = 'burn_in_convergence';
						$res = $orig_mod -> outputs -> [0] -> $accessor;	  
					}else{
						$res = undef;
					}
				}elsif ( $param eq 'subprob_est_time' ) {
					if ($PsN::nm_major_version >= 7){
						$accessor = 'sum_estimation_time';
						$res = $orig_mod -> outputs -> [0] -> $accessor;	  
					}else{
						$res = undef;
					}
				}elsif ( $param eq 'model_run_time' ) {
					if ($PsN::nm_major_version >= 7){
						#this is a scalar string
						$res = $orig_mod -> outputs -> [0] -> runtime();	  
					}else{
						$res = undef;
					}
				}elsif ( $param eq 'subprob_cov_time' ) {
					if ($PsN::nm_major_version >= 7){
						$accessor = 'sum_covariance_time';
						$res = $orig_mod -> outputs -> [0] -> $accessor;	  
					}else{
						$res = undef;
					}
				} elsif ( $param eq 'shrinkage_eta' ) {

					# Shrinkage does not work for subproblems right now.
					$res = $orig_mod -> eta_shrinkage;

				} elsif ( $param eq 'shrinkage_iwres' ) {

					$res = $orig_mod -> iwres_shrinkage;

				} else {

					$res = $orig_mod -> outputs -> [0] -> $param;

				}
				# To handle settings on problem level.

				if( defined $res){
					if ( ref $res eq 'ARRAY' ){
						if( ref $res -> [0] eq 'ARRAY' ){
							$res = $res -> [0][0];
						} else{
							$res = $res -> [0];
						}
					}
				}

				if ( $max_hash{$param} < 1 and  (not defined $res)) {
					1;
				}elsif (not ( ref $res eq 'ARRAY' )) {
					push( @orig_res, $res );
				}elsif (not ($param eq 'theta' or $param eq 'setheta')){
					if( defined $res ) {
						push( @orig_res, @{$res} ); #all
						push( @orig_res, (undef) x ($max_hash{$param} - scalar @{$res}) );
					} else {
						push( @orig_res, (undef) x $max_hash{$param} );
					}
				} else {
					#theta or setheta
					my $maxn=0;
					if( defined $res ) {
						$maxn = scalar(@{$res});
						my $kind = 'estimate';
						$kind = 'se' if ($param eq 'setheta');
						my $j=0;
						for (my $i=0;$i < $maxn; $i++){
							last if ($j == $npar_orig{'theta'});
							push(@orig_res,$res->[$j++]);
						}		    
						# Important to loop over the sorted hash
						# Add all included relations estimates
						my %results;
						if ($self->have_run_included == 1){
							foreach my $incl_par ( sort keys %included_relations ) {
								foreach my $incl_cov ( sort keys %{$included_relations{$incl_par}} ) {
									my $npar  = $included_relations{$incl_par}{$incl_cov}{'nthetas'};
									my $state = $included_relations{$incl_par}{$incl_cov}{'state'};
									for ( my $l = 1; $l <= $npar; $l++ ) {
										push( @{$results{$incl_par}{$incl_cov}{$state}{$kind}},$res ->[$j++] );
									}
								}
							}
						}
						#undefs for new relations not included...
						foreach my $par ( sort keys %npars ) {
							foreach my $cov ( sort keys %{$npars{$par}} ) {
								my $type = $relations{$par}{$cov}{'continuous'} == 1 ?
								'continuous' : 'categorical';
								foreach my $state ( @{$valid_states{$type}} ) {
									my $val = ( defined $results{$par} and defined $results{$par}{$cov} and
										defined $results{$par}{$cov}{$state} and
										defined $results{$par}{$cov}{$state}{$kind} ) ?
									$results{$par}{$cov}{$state}{$kind} :
									[(undef) x $npars{$par}{$cov}{$state}];
									push( @orig_res, @{$val} );# if( defined $val);
								}
							}
						}
					} else {
						for (my $j=0;$j< $npar_orig{'theta'}; $j++){
							push(@orig_res,undef);
						}		    
						# Push undef's for all possible relations
						foreach my $par ( sort keys %npars ) {
							foreach my $cov ( sort keys %{$npars{$par}} ) {
								my $type = $relations{$par}{$cov}{'continuous'} == 1 ?
								'continuous' : 'categorical';
								foreach my $state ( @{$valid_states{$type}} ) {
									my $val = [(undef) x $npars{$par}{$cov}{$state}];
									push( @orig_res, @{$val} );
								}
							}
						}
					}
				} #end if theta

			}
			unshift( @{$raw_results}, \@orig_res );
			my @new_header = ('step.number','action','relation' );
			foreach my $name ( @{$raw_results_header} ) {
				my @new_names = ();
				foreach my $param ( @params ) {
					if ( $name eq $param ) {
						@new_names = @{$param_names{$param}};
						if( $param eq 'theta' ) {
							push( @new_names, @rel_header );
						}
						last;
					}
					if ( $name eq 'se'.$param ) {
						if( $param eq 'theta' ) {
							foreach my $head_str ( @{$param_names{$param}}, @rel_header ) {
								push( @new_names, 'se'.$head_str );
							}
						}
						last;
					}
				}
				if ( $#new_names >= 0 ) {
					push( @new_header, @new_names );
					$max_hash{$name}=scalar(@new_names);
				} else {
					push( @new_header, $name ) if ( $max_hash{$name} > 0);
				}
			}
			$modelfit -> raw_results_header(\@new_header);
			#in struct need to change length theta start of all later 
			#then need to change length setheta and start of all later 
			#will assume theta is before setheta
			foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure})){
				my ($thstart,$len) = split(',',$self->raw_line_structure -> {$mod}->{'theta'});
				my $extra1 = scalar(@{$param_names{'theta'}})+scalar(@rel_header)-$len;
				$self->raw_line_structure -> {$mod}->{'theta'} = ($thstart).','.($len+$extra1);
				foreach my $st (sort({$a <=> $b} keys %start_category_hash)){
					next unless ($st > $thstart);
					my $category = $start_category_hash{$st};
					my ($start,$len) = split(',',$self->raw_line_structure -> {$mod}->{$category});
					$self->raw_line_structure -> {$mod}->{$category} = ($start+$extra1).','.$len;
				}
				my ($sethstart,$len) = split(',',$self->raw_line_structure -> {$mod}->{'setheta'});
				my $extra2 = scalar(@{$param_names{'theta'}})+scalar(@rel_header)-$len;
				$self->raw_line_structure -> {$mod}->{'setheta'} = ($sethstart).','.($len+$extra2);
				foreach my $st (sort({$a <=> $b} keys %start_category_hash)){
					next unless (($st+$extra1) > $sethstart);
					my $category = $start_category_hash{$st};
					my ($start,$len) = split(',',$self->raw_line_structure -> {$mod}->{$category});
					$self->raw_line_structure -> {$mod}->{$category} = ($start+$extra2).','.$len;
				}
			}
			$self->raw_line_structure -> write( $dir.'raw_results_structure' );

		} #end if step_number == 1

	};
	return $subroutine;
}

sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	my $model = $self->models->[$model_number - 1];

	if ($self->step_number == 1) {
		#set directory for final model
		my ($dir, $dummy) = OSspecific::absolute_path($self->directory . '/final_models', '');
		$self->final_model_directory($dir);
		unless (-d $self->final_model_directory) {
			mkdir ($self->final_model_directory);
		}
		if (defined $self->xv_pred_data) {
			$self->xv_results_file($self->directory . '/xv_results.txt');
		}
		#if linearize then copy original model here (only allow one model)
		if ($self->linearize) {
			my $tmp_orig = $model->copy(
				filename           => $self->final_model_directory.'/original.mod',
				copy_datafile          => 0,
				write_copy =>1,
				copy_output        => 0);
			$tmp_orig = undef;
		}
	}

	# If the number of threads are given per tool, e.g. [2,5] meaning 2 threads for
	# scm and 5 for the modelfit.
	my $mfit_threads = ref( $self -> threads ) eq 'ARRAY' ?
	$self -> threads -> [1]:$self -> threads;
	my $own_threads = ref( $self -> threads ) eq 'ARRAY' ?
	$self -> threads -> [0]:$self -> threads;
	# More threads than models?
	my $num = scalar @{$self -> models};
	$own_threads = $num if ( $own_threads > $num );

	#setup linearize here. 
	if ($self->linearize()){
		#this will modify $model if step_number > 1
		$self->linearize_setup(original_model => $model);
		return if ($self->return_after_derivatives_done());
	}
	# Check which models that hasn't been run and run them
	# This will be performed each step but would in old code only result in running
	# models at the first step, if at all. Now we also run if included_relations
	# in first step

	# If more than one process is used, there is a VERY high risk of interaction
	# between the processes when creating directories for model fits. Therefore
	# the directory attribute is given explicitly below.

	my %included_relations;
	%included_relations = %{$self -> included_relations} if 
	(defined $self -> included_relations);
	my $need_base_ofv = 1;
	$need_base_ofv = 0 if ( defined $self -> base_criteria_values and
		defined $self -> base_criteria_values -> {'ofv'} );

	if ( ( (not $model -> is_run and ($self->step_number()==1 or $self->update_derivatives())) 
				or ((%included_relations) and $need_base_ofv and $self->step_number()==1)
				or ((defined $self->xv_pred_data) and $self->step_number()==1)
		)
			and $self->run_linearized_base() ) {

		#according to Jakob's wish, run model here with included relations
		#will make base_criteria_values ofv redundant
		my $stepname = '';
		if ($self->step_number > 1) {
			$stepname = '_' . ($self->step_number - 1);
			if ($self->search_direction eq 'forward') {
				$stepname .= 'f';
			} else {
				$stepname .= 'b';
			}
		}
		my $fname = 'base_model_with_included_relations' . $stepname . '.mod';
		if (($self->max_steps == 0) and ($self->step_number == 1) and scalar(keys %{$self->test_relations}) == 0 and $self->linearize) {
			$fname = $self->basename . '.mod';
		}

		my $copy_datafile = 0;
		$copy_datafile = 1 if ((not $self->linearize ) and (not defined $self->xv_pred_data));

		my $start_model = $model->copy(filename => $fname,
									   write_copy => 0,
									   copy_datafile          => $copy_datafile,
									   copy_output        => 0);
		
		$start_model->directory($self->directory);
		if (scalar(keys %included_relations) > 0) {
			$self->have_run_included(1);
			#must not permanently modify bare base model, would cause errors when adding relations later
			#make copy and try to change reference base values etc,
			#add included relations
			#if not model run and no included relations, there will be no change here.
			# if linearize may or may not be a change
			my @used_covariates = ();
			foreach my $incl_par (sort keys %included_relations) {
				foreach my $incl_cov (sort keys %{$included_relations{$incl_par}}) {
					if ($self->linearize) {
						$self->add_code_linearize( definition_code => $included_relations{$incl_par}{$incl_cov}{'code'},
							nthetas         => $included_relations{$incl_par}{$incl_cov}{'nthetas'},
							inits           => $included_relations{$incl_par}{$incl_cov}{'inits'},
							bounds          => $included_relations{$incl_par}{$incl_cov}{'bounds'},
							applicant_model => $start_model,
							sum_covariates  => $self->sum_covariates_hash->{$incl_par},
							parameter       => $incl_par,
							covariate       => $incl_cov );
					} else {
						$self->add_code( definition_code => $included_relations{$incl_par}{$incl_cov}{'code'},
							nthetas         => $included_relations{$incl_par}{$incl_cov}{'nthetas'},
							inits           => $included_relations{$incl_par}{$incl_cov}{'inits'},
							bounds          => $included_relations{$incl_par}{$incl_cov}{'bounds'},
							applicant_model => $start_model,
							sum_covariates  => $self->sum_covariates_hash->{$incl_par},
							parameter       => $incl_par,
							covariate       => $incl_cov );
					}
					push(@used_covariates, $incl_cov);
				}
			}
			my @all_covariates;
			if (defined $self->categorical_covariates) {
				push(@all_covariates, @{$self->categorical_covariates});
			}
			if (defined $self->continuous_covariates) {
				push(@all_covariates, @{$self->continuous_covariates});
			}
			$self->drop_undrop_covariates(applicant_model => $start_model,
				used_covariates => \@used_covariates,
				all_covariates  => \@all_covariates,
				do_not_drop     => $self->do_not_drop);
		}
		$start_model->_write;

		my $orig_fit = tool::modelfit->new
		( %{common_options::restore_options(@common_options::tool_options)},
			base_directory => $self->directory,
			directory      => $self->directory . '/base_modelfit_dir' . $model_number . '/',
			models         => [$start_model],
			top_tool       => 0,
			parent_tool_id => $self->tool_id,
			threads        => $mfit_threads,
			parent_threads => $own_threads,
			copy_data  => (not $self->linearize));

		my $mess = "Estimating base model";
		$mess .= " with included_relations to get base ofv" if ($self->have_run_included);
		if ($self->linearize) {
			$mess = "Estimating linearized base model";
			if ($self->step_number > 1) {
				$mess .= " with updated derivatives and predictions";
			}
		}
		ui -> print(category => 'scm', message  => $mess) unless ($self->parent_threads > 1);
		$orig_fit->run;

		if (defined $start_model->outputs and defined $start_model->outputs->[0] and
			$start_model->outputs()->[0]-> have_output() and
			defined $start_model-> outputs -> [0] -> get_single_value(attribute=> 'ofv')) {
			my $start_ofv = $start_model -> outputs -> [0] -> get_single_value(attribute=> 'ofv');
			my $ofvname = 'ofv';
			my $start_name = $start_model->filename;
			#change base criteria values unless it is defined already (to override start value)
			if (($self->linearize() and $self->step_number() > 1) 
					or not ( defined $self -> base_criteria_values and 
					defined $self -> base_criteria_values -> {'ofv'})) {
				$self -> base_criteria_values -> {'ofv'} = $start_ofv;
			}
#override if update_derivatives, set even if old value defined
#	  we always reestimate included, so should not need to set derivatives ofv as linearized base
			if ($self->linearize()){
				my $ofv = sprintf("%12.5f",$start_ofv);
				open( LOG, ">>".$self -> logfile -> [$model_number-1] );
				if ($self->update_derivatives() and $self->step_number()>1){
					print LOG "The $ofvname of the updated linearized base model:$ofv        $start_name\n";
				}else{
					print LOG "The $ofvname of the linearized base model:$ofv        $start_name\n";
					ui -> print(category => 'linearize',
						message =>"\nThe $ofvname of the linearized base model:$ofv        $start_name\n");
				}
				print LOG "--------------------\n\n";
				close LOG;
			}

		}else{
			print "Warning: could not retrieve OFV from base model.\n";
		}
		if (defined $self->xv_pred_data) {
			$self->run_xv_pred_step(estimation_model => $start_model,
				model_name => 'base');
		}
		$self -> initial_estimates_model($start_model);
		#end if rerun start_model
	}

	my $temp_step_relations;
	( $self -> prepared_models->[$model_number-1]{'own'}, $temp_step_relations ) =
		$self -> _create_models( model_number => $model_number,
								 orig_model   => $self -> models -> [$model_number-1],
								 initial_estimates_model   => $self ->initial_estimates_model,
								 relations    => $self -> relations(),
								 included_relations =>  $self -> included_relations,
								 parallel_states => $self -> parallel_states());
	$self -> step_relations($temp_step_relations);
	# Create a modelfit tool for all the models of this step.
	# This is the last setup part before running the step.


	if ((defined $self->prepared_models) and (defined $self->prepared_models->[$model_number-1]{'own'})
			and scalar(@{$self->prepared_models->[$model_number-1]{'own'}}) > 0) {
		$self->tools([]) unless (defined $self->tools);
		push(@{$self->tools},
			tool::modelfit->new
			 ( %{common_options::restore_options(@common_options::tool_options)},
			   _raw_results_callback => $self->_raw_results_callback(model_number => $model_number),
			   models         => $self->prepared_models->[$model_number-1]{'own'},
			   threads        => $mfit_threads,
			   logfile        => [$self->directory."/modelfit".$model_number.".log"],
			   base_directory => $self->directory,
			   directory      => $self->directory.'/modelfit_dir'.$model_number,
			   parent_threads => $own_threads,
			   parent_tool_id => $self->tool_id,
			   top_tool       => 0,
			   copy_data => 0) );
		ui -> print( category => 'scm',
			message  => "Estimating the candidate models." ) if ($self->linearize());
	} else {
		my $mess;
		if ($self->search_direction eq 'forward') {
			$mess="No models to test, there are no relations to add.";
		} else {
			$mess="No models to test, there are no included relations to remove.";
			$self->base_criteria_values->{'ofv'} = 0;	  #to avoid crash later
		}
		ui -> print( category => 'scm',
			message  => $mess ) unless ( $self -> parent_threads > 1 );
		open( LOG, ">>".$self -> logfile -> [$model_number-1] );
		print LOG "\n\n"."$mess\n\n\n";
		close LOG;
	}
}

sub linearize_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		original_model => { isa => 'model', optional => 1 }
	);
	my $original_model = $parm{'original_model'};

	my $linearize_only=0;
	if (($self->max_steps() == 0) and ($self->step_number()==1) and 
		scalar(keys %{$self->test_relations()}) == 0){
		$linearize_only = 1;
		my $base = $original_model->filename();
		$base =~ s/\.mod$//;
		$base .= '_linbase';
		$self->basename($base);
	}
	#add_code_linearize needs input parameter or eta, plus translation
	#or simply use add_code for derivatives model?
	# make check that only one problem for linearize 
	my $derivatives_model; 
	my %included_relations;
	%included_relations = %{$self -> included_relations} if 
	(defined $self -> included_relations);
	my $datafilename;
	my $part='-part';
	my $rerun_derivatives_new_direction = 1;
	my $stepname='';
	if ($self->step_number()>1){
		$stepname = '_'.($self->step_number()-1);
		if ($self->search_direction() eq 'forward'){
			$stepname .= 'f';
		}else{
			$stepname .= 'b';
		}
	}
	if ($self->step_number() == 1){
		#if first step then prepare parameter_eta hash
		#assume parameters given and not etas. search code to find which eta goes with each param
		#1.7
		my $nETA=  $original_model-> nomegas(with_correlations => 0, with_same => 1) -> [0];
		my $nEPS=  $original_model-> nsigmas(with_correlations => 0, with_same => 1) -> [0];

		my %parameter_eta;
		my %parameter_relation;
		my @code;
		@code = @{$original_model -> pk( problem_number => 1 )};
		unless ( $#code > 0 ) {
			@code = @{$original_model -> pred( problem_number => 1 )};
		}
		if ( $#code <= 0 ) {
			croak("Neither PK or PRED defined in " .
				$original_model -> filename . ", cannot match parameters to ETAs\n" );
		}
		my $n_param=0;
		open( LOG, ">>".$self -> logfile -> [0] ); #model_number -1
		foreach my $parameter ( keys %{$self -> test_relations()} ){
			$n_param++;
			my $etanum = 0;
			my $relation='';

			for ( @code ) {
				if ( /^\s*(\w+)\s*=\s*/ and $1 eq $parameter ){
					s/^\s*(\w+)\s*=\s*//;
					my ($line,$comment) = split( ';', $_, 2 );
					$_ = $line;
					chomp;

					if (/\*\s*EXP\s*\(\s*ETA\(([0-9]+)\)/){
						$relation = 'exponential';
					}elsif (/[^A-Z0-9_]*EXP\s*\(\s*ETA\(([0-9]+)\)/){
						$relation = 'exponential';
					}elsif (/[^A-Z0-9_]*EXP\s*\(\s*MU\_([0-9]+)\s*\+\s*ETA\(([0-9]+)\)/){
						$relation = 'exponential';
					}elsif(/[^A-Z0-9_]*TV(\w+)\s*\+\s*ETA\(([0-9]+)\)/){
						if ($self->sum_covariates_hash->{$parameter}==1){
							$relation = 'logit';
						}else{
							$relation = 'additive';
						}
					}elsif(/[^A-Z0-9_]*TV(\w+)\s*\*\s*ETA\(([0-9]+)\)/){
						$relation = 'proportional';
					}elsif(/[^A-Z0-9_]*ETA\(([0-9]+)\)\s*\*\s*TV(\w+)/){
						$relation = 'proportional';
					}elsif(/\*\s*\(\s*1\s*\+\s*ETA\(([0-9]+)\)/){
						$relation = 'proportional';
					}elsif(/\*\(\s*ETA\(([0-9]+)\)\s*\+\s*1/){
						$relation = 'proportional';
					}

					if (s/[^A-Z0-9_]ETA\(([0-9]+)\)//){
						$etanum = $1;
					}else{
						last;
					}

					if (s/[^A-Z0-9_]ETA\(([0-9]+)\)//){
						croak("Could not determine the ETA ".
							"coupled to $parameter,\n".
							" two ETA(<number>) found ".
							"on $parameter = ... row\n" );
					}
				}
			}
			if ( $etanum ) {
				$parameter_eta{$parameter}=$etanum;
			}else{
				my $mes = "Could not determine the ETA coupled to $parameter\n";
				$mes .= " i.e. no $parameter = (expression with ETA) was ".
				"found in \$PK or \$PRED\n" ;
				croak($mes );
			}
			if ( length($relation) > 1 ) {
				$parameter_relation{$parameter}=$relation;
				if ($relation eq 'logit'){
					print "Detected ETA".$parameter_eta{$parameter}." added to the logit $parameter\n";
					print LOG "Detected ETA".$parameter_eta{$parameter}.
					" added to the logit $parameter\n";
				}else{
					print "Detected $relation ETA".$parameter_eta{$parameter}." on $parameter\n";
					print LOG "Detected $relation ETA".$parameter_eta{$parameter}." on $parameter\n";
				}
			}else{
				croak("Could not determine the ETA relation ".
					"(exponential/additive/proportional) for $parameter\n");
			}
		}
		print LOG "\nIf any of the above ETA relations is not correct all the following results will be wrong.\n".
		"--------------------\n" if (scalar(keys %{$self -> test_relations()})>0);
		close LOG;
		$self->parameter_eta(\%parameter_eta);
		$self->parameter_relation(\%parameter_relation);

		croak("too many problems in input model") 
		if (scalar(@{$original_model->problems}) > 1);

		#1.2 
		my @covariates=();
		if ( defined $self -> categorical_covariates() ) {
			push( @covariates, @{$self -> categorical_covariates()});
		}
		if ( defined $self -> continuous_covariates() ) {
			push( @covariates, @{$self -> continuous_covariates()});
		}


		#1.8
		my $singles = 3; # ID DV IPRED
		if ($self->epsilon()){
			#Hi1+Hi2+...+Hi(1+$nETA), i=1...$nEPS
			$singles = $singles + (1 + $nETA)*$nEPS;
		}else{
			$singles++ unless ($self->error eq 'user'); #W
			$singles++ if ($self->error eq 'propadd'); #WP
		}
		my @extra_parameters=();

		if (defined $self->do_not_drop and (scalar(@{$self->do_not_drop}) > 0)){
			#separate between parameters that should be put in $TABLE and covariates that 
			#should not be dropped
			my @do_not_drop=();
			foreach my $par (@{$self -> do_not_drop}){
				my $extra_param=1;
				foreach my $cov (@covariates){
					if ($cov eq $par){
						#this is a covariate, put in do_not_drop
						push(@do_not_drop,$par);
						$extra_param=0;
						last;
					}
				}
				push (@extra_parameters,$par) if ($extra_param);
			}
			$singles = $singles + scalar(@extra_parameters);
			$self->do_not_drop(\@do_not_drop); #reset to only covariates
		}

		$singles++;# unless (defined $original_model->problems->[0]-> preds and defined $mdv);

		my $num=0;
		if ($self->foce()){
			if ($self->second_order()){
				$num = (scalar(@covariates)+2*$nETA+$nETA*($nETA+1)/2+$singles+2*$n_param);
			}else{
				$num =(scalar(@covariates)+2*$nETA+$singles+2*$n_param);
			}
		}else{
			if ($self->second_order()){
				$num = (scalar(@covariates)+$nETA+$nETA*($nETA+1)/2+$singles+2*$n_param);
			}else{
				$num = (scalar(@covariates)+$nETA+$singles+2*$n_param);
			}
		}

		$self->data_items($num);

		my $use_tableformat = 0;

		if($self->data_items() > $self->max_data_items){
			if ($PsN::nm_minor_version >= 2){
				$use_tableformat = 1;
				my $max = $self->data_items();

				my $pdt_value = $original_model->get_option_value( option_name => 'PDT',
					record_name => 'sizes',
					fuzzy_match => 0);

				if (defined $pdt_value){
					$max=$pdt_value if ($pdt_value > $max);
				}
				my $pd_value = $original_model->get_option_value( option_name => 'PD',
					record_name => 'sizes',
					fuzzy_match => 0);

				if (defined $pd_value){
					$max=$pd_value if ($pd_value > $max);
				}
				$self->sizes_pd($max);


				if (defined $original_model ->problems->[0]->sizess() 
						and scalar(@{$original_model ->problems->[0]->sizess()})>0){
					$original_model -> set_option(record_name => 'sizes',
						record_number => 1,
						option_name => 'PD',
						option_value => $self->sizes_pd(),
						fuzzy_match => 0);

				}else{
					$original_model -> add_records( type => 'sizes',
						record_strings => [ " PD=".$max ] );
				}

			}else{
				my $mess = "$num items needed in \$INPUT, too many for NONMEM. ".
				"Use NM version 7.2 or later, which can handle more items in \$INPUT.".
				"If you are already using 7.2 or later, check that the version info in psn.conf is correct.";
				croak($mess);
			}
		}


		#create derivatives_model from original model (copy)
		$derivatives_model = $original_model -> copy ( filename => 'derivatives.mod',
													   output_same_directory => 1,
													   copy_datafile          => 0,
													   write_copy => 0,
													   copy_output        => 0);
		$derivatives_model->remove_records( type => 'table' );

		if ($self->sizes_pd() > 0){
			#need to set $SIZES PDT
			if (defined $derivatives_model ->problems->[0]->sizess() 
					and scalar(@{$derivatives_model ->problems->[0]->sizess()})>0){
				$derivatives_model -> set_option(record_name => 'sizes',
					record_number => 1,
					option_name => 'PDT',
					option_value => $self->sizes_pd(),
					fuzzy_match => 0);

			}else{
				$derivatives_model -> add_records( type => 'sizes',
					record_strings => [ " PDT=".$self->sizes_pd() ] );
			}
		}

#needed if not run_linearized_base???
#	  $derivatives_model -> filename('derivatives.mod');

		#1.1
		if( $self->lst_file ){
			$derivatives_model -> update_inits (from_output_file => $self->lst_file());
		}

		#1.9
		my @tablestrings = ('ID','DV');
		my @inputstrings = ('ID','DV');
		my $table_highprec=1;
		my $table_lowprec=0;

		push(@tablestrings,'MDV');
		push(@inputstrings,'MDV');
		$table_highprec++;

		#1.10

		if ($self->foce()){
			push(@tablestrings,'IPRED=OPRED');
			$table_highprec++;
		}else{
			push(@tablestrings,'PREDI=OPRED');
			$table_highprec++;
		}

		push(@inputstrings,'OPRED');
		#1.11
		if ($self->epsilon()){
			for (my $j=1; $j<=$nEPS; $j++){
				if ($j<10){
					push(@tablestrings,('H0'.$j.'1'));
					$table_highprec++;
				}else{
					push(@tablestrings,'H'.$j.'1');
					$table_highprec++;
				}
				push(@inputstrings,'D_EPS'.$j);
			}
		}else{
			if ($self->error eq 'propadd'){
				push(@tablestrings,'WP');
				$table_highprec++;
				push(@inputstrings,'OWP');
				push(@tablestrings,'WA');
				$table_highprec++;
				push(@inputstrings,'OWA');
			}elsif ($self->error eq 'add'){
				push(@tablestrings,'W');
				$table_highprec++;
				push(@inputstrings,'OW');
			}elsif ($self->error eq 'prop'){
				push(@tablestrings,'W');
				$table_highprec++;
				push(@inputstrings,'OW');
			}elsif ($self->error eq 'exp'){
				push(@tablestrings,'WE');
				$table_highprec++;
				push(@inputstrings,'OWE');
			}elsif ($self->error eq 'user'){
				1;
			}else{
				croak('Unknown error type '.$self->error);
			}
		}
		#these may be needed for user error code, or for IGNORE
		push(@tablestrings,@extra_parameters);
		$table_highprec = $table_highprec+scalar(@extra_parameters);
		push(@inputstrings,@extra_parameters);

		for (my $i=1;$i<=$nETA;$i++){
			if ($i<10){
				push(@tablestrings,('G0'.$i.'1'));
				$table_highprec++;
			}else{
				push(@tablestrings,('G'.$i.'1'));
				$table_highprec++;
			}
			push(@inputstrings,('D_ETA'.$i));
		}
		if ($self->foce()){
			for (my $i=1;$i<=$nETA;$i++){
				push(@tablestrings,('ETA'.$i));
				$table_highprec++;
				push(@inputstrings,('OETA'.$i));
			}
		}

		#handle second order and linearized epsilon imitate cwres
		if ($self->second_order() or $self->epsilon()){

			my $new_comresno = $nEPS*$nETA;
			$new_comresno += $nETA*($nETA+1)/2 if ($self->second_order());
			my $comresno;
			if (defined $derivatives_model ->problems->[0]->abbreviateds() 
					and scalar(@{$derivatives_model ->problems->[0]->abbreviateds()})>0){
				# Get current comres number
				$comresno = $derivatives_model->get_option_value( option_name => 'COMRES',
					record_name => 'abbreviated');

				$new_comresno += $comresno if ( defined $comresno );
				$derivatives_model->set_option( option_name => 'COMRES',
					record_name => 'abbreviated',
					fuzzy_match => 1,
					option_value => $new_comresno);
			}else {
				# Add $ABBREVIATED if necessary
				$derivatives_model -> add_records( type => 'abbreviated',
					record_strings => [ "COMRES=".($new_comresno) ] );
			}

			#can look for ADVAN<any number> this way
			my ($advan,$junk) = $derivatives_model->problems->[0] -> _option_val_pos( record_name => 'subroutine',
				name => 'ADVAN',
				exact_match => 0);
			my $have_advan = scalar(@{$advan}) > 0;

			my $code_records;
			my $H='H';
			if( $have_advan ){
				# We have and ADVAN option in $SUBROUTINE, get $ERROR code
				$code_records = $derivatives_model->problems->[0]-> errors();
				$H='HH';
			} else {
				# No ADVAN subroutine, we should modify $PRED code
				$code_records = $derivatives_model->problems->[0] -> preds;
			}

			# Get code array reference, so we can update the code inplace.
			my $code = $code_records -> [0] -> verbatim_last;

			unless( defined $code ){
				$code = [];
				$code_records -> [0] -> verbatim_last($code);
			}

			my $com = defined $comresno ? $comresno + 1 : 1;
			for(my $i= 1; $i<=$nEPS;$i++ ){
				for(my $j= 1; $j<=$nETA;$j++ ){
					push( @{$code},"\"  COM($com)=$H($i,".($j+1).")" );
					push( @tablestrings, "COM($com)=D_EPSETA$i"."_$j");
					$table_highprec++;
					push( @inputstrings, "D_EPSETA$i"."_$j");
					$com++;
				}
			}
			if ($self->second_order()){
				for(my $i= 1; $i<=$nETA;$i++ ){
					for(my $j= 1; $j<=$i;$j++ ){
						push( @{$code},"\"  COM($com)=G($i,".($j+1).")" );
						push( @tablestrings, "COM($com)=D2_ETA$i"."_$j");
						$table_highprec++;
						push( @inputstrings, "D2_ETA$i"."_$j");
						$com++;
					}
				}
			}
		} #end second_order or epsilons
		#1.12
		push(@tablestrings,@covariates);
		$table_highprec = $table_highprec + scalar(@covariates);
		push(@inputstrings,@covariates);

		#GZs and GKs are added to code further down, add_code_gfunc
		foreach my $parameter ( keys %{$self -> test_relations()} ){
			push(@tablestrings,'OGZ_'.$parameter);
			$table_lowprec++;
			push(@inputstrings,'OGZ_'.$parameter);
			push(@tablestrings,'OGK_'.$parameter);
			$table_lowprec++;
			push(@inputstrings,'OGK_'.$parameter);
		}

		#1.13
		$datafilename = 'derivatives_covariates.dta';
		if($linearize_only){
			$datafilename = $self->basename.'.dta';
		}

		push(@tablestrings,'NOPRINT','NOAPPEND','ONEHEADER');
		push(@tablestrings,'FILE='.$datafilename);
		$derivatives_model -> set_records(type => 'table',
			record_strings => \@tablestrings);

		if ((scalar(@tablestrings)-4) == (1+$table_highprec+$table_lowprec)){
			if ($use_tableformat){
				#more than 50 items, try to reduce field width to not hit 999 limit in NM7.2
				#do not use RFORMAT with space in specification, can get line break there.
				if ( ($table_lowprec+$table_highprec)*12> 999){
					#default format will give too long line
					#figure out how wide fields are ok
					my $width = int(999/($table_lowprec+$table_highprec)); #round down towards 0
					my $form = 's1PE'.$width.'.4'; #just hope this will work
					$derivatives_model -> add_option(record_name => 'table',
						option_name => 'FORMAT',
						option_value=> $form);
				}
			}

		}else{
			croak("Bug in setting table format when preparing derivatives model, please report\n".
				"highprec $table_highprec lowprec $table_lowprec items ".(scalar(@tablestrings)-4));
		}

		if ($self->update_derivatives()){
			#store derivatives_base_model if update_derivatives
			$self->derivatives_base_model($derivatives_model -> 
										  copy ( filename => 'derivatives_base.mod',
												 output_same_directory => 1,
												 copy_datafile =>0,
												 write_copy => 0,
												 copy_output        => 0));
		}
		#need to store GK and GZ funcs for covariates
		my %parameter_G;
		foreach my $parameter ( keys %{$self -> test_relations()} ){
			$parameter_G{$parameter}=1; #will later reset the ones that are included
			$parameter_G{$parameter}=0 if ($self->sum_covariates_hash->{$parameter}==1);
		}

		#add included relations add_code normal
		foreach my $incl_par ( sort keys %included_relations ) {
			foreach my $incl_cov ( sort keys %{$included_relations{$incl_par}} ) {
				$self -> 
				add_code( definition_code => $included_relations{$incl_par}{$incl_cov}{'code'},
					nthetas         => $included_relations{$incl_par}{$incl_cov}{'nthetas'},
					inits           => $included_relations{$incl_par}{$incl_cov}{'inits'},
					bounds          => $included_relations{$incl_par}{$incl_cov}{'bounds'},
					applicant_model => $derivatives_model,
					sum_covariates  => $self->sum_covariates_hash->{$incl_par},
					parameter       => $incl_par,
					covariate       => $incl_cov );
				#add in inner loop, otherwise may get params not included
				$parameter_G{$incl_par}=$incl_par.'COV';
			}
		}

		$self -> 
		add_code_gfunc( applicant_model => $derivatives_model,
			parameter_G => \%parameter_G,
			parameter_relation => \%parameter_relation);

		$derivatives_model ->_write(); #so that changes are reflected on disk

		#3.2, 3.3
		my @code; 
		@code = @{$original_model -> pk( problem_number => 1 )};
		unless ( $#code > 0 ) {
			@code = @{$original_model -> pred( problem_number => 1 )};
		}
		if ( $#code <= 0 ) {
			croak("Neither PK or PRED defined in " .
				$original_model -> filename . "\n" );
		}
		my $found_anchor = -1;
		my $i = 0;
		for ( @code ) {
			if ( /^;;;SCM-ANCHOR/) {
				$found_anchor = $i;
				last;
			}
			$i++;
		}

		#set IGNORE=@ since datafile will
		#get a header since it is a table file. Keep IGNORE=LIST
		$original_model->problems->[0]->datas->[0]->ignoresign('@');

		my $mceta = $original_model->get_option_value(record_name => 'estimation',
			option_name => 'MCETA',
			fuzzy_match => 1);

		$original_model->remove_records( type => 'theta' );
		$original_model->remove_records( type => 'table' );
		$original_model->remove_records( type => 'pk' );
		$original_model->remove_records( type => 'des' );
		$original_model->remove_records( type => 'error' );
		$original_model->remove_records( type => 'subroutine' );
		$original_model->remove_records( type => 'model' );
		$original_model->remove_records( type => 'covariance' );
		$original_model->remove_records( type => 'estimation' );
		#3.5
		$original_model -> set_records(type => 'input',
			record_strings => \@inputstrings);
		#3.6
		my @eststrings;
		if (defined $mceta){
			push(@eststrings,'MCETA='.$mceta);
		}

		if ($self->epsilon() or $self->error eq 'propadd' or $self->error eq 'prop'
				or $self->error eq 'exp' or $self->error eq 'user'){
			push(@eststrings,'METHOD=COND','INTERACTION');
		}else{
			push(@eststrings,'METHOD=ZERO');
		}
		push(@eststrings,$self->format) if (defined $self->format());
		if ($self->noabort()){
			push(@eststrings,'NOABORT');
		}
		$original_model -> set_records(type => 'estimation',
			record_strings => \@eststrings);
		#3.7
		my @pred_block; #make one element per row
		if ($found_anchor >= 0){
			@pred_block =  (@code[0..$found_anchor]);
		}
		my $base_count=0; #base count
		my $cov_count=0;

		my %eta_parameter;
		foreach my $parameter (keys %parameter_eta){
			if (defined $eta_parameter{($parameter_eta{$parameter})}){
				croak("ETA(".$parameter_eta{$parameter}.") affects both ".
					"$parameter and ".$eta_parameter{($parameter_eta{$parameter})}." but ".
					"the linearize option is only implemented for ".
					"models where each ETA appears on only one parameter.");
			}
			$eta_parameter{($parameter_eta{$parameter})} = $parameter;
		}
		#add code for covariate relations, will give zeros GZ-OGZ=1-1 in base model
		my @temp_block=();
		for (my $i=1;$i<=$nETA;$i++){
			#not all ETAs have a parameter
			if (defined ($eta_parameter{$i})){
				my $param = $eta_parameter{$i};
				if ($self->sum_covariates_hash->{$param}==1){
					push(@pred_block, ( ";;; $param-RELATION START\n",
							";;; This is 0 for logit parameters without covariate added\n",
							"GZ_$param"." = 0\n",
							";;; $param-RELATION END\n\n"));
				}else{
					push(@pred_block, ( ";;; $param-RELATION START\n",
							";;; This is 1 for parameters without covariate added\n",
							"GZ_$param"." = 1\n",
							";;; $param-RELATION END\n\n"));
				}
				$cov_count++;
				my $cname='COV'.$cov_count;
				push(@temp_block,$cname.'=D_ETA'.$i.'*OGK_'.$param.'*(GZ_'.$param.'-OGZ_'.$param.')');
			}else {
				my $param = 'ETA'.$i;
				$eta_parameter{$i}=$param;
				if ($self->second_order()){
					#probably important to have OGZ first and GZ last, it is last (GZ) that will be modified
					push(@pred_block, ( ";;; $param-RELATION START\n",
							";;; This will always be 1, no parameter with covariates for $param\n",
							"OGZ_$param"." = 1\n",
							"GZ_$param"." = 1\n",
							";;; $param-RELATION END\n\n"));
					$cov_count++;
				}
			}
		}
		#now all ETAs have parameter in hash eta_parameter

		push(@pred_block,@temp_block);

		if ($self->second_order()){

			#could just skip the ones where eta_parameter is undefined for j, set 0 factor if undef for i
			for (my $i=1;$i<=$nETA;$i++){
				my $parami = $eta_parameter{$i};
				for (my $j=1;$j<=$nETA;$j++){
					my $eta = ($j>$i) ? 'D2_ETA'.$j.'_'.$i :'D2_ETA'.$i.'_'.$j;
					my $paramj = $eta_parameter{$j};
					$cov_count++;
					my $cname='COV'.$cov_count; #($nETA+$i*($i-1)/2+$j);
					push(@pred_block,$cname.
						'='.$eta.'*0.5*(OGK_'.$parami.'*(GZ_'.$parami.'-OGZ_'.$parami.')*OGK_'.$paramj.'*(GZ_'.$paramj.'-OGZ_'.$paramj.')'.
						'+(ETA('.$i.')-OETA'.$i.')*OGK_'.$paramj.'*(GZ_'.$paramj.'-OGZ_'.$paramj.'))');
				}
			}
		} #end if second_order

		my $sum_count=1;
		my $sum_string='CSUM'.$sum_count;
		my $cov_string='COV_TERMS='.$sum_string;
		$sum_string .= '=';
		for (my $i=1;$i<=$cov_count;$i++){
			$sum_string .= 'COV'.$i;
			if ($i<$cov_count){
				if (length($sum_string) >= 63 ){
					push(@pred_block,$sum_string);
					$sum_count++;
					$sum_string='CSUM'.$sum_count;
					$cov_string .= '+'.$sum_string ;
					$sum_string .= '=';
				}else{
					$sum_string .= '+';
				}
			}
		}
		push(@pred_block,$sum_string)  if ($cov_count>0);
		push(@pred_block,$cov_string) if ($cov_count>0);

		for (my $i=1;$i<=$nETA;$i++){
			if ($self->foce()){
				push(@pred_block,'BASE'.$i.'=D_ETA'.$i.'*(ETA('.$i.')-OETA'.$i.')');
			}else{
				push(@pred_block,'BASE'.$i.'=D_ETA'.$i.'*ETA('.$i.')');
			}
			$base_count++;
		}

		if ($self->second_order()){
			for (my $i=1;$i<=$nETA;$i++){
				for (my $j=1;$j<=$nETA;$j++){
					$base_count++; 
					my $eta = ($j>$i) ? 'D2_ETA'.$j.'_'.$i :'D2_ETA'.$i.'_'.$j;
					push(@pred_block,'BASE'.$base_count.
						'='.$eta.'*0.5*(ETA('.$i.')-OETA'.$i.')*(ETA('.$j.')-OETA'.$j.')');
				}
			}
		}
		my $sum_count=1;
		my $sum_string='BSUM'.$sum_count;
		my $base_string='BASE_TERMS='.$sum_string;
		$sum_string .= '=';
		for (my $i=1;$i<=$base_count;$i++){
			$sum_string .= 'BASE'.$i;
			if ($i<$base_count){
				if (length($sum_string) >= 63){
					push(@pred_block,$sum_string);
					$sum_count++;
					$sum_string='BSUM'.$sum_count;
					$base_string .= '+'.$sum_string ;
					$sum_string .= '=';
				}else{
					$sum_string .= '+';
				}
			}
		}
		push(@pred_block,$sum_string);
		push(@pred_block,$base_string);

		my $sum_string='IPRED=OPRED+BASE_TERMS';
		$sum_string .='+COV_TERMS' if ($cov_count>0);
		push(@pred_block,$sum_string);
		#3.9
		if ($self->epsilon()){
			my $err_count=0;
			for (my $i=1; $i<= $nEPS; $i++){
				my $eps = 'EPS('.$i.')';
				$err_count++;
				my $string = 'ERR'.$err_count.'='.$eps.'*(D_EPS'.$i;
				for (my $j=1; $j<= $nETA; $j++){
					my $line = 'D_EPSETA'.$i.'_'.$j.'*(ETA('.$j.')-OETA'.$j.')';
					if (length($string.$line) >= 63){
						$string .= ')'."\n";
						push(@pred_block,$string);
						$err_count++;
						$string = 'ERR'.$err_count.'='.$eps.'*(';
					}else{
						$string .= '+';
					}
					$string .= $line;
				}#end inner loop eta
				$string .= ')'."\n";
				push(@pred_block,$string);
				if ($cov_count>0){
					$err_count++;
					my $string = 'ERR'.$err_count.'='.$eps.'*(';
					#now loop over params
					for (my $j=1;$j<=$nETA;$j++){
						if (defined ($eta_parameter{$j})){
							my $param = $eta_parameter{$j};
							next if ($param eq 'ETA'.$j);
							my $line = 'D_EPSETA'.$i.'_'.$j.'*OGK_'.$param.'*(GZ_'.$param.'-OGZ_'.$param.')';
							if (length($string.$line) >= 63){
								$string .= ')'."\n";
								push(@pred_block,$string);
								$err_count++;
								$string = 'ERR'.$err_count.'='.$eps.'*(';
							}else{
								$string .= '+' if (length($string) > 15);
							}
							$string .= $line;
						}
					}
					$string .= ")\n";
					push(@pred_block,$string);
				}#end if cov_count>0
			} #end loop over eps

			my $sum_count=1;
			my $sum_string='ESUM'.$sum_count;
			my $err_string='ERROR_TERMS='.$sum_string;
			$sum_string .= '=';
			for (my $i=1;$i<=$err_count;$i++){
				$sum_string .= 'ERR'.$i;
				if ($i<$err_count){
					if (length($sum_string) >= 63){
						push(@pred_block,$sum_string);
						$sum_count++;
						$sum_string='ESUM'.$sum_count;
						$err_string .= '+'.$sum_string ;
						$sum_string .= '=';
					}else{
						$sum_string .= '+';
					}
				}
			}
			push(@pred_block,$sum_string);
			push(@pred_block,$err_string);

			push(@pred_block,'Y=IPRED+ERROR_TERMS'."\n");
		}else{
			if ($self->error eq 'add'){
				push(@pred_block,'Y=IPRED+OW*EPS(1)'."\n");
			}elsif ($self->error eq 'prop'){
				push(@pred_block,'W=IPRED*OW'."\n");
				push(@pred_block,'IF (OPRED.NE.0) W=IPRED*(OW/OPRED)'."\n");
				push(@pred_block,'Y=IPRED+W*EPS(1)'."\n");
			}elsif ($self->error eq 'propadd'){
				push(@pred_block,'W=SQRT(OWA**2+(OWP*IPRED)**2)'."\n");
				push(@pred_block,'Y=IPRED+W*EPS(1)'."\n");
			}elsif ($self->error eq 'exp'){
				push(@pred_block,'Y=IPRED*EXP(EPS(1)*OWE)'."\n");
			}elsif ($self->error eq 'user'){
				push(@pred_block,@{$self->error_code()});
				push(@pred_block,"\n");
			}else{
				croak("Unknown error form ".$self->error);
			}	   
		}
		$original_model -> set_records( type => 'pred',
			record_strings => \@pred_block );

		$original_model -> fixed( parameter_type => 'sigma',
			new_values => [[(0) x $original_model -> nsigmas -> [0] ]] );

		#end if step_number == 1
	}elsif ($self->update_derivatives()){
		$datafilename = 'derivatives_covariates'.$stepname.'.dta';
		if ($self->step_number() == 2 and $self->both_directions() 
				and $self->search_direction() eq 'backward'){
			#if first backward step after switching directions we do not have to rerun 
			#derivatives if no covariates were significant in the last step == unless 
			#all covariates are in their final state. Instead copy derivatives_covariates.dta
			#from forward directory (check exists so that no cleaning has removed it)
			#to be used by linearized model. 
			#if we need to rerun then at least find lst-file of last derivatives run to be used for update_inits

			#we want to reuse the last derivatives run completely if no covariates were significant
			#in last step
			#check if all covariates are in their final state. Then rerun using update_inits, 
			#Otherwise reuse

			#if update_derivatives then want to find output of deepest derivatives run
			#use this as derivatives output to get best initial estimates for new derivatives run

			my $old_derivatives = '../derivatives.';
			my $exists = (-e $old_derivatives.'lst');
			#no not die here, could have derivatives_data set.
			#could still be derivatives from old run
			#croak("No original derivatives.lst") unless ($exists);
			my $dir = '../';
			my $new_file = 'derivatives_updated_1f.';
			my $new_dir = '../forward_scm_dir1/';
			my $level=1;
			while (-e $new_dir.$new_file.'lst'){
				$exists = 1;
				$dir = $new_dir;
				$old_derivatives = $dir.$new_file;
				$level++;
				$new_dir .='scm_dir1/';
				$new_file = 'derivatives_updated_'.$level.'f.';
			}
			$level=$level-1;
			my $stepname = ($level == 0)? '': "_$level".'f';
			$self->derivatives_output(output->new( filename => $old_derivatives.'lst')) 
			if ($exists);

			if (-e $dir.'derivatives_can_be_reused'){
				$rerun_derivatives_new_direction = 0;
				#copy deepest derivatives_covariates.dta from forward_scm_dir and put it locally with correct name
				# $datafilename
				if (-e $dir.'derivatives_covariates'.$stepname.'.dta' ){
					cp( $dir.'derivatives_covariates'.$stepname.'.dta', "$datafilename" );
					cp( $old_derivatives.'mod', 'copy_last_forward_derivatives.mod');
					cp( $old_derivatives.'lst', 'copy_last_forward_derivatives.lst');
				}else{
					ui -> print( category => 'scm',
						message  => "cannot find old derivatives data, rerunning",
						newline => 1);
					$rerun_derivatives_new_direction =1; #cannot find data
				}
			}
		}

		if ($rerun_derivatives_new_direction){
			$derivatives_model = $self->derivatives_base_model() -> 
				copy ( filename => 'derivatives_updated'.$stepname.'.mod',
					   output_same_directory => 1,
					   copy_datafile          => 0,
					   write_copy       => 0,
					   copy_output        => 0);
			$derivatives_model -> directory($self->directory());
			$derivatives_model->outputs->[0]->problems([]); #remove output so will be rerun

			$derivatives_model->set_option( option_name => 'FILE',
				record_name => 'table',
				fuzzy_match => 0,
				option_value => $datafilename,
				problem_numbers=>[1]);

			#need to store GZ and GK funcs for covariates
			my %parameter_G;
			foreach my $parameter ( keys %{$self -> test_relations()} ){
				$parameter_G{$parameter}=1; #will later reset the ones that are included
				$parameter_G{$parameter}=0 if ($self->sum_covariates_hash->{$parameter}==1);
			}
			#add included relations add_code normal
			foreach my $incl_par ( sort keys %included_relations ) {
				foreach my $incl_cov ( sort keys %{$included_relations{$incl_par}} ) {
					$self -> 
					add_code( definition_code => $included_relations{$incl_par}{$incl_cov}{'code'},
						nthetas         => $included_relations{$incl_par}{$incl_cov}{'nthetas'},
						inits           => $included_relations{$incl_par}{$incl_cov}{'inits'},
						bounds          => $included_relations{$incl_par}{$incl_cov}{'bounds'},
						applicant_model => $derivatives_model,
						sum_covariates  => $self->sum_covariates_hash->{$incl_par},
						parameter       => $incl_par,
						covariate       => $incl_cov );
					#add in inner loop, otherwise will get parameters that are not included 
					$parameter_G{$incl_par}=$incl_par.'COV';
				}
			}
			#need drop_undrop here?
			$self -> 
			add_code_gfunc( applicant_model => $derivatives_model,
				parameter_G => \%parameter_G,
				parameter_relation => $self->parameter_relation());

			if (defined $self->derivatives_output()){
				$derivatives_model ->update_inits(from_output => $self->derivatives_output(),
					ignore_missing_parameters => 1,
					problem_number => 1);
			}else{
				ui -> print( category => 'scm',
					message  => "Warning: No output stored from previous derivatives run.",newline => 1 )
				unless ($self->derivatives_data());
			}
			$derivatives_model ->_write();
		} #end if rerun_derivatives
	} #end elsif update_derivatives

	if ($self->step_number()==1 or $self->update_derivatives()){
		my $derivatives_ofv;
		my $ofvname = 'ofv';
		my $derivatives_name;
		my $reused=0;
		#set name of datafile before creating it so that will not read data here
		$original_model -> ignore_missing_data(1);
		$original_model -> datafiles(new_names => [$datafilename]);

		if ($self->step_number()==1 and $self->derivatives_data()){
			#do not run derivatives model, instead copy file to right name
			cp( $self->derivatives_data(), $datafilename );
		} elsif ($rerun_derivatives_new_direction){
			#run derivatives_model
			my $derivatives_fit = tool::modelfit -> new
			( %{common_options::restore_options(@common_options::tool_options)},
				base_directory => $self -> directory,
				directory      => $self -> directory.'/derivatives_modelfit_dir/',
				models         => [$derivatives_model],
				top_tool       => 0);

			my $updated = '';
			if ($self->step_number()> 1) {
				if ($self->search_direction() eq 'forward'){
					$updated = ' with the added covariate relation.';
				}elsif ($self->step_number() == 2 and $self->both_directions()){
					$updated = ' with all included covariate relations.';
				}else{
					$updated = ' without the removed covariate relation.';
				}
			}

			ui -> print( category => 'all',
				message  => "Estimating derivatives model$updated",
				newline => 1);
			$derivatives_fit -> run;

			if (defined $derivatives_model->outputs() and 
				defined $derivatives_model->outputs()->[0] and
				$derivatives_model->outputs()->[0]-> have_output()){
				$self->run_xv_pred_step(estimation_model => $derivatives_model,
										model_name => 'xv_pred_derivatives',
										derivatives_run => 1) 
					if (defined $self->xv_pred_data);
				if ($self->update_derivatives()){
					#store derivatives output if update_derivatives so that can use that in next iteration
					$self->derivatives_output($derivatives_model -> outputs -> [0]);
				}
				if ( defined $derivatives_model -> outputs->[0]->  get_single_value(attribute=> 'ofv') ) {
					$derivatives_ofv = $derivatives_model -> outputs->[0]->  
						get_single_value(attribute=> 'ofv');
					$derivatives_name = $derivatives_model ->filename();
				}else{
					print "Warning: could not retrieve OFV from derivatives model.\n";
				}
			}else{
				ui->print (category => 'scm',
						   message => "Warning: No output from derivatives run. Unexpected.",
						   newline => 1);
			}
		}else{
			ui -> print( category => 'scm',
				message  => "Reusing the derivatives model output from the ".
				"last step in the forward search.",
				newline => 1);
			if ( defined $self->derivatives_output() ->  get_single_value(attribute=> 'ofv') ) {
				$derivatives_ofv = $self->derivatives_output()-> get_single_value(attribute=> 'ofv');
				#$ofvname = 'DIC' if 
				#	  (defined $self->derivatives_output()->get_single_value(attribute=>'dic')); #error
				$derivatives_name = $self->derivatives_output()->filename();
				$derivatives_name =~ s/\.lst$/\.mod/;
				open( LOG, ">>".$self -> logfile -> [0] ); #model_number -1
				print LOG "Reusing the derivatives model output from the last step in the forward search.\n";
				close LOG;
				$reused=1;
			}else{
				print "Warning: could not retrieve OFV from derivatives model.\n";
			}
		}
		my $ofv;
		if (defined $derivatives_ofv){
			$ofv = sprintf("%12.5f",$derivatives_ofv);
		}else{
			$ofv = 'NA';
		}
		if ($self->run_linearized_base()){
			open( LOG, ">>".$self -> logfile -> [0] ); #model_number -1
			if ($self->step_number()==1){
				print LOG "The $ofvname of the nonlinear base model :$ofv        $derivatives_name\n";
				ui->print(category => 'linearize',
					message =>"\nThe $ofvname of the nonlinear base model :$ofv        $derivatives_name\n");
			}else{
				if ($reused){
					print LOG "The $ofvname of the nonlinear model              :$ofv        $derivatives_name\n";
				}else{
					print LOG "The $ofvname of the updated nonlinear model      :$ofv        $derivatives_name\n";
				}
			}
			close LOG;
		}else{
			if (($self->step_number() > 1) 
					or not ( defined $self -> base_criteria_values and 
					defined $self -> base_criteria_values -> {'ofv'})) {
				$self -> base_criteria_values -> {'ofv'} = $derivatives_ofv; #assuming $model_number==1
			}
			if ($self->update_derivatives() and $self->step_number()>1){
				open( LOG, ">>".$self -> logfile -> [0] ); #model_number -1
				print LOG "Updated base ofv value from derivatives run:$ofv        $derivatives_name\n";
				print LOG "--------------------\n\n";
				close LOG;
			}
		}

		if (($self->run_linearized_base() and (not $self->return_after_derivatives_done()))
				or $linearize_only){
			#transform $original_model to linearized base model
			$original_model -> filename('base_model'.$stepname.'.mod');
			$original_model -> outputfile('base_model'.$stepname.'.lst');
			$original_model -> directory($self->directory());
			#remove problem data from output object so that scm will rerun
			$original_model->outputs->[0]->problems([]);
			#update linearized model with estimates from derivatives model
			#3.1
			if ($self->step_number()==1 and $self->derivatives_data()){
				1;
				#have no derivatives output
			}elsif ($rerun_derivatives_new_direction){
				#have fresh output
				$original_model->update_inits(from_output => $derivatives_model->outputs->[0],
					ignore_missing_parameters => 1,
					problem_number => 1);
			}else{
				#have output stored
				$original_model->update_inits(from_output => $self->derivatives_output(),
					ignore_missing_parameters => 1,
					problem_number => 1);
			}
			#3.4
			$original_model -> _write();

		}else{
			$original_model -> _write() unless ($self->return_after_derivatives_done());
		}
	} #end if first step or update derivatives

	return $original_model;
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	return if ($self->return_after_derivatives_done());
	return if (($self->max_steps() == 0) and ($self->step_number()==1) and 
		scalar(keys %{$self->test_relations}) == 0
			and $self->linearize);

	# Own_threads is used to set parent_threads for child tools
	my $own_threads = ref( $self -> threads ) eq 'ARRAY' ?
	$self -> threads -> [0]:$self -> threads;
	# More threads than models?
	my $num = scalar @{$self -> models};
	$own_threads = $num if ( $own_threads > $num );

	my @results = @{$self -> results};

	my @minimizations;
	for ( my $i = 0; $i < scalar @{$self -> prepared_models->[$model_number-1]{'own'}}; $i++ ) {
		if (defined $self -> prepared_models->[$model_number-1]{'own'}[$i]->outputs() and 
			defined $self -> prepared_models->[$model_number-1]{'own'}[$i]->outputs()->[0] and
			$self -> prepared_models->[$model_number-1]{'own'}[$i]->outputs()->[0]-> have_output() ){
			my $term = $self -> prepared_models->[$model_number-1]{'own'}[$i] ->
			outputs -> [0] -> minimization_successful;
			push( @minimizations, $term->[0][0] );
		}else{
			push( @minimizations, 0 );
		}
	}
	# This loop checks the termination of all model fits
	foreach my $mod_return_type ( @{$results[$model_number-1]{'subtools'}[0]} ){
		my $crash = 0;
		my $crash_name;
		foreach my $type ( @{$mod_return_type -> {'own'}} ) {
			if ( $type -> {'name'} eq 'minimization_successful' ){
				for ( my $i = 0; $i < scalar @{$type -> {'values'}}; $i++ ) {
					for ( my $j = 0; $j < scalar @{$type -> {'values'}[$i]}; $j++ ) {
						if ( $self -> picky ) {
# Did minimization just loose one dimension?
							if ( not defined $type -> {'values'}[$i][$j] or 
								$type -> {'values'}[$i][$j] != 1 ) {
								$crash = 2;
							}
						} else {
							if ( not defined $type -> {'values'}[$i][$j] or 
								$type -> {'values'}[$i][$j] <= 0 ) {
								$crash = 1;
							}
						}
					}
				}
			}
			$crash_name = $type -> {'values'}[0] if ( $type -> {'name'} eq 'filename' );
		}
		if ( $crash == 2 and $self -> abort_on_fail ) {
			die "The execution of the modelfile ",
			$crash_name," failed (picky==1), aborting SCM, sorry\n";
		} elsif ( $crash == 1 and $self -> abort_on_fail ) {
			die "The execution of the modelfile ",
			$crash_name," failed (picky==0), aborting SCM, sorry\n";
		}
	}

	my %return_section;
	$return_section{'name'} = 'ofv.drop';
	$return_section{'values'} = [];
	$return_section{'labels'} = [];

	# Collect the drops. The $i is needed since the prepared
	# models are not indexed by parameters and covariate
	# combination
	my $base_ofv;
	if ( defined $self -> base_criteria_values and 
		defined $self -> base_criteria_values -> {'ofv'}) {
		$base_ofv = $self -> base_criteria_values -> {'ofv'};
	} else {
		if (defined $self -> models->[$model_number-1]->outputs() and 
			defined $self -> models->[$model_number-1]->outputs()->[0] and
			$self -> models->[$model_number-1]->outputs()->[0]-> have_output()){
			$base_ofv = $self -> models->[$model_number-1] ->
			outputs -> [0] -> get_single_value(attribute=> 'ofv'); #if we have run a start_model we do not want to use this, make sure base_criteria_values is defined if start_model has been run
		}else{
			$base_ofv=0;
			print "Warning: No base ofv. Using 0.\n";
		}
	}
	my $i = 0;

	foreach my $parameter ( sort keys %{$self -> relations()} ) {
		foreach my $covariate ( sort keys %{$self -> relations()->{$parameter}} ) {
			# Is this covariate continuous or not?
			my $continuous = 1;
			if (defined $self->categorical_covariates()){
				foreach my $cat ( @{$self -> categorical_covariates()} ) {
					$continuous = 0 if ( $covariate eq $cat );
				}
			}
			my @valid_states;
			if ( $continuous ) {
				@valid_states = @{$self -> valid_states->{'continuous'}};
			} else {
				@valid_states = @{$self -> valid_states->{'categorical'}};
			}

			my $state;
			# Important: just setting $state to $self->incl_rel....{'state'} initiates
			# included_relations for this parameter and covariate. Avoid this.
			if ( defined $self -> included_relations->{$parameter}{$covariate} ) {
				$state = $self -> included_relations->{$parameter}{$covariate}{'state'};
			}
			$state = defined $state ? $state : $valid_states[0];

			next if ( ( $self -> search_direction eq 'forward' and
					$state == $valid_states[$#valid_states] ) or
				( $self -> search_direction eq 'backward' and
					$state == $valid_states[0] ) );

			my $new_ofv;
			if (defined $self -> prepared_models->[$model_number-1] and
				defined $self -> prepared_models->[$model_number-1]{'own'} and
				defined $self -> prepared_models->[$model_number-1]{'own'}[$i] and
				defined $self -> prepared_models->[$model_number-1]{'own'}[$i]->outputs() and 
				defined $self -> prepared_models->[$model_number-1]{'own'}[$i]->outputs()->[0] and
				$self -> prepared_models->[$model_number-1]{'own'}[$i]->outputs()->[0]-> have_output()){
				$new_ofv = $self -> prepared_models->[$model_number-1]{'own'}[$i] -> outputs -> [0] -> get_single_value(attribute=> 'ofv');# -> [0][0];
			}
			$new_ofv = $base_ofv unless ( defined $new_ofv );
			# Only one problem and one sub problem
			push( @{$return_section{'values'}[0][0]}, $base_ofv - $new_ofv );
			$i++;
		}
	}
	push( @{$self -> results->[$model_number-1]{'own'}},
		\%return_section );
	my @ofv_changes;
	foreach my $par ( sort keys %{$self -> test_relations()} ) {
		foreach my $cov ( @{$self -> test_relations()->{$par}} ){
			push( @ofv_changes,
				$self -> relations()->{$par}{$cov}{'ofv_changes'} );
		}
	}
	my ( $chosen_parameter,
		$chosen_covariate,
		$chosen_state,
		$sign_models_ref,
		$test_vals_ref,
		$criterion,
		$test_log_ref,
		$new_base_crit_val_ref,
		$short_log_text,
		$short_log_ref ) = ( undef, undef, undef, undef, undef,
		undef, undef, undef, undef, undef );

	my $func;
	if ( lc($self -> gof()) eq 'ofv' ) {
		$func = \&gof_ofv;
	} elsif ( lc($self -> gof()) eq 'crc') {
		$func = \&gof_ofv;
	} elsif ( lc($self -> gof()) eq 'p_value') {
		$func = \&gof_pval;
	} 

	my $temp_res_mod;
	( $temp_res_mod,
		$chosen_parameter,
		$chosen_covariate,
		$chosen_state,
		$sign_models_ref,
		$test_vals_ref,
		$criterion,
		$test_log_ref,
		$new_base_crit_val_ref,
		$short_log_text,
		$short_log_ref )
	= $func -> ( $self,
		$self -> search_direction,
		$self -> models -> [$model_number-1],
		$model_number,
		\@ofv_changes);
	$self -> resulting_model($temp_res_mod);
	# Print a short summary of the step (All logging should be kept in a log-method in the future)
	open( LOG, ">>".$self -> short_logfile -> [$model_number-1] );
	print LOG $short_log_text;
	close( LOG );

	my %return_section;
	$return_section{'name'} = 'relation.chosen.in.step';
	$return_section{'values'} = [];
	$return_section{'labels'} = [];
	if ( defined $chosen_parameter and defined  $chosen_covariate ) {
		$return_section{'values'}[0][0] = $chosen_parameter.$chosen_covariate;
		my $task = $self -> search_direction eq 'forward' ? 'Adding' : 'Removing';
		ui -> print( category => 'scm',
			message  => "$task $chosen_covariate on $chosen_parameter state $chosen_state" )
		unless $self -> parent_threads > 1;
	}
	push( @{$self -> results->[$model_number-1]{'own'}},
		\%return_section );

	my $final_model;
	if ( defined $self -> resulting_model ) {
		# Promote and log the included relation:
		# Is this covariate continuous or not?
		my $continuous = 1;
		if (defined $self -> categorical_covariates()){
			foreach my $cat ( @{$self -> categorical_covariates()} ) {
				$continuous = 0 if ( $chosen_covariate eq $cat );
			}
		}
		my @valid_states;
		if ( $continuous ) {
			@valid_states = @{$self -> valid_states->{'continuous'}};
		} else {
			@valid_states = @{$self -> valid_states->{'categorical'}};
		}

		my $state;
		# Important: just setting $state to $self->incl_rel....{'state'} initiates
		# included_relations for this parameter and covariate. Avoid this.
		if ( defined $self -> included_relations->{$chosen_parameter}
			{$chosen_covariate} ) {
			$state = $self -> included_relations->{$chosen_parameter}
			{$chosen_covariate}{'state'};
		}
		$state = defined $state ? $state : $valid_states[0];
		$state = $chosen_state;
		# If the state is 1 (not included); remove the relation.
		if ( $state == 1 ) {
			# Check if this relation is the last for the parameter
			if ( scalar keys %{$self -> included_relations-> {$chosen_parameter}} == 1 ) {
				delete $self -> included_relations->{$chosen_parameter};
			} else {
				delete $self -> included_relations->{$chosen_parameter}
				{$chosen_covariate};
			}
		} else {
			$self -> included_relations->{$chosen_parameter}{$chosen_covariate}{'state'} = $state;
			$self -> included_relations->{$chosen_parameter}{$chosen_covariate}{'theta_estimates'} =
			$self -> resulting_model -> get_values_to_labels(category => 'theta');
			$self -> included_relations->{$chosen_parameter}{$chosen_covariate}{'code'} =
			$self -> relations()->{$chosen_parameter}{$chosen_covariate}{'code'}{$state};
			$self -> included_relations->{$chosen_parameter}{$chosen_covariate}{'nthetas'} =
			$self -> relations()->{$chosen_parameter}{$chosen_covariate}{'nthetas'}{$state};
			$self -> included_relations->{$chosen_parameter}{$chosen_covariate}{'inits'} =
			$self -> relations()->{$chosen_parameter}{$chosen_covariate}{'inits'}{$state};
			$self -> included_relations->{$chosen_parameter}{$chosen_covariate}{'bounds'} =
			$self -> relations()->{$chosen_parameter}{$chosen_covariate}{'bounds'}{$state};
		}

		$self -> write_log
		( direction          => $self -> search_direction,
			logfile            => $self -> logfile->[$model_number-1],
			included_relations => $self -> included_relations,
			chosen_parameter   => $chosen_parameter,
			chosen_covariate   => $chosen_covariate,
			chosen_state       => $chosen_state,
			results            => $self -> results->[$model_number-1]{'own'},
			criterion           => $criterion,
			test_log           => $test_log_ref,
			append             => $self -> append_log);

		# Check if there still are some states to test
		my $still_one_left = 0;
		foreach my $par ( sort keys %{$self -> test_relations()} ) {
			foreach my $cov ( @{$self -> test_relations()->{$par}} ){
				my $kind = 'continuous';
				if (defined $self -> categorical_covariates()){
					foreach my $cat ( @{$self -> categorical_covariates()} ) {
						$kind = 'categorical' if ( $cov eq $cat );
					}
				}
				#nothing to test if only one valid state
				next if (scalar(@{$self -> valid_states->{$kind}})<2);
				if ( defined $self -> included_relations->{$par}{$cov} ) {
					#tests left unless direction is forward and state is equal to last in seq
					$still_one_left = 1 unless ($self -> search_direction() eq 'forward' and
						( $self -> included_relations->{$par}{$cov}{'state'} ==
							$self -> valid_states->{$kind} ->
							[ scalar @{$self -> valid_states->{$kind}} - 1] )); 
				} else {
					#tests left unless direction is backward and there are no included relations
					$still_one_left = 1 unless ($self -> search_direction() eq 'backward');
				}
			}
		}

		my ( $returns, $prep_models );
		if ( $still_one_left and
			not (defined $self->max_steps() and $self->max_steps()<=$self->step_number())
		) {
			my $cpu_time = defined $self -> cpu_time ? int(($self -> cpu_time)*1.2) : undef;

			my $dir = $self -> directory.'/scm_dir'.$model_number;
			if ($self->search_direction eq 'forward' and $self->step_number()==1
					and $self->both_directions()){
				$dir = $self -> directory.'/forward_scm_dir'.$model_number;
			}
			my $internal_scm =
			tool::scm ->
			new( %{common_options::restore_options(@common_options::tool_options)},
				gof                    => $self -> gof(),
				test_relations         => $self -> test_relations,
				parameters             => $self -> parameters,
				check_nmtran            => 0,
				main_data_file            => $self->main_data_file,
				categorical_covariates => $self -> categorical_covariates(),
				continuous_covariates  => $self -> continuous_covariates(),
				do_not_drop            => $self -> do_not_drop,
				ofv_change             => $self -> ofv_change,
				p_value                => $self -> p_value,
				search_direction       => $self -> search_direction,
				valid_states           => $self -> valid_states,
				covariate_statistics_file => $self -> covariate_statistics_file,
				relations_file         => $self -> relations_file,
				short_logfile          => [$self -> short_logfile->[$model_number-1]],
				bounds                 => $self -> bounds,
				cpu_time             => $cpu_time,
				xv_pred_data          => $self -> xv_pred_data,
				max_steps             => $self -> max_steps,
				xv_results              => $self -> xv_results,
				global_init          => $self -> global_init,
				covariate_statistics => $self -> covariate_statistics,
				directory            => $dir,
				models               => [$self -> models->[$model_number-1]],
				relations            => $self -> relations(),
				initial_estimates_model => $self -> resulting_model,
				included_relations   => $self -> included_relations,
				append_log           => 1,
				step_number          => ($self -> step_number() + 1),
				raw_results_file     => [$self -> raw_results_file ->[$model_number-1]],
				logfile              => [$self -> logfile->[$model_number-1]],
				base_criteria_values => $new_base_crit_val_ref,
				parent_tool_id       => $self -> tool_id,
				parent_threads       => $own_threads , 
				top_tool             => 0,
				logit                => $self->logit(),
				linearize                 => $self->linearize,
				foce                 => $self->foce,
				second_order         => $self->second_order,
				only_successful        => $self->only_successful(),
				parameter_eta        => $self->parameter_eta,
				parameter_relation   => $self->parameter_relation,
				derivatives_base_model => $self->derivatives_base_model,
				derivatives_output    => $self->derivatives_output(),
				data_items    => $self->data_items(),
				sizes_pd    => $self->sizes_pd(),
				update_derivatives    => $self->update_derivatives(),
				error                 => $self->error(),
				error_code           => $self->error_code(),
				epsilon           => $self->epsilon(),
				parallel_states     => $self->parallel_states(),
				config_file          => undef,
				resulting_model      => undef,
				xv_results_file => $self->xv_results_file(),
				final_model_directory => $self->final_model_directory());

			ui -> print( category => 'scm',
				message  => "Taking a step " . $self -> search_direction )
			unless $self -> parent_threads > 1;
			$internal_scm -> run;
			$returns = $internal_scm -> results;
			$prep_models = $internal_scm -> prepared_models;
			ui -> print( category => 'scm',
				message  => $self -> search_direction . " search done." )
			unless ($self -> parent_threads > 1 or $self->step_number()>1);

			foreach my $return ( @{$returns ->[0]{'own'}} ) {
				if ( $return -> {'name'} eq 'base.criteria.values' ){
					$self -> base_criteria_values( $return -> {'values'} ); #FIXME ???Dereference, take first val of array?
				}
			}
		} else {
			#no relations left to add. write final models
			$self->write_final_models(final_model => $self -> resulting_model,
				model_number => $model_number);

			my @tmp_ret;
			$tmp_ret[0]{'own'}[0]{'name'}   = 'final.model';
			$tmp_ret[0]{'own'}[0]{'values'}[0][0] = 'basic.model';
			$returns = \@tmp_ret;

			$self -> base_criteria_values( $new_base_crit_val_ref);
		}

		if ( defined $prep_models ) {
			carp(" have called internal scm " .scalar @{$prep_models} );

			# Enclose $prep_models in array ref to reflect the
			# per-tool level, even though a modelfit does not
			# prepare any models itself
			#push ( @{$self -> prepared_models->[$model_number-1]{'subtools'}}, $prep_models -> [0]);

			# Push the results of the internal scm on the results attribute:
			#push( @{$self -> results->[$model_number-1]{'subtools'}}, $returns );
		} else {
			carp(" no prep_models defined from internal scm " );
		}

		# set final model to this steps' best model if the internal scm returned 'basic_model'.
		foreach my $return ( @{$returns ->[0]{'own'}} ) {
			$final_model = $return -> {'values'}[0][0] if ( $return -> {'name'} eq 'final.model' );
		}

		if ( not defined $final_model) {
			#this works if none significant in step below and not first step
			$self->write_final_models(final_model => $self -> resulting_model,
				model_number => $model_number);

			$self -> write_log
			( direction          => $self -> search_direction,
				logfile            => $self -> short_logfile->[$model_number-1],
				included_relations => $self -> included_relations,
				chosen_parameter   => $chosen_parameter,
				chosen_covariate   => $chosen_covariate,
				chosen_state       => $chosen_state,
				results            => $self -> results->[$model_number-1]{'own'},
				criterion          => $criterion,
				test_log           => $test_log_ref,
				append             => 1,
				final_short => 1);
		}
	} else {
		#no resulting model defined, i.e. none significant.
		# No resulting model from gof. This is the last step.

		#write final if this is first step (otherwise will write above)
		if ($self->step_number() == 1){
			if (defined $self -> initial_estimates_model){
				$self->write_final_models(final_model => $self -> initial_estimates_model,
					model_number => $model_number);
			}else{
				1;
			}
		}

		$self -> write_log ( direction         => $self -> search_direction,
			criterion          => $criterion,
			logfile            => $self -> logfile->[$model_number-1],
			results            => $self -> results->[$model_number-1]{'own'},
			append             => $self -> append_log);


		if( $self->update_derivatives()){
			#we have run the actual final nonlinear model, it is the derivatives model of the level below this
			if ($self->search_direction() eq 'forward'){
				#print a signal file
				open( LOG, ">".'derivatives_can_be_reused' );
				print LOG "derivatives from this directory can be reused for backward search\n";
			}
		}
		# Leave base_criteria_values as they are
	}

	my %return_section;
	$return_section{'name'} = 'base.criteria.values';
	$return_section{'values'} = $self -> base_criteria_values;
	$return_section{'labels'} = undef;
	push( @{$self -> results->[$model_number-1]{'own'}},\%return_section );

	my %return_section;
	$return_section{'name'} = 'included.relations';
	$return_section{'values'} = $self -> included_relations;
	$return_section{'labels'} = undef;
	push( @{$self -> results->[$model_number-1]{'own'}},\%return_section );


	# This loop tries to minimize the data written to disc.
	for ( my $i = 0; $i < scalar @{$self -> prepared_models->[$model_number-1]{'own'}}; $i++ ) {
		$self -> prepared_models->[$model_number-1]{'own'}[$i] -> {'outputs'} = undef; #FIXME
	}

	my %return_section;
	$return_section{'name'} = 'final.model';
	$return_section{'values'}[0][0] = $final_model;
	$return_section{'labels'} = undef;
	push( @{$self -> results->[$model_number-1]{'own'}},\%return_section );

	carp("Finished in modelfit_analyze!" );
}

sub gof_ofv
{
	my $self = shift;

	my ( $direction, $basic_model, $model_number, $ofv_ch_ref ) = @_;
	my @ofv_changes = @{$ofv_ch_ref};
	my $base_ofv;
	if ( defined $self -> base_criteria_values and
		defined $self -> base_criteria_values -> {'ofv'} ) {
		$base_ofv = $self -> base_criteria_values -> {'ofv'};
	} elsif ( $direction eq 'backward' ) {
		croak("Backward search needs a 'base' OFV estimate" );
	} else {
		if ( defined $self -> models -> [$model_number-1] -> outputs -> [0] ->
			get_single_value(attribute=> 'ofv') ) {
			$base_ofv = $self -> models -> [$model_number-1] -> outputs -> [0] ->
			get_single_value(attribute=> 'ofv');
			#do not want to use this ofv if have run a start_model, make sure base_criteria_values already defined 
		} else {
			croak("OFV estimates not available from model" . 
				$self -> models -> [$model_number-1] -> full_name );
		}
	}

	my @models = @{$self -> prepared_models -> [$model_number-1]{'own'}};
	my @ofvs;
	my @successful;
	my $ofvname = 'OFV';
	foreach my $model ( @models ) {
		#change to MAXNUM if not minimization successful?
		if ($model -> outputs -> [0]->have_output()){
			push( @ofvs, $model -> outputs -> [0] -> get_single_value(attribute=> 'ofv') );
			push( @successful, $model -> outputs -> [0] -> 
				get_single_value(attribute=> 'minimization_successful') );
		}else{
			push( @ofvs,undef);
			push( @successful,undef);
		}
	}

	my ( @ofv_drops, @log_texts );
	my $base_n_param = $self ->
	models->[$model_number-1] -> nthetas( problem_number => 1 );
	if ( defined $self -> included_relations ) {
		my %included_relations = %{$self -> included_relations};
		foreach my $incl_par ( sort keys %included_relations ) {
			foreach my $incl_cov ( sort keys %{$included_relations{$incl_par}} ) {
				$base_n_param += $included_relations{$incl_par}{$incl_cov}{'nthetas'};
			}
		}
	}

	open( LOG, ">>".$self -> logfile -> [$model_number-1] );
	print LOG "Model directory ".$self ->directory()."m1\n\n";
	my $un = $direction eq 'backward' ? '(IN)' : '';
	print LOG sprintf("%-8s",'MODEL'),
	sprintf("%12s",'TEST NAME'),
	sprintf("%12s",'BASE VAL'),
	sprintf("%12s",'NEW VAL'),
	sprintf("%50s",'TEST VAL (DROP)'),
	sprintf("%10s","GOAL"),
	sprintf("%14s"," $un".'SIGNIFICANT'),"\n";
	my ( %drop_sign, @n_param_diffs );
	my @step_relations = @{$self -> step_relations};
	for ( my $i = 0; $i <= $#step_relations; $i++ ) {
		my $n_param_diff = 
		$self -> prepared_models->[$model_number-1]{'own'}[$i] ->
		nthetas( problem_number => 1 ) - $base_n_param;
		push( @n_param_diffs, $n_param_diff );
		my $change = $direction eq 'forward' ?
		$ofv_changes[$model_number-1]{$n_param_diff} :
		-$ofv_changes[$model_number-1]{-$n_param_diff};
		my $test_val;
		my $ofv;
		if ( (not defined( $ofvs[$i] )) or (not defined $successful[$i]) or 
			($self->only_successful() and $successful[$i] != 1) ){
			$test_val = ' ' x 43 . 'FAILED';
			$ofv = ' ' x 4 . 'FAILED';
		} else {
			$test_val = $base_ofv - $ofvs[$i];
			$test_val = sprintf("%47.5f",$test_val);
			$ofv = sprintf("%12.5f",$ofvs[$i])
		}

		push ( @ofv_drops, $test_val );
		my $log_text = sprintf("%-8s",$step_relations[$i]{'parameter'}.
			$step_relations[$i]{'covariate'}.'-'.
			$step_relations[$i]{'state'}).
		sprintf("%12s","$ofvname  ").
		sprintf("%12.5f",$base_ofv).
		$ofv.
		$test_val. '  >'.
		sprintf("%10.5f",$change); 
		print LOG $log_text;
		# Significant ?
		if( defined $ofvs[$i] and $test_val > $change ){
			my $yes_text = sprintf("%12s",'YES!  ');
			$log_text = $log_text.$yes_text;
			print LOG $yes_text;
			$drop_sign{$i} = 1;
		}
		print LOG "\n";
		push( @log_texts, $log_text."\n" );
	}
	print LOG "\n";
	close ( LOG );

	my ( %sign, %l_text );
	for ( my $i = 0; $i <= $#models; $i++ ) {
		my $od = defined $drop_sign{$i} ? 1 : 0;
		$sign{$i} = $ofv_drops[$i] if ($od);
		$l_text{$i} = $log_texts[$i] if ($od);
	}

	my $chosen_ofv;
	my $resulting_model;
	my ( $chosen_parameter, $chosen_covariate, $chosen_state,$chosen_log_text );
	if ( scalar keys %sign > 0 ) {
		my @sorted_ids = sort { $sign{$b} <=> $sign{$a} } keys %sign;
		$resulting_model = $self ->
		prepared_models->[$model_number-1]{'own'}[$sorted_ids[0]];
		$chosen_ofv = $self ->
		prepared_models->[$model_number-1]{'own'}[$sorted_ids[0]] ->
		outputs -> [0] -> get_single_value(attribute=> 'ofv');
		$chosen_log_text = $l_text{$sorted_ids[0]};
		$chosen_parameter = $step_relations[$sorted_ids[0]]{'parameter'};
		$chosen_covariate = $step_relations[$sorted_ids[0]]{'covariate'};;
		$chosen_state = $step_relations[$sorted_ids[0]]{'state'};;
	}

	return ( $resulting_model,
		$chosen_parameter,
		$chosen_covariate,
		$chosen_state,
		\%drop_sign,
		\@ofv_drops,
		"$ofvname",
		{'BASE_MODEL_'.$ofvname => $base_ofv,
			'CHOSEN_MODEL_'.$ofvname => $chosen_ofv },
		{'ofv' => $chosen_ofv},
		$chosen_log_text,
		\%l_text );
}

sub gof_pval
{
	my $self = shift;

	my ( $direction, $basic_model, $model_number, $ofv_ch_ref ) = @_;
	my $pval = $self -> p_value();
	my $base_ofv;
	my $ofvname = 'OFV';
	if ( defined $self -> base_criteria_values and
		defined $self -> base_criteria_values -> {'ofv'} ) {
		$base_ofv = $self -> base_criteria_values -> {'ofv'};
	} elsif ( $direction eq 'backward' ) {
		croak("Backward search needs a 'base' OFV estimate" );
	} else {
		if ( defined $self -> models -> [$model_number-1]->outputs() and 
			defined $self -> models -> [$model_number-1]->outputs()->[0] and
			$self -> models -> [$model_number-1]->outputs()->[0]-> have_output() and
			defined $self -> models -> [$model_number-1] -> outputs -> [0] ->
			get_single_value(attribute=> 'ofv') ) {
			$base_ofv = $self -> models -> [$model_number-1] -> outputs -> [0] ->
			get_single_value(attribute=> 'ofv');
			#do not want to use this ofv if have run a start_model, make sure base_criteria_values already defined 
		} else {
			croak("OFV estimates not available from model" . 
				$self -> models -> [$model_number-1] -> full_name );
		}
	}

	my @models = @{$self -> prepared_models -> [$model_number-1]{'own'}};
	my @ofvs;
	my @successful;
	foreach my $model ( @models ) {
		#change to MAXNUM if not minimization successful?
		if (defined $model->outputs() and 
			defined $model->outputs()->[0] and
			$model->outputs()->[0]-> have_output()){
			push( @ofvs, $model -> outputs -> [0] -> get_single_value(attribute=> 'ofv') );
			push( @successful, $model -> outputs -> [0] -> 
				get_single_value(attribute=> 'minimization_successful') );
		}else{
			push( @ofvs,undef);
			push( @successful,undef);
		}
	}

	my ( @ofv_drops, @log_texts, @p_values);
	my $base_n_param = $self ->
	models->[$model_number-1] -> nthetas( problem_number => 1 );
	if ( defined $self -> included_relations ) {
		my %included_relations = %{$self -> included_relations};
		foreach my $incl_par ( sort keys %included_relations ) {
			foreach my $incl_cov ( sort keys %{$included_relations{$incl_par}} ) {
				$base_n_param += $included_relations{$incl_par}{$incl_cov}{'nthetas'};
			}
		}
	}

	open( LOG, ">>".$self -> logfile -> [$model_number-1] );
	print LOG "Model directory ".$self ->directory()."m1\n\n";
	my $un = $direction eq 'backward' ? 'IN' : '';
	print LOG sprintf("%-16s",'MODEL'),
	sprintf("%6s",' TEST '),
	sprintf("%12s","BASE $ofvname"),
	sprintf("%12s","NEW $ofvname"),
	sprintf("%24s","TEST $ofvname (DROP)"),
	sprintf("%8s","GOAL"),
	sprintf("%8s","dDF"),
	sprintf("%15s"," $un".'SIGNIFICANT'),
	sprintf("%5s","PVAL"),"\n";
	my ( %drop_sign, @n_param_diffs );
	my @step_relations = @{$self -> step_relations};
	for ( my $i = 0; $i <= $#step_relations; $i++ ) {
		my $n_param_diff = 
		$self -> prepared_models->[$model_number-1]{'own'}[$i] ->
		nthetas( problem_number => 1 ) - $base_n_param;
		my $df_sign= ($n_param_diff < 0)? -1:1;
		push( @n_param_diffs, $n_param_diff );
		my $change;
		if ($pval <= 0){
			if ($direction eq 'forward'){
				$change = 'inf';
			}else{
				$change = '-inf';
			}
		}elsif (($n_param_diff == 0) or ($pval >=1)){
			$change = 0;
		}else{
			$change = $df_sign*Statistics::Distributions::chisqrdistr($df_sign*$n_param_diff,$pval);
		}
		my $test_val;
		my $ofv;
		if ( (not defined( $ofvs[$i] )) or (not defined $successful[$i]) or 
			($self->only_successful() and $successful[$i] != 1) ){
			$test_val = ' ' x 17 . 'FAILED';
			$ofv = ' ' x 4 . 'FAILED';
			push (@p_values,999);
		} else {
			$test_val = $base_ofv - $ofvs[$i];
			#if df is unchanged or increases ($n_param_diff >= 0)then a negative drop is never significant
			#if df decreases ($n_param_diff < 0) then a positive drop is significant at 0
			#if df is unchanged ($n_param_diff == 0) then a 0 drop is significant at 1

			#put df_sign on pval. Samma antal parametrar. For att acceptera addition kravs 
			if ($n_param_diff == 0){
				if ($direction eq 'forward'){
					if ($test_val == 0){
						push ( @p_values, 99); #no p-value makes this accepted
					}elsif ($test_val < 0){ #there is no p-value high enough to make this accepted
						push ( @p_values, 999);
					}else{
						#test_val > 0
						push ( @p_values, 0); #accepted even with p-value 0 
					}
				}else{
					#which p-value, coming from the forward direction, was necessary for adding this
					#parameter
					if ($test_val == 0){
						push ( @p_values, 99); #larger than 1, we never accept 0 change in forward dir  
					}elsif ($test_val < 0){ #same as increase in forward, accepted even with p=0
						push ( @p_values, 0);
					}else{  #there is no p-value high enough to make this accepted in forward dir
						#test_val > 0
						push ( @p_values, 999);
					}
				}
			}elsif (($n_param_diff > 0) and ($test_val < 0)){
				if ($direction eq 'forward'){
					push ( @p_values, 9999); #no p-value high enough
				}else{
					push ( @p_values, -1); #optimal to get lower ofv with fewer parameters 
				}
			}elsif (($n_param_diff < 0) and ($test_val > 0)){
				if ($direction eq 'forward'){
					push ( @p_values, -1); #optimal to get lower ofv with fewer parameters 
				}else{
					push ( @p_values, 9999); #no p-value high enough
				}
			}else{
				push ( @p_values, (Statistics::Distributions::chisqrprob($df_sign*$n_param_diff,$df_sign*$test_val )));
			}
			$test_val = sprintf("%21.5f",$test_val);
			$ofv = sprintf("%12.5f",$ofvs[$i])
		}
		push ( @ofv_drops, $test_val );

		my $log_text = sprintf("%-16s",$step_relations[$i]{'parameter'}.
			$step_relations[$i]{'covariate'}.'-'.
			$step_relations[$i]{'state'}).
		sprintf("%6s"," PVAL ").
		sprintf("%12.5f",$base_ofv).
		$ofv.
		$test_val. '  >'.
		sprintf("%10.5f",$change).
		sprintf("%5s",$n_param_diff); 
		print LOG $log_text;
		# Significant ?
		if( ($change eq '-inf') or
			($change ne 'inf'  
					and defined $ofvs[$i] and $test_val > $change 
					and (($direction eq 'forward' and $p_values[$i] <= $pval) or
					($direction eq 'backward' and $p_values[$i] >= $pval)))
		){
			my $yes_text = sprintf("%14s",'YES!  ');
			my $pval_text;
			if ($p_values[$i]>1){
				$pval_text = sprintf("%9.0f",$p_values[$i]);
			}else{
				$pval_text = sprintf("%-9.6f",$p_values[$i]);
				if ($pval_text == 0){
					$pval_text = sprintf("%-9.2e",$p_values[$i]);
				}
			}
			$log_text = $log_text.$yes_text.$pval_text;
			print LOG $yes_text;
			print LOG $pval_text;
			$drop_sign{$i} = 1;
		}elsif (defined ($self->xv_pred_data)){
			#accept everything if xv
			my $yes_text = sprintf("%14s",'XV-ok ');
			my $pval_text;
			if ($p_values[$i]>1){
				$pval_text = sprintf("%9.0f",$p_values[$i]);
			}else{
				$pval_text = sprintf("%-9.6f",$p_values[$i]);
				if ($pval_text == 0){
					$pval_text = sprintf("%-9.2e",$p_values[$i]);
				}
			}
			$log_text = $log_text.$yes_text.$pval_text;
			print LOG $yes_text;
			print LOG $pval_text;
			$drop_sign{$i} = 1;

		}else{
			my $no_text = sprintf("%14s",'      ');
			my $pval_text;
			if ($p_values[$i]>1){
				$pval_text = sprintf("%9.0f",$p_values[$i]);
			}else{
				$pval_text = sprintf("%-9.6f",$p_values[$i]);
				if ($pval_text == 0){
					$pval_text = sprintf("%-9.2e",$p_values[$i]);
				}
			}
			$log_text = $log_text.$no_text.$pval_text;
			print LOG $no_text;
			print LOG $pval_text;
		}
		print LOG "\n";
		push( @log_texts, $log_text."\n" );
	}
	print LOG "\n";
	close ( LOG );

	my ( %sign, %l_text );
	for ( my $i = 0; $i <= $#models; $i++ ) {
		my $od = defined $drop_sign{$i} ? 1 : 0;
		$sign{$i} = $p_values[$i] if ($od);
		$l_text{$i} = $log_texts[$i] if ($od);
	}

	my $chosen_ofv;
	my $resulting_model;
	my ( $chosen_parameter, $chosen_covariate, $chosen_state, $chosen_log_text );
	if ( scalar keys %sign > 0 ) {
		my @sorted_ids;
		if ($direction eq 'forward'){
			@sorted_ids = sort { $sign{$a} <=> $sign{$b} } keys %sign; #smallest is best
		}else{
			#biggest is worst (most important to remove 
			@sorted_ids = sort { $sign{$b} <=> $sign{$a} } keys %sign; 
		}
		#check if there are any p_vals that are equal to best. In that case
		#find best ofv-drop
		my $i = 1;
		my $best = 0;
		if (scalar(@sorted_ids)>1){
			while ( $sign{$sorted_ids[$best]} == $sign{$sorted_ids[$i]}){
				if ($ofv_drops[$sorted_ids[$i]] > $ofv_drops[$sorted_ids[$best]]){
					$best = $i;
				}
				$i++;
				last if ($i > $#sorted_ids);
			}
		}
		$resulting_model = $self ->
		prepared_models->[$model_number-1]{'own'}[$sorted_ids[$best]];
		$chosen_ofv = $self ->
		prepared_models->[$model_number-1]{'own'}[$sorted_ids[$best]] ->
		outputs -> [0] -> get_single_value(attribute=> 'ofv');
		$chosen_log_text = $l_text{$sorted_ids[$best]};
		$chosen_parameter = $step_relations[$sorted_ids[$best]]{'parameter'};
		$chosen_covariate = $step_relations[$sorted_ids[$best]]{'covariate'};
		$chosen_state = $step_relations[$sorted_ids[$best]]{'state'};
		$self->run_xv_pred_step(estimation_model => $resulting_model,
			model_name => $chosen_parameter.$chosen_covariate.'_'.$chosen_state)
		if defined ($self->xv_pred_data);
	}

	my $gtpval = ($direction eq 'forward')? '< '.$pval : '> '.$pval;
	return ( $resulting_model,
		$chosen_parameter,
		$chosen_covariate,
		$chosen_state,
		\%drop_sign,
		\@ofv_drops,
		'PVAL '.$gtpval,
		{"BASE_MODEL_$ofvname" => $base_ofv,
			"CHOSEN_MODEL_$ofvname" => $chosen_ofv },
		{'ofv' => $chosen_ofv},
		$chosen_log_text,
		\%l_text );
}


sub _create_models
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 },
		parallel_states => { isa => 'Bool', default => 0, optional => 1 },
		orig_model => { isa => 'model', optional => 1 },
		initial_estimates_model => { isa => 'Maybe[model]', optional => 1 },
		relations => { isa => 'HashRef[HashRef]', optional => 1 },
		included_relations => { isa => 'HashRef[HashRef]', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $parallel_states = $parm{'parallel_states'};
	my $orig_model = $parm{'orig_model'};
	my $initial_estimates_model = $parm{'initial_estimates_model'};
	my %relations = defined $parm{'relations'} ? %{$parm{'relations'}} : ();
	my %included_relations = defined $parm{'included_relations'} ? %{$parm{'included_relations'}} : ();
	my @new_models;
	my @step_relations;

	# Set names of tested parameter-covariate-state combinations in results
	# Done in loop below after state change
	my %return_section;
	$return_section{'name'} = 'combinations';
	$return_section{'values'} = [];
	$return_section{'labels'} = [];

	my $sizes_lth = $orig_model->get_option_value(record_name=>'sizes',
		option_name=>'LTH',
		fuzzy_match => 0);
	my $have_sizes = 0;
	$have_sizes = 1 if ((defined $sizes_lth) or 
		(defined $orig_model ->problems->[0]->sizess() 
				and scalar(@{$orig_model ->problems->[0]->sizess()})>0));

	my $done = ( -e $self -> directory."/m$model_number/done" ) ? 1 : 0;

	if ( not $done ) {
		my $copy_datafile = 0;
		#check if should copy datafile to new level, done every 9th iteration
		my $maxlev = 9;
		#want mod to be zero when it is time to do something
		my $modulus = ($self->step_number()-1)%($maxlev); 
		if ($self->linearize){
			if (($modulus == 0 and ($self->step_number()>1) or ($self->search_direction eq 'backward' and $self->step_number == 2)) 
				and (not $self->update_derivatives)) {
				$copy_datafile = 1;
			}
		}elsif ($modulus == 0 or $self->step_number()==1 or ($self->search_direction eq 'backward' and $self->step_number == 2)) {
			$copy_datafile = 1;
		}
		if ($copy_datafile){
			my $fullpath = $orig_model->datafiles(absolute_path => 1)->[0];
			my $filename = $orig_model->datafiles(absolute_path => 0)->[0];
			my $string = File::Spec->catfile($self -> directory,$filename);
			cp($fullpath,$string);
			$self->main_data_file($string);		
		}else{
			unless (defined $self->main_data_file and length($self->main_data_file)>0){
				if ($self->step_number > 1){
					croak("step_number is ".$self->step_number." but main_data_file is not set. This is a bug");
				}
				$self->main_data_file($orig_model->datafiles(absolute_path => 1)->[0]);
			}
		}
		open( DONE_LOG, '>'.$self -> directory."/m$model_number/done.log" );
		foreach my $parameter ( sort keys %relations ) {
			foreach my $covariate ( sort keys %{$relations{$parameter}} ) {
				# Is this covariate continuous or not?
				my $continuous = 1;
				if (defined $self -> categorical_covariates()){
					foreach my $cat ( @{$self -> categorical_covariates()} ) {
						$continuous = 0 if ( $covariate eq $cat );
					}
				}
				my @valid_states;
				if ( $continuous ) {
					@valid_states = @{$self -> valid_states->{'continuous'}};
				} else {
					@valid_states = @{$self -> valid_states->{'categorical'}};
				}
				my $state;
				# Important: just setting $state to $self->incl_rel....{'state'} initiates
				# included_relations for this parameter and covariate. Avoid this.
				if ( defined $included_relations{$parameter}{$covariate} ) {
					$state = $included_relations{$parameter}{$covariate}{'state'};
				}
				$state = defined $state ? $state : $valid_states[0];
				#1. Create a new model if the state is not yet included and at highest level
				# (forward search) or base level (backward search).
				next if ( ( $self -> search_direction eq 'forward' and
						$state == $valid_states[$#valid_states] ) or
					( $self -> search_direction eq 'backward' and
						$state == $valid_states[0] ) );
				my $old_state = $state;
				my @new_states=();
				# Increment (forward search) or decrement (backward search) the state
				if ( $self -> search_direction eq 'forward' ) {
					my $flag = 0;
					for( my $s_idx = 0; $s_idx <= $#valid_states; $s_idx++ ) {
						if ( $flag ) {
							push(@new_states,$valid_states[$s_idx]);
							last unless ($parallel_states);
						}
						$flag = 1 if( $state == $valid_states[$s_idx] ); #found current state
					}
				} elsif ( $self -> search_direction eq 'backward' ) {
					my $flag = 0;
					for( my $s_idx = $#valid_states; $s_idx >= 0; $s_idx-- ) {
						if ( $flag ) {
							push(@new_states,$valid_states[$s_idx]);
							last unless ($parallel_states);
						}
						$flag = 1 if( $state == $valid_states[$s_idx] );
					}
				}

				#only one unless $parallel_states
				for (my $si=0; $si<= $#new_states; $si++){
					$state = @new_states[$si];

					# Only one problem and one sub problem
					push( @{$return_section{'values'}[0][0]}, "$parameter$covariate-$state" );

					my ( $dir, $filename ) =
					OSspecific::absolute_path( $self -> directory.
						'/m'.$model_number.'/',
						$parameter.$covariate.$state.".mod" );
					my ( $odir, $outfilename ) =
					OSspecific::absolute_path( $self -> directory.
						'/m'.$model_number.'/',
						$parameter.$covariate.$state.".lst" );

					my $applicant_model;
					$applicant_model = $orig_model -> copy( filename    => $dir.$filename,
															copy_datafile   => 0,
															write_copy => 0,
															copy_output => 0);
					$applicant_model -> ignore_missing_files(1);
					$applicant_model -> outputfile( $odir.$outfilename );
					my @table_names = @{$applicant_model -> table_names};
					for ( my $i = 0; $i <= $#table_names; $i++ ) {
						for ( my $j = 0; $j < scalar @{$table_names[$i]}; $j++ ) {
							$table_names[$i][$j] = $self -> directory.
							'/m'.$model_number.'/'.
							$filename.'.'.
							OSspecific::nopath($table_names[$i][$j]);
						}
					}
					$applicant_model -> table_names( new_names            => \@table_names,
													 ignore_missing_files => 1);
					my @used_covariates = ();
					# $included_relations is a reference to $self -> included_relations
					# and should only be initialized for truly incorporated relations
					# see beginning of loop above.
					foreach my $incl_par ( sort keys %included_relations ) {
						foreach my $incl_cov ( sort keys %{$included_relations{$incl_par}} ) {
							next if ( $incl_par eq $parameter and $incl_cov eq $covariate );
							if ($self->linearize()){
								$self -> 
								add_code_linearize( definition_code => $included_relations{$incl_par}{$incl_cov}{'code'},
									nthetas         => $included_relations{$incl_par}{$incl_cov}{'nthetas'},
									inits           => $included_relations{$incl_par}{$incl_cov}{'inits'},
									bounds          => $included_relations{$incl_par}{$incl_cov}{'bounds'},
									applicant_model => $applicant_model,
									sum_covariates  => $self->sum_covariates_hash->{$incl_par},
									parameter       => $incl_par,
									covariate       => $incl_cov );
							}else{
								$self -> 
								add_code( definition_code => $included_relations{$incl_par}{$incl_cov}{'code'},
									nthetas         => $included_relations{$incl_par}{$incl_cov}{'nthetas'},
									inits           => $included_relations{$incl_par}{$incl_cov}{'inits'},
									bounds          => $included_relations{$incl_par}{$incl_cov}{'bounds'},
									applicant_model => $applicant_model,
									sum_covariates  => $self->sum_covariates_hash->{$incl_par},
									parameter       => $incl_par,
									covariate       => $incl_cov );
							}
							push( @used_covariates, $incl_cov );
						}
					}
					# If the new state is base level (backward search) don't add this relation, otherwise:
					# But could the base level no be > 0??? if not then add input checks
					unless ( $state == 1 ) {
						if ($self->linearize()){

							$self -> add_code_linearize( definition_code => $relations{$parameter}{$covariate}{'code'}{$state},
								nthetas         => $relations{$parameter}{$covariate}{'nthetas'}{$state},
								inits           => $relations{$parameter}{$covariate}{'inits'}{$state},
								bounds          => $relations{$parameter}{$covariate}{'bounds'}{$state},
								applicant_model => $applicant_model,
								sum_covariates  => $self->sum_covariates_hash->{$parameter},
								parameter       => $parameter,
								covariate       => $covariate );
						}else{
							$self -> add_code( definition_code => $relations{$parameter}{$covariate}{'code'}{$state},
								nthetas         => $relations{$parameter}{$covariate}{'nthetas'}{$state},
								inits           => $relations{$parameter}{$covariate}{'inits'}{$state},
								bounds          => $relations{$parameter}{$covariate}{'bounds'}{$state},
								applicant_model => $applicant_model,
								sum_covariates  => $self->sum_covariates_hash->{$parameter},
								parameter       => $parameter,
								covariate       => $covariate );
						}
						push( @used_covariates, $covariate );
					}
					my @all_covariates;
					if ( defined $self -> categorical_covariates() ) {
						push( @all_covariates, @{$self -> categorical_covariates()});
					}
					if ( defined $self -> continuous_covariates() ) {
						push( @all_covariates, @{$self -> continuous_covariates()});
					}
					$self -> drop_undrop_covariates( applicant_model => $applicant_model,
						used_covariates => \@used_covariates,
						all_covariates  => \@all_covariates,
						do_not_drop     => $self -> do_not_drop);

					if ( defined $initial_estimates_model ) {
						$applicant_model -> update_inits( from_model    => $initial_estimates_model,
							update_thetas => 1,
							update_omegas => 1,
							update_sigmas => 1,
							ignore_missing_parameters => 1 )
						unless (not $self->run_linearized_base());
					} else {
						$applicant_model -> update_inits( from_model    => $orig_model,
							update_thetas => 1,
							update_omegas => 1,
							update_sigmas => 1,
							ignore_missing_parameters => 1 )
						unless (not $self->run_linearized_base());
					}

					my $needed_thetas = $applicant_model -> nthetas();
					if ($needed_thetas > 40){ #40 is limit in NONMEM, can handle more if NM7.2 and $SIZES LTH=needed_thetas
						if ($have_sizes){
							$applicant_model -> set_option(record_name => 'sizes',
								record_number => 1,
								option_name => 'LTH',
								option_value => $needed_thetas,
								fuzzy_match => 0);
						}elsif (($PsN::nm_minor_version >= 2) and ($PsN::nm_major_version >= 7)){
							$applicant_model -> add_records( type => 'sizes',
								record_strings => [ " LTH=".$needed_thetas ] );
							$orig_model -> add_records( type => 'sizes',
								record_strings => [ " LTH=".$needed_thetas ] );
							$have_sizes = 1;
						}else{
							my $mess = "$needed_thetas \$THETA needed in candidate model, too many for NONMEM. ".
							"Use NM version 7.2 or later, which can handle more than 40.".
							"If you are already using 7.2 or later, check that the version info in psn.conf is correct.";
							croak($mess);
						}
					}

					my @new_names = ($self->main_data_file) x scalar(@{$applicant_model->problems});
					$applicant_model->datafiles(new_names => \@new_names);
					$applicant_model -> _write();
					push( @new_models, $applicant_model );

					my %st_rel;
					$st_rel{'parameter'} = $parameter;
					$st_rel{'covariate'} = $covariate;
					$st_rel{'state'}     = $state;
					$st_rel{'continuous'} = $continuous;
					push( @step_relations, \%st_rel );
					print DONE_LOG "$parameter $covariate $continuous $old_state $state\n";
				}
			}
		}
		open( TMP, ">".$self -> directory."/m$model_number/done" );
		close( TMP );
		close( DONE_LOG );
	} else {
		ui -> print( category => 'scm',
			message  => "Recreating models from previously run step" );
		carp("Creating applicant model from file on disk" );
		if ( not -e $self -> directory."/m$model_number/done.log" ) {
			croak("No file ".$self -> directory.
				"/m$model_number/done.log seem to exist although the existance".
				" of the file ".$self -> directory.
				"/m$model_number/done indicates so.");
		}
		open( DONE_LOG, $self -> directory."/m$model_number/done.log" );
		my @rows = <DONE_LOG>;
		close( DONE_LOG );
		for( my $i = 0; $i <= $#rows; $i++ ) { # skip first row
			chomp( $rows[$i] );
			my ( $parameter, $covariate, $continuous, $old_state, $state ) =
			split(' ',$rows[$i],5);
			my @valid_states;
			if ( $continuous ) {
				@valid_states = @{$self -> valid_states->{'continuous'}};
			} else {
				@valid_states = @{$self -> valid_states->{'categorical'}};
			}
			#1. Recreate the model if the state is not yet included and at highest level
			# (forward search) or base level (backward search).
			next if ( ( $self -> search_direction eq 'forward' and
					$old_state == $valid_states[$#valid_states] ) or
				( $self -> search_direction eq 'backward' and
					$old_state == $valid_states[0] ) );

			my ( $dir, $filename ) = OSspecific::absolute_path( $self -> directory.'/m'.$model_number.'/',$parameter.$covariate.$state.".mod");
			my ( $odir, $outfilename ) = OSspecific::absolute_path( $self -> directory.'/m'.$model_number.'/',$parameter.$covariate.$state.".lst");

			#orig_model reference
			my $applicant_model = model -> new( %{common_options::restore_options(@common_options::model_options)},
												outputs              => undef,
												problems             => undef,
												active_problems      => undef,
												filename   => $dir.$filename,
												outputfile => $odir.$outfilename,
												ignore_missing_files => 1 );
			# Set the correct data file for the object
			my $moddir = $orig_model -> directory;
			my @datafiles = @{$orig_model -> datafiles};
			for( my $df = 0; $df <= $#datafiles; $df++ ) {
				$datafiles[$df] = $moddir.'/'.$datafiles[$df];
			}
			$applicant_model -> datafiles( new_names => \@datafiles );
			#TODO should we write the model here??? Anyway, this code is probably never ever run
			push( @new_models, $applicant_model );
			my %st_rel;
			$st_rel{'parameter'} = $parameter;
			$st_rel{'covariate'} = $covariate;
			$st_rel{'state'}     = $state;
			$st_rel{'continuous'} = $continuous;
			push( @step_relations, \%st_rel );
			my $nl = $i == $#rows ? "" : "\r"; 
			ui -> print( category => 'scm',
				message  => ui -> status_bar( sofar => $i+1,
					goal  => $#rows+1 ).$nl,
				wrap     => 0,
				newline  => 0 )
		}
		ui -> print( category => 'scm',
			message  => " ... done." );
	}
	push( @{$self -> results->[$model_number-1]{'own'}},\%return_section );

	return \@new_models ,\@step_relations;
}

sub create_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		state => { isa => 'Int', optional => 1 },
		start_theta => { isa => 'Int', optional => 1 },
		model_number => { isa => 'Int', optional => 1 },
		parameter => { isa => 'Str', optional => 1 },
		covariate => { isa => 'Str', optional => 1 },
		continuous => { isa => 'Bool', optional => 1 },
		statistics => { isa => 'HashRef', optional => 1 },
		missing_data_token => { isa => 'Str', optional => 1 },
		sum_covariates => { isa => 'Bool', default => 0, optional => 1 },
		code => { isa => 'ArrayRef[Str]', optional => 1 },
		inits => { isa => 'ArrayRef', optional => 1 },
		bounds => { isa => 'HashRef', optional => 1 }
	);
	my $state = $parm{'state'};
	my $start_theta = $parm{'start_theta'};
	my $model_number = $parm{'model_number'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : ();
	my $missing_data_token = $parm{'missing_data_token'};
	my $sum_covariates = $parm{'sum_covariates'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();

	my $new_code = [];
	my $new_inits = [];
	my $new_bounds = {};
	my $codetype= $state;

	if ( scalar @code == 1){
		if( $code[0] eq 'none' ) {
			$codetype = 1;
		}elsif( $code[0] eq 'linear' ) {
			$codetype = 2;
		}elsif( $code[0] eq 'hockey-stick' ) {
			$codetype = 3;
		}elsif( $code[0] eq 'exponential' ) {
			$codetype = 4;
		}elsif( $code[0] eq 'power' ) {
			$codetype = 5;
		}elsif ( $code[0] ne '' ){
			$codetype = 'user';
		}
	}
	#only format if first level recursion
	if ($self->step_number() == 1){
		for (my $i=0; $i< scalar(@inits);$i++){
			$inits[$i] = $self->format_inits_bounds(string => $inits[$i],
				statistics => \%statistics,
				continuous => $continuous,
				is_bound => 0)
			if (defined $inits[$i]);
		}
		if ( defined $bounds{'lower'}){
			for (my $i=0; $i< 25;$i++){
				if (defined $bounds{'lower'}[$i] ){
					$bounds{'lower'}[$i] = $self->format_inits_bounds(string => $bounds{'lower'}[$i],
						statistics => \%statistics,
						continuous => $continuous,
						is_bound => 1);
				}
			}
		}
		if ( defined $bounds{'upper'}){
			for (my $i=0; $i< 25;$i++){
				if (defined $bounds{'upper'}[$i] ){
					$bounds{'upper'}[$i] = $self->format_inits_bounds(string => $bounds{'upper'}[$i],
						statistics => \%statistics,
						continuous => $continuous,
						is_bound => 1); 
				}
			}
		}
	}
	#handle all redefinitions in create_user_code
	if ( scalar @code > 1 or ( scalar @code == 1 and $codetype eq 'user' )) {
		( $new_code, $end_theta, $new_inits, $new_bounds ) =
		$self -> create_user_code( start_theta        => $start_theta,
			parameter          => $parameter,
			covariate          => $covariate,
			continuous          => $continuous,
			bounds             => \%bounds,
			inits              => \@inits,
			code               => \@code,
			statistics         => \%statistics,
			sum_covariates     => $sum_covariates,
			missing_data_token => $missing_data_token);
	}elsif ( $codetype == 1 ) {
		( $new_code, $end_theta, $new_inits, $new_bounds ) =
		$self -> create_state1_code( start_theta        => $start_theta,
			parameter          => $parameter,
			covariate          => $covariate,
			continuous          => $continuous,
			bounds             => \%bounds,
			inits              => \@inits,
			code               => \@code,
			statistics         => \%statistics,
			sum_covariates     => $sum_covariates,
			missing_data_token => $missing_data_token);
	} elsif ( $codetype == 2 ) {
		# First state
		( $new_code, $end_theta, $new_inits, $new_bounds ) =
		$self -> create_linear_code( start_theta        => $start_theta,
			parameter          => $parameter,
			covariate          => $covariate,
			continuous          => $continuous,
			bounds             => \%bounds,
			inits              => \@inits,
			code               => \@code,
			statistics         => \%statistics,
			sum_covariates     => $sum_covariates,
			missing_data_token => $missing_data_token);

	} elsif ( $codetype == 3) {
		( $new_code, $end_theta, $new_inits, $new_bounds ) =
		$self -> create_hockey_stick_code( start_theta        => $start_theta,
			parameter          => $parameter,
			covariate          => $covariate,
			continuous          => $continuous,
			bounds             => \%bounds,
			inits              => \@inits,
			code               => \@code,
			statistics         => \%statistics,
			sum_covariates     => $sum_covariates,
			missing_data_token => $missing_data_token);
	} elsif ( $codetype == 4 ) {
		( $new_code, $end_theta, $new_inits, $new_bounds ) =
		$self -> create_exponential_code( start_theta        => $start_theta,
			parameter          => $parameter,
			covariate          => $covariate,
			continuous          => $continuous,
			bounds             => \%bounds,
			inits              => \@inits,
			code               => \@code,
			statistics         => \%statistics,
			sum_covariates     => $sum_covariates,
			missing_data_token => $missing_data_token);
	} elsif ( $codetype == 5 ) {
		( $new_code, $end_theta, $new_inits, $new_bounds ) =
		$self -> create_power_code( start_theta        => $start_theta,
			parameter          => $parameter,
			covariate          => $covariate,
			continuous          => $continuous,
			bounds             => \%bounds,
			inits              => \@inits,
			code               => \@code,
			statistics         => \%statistics,
			sum_covariates     => $sum_covariates,
			missing_data_token => $missing_data_token);
	} else {
		croak("State $state cannot be used when not defined in the [code] section.\n" );
	}
	@inits = @{$new_inits};
	%bounds = %{$new_bounds};
	@code = @{$new_code};

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub add_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		parameter => { isa => 'Str', optional => 1 },
		covariate => { isa => 'Str', optional => 1 },
		sum_covariates => { isa => 'Bool', default => 0, optional => 1 },
		nthetas => { isa => 'Int', optional => 1 },
		definition_code => { isa => 'ArrayRef[Str]', optional => 1 },
		bounds => { isa => 'HashRef', optional => 1 },
		inits => { isa => 'ArrayRef[Num]', optional => 1 },
		applicant_model => { isa => 'model', optional => 1 }
	);
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $sum_covariates = $parm{'sum_covariates'};
	my $nthetas = $parm{'nthetas'};
	my @definition_code = defined $parm{'definition_code'} ? @{$parm{'definition_code'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my $applicant_model = $parm{'applicant_model'};

	my @labels;
	for ( my $i = 1; $i <= $nthetas; $i++ ) {
		push( @labels, $parameter.$covariate.$i );
	}
	my $operator='*';
	$operator='+' if ($sum_covariates);

	my $start_theta = $applicant_model -> nthetas() + 1;
	my $end_theta = $start_theta + $nthetas - 1;

	my $tmp = $start_theta;

	#handle mulitple THETA on same line, handle multiple uses of same THETA
	my %original_to_model_thetas;
	for (my $i=1; $i<=$nthetas;$i++){
		$original_to_model_thetas{$i} = 0;
	}
	for ( @definition_code ) {
		while ( /THETA\((\d+)\)/ ) {
			if ($original_to_model_thetas{$1} == 0){
				$original_to_model_thetas{$1} = $tmp++;
			}
			my $num = $original_to_model_thetas{$1};
			s/THETA\((\d+)\)/THETA\(newnum$num\)/;
		}
		s/newnum//g;
	}

	# Add the definition_code to the PK or PRED block
	my @code;
	@code = @{$applicant_model -> pk( problem_number => 1 )};
	my $use_pred = 0;
	unless ( $#code > 0 ) {
		@code = @{$applicant_model -> pred( problem_number => 1 )};
		$use_pred = 1;
	}
	if ( $#code <= 0 ) {
		croak("Neither PK or PRED defined in " .
			$applicant_model -> filename . "\n" );
	}

	my $found_REL = 0;
	my $found_anchor = -1;
	my $i = 0;
	my $relationarea = 0;
	my @row;
	my $found_correct_REL = 0;
	for ( @code ) {
		if ( /^;;;SCM-ANCHOR/) {
			$found_anchor = $i;
			$i++;
			next;
		}
		if ( /^;;; (\w+)-RELATION START/ and $1 eq $parameter ) {
			$relationarea = 1;
			$i++;
			next;
		}
		if ( /^;;; (\w+)-RELATION END/ and $1 eq $parameter ) {
			$relationarea = 0;
			last;
		}
		if ( $relationarea ) {
			$found_REL = $i;
			carp($parameter . "COV has already been added to the code" );
			$found_correct_REL = 1 and last if ( /$parameter$covariate/ );
			if ($sum_covariates){
				@row = split(/\)\+\(/);
			}else{
				@row = split(/\)\*\(/);
			}
		}
		$i++;
	}

	# If we have old scm code present.
	if ( $found_REL ) {
		unless ( $found_correct_REL ) {
			if ( $#row > 2 ) {
				@code =  (@code[0..$found_REL],
					"$parameter"."COV=$parameter"."COV$operator$parameter$covariate\n",
					@code[$found_REL+1..$#code]);
			} else {
				chomp($code[$found_REL]);
				$code[$found_REL] = $code[$found_REL]."$operator$parameter$covariate\n";
			}
		}
	} else {
		if ($found_anchor >= 0){
			@code =  (@code[0..$found_anchor],
				";;; $parameter-RELATION START\n",
				"$parameter"."COV=$parameter$covariate\n",
				";;; $parameter-RELATION END\n\n",
				@code[$found_anchor+1..$#code]);
		}else{
			@code = ( ";;; $parameter-RELATION START\n",
				"$parameter"."COV=$parameter$covariate\n",
				";;; $parameter-RELATION END\n\n",
				@code );
		}
	}

	if ($found_anchor >= 0){
		@code =  (@code[0..$found_anchor],
			"\n;;; $parameter$covariate-DEFINITION START\n",
			@definition_code,
			";;; $parameter$covariate-DEFINITION END\n\n",
			@code[$found_anchor+1..$#code]);
	}else{
		@code = ( "\n;;; $parameter$covariate-DEFINITION START\n",
			@definition_code,
			";;; $parameter$covariate-DEFINITION END\n\n",
			@code );
	}
	# Add to the parameter code
	unless ( $found_REL ) {
		my $success = 0;
		for ( reverse @code ) {
			#want to find last occurrence
			if ( /[^A-Z0-9_]*TV(\w+)\s*=\s*/ and $1 eq $parameter){
				#add new definition line after last occurence
				$_ = $_."\n"."TV$parameter = $parameter"."COV$operator"."TV$parameter\n";
				$success = 1;
				last; #only change the last line where appears
			}
		}
		unless ( $success ) {
			croak("Could not determine a good place to add the covariate relation.\n".
				" i.e. No TV$parameter was found\n" );
		}
	}
	if ( $use_pred ) {
		$applicant_model -> pred( problem_number => 1,
			new_pred       => \@code );
	} else {
		$applicant_model -> pk( problem_number => 1,
			new_pk         => \@code );
	}

	#initial values must be set first, since we need to add if absent

	$applicant_model -> initial_values( parameter_numbers => [[$start_theta..$end_theta]],
		new_values        => [\@inits],
		add_if_absent     => 1,
		parameter_type    => 'theta',
		problem_numbers   => [1]);
	$applicant_model -> lower_bounds( parameter_type    => 'theta',
		parameter_numbers => [[$start_theta..$end_theta]],
		problem_numbers   => [1],
		new_values        => [$bounds{'lower'}] );
	$applicant_model -> upper_bounds( parameter_type    => 'theta',
		parameter_numbers => [[$start_theta..$end_theta]],
		problem_numbers   => [1],
		new_values        => [$bounds{'upper'}] );
	$applicant_model -> labels( parameter_type    => 'theta',
		parameter_numbers => [[$start_theta..$end_theta]],
		problem_numbers   => [1],
		new_values        => [\@labels] );

	return $applicant_model;
}

sub add_code_linearize
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		parameter => { isa => 'Str', optional => 1 },
		covariate => { isa => 'Str', optional => 1 },
		sum_covariates => { isa => 'Bool', default => 0, optional => 1 },
		nthetas => { isa => 'Int', optional => 1 },
		definition_code => { isa => 'ArrayRef[Str]', optional => 1 },
		bounds => { isa => 'HashRef', optional => 1 },
		inits => { isa => 'ArrayRef[Num]', optional => 1 },
		applicant_model => { isa => 'model', optional => 1 }
	);
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $sum_covariates = $parm{'sum_covariates'};
	my $nthetas = $parm{'nthetas'};
	my @definition_code = defined $parm{'definition_code'} ? @{$parm{'definition_code'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my $applicant_model = $parm{'applicant_model'};

	my @labels;
	for ( my $i = 1; $i <= $nthetas; $i++ ) {
		push( @labels, $parameter.$covariate.$i );
	}

	my $start_theta = $applicant_model -> nthetas() + 1;
	my $end_theta = $start_theta + $nthetas - 1;
	my $operator='*';
	$operator='+' if ($sum_covariates);

	my $tmp = $start_theta;

	#handle mulitple THETA on same line, handle multiple uses of same THETA
	my %original_to_model_thetas;
	for (my $i=1; $i<=$nthetas;$i++){
		$original_to_model_thetas{$i} = 0;
	}
	for ( @definition_code ) {
		while ( /THETA\((\d+)\)/ ) {
			if ($original_to_model_thetas{$1} == 0){
				$original_to_model_thetas{$1} = $tmp++;
			}
			my $num = $original_to_model_thetas{$1};
			s/THETA\((\d+)\)/THETA\(newnum$num\)/;
		}
		s/newnum//g;
	}

	# Add the definition_code to the PK or PRED block
	my @code;
	my $use_pred = 0;
	@code = @{$applicant_model -> pred( problem_number => 1 )};
	$use_pred = 1;
	if ( $#code <= 0 ) {
		croak("PRED not defined in " .
			$applicant_model -> filename . "\n" );
	}

	my $found_REL = 0;
	my $found_anchor = -1;
	my $i = 0;
	my $relationarea = 0;
	my @row;
	my $found_correct_REL = 0;
	for ( @code ) {
		if ( /^;;;SCM-ANCHOR/) {
			$found_anchor = $i;
			$i++;
			next;
		}
		if ( /^;;; (\w+)-RELATION START/ and $1 eq $parameter ) {
			$relationarea = 1;
			$i++;
			next;
		}
		if ( /^;;; (\w+)-RELATION END/ and $1 eq $parameter ) {
			$relationarea = 0;
			last;
		}
		if ( $relationarea ) {
			$found_REL = $i;
			carp("GZ_".$parameter . " has already been added to the code" );
			$found_correct_REL = 1 and last if ( /$parameter$covariate/ );
			if ($sum_covariates){
				@row = split(/\)\+\(/);
			}else{
				@row = split(/\)\*\(/);
			}
		}
		$i++;
	}

	# If we have old scm code present.
	my $etanum;
	if ( $found_REL ) {
		unless ( $found_correct_REL ) {
			if ( $#row > 2 ) {
				print "warning: adding covariates to old scm code not tested\n";
				@code =  (@code[0..$found_REL],
					"GZ_$parameter"."=GZ_$parameter"."$operator$parameter$covariate\n",
					@code[$found_REL+1..$#code]);
			} else {
				chomp($code[$found_REL]);
				$code[$found_REL] = $code[$found_REL]."$operator$parameter$covariate\n";
			}
		}
	} else {
		my %parameter_eta;
		%parameter_eta = %{$self->parameter_eta()} if defined $self->parameter_eta();
		if ( defined $parameter_eta{$parameter}){
			$etanum= $parameter_eta{$parameter};
		}else{
			croak("Could not extract ETA number for $parameter");
		}
		if ($found_anchor >= 0){
			@code =  (@code[0..$found_anchor],
				";;; $parameter-RELATION START\n",
				"; $parameter IS ETA$etanum",
				"GZ_$parameter"." = $parameter$covariate\n",
				";;; $parameter-RELATION END\n\n",
				@code[$found_anchor+1..$#code]);
		}else {
			@code = ( ";;; $parameter-RELATION START\n",
				"; $parameter IS ETA$etanum",
				"GZ_$parameter"." = $parameter$covariate\n",
				";;; $parameter-RELATION END\n\n",
				@code );
		}
	}

	if ($found_anchor >= 0){
		@code =  (@code[0..$found_anchor],
			"\n;;; $parameter$covariate-DEFINITION START\n",
			@definition_code,
			";;; $parameter$covariate-DEFINITION END\n\n",
			@code[$found_anchor+1..$#code]);
	}else{
		@code = ( "\n;;; $parameter$covariate-DEFINITION START\n",
			@definition_code,
			";;; $parameter$covariate-DEFINITION END\n\n",
			@code );
	}

	# Add to the parameter code
	unless ( $found_REL ) {
		my $success = 0;
		for ( @code ) {
			if ( /^\s*IPRED\s*=/) {
				my ($line,$comment) = split( ';', $_, 2 );
				$_ = $line;
				chomp;
				s/\s*$//;
				my $newterm = '+D_ETA'.$etanum.'*OGK_'."$parameter".
				'*(GZ_'."$parameter".'-OGZ_'."$parameter".')';
				if (length($_."$newterm".';'.$comment) > 70){
					$_ = $_.';'.$comment."\nIPRED=IPRED"."$newterm\n";
				}else{
					$_ = $_.$newterm.';'.$comment."\n";
				}
				$success = 1;
				last; #so not add term on multiple lines
			}
		}
		unless ( $success ) {
			croak("Could not determine a good place to add the covariate relation.\n".
				" i.e. No IPRED= was found\n" );
		}
	}
	if ( $use_pred ) {
		$applicant_model -> pred( problem_number => 1,
			new_pred       => \@code );
	} else {
		$applicant_model -> pk( problem_number => 1,
			new_pk         => \@code );
	}

	#must have initial_values first with add_if_absent
	$applicant_model -> initial_values( parameter_numbers => [[$start_theta..$end_theta]],
		new_values        => [\@inits],
		add_if_absent     => 1,
		parameter_type    => 'theta',
		problem_numbers   => [1]);
	$applicant_model -> upper_bounds( parameter_type    => 'theta',
		parameter_numbers => [[$start_theta..$end_theta]],
		problem_numbers   => [1],
		new_values        => [$bounds{'upper'}] );
	$applicant_model -> lower_bounds( parameter_type    => 'theta',
		parameter_numbers => [[$start_theta..$end_theta]],
		problem_numbers   => [1],
		new_values        => [$bounds{'lower'}] );
	$applicant_model -> labels( parameter_type    => 'theta',
		parameter_numbers => [[$start_theta..$end_theta]],
		problem_numbers   => [1],
		new_values        => [\@labels] );

	return $applicant_model;
}

sub add_code_gfunc
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		parameter_G => { isa => 'HashRef', optional => 1 },
		parameter_relation => { isa => 'HashRef', optional => 1 },
		applicant_model => { isa => 'model', optional => 1 }
	);
	my %parameter_G = defined $parm{'parameter_G'} ? %{$parm{'parameter_G'}} : ();
	my %parameter_relation = defined $parm{'parameter_relation'} ? %{$parm{'parameter_relation'}} : ();
	my $applicant_model = $parm{'applicant_model'};

	#parameter_G contains all parameters,
	#for some the value is 1, for others it is parCOV.
	#parameter_relation is either additive, proportional or exponential
	my @code = @{$applicant_model -> pk( problem_number => 1 )};
	my $use_pred = 0;
	unless ( $#code > 0 ) {
		@code = @{$applicant_model -> pred( problem_number => 1 )};
		$use_pred = 1;
	}
	if ( $#code <= 0 ) {
		croak("Neither PK or PRED defined in " .
			$applicant_model -> filename . "\n" );
	}

	push(@code,';;;SCM-LINEARIZE_CONSTANTS'."\n") unless ($self->directory_name_prefix eq 'linearize');
	foreach my $parameter (keys %parameter_G){
		push(@code,'OGZ_'.$parameter.'='.$parameter_G{$parameter}."\n");
		if ($parameter_relation{$parameter} eq 'exponential'){
			push(@code,'OGK_'.$parameter.'=1/'.$parameter_G{$parameter}."\n");
		}elsif ($parameter_relation{$parameter} eq 'proportional'){
			push(@code,'OGK_'.$parameter.
				'='.$parameter.'/(TV'.$parameter.'*'.$parameter_G{$parameter}.')'.
				' ; This gives (1+ETA)/'.$parameter.'COV'."\n");
		}elsif ($parameter_relation{$parameter} eq 'additive'){
			push(@code,'OGK_'.$parameter.
				'=TV'.$parameter.'/'.$parameter_G{$parameter}."\n");
		}elsif ($parameter_relation{$parameter} eq 'logit'){
			push(@code,'OGK_'.$parameter.
				"=1\n");
		}else{
			croak("No relation (additive/exponential/proportional) defined".
				"for ETA on $parameter");
		}
	}
	if ( $use_pred ) {
		$applicant_model -> pred( problem_number => 1,
			new_pred       => \@code );
	} else {
		$applicant_model -> pk( problem_number => 1,
			new_pk         => \@code );
	}

	return $applicant_model;
}

sub run_xv_pred_step
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		estimation_model => { isa => 'model', optional => 1 },
		derivatives_run => { isa => 'Bool', default => 0, optional => 1 },
		model_name => { isa => 'Str', optional => 0 }
	);
	my $estimation_model = $parm{'estimation_model'};
	my $derivatives_run = $parm{'derivatives_run'};
	my $model_name = $parm{'model_name'};

	#make copy of $estimation_model, set data to pred_data
	#pred_data will be derivatives if linearized
	#update inits from $estimation_model -> outputs
	my $oldcat = ui -> category();
	ui -> category('xv_scm');

	my $directory = 'xv_pred_dir_'.$model_name.'/';
	my $base_directory = $self -> directory.'xv_dir/';
	my $mess = "Running xv prediction step for $model_name".'.mod';
	if ($derivatives_run){
		$directory = 'derivatives_xv_pred_dir/';
		$mess = "Running xv prediction derivatives model";
	}
	unless (-d $base_directory){
		mkdir ($base_directory);
	}

	chdir($base_directory);

	my $model_copy_pred = $estimation_model -> copy ( filename => $base_directory.$model_name.'.mod',
													  copy_datafile          => 0,
													  write_copy =>0,
													  copy_output        => 0);

	$model_copy_pred -> datafiles(new_names =>[$self->xv_pred_data]);

	$model_copy_pred -> update_inits(from_output => $estimation_model-> outputs -> [0]);
	$model_copy_pred -> set_maxeval_zero(print_warning => 0,
										 need_ofv => 1,
										 last_est_complete => $self->last_est_complete());

	$model_copy_pred -> _write();

	my $xv_base_fit = tool::modelfit -> new
		( %{common_options::restore_options(@common_options::tool_options)},
		  base_directory => $base_directory,
		  directory      => $base_directory.$directory,
		  models         => [$model_copy_pred],
		  top_tool       => 0,
		  clean => 1,
		  parent_tool_id   => $self -> tool_id,
		  threads        => 1,
		  copy_data => 1); 
#clean 2 later
	ui -> print( category => 'xv_scm',
		message  => $mess ) unless ( $self -> parent_threads > 1 );
	$xv_base_fit -> run;

	if ($derivatives_run){
		#change $self->xv_pred_data to new filename from derivatives output.
		#this makes it impossible to use update_derivatives unless original pred_data is kept

		my $datafilename = 'derivatives_covariates.dta';
		my $newfilename = 'derivatives_covariates_pred.dta';

		cp($datafilename,$newfilename);
		unlink($datafilename);
		my ( $dir, $file ) = OSspecific::absolute_path('',$newfilename);

		$self->xv_pred_data($dir.$file);

	}else{
		#not a derivatives run
		if ( defined $model_copy_pred->outputs() and 
			defined $model_copy_pred->outputs()->[0] and
			$model_copy_pred->outputs()->[0]-> have_output() and
			defined $model_copy_pred->outputs->[0]->get_single_value(attribute=> 'ofv') ) {
			my $xv_ofv = $model_copy_pred -> outputs->[0]->get_single_value(attribute=> 'ofv');
			open( XV, ">>".$self -> xv_results_file );
			print XV "$xv_ofv : $model_name\n";
			close XV;
			$self->xv_results({}) unless (defined $self->xv_results);
			my $st = $self->step_number();
			if ($model_name =~ /base/){
				$st = 0;
			}
			$self->xv_results->{$st}{'ofv'} = $xv_ofv;
			$self->xv_results->{$st}{'relation'} = $model_name;
		}else{
			print "Warning: could not retrieve OFV from xv pred run.\n";
		}
	}
	chdir('..');
	ui -> category($oldcat);
}

sub format_inits_bounds
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		string => { isa => 'Str', optional => 0 },
		continuous => { isa => 'Bool', optional => 0 },
		is_bound => { isa => 'Bool', optional => 0 },
		statistics => { isa => 'HashRef', optional => 0 }
	);
	my $string = $parm{'string'};
	my $continuous = $parm{'continuous'};
	my $is_bound = $parm{'is_bound'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : ();
	my $value;

	my $median = $statistics{'median'};
	my $mean = $statistics{'mean'};
	my $max = $statistics{'max'};
	my $min = $statistics{'min'};

	if ($continuous){
		$string =~ s/median/$median/g;
	}elsif ( $string =~ /median/){
		croak("The median is undefined for categorical covariates and cannot be used"
			." in inits or bounds.");
	}
	if ($continuous){
		$string =~ s/mean/$mean/g;
	}elsif ( $string =~ /mean/){
		croak("The mean is undefined for categorical covariates and cannot be used"
			." in inits or bounds.");
	}
	$string =~ s/maximum/$max/g;
	$string =~ s/minimum/$min/g;
	$value = eval($string);
	if ($is_bound){
		if ($value < 1 and $value > 0){
			$value = sprintf "%.6f", $value; #need to control so dont get e notation
			$value     = '0' if eval($value) == 0;
		}elsif ($value > -1 and $value < 0){
			$value = sprintf "%.5f", $value; #need to control so dont get e notation
			$value     = '0' if eval($value) == 0;
		}else{
			$value = sprintf "%6.2f", $value; #need to control so dont get e notation
			my ($big,$small) = split('\.',$value);
			$small           = substr($small,0,3);
			if ((length($big)+ length($small)) > 7){
				$value = $big;
			}else{
				$value     = $big.'.'.$small;
			}
			$value     = '0' if eval($value) == 0;
		}
	}

	return $value;
}

sub create_linear_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		start_theta => { isa => 'Int', optional => 1 },
		parameter => { isa => 'Str', optional => 1 },
		covariate => { isa => 'Str', optional => 1 },
		continuous => { isa => 'Bool', optional => 1 },
		statistics => { isa => 'HashRef', optional => 1 },
		sum_covariates => { isa => 'Bool', default => 0, optional => 1 },
		missing_data_token => { isa => 'Str', optional => 1 },
		code => { isa => 'ArrayRef[Str]', optional => 1 },
		inits => { isa => 'ArrayRef[Num]', optional => 1 },
		bounds => { isa => 'HashRef', optional => 1 }
	);
	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : ();
	my $sum_covariates = $parm{'sum_covariates'};
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();

	# Creates code for linear continuous and categorical
	# parameter-covariate relations Returns code, the number of
	# thetas necessary for the relation plus initial values and
	# boundaries for these.
	if ( defined $bounds{'upper'} and ref $bounds{'upper'} ne 'ARRAY' ) {
		croak("Wrong data type of upper bounds. The upper bounds should be kept in an array" );
	}
	if ( defined $bounds{'lower'} and ref $bounds{'lower'} ne 'ARRAY' ) {
		croak("Wrong data type of lower bounds. The lower bounds should be kept in an array" );
	}

	my $theta_number = defined $start_theta ? $start_theta : 1;
	my $comment = "";
	my $median = $statistics{'median'};
	$median = sprintf "%6.2f", $median;
	$median =~ s/\s*//;
	my $min = $statistics{'min'};
	$min = sprintf "%6.2f", $min;
	$min =~ s/\s*//;
	my $max = $statistics{'max'};
	$max = sprintf "%6.2f", $max;
	$max =~ s/\s*//;
	my $offset = '1';
	$offset = '0' if ($sum_covariates);
	if ( $continuous ) {
		if ( scalar @code < 1 or ( scalar @code == 1 and $code[0] eq '' )
				or ( scalar @code == 1 and $code[0] eq 'linear' )) {
			if( $statistics{'have_missing_data'} ) {
				$code[0] = $comment."IF($covariate.EQ.$missing_data_token) THEN\n";
				$code[1] = "$comment   $parameter$covariate = $offset\n";
				$code[2] = $comment."ELSE\n";
				my $sign = '-';
				if ( $median < 0 ) { $sign = '+'; $median = -$median; }
				$code[3] = "$comment   $parameter$covariate = ( $offset + THETA(".$theta_number++.")*($covariate $sign $median))\n";
				$code[4] = $comment."ENDIF\n";
			} else {
				my $sign = '-';
				if ( $median < 0 ) { $sign = '+'; $median = -$median; }
				$code[0] = "$comment$parameter$covariate = ( $offset + THETA(".$theta_number++.")*($covariate $sign $median))\n";
			}
		} else {
			#state has been redefined
			#should never end up here
			ui->print(category => 'scm',
				message => "Redefined code in the linear section should never be encountered. Please report this error to the PsN development team.",newline => 1);
			for ( @code ) {
				my $copy = $_;
				while ($copy =~ s/THETA\((\d+)\)//){
					$theta_number++;
				}
			}
		}
		# Boundaries

		unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[0] ) {
			if ($sum_covariates){
				$bounds{'upper'}[0] = 20;
			}else{
				my $upper_bound;
				if ( $median-$min == 0 ) {
					my $mes="median($median)-min($min)".
					" for column $covariate is zero. Cannot calculate an upper boundary.";
					carp($mes );
					$upper_bound=100000;
				} else{
					$upper_bound     = 1/($median-$min);
				}
				my ($big,$small) = split('\.',$upper_bound);
				$small           = substr($small,0,3);
				$upper_bound     = $big.'.'.$small;
				$upper_bound     = '0' if eval($upper_bound) == 0;
				$bounds{'upper'}[0] = $upper_bound;
			}
		}
		unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[0] ) {
			if ($sum_covariates){
				$bounds{'lower'}[0] = -20;
			}else{
				my $lower_bound; 
				if ( $median-$max == 0 ) {
					my $mes = "median($median)-max($min)".
					" for column $covariate is zero. Cannot calculate a lower boundary.";
					carp($mes );
					$lower_bound=-100000;
				}else{
					$lower_bound     = 1/($median - $max);
				}
				my ($big,$small)    = split('\.',$lower_bound);
				$small           = substr($small,0,3);
				$lower_bound     = $big.'.'.$small;
				$lower_bound     = '0' if eval($lower_bound) == 0;
				$bounds{'lower'}[0] = $lower_bound;
			}
		}

		## Boundaries:

	} else {
		#categorical
		my %factors = %{$statistics{'factors'}};
		my @levels = sort {$a<=>$b} keys %factors;
		my @sorted = sort {$factors{$b}<=>$factors{$a}} keys(%factors); #most common first
		my $numlvs = scalar @levels;
		$numlvs = $numlvs -1 if $statistics{'have_missing_data'};
		my $sum_values=0;
		my $fraction;
		if ($self->linearize()){
			if ($numlvs > 2){
				croak("linearize option does not yet work with categorical covariates with more than two categories");
			}else{
				foreach my $key (@sorted){
					#switch for experiment
					$sum_values += $factors{$key} unless ($statistics{'have_missing_data'} and
						( $self -> missing_data_token eq $key ));
				}
			}
		}
		my $first_non_missing = 1;
		my $missing_line;
		if ( scalar @code < 1 or ( scalar @code == 1 and $code[0] eq '') 
				or ( scalar @code == 1 and $code[0] eq 'linear') ) {
			@code = ();
			#initiate COMMON parameter if linearize and have missing data
			if ($self->linearize() and $statistics{'have_missing_data'}){
				push @code, $comment."$parameter$covariate"."_COMMON=0  ; Initiate variable";
			}
			for ( my $i = 0; $i <= $#sorted; $i++ ) {
				if ( $statistics{'have_missing_data'} and
					( $self -> missing_data_token eq $sorted[$i] 
							or $self -> missing_data_token == $sorted[$i]) ) {
					if ($self->linearize()){
						#comment for experiment
						$missing_line = $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
						" = $offset  ; Missing data\n";
						1;
					}else{
						push @code, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
						" = $offset  ; Missing data\n";
					}
				} else {
					if ( $first_non_missing ) {
						if ($self->linearize()){
							$fraction=$factors{$sorted[$i]}/$sum_values;
							$fraction = sprintf("%.6G",$fraction);
							#comment for experiment
							push @code, $comment."; Frequency of most common case is ".$factors{$sorted[$i]}.
							"/".$sum_values."=$fraction";
							push @code, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
							"_COMMON=$offset; Most common case, indicator variable is $offset";
						}else{
							push @code, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
							" = $offset  ; Most common\n";
						}
						$first_non_missing = 0;
					} else {
						if ($self->linearize()){
							push @code, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
							"_COMMON=0";
							push @code, $comment."$parameter$covariate = ($offset + THETA(".$theta_number++.
							")*($fraction-$parameter$covariate"."_COMMON)) \n";
							#comment for experiment
							if (defined $missing_line){
								push @code,$missing_line;
							}
						}else{
							push @code, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
							" = ( $offset + THETA(".$theta_number++."))\n";
						}
					}
				}
			}
		} else {
			# count the thetas.
			#should never end up here
			ui->print(category => 'scm',
				message => "Redefined code in the linear section should never be encountered. Please report this error to the PsN development team.",newline => 1);
			for ( @code ) {
				my $copy = $_;
				while ($copy =~ s/THETA\((\d+)\)//){
					$theta_number++;
				}
			}
		}
		## Boundaries:
		for ( my $i = 0; $i < $theta_number - $start_theta; $i++ ) {
			if ($sum_covariates){
				$bounds{'upper'}[$i] = 20
				unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[$i]);
				$bounds{'lower'}[$i] = -20
				unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[$i] );
			}else{
				if ($self->linearize()){
					if ($fraction == 1){
						unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[$i] ){
							$bounds{'upper'}[$i] = 5;
						}
						unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[$i] ){
							$bounds{'lower'}[$i] = -1  ;
						}
					}else{
						unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[$i] ){
							my $bound = 1/(1-$fraction);
							my ($big,$small) = split('\.',$bound);
							$small           = substr($small,0,3);
							$bound     = $big.'.'.$small;
							$bound     = '0' if eval($bound) == 0;
							$bounds{'upper'}[$i] = $bound;
						}
						unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[$i] ){
							my $bound = (-1/$fraction);
							my ($big,$small) = split('\.',$bound);
							$small           = substr($small,0,3);
							$bound     = $big.'.'.$small;
							$bound     = '0' if eval($bound) == 0;
							$bounds{'lower'}[$i] = $bound;
						}
					}
				}else{
					unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[$i] ){
						$bounds{'upper'}[$i] = 5;
					}
					unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[$i] ){
						$bounds{'lower'}[$i] = -1;
					}
				}
			}
		}
	}
	# Initial values
	for ( my $i = 0; $i < $theta_number - $start_theta; $i++ ) {
		unless ( defined $inits[$i] ){
			my $fraction = $self -> global_init;
			my $tmp;
			if( ( abs($bounds{'upper'}[$i]) >= 1000000 or 
					not defined $bounds{'upper'}[$i] ) and
				( abs($bounds{'lower'}[$i]) >= 1000000 or
					not defined $bounds{'lower'}[$i] ) ) {
				$tmp = $fraction*100;
			} else {
				if ( abs($bounds{'upper'}[$i]) < abs($bounds{'lower'}[$i]) ) {
					$tmp = $bounds{'upper'}[$i] == 0 ? $bounds{'lower'}[$i]*$fraction :
					$bounds{'upper'}[$i]*$fraction;
				} else {
					$tmp = $bounds{'lower'}[$i] == 0 ? $bounds{'upper'}[$i]*$fraction :
					$bounds{'lower'}[$i]*$fraction;
				}
			}
			$inits[$i] = $tmp;

		}else{
		}
	}
	$end_theta = --$theta_number;

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub create_hockey_stick_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		start_theta => { isa => 'Int', optional => 1 },
		parameter => { isa => 'Str', optional => 1 },
		covariate => { isa => 'Str', optional => 1 },
		continuous => { isa => 'Bool', optional => 1 },
		statistics => { isa => 'HashRef', optional => 1 },
		sum_covariates => { isa => 'Bool', default => 0, optional => 1 },
		missing_data_token => { isa => 'Str', optional => 1 },
		code => { isa => 'ArrayRef[Str]', optional => 1 },
		inits => { isa => 'ArrayRef[Num]', optional => 1 },
		bounds => { isa => 'HashRef', optional => 1 }
	);
	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : ();
	my $sum_covariates = $parm{'sum_covariates'};
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();

	my $theta_number = $start_theta;
	my $median = $statistics{'median'};
	$median = sprintf "%6.2f", $median;
	$median =~ s/\s*//;
	my $min = $statistics{'min'};
	$min = sprintf "%6.2f", $min;
	$min =~ s/\s*//;
	my $max = $statistics{'max'};
	$max = sprintf "%6.2f", $max;
	$max =~ s/\s*//;

	my $offset = '1';
	$offset = '0' if ($sum_covariates);
	if ( $continuous ) {
		if ( scalar @code < 1 or ( scalar @code == 1 and $code[0] eq '' )
				or ( scalar @code == 1 and $code[0] eq 'hockey-stick' )) {
			my $sign = '-';
			if ( $median < 0 ) { $sign = '+'; $median = -$median; }
			$code[0] = "IF($covariate.LE.$median) $parameter$covariate = ( $offset + THETA(".
			$theta_number++.")*($covariate $sign $median))\n";
			$code[1] = "IF($covariate.GT.$median) $parameter$covariate ".
			"= ( $offset + THETA(".$theta_number++.")*($covariate $sign $median))\n";
			if ( $statistics{'have_missing_data'} ) {
				$code[2] = "IF($covariate.EQ.$missing_data_token)   ".
				"$parameter$covariate = $offset\n";
			}
		} else {
			#should never end up here
			ui->print(category => 'scm',
				message => "Redefined code in the hockey-stick section should never be encountered. Please report this error.",newline => 1);
			for ( @code ) {
				my $copy = $_;
				while ($copy =~ s/THETA\((\d+)\)//){
					$theta_number++;
				}
			}
		}
		## Boundaries:
		unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[0] ) {
			if ($sum_covariates){
				$bounds{'upper'}[0] = 20;
			}else{
				croak("the median and min are equal ($min) for covariate $covariate, cannot use hockey-stick parameterization.") 
				if ($median == $min);
				my $upper_bound     = 1/($median-$min);
				my ($big,$small) = split('\.',$upper_bound);
				$small           = substr($small,0,3);
				$upper_bound     = $big.'.'.$small;
				$upper_bound     = '0' if eval($upper_bound) == 0;
				$bounds{'upper'}[0] = $upper_bound;
			}
		}
		unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[1] ) {
			if ($sum_covariates){
				$bounds{'upper'}[1] = 20;
			}else{
				$bounds{'upper'}[1] = 1000000;
			}
		}

		unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[0] ) {
			if ($sum_covariates){
				$bounds{'lower'}[0] = -20;
			}else{
				$bounds{'lower'}[0] = -1000000;
			}
		}
		unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[1] ) {
			if ($sum_covariates){
				$bounds{'lower'}[1] = -20;
			}else{
				croak("the median and max are equal ($max) for covariate $covariate, cannot use hockey-stick parameterization.") 
				if ($median == $max);
				my $lower_bound     = 1/($median - $max);
				my ($big,$small)    = split('\.',$lower_bound);
				$small           = substr($small,0,3);
				$lower_bound     = $big.'.'.$small;
				$lower_bound     = '0' if eval($lower_bound) == 0;
				$bounds{'lower'}[1] = $lower_bound;
			}
		}
	}else{
		ui->print(category => 'scm',
			message => "The hockey-stick relation is not defined for categorical covariates. ".
			"No code will be written to the model for $parameter-$covariate.",
			newline => 1);
		$code[0]=  "$parameter$covariate = $offset\n";
	}
	# Initial values
	for ( my $i = 0; $i < $theta_number - $start_theta; $i++ ) {
		unless ( defined $inits[$i] ){
			my $fraction = $self -> global_init;
			my $tmp;
			if( ( abs($bounds{'upper'}[$i]) >= 1000000 or 
					not defined $bounds{'upper'}[$i] ) and
				( abs($bounds{'lower'}[$i]) >= 1000000 or
					not defined $bounds{'lower'}[$i] ) ) {
				$tmp = $fraction*100;
			} else {
				if ( abs($bounds{'upper'}[$i]) < abs($bounds{'lower'}[$i]) ) {
					$tmp = $bounds{'upper'}[$i] == 0 ? $bounds{'lower'}[$i]*$fraction :
					$bounds{'upper'}[$i]*$fraction;
				} else {
					$tmp = $bounds{'lower'}[$i] == 0 ? $bounds{'upper'}[$i]*$fraction :
					$bounds{'lower'}[$i]*$fraction;
				}
			}
			$inits[$i] = $tmp;
		}
	}
	$end_theta = --$theta_number;

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub write_log
{
	my $self = shift;

	my %parm = @_;
	my $direction = $parm{'direction'};
	my $logfile = $parm{'logfile'};
	my $included_relations = $parm{'included_relations'};
	my $chosen_parameter = $parm{'chosen_parameter'};
	my $chosen_covariate = $parm{'chosen_covariate'};
	my $chosen_state = $parm{'chosen_state'};
	my $results = $parm{'results'};
	my $criterion = $parm{'criterion'};
	my $test_log_ref = $parm{'test_log'};
	my $append  = $parm{'append'};
	my $final  = (defined $parm{'final_short'}) ? $parm{'final_short'} : 0;
	my %test_log = %{$test_log_ref} if defined ( $test_log_ref );
	my @names = ();
	my @drops = ();
	my $chosen;
	foreach my $result ( @{$results} ) {
		@names = @{$result -> {'values'}} if ($result -> {'name'} eq 'combinations');
		@drops = @{$result -> {'values'}} if ($result -> {'name'} eq 'ofv.drop');
		$chosen = $result -> {'values'} if ($result -> {'name'} eq 'relation.chosen.in.step');
	}

	open( LOG, ">>$logfile" );

	if ( defined $chosen and (not $final)) {
		print LOG "Parameter-covariate relation chosen in this $direction step: ",
		"$chosen_parameter-$chosen_covariate-$chosen_state\n";
		print LOG sprintf("%-23s",'CRITERION'),uc( $criterion ),"\n" if (length($criterion)>0);
		my @names = sort keys %test_log;
		foreach my $name ( @names ) {
			my $val = $test_log{$name};
			if ( ref($val) eq 'HASH' ) {
				foreach my $name2 ( sort keys %{$val} ) {
					print LOG sprintf("%-20s",uc($name)),sprintf("%-30s",uc( $name2) ),
					sprintf("%12.5f",uc( $val -> {$name2} )),"\n";
				}
			} else {
				print LOG sprintf("%-20s",uc($name)),sprintf("%12.5f",uc( $val )),"\n";
			}
		}
	}
	if (defined $included_relations and scalar %{$included_relations} > 0 ) {
		if ($final){
			print LOG "Relations included after final step:\n";	    
		}else{
			print LOG "Relations included after this step:\n";
		}

		foreach my $par ( sort keys %{$included_relations} ) {
			print LOG sprintf("%-8s",$par);
			foreach my $cov ( sort keys %{$included_relations -> {$par}} ) {
				# Is this covariate continuous or not?
				my $continuous = 1;
				if (defined $self -> categorical_covariates()){
					foreach my $cat ( @{$self -> categorical_covariates()} ) {
						$continuous = 0 if ( $cov eq $cat );
					}
				}
				print LOG sprintf("%-8s",$cov.'-'.$included_relations -> {$par}{$cov}{'state'});
			}
			print LOG "\n";
		}

	}
	print LOG "--------------------\n\n";
	close( LOG );
}

sub drop_undrop_covariates
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		applicant_model => { isa => 'model', optional => 1 },
		used_covariates => { isa => 'ArrayRef[Str]', optional => 1 },
		all_covariates => { isa => 'ArrayRef[Str]', optional => 1 },
		do_not_drop => { isa => 'Maybe[ArrayRef[Str]]', optional => 1 }
	);
	my $applicant_model = $parm{'applicant_model'};
	my @used_covariates = defined $parm{'used_covariates'} ? @{$parm{'used_covariates'}} : ();
	my @all_covariates = defined $parm{'all_covariates'} ? @{$parm{'all_covariates'}} : ();
	my @do_not_drop = defined $parm{'do_not_drop'} ? @{$parm{'do_not_drop'}} : ();

	OUTER:	foreach my $cov ( @all_covariates ) {
		my $used = 0;
		foreach my $do_not_cov ( @do_not_drop ) {
			next OUTER if ( $cov eq $do_not_cov );
		}
		foreach my $used_cov ( @used_covariates ) {
			$used = 1 if ( $cov eq $used_cov );
		}
		if ( $used ) {
			carp("undropping $cov" );
			$applicant_model -> _option_val_pos ( problem_numbers  => [1],
				instance_numbers => [[1]],
				name             => $cov,
				record_name      => 'input',
				new_values       => [['']],
				exact_match      => 1 );
		} else {
			carp("dropping $cov" );
			$applicant_model -> _option_val_pos ( problem_numbers  => [1],
				instance_numbers => [[1]],
				name             => $cov,
				record_name      => 'input',
				new_values       => [['DROP']],
				exact_match      => 1 );
		}
	}

	return $applicant_model;
}

sub write_final_models
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		final_model => { isa => 'model', optional => 1 },
		model_number => { isa => 'Int', optional => 1 }
	);
	my $final_model = $parm{'final_model'};
	my $model_number = $parm{'model_number'};

	my $fname = 'final_'.$self->search_direction().'.mod';
	if ($self->linearize()){
		$fname = 'final_'.$self->search_direction().'_linear.mod';
	}
	my $fdir = $self->final_model_directory();
	return if (-e "$fdir$fname"); #otherwise may write twice

	ui -> print( category => 'scm',
		message => "Writing final models from the ".$self->search_direction()." search." );
	$final_model -> filename($fname);
	$final_model -> directory( $fdir);
	$fname =~ s/\.mod/\.lst/;
	return unless (-e $final_model->outputfile()); #unless lst-file exists (could have crashed)
	cp( $final_model -> outputfile, "$fdir$fname" );
	my $prob_num = undef;
	$final_model -> update_inits(from_output => $final_model->outputs->[0],
		problem_number => $prob_num);
	$final_model -> outputfile( "$fdir$fname" );
	if ($self->linearize()){
		#set datafilename to something ok
		$final_model -> ignore_missing_files(1);
		my $datafilename = 'derivatives_covariates.dta';
		if ($self->update_derivatives()){
			my $stepname='';
			if ($self->step_number()>1){
				$stepname = '_'.($self->step_number()-1);
				if ($self->search_direction() eq 'forward'){
					$stepname .= 'f';
				}else{
					$stepname .= 'b';
				}
			}
			$datafilename = 'derivatives_covariates'.$stepname.'.dta';
		}

		my @new_names = ($datafilename) x scalar(@{$final_model ->problems});
		$final_model -> datafiles(new_names => \@new_names); #one for each $PROB

	}else{
		$final_model -> ignore_missing_files(1);
		#ref to all data filenames
		my $datafilenames = $self->models()->[$model_number -1]->datafiles(absolute_path => 1);
		$final_model -> datafiles(new_names => $datafilenames); #one for each $PROB
	}
	$final_model -> _write;

	if ($self->linearize()){
		#create final nonlinear model
		my $final_nonlin = model -> new ( filename => $self->final_model_directory().'original.mod',
										  ignore_missing_files => 1);
		$final_nonlin ->filename('final_'.$self->search_direction().'_nonlinear.mod');
		#add all included  relations

		my %included_relations;
		%included_relations = %{$self -> included_relations} if 
		(defined $self -> included_relations);
		foreach my $incl_par ( sort keys %included_relations ) {
			foreach my $incl_cov ( sort keys %{$included_relations{$incl_par}} ) {
				$self -> 
				add_code( definition_code => $included_relations{$incl_par}{$incl_cov}{'code'},
					nthetas         => $included_relations{$incl_par}{$incl_cov}{'nthetas'},
					inits           => $included_relations{$incl_par}{$incl_cov}{'inits'},
					bounds          => $included_relations{$incl_par}{$incl_cov}{'bounds'},
					applicant_model => $final_nonlin,
					sum_covariates  => $self->sum_covariates_hash->{$incl_par},
					parameter       => $incl_par,
					covariate       => $incl_cov );
			}
		}
		#update initials, from initial_estimates_model??? from where?
		if ($self->update_derivatives()){
			my $fb = ($self->search_direction() eq 'forward')? 'f' : 'b';
			my $outf='scm_dir1/derivatives_updated_'.($self->step_number()).$fb.'.lst'; 
			if (-e $outf){
				$final_nonlin->update_inits(from_output_file => $outf,
					problem_number => $prob_num);
			}else{
				#print "nothing left to add? could not find $outf\n";
				#we will also end up here if first step and nothing significant. Would liek
				# to keep all estimates from derivatives run in that case, but ahve no way of 
				#separating cases right now.
				$outf='derivatives_updated_'.($self->step_number()-1).$fb.'.lst'; 
				$outf='derivatives.lst' if ($self->step_number() == 1);
				if (-e $outf){
					$final_nonlin->update_inits(from_output_file => $outf,
						ignore_missing_parameters => 1,
						problem_number => $prob_num);
				}else{
					my $outf2='copy_last_forward_derivatives.lst';
					if (-e $outf2){
						$final_nonlin->update_inits(from_output_file => $outf2,
							ignore_missing_parameters => 1,
							problem_number => $prob_num);
					}else{
					}
				}
				$final_nonlin -> update_inits(from_model => $final_model,
					ignore_missing_parameters => 1,
					problem_number => $prob_num);
			}

		}else{
			#if not update derivatives then first take original derivatives, ignore missing,
			#and then final linear, by labels ,ignore missing
			my $outf= $fdir.'../derivatives.lst'; 
			if (-e $outf){
				$final_nonlin->update_inits(from_output_file => $outf,
					ignore_missing_parameters => 1,
					problem_number => $prob_num);
			}else{
			}
			$final_nonlin -> update_inits(from_model => $final_model,
				ignore_missing_parameters => 1,
				problem_number => $prob_num);
		}
		$final_nonlin -> _write();
		$final_nonlin = undef;
	}

	return $final_model;
}

sub modelfit_post_fork_analyze
{
	my $self = shift;

	# It is not necessary to collect the included relations from
	# the runs if no parallelism has been used or if only one run
	# has been performed. Now there is allways parallelism! see
	# tool::run and Readme.txt

	my @included_relations = ();
	foreach my $return ( @{$self -> results->[0]{'own'}} ) {
		if ( $return -> {'name'} eq 'included.relations' ){
			$self -> included_relations($return -> {'values'});
		}
		if ( $return -> {'name'} eq 'base.criteria.values' ){
			$self -> base_criteria_values( $return -> {'values'} );
		}
	}

	if ($self->search_direction() eq 'forward' and $self->both_directions() 
			and $self->step_number()==1 and 
		scalar(@{$self->models}) == 1) {
		#we are all but done with the $scm->run called from the top script.
		#experiment, try switching directions here instead of returning
		ui -> print( category => 'scm',
			message => "Starting scm backward search inside forward top level directory" );

		open( LOG, ">>".$self -> logfile -> [0] );
		print LOG "\n--------------------\n";
		print LOG "Forward search done. Starting backward search inside forward top level directory\n";
		close LOG;

		# Check the thread number of this tool level:
		my $threads = ref( $self -> threads ) eq 'ARRAY' ? 
		$self -> threads -> [0]:$self -> threads;
		# More threads than models?
		$threads = 1 if ( $threads > 1);

		if (defined $self->p_backward()){
			#p_backward will never be defined for other than top level scm
			$self->p_value($self->p_backward());
		}
		if (defined $self->ofv_backward()){
			#ofv_backward will never be defined for other than top level scm
			$self->ofv_change($self->ofv_backward());
		}
		#warn cannot set backwards dir if direction is both
		$self->search_direction('backward');
		my $cpu_time = defined $self -> cpu_time ? int(($self -> cpu_time)*1.2) : undef;
		my $num = scalar @{$self -> models};

		# Own_threads is used to set parent_threads for child tools
		my $own_threads = ref( $self -> threads ) eq 'ARRAY' ?
		$self -> threads -> [0]:$self -> threads;
		# More threads than models?
		my $num = scalar @{$self -> models};
		$own_threads = $num if ( $own_threads > $num );

		#increase step index and step_number in subtool as always
		#this stepnumber will be 1 (the default)
		#below is copied from modelfit_analyze, except modelnumber is 1 and all indices 0
		#and base_criteria_values unchanged (wild guess)
		#both directions is set

		my $backward_scm =
		tool::scm ->
		new( %{common_options::restore_options(@common_options::tool_options)},
			 main_data_file            => undef,
			gof                    => $self -> gof(),
			test_relations         => $self -> test_relations,
			parameters             => $self -> parameters,
			categorical_covariates => $self -> categorical_covariates(),
			continuous_covariates  => $self -> continuous_covariates(),
			do_not_drop            => $self -> do_not_drop,
			ofv_change             => $self -> ofv_change,
			p_value                => $self -> p_value,
			search_direction       => $self -> search_direction,
			both_directions        => 1,
			valid_states           => $self -> valid_states,
			covariate_statistics_file => $self -> covariate_statistics_file,
			relations_file         => $self -> relations_file,
			short_logfile          => [$self -> short_logfile ->[0]],
			bounds                 => $self -> bounds,
			cpu_time             => $cpu_time,
			xv_pred_data         => $self -> xv_pred_data,
			max_steps             => $self -> max_steps,
			xv_results         => $self -> xv_results,
			global_init          => $self -> global_init,
			covariate_statistics => $self -> covariate_statistics,
			directory            => $self -> directory.'/backward_scm_dir1',
			models               => [$self -> models->[0]],
			relations            => $self -> relations(),
			initial_estimates_model => $self -> initial_estimates_model,
			included_relations   => $self -> included_relations,
			append_log           => 1,
			step_number          => ($self -> step_number() + 1),
			raw_results_file     => [$self -> raw_results_file ->[0]],
			logfile              => [$self -> logfile ->[0]],
			base_criteria_values => $self->base_criteria_values,
			parent_tool_id       => $self -> tool_id,
			parent_threads       => $own_threads , 
			top_tool             => 0,
			logit                => $self->logit(),
			linearize                 => $self->linearize,
			foce                 => $self->foce,
			second_order         => $self->second_order,
			only_successful        => $self->only_successful(),
			parameter_eta        => $self->parameter_eta,
			parameter_relation   => $self->parameter_relation,
			derivatives_base_model => $self->derivatives_base_model,
			data_items    => $self->data_items(),
			sizes_pd    => $self->sizes_pd(),
			derivatives_output    => $self->derivatives_output(),
			update_derivatives    => $self->update_derivatives(),
			error                 => $self->error(),
			error_code           => $self->error_code(),
			epsilon           => $self->epsilon(),
			parallel_states     => $self->parallel_states(),
			config_file          => undef,
			resulting_model      => undef,
			xv_results_file => $self->xv_results_file(),
			final_model_directory => $self->final_model_directory());

		$backward_scm -> run;
	}
}

sub read_config_file
{
	my $self = shift;

	unless( defined $self -> config_file ){
		$self -> config_file(config_file -> new( file -> new( path => './', name => $self -> config_file_name ) ));
	}

	my $config_file = $self -> config_file;

	if( defined( $config_file -> relations ) ){
		$self -> relations($config_file -> relations);
	}

	foreach my $config_option ( keys %{$config_file -> valid_scalar_options}, 
		keys %{$config_file -> valid_array_options},
		keys %{$config_file -> valid_hash_options},
		keys %{$config_file -> valid_code_options} ){

		# These are options passed to the modelfile in bin/scm.pl
		next if( $config_option eq 'extra_files' );
		next if( $config_option eq 'model' );
		next if( $config_option eq 'p_forward' );
		next if( $config_option eq 'p_backward' );
		next if( $config_option eq 'ofv_backward' );
		next if( $config_option eq 'upper_bounds' );
		next if( $config_option eq 'lower_bounds' );
		next if( $config_option eq 'inits' );
		next if( $config_option eq 'code' );

		# Handle some special cases (where the option is per model or option is logfile)
		if( $config_option eq 'base_criteria_values' and defined $config_file -> base_criteria_values ){
			$self -> base_criteria_values($config_file -> base_criteria_values);
		} elsif( $config_option eq 'test_relations' and defined $config_file -> test_relations ){
			$self -> test_relations($config_file -> test_relations);
		} elsif( $config_option eq 'included_relations' and defined $config_file -> included_relations ){
			$self -> included_relations($config_file -> included_relations);
		} elsif( $config_option eq 'logfile' and defined $config_file -> logfile ){
			$self -> logfile([$config_file -> logfile]);
		} elsif( $config_option eq 'valid_states' and defined $config_file -> valid_states ){
			if( not defined $config_file -> valid_states -> {'continuous'} ) {
				carp("The valid_states section is defined in the configuration file but ".
					"no states were defined for continuous covariates. Assuming the default valid states: ".
					join( ', ',@{$self -> valid_states -> {'continuous'}}) );
			} else {
				$self -> valid_states -> {'continuous'} = $config_file -> valid_states -> {'continuous'};
			}
			if( not defined $config_file -> valid_states -> {'categorical'} ) {
				carp("The valid_states section is defined in the configuration file but ".
					"no states were defined for categorical covariates. Assuming the default valid states: ".
					join( ', ',@{$self -> valid_states -> {'categorical'}}) );
			} else {
				$self -> valid_states -> {'categorical'} = $config_file -> valid_states -> {'categorical'};
			}
		} elsif( defined $config_file -> $config_option  ){

			# This is the general case where we just copy the option.
			$self -> $config_option($config_file -> $config_option);
		}
	}
}

sub create_exponential_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		start_theta => { isa => 'Int', optional => 1 },
		parameter => { isa => 'Str', optional => 1 },
		covariate => { isa => 'Str', optional => 1 },
		continuous => { isa => 'Bool', optional => 1 },
		statistics => { isa => 'HashRef', optional => 1 },
		sum_covariates => { isa => 'Bool', default => 0, optional => 1 },
		missing_data_token => { isa => 'Str', optional => 1 },
		code => { isa => 'ArrayRef[Str]', optional => 1 },
		inits => { isa => 'ArrayRef[Num]', optional => 1 },
		bounds => { isa => 'HashRef', optional => 1 }
	);
	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : ();
	my $sum_covariates = $parm{'sum_covariates'};
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();

	my $theta_number = $start_theta;
	my $median = $statistics{'median'};
	$median = sprintf "%6.2f", $median;
	$median =~ s/\s*//;
	my $min = $statistics{'min'};
	$min = sprintf "%6.2f", $min;
	$min =~ s/\s*//;
	my $max = $statistics{'max'};
	$max = sprintf "%6.2f", $max;
	$max =~ s/\s*//;
	my $offset = '1';
	ui->print(category => 'scm',
		message => "Warning: The exponential relation is inappropriate on ".
		"logit parameters.",newline => 1) if ($sum_covariates);

	if ( $continuous ) {
		if ( scalar @code < 1 or ( scalar @code == 1 and $code[0] eq '' ) 
				or ( scalar @code == 1 and $code[0] eq 'exponential' )) {
			my $sign = '-';
			if ( $median < 0 ) { $sign = '+'; $median = -$median; }
			if ( $statistics{'have_missing_data'} ) {
				$code[0] = "IF($covariate.EQ.$missing_data_token) THEN\n".
				"   $parameter$covariate = $offset\n".
				"ELSE\n".
				"   $parameter$covariate = EXP(THETA(".$theta_number++.")*($covariate $sign $median))\n".
				"ENDIF\n";
			}else{
				$code[0] = "   $parameter$covariate = EXP(THETA(".$theta_number++.")*($covariate $sign $median))\n";
			}
		} else {
			#state has been redefined
			#should never end up here
			ui->print(category => 'scm',
				message => "Redefined code in the exponential section should never be encountered. Please report this error to the PsN developtment team.",newline => 1);
			for ( @code ) {
				my $copy = $_;
				while ($copy =~ s/THETA\((\d+)\)//){
					$theta_number++;
				}
			}
		}
	}else{
		ui->print(category => 'scm',
			message => "The exponential relation is not defined for categorical covariates. ".
			"No code will be written to the model for $parameter-$covariate.",
			newline => 1);
		$code[0]=  "$parameter$covariate = $offset\n";
	}
	# Initial values and boundaries
	for ( my $i = 0; $i < $theta_number - $start_theta; $i++ ) {
		unless ( defined $inits[$i] ){
			$inits[$i] = $self -> global_init; #0.001
		}
		unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[$i] ) {
			$bounds{'upper'}[$i] = 1000000;
		}
		unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[$i] ) {
			$bounds{'lower'}[$i] = -1000000;
		}
	}

	$end_theta = --$theta_number;

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub create_power_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		start_theta => { isa => 'Int', optional => 1 },
		parameter => { isa => 'Str', optional => 1 },
		covariate => { isa => 'Str', optional => 1 },
		continuous => { isa => 'Bool', optional => 1 },
		statistics => { isa => 'HashRef', optional => 1 },
		sum_covariates => { isa => 'Bool', default => 0, optional => 1 },
		missing_data_token => { isa => 'Str', optional => 1 },
		code => { isa => 'ArrayRef[Str]', optional => 1 },
		inits => { isa => 'ArrayRef[Num]', optional => 1 },
		bounds => { isa => 'HashRef', optional => 1 }
	);
	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : ();
	my $sum_covariates = $parm{'sum_covariates'};
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();

	my $theta_number = $start_theta;
	if ($statistics{'min'} < 0){
		ui -> print( category => 'scm',
			message  => "Warning: Creating power code for $covariate which has minimum < 0. ".
			"Covariate function value ".
			"may be negative or imaginary for observations where $covariate is < 0, which would lead to ".
			"errors.",newline => 1);
	}
	my $median = $statistics{'median'};
	$median = sprintf "%6.2f", $median;
	$median =~ s/\s*//;
	my $min = $statistics{'min'};
	$min = sprintf "%6.2f", $min;
	$min =~ s/\s*//;
	my $max = $statistics{'max'};
	$max = sprintf "%6.2f", $max;
	$max =~ s/\s*//;
	my $offset = '1';
	ui->print(category => 'scm',
		message => "Warning: The exponential relation is inappropriate on ".
		"logit parameters.",newline => 1) if ($sum_covariates);

	if ( $continuous ) {
		if ( scalar @code < 1 or ( scalar @code == 1 and $code[0] eq '' ) 
				or ( scalar @code == 1 and $code[0] eq 'power' )) {
			my $sign = '';
			if ( $median < 0 ) { 
				$sign = '-'; 
				$median = -$median; 
			}
			if ( $statistics{'have_missing_data'} ) {
				$code[0] = "IF($covariate.EQ.$missing_data_token) THEN\n".
				"   $parameter$covariate = $offset\n".
				"ELSE\n".
				"   $parameter$covariate = (($sign$covariate/$median)**THETA(".$theta_number++."))\n".
				"ENDIF\n";
			}else{
				$code[0] = "   $parameter$covariate = (($sign$covariate/$median)**THETA(".$theta_number++."))\n";
			}
		} else {
			#state has been redefined
			#should never end up here
			ui->print(category => 'scm',
				message => "Redefined code in the power section should never be encountered. Please report this error.",newline => 1);
			for ( @code ) {
				my $copy = $_;
				while ($copy =~ s/THETA\((\d+)\)//){
					$theta_number++;
				}
			}
		}
	}else{
		ui->print(category => 'scm',
			message => "The power relation is not defined for categorical covariates. ".
			"No code will be written to the model for $parameter-$covariate.",
			newline => 1);
		$code[0]=  "$parameter$covariate = $offset\n";
	}
	# Initial values and boundaries
	for ( my $i = 0; $i < $theta_number - $start_theta; $i++ ) {
		unless ( defined $inits[$i] ){
			$inits[$i] = $self -> global_init; #0.001
		}
		unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[$i] ) {
			$bounds{'upper'}[$i] = 1000000;
		}
		unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[$i] ) {
			$bounds{'lower'}[$i] = -1000000;
		}
	}
	$end_theta = --$theta_number;

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub create_user_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		start_theta => { isa => 'Int', optional => 1 },
		parameter => { isa => 'Str', optional => 1 },
		covariate => { isa => 'Str', optional => 1 },
		continuous => { isa => 'Bool', optional => 1 },
		statistics => { isa => 'HashRef', optional => 1 },
		sum_covariates => { isa => 'Bool', default => 0, optional => 1 },
		missing_data_token => { isa => 'Str', optional => 1 },
		code => { isa => 'ArrayRef[Str]', optional => 1 },
		inits => { isa => 'ArrayRef[Num]', optional => 1 },
		bounds => { isa => 'HashRef', optional => 1 }
	);
	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : ();
	my $sum_covariates = $parm{'sum_covariates'};
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();

	my $median = $statistics{'median'};
	$median = sprintf "%6.2f", $median;
	$median =~ s/\s*//;
	my $min = $statistics{'min'};
	$min = sprintf "%6.2f", $min;
	$min =~ s/\s*//;
	my $max = $statistics{'max'};
	$max = sprintf "%6.2f", $max;
	$max =~ s/\s*//;
	my $mean = $statistics{'mean'};
	$mean = sprintf "%6.2f", $mean;
	$mean =~ s/\s*//;

	my $theta_number = $start_theta;
	my %unique_thetas;
	if ( scalar @code < 1 or ( scalar @code == 1 and $code[0] eq '' ) ) {
		croak("State cannot be used when not defined in the [code] section.");
	} else {
		# count the thetas.
		for ( @code ) {
			if ($continuous){
				s/median/$median/g;
			}else{
				if (/median/){
					croak("The median is undefined for categorical covariates and cannot ".
						"be used in user-written code.");
				}
			}
			if ($continuous){
				s/mean/$mean/g;
			}else{
				if (/mean/){
					croak("The mean is undefined for categorical covariates and cannot ".
						"be used in user-written code.");
				}
			}
			s/maximum/$max/g;
			s/minimum/$min/g;
			my $copy = $_;
			while ($copy =~ s/THETA\((\d+)\)//){
				unless ($unique_thetas{$1} == 1){
					$unique_thetas{$1} = 1;
					$theta_number++;
				}
			}
		}
	}
	# Initial values and boundaries
	for ( my $i = 0; $i < $theta_number - $start_theta; $i++ ) {
		unless ( defined $inits[$i] ){
			$inits[$i] = $self -> global_init; #0.001
		}
		unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[$i] ){
			$bounds{'upper'}[$i] = 1000000;
		}

		unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[$i] ){
			$bounds{'lower'}[$i] = -1000000;
		}
	}
	$end_theta = --$theta_number;

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub create_state1_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		start_theta => { isa => 'Int', optional => 1 },
		parameter => { isa => 'Str', optional => 1 },
		covariate => { isa => 'Str', optional => 1 },
		continuous => { isa => 'Bool', optional => 1 },
		statistics => { isa => 'HashRef', optional => 1 },
		sum_covariates => { isa => 'Bool', default => 0, optional => 1 },
		missing_data_token => { isa => 'Str', optional => 1 },
		code => { isa => 'ArrayRef[Str]', optional => 1 },
		inits => { isa => 'ArrayRef[Num]', optional => 1 },
		bounds => { isa => 'HashRef', optional => 1 }
	);
	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : ();
	my $sum_covariates = $parm{'sum_covariates'};
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();

	my $theta_number = $start_theta;
	my $median = $statistics{'median'};
	$median = sprintf "%6.2f", $median;
	$median =~ s/\s*//;
	my $min = $statistics{'min'};
	$min = sprintf "%6.2f", $min;
	$min =~ s/\s*//;
	my $max = $statistics{'max'};
	$max = sprintf "%6.2f", $max;
	$max =~ s/\s*//;

	if ( scalar @code < 1 or ( scalar @code == 1 and $code[0] eq '' )
			or ( scalar @code == 1 and $code[0] eq 'none' )) {
		if ($sum_covariates){
			$code[0] = "   $parameter$covariate = 0\n";
		}else {
			$code[0] = "   $parameter$covariate = 1\n";
		}
	} else {
		#state has been redefined
		#should never end up here
		ui->print(category => 'scm',
			message => "Redefined code in the state 1 section should never be encountered. Please report this error.",newline => 1);
		for ( @code ) {
			my $copy = $_;
			while ($copy =~ s/THETA\((\d+)\)//){
				$theta_number++;
			}
		}
	}
	# Initial values
	for ( my $i = 0; $i < $theta_number - $start_theta; $i++ ) {
		unless ( defined $inits[$i] ){
			$inits[$i] = $self -> global_init; #0.001
		}
		unless ( defined $bounds{'upper'} and defined $bounds{'upper'}[$i] ) {
			$bounds{'upper'}[$i] = 1000000;
		}
		unless ( defined $bounds{'lower'} and defined $bounds{'lower'}[$i] ) {
			$bounds{'lower'}[$i] = -1000000;
		}
	}
	$end_theta = --$theta_number;

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub preprocess_data
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model => { isa => 'Ref', optional => 1 },
		filter => { isa => 'Bool', optional => 1 },
		test_relations => { isa => 'Ref', optional => 1 },
		time_varying => { isa => 'Maybe[Ref]', optional => 1 },
		directory => { isa => 'Str', optional => 1 }
	);
	my $model = $parm{'model'};
	my $filter = $parm{'filter'};
	my $test_relations = $parm{'test_relations'};
	my $time_varying = $parm{'time_varying'};
	my $directory = $parm{'directory'};
	my $filtered_data_model;

	#in ref of model, directory, $time_varying $test_relations
	#out model

	$filtered_data_model = $model -> copy ( filename => 'filter_data.mod',
											directory => $directory, 
											output_same_directory => 1,
											copy_datafile          => 0,
											write_copy => 0,
											copy_output        => 0);

	die "no problems" unless defined $filtered_data_model->problems();
	die "more than one problem" unless (scalar(@{$filtered_data_model->problems()})==1);

	my $datafile = 'filtered.dta';
	my $timevarfile = '_time_varying.dta';
	my %parmcovhash;
	my %parmetahash;
	my $run_mess;
	my @filter_table_header;

	my $only_filter = 0;

	#must handle DROP here without header name.
	my $dummycounter=0;
	if( defined $filtered_data_model->problems()->[0] -> inputs and 
		defined $filtered_data_model->problems()->[0] -> inputs -> [0] -> options ) {
		foreach my $option ( @{$filtered_data_model->problems()->[0] -> inputs -> [0] -> options} ) {
			if ($option->name eq 'DROP' or $option->name eq 'SKIP'){
				if (defined $option->value and $option->value ne '' and not ($option->value eq 'DROP' or $option->value eq 'SKIP') ){
					push( @filter_table_header, $option -> value );
				}else{
					#simple drop used in $INPUT without name. Set dummy name as placeholder here.
					$dummycounter++;
					$option->value('DUMMY'.$dummycounter);
					push( @filter_table_header, $option -> value );
				}
			}else{
				push( @filter_table_header, $option -> name );
			}
		}
	} else {
		croak("Trying to construct table for filtering data".
			" but no headers were found in \$INPUT" );
	}
	foreach my $remove_rec ('simulation','covariance','table','scatter','input'){
		#will put back input without DROP below
		$filtered_data_model -> remove_records(type => $remove_rec);
	}


	if (defined $time_varying and scalar(@{$time_varying}>0)){

		#collect all parameters with time-varying cov on them
		my $pair_counter=0;
		my $parm_counter=0;
		my %test_relations = %{$test_relations};
		foreach my $par ( sort keys %test_relations ){
			my @covarr=();
			foreach my $cov ( @{$test_relations{$par}} ){
				foreach my $tvar (@{$time_varying}){
					if ($cov eq $tvar){
						push(@covarr,$cov);
						$pair_counter++;
						last;
					}
				}
			}
			if (scalar(@covarr)>0){
				$parmcovhash{$par} =\@covarr;
				$parm_counter++;
			}
		}

		my $new_comresno = $parm_counter+$pair_counter;

		$run_mess = "Running input model to determine median of time-varying covariates";	
		if (($new_comresno + 2*$parm_counter + scalar(@filter_table_header))>50){
			if ($PsN::nm_minor_version >= 2){
				my $max = $new_comresno + 2*$parm_counter + scalar(@filter_table_header);

				my $pdt_value = $filtered_data_model->get_option_value( option_name => 'PDT',
					record_name => 'sizes',
					fuzzy_match => 0);

				if (defined $pdt_value){
					$max=$pdt_value if ($pdt_value > $max);
				}

				if (defined $filtered_data_model ->problems->[0]->sizess() 
						and scalar(@{$filtered_data_model ->problems->[0]->sizess()})>0){
					$filtered_data_model -> set_option(record_name => 'sizes',
						record_number => 1,
						option_name => 'PDT',
						option_value => $max,
						fuzzy_match => 0);

				}else{
					$filtered_data_model -> add_records( type => 'sizes',
						record_strings => [ " PDT=".$max ] );
				}

			}else{
				my $num = $new_comresno + 2*$parm_counter + scalar(@filter_table_header);
				my $mess = "$num items needed in \$TABLE, too many for NONMEM. ".
				"Use NM version 7.2 or later, which can handle more items.".
				"If you are already using 7.2 or later, check that the version info in psn.conf is correct.";
				croak($mess);
			}	    

		}

		#update_inits from $model output
		if (defined $model->outputs() and 
			defined $model ->outputs()->[0] and
			$model ->outputs()->[0]-> have_output()){
			$filtered_data_model -> update_inits ( from_output => $model->outputs()->[0]);
		}elsif( $self->linearize and $self->lst_file ){
			$filtered_data_model -> update_inits (from_output_file => $self->lst_file());
		}


		my $comresno;
		if (defined $filtered_data_model ->problems->[0]->abbreviateds() 
				and scalar(@{$filtered_data_model ->problems->[0]->abbreviateds()})>0){
			# Get current comres number
			$comresno = $filtered_data_model->get_option_value( option_name => 'COMRES',
				record_name => 'abbreviated');

			$new_comresno += $comresno if ( defined $comresno );
			$filtered_data_model->set_option( option_name => 'COMRES',
				record_name => 'abbreviated',
				fuzzy_match => 1,
				option_value => $new_comresno);
		}else {
			# Add $ABBREVIATED if necessary
			$filtered_data_model -> add_records( type => 'abbreviated',
				record_strings => [ "COMRES=".($new_comresno) ] );
		}

		my @code;
		@code = @{$filtered_data_model -> pk( problem_number => 1 )};
		unless ( $#code > 0 ) {
			@code = @{$filtered_data_model -> pred( problem_number => 1 )};
		}
		if ( $#code <= 0 ) {
			croak("Neither PK or PRED defined in " .
				$filtered_data_model -> filename . ", cannot match parameters to ETAs\n" );
		}

		foreach my $parameter ( keys %parmcovhash ){
			my $etanum = 0;
			for ( @code ) {
				my $row = $_;
				if ( $row =~ /^\s*(\w+)\s*=\s*/ and $1 eq $parameter ){
					$row =~ s/^\s*(\w+)\s*=\s*//;
					my ($line,$comment) = split( ';', $row, 2 );
					chomp $line;
					if ($line =~ s/[^A-Z0-9_]ETA\(([0-9]+)\)//){
						$etanum = $1;
					}else{
						last;
					}

					if ($line =~ s/[^A-Z0-9_]ETA\(([0-9]+)\)//){
						croak("Could not determine the ETA ".
							"coupled to $parameter,\n".
							" two ETA(<number>) found ".
							"on $parameter = ... row\n" );
					}
				}
			}
			if ( $etanum ) {
				$parmetahash{$parameter}=$etanum;
			}else{
				my $mes = "Could not determine the ETA coupled to $parameter\n";
				$mes .= " i.e. no $parameter = (expression with ETA) was ".
				"found in \$PK or \$PRED\n" ;
				croak($mes );
			}
		}

		#can look for ADVAN<any number> this way
		my ($advan,$junk) = $filtered_data_model->problems->[0] -> _option_val_pos( record_name => 'subroutine',
			name => 'ADVAN',
			exact_match => 0);
		my $have_advan = scalar(@{$advan}) > 0;

		my $code_records;
		if( $have_advan ){
			# We have an ADVAN option in $SUBROUTINE, get $ERROR code
			$code_records = $filtered_data_model->problems->[0]-> errors();
		} else {
			# No ADVAN subroutine, we should modify $PRED code
			$code_records = $filtered_data_model->problems->[0] -> preds;
		}

		# Get code array reference, so we can update the code inplace.
		my $coderef = $code_records -> [0] -> verbatim_last;

		unless( defined $coderef ){
			$coderef = [];
			$code_records -> [0] -> verbatim_last($coderef);
		}



		my $com = defined $comresno ? $comresno + 1 : 1;
		foreach my $par ( keys %parmcovhash ){
			my @tablestrings =();
			push( @{$coderef},"\"  COM($com)=".'ABS(G('.$parmetahash{$par}.',1))/W' );
			push( @tablestrings, "COM($com)=$par".'RATIO');
			$com++;

			foreach my $cov (@{$parmcovhash{$par}}){
				push( @{$coderef},"\"  COM($com)=$cov".'*ABS(G('.$parmetahash{$par}.',1))/W' );
				push( @tablestrings, "COM($com)=$par$cov".'NUM');
				$com++;
			}
			$filtered_data_model -> 
			add_records( type           => 'table',
				record_strings => [ 'MDV ID '.join( ' ', @tablestrings ).
					' NOAPPEND NOPRINT ONEHEADER FILE='.$par.$timevarfile]);
		}

	}else{
		#only $filter is true
		$only_filter = 1;	

	}

	#may still have to fix PD for number of $INPUT items
	if (scalar(@filter_table_header)>50){
		if ($PsN::nm_minor_version >= 2){
			my $max = scalar(@filter_table_header);

			my $pd_value = $filtered_data_model->get_option_value( option_name => 'PD',
				record_name => 'sizes',
				fuzzy_match => 0);

			if (defined $pd_value){
				$max=$pd_value if ($pd_value > $max);
			}

			if (defined $filtered_data_model ->problems->[0]->sizess() 
					and scalar(@{$filtered_data_model ->problems->[0]->sizess()})>0){
				$filtered_data_model -> set_option(record_name => 'sizes',
					record_number => 1,
					option_name => 'PD',
					option_value => $max,
					fuzzy_match => 0);

			}else{
				$filtered_data_model -> add_records( type => 'sizes',
					record_strings => [ " PD=".$max ] );
			}

		}else{
			my $num = scalar(@filter_table_header);
			my $mess = "$num items needed in \$INPUT in filter model, too many for NONMEM. ".
			"Use NM version 7.2 or later, which can handle more items.".
			"If you are already using 7.2 or later, check that the version info in psn.conf is correct.\n".
			"Alternatively, create a new data set so that no IGNORE=(list) and hence no filtering is needed, ".
			" or a data set ".
			"with fewer columns than 50. Note that it is not enough to DROP ".
			"columns, they must be removed completely from the data set.";
			croak($mess);
		}	    

	}

	if ($only_filter){
		foreach my $remove_rec ('abbreviated','msfi','contr','subroutine','prior','model','tol','infn','omega','pk','aesinitial','aes','des','error','pred','mix','theta','sigma','estimation','nonparametric'){
			$filtered_data_model -> remove_records(type => $remove_rec);
		}

		$filtered_data_model -> add_records(type => 'pred',
			record_strings => ['Y=THETA(1)+ETA(1)+EPS(1)']);

		$filtered_data_model -> add_records(type => 'theta',
			record_strings => ['1']);
		$filtered_data_model -> add_records(type => 'omega',
			record_strings => ['1']);
		$filtered_data_model -> add_records(type => 'sigma',
			record_strings => ['1']);
		$filtered_data_model -> add_records(type => 'estimation',
			record_strings => ['MAXEVALS=0 METHOD=ZERO']);
	}
	# set $TABLE record

	if ($filter){
		if (length($run_mess)>0 ){
			$run_mess .= " and to filter data";
		}else{
			$run_mess = "Running dummy model to filter data";
		}

		$filtered_data_model -> add_records( type           => 'table',
			record_strings => [ join( ' ', @filter_table_header ).
				' NOAPPEND NOPRINT ONEHEADER FILE='.$datafile]);
	}
	#put back input without DROP
	$filtered_data_model -> add_records(
		type => 'input',
		record_strings => [join(' ',@filter_table_header)]);

	$filtered_data_model->_write();
	# run model in data_filtering_dir clean=3
	my $filter_fit = tool::modelfit -> new
	( %{common_options::restore_options(@common_options::tool_options)},
		base_directory => $directory,
		directory      => $directory.'/data_preprocessing_dir/',
		models         => [$filtered_data_model],
		top_tool       => 0,
		clean => 3  );
	ui -> print( category => 'all',
		message  => $run_mess,newline => 1 );

	$filter_fit -> run;


	if (defined $time_varying and scalar(@{$time_varying}>0)){
		foreach my $par ( keys %parmcovhash ){
			my $ncov = scalar(@{$parmcovhash{$par}});
			open( FILE, "$directory$par$timevarfile" ) ||
			croak("Could not open $directory$par$timevarfile"." for reading" );
			my @lines = <FILE>;
			close( FILE );
			my @sum_arr=(0) x $ncov;
			my @ave_arr;

			for (my $i=0; $i<$ncov;$i++){
				$ave_arr[$i]=[];
			}
			my $prev_id;
			my $ratsum=0;
			foreach (@lines){
				chomp;
				next unless (/^\s*0/); #only use lines with mdv=0
				my @vals=split;
				#items in @vals are
				# MDV ID $parRATIO (array over) $par$covNUM

				unless ((not defined $prev_id) or ($vals[1] == $prev_id)){
					#found new id, process it

					for (my $i=0; $i<$ncov;$i++){
						#do not die: if $ratsum is zero it must be that the whole expression is 0
						if ($ratsum == 0){
							push(@{$ave_arr[$i]},0);
						}else{
							push(@{$ave_arr[$i]},$sum_arr[$i]/$ratsum);
						}
					}
					#reset
					@sum_arr=(0) x $ncov;
					$ratsum=0;
				}
				#store new
				$ratsum += $vals[2];
				for (my $i=0; $i<$ncov;$i++){
					$sum_arr[$i] += $vals[$i+3];
				} 
				$prev_id = $vals[1];
			}
			#process the last one
			#new id, process
			for (my $i=0; $i<$ncov;$i++){
				#do not die: if $ratsum is zero it must be that the whole expression is 0
				if ($ratsum == 0){
					push(@{$ave_arr[$i]},0);
				}else{
					push(@{$ave_arr[$i]},$sum_arr[$i]/$ratsum);
				}
			} 

			open( LOG, ">>".$self -> logfile -> [0] ); #model_number -1
			for (my $i=0; $i<$ncov;$i++){
				my $cov = $parmcovhash{$par}->[$i];
				my @sorted_array =  (sort {$a <=> $b} @{$ave_arr[$i]}); #sort ascending
				my $len= scalar( @sorted_array );
				my $median;
				if( $len  % 2 ){
					$median = $sorted_array[($len-1)/2];
				} else {
					$median = ($sorted_array[$len/2]+$sorted_array[($len-2)/2])/ 2;
				}
				$self->medians->{$par.'_'.$cov}=$median;

				my $sum=0;
				foreach my $val (@sorted_array){
					$sum += $val;
				}
				$self->means->{$par.'_'.$cov}=$sum/$len if ($len>0);
				$median = sprintf("%6.2f", $median );
				my $mean = sprintf("%6.2f", $sum/$len ) if ($len>0);
				print LOG "Time-varying $cov on $par (ETA".$parmetahash{$par}.
				") has median $median and mean $mean\n";
			}
			close LOG;
		}
	}

	if ($filter){
		#create data object from table

		my @idcolumns = @{$model -> idcolumns};
		my $idcolumn = $idcolumns[0];

		#have checked that ignoresign and idcol is ok
		if ( defined $idcolumn ) {
			$filtered_data_model->datafiles(new_names => [$filtered_data_model -> directory.$datafile],
											problem_numbers =>[1]);
		} else {
			croak("No id column definition found in the model file." );
		}
	}

	return $filtered_data_model;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
