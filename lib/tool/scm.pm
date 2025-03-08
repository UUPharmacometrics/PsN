package tool::scm;

use include_modules;
use strict;
use Cwd;
use tool::modelfit;
use OSspecific;
use Data::Dumper;
use File::Copy 'copy';
use File::Spec;
use List::Util qw(max);
use status_bar;
use Mouse;
use MouseX::Params::Validate;
use math;
use utils::file;
use array qw(get_positions any_nonzero);
use nmtablefile;
use code_parsing;
use PsN;
use model_transformations;
use filter_data;


extends 'tool';

use tool::scm::config_file;

has 'config_file' => ( is => 'rw', isa => 'tool::scm::config_file' );
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
has 'from_linearize' => ( is => 'rw', isa => 'Bool', default => 0 );    # Was the scm-object created by linearize?
has 'original_nonlinear_model' => ( is => 'rw', isa => 'model' );       # If linearizing this will be the real original model
has 'keep_covariance' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'extra_table_columns' => ( is => 'rw', isa => 'ArrayRef[Str]' ); # Set to array of colnames to add to an extra data table output by derivatives.mod
has 'nointer' => ( is => 'rw', isa => 'Bool', default => 0 );   # Set to not use interaction columns in linearization (set D_EPSETA to 0)
has 'use_data_format' => ( is => 'rw', isa => 'Bool', default => 0 );   # Should we use the workaround for big datasets
has 'from_bootscm' => ( is => 'rw', isa => 'Bool', default => 0 );  # Are we called from a bootscm. This is a hack to fix a specific bug with non-binary catcovs under linearization
has 'categorical_mean_offset' => ( is => 'rw', isa => 'Bool', default => 0 );   # Use mean instead of mode as offset
has 'extra_data_columns' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );  # Columns to add to linbase.dta and $INPUT for linbase.mod
has 'force_binarize' => ( is => 'rw', isa => 'Bool', default => 0 );   # Force binarization of categorical covariates
has 'estimation_options' => ( is => 'rw', isa => 'Str' );
has 'auto_tv' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'one_model_per_covariate' => ( is => 'rw', isa => 'Bool', default => 0);


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
        unless ($self->linearize or defined $self->derivatives_data or $self->skip_filtering){
            $do_filtering = $self -> models->[0]->need_data_filtering();
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

    my $unique = array::unique($self->valid_states->{'continuous'});
    if (scalar(@$unique) != scalar(@{$self->valid_states->{'continuous'}})) {
        croak("Duplicate states in the continuous valid_states");
    }
    $unique = array::unique($self->valid_states->{'categorical'});
    if (scalar(@$unique) != scalar(@{$self->valid_states->{'categorical'}})) {
        croak("Duplicate states in the categorical valid_states");
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

    if (defined $self->error and ($self->error eq 'user')) {
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

    if (not defined $self->p_value() or $self->p_value() eq '') {
        $self->p_value(0.05);
    }
    croak("Option p_value (p_forward/p_backward) must be either ".
        "0.05 (default), 0.01, 0.005 or 0.001")
    unless ($self->p_value() == 0.05 or $self->p_value() == 0.01 or
        $self->p_value() == 0.005 or $self->p_value() == 0.001
            or $self->have_Math_CDF());

    croak("Option p_value (p_forward/p_backward) must be in the range 0-1")
    unless ($self->p_value >= 0 and $self->p_value <=1);

    # Auto add TVxx
    if ($self->auto_tv) {
        my @scm_parameters = keys %{$self->test_relations};
        model_transformations::add_tv(model => $self->models->[0], parameters => \@scm_parameters, type => 'multiplicative');
    }

    # Add derived covariates to dataset (to first model)
    my @covlist;
    if (defined $self->categorical_covariates) {
        @covlist = @{$self->categorical_covariates};
    }
    if (defined $self->continuous_covariates) {
        push @covlist, @{$self->continuous_covariates};
    }
    if (not $self->from_linearize) {
        $self->models->[0] = filter_data::add_derived_columns(
            model => $self->models->[0],
            directory => $self->directory,
            columns => \@covlist,
        );
    }

    # Pre-process non-bivariate categoricals for linearized scm
    if ((not $self->from_linearize and not $self->from_bootscm and ($self->linearize or $self->force_binarize) and $self->step_number == 1 and defined $self->categorical_covariates)) {
        my $model = $self->models->[0];
        my $columns = $model->problems->[0]->columns_list();
        my $positions = array::get_positions(target => $columns, keys => $self->categorical_covariates);
        my $data = data->new(
            filename => $model->problems->[0]->datas->[0]->get_absolute_filename(),
            ignoresign => $model->problems->[0]->datas->[0]->ignoresign,
            idcolumn => $model->idcolumns->[0],
            missing_data_token => $self->missing_data_token
        );
        my $original_dataset_numcols = $data->column_count();
        my $initial_number_of_columns = scalar(@$columns);
        for (my $i = 0; $i < $original_dataset_numcols - $initial_number_of_columns; $i++) {     # Pad column vector if dataset is actually bigger
            push @$columns, 'DROP';
        }
        my ($mapping, $new_indices, $new_categorical, $warn_multiple) =
            $data->append_binary_columns(start_header => $columns, indices => $positions, baseline_only => 0);

        # For some reason append_binary_column retains covariates that was already binary. Remove them here.
        my @only_new;
        for my $cov (@{$self->categorical_covariates}) {
            for my $newcov (@$new_categorical) {
                if ($newcov =~ /^${cov}_\d+$/) {
                    push @only_new, $newcov;
                }
            }
        }
        my $added = 0;
        $added = 1 if (scalar(@only_new) > 0);
        $new_categorical = \@only_new;

        if ($added) {
            my $dataset_name = 'data_with_updated_categoricals';
            $data->filename($dataset_name);
            $data->directory($self->directory);
            $data->header([]);
            $data->_write();
            $model->problems->[0]->datas->[0]->set_filename(filename => $dataset_name, directory => $self->directory);
            my $inputs = $model->problems->[0]->inputs;
            my $no_inputs = scalar(@$inputs);
            # Add DROP for extra columns already in dataset but not in $INPUT
            my $input_numcols = 0;
            for my $inp (@$inputs) {
                $input_numcols += scalar(@{$inp->options});
            }
            for (my $i = 0; $i < $original_dataset_numcols - $input_numcols; $i++) {
                $inputs->[$no_inputs - 1]->_add_option(option_string => 'DROP');
            }
            # Add new columns to $INPUT
            for my $colname (@$new_categorical) {
                $inputs->[$no_inputs - 1]->_add_option(option_string => $colname);
            }
            my %colhash;    # Which original catcovs have been binarized?
            for my $newcol (@$new_categorical) {
                my $replace = $newcol;
                $replace =~ s/_\d+$//;
                $colhash{$replace} = 1;
            }
            my @newcat;
            for my $categorical (@{$self->categorical_covariates}) {
                if (not $colhash{$categorical}) {
                    push @newcat, $categorical;
                }
            }
            push @newcat, @$new_categorical;
            $self->categorical_covariates(\@newcat);
            # Update the test_relations
            for my $param (keys %{$self->test_relations}) {
                my @newrel;
                for my $cov (@{$self->test_relations->{$param}}) {
                    if (not $colhash{$cov}) {
                        push @newrel, $cov;
                    } else {
                        # Add only the new covariates for this particular old covariate
                        for my $newcov (@$new_categorical) {
                            if ($newcov =~ /^${cov}_\d+$/) {
                                push @newrel, $newcov;
                            }
                        }
                    }
                }
                $self->test_relations->{$param} = \@newrel;
            }
        }
    }

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

    if (defined $self->continuous_covariates() or defined $self->categorical_covariates()) {

        my @continuous = defined $self->continuous_covariates() ? @{$self->continuous_covariates()} : ();
        my @categorical = defined $self->categorical_covariates() ? @{$self->categorical_covariates()} : ();

        my $undropped_columns = $self->models->[0]->problems->[0]->undrop_columns(columns => [@continuous, @categorical]);
        if (scalar(@$undropped_columns)) {
            print "The following data columns were undropped from the \$INPUT because they were requested as covariates:\n";
            print "   ", join(', ', @$undropped_columns), "\n";
        }

        my @not_found = ();
        my @nonskipped = ();
        foreach my $input (@{$self->models->[0]->problems->[0]->inputs}) {
            next if (not defined $input);
            push(@nonskipped, @{$input->get_nonskipped_columns});
        }
        foreach my $cov (@continuous, @categorical) {
            #check if reserved words
            if (($cov eq 'PAR') or ($cov eq 'COV')) {
                croak("PAR and COV are reserved words in scm and must not be ".
                      "used as name for a covariate.");
            }
        }
        my @covs = (@continuous, @categorical);
        my $positions = get_positions(target => \@nonskipped, keys => \@covs);
        for (my $i = 0; $i < scalar(@covs); $i++) {
            if (not defined $positions->[$i]) {
                push(@not_found, $covs[$i]);
            }
        }
        if (scalar @not_found) {
            croak("Covariate(s) [ " . join(',', @not_found) . " ] was not defined in " . $self->models->[0]->filename);
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
                    if( $cov eq $specified_cov ){
                        $covariate_test = 1;
                        last ;
                    }
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
        my $tmp = "";
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
                parse_header                 => 1    ); #ok parse_header, do not know idcol

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
            if (defined $self->global_covariate_statistics and scalar(keys %{$self->global_covariate_statistics}) > 0) {
                #this is necessary for xv_scm
                if (defined $self->continuous_covariates) {
                    foreach my $cov (@{$self -> continuous_covariates()}){
                        $self->covariate_statistics->{$cov}{'have_missing_data'} = $self->global_covariate_statistics->{$cov}{'have_missing_data'};
                        $self->covariate_statistics->{$cov}{'min'} = $self->global_covariate_statistics->{$cov}{'min'};
                        $self->covariate_statistics->{$cov}{'max'} = $self->global_covariate_statistics->{$cov}{'max'};
                    }
                }
                if ( defined $self -> categorical_covariates()) {
                    foreach my $cov (@{$self -> categorical_covariates()}){
                        #this is necessary for xv_scm
                        $self->covariate_statistics->{$cov}{'have_missing_data'} = $self->global_covariate_statistics->{$cov}{'have_missing_data'};
                        $self->covariate_statistics->{$cov}{'min'} = $self->global_covariate_statistics->{$cov}{'min'};
                        $self->covariate_statistics->{$cov}{'max'} = $self->global_covariate_statistics->{$cov}{'max'};
                    }
                }
            }
        }
        open(STAT, '>' . $self->covariate_statistics_file);
        $Data::Dumper::Purity = 1;
        print STAT Dumper $self->covariate_statistics;
        $Data::Dumper::Purity = 0;
        close(STAT);
    }

    for my $cov (keys %{$self->covariate_statistics}) {
        if (scalar(keys %{$self->covariate_statistics->{$cov}{'factors'}} < 2)) {
            print "Warning: The covariate $cov has only one value for all individuals\n";
        }
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
    my $timevar_minmax_warning = 0;
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
                    unless (defined $self -> covariate_statistics->{$cov}){
                        ui->print(category=> 'all',
                                  message => "\nError covariate statistics for :$cov:, not stored.\n".
                                  "Have stored :".join(':',keys %{$self -> covariate_statistics}).":\n");
                    }

                    my %local_statistics = %{$self -> covariate_statistics->{$cov}};
                    if (defined $self->medians->{$par . '_' . $cov}) {
                        my $tv_median = $self->medians->{$par . '_' . $cov};
                        if ($tv_median > $local_statistics{'min'} and $tv_median < $local_statistics{'max'}) {
                            $local_statistics{'median'} = $tv_median;
                        } else {
                            if (not $timevar_minmax_warning) {
                                print "Warning: Computed median $tv_median of time-varying $cov on $par is smaller" .
                                    " than min $cov $local_statistics{'min'} or larger than max $cov $local_statistics{'max'}." .
                                    " Falling back to using the median of the individual median. This can happen when the model has IOV.".
                                    " More warnings of this type will be suppressed.\n";
                                $timevar_minmax_warning = 1;
                            }
                        }
                    }
                    if (defined $self->means->{$par . '_' . $cov}) {
                        my $tv_mean = $self->means->{$par . '_' . $cov};
                        if ($tv_mean > $local_statistics{'min'} and $tv_mean < $local_statistics{'max'}) {
                            $local_statistics{'mean'} = $tv_mean;
                        } else {
                            if (not $timevar_minmax_warning) {
                                print "Warning: Computed mean $tv_mean of time-varying $cov on $par is smaller" .
                                    " than min $cov $local_statistics{'min'} or larger than max $cov $local_statistics{'max'}." .
                                    " Falling back to using the mean of the individual mean. This can happen when the model has IOV.".
                                    " More warnings of this type will be suppressed.\n";
                                $timevar_minmax_warning = 1;
                            }
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
                }
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
            }
        }

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

    }
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
        #if model 1 does not have ofv (crashed?) select another row
        my $rawline_index = '1';
        if (not defined $modelfit->raw_line_structure->{'1'}->{'ofv'}) {
            for my $n (keys %{$modelfit->raw_line_structure}) {
                if (defined $modelfit->raw_line_structure->{$n}->{'ofv'}) {
                    $rawline_index = $n;
                    last;
                }
            }
        }

        my @diagnostic_params = @{$self -> diagnostic_parameters};
        my @diagnostic_indices;
        unshift(@diagnostic_params,('model','problem','subproblem'));
        push(@diagnostic_params,'ofv');
        foreach my $param (@diagnostic_params){
            no warnings qw(uninitialized);
            my ($start,$len) = split(',',$modelfit->raw_line_structure->{$rawline_index}->{$param});
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

        ($theta_start,$len) = split(',',$modelfit->raw_line_structure->{$rawline_index}->{'theta'})
        if (defined $modelfit->raw_line_structure->{$rawline_index}->{'theta'});
        ($omega_start,$len) = split(',',$modelfit->raw_line_structure->{$rawline_index}->{'omega'})
        if (defined $modelfit->raw_line_structure->{$rawline_index}->{'omega'});
        ($sigma_start,$len) = split(',',$modelfit->raw_line_structure->{$rawline_index}->{'sigma'})
        if (defined $modelfit->raw_line_structure->{$rawline_index}->{'sigma'});
        ($setheta_start,$len_setheta) =
        split(',',$modelfit->raw_line_structure->{$rawline_index}->{'setheta'})
        if (defined $modelfit->raw_line_structure->{$rawline_index}->{'setheta'});
        ($seomega_start,$len_seomega) =
        split(',',$modelfit->raw_line_structure->{$rawline_index}->{'seomega'})
        if (defined $modelfit->raw_line_structure->{$rawline_index}->{'seomega'});
        ($sesigma_start,$len_sesigma) =
        split(',',$modelfit->raw_line_structure->{$rawline_index}->{'sesigma'})
        if (defined $modelfit->raw_line_structure->{$rawline_index}->{'sesigma'});
        ($shrinkage_eta_start,$len_shrinkage_eta) =
        split(',',$modelfit->raw_line_structure->{$rawline_index}->{'shrinkage_eta'})
        if (defined $modelfit->raw_line_structure->{$rawline_index}->{'shrinkage_eta'});
        ($shrinkage_iwres_start,$len_shrinkage_iwres) =
        split(',',$modelfit->raw_line_structure->{$rawline_index}->{'shrinkage_iwres'})
        if (defined $modelfit->raw_line_structure->{$rawline_index}->{'shrinkage_iwres'});

        for ( my $i = 0; $i < scalar @{$modelfit -> raw_results}; $i++ ) {
            my @new_raw_results = ( $step_number,$action,$step_rel_names[$i] );

            my ( @diagnostics, @thetas, @omsi, @sethetas, @seomsi, @shrinkage_eta,$shrinkage_iwres );

            # Get diagnostic results:
            #every column up to first theta.

            for ( my $j = 0; $j < scalar(@diagnostic_indices); $j++ ) {
                push( @diagnostics, $modelfit -> raw_results -> [$i][$diagnostic_indices[$j]] );
            }

            # Get the thetas that were present in the original model
            for ( my $j = $theta_start;
                $j < ($theta_start+$npar_orig{'theta'}); $j++ ) {
                push( @thetas, $modelfit -> raw_results -> [$i][$j] );
            }

            # Get the results for all par-cov-relation
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

            # Get all the omegas and sigmas

            for ( my $j = $omega_start; $j < ( $omega_start +$npar_orig{'omega'}); $j++ ) {
                push( @omsi, $modelfit -> raw_results() -> [$i][$j] );
            }
            for ( my $j = $sigma_start; $j < ( $sigma_start +$npar_orig{'sigma'}); $j++ ) {
                push( @omsi, $modelfit -> raw_results() -> [$i][$j] );
            }

            # Get all the standard errors of the omegas and sigmas
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
                            (index('SAEM',$string_arr[$#string_arr])==0));
                        $bayes = 1 if ($string_arr[$#string_arr] eq 'BAYES' or
                            (index('BAYES',$string_arr[$#string_arr])==0));
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
                }

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
            my ($start,$sethstart,$len,$thstart);
            foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure})){
                no warnings qw(numeric uninitialized);
                ($thstart,$len) = split(',',$self->raw_line_structure -> {$mod}->{'theta'});
                my $extra1 = scalar(@{$param_names{'theta'}})+scalar(@rel_header)-$len;
                $self->raw_line_structure -> {$mod}->{'theta'} = ($thstart).','.($len+$extra1);
                foreach my $st (sort({$a <=> $b} keys %start_category_hash)){
                    next unless ($st > $thstart);
                    my $category = $start_category_hash{$st};
                    ($start,$len) = split(',',$self->raw_line_structure -> {$mod}->{$category});
                    $self->raw_line_structure -> {$mod}->{$category} = ($start+$extra1).','.$len;
                }
                ($sethstart,$len) = split(',',$self->raw_line_structure -> {$mod}->{'setheta'});
                my $extra2 = scalar(@{$param_names{'theta'}})+scalar(@rel_header)-$len;
                $self->raw_line_structure -> {$mod}->{'setheta'} = ($sethstart).','.($len+$extra2);
                foreach my $st (sort({$a <=> $b} keys %start_category_hash)){
                    next unless (($st+$extra1) > $sethstart);
                    my $category = $start_category_hash{$st};
                    ($start,$len) = split(',',$self->raw_line_structure -> {$mod}->{$category});
                    $self->raw_line_structure -> {$mod}->{$category} = ($start+$extra2).','.$len;
                }
            }
            $self->raw_line_structure -> write( $dir.'raw_results_structure' );

        }
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
                filename => $self->final_model_directory.'/original.mod',
                copy_datafile => 0,
                write_copy => 1,
                copy_output => 0,
            );
            $self->original_nonlinear_model($tmp_orig);
        }

    }


    #setup linearize here.
    if ($self->linearize()) {
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
    %included_relations = %{$self->included_relations} if (defined $self->included_relations);
    my $need_base_ofv = 1;
    $need_base_ofv = 0 if (defined $self->base_criteria_values and
        defined $self->base_criteria_values->{'ofv'});

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
        if ((defined $self->max_steps and $self->max_steps == 0) and ($self->step_number == 1) and scalar(keys %{$self->test_relations}) == 0 and $self->linearize) {
            $fname = $self->basename . '.mod';
        }

        my $copy_datafile = 0;
        $copy_datafile = 1 if ((not $self->linearize ) and (not defined $self->xv_pred_data));

        my $start_model = $model->copy(
            filename => $fname,
            write_copy => 0,
            copy_datafile => $copy_datafile,
            copy_output => 0
        );

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

        my $orig_fit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            base_directory => $self->directory,
            directory => $self->directory . '/base_modelfit_dir' . $model_number . '/',
            models => [$start_model],
            top_tool => 0,
            parent_tool_id => $self->tool_id,
            copy_data => (not $self->linearize)
        );

        my $mess = "Estimating base model";
        $mess .= " with included_relations to get base ofv" if ($self->have_run_included);
        if ($self->linearize) {
            $mess = "Estimating linearized base model";
            if ($self->step_number > 1) {
                $mess .= " with updated derivatives and predictions";
            }
        }
        ui->print(category => 'scm', message  => $mess) unless ($self->parent_threads > 1);
        $orig_fit->run;

        if (defined $start_model->outputs and defined $start_model->outputs->[0] and
            $start_model->outputs()->[0]-> have_output() and
            defined $start_model-> outputs -> [0] -> get_single_value(attribute=> 'ofv')) {
            my $start_ofv = $start_model -> outputs -> [0] -> get_single_value(attribute=> 'ofv');
            my $start_name = $start_model->filename;
            #change base criteria values unless it is defined already (to override start value)
            if (($self->linearize() and $self->step_number() > 1)
                    or not ( defined $self -> base_criteria_values and
                    defined $self -> base_criteria_values -> {'ofv'})) {
                $self -> base_criteria_values -> {'ofv'} = $start_ofv;
            }
            #override if update_derivatives, set even if old value defined
            #      we always reestimate included, so should not need to set derivatives ofv as linearized base
            if ($self->linearize()){
                my $ofv = sprintf("%12.5f",$start_ofv);
                open( LOG, ">>".$self -> logfile -> [$model_number-1] );
                if ($self->update_derivatives() and $self->step_number()>1){
                    print LOG "The ofv of the updated linearized base model:$ofv        $start_name\n";
                } else {
                    if ($self->from_linearize) {
                        my $initial_ofv;
                        my $ofv_path = $start_model->outputs->[0]->get_single_value(attribute => 'ofvpath');
                        if (defined $ofv_path) {
                            $initial_ofv = $ofv_path->[0];
                        }
                        if (defined $initial_ofv) {
                            my $initial_ofv = sprintf("%12.5f", $initial_ofv);
                            ui->print(category => 'linearize',
                                message => "\nThe ofv of the linearized base model before estimation:$initial_ofv\n");
                        }
                        my $datafile = $start_model->datafiles(problem_numbers => [ 1 ], absolute_path => 1)->[0];
                        my $inter_in_model = _inter_in_model(datafile => $datafile, model => $self->original_nonlinear_model);
                        my $inter_in_est = _inter_in_est(model => $self->original_nonlinear_model);
                        print('Interaction in model (have non-zero D_EPSETA): ');
                        if ($inter_in_model) {
                            print("YES\n");
                        } else {
                            print("NO\n");
                        }
                        print('Interaction in $EST: ');
                        if ($inter_in_est) {
                            print("YES\n");
                        } else {
                            print("NO\n");
                        }
                    }
                    print LOG "The ofv of the linearized base model:$ofv        $start_name\n";
                    ui -> print(category => 'linearize',
                        message =>"\nThe ofv of the linearized base model:$ofv        $start_name\n");
                }
                print LOG "--------------------\n\n";
                close LOG;
            }

        }else{
            ui->print(category => 'scm', message => "Warning: could not retrieve OFV from base model. This probably means that the base model run did not terminate ok and that other problems will arise later in this scm run\n");
        }
        if (defined $self->xv_pred_data) {
            $self->run_xv_pred_step(estimation_model => $start_model,
                model_name => 'base');
        }
        $self -> initial_estimates_model($start_model);
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
               logfile        => [$self->directory."/modelfit".$model_number.".log"],
               base_directory => $self->directory,
               directory      => $self->directory.'/modelfit_dir'.$model_number,
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
            $self->base_criteria_values->{'ofv'} = 0;      #to avoid crash later
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

    # Check if there are omegas or sigmas defined but not in code.
    my $nsigmas = $original_model->problems->[0]->nsigmas();
    if ($nsigmas > 0 and not (code_parsing::used_symbol(model => $original_model, symbol => "EPS($nsigmas)") or
        code_parsing::used_symbol(model => $original_model, symbol => "ERR($nsigmas)"))) {
        croak("SIGMA($nsigmas) is defined but ERR($nsigmas) or EPS($nsigmas) is not used in the model code. Linearize cannot handle this for the SIGMA with the highest index.");
    }

    my $l2_index;
    my $linearize_only = 0;
    if ((defined $self->max_steps() and $self->max_steps() == 0) and ($self->step_number()==1) and
        scalar(keys %{$self->test_relations()}) == 0) {
        $linearize_only = 1;
        my $base = $original_model->filename();
        $base =~ s/\.(mod|ctl)$//;
        $base .= '_linbase';
        $self->basename($base);
    }
    #add_code_linearize needs input parameter or eta, plus translation
    #or simply use add_code for derivatives model?
    # make check that only one problem for linearize
    my $derivatives_model;
    my %included_relations;
    %included_relations = %{$self->included_relations} if
    (defined $self->included_relations);
    my $datafilename;
    my $part = '-part';
    my $rerun_derivatives_new_direction = 1;
    my $stepname = '';
    if ($self->step_number() > 1) {
        $stepname = '_' . ($self->step_number() - 1);
        if ($self->search_direction() eq 'forward') {
            $stepname .= 'f';
        }else{
            $stepname .= 'b';
        }
    }
    if ($self->step_number() == 1) {
        #if first step then prepare parameter_eta hash
        #assume parameters given and not etas. search code to find which eta goes with each param
        #1.7
        my $nETA=  $original_model->nomegas(with_correlations => 0, with_same => 1)->[0];
        my $nEPS=  $original_model->nsigmas(with_correlations => 0, with_same => 1)->[0];

        my %parameter_eta;
        my %parameter_relation;
        my @code;
        @code = @{$original_model->get_code(record => 'pk')};
        unless ($#code > 0) {
            @code = @{$original_model->get_code(record => 'pred')};
        }
        if ($#code <= 0) {
            croak("Neither PK or PRED defined in " . $original_model->filename . ", cannot match parameters to ETAs\n");
        }
        my @error_code = @{$original_model->get_code(record => 'error')};
        push @code, @error_code;

        my $iov_etas = model_transformations::find_etas(model => $original_model, type => 'iov');
        my $n_param = 0;
        open(LOG, ">>" . $self->logfile->[0]); #model_number -1
        foreach my $parameter (keys %{$self->test_relations()}) {
            $n_param++;
            my $etanum = 0;
            my $relation = '';
            my @passed_code;        # Used for expression parsing
            for (@code) {
                my $current_line = $_;
                if (/^\s*(\w+)\s*=\s*/ and $1 eq $parameter) {
                    s/^\s*(\w+)\s*=\s*//;
                    my ($code_line, $comment) = split(';', $_, 2);
                    if ($code_line =~ /[^A-Z0-9_]*EXP\s*\(\s*MU\_([0-9]+)\s*\+\s*ETA\(([0-9]+)\)/) {    # Check this here before we loose MU_ information
                        $relation = 'exponential';
                    }
                    my $assignments = code_parsing::find_assignments(\@passed_code);
                    my $line = code_parsing::merge_assignments_and_expression(expression => $code_line, assignments => $assignments);
                    $_ = $line;
                    chomp;

                    if (/\*\s*EXP\s*\(\s*-?ETA\(([0-9]+)\)/) {  # -? To allow for the rare case of a minus sign
                        $relation = 'exponential';
                    } elsif (/[^A-Z0-9_]*EXP\s*\(\s*ETA\(([0-9]+)\)/) {
                        $relation = 'exponential';
                    } elsif (/[^A-Z0-9_]*EXP\s*\(\s*MU\_([0-9]+)\s*\+\s*ETA\(([0-9]+)\)/) {
                        $relation = 'exponential';
                    } elsif ($code_line =~ /[^A-Z0-9_]*TV(\w+)\s*\+\s*ETA\(([0-9]+)\)/) {
                        if ($self->sum_covariates_hash->{$parameter} == 1) {
                            $relation = 'logit';
                        } else {
                            $relation = 'additive';
                        }
                    } elsif (code_parsing::check_additive_eta(expression => $line)) {
                        $relation = 'additive';
                    } elsif (/[^A-Z0-9_]*TV(\w+)\s*\*\s*ETA\(([0-9]+)\)/) {
                        $relation = 'proportional';
                    } elsif (/[^A-Z0-9_]*ETA\(([0-9]+)\)\s*\*\s*TV(\w+)/) {
                        $relation = 'proportional';
                    } elsif (/\*\s*\(\s*1\s*\+\s*ETA\(([0-9]+)\)/) {
                        $relation = 'proportional';
                    } elsif (/\*\(\s*ETA\(([0-9]+)\)\s*\+\s*1/) {
                        $relation = 'proportional';
                    }

                    if (s/[^A-Z0-9_]ETA\(([0-9]+)\)//) {
                        $etanum = $1;
                    } else {
                        last;
                    }

                    # Check if more IIV etas are connected to the same parameter. Note that IIV must come before IOV in the parameter definition
                    while (/[^A-Z0-9_]ETA\((\d+)\)/g) {
                        my $found = 0;
                        for my $eta (@$iov_etas) {
                            if ($1 == $eta) {
                                $found = 1;
                                last;
                            }
                        }
                        if (not $found) {   # We have found an eta that is not iov. Select the first ETA and warn
                            /[^A-Z0-9_]ETA\((\d+)\)/;
                            print "Warning: More than one ETA associatied with $parameter. Selecting ETA($1)\n";
                            $etanum = $1;
                        }
                    }
                }
                push @passed_code, $current_line;
            }
            if ($etanum) {
                $parameter_eta{$parameter} = $etanum;
            } else {
                my $mes = "Could not determine the ETA coupled to $parameter\n";
                $mes .= " i.e. no $parameter = (expression with ETA) was " .
                "found in \$PK, \$PRED or \$ERROR\n";
                croak($mes);
            }
            if (length($relation) > 1) {
                $parameter_relation{$parameter} = $relation;
                if ($relation eq 'logit') {
                    print "Detected ETA".$parameter_eta{$parameter}." added to the logit $parameter\n";
                    print LOG "Detected ETA".$parameter_eta{$parameter} .
                    " added to the logit $parameter\n";
                } else {
                    print "Detected $relation ETA".$parameter_eta{$parameter}." on $parameter\n";
                    print LOG "Detected $relation ETA".$parameter_eta{$parameter}." on $parameter\n";
                }
            } else {
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
        my $singles = 4; # ID DV IPRED MDV
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

        if($self->data_items() > $self->max_data_items){
            if ($PsN::nm_minor_version >= 2){
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
                $self->sizes_pd($max + 20);     # Added margin

                if (defined $original_model ->problems->[0]->sizess()
                        and scalar(@{$original_model ->problems->[0]->sizess()})>0){
                    $original_model -> set_option(record_name => 'sizes',
                        record_number => 1,
                        option_name => 'PD',
                        option_value => $self->sizes_pd(),
                        fuzzy_match => 0);

                }else{
                    $original_model -> add_records( type => 'sizes',
                        record_strings => [ " PD=" . $self->sizes_pd ] );
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
                                                       copy_datafile => 0,
                                                       write_copy => 0,
                                                       copy_output => 0);

        model_transformations::add_missing_etas(model => $derivatives_model);

        if (PsN::minimum_nonmem_version(7, 3)) {
            my $mceta = $original_model->get_option_value(record_name => 'estimation',
                option_name => 'MCETA',
                fuzzy_match => 1
            );
            $DB::single = 1;
            my $etas_records = $original_model->record(record_name => 'etas');
            my $etas_file = $original_model->get_or_set_etas_file();
            if ($original_model->is_run()) {
                $derivatives_model->update_inits(from_output => $original_model->outputs->[0]);
            }
            my $phi_file = $original_model->get_phi_file();
            if (defined $phi_file) {
                # use phi file from original execution as input
                print "Original model has phi output file; will use to initialize ETAs\n";
                $etas_file = $phi_file;
            } elsif ($etas_file && -f $etas_file) {
                # use $ETAS FILE= in abscence of original execution phi file
                print "Original model has no phi output but has \$ETAS file; will use to initialize ETAs\n";
            }
            if (defined $etas_file) {
                if (!defined $mceta or $mceta < 1) {
                    $mceta = '1';
                    $derivatives_model->add_option(
                        record_name => 'estimation',
                        option_name => 'MCETA',
                        option_value => $mceta,
                        add_record => 1,
                    );
                }
                if (scalar @{ $etas_records } > 0 and $phi_file and ($phi_file ne $etas_file)) {
                    print "> Overwriting previous \$ETAS records with phi output file\n";
                }
                $derivatives_model->set_records(type => 'etas', record_strings => [ "FILE=$etas_file" ]);
                if (not defined $derivatives_model->extra_files) {
                    $derivatives_model->extra_files([]);
                }
                push @{$derivatives_model->extra_files}, $etas_file;
            } elsif (defined $mceta and $mceta > 1) {
                print "Original model has no phi output nor \$ETAS file record, but has MCETA=$mceta (can be slow)\n";
            } else {
                print "Warning: Original model has no phi output, \$ETAS file nor MCETA>1; ETAs will be initialized at 0\n";
            }
            if (!defined $phi_file) {
                print "> If OFV differ (due to shape-distortion of EBE profiles) consider executing original model for phi output file\n";
            }
        }

        $derivatives_model->set_maxeval_zero(
            print_warning => 1,
            last_est_complete => $self->last_est_complete(),
            niter_eonly => $self->niter_eonly(),
            need_ofv => 1,
        );

        $derivatives_model->remove_records( type => 'table' );

        if ($self->sizes_pd() > 50 and not PsN::minimum_nonmem_version(7, 3) or $self->sizes_pd() > 500) {
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

        #1.1
        if( $self->lst_file ){
            $derivatives_model -> update_inits (from_output_file => $self->lst_file());
        }

        #1.9
        my @tablestrings = ('ID','DV');
        my @inputstrings = ('ID','DV');

        if ( should_add_mdv(model => $derivatives_model) ){
            push(@tablestrings,'MDV');
            if (not $self->use_data_format) {
                push(@inputstrings, 'MDV');
            }
        }

        #1.10

        if ($self->foce()){
            push(@tablestrings,'CIPREDI=OPRED');
        }else{
            push(@tablestrings,'PREDI=OPRED');
        }

        push(@inputstrings,'OPRED');
        #1.11
        if ($self->epsilon()){
            for (my $j=1; $j<=$nEPS; $j++){
                if ($j<10){
                    push(@tablestrings,('H0'.$j.'1'));
                }else{
                    push(@tablestrings,'H'.$j.'1');
                }
                push(@inputstrings,'D_EPS'.$j);
            }
        }else{
            if ($self->error eq 'propadd'){
                push(@tablestrings,'WP');
                push(@inputstrings,'OWP');
                push(@tablestrings,'WA');
                push(@inputstrings,'OWA');
            }elsif ($self->error eq 'add'){
                push(@tablestrings,'W');
                push(@inputstrings,'OW');
            }elsif ($self->error eq 'prop'){
                push(@tablestrings,'W');
                push(@inputstrings,'OW');
            }elsif ($self->error eq 'exp'){
                push(@tablestrings,'WE');
                push(@inputstrings,'OWE');
            }elsif ($self->error eq 'user'){
                1;
            }else{
                croak('Unknown error type '.$self->error);
            }
        }
        #these may be needed for user error code, or for IGNORE
        push(@tablestrings,@extra_parameters);
        push(@inputstrings,@extra_parameters);

        for (my $i=1;$i<=$nETA;$i++){
            if ($i<10){
                push(@tablestrings,('G0'.$i.'1'));
            }else{
                push(@tablestrings,('G'.$i.'1'));
            }
            push(@inputstrings,('D_ETA'.$i));
        }
        if ($self->foce()){
            for (my $i=1;$i<=$nETA;$i++){
                push(@tablestrings,('ETA'.$i));
                push(@inputstrings,('OETA'.$i));
            }
        }

        #handle second order and linearized epsilon imitate cwres
        if ($self->second_order() or $self->epsilon()) {
            #can look for ADVAN<any number> this way
            my ($advan, $junk) = $derivatives_model->problems->[0]->_option_val_pos(
                record_name => 'subroutine',
                name => 'ADVAN',
                exact_match => 0
            );
            my $have_advan = scalar(@{$advan}) > 0;

            my $code_records;
            my $H = 'H';
            if ($have_advan) {
                # We have and ADVAN option in $SUBROUTINE, get $ERROR code
                $code_records = $derivatives_model->problems->[0]->errors;
                $H = 'HH';
            } else {
                # No ADVAN subroutine, we should modify $PRED code
                $code_records = $derivatives_model->problems->[0]->preds;
            }

            # Get code array reference, so we can update the code inplace.
            my $code = $code_records->[0]->verbatim_last;
            my $abbr_code = $code_records->[0]->code;

            if (not defined $code) {
                $code = [];
                $code_records->[0]->verbatim_last($code);
            }

            for (my $i = 1; $i <= $nEPS; $i++) {
                for (my $j = 1; $j <= $nETA;$j++) {
                    push @{$abbr_code}, "D_EPSETA$i" . "_$j = 0";
                    if (not $self->nointer) {
                        push(@{$code}, "\"  D_EPSETA$i" . "_$j=$H($i," . ($j + 1) . ")");
                    }
                    push(@tablestrings, "D_EPSETA$i"."_$j");
                    push(@inputstrings, "D_EPSETA$i"."_$j");
                }
            }
            if ($self->second_order()) {
                for (my $i = 1; $i <= $nETA; $i++) {
                    for (my $j = 1; $j <= $i; $j++) {
                        push @{$abbr_code}, "D2_ETA$i" . "_$j = 0";
                        push(@{$code}, "\"  D2_ETA$i" . "_$j=G($i," . ($j + 1) . ")");
                        push(@tablestrings, "D2_ETA$i"."_$j");
                        push(@inputstrings, "D2_ETA$i"."_$j");
                    }
                }
            }
        }
        #1.12
        push(@tablestrings, @covariates);
        push(@inputstrings, @covariates);
        push(@tablestrings, @{$self->extra_data_columns});
        push(@inputstrings, @{$self->extra_data_columns});

        #GZs and GKs are added to code further down, add_code_gfunc
        foreach my $parameter ( keys %{$self -> test_relations()} ){
            push(@tablestrings,'OGZ_'.$parameter);
            push(@inputstrings,'OGZ_'.$parameter);
            push(@tablestrings,'OGK_'.$parameter);
            push(@inputstrings,'OGK_'.$parameter);
        }

        #1.13
        $datafilename = 'derivatives_covariates.dta';
        if($linearize_only){
            $datafilename = $self->basename.'.dta';
        }

        # Make sure that all ignored or accepted columns get added
        my $ignored_columns = $derivatives_model->problems->[0]->ignored_or_accepted_columns();

        for my $colname (@$ignored_columns) {
            if (not array::string_in($colname, \@tablestrings) and not array::string_in($colname, \@inputstrings)) {
                push @tablestrings, $colname;
                push @inputstrings, $colname;
            }
        }

        my $l2_colno = $derivatives_model->problems->[0]->find_data_column(column_name => 'L2');
        if (not array::string_in('L2', \@inputstrings) and $l2_colno != -1) {
            push(@inputstrings, 'L2');
            push(@tablestrings, 'L2');
        }
        ($l2_index) = grep { $inputstrings[$_] eq 'L2' } (0 .. @inputstrings-1);

        my $format = '24.16';
        if ($PsN::nm_major_version == 7 and $PsN::nm_minor_version == 4 and (not defined $PsN::nm_patch_version or $PsN::nm_patch_version == 0 or $PsN::nm_patch_version == 1)) {
            # Try to reduce precision in table output if buggy versions of NONMEM used with maximum 1000 characers wide datasets
            my $column_width = 25;
            my $table_width = scalar(@tablestrings) * $column_width;
            if ($table_width >= 1000) {
                while (1) {
                    $column_width--;
                    $table_width = scalar(@tablestrings) * $column_width;
                    if ($table_width < 1000) {
                        $format = ($column_width - 1) . '.' . ($column_width - 9);
                        print("Warning: PsN had to reduce the precision of the linearised dataset beacuse the used NONMEM version cannot handle " . 
                              "dataset files wider than 1000 characters. Concider using NONMEM 7.4.3 or newer.\n");
                        last;
                    } elsif ($column_width <= 16) {
                        croak("Your version of NONMEM cannot handle dataset files wider than 1000 characters. PsN has tried to reduce the precision " .
                              "of the linearised dattaset, but was unable to reduce it enough. Concider using NONMEM 7.4.3 or newer.");
                    }
                }
            }
        }

        if (defined $self->extra_table_columns) {
            # Add TAD to linbase dataset if TAD available. Used for linearized VA plots in FREM via qa.
            if (not array::string_in('TAD', \@tablestrings) and array::string_in('TAD', $self->extra_table_columns)) {
                push @tablestrings, 'TAD';
                push @inputstrings, 'TAD';
            }
        }

        push(@tablestrings,'NOPRINT','NOAPPEND','ONEHEADER');
        push(@tablestrings,'FILE='.$datafilename);
        push(@tablestrings, "RFORMAT=\"(1PE16.9,300(1PE$format))\"");
        $derivatives_model->set_records(type => 'table', record_strings => \@tablestrings);

        # An extra table was requested
        if (defined $self->extra_table_columns) {
            my @extra_tablestrings = @{$self->extra_table_columns};
            if (should_add_mdv(model => $derivatives_model)) {
                push @extra_tablestrings, 'MDV';
            }
            push @extra_tablestrings, ( 'NOPRINT', 'NOAPPEND', 'ONEHEADER', 'FILE=extra_table' );
            $derivatives_model->add_records(type => 'table', record_strings => \@extra_tablestrings);
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
        @code =();
        @code = @{$original_model->get_code(record => 'pk')};
        unless ( $#code > 0 ) {
            @code = @{$original_model->get_code(record => 'pred')};
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
            fuzzy_match => 1
        );

        $original_model->remove_records(type => 'theta');
        $original_model->remove_records(type => 'table');
        $original_model->remove_records(type => 'pk');
        $original_model->remove_records(type => 'des');
        $original_model->remove_records(type => 'error');
        $original_model->remove_records(type => 'subroutine');
        $original_model->remove_records(type => 'model');
        unless ($self->from_linearize and $self->keep_covariance) {
            $original_model->remove_records(type => 'covariance');
        }
        $original_model->remove_records(type => 'estimation');

        $original_model->set_records(type => 'input', record_strings => \@inputstrings);

        if (PsN::minimum_nonmem_version(7, 3)) {
            # replicate phi file usage as done for derivatives model earlier (see above)
            my $phi_file = $original_model->get_phi_file();
            my $etas_records = $original_model->record(record_name => 'etas');
            my $etas_file = $original_model->get_or_set_etas_file();
            if (!defined $phi_file and $etas_file and -f $etas_file) {
                $phi_file = $etas_file;
            }
            if (defined $phi_file) {
                if (!defined $mceta or $mceta < 1) {
                    $mceta = '1';
                }
                $original_model->set_records(type => 'etas', record_strings => [ "FILE=$phi_file" ]);
                if (not defined $original_model->extra_files) {
                    $original_model->extra_files([]);
                }
                push @{$original_model->extra_files}, $phi_file;
            }
        }

        my @eststrings;
        if (defined $mceta) {
            push(@eststrings, 'MCETA=' . $mceta);
        }

        if ($self->epsilon() or $self->error eq 'propadd' or $self->error eq 'prop'
                or $self->error eq 'exp' or $self->error eq 'user') {
            push(@eststrings, 'METHOD=COND', 'INTERACTION');
        } else {
            push(@eststrings, 'METHOD=ZERO');
        }
        push(@eststrings, 'MAXEVALS=9999999');
        push(@eststrings,$self->format) if (defined $self->format());
        if ($self->noabort()){
            push(@eststrings,'NOABORT');
        }
        if (defined $self->estimation_options) {
            push @eststrings, $self->estimation_options;
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
        }

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
        $sum_count=1;
        $sum_string='BSUM'.$sum_count;
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

        $sum_string='IPRED=OPRED+BASE_TERMS';
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
                    my $line;
                    if ($self->foce) {
                        $line = 'D_EPSETA'.$i.'_'.$j.'*(ETA('.$j.')-OETA'.$j.')';
                    } else {
                        $line = 'D_EPSETA'.$i.'_'.$j.'*ETA('.$j.')';
                    }
                    if (length($string.$line) >= 63){
                        $string .= ')'."\n";
                        push(@pred_block,$string);
                        $err_count++;
                        $string = 'ERR'.$err_count.'='.$eps.'*(';
                    }else{
                        $string .= '+';
                    }
                    $string .= $line;
                }
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
                                if ($string !~ /\(\)/) {        # Workaround to fix bug with ()
                                    push(@pred_block,$string);
                                    $err_count++;
                                }
                                $string = 'ERR'.$err_count.'='.$eps.'*(';
                            }else{
                                $string .= '+' if (length($string) > 15);
                            }
                            $string .= $line;
                        }
                    }
                    $string .= ")\n";
                    push(@pred_block,$string);
                }
            }

            if ($nsigmas > 0) {
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
            } else {
                push @pred_block, "Y=IPRED\n";
            }
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
        unshift @pred_block, "\n";
        $original_model -> set_records( type => 'pred',
            record_strings => \@pred_block );

        $original_model -> fixed( parameter_type => 'sigma',
            new_values => [[(0) x $original_model -> nsigmas -> [0] ]] );
        # Refix all sigma 0
        if ($nsigmas > 0) {
            my @sigmas = @{$original_model->problems->[0]->sigmas};
            for my $record (@sigmas) {
                for my $option (@{$record->options}) {
                    if ($option->init == 0) {
                        $option->fix(1);
                    }
                }
            }
        }

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
                    copy( $dir.'derivatives_covariates'.$stepname.'.dta', "$datafilename" );
                    copy( $old_derivatives.'mod', 'copy_last_forward_derivatives.mod');
                    copy( $old_derivatives.'lst', 'copy_last_forward_derivatives.lst');
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
        }
    }

    if ($self->step_number()==1 or $self->update_derivatives()){
        my $derivatives_ofv;
        my $derivatives_name = '';
        my $reused=0;
        #set name of datafile before creating it so that will not read data here
        $original_model -> ignore_missing_data(1);
        $original_model -> datafiles(new_names => [$datafilename]);

        if ($self->step_number()==1 and $self->derivatives_data()){
            #do not run derivatives model, instead copy file to right name
            copy( $self->derivatives_data(), $datafilename );
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

            if ($derivatives_model->have_output()) {
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
                # Find synonyms to DV and MDV and replace header in generated dataset
                # This is a workaround for a bug in NONMEM causing the synonym names always to be put in the header of tables
                # Trigger on extra_table_columns to connect it to postprocessing needs of dataset
                if (defined $self->extra_table_columns) {
                    my $synonyms = $derivatives_model->find_input_synonyms(columns => ['DV', 'MDV']);
                    if (scalar(keys %$synonyms) > 0) {
                        nmtablefile::rename_column_names(filename => $datafilename, replacements => $synonyms);
                    }
                }
                if (defined $l2_index) {       # Have L2 must renumber. Risk of mangling it with IGNORE
                    my $tablefile = nmtablefile->new(filename => $datafilename);
                    $tablefile->renumber_l2_column(column => $l2_index, format => '%.16E');
                    $tablefile->write(path => $datafilename, colsize => 24);
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
                print LOG "The ofv of the nonlinear base model :$ofv        $derivatives_name\n";
                ui->print(category => 'linearize',
                    message =>"\nThe ofv of the nonlinear base model :$ofv        $derivatives_name\n");
            }else{
                if ($reused){
                    print LOG "The ofv of the nonlinear model              :$ofv        $derivatives_name\n";
                }else{
                    print LOG "The ofv of the updated nonlinear model      :$ofv        $derivatives_name\n";
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
            $original_model -> set_outputfile();
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
    }

    # Remove no longer needed IGN or ACC in $DATA. Might crash future runs, i.e. with .EQ. or .NE.
    $original_model->problems->[0]->datas->[0]->remove_ignore_accept();

    if ($self->use_data_format and defined $datafilename) {       # To account for bug in NONMEM for 1000+ charcolumn data sets

        my $data = data->new(
            filename => $datafilename,
            ignoresign => '@',
            missing_data_token => $self->missing_data_token,
            ignore_missing_files => 0,
            parse_header => 1,
            space_separated => 1);

        # Remove all MDV != 0
        my $was_filtered = $data->filter_column(colname => 'MDV', value => 1);
        if ($was_filtered) {
            $data->_write(overwrite => 1, as_table => 1);
        }

        #Also filter extra_table to keep it of same length as the dataset
        if (defined $self->extra_table_columns) {
            my $extra_table_data = data->new(
                filename => 'extra_table',
                ignoresign => '@',
                missing_data_token => $self->missing_data_token,
                ignore_missing_files => 0,
                parse_header => 1,
                space_separated => 1);

            # Remove all MDV != 0
            $was_filtered = $extra_table_data->filter_column(colname => 'MDV', value => 1);
            if ($was_filtered) {
                $extra_table_data->_write(overwrite => 1, as_table => 1);
            }
        }

        my $numcols = scalar(@{$data->header});
        $original_model->add_option(record_name => 'data', option_name => "($numcols(1X,E15.8))");
    } else {
        # If have MDV ignore all MDV != 0
        if (should_add_mdv(model => $original_model)) {
            my $ign_acc = $original_model->problems->[0]->datas->[0]->have_ignore_accept();
            if ($ign_acc != 2) {
                $original_model->add_option(record_name => 'data', option_name => 'IGNORE(MDV.NEN.0)');
            } else {
                $original_model->add_option(record_name => 'data', option_name => 'ACCEPT(MDV.EQN.0)');
            }
            if ($ign_acc == 0) {        # If adding an ignore or accept we might need to reorder
                $original_model->problems->[0]->psn_record_order(1);   # In case $DATA was before $INPUT
            }
        }
    }

    # Account for an estimation bug in NONMEM 7.4.0 and 7.4.1
    if ($PsN::nm_major_version == 7 and $PsN::nm_minor_version == 4 and defined $PsN::nm_patch_version and ($PsN::nm_patch_version == 0 or $PsN::nm_patch_version == 1)) {
        $original_model->add_option(record_name => 'estimation', option_name => 'SLOW');
    }

    # Set PRINT=1 to facilitate fault finding
    $original_model->add_option(record_name => 'estimation', option_name => 'PRINT', option_value => '1');

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
    return if ((defined $self->max_steps() and $self->max_steps() == 0) and ($self->step_number()==1) and
        scalar(keys %{$self->test_relations}) == 0
            and $self->linearize);


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
                        if ($self->picky) {
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
    print LOG $short_log_text if (defined $short_log_text);
    close( LOG );

    my %return_section1;
    $return_section1{'name'} = 'relation.chosen.in.step';
    $return_section1{'values'} = [];
    $return_section1{'labels'} = [];
    if ( defined $chosen_parameter and defined  $chosen_covariate ) {
        $return_section1{'values'}[0][0] = $chosen_parameter.$chosen_covariate;
        my $task = $self -> search_direction eq 'forward' ? 'Adding' : 'Removing';
        ui -> print( category => 'scm',
            message  => "$task $chosen_covariate on $chosen_parameter state $chosen_state" )
        unless $self -> parent_threads > 1;
    }
    push( @{$self -> results->[$model_number-1]{'own'}},
        \%return_section1 );

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
            test_log           => $test_log_ref);

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
            tool::scm ->new( %{common_options::restore_options(@common_options::tool_options)},
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
                step_number          => ($self -> step_number() + 1),
                raw_results_file     => [$self -> raw_results_file ->[$model_number-1]],
                logfile              => [$self -> logfile->[$model_number-1]],
                base_criteria_values => $new_base_crit_val_ref,
                parent_tool_id       => $self -> tool_id,
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
            if (defined $internal_scm->resulting_model) {
                $self->resulting_model($internal_scm->resulting_model);
            }
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
            results            => $self -> results->[$model_number-1]{'own'});


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

    my %return_section2;
    $return_section2{'name'} = 'base.criteria.values';
    $return_section2{'values'} = $self -> base_criteria_values;
    $return_section2{'labels'} = undef;
    push( @{$self -> results->[$model_number-1]{'own'}},\%return_section2 );

    my %return_section3;
    $return_section3{'name'} = 'included.relations';
    $return_section3{'values'} = $self -> included_relations;
    $return_section3{'labels'} = undef;
    push( @{$self -> results->[$model_number-1]{'own'}},\%return_section3 );


    # This loop tries to minimize the data written to disc.
    for ( my $i = 0; $i < scalar @{$self -> prepared_models->[$model_number-1]{'own'}}; $i++ ) {
        $self -> prepared_models->[$model_number-1]{'own'}[$i] -> {'outputs'} = undef; #FIXME
    }

    my %return_section4;
    $return_section4{'name'} = 'final.model';
    $return_section4{'values'}[0][0] = $final_model;
    $return_section4{'labels'} = undef;
    push( @{$self -> results->[$model_number-1]{'own'}},\%return_section4 );
}

sub should_add_mdv
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
        );
    my $model = $parm{'model'};

    my $add_mdv=0;
    if ( defined $model->problems->[0] -> preds and scalar(@{$model->problems->[0] -> preds})>0){
        if( defined $model->problems()->[0] -> inputs and
            defined $model->problems()->[0] -> inputs -> [0] -> options ) {
            my ($arr,$time_added) = $model->problems()->[0] -> inputs -> [0]->get_filter_table_names;
            croak ("found no undropped data column in \$INPUT ") unless (defined $arr);
            for (my $i=0; $i< scalar(@{$arr}); $i++){
                if($arr->[$i] eq 'MDV'){
                    $add_mdv=1;
                    last;
                }
            }
        }
    }else{
        $add_mdv=1;
    }
    return $add_mdv;
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
        my $log_text;
        if (1){
            no warnings qw(uninitialized);
            $log_text = sprintf("%-8s",$step_relations[$i]{'parameter'}.
                                $step_relations[$i]{'covariate'}.'-'.
                                $step_relations[$i]{'state'}).
                                sprintf("%12s","$ofvname  ").
                                sprintf("%12.5f",$base_ofv) . ' ' .
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
            $change = $df_sign * Statistics::Distributions::chisqrdistr($df_sign * $n_param_diff, $pval);
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
                push (@p_values, (Statistics::Distributions::chisqrprob($df_sign * $n_param_diff, $df_sign * $test_val)));
            }
            $test_val = sprintf("%21.5f",$test_val);
            $ofv = sprintf("%12.5f",$ofvs[$i])
        }
        push ( @ofv_drops, $test_val );

        my $log_text = sprintf("%-16s",$step_relations[$i]{'parameter'}.
            $step_relations[$i]{'covariate'}.'-'.
            $step_relations[$i]{'state'}).
            sprintf("%6s"," PVAL ").
            sprintf("%12.5f",$base_ofv) . ' ' .
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
            if ($fullpath ne $string) { # For the same path do not copy to avoid warning
                copy($fullpath, $string);
            }
            $self->main_data_file($string);
        }else{
            unless (defined $self->main_data_file and length($self->main_data_file)>0){
                $self->main_data_file($orig_model->datafiles(absolute_path => 1)->[0]);
            }
        }
        my $one_model_per_covariate = $self->one_model_per_covariate;
        # hash from covariate to model object
        # Only used if one_model_per_covariate
        my %applicant_models;
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
                    $state = $new_states[$si];

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
                    my $first = 1;
                    if ($one_model_per_covariate) {
                        if (exists($applicant_models{$covariate})) {
                            $applicant_model = $applicant_models{$covariate};
                            $first = 0;
                        }
                    }
                    my @used_covariates;
                    if ($first) {
                        $applicant_model = $orig_model->copy(filename => $dir.$filename,
                                                             copy_datafile => 0,
                                                             write_copy => 0,
                                                             copy_output => 0);
                        $applicant_model->ignore_missing_files(1);
                        $applicant_model->outputfile($odir.$outfilename);
                        $applicant_model->set_outputfile();
                        my @table_names = @{$applicant_model->table_names};
                        for (my $i = 0; $i <= $#table_names; $i++) {
                            for (my $j = 0; $j < scalar @{$table_names[$i]}; $j++) {
                                (undef, undef, my $name_of_table) = File::Spec->splitpath($table_names[$i][$j]);
                                $table_names[$i][$j] = $self->directory .
                                '/m'.$model_number.'/' .
                                $filename.'.' .
                                $name_of_table;
                            }
                        }
                        $applicant_model -> table_names( new_names            => \@table_names,
                                                         ignore_missing_files => 1);
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
                    if ($first) {
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
                            if ($self->linearize) {
                                $applicant_model->remove_records(type => 'etas');
                                my $phi_file = $initial_estimates_model->get_phi_file();

                                my $mceta = $initial_estimates_model->get_option_value(record_name => 'estimation',
                                    option_name => 'MCETA',
                                    fuzzy_match => 1
                                );
                                if (defined $phi_file) {
                                    if (not defined $mceta or $mceta < 1) {
                                        $applicant_model->add_option(
                                            record_name => 'estimation',
                                            option_name => 'MCETA',
                                            option_value => 1,
                                            add_record => 1,
                                        );
                                    }
                                    $applicant_model->set_records(type => 'etas', record_strings => [ "FILE=$phi_file" ]);
                                    if (not defined $applicant_model->extra_files) {
                                        $applicant_model->extra_files([]);
                                    }
                                    push @{$applicant_model->extra_files}, $phi_file;
                                }
                            }
                        } else {
                            $applicant_model -> update_inits( from_model    => $orig_model,
                                update_thetas => 1,
                                update_omegas => 1,
                                update_sigmas => 1,
                                ignore_missing_parameters => 1 )
                            unless (not $self->run_linearized_base());
                        }
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
                    if (not $one_model_per_covariate) {
                        $applicant_model->_write();
                        push(@new_models, $applicant_model);
                    } else {
                        $applicant_models{$covariate} = $applicant_model;
                    }

                    if (not $one_model_per_covariate or $first) {
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
        }
        if ($one_model_per_covariate) {
            foreach my $model (values %applicant_models) {
                $model->_write();
                push(@new_models, $model);
            }
        }
        open( TMP, ">".$self -> directory."/m$model_number/done" );
        close( TMP );
        close( DONE_LOG );
    } else {
        ui -> print( category => 'scm',
            message  => "Recreating models from previously run step" );
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

sub get_typestring
{
    my %parm = validated_hash(\@_,
        state => { isa => 'Int', optional => 0 },
        continuous => { isa => 'Bool', optional => 0 },
        code => { isa => 'ArrayRef', optional => 0 },
    );
    my $state = $parm{'state'};
    my $continuous = $parm{'continuous'};
    my $code = $parm{'code'};

    my $codetype = $state;
    my $typestring;

    if ( scalar @{$code} == 1){
        if( $code->[0] eq 'none' ) {
            $codetype = 1;
            $typestring = 'none';
        }elsif( $code->[0] eq 'linear' ) {
            $codetype = 2;
            if ($continuous){
                $typestring = 'linear';
            }else{
                $typestring = 'categorical';
            }
        }elsif( $code->[0] eq 'hockey-stick' ) {
            $codetype = 3;
            $typestring = 'hockey-stick';
        }elsif( $code->[0] eq 'exponential' ) {
            $codetype = 4;
            $typestring = 'exponential';
        }elsif( $code->[0] eq 'power' ) {
            $codetype = 5;
            $typestring = 'power';
        }elsif ( $code->[0] ne '' ){
            $typestring = 'user';
        }
    }elsif ( scalar @{$code} > 1){
        $typestring = 'user';
    }

    unless (defined $typestring){
        if ( $codetype == 1 ) {
            $typestring = 'none';
        } elsif ( $codetype == 2 ) {
            if ($continuous){
                $typestring = 'linear';
            }else{
                $typestring = 'categorical';
            }
        } elsif ( $codetype == 3 ) {
            $typestring = 'hockey-stick';
        } elsif ( $codetype == 4 ) {
            $typestring = 'exponential';
        } elsif ( $codetype == 5 ) {
            $typestring = 'power';
        } else {
            croak("State $codetype cannot be used when not defined in the [code] section.\n" );
        }
    }
    unless ($typestring eq 'user'){
        $code->[0]='' if (scalar(@{$code})>0);
    }

    if (not $continuous){
        if (($typestring eq 'hockey-stick') or ($typestring eq 'power') or ($typestring eq 'exponential')){
            croak("The $typestring parameterization is not defined for categorical covariates");
        }
    }

    return $typestring;
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
    my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
    my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : ();

    my $new_code = [];
    my $new_inits = [];
    my $new_bounds = {};

    my $typestring = get_typestring(state => $state,
                                    continuous => $continuous,
                                    code => \@code);


    if ($typestring eq 'power'){
        if ($statistics{'min'} < 0){
            ui -> print( category => 'scm',
                         message  => "Warning: Creating power code for $covariate which has minimum < 0. ".
                         "Covariate function value ".
                         "may be negative or imaginary for observations where $covariate is < 0, which would lead to ".
                         "errors.",newline => 1);
        }
    }
    if ($sum_covariates and (($typestring eq 'power') or ($typestring eq 'exponential'))){
        ui->print(category => 'scm',
                  message => "Warning: The $typestring relation is inappropriate on ".
                  "logit parameters.",newline => 1) ;
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

    my ($nexttheta,$fraction) = get_covariate_code(statistics => \%statistics,
                                                   type => $typestring,
                                                   sum_covariates => $sum_covariates,
                                                   linearize =>$self->linearize,
                                                   missing_data_token => $missing_data_token,
                                                   parameter => $parameter,
                                                   covariate => $covariate,
                                                   code => \@code,
                                                   theta_number =>$start_theta,
                                               categorical_mean_offset => $self->categorical_mean_offset);

    my ($max,$min,$median,$mean) = format_max_min_median_mean(statistics => \%statistics);

    get_covariate_theta_bounds_inits(bounds => \%bounds,
                                     max => $max,
                                     min => $min,
                                     median => $median,
                                     type => $typestring,
                                     inits => \@inits,
                                     global_init => $self->global_init,
                                     ntheta => ($nexttheta-$start_theta),
                                     linearize => $self->linearize,
                                     fraction => $fraction,
                                     sum_covariates => $sum_covariates);

    my $end_theta = $nexttheta -1;

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
    @code = @{$applicant_model->get_code(record => 'pk')};
    my $use_pred = 0;
    unless ($#code > 0) {
        @code = @{$applicant_model->get_code(record => 'pred')};
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
        if ($relationarea) {
            $found_REL = $i;
            if (/$parameter$covariate/) {
                $found_correct_REL = 1;
                last ;
            }
            if ($sum_covariates){
                @row = split(/\)\+\(/);
            }else{
                @row = split(/\)\*\(/);
            }
        }
        $i++;
    }

    # If we have old scm code present.
    if ($found_REL) {
        if (not $found_correct_REL) {
            if ($#row > 2) {
                @code =  (@code[0..$found_REL],
                    "$parameter"."COV=$parameter"."COV$operator$parameter$covariate\n",
                    @code[$found_REL+1..$#code]);
            } else {
                chomp($code[$found_REL]);
                $code[$found_REL] = $code[$found_REL]."$operator$parameter$covariate\n";
            }
        }
    } else {
        if ($found_anchor >= 0) {
            @code = (@code[0..$found_anchor],
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

    if ($found_anchor >= 0) {
        @code = (@code[0..$found_anchor],
            "\n;;; $parameter$covariate-DEFINITION START\n",
            @definition_code,
            ";;; $parameter$covariate-DEFINITION END\n\n",
            @code[$found_anchor+1..$#code]);
    } else {
        @code = ( "\n;;; $parameter$covariate-DEFINITION START\n",
            @definition_code,
            ";;; $parameter$covariate-DEFINITION END\n\n",
            @code );
    }
    # Add to the parameter code
    if (not $found_REL) {
        my $success = 0;
        for (reverse @code) {
            #want to find last occurrence
            if (/[^A-Z0-9_]*TV(\w+)\s*=\s*/ and $1 eq $parameter) {
                #add new definition line after last occurence
                $_ = $_."\n"."TV$parameter = $parameter"."COV$operator"."TV$parameter\n";
                $success = 1;
                last; #only change the last line where appears
            }
        }
        if (not $success) {
            croak("Could not determine a good place to add the covariate relation.\n".
                " i.e. No TV$parameter was found\n" );
        }
    }
    if ($use_pred) {
        $applicant_model->set_code(record => 'pred', code => \@code);
    } else {
        $applicant_model->set_code(record => 'pk', code => \@code);
    }

    #initial values must be set first, since we need to add if absent

    $applicant_model->initial_values(
        parameter_numbers => [[$start_theta..$end_theta]],
        new_values => [\@inits],
        add_if_absent => 1,
        parameter_type => 'theta',
        problem_numbers => [1],
    );
    $applicant_model->lower_bounds(
        parameter_type => 'theta',
        parameter_numbers => [[$start_theta..$end_theta]],
        problem_numbers => [1],
        new_values => [$bounds{'lower'}],
    );
    $applicant_model->upper_bounds(
        parameter_type => 'theta',
        parameter_numbers => [[$start_theta..$end_theta]],
        problem_numbers => [1],
        new_values => [$bounds{'upper'}],
    );
    $applicant_model->labels(
        parameter_type => 'theta',
        parameter_numbers => [[$start_theta..$end_theta]],
        problem_numbers => [1],
        new_values => [\@labels],
    );

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
    for (my $i = 1; $i <= $nthetas; $i++) {
        push (@labels, $parameter . $covariate . $i);
    }

    my $start_theta = $applicant_model->nthetas() + 1;
    my $end_theta = $start_theta + $nthetas - 1;
    my $operator = '*';
    $operator = '+' if ($sum_covariates);

    my $tmp = $start_theta;

    #handle mulitple THETA on same line, handle multiple uses of same THETA
    my %original_to_model_thetas;
    for (my $i = 1; $i <= $nthetas; $i++) {
        $original_to_model_thetas{$i} = 0;
    }
    for (@definition_code) {
        while (/THETA\((\d+)\)/) {
            if ($original_to_model_thetas{$1} == 0) {
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
    @code = @{$applicant_model->get_code(record => 'pred')};
    $use_pred = 1;
    if ($#code <= 0) {
        croak("PRED not defined in " . $applicant_model->filename . "\n");
    }

    my $found_REL = 0;
    my $found_anchor = -1;
    my $i = 0;
    my $relationarea = 0;
    my @row;
    my $found_correct_REL = 0;
    for (@code) {
        if (/^;;;SCM-ANCHOR/) {
            $found_anchor = $i;
            $i++;
            next;
        }
        if (/^;;; (\w+)-RELATION START/ and $1 eq $parameter) {
            $relationarea = 1;
            $i++;
            next;
        }
        if (/^;;; (\w+)-RELATION END/ and $1 eq $parameter) {
            $relationarea = 0;
            last;
        }
        if ($relationarea) {
            $found_REL = $i;
            if (/$parameter$covariate/) {
                $found_correct_REL = 1;
                last;
            }
            if ($sum_covariates) {
                @row = split(/\)\+\(/);
            } else {
                @row = split(/\)\*\(/);
            }
        }
        $i++;
    }

    # If we have old scm code present.
    my $etanum;
    if ($found_REL) {
        if (not $found_correct_REL) {
            if ($#row > 2) {
                print "warning: adding covariates to old scm code not tested\n";
                @code = (@code[0..$found_REL],
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
        if (defined $parameter_eta{$parameter}) {
            $etanum= $parameter_eta{$parameter};
        } else {
            croak("Could not extract ETA number for $parameter");
        }
        if ($found_anchor >= 0) {
            @code =  (@code[0..$found_anchor],
                ";;; $parameter-RELATION START\n",
                "; $parameter IS ETA$etanum",
                "GZ_$parameter"." = $parameter$covariate\n",
                ";;; $parameter-RELATION END\n\n",
                @code[$found_anchor+1..$#code]);
        } else {
            @code = ( ";;; $parameter-RELATION START\n",
                "; $parameter IS ETA$etanum",
                "GZ_$parameter"." = $parameter$covariate\n",
                ";;; $parameter-RELATION END\n\n",
                @code );
        }
    }

    if ($found_anchor >= 0) {
        @code =  (@code[0..$found_anchor],
            "\n;;; $parameter$covariate-DEFINITION START\n",
            @definition_code,
            ";;; $parameter$covariate-DEFINITION END\n\n",
            @code[$found_anchor+1..$#code]);
    } else {
        @code = ( "\n;;; $parameter$covariate-DEFINITION START\n",
            @definition_code,
            ";;; $parameter$covariate-DEFINITION END\n\n",
            @code );
    }

    # Add to the parameter code
    if (not $found_REL) {
        my $success = 0;
        for (@code) {
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
        if (not $success) {
            croak("Could not determine a good place to add the covariate relation.\n".
                " i.e. No IPRED= was found\n");
        }
    }
    if ($use_pred) {
        $applicant_model->set_code(record => 'pred', code => \@code);
    } else {
        $applicant_model->set_code(record => 'pk', code => \@code);
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
    my @code = @{$applicant_model->get_code(record => 'pk')};
    my $use_pred = 0;
    unless ($#code > 0) {
        @code = @{$applicant_model->get_code(record => 'pred')};
        $use_pred = 1;
    }
    if ($#code <= 0) {
        croak("Neither PK or PRED defined in " .
            $applicant_model -> filename . "\n" );
    }

    push(@code,';;;SCM-LINEARIZE_CONSTANTS'."\n") unless ((defined $self->directory_name_prefix) and
                                                          $self->directory_name_prefix eq 'linearize');
    foreach my $parameter (keys %parameter_G){
        push(@code, 'OGZ_' . $parameter . '=' . $parameter_G{$parameter} . "\n");
        if ($parameter_relation{$parameter} eq 'exponential') {
            push(@code, 'OGK_' . $parameter . '=1/' . $parameter_G{$parameter} . "\n");
        } elsif ($parameter_relation{$parameter} eq 'proportional') {
            push(@code,'OGK_'.$parameter.
                '='.$parameter.'/(TV'.$parameter.'*'.$parameter_G{$parameter}.')'.
                ' ; This gives (1+ETA)/'.$parameter.'COV'."\n");
        } elsif ($parameter_relation{$parameter} eq 'additive') {
            push(@code,'OGK_'.$parameter.
                '=TV'.$parameter.'/'.$parameter_G{$parameter}."\n");
        } elsif ($parameter_relation{$parameter} eq 'logit') {
            push(@code, 'OGK_' . $parameter . "=1\n");
        } else {
            croak("No relation (additive/exponential/proportional) defined for ETA on $parameter");
        }
    }
    if ($use_pred) {
        $applicant_model->set_code(record => 'pred', code => \@code);
    } else {
        $applicant_model->set_code(record => 'pk', code => \@code);
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
          copy_data => 1);
    #clean 2 later
    ui->print(category => 'xv_scm', message => $mess) unless ($self->parent_threads > 1);
    $xv_base_fit->run();

    if ($derivatives_run) {
        #change $self->xv_pred_data to new filename from derivatives output.
        #this makes it impossible to use update_derivatives unless original pred_data is kept

        my $datafilename = 'derivatives_covariates.dta';
        my $newfilename = 'derivatives_covariates_pred.dta';

        copy($datafilename,$newfilename);
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
    ui->category($oldcat);
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

sub format_max_min_median_mean
{
    my %parm = validated_hash(\@_,
        statistics => { isa => 'HashRef', optional => 0 },
    );
    my $statistics = $parm{'statistics'};

    my $median = $statistics->{'median'};
    if (defined $median){
        $median = sprintf "%6.2f", $median;
        $median =~ s/^\s*//;
    }else{
        $median = '';
    }
    my $min = $statistics->{'min'};
    if (defined $min){
        $min = sprintf "%6.2f", $min;
        $min =~ s/^\s*//;
    }else{
        $min = '';
    }
    my $max = $statistics->{'max'};
    if (defined $max){
        $max = sprintf "%6.2f", $max;
        $max =~ s/^\s*//;
    }else{
        $max = '';
    }
    my $mean = $statistics->{'mean'};
    if (defined $mean) {
        $mean = sprintf "%6.2f", $mean;
        $mean =~ s/^\s*//;
    } else {
        $mean = '';
    }

    return ($max, $min, $median, $mean);
}

sub get_covariate_code
{
    my %parm = validated_hash(\@_,
        code => { isa => 'ArrayRef', optional => 0 },
        statistics => { isa => 'HashRef', optional => 0 },
        type => { isa => 'Str', optional => 0 },
        sum_covariates => { isa => 'Bool', optional => 0 },
        linearize => { isa => 'Bool', optional => 0 },
        missing_data_token => { isa => 'Str', optional => 0 },
        parameter => { isa => 'Str', optional => 0 },
        covariate => { isa => 'Str', optional => 0 },
        theta_number => { isa => 'Int', optional => 0 },
        categorical_mean_offset => { isa => 'Bool', default => 0 },
    );
    my $statistics = $parm{'statistics'};
    my $type = $parm{'type'};
    my $sum_covariates = $parm{'sum_covariates'};
    my $linearize = $parm{'linearize'};
    my $missing_data_token = $parm{'missing_data_token'};
    my $parameter = $parm{'parameter'};
    my $covariate = $parm{'covariate'};
    my $theta_number = $parm{'theta_number'};
    my $code = $parm{'code'};
    my $categorical_mean_offset = $parm{'categorical_mean_offset'};

    my $offset = '1';
    $offset = '0' if ($sum_covariates);

    my ($max,$min,$median,$mean) = format_max_min_median_mean(statistics => $statistics);
    my $have_missing_data = 0;
    $have_missing_data = 1 if $statistics->{'have_missing_data'};
    my $fraction;

    my $comment = '';

    if ( scalar @{$code} < 1 or ( scalar @{$code} == 1 and $code->[0] eq '' ) ) {
        croak("Input code is empty but type is 'user'.") if ($type eq 'user');
    } else {
        croak("Input code is defined but type is $type.") unless ($type eq 'user');
    }

    if ($type eq 'none') {
        $code->[0] = "   $parameter$covariate = $offset\n";
    } elsif ($type eq 'user') {
        my %unique_thetas;
        # count the thetas.
        for ( @{$code} ) {
            if (/mean/ and (not defined $mean or length($mean)==0)){
                croak("The mean is undefined for $covariate and cannot ".
                      "be used in user-written code.");
            }
            if (/median/ and (not defined $median or length($median)==0)){
                croak("The median is undefined for $covariate and cannot ".
                      "be used in user-written code.");
            }

            s/median/$median/g;
            s/mean/$mean/g;
            s/maximum/$max/g;
            s/minimum/$min/g;
            my $copy = $_;
            while ($copy =~ s/THETA\((\d+)\)//){
                unless (defined $unique_thetas{$1} and $unique_thetas{$1} == 1){
                    $unique_thetas{$1} = 1;
                    $theta_number++;
                }
            }
        }
    } elsif ($type eq 'linear') {
        if ($have_missing_data) {
            $code->[0] = $comment."IF($covariate.EQ.$missing_data_token) THEN\n";
            $code->[1] = "$comment   $parameter$covariate = $offset\n";
            $code->[2] = $comment."ELSE\n";
            my $sign = '-';
            $sign = '+' if ( $median < 0 );
            $code->[3] = "$comment   $parameter$covariate = ( $offset + THETA(".$theta_number++.
                ")*($covariate $sign ".abs($median)."))\n";
            $code->[4] = $comment."ENDIF\n";
        } else {
            my $sign = '-';
            $sign = '+' if ( $median < 0 );
            $code->[0] = "$comment$parameter$covariate = ( $offset + THETA(".$theta_number++.
                ")*($covariate $sign ".abs($median)."))\n";
        }
    }elsif ($type eq 'categorical'){
        my %factors = %{$statistics->{'factors'}};
        my @sorted = sort {$factors{$b}<=>$factors{$a}} keys(%factors); #most common first
        my $numlvs = scalar @sorted;
        $numlvs = $numlvs -1 if $have_missing_data;
        my $sum_values=0;
        if ($linearize or $categorical_mean_offset) {
            if ($numlvs > 2){
                croak("linearize option does not yet work with categorical covariates with more than two categories");
            }else{
                foreach my $key (@sorted){
                    $sum_values += $factors{$key} unless ($have_missing_data and ( $missing_data_token eq $key ));
                }
            }
        }

        my $first_non_missing = 1;
        my $missing_line;
        #initiate COMMON parameter if linearize and have missing data
        if ($linearize and $have_missing_data){
            push @{$code}, $comment."$parameter$covariate"."_COMMON=0  ";
        }
        for ( my $i = 0; $i <= $#sorted; $i++ ) {
            if ( $have_missing_data and ( $missing_data_token eq $sorted[$i]
                                          or $missing_data_token == $sorted[$i]) ) {
                if ($linearize){
                    $missing_line = $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate = $offset  ; Missing data\n";
                }else{
                    push @{$code}, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate = $offset  ; Missing data\n";
                }
            } else {
                if ( $first_non_missing ) {
                    if ($linearize or $categorical_mean_offset) {
                        $fraction=$factors{$sorted[$i]}/$sum_values;
                        $fraction = sprintf("%.6G",$fraction);
                        push @{$code}, $comment."; Frequency of most common case is ".$factors{$sorted[$i]}.
                            "/".$sum_values."=$fraction";
                        push @{$code}, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
                            "_COMMON=1; Most common case, indicator variable is 1";
                    }else{
                        push @{$code}, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
                            " = $offset  ; Most common\n";
                    }
                    $first_non_missing = 0;
                } else {
                    if ($linearize or $categorical_mean_offset) {
                        push @{$code}, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
                            "_COMMON=0";
                        push @{$code}, $comment."$parameter$covariate = ($offset + THETA(".$theta_number++.
                            ")*($fraction-$parameter$covariate"."_COMMON)) \n";
                        #comment for experiment
                        if (defined $missing_line){
                            push @{$code},$missing_line;
                        }
                    }else{
                        push @{$code}, $comment."IF($covariate.EQ.$sorted[$i]) $parameter$covariate".
                            " = ( $offset + THETA(".$theta_number++."))\n";
                    }
                }
            }
        }

    }elsif ($type eq 'hockey-stick'){
        my $sign = '-';
        $sign = '+' if ( $median < 0 );
        $code->[0] = "IF($covariate.LE.$median) $parameter$covariate = ( $offset + THETA(".
            $theta_number++.")*($covariate $sign ".abs($median)."))\n";
        $code->[1] = "IF($covariate.GT.$median) $parameter$covariate ".
            "= ( $offset + THETA(".$theta_number++.")*($covariate $sign ".abs($median)."))\n";
        if ( $have_missing_data ) {
            $code->[2] = "IF($covariate.EQ.$missing_data_token)   ".
                "$parameter$covariate = $offset\n";
        }
    }elsif ($type eq 'power'){
        ui -> print( category => 'scm',
                     message  => "Warning: Creating power code for $covariate which has minimum < 0. ".
                     "Covariate function value ".
                     "may be negative or imaginary for observations where $covariate is < 0, which would lead to ".
                     "errors.", newline => 1) if ($min < 0);

        ui->print(category => 'scm',
                  message => "Warning: The exponential relation is inappropriate on ".
                  "logit parameters.",newline => 1) if ($sum_covariates);
        my $sign = '';
        $sign = '-' if ( $median < 0 );
        if ( $have_missing_data ) {
            $code->[0] = "IF($covariate.EQ.$missing_data_token) THEN\n".
                "   $parameter$covariate = $offset\n".
                "ELSE\n".
                "   $parameter$covariate = (($sign$covariate/".abs($median).")**THETA(".$theta_number++."))\n".
                "ENDIF\n";
        }else{
            $code->[0] = "   $parameter$covariate = (($sign$covariate/".abs($median).")**THETA(".$theta_number++."))\n";
        }

    }elsif ($type eq 'exponential'){
        ui->print(category => 'scm',
                  message => "Warning: The exponential relation is inappropriate on ".
                  "logit parameters.",newline => 1) if ($sum_covariates);

        my $sign = '-';
        $sign = '+' if ( $median < 0 );
        if ( $have_missing_data ) {
            $code->[0] = "IF($covariate.EQ.$missing_data_token) THEN\n".
                "   $parameter$covariate = $offset\n".
                "ELSE\n".
                "   $parameter$covariate = EXP(THETA(".$theta_number++.")*($covariate $sign ".abs($median)."))\n".
                "ENDIF\n";
        }else{
            $code->[0] = "   $parameter$covariate = EXP(THETA(".$theta_number++.")*($covariate $sign ".abs($median)."))\n";
        }

    }else{
        croak("unknown get_code type $type");
    }

    return $theta_number,$fraction;
}

sub get_covariate_theta_bounds_inits
{
    my %parm = validated_hash(\@_,
        bounds => { isa => 'HashRef', optional => 0 },
        inits => { isa => 'ArrayRef', optional => 0 },
        max => { isa => 'Num', optional => 0 },
        min => { isa => 'Num', optional => 0 },
        median => { isa => 'Num', optional => 0 },
        fraction => { isa => 'Maybe[Num]', optional => 1 },
        ntheta => { isa => 'Int', optional => 0 },
        type => { isa => 'Str', optional => 0 },
        sum_covariates => { isa => 'Bool', optional => 0 },
        linearize => {isa => 'Bool', optional => 0},
        global_init => {isa => 'Num', optional => 0},
    );
    my $max = $parm{'max'};
    my $min = $parm{'min'};
    my $median = $parm{'median'};
    my $fraction = $parm{'fraction'};
    my $ntheta = $parm{'ntheta'};
    my $type = $parm{'type'};
    my $sum_covariates = $parm{'sum_covariates'};
    my $bounds = $parm{'bounds'};
    my $linearize = $parm{'linearize'};
    my $inits = $parm{'inits'};
    my $global_init = $parm{'global_init'};

    if ($linearize and ($type eq 'categorical') and (not defined $fraction)) {
        croak("must define fraction if linearize and categorical");
    }

    # Want dynamic bounds for exponential
    # 0.01 < exp(theta*(WT-meanWT)) < 100
    my $upper_bound;
    my $lower_bound;
    if ($type eq 'exponential') {
        my $min_diff = $min - $median;
        my $max_diff = $max - $median;
        my $low_exp_bound = 0.01;
        my $high_exp_bound = 100;
        if ($min_diff == 0 or $max_diff == 0) {     # No difference from median. We can set any bounds
            $lower_bound = 0.01;
            $upper_bound = 100;
        } else {
            $upper_bound = array::min((log($low_exp_bound) / $min_diff), log($high_exp_bound) / $max_diff);
            $lower_bound = array::max((log($low_exp_bound) / $max_diff), log($high_exp_bound) / $min_diff);
        }
    }

    unless (defined $bounds->{'upper'} and defined $bounds->{'upper'}[0]) {
        if ($sum_covariates) {
            for (my $i = 0; $i < $ntheta; $i++) {
                $bounds->{'upper'}[$i] = 20;
            }
        } elsif ($type eq 'linear') {
            my $upper_bound;
            if ($median-$min == 0) {
                $upper_bound = 100000;
            } else {
                $upper_bound = 1 / ($median - $min);
                $upper_bound = sprintf("%.3f", $upper_bound);
                $upper_bound = '0' if eval($upper_bound) == 0;
            }
            $bounds->{'upper'}[0] = $upper_bound;
        } elsif ($type eq 'categorical') {
            for (my $i = 0; $i < $ntheta; $i++) {
                if ($linearize and ($fraction != 1)) {
                    my $bound = 1 / (1 - $fraction);
                    $bound = sprintf("%.3f", $bound);
                    $bound = '0' if eval($bound) == 0;
                    $bounds->{'upper'}[$i] = $bound;
                } else {
                    $bounds->{'upper'}[$i] = 5;
                }
            }
        } elsif ($type eq 'hockey-stick') {
            if ($median == $min) {
                croak("the median and min are equal ($min) for covariate, cannot use hockey-stick parameterization.")
            }
            my $upper_bound = 1 / ($median - $min);
            $upper_bound = sprintf("%.3f", $upper_bound);
            $upper_bound = '0' if eval($upper_bound) == 0;
            $bounds->{'upper'}[0] = $upper_bound;
            $bounds->{'upper'}[1] = 100000;
        } elsif ($type eq 'power') {
            $bounds->{'upper'}[0] = 100000;
        } elsif ($type eq 'exponential') {
            $bounds->{'upper'}[0] = $upper_bound;
        } elsif ($type eq 'user') {
            for (my $i = 0; $i < $ntheta; $i++) {
                $bounds->{'upper'}[$i] = 100000;
            }
        } elsif ($type eq 'none') {
            $bounds->{'upper'} = [];
        } else {
            croak("unknown type $type");
        }
    }

    unless (defined $bounds->{'lower'} and defined $bounds->{'lower'}[0]) {
        if ($sum_covariates) {
            for (my $i = 0; $i < $ntheta; $i++) {
                $bounds->{'lower'}[$i] = -20;
            }
        } elsif ($type eq 'linear') {
            my $lower_bound;
            if ($median-$max == 0) {
                $lower_bound = -100000;
            } else {
                $lower_bound = 1 / ($median - $max);
                $lower_bound = sprintf("%.3f", $lower_bound);
                $lower_bound = '0' if eval($lower_bound) == 0;
            }
            $bounds->{'lower'}[0] = $lower_bound;
        } elsif ($type eq 'categorical') {
            for (my $i = 0; $i < $ntheta; $i++) {
                if ($linearize and ($fraction != 1)) {
                    my $bound = (-1 / $fraction);
                    $bound = sprintf("%.3f", $bound);
                    $bound = '0' if eval($bound) == 0;
                    $bounds->{'lower'}[$i] = $bound;
                } else {
                    $bounds->{'lower'}[$i] = -1;
                }
            }
        } elsif ($type eq 'hockey-stick') {
            $bounds->{'lower'}[0] = -100000;
            if ($median == $max) {
                croak("the median and max are equal ($max) for covariate, cannot use hockey-stick parameterization.") ;
            }
            my $lower_bound = 1 / ($median - $max);
            $lower_bound = sprintf("%.3f", $lower_bound);
            $lower_bound = '0' if eval($lower_bound) == 0;
            $bounds->{'lower'}[1] = $lower_bound;
        } elsif ($type eq 'power') {
            $bounds->{'lower'}[0] = -100;
        } elsif ($type eq 'exponential') {
            $bounds->{'lower'}[0] = $lower_bound;
        } elsif ($type eq 'user') {
            for (my $i = 0; $i < $ntheta; $i++) {
                $bounds->{'lower'}[$i] = -100000;
            }
        } elsif ($type eq 'none') {
            $bounds->{'lower'} = [];
        } else {
            croak("unknown type $type");
        }
    }

    for (my $i = 0; $i < $ntheta; $i++) {
        if (not defined $inits->[$i]) {
            my $tmp;
            if (($type eq 'power') or ($type eq 'user')) {
                $tmp = $global_init;
            } elsif ($type eq 'exponential') {
                if ($global_init > $lower_bound and $global_init < $upper_bound) {
                    $tmp = $global_init;
                } else {
                    $tmp = ($upper_bound + $lower_bound) / 2;
                    if ($tmp == 0) {
                        $tmp = $upper_bound / 5;        # Ad hoc
                    }
                }
            } elsif ((abs($bounds->{'upper'}[$i]) >= 100000 or not defined $bounds->{'upper'}[$i]) and
                    (abs($bounds->{'lower'}[$i]) >= 100000 or not defined $bounds->{'lower'}[$i])) {
                $tmp = 100 * $global_init;
            } else {
                if (abs($bounds->{'upper'}[$i]) <= abs($bounds->{'lower'}[$i])) {
                    $tmp = $bounds->{'upper'}[$i] == 0 ? $bounds->{'lower'}[$i] * $global_init : $bounds->{'upper'}[$i] * $global_init;
                } else {
                    $tmp = $bounds->{'lower'}[$i] == 0 ? $bounds->{'upper'}[$i] * $global_init : $bounds->{'lower'}[$i] * $global_init;
                }
            }
            $inits->[$i] = $tmp;
        }
    }
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
        if (defined $chosen_parameter or (defined $chosen_covariate) or (defined $chosen_state)){
            print LOG "Parameter-covariate relation chosen in this $direction step: ",
            "$chosen_parameter-$chosen_covariate-$chosen_state\n";
            print LOG sprintf("%-23s",'CRITERION'),uc( $criterion ),"\n" if (length($criterion)>0);
        }
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
    if (defined $included_relations and (scalar (keys %{$included_relations}) > 0 )) {
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
                print LOG sprintf("%-17s",$cov.'-'.$included_relations -> {$par}{$cov}{'state'});
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

    OUTER:    foreach my $cov ( @all_covariates ) {
        my $used = 0;
        foreach my $do_not_cov ( @do_not_drop ) {
            next OUTER if ( $cov eq $do_not_cov );
        }
        foreach my $used_cov ( @used_covariates ) {
            $used = 1 if ( $cov eq $used_cov );
        }
        if ( $used ) {
            $applicant_model -> _option_val_pos ( problem_numbers  => [1],
                instance_numbers => [[1]],
                name             => $cov,
                record_name      => 'input',
                new_values       => [['']],
                exact_match      => 1 );
        } else {
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

    my $fname = 'final_' . $self->search_direction() . '.mod';
    if ($self->linearize()){
        $fname = 'final_' . $self->search_direction() . '_linear.mod';
    }
    my $fdir = $self->final_model_directory();
    return if (-e "$fdir$fname"); #otherwise may write twice

    ui->print(
        category => 'scm',
        message => "Writing final models from the " . $self->search_direction() . " search."
    );
    $final_model->filename($fname);
    $final_model->directory( $fdir);
    $fname =~ s/\.mod/\.lst/;
    return unless (-e $final_model->outputfile); #unless lst-file exists (could have crashed)
    copy($final_model->outputfile, "$fdir$fname");
    copy(utils::file::replace_extension($final_model->outputfile, 'ext'), utils::file::replace_extension("$fdir$fname", "ext"));     # Also copy the ext file to get better precision on inits
    my $prob_num = undef;
    $final_model->update_inits(
        from_output => $final_model->outputs->[0],
        problem_number => $prob_num,
    );
    $final_model->outputfile("$fdir$fname");
    $final_model->set_outputfile();
    if ($self->linearize()) {
        #set datafilename to something ok
        $final_model->ignore_missing_files(1);
        my $datafilename = 'derivatives_covariates.dta';
        if ($self->update_derivatives()){
            my $stepname = '';
            if ($self->step_number() > 1) {
                $stepname = '_' . ($self->step_number() - 1);
                if ($self->search_direction() eq 'forward') {
                    $stepname .= 'f';
                }else{
                    $stepname .= 'b';
                }
            }
            $datafilename = "derivatives_covariates$stepname.dta";
        }

        my @new_names = ($datafilename) x scalar(@{$final_model->problems});
        $final_model->datafiles(new_names => \@new_names); #one for each $PROB

    }else{
        $final_model->ignore_missing_files(1);
        #ref to all data filenames
        my $datafilenames = $self->models()->[$model_number - 1]->datafiles(absolute_path => 1);
        $final_model->datafiles(new_names => $datafilenames); #one for each $PROB
    }
    $final_model->_write;

    if ($self->linearize()){
        #create final nonlinear model
        my $final_nonlin = model->new(
            %{common_options::restore_options(@common_options::model_options)},
            filename => $self->final_model_directory() . 'original.mod',
            ignore_missing_files => 1
        );
        $final_nonlin->filename('final_' . $self->search_direction() . '_nonlinear.mod');
        #add all included  relations

        my %included_relations;
        %included_relations = %{$self->included_relations} if (defined $self->included_relations);
        foreach my $incl_par (sort keys %included_relations) {
            foreach my $incl_cov (sort keys %{$included_relations{$incl_par}}) {
                $self->add_code(
                    definition_code => $included_relations{$incl_par}{$incl_cov}{'code'},
                    nthetas => $included_relations{$incl_par}{$incl_cov}{'nthetas'},
                    inits => $included_relations{$incl_par}{$incl_cov}{'inits'},
                    bounds => $included_relations{$incl_par}{$incl_cov}{'bounds'},
                    applicant_model => $final_nonlin,
                    sum_covariates => $self->sum_covariates_hash->{$incl_par},
                    parameter => $incl_par,
                    covariate => $incl_cov,
                );
            }
        }
        #update initials, from initial_estimates_model??? from where?
        if ($self->update_derivatives()) {
            my $fb = ($self->search_direction() eq 'forward') ? 'f' : 'b';
            my $outf = 'scm_dir1/derivatives_updated_' . ($self->step_number()) . $fb . '.lst';
            if (-e $outf) {
                $final_nonlin->update_inits(from_output_file => $outf, problem_number => $prob_num);
            } else {
                #print "nothing left to add? could not find $outf\n";
                #we will also end up here if first step and nothing significant. Would liek
                # to keep all estimates from derivatives run in that case, but ahve no way of
                #separating cases right now.
                $outf = 'derivatives_updated_' . ($self->step_number() - 1) . $fb . '.lst';
                $outf = 'derivatives.lst' if ($self->step_number() == 1);
                if (-e $outf) {
                    $final_nonlin->update_inits(
                        from_output_file => $outf,
                        ignore_missing_parameters => 1,
                        problem_number => $prob_num,
                    );
                } else {
                    my $outf2 = 'copy_last_forward_derivatives.lst';
                    if (-e $outf2) {
                        $final_nonlin->update_inits(
                            from_output_file => $outf2,
                            ignore_missing_parameters => 1,
                            problem_number => $prob_num,
                        );
                    }
                }
                $final_nonlin->update_inits(
                    from_model => $final_model,
                    ignore_missing_parameters => 1,
                    problem_number => $prob_num,
                );
            }
        } else {
            #if not update derivatives then first take original derivatives, ignore missing,
            #and then final linear, by labels ,ignore missing
            my $outf = $fdir . '../derivatives.lst';
            if (-e $outf) {
                $final_nonlin->update_inits(
                    from_output_file => $outf,
                    ignore_missing_parameters => 1,
                    problem_number => $prob_num,
                );
            }
            $final_nonlin->update_inits(
                from_model => $final_model,
                ignore_missing_parameters => 1,
                problem_number => $prob_num,
            );
        }
        $final_nonlin->_write();
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


        #increase step index and step_number in subtool as always
        #this stepnumber will be 1 (the default)
        #below is copied from modelfit_analyze, except modelnumber is 1 and all indices 0
        #and base_criteria_values unchanged (wild guess)
        #both directions is set

        my $backward_scm =
        tool::scm ->new( %{common_options::restore_options(@common_options::tool_options)},
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
            step_number          => ($self -> step_number() + 1),
            raw_results_file     => [$self -> raw_results_file ->[0]],
            logfile              => [$self -> logfile ->[0]],
            base_criteria_values => $self->base_criteria_values,
            parent_tool_id       => $self -> tool_id,
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
        next if( $config_option eq 'ofv_forward' );
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
                warn "The valid_states section is defined in the configuration file but ".
                    "no states were defined for continuous covariates. Assuming the default valid states: ".
                    join( ', ',@{$self -> valid_states -> {'continuous'}});
            } else {
                $self -> valid_states -> {'continuous'} = $config_file -> valid_states -> {'continuous'};
            }
            if( not defined $config_file -> valid_states -> {'categorical'} ) {
                warn "The valid_states section is defined in the configuration file but ".
                    "no states were defined for categorical covariates. Assuming the default valid states: ".
                    join( ', ',@{$self -> valid_states -> {'categorical'}});
            } else {
                $self -> valid_states -> {'categorical'} = $config_file -> valid_states -> {'categorical'};
            }
        } elsif( defined $config_file -> $config_option  ){

            # This is the general case where we just copy the option.
            $self -> $config_option($config_file -> $config_option);
        }
    }
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
    my $run_mess = '';
    my @filter_table_header;

    my $only_filter = 0;

    my $time_added=0;
    if( defined $filtered_data_model->problems()->[0] -> inputs and
        defined $filtered_data_model->problems()->[0] -> inputs -> [0] -> options ) {
        my $arr;
        ($arr,$time_added) = $filtered_data_model->problems()->[0] -> inputs -> [0]->get_filter_table_names;
        croak ("found no undropped data column in \$INPUT ") unless (defined $arr);
        croak ("automatic filtering cannot yet handle \$INPUT with DATX but without TIME") if ($time_added);
        @filter_table_header = @{$arr};
    } else {
        croak("Trying to construct table for filtering data".
            " but no headers were found in \$INPUT" );
    }
    foreach my $remove_rec ('simulation','covariance','table','scatter'){
        $filtered_data_model -> remove_records(type => $remove_rec);
    }

    my $use_mdv = should_add_mdv(model=> $filtered_data_model);

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
        @code = @{$filtered_data_model->get_code(record => 'pk')};
        unless ( $#code > 0 ) {
            @code = @{$filtered_data_model->get_code(record => 'pred')};
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
                    if ($line =~ s/\bETA\(([0-9]+)\)//){
                        $etanum = $1;
                    }else{
                        last;
                    }

                    if ($line =~ s/\bETA\(([0-9]+)\)//){
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
        my $mdvstring= 'ID '; #placeholder so that positions are the same
        $mdvstring='MDV ' if ($use_mdv);
        foreach my $par ( keys %parmcovhash ){
            my @tablestrings =();
            push( @{$coderef},"\"  COM($com)=".'ABS(G('.$parmetahash{$par}.',1))' );
            push( @tablestrings, "COM($com)=$par".'RATIO');
            $com++;

            foreach my $cov (@{$parmcovhash{$par}}){
                push( @{$coderef},"\"  COM($com)=$cov".'*ABS(G('.$parmetahash{$par}.',1))' );
                push( @tablestrings, "COM($com)=$par$cov".'NUM');
                $com++;
            }
            $filtered_data_model ->
            add_records( type           => 'table',
                record_strings => [ $mdvstring.'ID '.join( ' ', @tablestrings ).
                    ' DV CIPREDI CIWRESI NOAPPEND NOPRINT ONEHEADER FORMAT=s1PE23.16 FILE='.$par.$timevarfile]);
        }

        $filtered_data_model->set_maxeval_zero();

    } else {
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
        foreach my $remove_rec ('abbreviated','msfi','contr','subroutine','prior','model','tol','infn','omega','pk','aesinitial','aes','des','error','pred','mix','theta','sigma','estimation','nonparametric','level'){
            $filtered_data_model -> remove_records(type => $remove_rec);
        }
        my @predcode = ('Y=THETA(1)+ETA(1)+EPS(1)');
        if ($filtered_data_model->problems->[0]->find_data_column(column_name => 'L2') != -1) {     # Do we have L2?
            my $dummy_name = 'DMY6142';
            model_transformations::rename_column(model => $filtered_data_model, from => 'L2', to => $dummy_name);
            push @predcode, "L2=$dummy_name";
        }

        $filtered_data_model -> add_records(type => 'pred',
            record_strings => \@predcode);

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
                ' NOAPPEND NOPRINT NOTITLE ONEHEADER FILE='.$datafile]);
    }



    $filtered_data_model->_write();
    # run model in data_filtering_dir
    my $filter_fit = tool::modelfit -> new
    ( %{common_options::restore_options(@common_options::tool_options)},
        base_directory => $directory,
        directory      => $directory.'/data_preprocessing_dir/',
        models         => [$filtered_data_model],
        top_tool       => 0);
    ui -> print( category => 'all',
        message  => $run_mess,newline => 1 );

    $filter_fit -> run;


    if (defined $time_varying and scalar(@{$time_varying} > 0)) {
        foreach my $par (keys %parmcovhash) {
            my $ncov = scalar(@{$parmcovhash{$par}});
            open(FILE, "$directory$par$timevarfile") ||
                croak("Could not open $directory$par$timevarfile for reading");
            my @lines = <FILE>;
            close(FILE);
            my @sum_arr = (0) x $ncov;
            my @ave_arr;

            for (my $i = 0; $i < $ncov;$i++) {
                $ave_arr[$i] = [];
            }
            my $prev_id;
            my $ratsum = 0;
            foreach (@lines) {
                chomp;
                if ($use_mdv) {
                    next unless (/^\s*0/); #only use lines with mdv=0
                } else {
                    next if (/^\s*(ID|TAB)/);
                }
                my @vals = split;
                #items in @vals are
                # MDV ID $parRATIO (array over) $par$covNUM

                unless ((not defined $prev_id) or ($vals[1] == $prev_id)) {
                    #found new id, process it

                    for (my $i = 0; $i < $ncov; $i++) {
                        #do not die: if $ratsum is zero it must be that the whole expression is 0
                        if ($ratsum == 0) {
                            push(@{$ave_arr[$i]}, 0);
                        } else {
                            push(@{$ave_arr[$i]}, $sum_arr[$i] / $ratsum);
                        }
                    }
                    #reset
                    @sum_arr = (0) x $ncov;
                    $ratsum = 0;
                }
                #store new
                my $DV = $vals[$ncov + 3];
                my $CIPREDI = $vals[$ncov + 4];
                my $CIWRESI = $vals[$ncov + 5];
                my $W = ($DV - $CIPREDI) / $CIWRESI;
                $ratsum += $vals[2] / $W;
                for (my $i = 0; $i < $ncov; $i++) {
                    $sum_arr[$i] += $vals[$i + 3] / $W;
                }
                $prev_id = $vals[1];
            }

            #process the last one
            #new id, process
            for (my $i = 0; $i < $ncov; $i++) {
                #do not die: if $ratsum is zero it must be that the whole expression is 0
                if ($ratsum == 0) {
                    push(@{$ave_arr[$i]}, 0);
                }else{
                    push(@{$ave_arr[$i]}, $sum_arr[$i] / $ratsum);
                }
            }

            open(LOG, ">>" . $self->logfile->[0]); #model_number -1
            for (my $i = 0; $i < $ncov; $i++) {
                my $cov = $parmcovhash{$par}->[$i];
                my @sorted_array = (sort {$a <=> $b} @{$ave_arr[$i]}); #sort ascending
                my $len = scalar(@sorted_array);
                my $median;
                if ($len % 2) {
                    $median = $sorted_array[($len - 1) / 2];
                } else {
                    $median = ($sorted_array[$len / 2]+$sorted_array[($len - 2) / 2]) / 2;
                }
                $self->medians->{$par . '_' . $cov} = $median;

                my $sum = 0;
                foreach my $val (@sorted_array) {
                    $sum += $val;
                }
                $self->means->{$par . '_' . $cov} = $sum / $len if ($len > 0);
                $median = sprintf("%6.2f", $median);
                my $mean = sprintf("%6.2f", $sum / $len) if ($len > 0);
                unless (math::usable_number($median) and math::usable_number($mean)) {
                    croak("Error in calculation of median: $median and mean $mean not usable numbers");
                }
                print LOG "Time-varying $cov on $par (ETA" . $parmetahash{$par} .
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

sub create_R_plots_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              rplot => { isa => 'rplots', optional => 0 }
        );
    my $rplot = $parm{'rplot'};

    my $cont='';
    if (defined $self->continuous_covariates and scalar(@{$self->continuous_covariates})>0){
        $cont = "'".join("','",@{$self->continuous_covariates})."'";
    }
    my $cat='';
    if (defined $self->categorical_covariates and scalar(@{$self->categorical_covariates})>0){
        $cat = "'".join("','",@{$self->categorical_covariates})."'";
    }

    my %parcov;

    foreach my $par ( sort keys %{$self -> test_relations()} ){
        $parcov{$par}=[];
        foreach my $cov ( @{$self -> test_relations()->{$par}} ){
            push(@{$parcov{$par}},$cov);
        }
    }
    my $parameters = "'".join("','",sort(keys %parcov))."'";
    my $parcovstring = '';
    foreach my $par (sort(keys %parcov)){
        $parcovstring .= ',' if(length($parcovstring)>0); #not first
        $parcovstring .= "'".$par."'=c('".join("','",@{$parcov{$par}})."')";
    }
    my $scm_log_file = $self -> logfile -> [0];
    $scm_log_file =~ s/\\/\\\\/g;
    my $scm_short_log = $self -> short_logfile -> [0];
    $scm_short_log =~ s/\\/\\\\/g;
    $rplot->add_preamble(code => [
                             "scm.log.file <-'".$scm_log_file."'",
                             "scm.short.log   <- '".$scm_short_log."'",
                             'continuous.covariates <- c('.$cont.')',
                             'categorical.covariates <- c('.$cat.')',
                             'parameters <- c('.$parameters.')',
                             'parameters.covariates <- list('.$parcovstring.')'
                         ]);

}

sub _check_interaction
{
    # Check if this model has interaction i.e.
    #   1. It has D_EPSETA non-zero in derivatives data
    #   2. It has INTER on $EST in original model
    my %parm = validated_hash(\@_,
        datafile => { isa => 'Str' },
        model => { isa => 'model' },
    );
    my $datafile = $parm{'datafile'};
    my $model = $parm{'model'};

    return _inter_in_model(datafile => $datafile, model => $model) && _inter_in_est(model => $model);
}

sub _inter_in_model
{
    # Check if a model has interaction in its code
    # this is done by checking if there is D_EPSETA in the dataset
    my %parm = validated_hash(\@_,
        datafile => { isa => 'Str' },
        model => { isa => 'model' },
    );
    my $datafile = $parm{'datafile'};
    my $model = $parm{'model'};

    my $table_file = nmtablefile->new(filename => $datafile);
    my $table = $table_file->tables->[0];

    my $nonzero = 0;
    for (my $col = 0; $col < @{$table->columns}; $col++) {
        if ($table->header_array->[$col] =~ /^D_EPSETA/) {
            $nonzero = array::any_nonzero($table->columns->[$col]);
            last if $nonzero;
        }
    }

    return $nonzero;
}

sub _inter_in_est
{
    # Check if a model has INTER in $EST
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my $inter = $model->is_option_set(record => 'estimation', name => 'INTERACTION', fuzzy_match => 1);

    return $inter;
}

1;
