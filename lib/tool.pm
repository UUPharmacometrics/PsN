package tool;

use include_modules;
use strict;
use Cwd;
use File::Copy 'cp';
use File::Path qw(mkpath rmtree);
use File::Spec;
use Scalar::Util qw(blessed);
use OSspecific;
use Math::Random;
use Archive::Zip;
use utils::file;
use PsN;
use ui;
use Config;
use YAML;
our $AUTOLOAD;

use Mouse;
use MouseX::Params::Validate;

use model;
use rplots;

has 'models' => ( is => 'rw', isa => 'ArrayRef[model]' );
has 'tools' => ( is => 'rw', isa => 'ArrayRef[tool]' );
has 'first_callback' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'adaptive' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'always_datafile_in_nmrun' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'raw_line_structure' => ( is => 'rw', isa => 'Ref' );
has 'check_nmtran' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'last_est_complete' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'add_retries' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'niter_eonly' => ( is => 'rw', isa => 'Int' );
has 'abort_on_fail' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'accepted_ofv_difference' => ( is => 'rw', isa => 'Num', default => 0.5 );
has 'base_directory' => ( is => 'rw' );
has 'clean' => ( is => 'rw', isa => 'Int', default => 1 );
has 'compress' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'cpu_time' => ( is => 'rw', isa => 'Num', default => 120 );
has 'diagnostic_parameters' => ( is => 'rw', isa => 'ArrayRef[Str]', default =>
    sub { ['covariance_step_run','minimization_successful','covariance_step_successful','covariance_step_warnings',
            'estimate_near_boundary','rounding_errors','zero_gradients','final_zero_gradients','hessian_reset','s_matrix_singular',
            'significant_digits','condition_number','est_methods','model_run_time','subprob_est_time','subprob_cov_time'] }  );
has 'directory' => ( is => 'rw' );
has 'grid_batch_size' => ( is => 'rw', isa => 'Int', default => 1 );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub {  ['psn_logfile.csv'] }  );
has 'max_runtime' => ( is => 'rw', isa => 'Str' );
has 'min_retries' => ( is => 'rw', isa => 'Int', default => 0 );
has 'missing_data_token' => ( is => 'rw', isa => 'Str', default => '-99' );
has 'model_number' => ( is => 'rw', isa => 'Int' );
has 'nice' => ( is => 'rw', isa => 'Int', default => 19 );
has 'nm_version' => ( is => 'rw', isa => 'Str', default => 'default' );
has 'prepend_options_to_lst' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'tool_id' => ( is => 'rw', isa => 'Int' );
has 'parent_tool_id' => ( is => 'rw', isa => 'Int' );
has 'parent_threads' => ( is => 'rw', isa => 'Int', default => 1 );
has 'picky' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'prepared_models' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'prepend_model_file_name' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'raw_results' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_results_append' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'raw_results_file' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['raw_results.csv'] } );
has 'raw_results_header' => ( is => 'rw', isa => 'ArrayRef[Str]', clearer => 'clear_raw_results_header', default =>
    sub {  ['model','problem','subproblem','covariance_step_run','minimization_successful','covariance_step_successful',
            'covariance_step_warnings','estimate_near_boundary','rounding_errors','zero_gradients','final_zero_gradients','hessian_reset',
            's_matrix_singular','significant_digits','condition_number','est_methods','nburn_set','burn_in_iter','burn_in_conv','model_run_time',
            'subprob_est_time','subprob_cov_time','ofv','theta','omega','sigma','setheta','seomega','sesigma','shrinkage_eta','shrinkage_iwres','eigen'] }  );
has 'raw_nonp_results_header' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub {  ['model','problem','subproblem','minimization_successful','ofv','npofv','npeta','npomega'] } );
has 'reference_object' => ( is => 'rw' );
has 'results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'psn_results.csv' );
has 'resume' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'retries' => ( is => 'rw', default => 0 );
has 'run_on_lsf' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'run_local' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'run_on_mosix' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'seed' => ( is => 'rw', isa => 'Str', default => int(rand() * 10000000) );
has 'significant_digits_accept' => ( is => 'rw', isa => 'Num', default => 0 );
has 'subtools' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['modelfit'] });
has 'subtool_arguments' => ( is => 'rw', isa => 'HashRef' );
has 'threads' => ( is => 'rw', isa => 'Int', default => 1 );
has 'parafile' => ( is => 'rw', isa => 'Str' );
has 'nodes' => ( is => 'rw', isa => 'Int', default => 0 );
has 'nmfe_options' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'nm_output' => ( is => 'rw', isa => 'Str' );
has 'tool_id' => ( is => 'rw', isa => 'Int' );
has 'tweak_inits' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'degree' => ( is => 'rw', isa => 'Num', default => 0.1 );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has '_raw_results_callback' => ( is => 'rw' );
has 'correlation_limit' => ( is => 'rw', isa => 'Num', default => 0.85 );
has 'condition_number_limit' => ( is => 'rw', isa => 'Num', default => 1000 );
has 'near_bound_sign_digits' => ( is => 'rw', isa => 'Int', default => 2 );
has 'near_zero_boundary_limit' => ( is => 'rw', isa => 'Num', default => 0.01 );
has 'sign_digits_off_diagonals' => ( is => 'rw', isa => 'Int', default => 2 );
has 'large_theta_cv_limit' => ( is => 'rw', isa => 'Num', default => 0.50 );
has 'large_omega_cv_limit' => ( is => 'rw', isa => 'Num', default => 0.8 );
has 'large_sigma_cv_limit' => ( is => 'rw', isa => 'Num', default => 0.8 );
has 'lsf_job_name' => ( is => 'rw', isa => 'Str' );
has 'lsf_project_name' => ( is => 'rw', isa => 'Str' );
has 'lsf_queue' => ( is => 'rw', isa => 'Str' );
has 'lsf_resources' => ( is => 'rw', isa => 'Str' );
has 'lsf_ttl' => ( is => 'rw', isa => 'Str' );
has 'lsf_sleep' => ( is => 'rw', isa => 'Int', default => 3 );
has 'lsf_options' => ( is => 'rw', isa => 'Str' );
has 'grid_poll_interval' => ( is => 'rw', isa => 'Int' );

#this has been turned off in common_options, does not work
has 'nonparametric_etas' => ( is => 'rw', isa => 'Bool', default => 0 );
#this has been turned off in common_options, does not work
has 'nonparametric_marginals' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'shrinkage' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'eigen_values' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'precision' => ( is => 'rw', isa => 'Num' );
has 'quick_summarize' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'handle_crashes' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'handle_msfo' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'raw_nonp_results' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_nonp_file' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['raw_nonparametric.csv'] } );
has 'sge_prepend_flags' => ( is => 'rw', isa => 'Str' );
has 'nmfe' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'top_tool' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'ud_native_retrieve' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'ud_sleep' => ( is => 'rw', isa => 'Int', default => 30 );
has 'expected_run_time' => ( is => 'rw', isa => 'Num', default => 5 );
has 'crash_restarts' => ( is => 'rw', isa => 'Int', default => 4 );
has 'run_on_ud' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'run_on_sge' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'run_on_slurm' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'email_address' => ( is => 'rw', isa => 'Str' );
has 'send_email' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'run_on_zink' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'display_iterations' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'slurm_account' => ( is => 'rw', isa => 'Str' );
has 'slurm_partition' => ( is => 'rw', isa => 'Str' );
has 'slurm_prepend_flags' => ( is => 'rw', isa => 'Str' );
has 'slurm_cluster' => ( is => 'rw', isa => 'Str' );
has 'sge_resource' => ( is => 'rw', isa => 'Str' );
has 'sge_queue' => ( is => 'rw', isa => 'Str' );
has 'torque_prepend_flags' => ( is => 'rw', isa => 'Str' );
has 'torque_queue' => ( is => 'rw', isa => 'Str' );
has 'run_on_torque' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'directory_name_prefix' => ( is => 'rw', isa => 'Str' );
has 'rplots' => ( is => 'rw', isa => 'Int', default => 0 );
has 'template_directory_rplots' => ( is => 'rw', isa => 'Str');
has 'template_file_rplots' => ( is => 'rw', isa => 'Str');
has 'so' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'subset_variable_rplots' => ( is => 'rw', isa => 'Str');
has 'zip' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'rmarkdown' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'model_subdir' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'model_subdir_name' => ( is => 'rw', isa => 'Str' );
has 'metadata' => ( is => 'rw', isa => 'HashRef', default => sub {{}} );     # Complex data structure for metadata of run to be stored as meta.yaml
has 'copy_up' => ( is => 'rw', isa => 'Bool' );     # Set for non top-tools to still copy up results files
has 'debug_rmd' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'html' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'pdf' => ( is => 'rw', isa => 'Bool', default => 0 );   # If both html and pdf are zero set pdf to 1
has 'rprofile' => ( is => 'rw', isa => 'Bool', default => 1 );  # Set to zero if no .Rprofile wanted


sub BUILDARGS
{
    my $self = shift;

    my %hash = @_;

    for my $key (keys %hash) {
        delete $hash{$key} unless defined $hash{$key};
    }

    return $self->SUPER::BUILDARGS(%hash);
}

sub BUILD
{
    my $self  = shift;
    my $parmref = shift;
    my %parm  = %{$parmref};

    # The I<retries>, I<picky>, I<nm_version>, I<seed> and I<threads>
    # attributes can be specified as either a scalar or as an
    # array. The scalar value will be applied to all models whereas
    # the array holds values per model. If an array is given it must
    # be of the same length as the number of models.
    #
    # The I<directory> is the folder where the tools stores
    # temporary data and runs subtools (or in the modelfit case,
    # runs NONMEM). If unsure of what this means, leave it undefined
    # and a default will be used, e.g. modelfit_dir3 or something.
    #
    # The base_directory refers to the directory where the tool
    # should place its own directory. Default is current directory.
    #
    # A more interresting attribute is I<threads> which sets how many
    # parallel executions of NONMEM that will run. Some tips are:
    # Setting the number of threads higher than the number of nodes in
    # your cluster/supercomputer can make your runs slower. The
    # biggest limiting factor is the amount of memory needed by
    # NONMEM. With smaller runs, just set the thread number to the
    # number of nodes available.
    #
    # The I<directory> is the folder where the tools stores
    # temporary data and runs subtools (or in the modelfit case,
    # runs NONMEM). Each NONMEM run will have its own sub directory
    # NM_run[X] where [X] is an index running from 0 to the number of
    # runs-1. If unsure of what this means, leave it undefined and a
    # default will be used, e.g. modelfit_dir3 or something.
    #
    # Next, the I<compress> attribute are good
    # if you want to save some hard disk space. I<compress> set to 1
    # will put all NONMEM output in to an tar/gz archive named
    # I<problem_files.tgz> placed in the I<NM_run[X]> directory
    # described above.
    #
    # I<retries> is the number of times L</run> will alter initial
    # values and (re)execute NONMEM when executions fail. I<retries>
    # can either be an integer, specifying the number of retries for
    # all models, or it can be an array with the number of retries
    # specific for each modelfile as elements. The default value is
    # B<5>. The algorithm for altering the initial values works
    # roughly like this: For each each new try, a random new initial
    # value is drawn from a uniform distribution with limits +-n*10%
    # of the original intial estimate and where n i equal to the retry
    # number. I.e. the first retry, the borders of the distribution
    # are +-10%. The algorithm ensures that the new values are within
    # specified boundaries.
    #
    # =begin html
    #
    # For a full dexcription of the algorithm, see <a
    # href="model/problem/record/init_option.html#set_random_init">set_random_init</a>
    # of the <a
    # href="model/problem/record/init_option.html">init_option
    # class</a>.
    #
    # =end html
    #
    # =begin man
    #
    # For a full dexcription of the algorithm, see I<set_random_init>
    # of the I<init_option> class.
    #
    # =end man
    #
    # If I<picky> is set to 1, the output from NONMEM will be checked
    # more thoroughly. If any of the lines below are found in the
    # minimization message, a rerun is initiated.
    #
    #    ESTIMATE OF THETA IS NEAR THE BOUNDARY AND
    #    PARAMETER ESTIMATE IS NEAR ITS BOUNDARY
    #    R MATRIX ALGORITHMICALLY SINGULAR
    #    S MATRIX ALGORITHMICALLY SINGULAR
    #
    # I<nm_version> is a string with the version number of NONMEM that
    # will be used. The installed versions of NONMEM must be specified
    # in OSspecific.pm, the class responsible for system specific
    # features settings.
    #
    # I<logfile> specifies the name of the logfile.
    #
    # I<extra_files> is an array of strings where each string is a
    # file needed for NONMEM execution. Those file will be moved
    # to the I<NM_run[X]> directory.
    #
    # I<seed> is just a way to set a seed number.
    #
    # If a directory is given as argument to a tool, it will extract
    # all information about what has already been run in this
    # directory and continue there. If nothing is left to do, it will
    # still produce the output as a normal run would. This is useful
    # both for resuming crashed runs as well as for extracting
    # information form an old run.

    if (defined $self->parafile) {
        $self->parafile(File::Spec->rel2abs($self->parafile));
    }

    if ($self->model_subdir and ($self->top_tool or $self->copy_up)) {
        my $model_subdir_name = utils::file::get_file_stem($self->models->[0]->filename) . '/';
        $self->model_subdir_name($model_subdir_name);
    } else {
        $self->model_subdir(0);
    }

    if( $PsN::config -> {'default_options'} -> {'lsf_pfizer'}){
        my $apath  = cwd();
        my $default_jobname;
        if ($apath =~ m/\/(AnalysisStep\d+)/) {
            $default_jobname = 'PsN-'.$1;
        }else {
            $default_jobname = 'PsN-'.time();
        }
        $self->lsf_job_name(defined $parm{'lsf_job_name'} ? $parm{'lsf_job_name'} : $default_jobname) unless defined $self->lsf_job_name;
    }
    $self->seed(defined $parm{'seed'} ? $parm{'seed'} : random_uniform_integer(1,0,10000000));

    #Initiate the random generator if a seed is given (which it is, see above)
    random_set_seed_from_phrase( $self->seed );

    # The base_directory refers to the directory where the tool should place its own
    # directory
    if ( defined $parm{'base_directory'} ) {
        $self->base_directory($parm{'base_directory'});
    } else {
        my ($uniquePath, $file) = OSspecific::absolute_path('', '');
        $self->base_directory($uniquePath);
    }

    # The directory is the folder where the tools stores temporary data and
    # runs subtools (or in the modelfit case, runs NONMEM)
    if ( defined $parm{'directory'} ) {
        my $dummy;
        my $dir;
        ( $dir, $dummy ) = OSspecific::absolute_path( $parm{'directory'}, '');
        $self->directory($dir);
        # Unzip if m1 if zipped
        $self->uncompress_m1();
    } else {
        my $tool_name;
        if (defined $self->directory_name_prefix) {
            $tool_name = $self->directory_name_prefix;
        } else {
            my @tool_name_full = split('::', ref $self);
            $tool_name = $tool_name_full[$#tool_name_full];
        }
        $self->directory(OSspecific::unique_path($tool_name . '_dir', $self->base_directory));
    }
    if (ui->silent() and not defined ui->logfile()){
        ui->logfile($self->directory . 'run_messages.txt');
    }

    # Create my temporary directory
    $self -> _make_dir;

    croak("No model specified!" )
    unless ( defined $self->models and scalar @{$self->models} > 0 );
    foreach my $mod ( @{$self->models} ) {
        croak("Supplied argument model is not defined" )
        unless defined $mod;
    }
    # Make sure that the filenames are absolute and collect model_ids
    my $first =1;
    foreach my $model ( @{$self->models} ) {

        my ($directory, $filename) = OSspecific::absolute_path( $model->directory, $model->filename );
        $model->filename( $filename );
        $model->directory( $directory );
        $filename =~ s/\.[^.]+$//; #remove last dot and extension

        if ( $self->top_tool() and $first  ) {
            $first =0;
            foreach my $attribute ( 'raw_results_file','raw_nonp_file' ) {
                my $name = $self->$attribute->[0];
                unless ($name =~ /$filename/){
                    if ( $name =~ /\./ ) {
                        $name =~ s/\./_$filename\./;
                    } else {
                        $name = $name.'_'.$filename;
                    }
                    $self -> $attribute->[0] = $name;
                }
            }
        }

        if ( defined $model -> outputs ) {
            my @outputs = @{$model -> outputs};
            foreach my $output ( @outputs ) {
                my ($directory, $filename) = OSspecific::absolute_path( $outputs[0] -> directory, $outputs[0] -> filename );
                $output -> filename( $filename );
                $output -> directory( $directory );
            }
        }
    }

    # Use pdf as default.
    if (not $self->html and not $self->pdf) {
        if (blessed($self) eq 'tool::qa') {
            $self->html(1);
        } else {
            $self->pdf(1);
        }
    }
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

sub add_to_nmoutput
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        extensions => {isa => 'ArrayRef', optional => 0}
    );
    my $extensions = $parm{'extensions'};

    if (defined $self->nm_output and length($self->nm_output) > 0) {
        my $old = $self->nm_output;
        foreach my $extension (@{$extensions}) {
            unless ($self->nm_output =~ /$extension/) {
                $old .= ',' . $extension;
            }
        }
        $self->nm_output($old);
    } else {
        $self->nm_output(join(',', @{$extensions}));
    }
}

sub add_model
{
    my ($self, %parm) = validated_hash(@_,
        init_data => {isa => 'Any', optional => 0}
    );
    $self->models([]) unless defined $self->models;
    push( @{$self->models}, model->new ( %{$parm{'init_data'}} ) );
}

sub analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    $self->raw_results([]) unless defined $self->raw_results;
    $self->raw_results->[$model_number - 1] =
      $self->tools->[0]->raw_results if ( defined $self->tools and defined $self->tools->[0] );
    my $sub_analyze = $self->subtools->[0];
    if ( defined $sub_analyze ) {
      $sub_analyze = $sub_analyze.'_analyze';
      if( defined $self -> can( $sub_analyze ) ){
        $self -> $sub_analyze( model_number => $model_number );
      }
    }
}

sub _make_dir
{
    my $self = shift;

    unless ( -e $self->directory ) {
        unless (mkpath($self->directory)) {
            croak "Unable to create directory ".$self->directory.". Aborting\n";
        }
    }
}

sub post_fork_analyze
{
    my $self = shift;

    # Runs the post_fork_analyze specific for the subtool
    if (defined $self->subtools) {
        my $sub_post_fork_analyze = $self->subtools->[0];
        if ( defined $sub_post_fork_analyze ) {
            $sub_post_fork_analyze = $sub_post_fork_analyze.'_post_fork_analyze';
            if ( defined( $self -> can( $sub_post_fork_analyze ) ) ) {
                $self -> $sub_post_fork_analyze;
            }
        }
    }
}

sub post_subtool_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    if (defined $self->subtools) {
        my $sub_analyze = $self->subtools->[0];
        if ( defined $sub_analyze ) {
            $sub_analyze = $sub_analyze.'_post_subtool_analyze';
            if( defined $self -> can( $sub_analyze ) ){
                $self -> $sub_analyze( model_number => $model_number );
            }
        }
    }
}

sub pre_fork_setup
{
    my $self = shift;

    # Runs the pre_fork_setup specific for the subtool
    if (defined $self->subtools) {
        my $sub_pre_fork_setup = $self->subtools->[0];
        if ( defined $sub_pre_fork_setup ) {
            $sub_pre_fork_setup = $sub_pre_fork_setup.'_pre_fork_setup';
            if ( defined( $self -> can( $sub_pre_fork_setup ) ) ) {
                $self -> $sub_pre_fork_setup;
            }
        }
    }
}

sub print_results
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         skip_print => { isa => 'Bool', optional => 1, default => 0 }
    );
    my $skip_print = $parm{'skip_print'};

    if ($self->clean == 5) {        # Highest clean level. Remove the whole run directory
        rmtree([$self->directory]);
        exit;
    }

    # Print results created by 'prepare_results' methods specific to the
    # tools. prepare_results and print_results are usually called from
    # the tool scripts (e.g. bin/bootstrap)

    my $sub_print_results;

    if ( defined $self->subtools and defined $self->subtools->[0] ) {
        $sub_print_results = $self->subtools->[0];
        # Only if we have a subtool, which we allways do, 'modelfit' is as
        # usual the inner tool in the basic case.


        ### get_dim subroutine recurses through arrays of arrays and
        ### returns the number of levels (assumes the same number of
        ### levels in alls subarrays).
        ###
        ### 1st argument is the reference to the toplevel array.
        ### 2nd argument is a starting level.
        ### 3rd argument is an array giving the size of the arrays at each
        ### level (assuming same size arrays at each level)

        sub get_dim {
            my $arr      = shift;
            my $dim      = shift;
            my $size_ref = shift;
            $dim++;
            if ( defined $arr and ref($arr) eq 'ARRAY' ) {
                push( @{$size_ref}, scalar @{$arr} );
                ( $dim, $size_ref ) = get_dim( $arr->[0], $dim, $size_ref );
            }
            return ( $dim, $size_ref );
        }

        ### format_value returns a string for a given number. If the value
        ### is not defined it is returned as NaN or NA, depending on the
        ### output_style configured. Numbers without decimals get 10
        ### digits, Numbers with decimals get 10 digis and 5 decimal
        ### values.

        sub format_value {
            my $val = shift;
            if ( not defined $val or $val eq '' ) {
                return sprintf("%16s",$PsN::out_miss_data).',';
            } else {
                $_ = $val;
                my $nodot = /.*\..*/ ? 0 : 1;
                $_ =~ s/\.//g;
                if ( ($val =~ /^\d+\.\d+\.\d+$/) or /.*\D+.*/ or $nodot) { #version number or non-digit or no decimal point
                    return sprintf("%14s", $val) . ',';
                } else {
                    return sprintf("%14.7g", $val) . ','; #7 value digits
                }
            }
        }

        ### format_label does the same thing as format value, but does not
        ### print out "NA" or "NaN" in case of missing data.

        sub format_label {
            my $val = shift;
            if ( not defined $val or $val eq '' ) {
                return '"",'; #added
            } else {
                $_ = $val;
                my $nodot = /.*\..*/ ? 0 : 1;
                #protect cells with commas
                $val =~ s/\"/\"\"/g;
                $_ =~ s/\.//g;
                if ( /.*\D+.*/ or $nodot) {
                    return '"'.sprintf("%14s",$val).'",';
                } else {
                    return '"'.sprintf("%14.5f",$val).'",';
                }
            }
        }

        ### The main part of the method will loop through the 'own'
        ### results, each element of the 'own' array is a hash with three
        ### keys:
        ###
        ### 'name' of the result, will be used as header (only if
        ### values are defined).
        ###
        ### 'values' either a single value, a list of values or a table of
        ### values.
        ###
        ### 'lables' either a single value(?), a list of values used as
        ### header for the 'values' list or table. It can be a table, Then
        ### the first row will be printed before each row in the values
        ### table, and the second row will be the header.

        croak("No results_file defined" ) unless ( defined $self->results_file );

        $skip_print = 1 if ( defined $self->results and scalar(@{$self->results}) == 1
                             and not defined $self->results->[0]{'own'});

        $skip_print = 1 if ( defined $self->results and scalar(@{$self->results}) == 0);

        #the unless is here to prevent empty file from being produced, especially for mcmp
        unless  ($skip_print ){
            open ( RES, ">" . $self->directory . $self->results_file );
        }


        if ( defined $self->results and (not $skip_print)) {
            my @all_results = @{$self->results};
            no warnings qw(uninitialized); #local
            for ( my $i = 0; $i <= $#all_results; $i++ ) {
                if ( defined $all_results[$i]{'own'} ) {
                    my @my_results = @{$all_results[$i]{'own'}};

                    for ( my $j = 0; $j <= $#my_results; $j++ ) {
                        # These size estimates include the problem and sub_problem dimensions:
                        my ( $ldim, $lsize_ref ) = get_dim( $my_results[$j]{'labels'}, -1, [] );
                        my ( $vdim, $vsize_ref ) = get_dim( $my_results[$j]{'values'}, -1, [] );
                        print RES $my_results[$j]{'name'},"\n" if ( $vdim > 1 );
                        if ( defined $my_results[$j]{'values'} and
                            scalar @{$my_results[$j]{'values'}} >= 0 ) {
                            my @values  = @{$my_results[$j]{'values'}};
                            my @labels;
                            if ( defined $my_results[$j]{'labels'} and
                                scalar @{$my_results[$j]{'labels'}} >= 0 ) {
                                @labels = @{$my_results[$j]{'labels'}};
                            }

                            # Print Header Labels
                            if ( $ldim == 0 ) {
                                my $label = \@labels;
                                print RES '"",'.format_label($label),"\n"; #added
                            } elsif ( $ldim == 2 ) {
                                print RES '"",'; #added
                                for ( my $n = 0; $n < scalar @{$labels[1]}; $n++ ) {
                                    my $label = $labels[1][$n];
                                    print RES format_label($label);
                                }
                                print RES "\n";
                            }

                            # Print the values (with labels on each row if ldim == 2:
                            if ( $vdim == 0 ) {
                                print RES ','.format_value(\@values),"\n";
                            } elsif ( $vdim == 1 ) {
                                for ( my $m = 0; $m < scalar @values; $m++ ) {
                                    my $label = $labels[$m];
                                    print RES ','.format_label($label);
                                    my $val = $values[$m];
                                    print RES ','.format_value($val),"\n";
                                }
                            } elsif ( $vdim == 2 ) {
                                for ( my $m = 0; $m < scalar @values; $m++ ) {
                                    my $label;
                                    if ( $ldim == 1 ) {
                                        $label = $labels[$m];
                                    } elsif ( $ldim == 2 ) {
                                        $label = $labels[0][$m];
                                    }
                                    print RES format_label($label);
                                    if( defined $values[$m] ){
                                        for ( my $n = 0; $n < scalar @{$values[$m]}; $n++ ) {
                                            print RES format_value($values[$m][$n]);
                                        }
                                    }
                                    print RES "\n";
                                }
                            }
                        }
                    }
                }
            }
        }
        close( RES );
    }

    $self->create_R_script() if ($self->top_tool);

    if ($self->clean > 3) {
        my $m1_path = $self->directory . 'm1';
        unlink(<$m1_path/*>);
        rmdir($m1_path);
    }

    if ($self->zip) {
        $self->compress_m1();
    }

    # Add finish time to version_and_option_info.txt
    my @datearr = localtime;
    my $theDate = sprintf "%4.4d-%2.2d-%2.2d",($datearr[5]+1900),($datearr[4]+1),($datearr[3]);
    my $theTime = sprintf "%2.2d:%2.2d:%2.2d",($datearr[2]),($datearr[1]), $datearr[0];
    my $info_line = "Run finished: $theDate at $theTime\n";

    my @content = utils::file::slurp_file($self->directory . 'version_and_option_info.txt');

    open my $fh, '>', $self->directory . "version_and_option_info.txt";
    for my $line (@content) {
        print $fh $line;
        if ($line =~ /^Run started:/) {
            print $fh $info_line;
        }
    }
    close $fh;

    $self->update_meta();
}

sub update_meta
{
    my $self = shift;

    # Update the meta.yaml file after tool finished
    my @datearr = localtime;        # This is duplicated from above. Better solution?
    my $theDate = sprintf "%4.4d-%2.2d-%2.2d",($datearr[5]+1900),($datearr[4]+1),($datearr[3]);
    my $theTime = sprintf "%2.2d:%2.2d:%2.2d",($datearr[2]),($datearr[1]), $datearr[0];

    my $yaml_filename = $self->directory . 'meta.yaml';
    if (-e $yaml_filename) {
        # Procedure to handle differences in legal yaml between Perl and R libs
        open(my $fh, '<', $yaml_filename);
        my $in_cmd = 0;
        my $concat;
        while (my $line = <$fh>) {
            if ($line =~ /^(command_line: .*)\n$/) {
                $concat = $1;
                $in_cmd = 1;
            } elsif ($in_cmd and $line =~ /^  /) {
                $concat .= substr($line, 2, -1);
            } elsif ($in_cmd) {
                $in_cmd = 0;
                $concat .= "\n" . $line;
            } else {
                $concat .= $line;
            }
        }
        if ($concat !~ /^---/) {
            $concat = "---\n" . $concat;
        }
        my $meta_file = YAML::Load($concat);
        for my $key (keys %{$meta_file}) {      # Merge $self->metadata with file prefering $self->metadata
            if (not exists $self->metadata->{$key}) {
                $self->metadata->{$key} = $meta_file->{$key};
            }
        }
        $self->metadata->{'finish_time'} = "$theDate $theTime";
        $self->write_meta();
    }
}

sub compress_m1
{
    my $self = shift;

    my $dir = $self->directory;
    if (-d "${dir}m1") {
        my $zip = Archive::Zip->new();
        $zip->addTree("${dir}m1", "m1");
        map { $_->desiredCompressionLevel(9) } $zip->members();
        if ($zip->writeToFileNamed("${dir}m1.zip") == 0) {
            if (-e "${dir}m1.zip") {
                rmtree(["${dir}m1"]);
            }
        } else {
            # in case of zip fail and zip file was created. Remove it
            if (-e "${dir}m1.zip") {
                unlink "${dir}m1.zip";
            }
        }
    }
}

sub uncompress_m1
{
    my $self = shift;
    if (not -e $self->directory) {
        return;
    }
    my $dir = Cwd::abs_path($self->directory);

    my $zip_name = File::Spec->catfile($dir, 'm1.zip');
    my $m1_name = File::Spec->catfile($dir, 'm1');
    if (-e $zip_name) {
        if (-z $zip_name) {
            unlink($zip_name);
            return;
        }

        if (-d $m1_name) {
            # Check if m1 is empty
            opendir my $dh, $m1_name or die $!;
            my $empty_m1;
            if (not (grep ! /^\.\.?$/, readdir $dh)) {
                $empty_m1 = 1;
            }
            closedir $dh;
            if ($empty_m1) {
                rmdir $m1_name;
            } else {
                croak("Both $zip_name and $m1_name exists. Please remove one of them before restarting.\n");
            }
        }

        my $zip = Archive::Zip->new($zip_name);
        $zip->extractTree(undef, $dir);
        unlink($zip_name);
    }
}

sub read_raw_results
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              model_number => { isa => 'Int', optional => 1 }
        );
    my $model_number = $parm{'model_number'};

    $self->clear_raw_results_header;
    for ( my $i = 1; $i <= scalar @{$self->models}; $i++ ) { # All models
        if ( defined $self->raw_results_file and -e $self->raw_results_file->[$i-1] ) {
            open( RRES, $self->raw_results_file->[$i - 1] );
            my @read_file = <RRES>;
            close( RRES );
            my @file;

            foreach (@read_file){
                chomp;
                if (/\"\,\".*/ ) {
                    s/^\"//;
                    s/\"$//;
                    my @tmp = split('\"\,\"',$_);
                    push (@file,\@tmp);
                } else {
                    #substitute NA with undef, assuming first col not NA
                    #positive lookahead ,NA followed by either , or eol
                    s/,NA(?=(,|$))/,/g;
                    my @tmp = split(',',$_);
                    push (@file,\@tmp);
                }
            }
            $self->raw_results_header([]) unless defined $self->raw_results_header;
            $self->raw_results_header->[$i - 1] = shift @file;
            $self->raw_results([]) unless defined $self->raw_results;
            $self->raw_results->[$i - 1] = \@file;
        }else{
            1;
        }
        if ( defined $self->raw_nonp_file and ref $self->raw_nonp_file eq 'ARRAY' and
             -e $self->raw_nonp_file->[$i - 1] ) {
            open( RRES, $self->raw_nonp_file->[$i - 1] );
            my @file = <RRES>;
            close( RRES );
            map { chomp; my @tmp = split(',',$_); $_ = \@tmp } @file ;
            $self -> {'raw_nonp_results'} -> [$i-1] = \@file;
        }
    }
}

# results structure:

# {results}
#      |
#      |->[0]                                First model
#      |   |
#      |   |->{own}                          The results from this tool on the first model
#      |   |    |
#      |   |    |->[0]
#      |   |    |   |
#      |   |    |   |->{name}                    e.g. 'parameter.estimates'
#      |   |    |   |
#      |   |    |   |->{labels}
#      |   |    |   |     |
#      |   |    |   |     |->[0]...              e.g. ['TH1', 'TH2', 'TH3'] indexed on problem and sub problem
#      |   |    |   |     |->[1]
#      |   |    |   |     |...
#      |   |    |   |     |->[#problems]
#      |   |    |   |
#      |   |    |   |->{values}
#      |   |    |         |
#      |   |    |         |->[0]                 e.g. [0.21, 20.3, 3] indexed as above
#      |   |    |         |->[1]
#      |   |    |         |...
#      |   |    |         |->[#problems]
#      |   |    |
#      |   |    |->[1]
#      |   |    |   |
#      |   |    |   |->{name}                    e.g. 'standard.errors'
#      |   |    |   |->{labels}
#      |   |    |   |->{values}
#      |   |->{subtools}                   The results from the subtools on the first model
#      |           |
#      |           |->[0]                    First sub tool
#      |           |   |
#      |           |   |->[0]                First model of the prepared models sent to the first sub tool
#      |           |   |   |
#      |           |   |   |->{own}          The first sub tools results on the first model
#      |           |   |   |    |
#      |           |   |   |    |->[0]       First result type
#      |           |   |   |    |   |
#      |           |   |   |    |   |->{name}
#      |           |   |   |    |   |->{labels}
#      |           |   |   |    |   |->{values}
#      |           |   |   |    |
#      |           |   |   |    |->[1]       Second result type
#      |           |   |   |    |   |
#      |           |   |   |    |   |->{name}
#      |           |   |   |    |   |->{labels}
#      |           |   |   |    |   |->{values}
#      |           |   |   |->{subtools}   Another tool level
#      |           |   |   |      ...
#      |           |   |->[1]                Second model of the prepared models sent to the first sub tool
#      |           |   |   |
#      |           |   |   |->{own}          The first sub tools results on the second model
#      |           |   |   |    |
#      |           |   |   |    |->[0]       First result type
#      |           |   |   |    |   |
#      |           |   |   |    |   |->{name}
#      |           |   |   |    |   |->{labels}
#      |           |   |   |    |   |->{values}
#      |           |   |   |    |
#      |           |   |   |    |->[1]       Second result type
#      |           |   |   |    |   |
#      |           |   |   |    |   |->{name}
#      |           |   |   |    |   |->{labels}
#      |           |   |   |    |   |->{values}
#      |           |   |   |->{subtools}   Another tool level
#      |           |   |   |      ...
#      |           |   |   |...
#      |           |   |->[#prepared models] Last model of the prepared models sent to the first sub tool
#      |           |   |   |
#      |           |   |   |->{own}          The first sub tools results on the last model
#      |           |   |   |    |
#      |           |   |   |    |->[0]       First result type
#      |           |   |   |    |   |
#      |           |   |   |    |   |->{name}
#      |           |   |   |    |   |->{labels}
#      |           |   |   |    |   |->{values}
#      |           |   |   |    |
#      |           |   |   |    |->[1]       Second result type
#      |           |   |   |    |   |
#      |           |   |   |    |   |->{name}
#      |           |   |   |    |   |->{labels}
#      |           |   |   |    |   |->{values}
#      |           |   |   |->{subtools}   Another tool level
#      |           |   |   |      ...
#      |           |->[1]                    Second sub tool
#      |           |...
#      |           |->[#tools]             Last sub tool
#      |
#      |->[1]                                Second model. All above repeated for this model.
#      |...
#      |->[#models]                          Last model. As above.

# Prepared_models structure:

# {prepared_models}
#      |
#      |->[0]                                First model
#      |   |
#      |   |->{own}                          The prepared models of this tool using the first model as base
#      |   |    |
#      |   |    |->[0]                       First prep model
#      |   |    |->[1]                       Second prep model
#      |   |    |...
#      |   |    |->[#prep_models]            Last prep model
#      |   |
#      |   |->{subtools}                   The prepared models of the subtools on the first model. Only one sub tool per prepared model above.
#      |           |
#      |           |->[0]                    First model of the models (prepared above) sent to the first sub tool
#      |           |   |
#      |           |   |->{own}              The first sub tools prepared models on its first model
#      |           |   |    |
#      |           |   |    |->[0]           First prep model
#      |           |   |    |->[1]           Second prep model
#      |           |   |    |...
#      |           |   |    |->[#prep_models]Last prep model
#      |           |   |
#      |           |   |->{subtools}
#      |           |
#      |           |->[1]                    Second model of the models (prepared above) sent to the first sub tool
#      |           |   |
#      |           |   |->{own}              The first sub tools prepared models on its second model
#      |           |   |    |
#      |           |   |    |->[0]           First prep model
#      |           |   |    |->[1]           Second prep model
#      |           |   |    |...
#      |           |   |    |->[#prep_models]Last prep model
#      |           |   |
#      |           |   |->{subtools}
#      |           |

sub run
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my @results;
    my @prepared_models;

    my $return_dir = getcwd();
    unless (chdir( $self->directory )){
        croak("tool->run failed to chdir to self->directory ".$self->directory);
    }

    #even if there is never any forking over tools, there is code in pre_fork_setup that we keep.
    $self->pre_fork_setup;

    #this tells the tool which model it is working on. In practice this is always the same
    $self -> model_number(1);
    # This is only for backwards compatibility, otherwise we should not need to reset the seed.
    random_set_seed_from_phrase(random_uniform_integer(1,0,10000));

    # First, run setup
    $self -> setup( model_number => 1 );

    # Run the subtools
    my @tool_results = ();
    my @tool_models = ();
    if ( defined $self->tools ) {
        foreach my $tool (@{$self->tools}) {
            # There is to date (2004-01-27 no tool that creates more than one internal
            # tool. Hence this is a loop of one cycle. But to be general, again...
            # Run the tool:
            my( $returns, $prep_models ) = $tool -> run;
            # push the sub tool's return values
            push ( @tool_results, $returns );
            if ( defined $prep_models ) {
                push ( @tool_models, $prep_models );
            }
            $self -> post_subtool_analyze;
        }

    }

    $self->results->[0]{'subtools'} = \@tool_results;
    $self->prepared_models->[0]{'subtools'} = \@tool_models;

    # Analyze the results
    $self -> analyze( model_number => 1 );

    # Perform analyses that need to be done after model has
    # been run and processed. Also write a result file if one is
    # defined.
    $self -> post_fork_analyze;

    unless(chdir($return_dir)){
        croak("tool failed chdir to return_dir $return_dir");
    }

    # Remove all NM_run and m folders for non-top_tools
    if( $self->clean >= 3 and not $self->top_tool ) {
        my $top_dir = $self->directory;
        foreach my $dir ( <$top_dir/m*> ){
            if( $dir =~ /m[0123456789]+$/ ){
                unlink( <$dir/*> );
                rmdir( $dir );
            }
        }
        my @NM_runs=<$top_dir/NM_run*>;
        unless (scalar(@NM_runs)>0){
            #kkep if error made NM_run stay
            my $dir = $self->directory;
            unlink( <$dir/*> );
            rmdir( $dir );
        }
    }

    return \@results ,\@prepared_models;
}

sub setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    $self -> _prepare_model( model_number => $model_number );

    # Run the setup specific for the subtool
    if (defined $self->subtools) {
        my $sub_setup = $self->subtools->[0];
        if ( defined $sub_setup ) {
            $sub_setup = $sub_setup.'_setup';
            $self -> $sub_setup( model_number => $model_number );
        }
    }
}

sub harvest_output
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        search_models => { isa => 'Bool', default => 0, optional => 1 },
        search_output => { isa => 'Bool', default => 0, optional => 1 },
        search_subtools => { isa => 'Bool', default => 0, optional => 1 },
        search_original_models => { isa => 'Bool', default => 0, optional => 1 },
        accessor_parameters => { isa => 'HashRef', optional => 1 },
        accessors => { isa => 'ArrayRef[Str]', optional => 0 }
    );
    my $search_models = $parm{'search_models'};
    my $search_output = $parm{'search_output'};
    my $search_subtools = $parm{'search_subtools'};
    my $search_original_models = $parm{'search_original_models'};
    my %accessor_parameters = defined $parm{'accessor_parameters'} ? %{$parm{'accessor_parameters'}} : ();
    my @accessors = defined $parm{'accessors'} ? @{$parm{'accessors'}} : ();
    my %result;

    # harvest_output is a complement to AUTOLOAD below. AUTOLOAD is
    # currently used to find the AUTOLOAD:ed accessor in any
    # existing subtool, model, data or outputobject. It is
    # inefficient in that it will have to be called for once for
    # each accessor. harvest_output will take a list of accessors
    # that it will search for in each object, saving time and
    # foremost; memory. Also it will take arguments such as
    # "search_models", "search_subtools" that will make things more
    # efficient if you know where to search.

    unless( $search_models + $search_output <= 1 ){
        croak("This is a PsN bug: Only one of the 'search_' options can and must be specified.".
            "\t search_models: $search_models\n".
            "\t search_output: $search_output");
    }

    if (not $search_subtools) {

        sub models_traverse2 {
            my %parameters = @_;
            my @models = $parameters{'models'} ? @{$parameters{'models'}} : ();
            my $search_models = $parameters{'search_models'};
            my $search_output = $parameters{'search_output'};
            my $accessor_parameters = $parameters{'accessor_parameters'};
            my $accessors = $parameters{'accessors'};
            my %results;

            for( my $i = 0; $i < scalar (@models); $i++ ){

                foreach my $model ( @{$models[$i]{'own'}} ) {

                    foreach my $accessor( @{$accessors} ) {

                        if( $search_models and $model -> can( $accessor ) ) {
                            push( @{$results{$accessor}[$i]{'own'}}, $model -> $accessor( %{$accessor_parameters} ) );

                        } elsif( $search_output and $model -> outputs -> [0] -> can( $accessor ) ) {
                            push( @{$results{$accessor}[$i]{'own'}}, $model -> outputs -> [0] -> $accessor( %{$accessor_parameters} ) );

                        } else {
                            croak("Neither model, output have a method for $accessor" );
                        }

                        if ( defined $models[$i]{'subtools'} ) {
                            push( @{$results{$accessor}[$i]{'subtools'}}, models_traverse2( models => $models[$i]{'subtools'} ) );
                        }
                    }

                    if( $search_output ){
                        $model -> outputs -> [0] -> flush();
                    }
                }

            }
            return \%results;
        }
    }

    my @models;

    if ( $search_original_models ) {
        @models = @{$self->models};
    } elsif ( defined $self->prepared_models ) {
        @models = @{$self->prepared_models};
    } else {
        return {};
    }

    %result = %{models_traverse2( models => \@models,
        search_models => $search_models,
        search_output => $search_output,
        accessor_parameters => \%accessor_parameters,
        accessors => \@accessors )};

    return \%result;
}

sub create_raw_results_rows
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 1 },
        label_model => { isa => 'model', optional => 1 },
        raw_line_structure => { isa => 'Ref', optional => 1 },
        max_hash => { isa => 'Ref', optional => 1 },
        model_number => { isa => 'Int', default => 0, optional => 1 },
        eta_shrinkage_file => { isa => 'Maybe[Str]', optional => 1 },
        iwres_shrinkage_file => { isa => 'Maybe[Str]', optional => 1 }
    );
    my $model = $parm{'model'};
    my $label_model = $parm{'label_model'};
    my $raw_line_structure = $parm{'raw_line_structure'};
    my $max_hash = $parm{'max_hash'};
    my $model_number = $parm{'model_number'};
    my $eta_shrinkage_file = $parm{'eta_shrinkage_file'};
    my $iwres_shrinkage_file = $parm{'iwres_shrinkage_file'};
    my @return_rows;
    my @nonp_return_rows;

    #note that local variable hash raw_line_structure is different from attribute raw_line_structure
    #do this also for unsuccessful models
    unless( $model -> outputs -> [0] -> parsed ){
        $model -> outputs -> [0] -> _read_problems;
    }
    my $data_stored = 0;
    my @probs;
    my $np = 0;
    my $model_row = 0;
    if ($model -> outputs -> [0] -> parsed_successfully) {
        @probs = @{$model -> outputs -> [0] -> problem_structure};
        $np = scalar @probs; # #probs
        # ------------  Push model, problem and sub-problem numbers  --------------

        for( my $j = 0; $j < $np; $j++ ) {
            my $ns = $probs[$j]; # #subprobs
            for( my $k = 0; $k < $ns; $k++ ) {
                $data_stored=1;
                my $row = $model_row++;
                push( @{$return_rows[$row]}, ($model_number,($j+1),($k+1)) );
                push( @{$nonp_return_rows[$row]}, ($model_number,($j+1),($k+1)) );
            }
        }
    }

    if( $data_stored ){
        $raw_line_structure -> {$model_number} -> {'model'} = "0,1";
        $raw_line_structure -> {$model_number} -> {'problem'} = "1,1";
        $raw_line_structure -> {$model_number} -> {'subproblem'} = "2,1";

        # ------------  Push model, problem and sub-problem numbers  --------------


        # ---------------------  Loop all result categories  ----------------------

        my $saem = 0;
        my $bayes = 0;
        $self->raw_results_header([]) unless defined $self->raw_results_header;
        foreach my $category ( @{$self->raw_results_header} ){
            next if( $category eq 'model' or $category eq 'problem' or $category eq 'subproblem' or $category eq 'method' );
            my ( $accessor, $res );

            # Get the values for the category

            if ( $category eq 'theta' or $category eq 'omega' or $category eq 'sigma' or
                $category eq 'setheta' or $category eq 'seomega' or $category eq 'sesigma' ) {
                #get_values_to_labels does matching on labels instead of array position, safer
                #for matrices
                #however, the printed header is for first problem, not sure it matches the
                #next.  If eg CL is different coords for second problem, the header will be wrong,
                #old problem not connected to get_values_to_labels

                #problem with handle_maxevals, change model where omega theta sigma removed.
                $res = $model -> get_values_to_labels(category => $category,
                    label_model => $label_model);

            }elsif ( $category eq 'eigen' ) {
                $accessor = $category.'s';
                $res = $model->outputs->[0]->$accessor;
            }elsif ( $category eq 'est_methods' ) {
                #array over $PROB
                my @arr=();
                for (my $i=0;$i< scalar(@{$model ->problems()}); $i++){
                    #get ref of array of methods
                    my $methref = $model -> get_option_value(record_name => 'estimation',
                        option_name => 'METHOD',
                        problem_index => $i, record_index => 'all');

                    my $eonlyref = $model -> get_option_value(record_name => 'estimation',
                        option_name => 'EONLY',
                        problem_index => $i, record_index => 'all');
                    my @string_arr;
                    for (my $j=0; $j< scalar(@{$methref}); $j++){
                        my $methstring;
                        if (defined $methref->[$j]){
                            if ($methref->[$j] eq '1' or $methref->[$j] eq 'COND' or
                                (index('COND', $methref->[$j]) == 0)){
                                if( $model-> is_option_set( record => 'estimation', name => 'LAPLACE',
                                        record_number => ($j+1),fuzzy_match =>1) or
                                    $model-> is_option_set( record => 'estimation', name => 'LAPLACIAN',
                                        record_number => ($j+1),
                                        fuzzy_match =>1)){
                                    $methstring = 'LAPLACE';
                                }else{
                                    $methstring = 'FOCE';
                                }
                            }elsif ($methref->[$j] eq '0' or $methref->[$j] eq 'ZERO' or
                                (index('ZERO', $methref->[$j]) == 0)){
                                $methstring ='FO';
                            }elsif (defined $eonlyref->[$j] and $eonlyref->[$j] == 1){
                                $methstring = $methref->[$j].'*';
                            }else{
                                $methstring = $methref->[$j];
                            }
                        }else{
                            $methstring ='FO'; #default
                        }
                        if ($model -> is_option_set(record => 'estimation',
                                name => 'INTERACTION',
                                problem_number => ($i+1),
                                record_number => ($j+1),
                                fuzzy_match => 1)){
                            $methstring .= '_I';
                        }
                        push(@string_arr,$methstring);
                        last unless ($PsN::nm_major_version >= 7);
                    }
                    push(@arr,join('-',@string_arr));
                    if(defined $string_arr[$#string_arr]){
                        $saem = 1 if ( $string_arr[$#string_arr] eq 'SAEM' or
                                       (index('SAEM',$string_arr[$#string_arr])==0));
                        $bayes = 1 if ($string_arr[$#string_arr] eq 'BAYES' or
                                       (index('BAYES',$string_arr[$#string_arr])==0));
                    }
                }
                $res = \@arr;
            }elsif ( $category eq 'nburn_set' ) {
                if ($saem or $bayes){
                    my @arr=();
                    for (my $i=0;$i< scalar(@{$model ->problems()}); $i++){
                        my $nburnref = $model -> get_option_value(record_name => 'estimation',
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
            }elsif ( $category eq 'burn_in_iter' ) {
                if ($saem or $bayes){
                    $accessor = 'burn_in_iterations';
                    $res = $model->outputs->[0]->$accessor;
                }else{
                    $res = undef;
                }
            }elsif ( $category eq 'burn_in_conv' ) {
                if ($saem or $bayes){
                    $accessor = 'burn_in_convergence';
                    $res = $model->outputs->[0]->$accessor;
                }else{
                    $res = undef;
                }
            }elsif ( $category eq 'subprob_est_time' ) {
                if ($PsN::nm_major_version >= 7){
                    $accessor = 'sum_estimation_time';
                    $res = $model->outputs->[0]->$accessor;
                }else{
                    $res = undef;
                }
            }elsif ( $category eq 'model_run_time' ) {
                if ($PsN::nm_major_version >= 7){
                    #this is a scalar string
                    $res = $model->outputs->[0]->runtime();
                }else{
                    $res = undef;
                }
            }elsif ( $category eq 'subprob_cov_time' ) {
                if ($PsN::nm_major_version >= 7){
                    $accessor = 'sum_covariance_time';
                    $res = $model->outputs->[0]->$accessor;
                }else{
                    $res = undef;
                }
            } elsif ( $category eq 'shrinkage_eta' ) {
                #    Shrinkage in NM 7.1 is incorrect
                # Shrinkage does not work for subproblems right now.
                # compute shrinkage
                $res = $model -> eta_shrinkage(eta_filename => $eta_shrinkage_file);
            } elsif ( $category eq 'shrinkage_iwres' ) {
                # Shrinkage does not work for subproblems right now.
                $res = $model -> iwres_shrinkage(iwres_filename => $iwres_shrinkage_file);
            } else {
                #covariance_step_run comes here
                $accessor = $category;
                $res = $model->outputs->[0]->$accessor;
            }

            # Create entry in raw_line_structure
            my $added_entry=0;
            if( defined $res){
                if ( ref $res eq 'ARRAY' ){
                    my $prob_num = 0;
                    foreach my $prob ( @{$res} ){ #over $j
                        if( defined $prob){
                            if (ref $prob eq 'ARRAY' ){
                                if( defined $prob -> [0] and ref $prob -> [0] eq 'ARRAY' and
                                    defined $return_rows[$prob_num] ){
                                    # The last check in the IF above could be put there to
                                    # avoid a bug. If "output::problem_structure" is
                                    # correct and output::accessor is correct,
                                    # $return_rows[$prob_num] should allways be
                                    # defined. TODO

                                    my $tmp = scalar @{$return_rows[$prob_num]} . ",". scalar @{$prob -> [0]};
                                    $raw_line_structure -> {$model_number} -> { $category } = $tmp;
                                    $added_entry=1;

                                } elsif( defined $prob -> [0] and defined $return_rows[$prob_num]) {
                                    my $tmp = scalar @{$return_rows[$prob_num]} . ",1";
                                    $raw_line_structure -> {$model_number} -> { $category } = $tmp;
                                    $added_entry=1;
                                }
                            }elsif (defined $return_rows[$prob_num]){
                                #covariance_step_run comes here
                                my $tmp = scalar @{$return_rows[$prob_num]} . ",1";
                                $raw_line_structure -> {$model_number} -> { $category } = $tmp;
                                $added_entry=1;
                            }
                        }
                        $prob_num++;
                    }
                }else { # defined but not ref eq 'array'
                    for( my $j = 0; $j < $np; $j++ ) {
                        #res is scalar string
                        my $tmp = scalar @{$return_rows[$j]} . ",1";
                        $raw_line_structure -> {$model_number} -> { $category } = $tmp;
                        $added_entry=1;
                    }
                }
            }

            unless ($added_entry){
                #push to structure for undefs
                my $tmp = scalar @{$return_rows[0]} . ",". $max_hash -> {$category};
                $raw_line_structure -> {$model_number} -> { $category } = $tmp;
            }

            # Get the values for the category
            my $return_array_ref = \@return_rows;

            my $model_row = 0; # Need to mask previous definition of model_row

            if( defined $res ) {
                for( my $j = 0; $j < $np; $j++ ) {
                    my $ns = $probs[$j]; # #subprobs
                    if( not ref $res eq 'ARRAY' ){
                        for( my $k = 0; $k < $ns; $k++ ) {
                            my $row = $model_row++;
                            push( @{$return_array_ref -> [$row]},
                                ($res) x $max_hash -> {$category}  );
                        }
                    }else {
                        if( defined $res -> [$j] ) {
                            for( my $k = 0; $k < $ns; $k++ ) {
                                my $row = $model_row++;
                                if( ref $res -> [$j] eq 'ARRAY' ){
                                    if( defined $res -> [$j][$k] ) {
                                        if ( ref $res -> [$j][$k] eq 'ARRAY' ) {
                                            #FIXME count can be negative, probably error in max_hash
                                            my $undefcount = $max_hash->{$category} - scalar(@{$res -> [$j][$k]});
                                            push( @{$return_array_ref -> [$row]}, @{$res -> [$j][$k]} );
                                            push( @{$return_array_ref -> [$row]},(undef) x $undefcount  ) if ($undefcount >0);
                                        } else {
                                            push( @{$return_array_ref -> [$row]}, $res -> [$j][$k] );
                                        }
                                    } else {
                                        push( @{$return_array_ref -> [$row]},
                                            (undef) x $max_hash -> {$category}  );
                                    }
                                } else {
                                    push( @{$return_array_ref -> [$row]},
                                        $res -> [$j] );
                                }
                            }
                        } else {

                            # Push undefs for missing subprobs
                            for( my $k = 0; $k < $ns; $k++ ) {
                                my $row = $model_row++;
                                push( @{$return_array_ref -> [$row]},
                                    (undef) x $max_hash -> {$category}  );
                            }
                        }
                    }
                }
            } else {
                # Push undefs for missing probs/subprobs
                for( my $j = 0; $j < $np; $j++ ) {
                    my $ns = $probs[$j]; # #subprobs
                    for( my $k = 0; $k < $ns; $k++ ) {
                        my $row = $model_row++;
                        push( @{$return_array_ref -> [$row]},
                            (undef) x $max_hash -> {$category}  );
                    }
                }
            }

        } #end foreach category

        $raw_line_structure -> {$model_number} -> {'line_numbers'} = scalar @return_rows;

        #start nonp
        foreach my $category ( @{$self->raw_nonp_results_header}) {
            next if( $category eq 'model' or $category eq 'problem' or $category eq 'subproblem');
            my ( $accessor, $res );

            # Get the values for the category

            if ( $category eq 'npomega' ) {
                $accessor = $category.'s';
                $res = $model->outputs->[0]->$accessor;
            }elsif ( $category eq 'npeta') {
                $accessor = $category.'bars';
                $res = $model->outputs->[0]->$accessor;
            } else {
                $accessor = $category; #npofv
                $res = $model->outputs->[0]->$accessor;
            }

            # Create entry in nonp_raw_line_structure
            # Not done, no return param nonp_raw_line_structure yet
            if( defined $res and ref $res eq 'ARRAY'){
                my $prob_num = 0;
                foreach my $prob ( @{$res} ){
                    if( defined $prob and ref $prob eq 'ARRAY' ){
                        if( defined $prob -> [0] and ref $prob -> [0] eq 'ARRAY' and
                            defined $nonp_return_rows[$prob_num] ){

                            # The last check in the IF above could be put there to
                            # avoid a bug. If "output::problem_structure" is
                            # correct and output::accessor is correct,
                            # $return_rows[$prob_num] should allways be
                            # defined. TODO

                            my $tmp = scalar @{$nonp_return_rows[$prob_num]} . ",". scalar @{$prob -> [0]};

                        } elsif( defined $prob -> [0] and defined $nonp_return_rows[$prob_num]) {
                            my $tmp = scalar @{$nonp_return_rows[$prob_num]} . ",1";
                        }
                    }
                    $prob_num++;
                }
            }

            # Get the values for the category
            my $return_array_ref = \@nonp_return_rows;
            my $model_row = 0; # Need to mask previous definition of model_row
            no warnings qw(uninitialized);
            if( defined $res ) {
                for( my $j = 0; $j < $np; $j++ ) {
                    my $ns = $probs[$j]; # #subprobs
                    if( defined $res -> [$j] ) {
                        for( my $k = 0; $k < $ns; $k++ ) {
                            my $row = $model_row++;
                            if( ref $res -> [$j] eq 'ARRAY' ){
                                if( defined $res -> [$j][$k] ) {
                                    if ( ref $res -> [$j][$k] eq 'ARRAY' ) {
                                        push( @{$return_array_ref -> [$row]}, @{$res -> [$j][$k]} );
                                        my $n = $max_hash->{$category} - scalar(@{$res->[$j][$k]});
                                        push( @{$return_array_ref -> [$row]},
                                            (undef) x ($max_hash -> {$category} - scalar @{$res -> [$j][$k]})  ) if ($n > 0);
                                    } else {
                                        push( @{$return_array_ref -> [$row]}, $res -> [$j][$k] );
                                    }
                                } else {
                                    push( @{$return_array_ref -> [$row]},
                                        (undef) x $max_hash -> {$category}  );
                                }
                            } else {
                                push( @{$return_array_ref -> [$row]},
                                    $res -> [$j] );
                            }
                        }
                    } else {

                        # Push undefs for missing subprobs
                        for( my $k = 0; $k < $ns; $k++ ) {
                            my $row = $model_row++;
                            push( @{$return_array_ref -> [$row]},
                                (undef) x $max_hash -> {$category}  );
                        }
                    }
                }
            } else {

                # Push undefs for missing probs/subprobs
                for( my $j = 0; $j < $np; $j++ ) {
                    my $ns = $probs[$j]; # #subprobs
                    for( my $k = 0; $k < $ns; $k++ ) {
                        my $row = $model_row++;
                        push( @{$return_array_ref -> [$row]},
                            (undef) x $max_hash -> {$category}  );
                    }
                }
            }

        }
        #end nonp

    } else {
        # not $model->outputs->[0]->parsed_successfully.
        # or not data_stored (0 problems or 0 subproblems in all problems)

        my $mes = "Could not parse the output file: ".
        $model->outputs->[0]->filename;
        push(@{$return_rows[0]}, ($model_number,(1),(1),($mes)) );
        push(@{$nonp_return_rows[0]}, ($model_number,(1),(1),($mes)) );
        $raw_line_structure -> {$model_number} -> {'line_numbers'} = scalar @return_rows;
    }

    return \@return_rows ,\@nonp_return_rows;
}

sub _prepare_model
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    my ($newdir, $newfile) = OSspecific::absolute_path( $self->directory .  '/m'.$model_number, '' );
    unless (-e $newdir){
        unless(mkdir( $newdir )){
            croak("_prepare_model failed to create $newdir : $!");
        }
    }
}

sub print_options
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        toolname => { isa => 'Str', optional => 0 },
        cmd_line => { isa => 'Str', optional => 1 },
        directory => { isa => 'Str', optional => 1 },
        local_options => { isa =>'Ref', optional => 1 },
        common_options => { isa => 'Ref', optional => 1 }
    );
    my $toolname = $parm{'toolname'};
    my $cmd_line = $parm{'cmd_line'};
    my $directory = $parm{'directory'};
    my $local_options = $parm{'local_options'};
    my $common_options = $parm{'common_options'};

    my $dir = (defined $directory)? $directory : $self -> directory;
    my $option_file = $dir . "/version_and_option_info.txt";

    if (( -e $dir."/command.txt" ) and not ( -e $dir."/original_command.txt" )){
        #first restart
        cp($dir . "/command.txt",$dir."/original_command.txt");
        if (-e $option_file ){
            cp($option_file,$dir."/original_version_and_option_info.txt");
        }
    }

    #append
    if ($cmd_line) {
        open(CMD, ">>", $dir . "/command.txt");
        print CMD $cmd_line, "\n";
        close(CMD);
        $self->metadata->{'command_line'} = $cmd_line;
        $cmd_line =~ /(.*?)\s+/;
        my $tool_name = $1;
        (undef, undef, $tool_name) = File::Spec->splitpath($tool_name);
        my @a = split /-/, $tool_name;
        $self->metadata->{'tool_name'} = $a[0];
    }

    open(CMD, "> ", $option_file);
    my @datearr=localtime;
    my $theDate=sprintf "%4.4d-%2.2d-%2.2d",($datearr[5]+1900),($datearr[4]+1),($datearr[3]);
    my $theTime=sprintf "%2.2d:%2.2d:%2.2d",($datearr[2]),($datearr[1]), $datearr[0];
    my $info_line = "PsN version: ".$PsN::version."\nRun started: $theDate at $theTime\n";
    $self->metadata->{'PsN_version'} = $PsN::version;
    $self->metadata->{'start_time'} = "$theDate $theTime";
    print CMD "$info_line";
    print CMD "version_and_option_info.txt is overwitten if the run is restarted later using option -directory.\n";
    print CMD "The original file from the first call is saved as original_version_and_option_info.txt.\n\n";

    if ($cmd_line) {
        print CMD "Command:\n" . $cmd_line . "\n\n";
    }

    PsN::set_nonmem_info($self->nm_version);
    print CMD "NONMEM:\n" . $PsN::nmdir . "\n\n";
    $self->metadata->{'NONMEM_directory'} = $PsN::nmdir;
    $self->metadata->{'NONMEM_version'} = $PsN::nm_major_version . "." . $PsN::nm_minor_version;

    $self->metadata->{'tool_options'} = {};
    # Don't change the string "Actual values optional". It is used to find the toolname by nmoutput2so
    print CMD "Actual values optional $toolname options (undefined values not listed):\n";
    foreach my $opt (sort(@{$local_options})){
        $opt =~ s/[!:|].*//g; #get rid of :s |? :i etcetera
        if (defined $self->{$opt}){
            if (not ref($self->{$opt})){
                print CMD "-$opt=".$self->{$opt}."\n";
                $self->metadata->{'tool_options'}->{$opt} = $self->{$opt};
            } elsif ( ref($self->{$opt}) eq "ARRAY") {
                if (not ref($self->{$opt}->[0])){
                    my $opt_string = join(',', @{$self->{$opt}});
                    print CMD "-$opt=" . $opt_string . "\n";
                    $self->metadata->{'tool_options'}->{$opt} = $opt_string;
                }
            }
        }
    }

    $self->metadata->{'common_options'} = {};
    print CMD "\nActual values optional PsN (common) options (undefined values not listed):\n";
    if (ui->silent()) {
        print CMD "-silent=1\n";
        $self->metadata->{'common_options'}->{'silent'} = 1;
    }
    foreach my $opt (sort(@{$common_options})) {
        $opt =~ s/[!:|].*//g; #get rid of :s |? :i etcetera
        if (defined $self->{$opt}){
            if (not ref($self->{$opt})){
                print CMD "-$opt=".$self->{$opt}."\n";
                $self->metadata->{'common_options'}->{$opt} = $self->{$opt};
            } elsif ($opt eq 'threads') {
                print CMD "-$opt=".$self->{$opt}->[1]."\n";
                $self->metadata->{'common_options'}->{$opt} = $self->{$opt}->[1];
            } elsif (ref($self->{$opt}) eq "ARRAY") {
                my $opt_string = join(',', @{$self->{$opt}});
                print CMD "-$opt=" . $opt_string . "\n";
                $self->metadata->{'common_options'}->{$opt} = $opt_string;
            } elsif (ref($self->{$opt}) eq "HASH") {
                my $opt_string = join(',', @{$self->{$opt}});
                print CMD "-$opt=" . $opt_string . "\n";
                $self->metadata->{'common_options'}->{$opt} = $opt_string;
            }
        }
    }
    close(CMD);

    if ((lc($toolname) eq 'vpc') or (lc($toolname) eq 'npc')){
        unless ( -e $dir."/original_command.txt" ){
            #first run
            cp($dir . "/command.txt",$dir."/original_command.txt");
            if (-e $option_file ){
                cp($option_file,$dir."/original_version_and_option_info.txt");
            }
        }
    }

    if (defined $self->models) {
        $self->metadata->{'model_files'} = [];
        for my $model (@{$self->models}) {
            push @{$self->metadata->{'model_files'}}, $model->full_name();
        }
    }

    $self->write_meta(directory => $dir);
}

sub write_meta
{
    # Write meta.yaml to disk
    my $self = shift;
    my %parm = validated_hash(\@_,
        directory => { isa => 'Str', optional => 1, default => $self->directory },
        MX_PARAMS_VALIDATE_NO_CACHE => 1,
    );
    my $directory = $parm{'directory'};

    YAML::DumpFile("$directory/meta.yaml", $self->metadata);
}

sub get_rundir
{
    #static no shift
    #assume we are in main directory
    my %parm = validated_hash(\@_,
        basename => { isa => 'Str', optional => 0 },
        directory_option => { isa => 'Maybe[Str]', optional => 1 },
        model_dir_name => { isa => 'Bool', optional => 1, default => 0 },
        timestamp => { isa => 'Bool', optional => 1, default => 0 },
        modelname => { isa => 'Maybe[Str]', optional => 1 },
        create => { isa => 'Bool', optional => 1, default => 1 },
        model_subdir => { isa => 'Bool', default => 0 },
    );
    my $basename = $parm{'basename'};
    my $directory_option = $parm{'directory_option'};
    my $model_dir_name = $parm{'model_dir_name'};
    my $modelname = $parm{'modelname'};
    my $timestamp = $parm{'timestamp'};
    my $create = $parm{'create'};
    my $model_subdir = $parm{'model_subdir'};

    my $rundir;

    # Get the subdir name if model_subdir
    my $model_subdir_name;
    if ($model_subdir) {
        $model_subdir_name = utils::file::get_file_stem($modelname) . '/';
    }

    if (defined $directory_option and length($directory_option) > 0) {
        if ($model_subdir) {
            if ($directory_option =~ /[\/\\]/) {
                die "Only directory name allowed for -directory together with -model_subdir\n";
            }
            ($rundir, undef) = OSspecific::absolute_path($model_subdir_name . $directory_option, 'file');
        } else {
            ($rundir, undef) = OSspecific::absolute_path($directory_option, 'file');
        }
    } elsif (($timestamp or ($model_dir_name and (defined $modelname) and length($modelname) > 0)) and not $model_subdir_name) {

        my $return_dir = getcwd();
        my $dotless_model_filename = '';
        $modelname = '.mod' unless ((defined $modelname) and length($modelname) > 0);
        my $dirt;
        #shave off any path from modelname
        ($dirt, $dotless_model_filename) = OSspecific::absolute_path(undef,$modelname);
        $dotless_model_filename =~ s/\.[^.]+$//; #last dot and extension

        if ($timestamp) {
            my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) = localtime(time);
            my $timestring = sprintf("-PsN-%s-%02i-%02i-%02i%02i%02i", ($year + 1900), ($mon + 1), $mday, $hour, $min, $sec);
            #timestamped but not numbered folder
            my ($path, $dir) = OSspecific::absolute_path($return_dir, $dotless_model_filename . $timestring);

            $rundir = $path . $dir;
            if (-e $rundir) {
                #number to avoid returning existing folder name
                ui -> print(category => 'all',
                             message  => "$rundir exists, append number");
                $rundir = OSspecific::unique_path($dotless_model_filename . $timestring . '.dir', $return_dir);
            }
        } else {
            #numbered folder
            my $dirnamebase = $dotless_model_filename . '.dir';
            $rundir = OSspecific::unique_path($dirnamebase, $return_dir);
        }
    } else {
        my $return_dir = getcwd();
        if ($model_subdir) {
            $return_dir .= "/$model_subdir_name";
            if (not -e $return_dir) {
                $rundir = $return_dir . $basename . "1";
            } else {
                $rundir = OSspecific::unique_path($basename, $return_dir);
            }
        } else {
            $rundir = OSspecific::unique_path($basename, $return_dir);
        }
    }

    if ($create) {
        unless (-d $rundir) {
            unless(mkpath($rundir)) {
                croak("get_rundir: Failed to create $rundir : $!");
            }
        }
    }

    return $rundir;
}

sub create_R_script
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        tool_name => { isa => 'Str', optional => 1 }
    );
    my $tool_name = $parm{'tool_name'};

    return if ($self->rplots < 0);

    unless (defined $tool_name){
        if (defined $self->directory_name_prefix) {
            $tool_name = $self->directory_name_prefix;
        } else {
            my @tool_name_full = split('::', ref $self);
            $tool_name = $tool_name_full[$#tool_name_full];
            if ($tool_name eq 'modelfit'){
                $tool_name = 'execute';
            }
        }
    }

    my $R_lib_path = PsN::get_R_lib_path();
    if ($self->rprofile and length($R_lib_path) > 0) {
        open my $fh, '>', $self->directory . '.Rprofile';
        my $rlib = ".libPaths('" . $R_lib_path . "')\n";
        print $fh 'cat("*** Starting R using .Rprofile generated by PsN ***\n")' . "\n";
        print $fh 'cat("Using library path ' . $R_lib_path . '\n")' . "\n";
        print $fh $rlib;
        close $fh;
    }

    #these are set in common_options, should never be undefined
    my ($dir, $file) = OSspecific::absolute_path($self->template_directory_rplots,$self->template_file_rplots);
    my $template_file = $dir.$file;

    # check if tool default R file is an Rmarkdown file
    my $rmarkdown = 0; # FALSE
    my $Rmarkdown_installed = 0;
    my $file_type = 'R';
    if ($template_file =~ /\.Rmd$/ and defined PsN::get_R_exec()) {
        $file_type = 'Rmd';
        $rmarkdown = 1; #TRUE
        if($self->rmarkdown) { # check if option -no-rmarkdown is used, then Rmarkdown file will not be created
                    #check if rmarkdown/pandoc(version 1.12.3 or higher)/latex are installed
            if ($self->pdf) {
                chdir $self->directory or die "Could not change directory before rmarkdown checking";
                my $test_file = 'test_file_rmarkdown_installed.Rmd';
                open ( SCRIPT, ">" . $test_file );
                print SCRIPT join("\n",'experiment')."\n";
                close SCRIPT;

                my $rscript = PsN::get_R_exec() . 'script';
                system("$rscript -e \"rmarkdown::render(input='".$test_file."',output_format='pdf_document',output_file='test_file_rmarkdown_installed.pdf')\" > test_file_rmarkdown_installed.Rout 2>&1");
                if (-e 'test_file_rmarkdown_installed.pdf') {
                    $Rmarkdown_installed = 1;
                    unlink('test_file_rmarkdown_installed.pdf'); # delete test files
                }
                unlink('test_file_rmarkdown_installed.Rmd');
                unlink('test_file_rmarkdown_installed.Rout');
                chdir ".." or die "Could not change directory after rmarkdown checking";
            } else {
                $Rmarkdown_installed = 1;    # For now don't check if html rendering
            }
        }
    }

    if (-e $template_file) {
        open(FILE, $template_file) ||
            croak("Could not open $template_file for reading");

        my @code = ();
        foreach my $line (<FILE>) {
            chomp($line);
            push(@code, $line);
        }
        close(FILE);
        my $rplot = rplots->new(
            toolname => $tool_name,
            directory => $self->directory,
            level => $self->rplots,
            raw_results_file => $self->raw_results_file->[0],
            tool_results_file => $self->results_file,
            plotcode => \@code,
            subset_variable => $self->subset_variable_rplots,
            model => $self->models->[0],
            R_markdown => $rmarkdown,
            rmarkdown_installed => $Rmarkdown_installed,
            model_subdir => $self->model_subdir,
            file_type => $file_type,
            debug_rmd => $self->debug_rmd,
            html => $self->html,
            pdf => $self->pdf,
        );

        $self->create_R_plots_code(rplot => $rplot) if ($self->can("create_R_plots_code"));
        $rplot->make_plots;
    }
}

sub default_update_inits
{
    # Update initial estimates of model either from lst_file if available or from lst file from the model run
    my $self = shift;
    my %parm = validated_hash(\@_,
        lst_file => { isa => 'Maybe[Str]', optional => 1 },
        model => { isa => 'model', optional => 0 },
        problem => { isa => 'Int', default => 1 },
    );
    my $lst_file = $parm{'ĺst_file'};
    my $model = $parm{'model'};
    my $problem = $parm{'problem'};

    if (defined $lst_file) {
        my $lst_name;
        if (File::Spec->file_name_is_absolute($lst_file)) {
            $lst_name = $lst_file;
        } else {
            $lst_name = $self->base_directory . $lst_file;
        }
        print "Updating initial estimates from: ", $lst_name, "\n";
        $model->update_inits(from_output_file => $lst_name, problem_number => $problem);
    } elsif ($model->is_run()) {
        print "Updating initial estimates from: ", $model->outputs->[0]->full_name(), "\n";
        $model->update_inits(from_output => $model->outputs->[0], problem_number => $problem);
    } else {
        print "Warning: no lst-file could be found. Will use the parameter estimates in the model file.\n";
    }
}

1;
