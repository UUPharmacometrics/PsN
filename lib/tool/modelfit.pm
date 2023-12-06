package tool::modelfit;

use include_modules;
use Config;
use Cwd;
use Data::Dumper;
use File::Copy qw/copy mv/;
use File::Path;
use File::Glob;
use File::Spec;
use Storable;
use random;
use nonmemrun;
use nonmemrun::localunix;
use nonmemrun::localwindows;
use nonmemrun::slurm;
use nonmemrun::torque;
use nonmemrun::lsf;
use nonmemrun::zink;
use nonmemrun::sge;
use nonmemrun::mosix;
use nonmemrun::ud;
use output;
use OSspecific;
use ui;
use Time::HiRes;
use Mouse;
use MouseX::Params::Validate;
use PsN;

extends 'tool';

has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'tail_output' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'resume' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nmtran_skip_model' => ( is => 'rw', isa => 'Int', default => 10000 );
has 'full_path_nmtran' => ( is => 'rw', isa => 'Str' );
has 'nmtran_error_file' => ( is => 'rw', isa => 'Str', default => 'nmtran_error.txt' );
has 'general_error_file' => ( is => 'rw', isa => 'Str', default => 'psn_nonmem_error_messages.txt' );
has 'base_msfo_name' => ( is => 'rw', isa => 'Str' );
has 'max_hash' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'full_path_runscript' => ( is => 'rw', isa => 'Str' );
has 'modext' => ( is => 'rw', isa => 'Str', default => 'mod' );
has 'wintail_exe' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'wintail_command' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'reduced_model_ofv' => ( is => 'rw', isa => 'Num' );
has 'cutoff' => ( is => 'rw', isa => 'Num' );
has 'cutoff_thetas' => ( is => 'rw', isa => 'ArrayRef' );
has 'cut_thetas_rounding_errors' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'cut_thetas_maxevals' => ( is => 'rw', isa => 'Bool', default => 0);
has 'handle_hessian_npd' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['modelfit.log'] } );
has '_raw_results_callback' => ( is => 'rw' );
has 'any_nonparametric_step' => ( is => 'rw', isa => 'Bool', default => 0 );
#start description
    #
    # modelfit class is therefore a specialization of a general PsN
    # tool class. The tool class itself is not capable of much at
    # all but to define a common structure for all PsN tools.
    #
    # All attributes and (class) methods specified for the general
    # tool class are inherited in the modelfit class. Some (class) methods
    # are defined in both classes (e.g. the L</run>) and in these
    # cases it is the modelfit version that will be used.
    #
#end description

#start examples
    # The following code may be used to create a simple modelfit
    # object and to run it.
    #
    #   use lib qw(path to PsN installation/lib);
    #   use tool::modelfit;
    #   use model;
    #
    #   my $model = model -> new( filename => 'run1.mod' );
    #   my $modelfit = tool::modelfit -> new( models => [$model] );
    #   my %results = %{$modelfit -> run};
    #
    # To illustrate a more complex use of modelfit, we can run a
    # bootstrap of 200 resampled data sets of same size as the
    # original data set. In this example we assume that the modelfile
    # we use contains only one problem and one sub problem.
    #
    #   use lib qw(path to PsN installation/lib);
    #   use tool::modelfit;
    #   use data;
    #   use model;
    #   use output;
    #   Math::Random;       # For perturbation of initial estimates
    #                       # after unsuccessful minimizations
    #
    #   # set these to appropriate values for your own run
    #   my $samples = 200;
    #   my $modelfile = 'run1.mod';
    #   my $boot_sample_file = 'boot_ind.csv';
    #   my $boot_results_file = 'boot_results.csv';
    #
    #   # set the seed from a phrase (consistent over different hardware,
    #   # see Math::Random )
    #   random_set_seed_from_phrase('testing');
    #
    #   # ignore missing data and (especially) output files
    #   my $model = model -> new( filename => $modelfile,
    #                             ignore_missing_files => 1 );
    #
    #
    #   # Create the bootstrap data sets. The default name for each
    #   # new resampled data file will be bs###.dta, where ### is a
    #   # number from 1 to $samples.
    #   my ( $dataref, $incl_ind_ref, $incl_keys_ref ) =
    #       $data -> bootstrap ( samples => $samples );
    #
    #   # Save the resampled ID numbers in a file, one row for each
    #   # bootstrap data set
    #   open ( LOG, ">$boot_sample_file" );
    #   foreach my $sample ( @{$incl_ind_ref} ) {
    #     print LOG join(';', @{$sample} ),"\n";
    #   }
    #
    #   # Create the boostrap models and set the correct resampled
    #   # data file in $DATA. Save them in @bs_models.
    #   my @bs_models  = ();
    #   for ( my $i = 1; $i <= $samples; $i++ ) {
    #     my $bs_mod = $model -> copy( filename    => 'bs'.$i.'.mod',
    #                                  copy_data   => 0,
    #                                  copy_output => 0);
    #     $bs_mod -> datafiles( new_names     => ['bs'.$i.'.dta'],
    #                           absolute_path => 1 );
    #     $bs_mod -> _write;
    #     push( @bs_models, $bs_mod );
    #   }
    #
    #   # Create a modelfit object with the bootstrap models as
    #   # input. Set the number of parallel runs to 2.
    #   my $mfitobj = tool::modelfit -> new ( models     => \@bs_models,
    #                                           threads    => 2 );
    #
    #   # Run the model fit. Since the bootstrap models are named
    #   # bs###.mod, the default names for the output files will be
    #   # bs###.lst.
    #   $mfitobj -> run;
    #
    #   # We'll save the OFV plus the theta, omega and sigma estimates
    #   # for each run in a file.
    #   open( RESULT, ">$boot_results_file" );
    #
    #   for ( my $i = 1; $i <= $samples; $i++ ) {
    #     my $bs_out = output -> new( filename => 'bs'.$i.'.lst' );
    #     my @ofv    = @{$bs_out -> ofv};
    #     my @thetas = @{$bs_out -> thetas};
    #     my @omegas = @{$bs_out -> omegas};
    #     my @sigmas = @{$bs_out -> sigmas};
    #     # We know that we only have one problem and one sub problem in this
    #     # example. ([0][0] comes from that fact)
    #     my @print_strings = ();
    #     push( @print_strings, $ofv[0][0] );
    #     push( @print_strings, @{$thetas[0][0]} );
    #     push( @print_strings, @{$omegas[0][0]} );
    #     push( @print_strings, @{$sigmas[0][0]} );
    #     print RESULT join( ';', @print_strings ),"\n";
    #   }
    #
    #   close( RESULT );
    #
    #   # We're done!
#end examples

#start synopsis
    #   use tool::modelfit;
    #   use model;
    #
    #   my $model_obj = model -> new ( filename => 'run1.mod' );
    #
    #   my $modelfit_obj = tool::modelfit -> new ( models => [$model_obj] );
    #
    #   my $output_obj = $modelfit_obj -> run;
#end synopsis

sub BUILDARGS
{
    my $self = shift;
    return $self->SUPER::BUILDARGS(@_);
}

sub BUILD
{
    my $self = shift;

    # Usage:
    #
    #   $modelfit_object = tool::modelfit -> new( models  => [$model_object],
    #                                               retries => 5 );
    #
    # This is the basic usage and it creates a modelfit object that
    # can later be run using the L</run> method. I<models> is an array
    # of PsN model objects.
    #
    #   $modelfitObject = $tool::modelfit -> new( 'retries' => 5 );
    #   $modelfitObject -> add_model( init_data => { filename => $modelFileName } );
    #
    # This way of using modelfit is suitable if you have a list with
    # filenames of modelfiles. "add_model> will create modelfitobject
    # for you.
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
    # NM_run[X] where [X] is an index running from 1 to the number of
    # runs. If unsure of what this means, leave it undefined and a
    # default will be used, e.g. modelfit_dir3 or something.
    #
    # Next, the I<compress> and I<remove_temp_files> attributes are good
    # if you want to save some hard disk space. I<compress> set to 1
    # will put all NONMEM output in to an tar/gz archive named
    # I<nonmem_files.tgz> placed in the I<NM_run[X]> directory
    # described above. If I<remove_temp_files> is set to 1,  the NONMEM
    # files: 'FCON', 'FDATA', 'FSTREAM', 'PRDERR' will be removed.
    #
    # I<clean> is a stronger version of I<remove_temp_files>; it will also
    # remove I<NM_run[X]> and all that is in these.
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
    # href="../model/problem/record/init_option.html#set_random_init">set_random_init</a>
    # of the <a
    # href="../model/problem/record/init_option.html">init_option
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
    #    COVARIANCE STEP ABORTED
    #    PROGRAM TERMINATED BY OBJ
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
    # if I<silent_logfile> is defined all NONMEM output will
    # be written to I<NM_run[X]/xxx>, where xxx is the defined value.
    #
    # I<extra_files> is an array of strings where each string is a
    # file needed for NONMEM execution. Those file will be moved
    # to the I<NM_run[X]> directory.
    #
    # I<seed> is just a way to set a random seed number.

    # I<cpuTime> Is an estimated execution time for each individual
    # modelfile. It should preferably be a bit longer than reality. If
    # you specify a cpuTime that is to short, you risk that the grid
    # kills your jobs prematurely. The unit of I<cpuTime> is minutes.

    # I<grid_cpuTime> is the time of the actual grid job. It should be
    # used to group modelfiles together. For example, if you set
    # I<cpuTime> to ten minutes, I<grid_cpuTime> to 60 minutes and the
    # number of modelfiles is 14 modelfit will create three grid jobs,
    # two with six model files each and one with two modelfiles.

    # I<grid_adress> is the URL of the grid submission server,
    # e.g. hagrid.it.uu.se.

    if (defined $self->logfile) {
        $self->logfile([join('', OSspecific::absolute_path( $self->directory, $self->logfile->[0]) ) ]);
    }

    my $ref = nonmemrun::setup_paths(nm_version => $self->nm_version);

    $self->full_path_runscript($ref->{'full_path_runscript'});
    $self->full_path_nmtran($ref->{'full_path_nmtran'}) if (defined $ref->{'full_path_nmtran'});

    if ($self->run_on_lsf or $self->run_on_ud or $self->run_on_zink or
        $self->run_on_torque or $self->run_on_slurm or
        $self->run_on_sge) {
        $self->run_local(0);
    } else {
        $self->run_local(1);
    }

    if ($self->handle_msfo) {
        $self->handle_crashes(1);
    }

    if ($self->handle_crashes and $self->crash_restarts > 0) {
        $self->crash_restarts($self->crash_restarts + 1);
    }

    $self->calculate_raw_results_width();

    $self->raw_line_structure(ext::Config::Tiny->new());
}

sub run
{
    my $self = shift;
    my @results;

    my $cwd = getcwd();
    my $started_all_models = 0;
    my $started_all_models_print = 0;
    my $do_abort = 0;
    my $failed_run_message = '';
    unless (chdir($self->directory)){
        croak("Failed chdir to self->directory: ".$self->directory."\n system error $!");
    }

    # sanity checks

    my @models;
    if (defined $self->models) {
        @models = @{$self->models};
        for (my $i=0;$i< scalar(@models); $i++){
            if (defined $models[$i]->problems){
                for (my $j=0; $j < scalar(@{$models[$i]->problems}); $j++){
                    if (defined $models[$i]->problems->[$j] and
                        defined $models[$i]->problems->[$j]->nonparametrics and
                        scalar(@{$models[$i]->problems->[$j]->nonparametrics})>0){
                        $self->any_nonparametric_step(1);
                        last;
                    }
                }
            }
        }
    } else {
        croak("Have no models!");
    }

    my $threads = $self->threads;
    $threads = $#models + 1 if ($threads > $#models + 1);

    # print starting messages
    my $message;
    if ($self->resume){
        my $count_not_done=0;
        for (my $i=0; $i< scalar(@models); $i++){
            $count_not_done++ unless ($self->models->[$i]->is_run);
        }
        if ($count_not_done > 0){
            $threads = $count_not_done if ($threads > $count_not_done);
            $message = "Restarting $count_not_done out of " . scalar(@models) . ' NONMEM executions. '. $threads .' in parallel.'."\n";
        }else{
            $message = "Restarting 0 out of " . scalar(@models) . ' NONMEM executions. ';
        }
    }else{
        $message = 'Starting ' . scalar(@models) . ' NONMEM executions. '. $threads .' in parallel.'."\n";
    }
    ui -> print( category => 'all',
                 message  => $message )
        unless ($self->parent_threads > 1);
    my $verbose_header = sprintf("%10s%18s%12s%30s",
                                 'Run number','Model name','OFV','Covariance step successful');

    ui -> print( category => 'all',
        message  => $verbose_header,
        newline => 1)  if $self->verbose;

    # Create NM_run subdirs. Print model-NM_run translation file

    open( MNT, ">model_NMrun_translation.txt");
    for ( my $run = 0; $run <= $#models; $run ++ ) {
        $self -> create_sub_dir( subDir => '/NM_run'.($run+1),
                                 modelname => $models[$run]->filename);
        print MNT sprintf("%-40s", $models[$run]->filename), "NM_run", ($run + 1), "\n";
    }
    close(MNT);

    # Local execution

    # %queue_map is a mapping from nonmem.pm pID to run number.

    my %queue_map;

    # %queue_info is keyed on run number and contains information
    # about each nonmem run.

    my %queue_info;

    my $old_inthandler = $SIG{'INT'};
    if ($self->run_on_slurm) {
        $SIG{'INT'} = sub {
            for my $key (keys %queue_info) {
                my $jobid = $queue_info{$key}->{'nonmemrun'}->{'job_id'};
                system("scancel $jobid");
            }
            exit;
        };
    }

    # @queue is an array of NM_run directory numbers. If "X" is in
    # @queue, then psn.mod in "NM_runX" is scheduled to be run. It
    # initialized to all models in the tool. Note that if X is in
    # the queue, it doesn't mean NM_runX exists.

    my @queue = (0 .. $#models);
    my $all_jobs_started = 0;

    # We loop while there is content in the queue (which shrinks when jobs are submitted and grows when restarts are needed)
    # and while we have jobs running, i.e. scalar keys %queue_map > 0 (represented in the queue_info)

    while ((scalar(@queue) > 0) or (scalar keys %queue_map > 0)) {
        if ((scalar(@queue) > 0) and (scalar keys %queue_map < $threads)) {
            #we may start a new job here
            # This is where we initiate a new job:

            my $run = shift(@queue);

            # check for no run conditions. (e.g. job already run)

            if ($self->models->[$run]->is_run and $self->resume) {

                #we have output in m1 or model's home directory. Ignore NM_run
                my ($raw_results_row, $nonp_row) = $self->create_raw_results_rows(max_hash => $self->max_hash,
                                                                                  model => $self->models->[$run],
                                                                                  model_number => $run + 1,
                                                                                  raw_line_structure => $self->raw_line_structure
                    );

                $self->raw_results([]) unless defined $self->raw_results;
                $self->raw_nonp_results([]) unless defined $self->raw_nonp_results;

                push(@{$self->raw_results}, @{$raw_results_row});
                push(@{$self->raw_nonp_results}, @{$nonp_row});

                next; # We are done with this model. It has already been run. Go back to main while loop.
            }

            # delay code (to avoid overload of new processes)
            make_delay(threads => $threads,
                       run => $run,
                       min_fork_delay => $PsN::config -> {'_'} -> {'min_fork_delay'}, #can be undef
                       max_fork_delay => $PsN::config -> {'_'} -> {'max_fork_delay'});

            # Call to run_nonmem

            # This will stop nasty prints from model, output and data
            # which are set to print for the scm.
            my $old_category = ui -> category();

            ui -> category('modelfit');

            unless(chdir( 'NM_run'.($run+1) )){
                croak("Failed chdir to ".'NM_run'.($run+1)."\n system error $!");
            }

            ## Start tail of output if requested. Only works for Win32.
            if( $Config{osname} eq 'MSWin32' and $self->tail_output ) {
                require Win32::Process;

                my $prio_class = "NORMAL_PRIORITY_CLASS";

                Win32::Process::Create(my $ProcessObj,
                    eval($self->wintail_exe),
                    eval($self->wintail_command),
                    0,
                    $prio_class,
                    ".");
            }

            # Initiate queue_info entry (unless its a retry)

            unless (exists $queue_info{$run}) {
                #if stats-runs.csv exists then move_model_and_input does not do anything
                #but read psn.mod into candidate_model object
                my $run_nmtran = 0;
                my $check_verbatim = 0;
                if ($self->check_nmtran and (($run + 1) < $self->nmtran_skip_model)) {
                    $run_nmtran = 1;
                    if (scalar(@models) == 1){
                        $check_verbatim = 1;
                    }
                }
                ($queue_info{$run}{'candidate_model'},$queue_info{$run}{'tries'}) =
                    $self -> copy_model_and_input(model => $models[$run],
                                                  source => '../',
                                                  run_nmtran => $run_nmtran,
                                                  check_verbatim => $check_verbatim);

                $queue_info{$run}{'model'} = $models[$run];
                $queue_info{$run}{'have_accepted_run'} = 0;
                $queue_info{$run}{'crashes'} = 0;
                $queue_info{$run}{'evals'} = 0;
                $queue_info{$run}{'run_results'} = [];
                $queue_info{$run}{'raw_results'} = [];
                $queue_info{$run}{'raw_nonp_results'} = [];



                # printing progress

                # We don't want to print all starting models if they are
                # more than ten. But we always want to print the first
                # and last

                my $modulus = (($#models + 1) <= 10) ? 1 : (($#models + 1) / 10);

                if ($self->send_email) {
                    my $mail_modulus = (($#models + 1) <= 4) ? 1 : (($#models + 1) / 5);
                    if ($run == 0) {
                        $queue_info{$run}{'send_email'} = 'ALL';
                    } elsif ($run % $mail_modulus == 0 or $run == $#models) {
                        $queue_info{$run}{'send_email'} = 'END';
                    }
                }

                if ($run % $modulus == 0 or $run == 0 or $run == $#models) {
                    #only slurm_submit uses email info right now
                    # The unless checks if tool owning the modelfit is
                    # running more modelfits, in wich case we should be
                    # silent to avoid confusion. The $done check is made do
                    # diffrentiate between allready run processes.

                    ui -> print(category => 'all', wrap => 0, newline => 0, message  => 'S:'.($run+1).' .. ')
                        unless ($self->parent_threads > 1 or $self->verbose);
                }
                $started_all_models = 1 if ($run == $#models);

            }

            my %options_hash = %{$self->_get_run_options(run_id => $run)};

            $self->run_nonmem(run_no => $run,
                                  nm_version      => $options_hash{'nm_version'},
                                  queue_info      => $queue_info{$run},
                                  queue_map       => \%queue_map);

            ui -> print(category => 'all', message  => "\nAll executions started.", newline => 1)
                if ($started_all_models and $self->parent_threads <= 1  and not $self->verbose and not $started_all_models_print);
            $started_all_models_print = 1;
            chdir('..');

            ui -> category($old_category);

            next; #go back to main while loop to check if there is another job that can be started
        }


        # We could not start a new job, so we look for jobs that have been started and
        # finished. If we find one, we set $pid to that job.
        #we must loop here until a job is finished, because we know we cannot start new job
        # until one is finished

        my $pid = 0;

        # Get potiential finished pid
        my $firstpid;
        while (not $pid) {
            $firstpid=1;
            foreach my $check_pid (sort {$queue_map{$a} <=> $queue_map{$b} } keys %queue_map) {
                if ($check_pid =~ /^rerun_/) {

                    # A pid that starts with "rerun" is a rerun and is always
                    # "finished".

                    $pid = $check_pid;
                    last;
                } elsif ($check_pid =~ /^fail_/) {

                    # A pid fail_ is a failed grid submit and is always
                    # "finished".

                    $pid = $check_pid;
                    last;
                }

                # Check the job status
                my $nonmemrun = $queue_info{$queue_map{$check_pid}}{'nonmemrun'};
                $pid = $nonmemrun->monitor;

                if ($pid) {
                    last; #we found a finished run, do not loop over more running pid
                }
                Time::HiRes::usleep(10000) unless ($firstpid or $self->run_local); #in microseconds
                $firstpid=0;
            }

            if (not $pid) {

                # No process has finished.
                # we cannot start another job
                # sleep to make polling less demanding
                if (defined $PsN::config->{'_'}->{'job_polling_interval'} and $PsN::config->{'_'}->{'job_polling_interval'} > 0) {
                    sleep($PsN::config->{'_'}->{'job_polling_interval'});
                } else {
                    sleep(1);
                }
                next; # Return to polling for finished jobs.

            } else {

                # Here, a process has finished and we check for restarts.

                my $run = $queue_map{$pid};

                my $candidate_model = $queue_info{$run}{'candidate_model'};

                my $work_dir = 'NM_run' . ($run + 1);
                unless(chdir($work_dir)){
                    croak("Failed chdir to work_dir $work_dir"."\n system error $!");
                }

                #here we try to make sure files are synced before we start processing output
                #if a missing psn.lst is due to NMtran or compilation failure we should see a file
                #$self->general_error_file or $self->nmtran_error_file in the directory. Then do not wait, continue directly with
                #storing failure messages. On the other hand, if we do not see psn.lst due to
                #a file sync delay, there will be no $self->general_error_file. Wait for a long time
                #to see if $self->general_error_file appears.
                #neither psn_nonmem_error_messages.txt nor psn.lst will appear if run killed
                #No hurry in those cases. Can always wait for a long
                #time if neither file has appeared.

                my $dirt;
                for (my $i = 0; $i < 20; $i++) {
                    #an ls might make files sync and become visible
                    $dirt = `ls -la 2>&1` unless ($Config{osname} eq 'MSWin32');
                    last if((-e 'psn.lst') or (-e 'stats-runs.csv') or (-e 'job_submission_error')
                            or (-e $self->general_error_file)
                            or (-e $self->nmtran_error_file)
                            or $self->run_local
                            or ($pid =~ /^rerun_/)
                            or ($pid =~ /^fail_/)
                        ); #psn.lst exists or will never appear
                    if (defined $PsN::config->{'_'}->{'file_polling_interval'} and $PsN::config->{'_'}->{'file_polling_interval'} >= 0) {
                        sleep($PsN::config->{'_'}->{'file_polling_interval'});
                    } else {
                        sleep(6);
                    }
                }

                #we do this before restart_needed so post-processed tables are properly handled
                $self->compute_cwres(queue_info => $queue_info{$run}, run_no => $run);

                $self->compute_iofv(queue_info => $queue_info{$run}, run_no => $run);

                # Make sure that each process gets a unique random sequence:
                my $tmpseed = defined $self->seed ? $self->seed : random_uniform_integer(1, 1, 99999999);
                my $tmptry  = exists $queue_info{$run}{'tries'} ? $queue_info{$run}{'tries'} : 0;
                #have two alternatives: first for backward reproducability of sequences
                #second to prevent bug when very large number of models
                if ($run < 5000) {
                    random_set_seed(($tmpseed + 100000 * ($run + 1)), ($tmptry + 1));
                } else {
                    my $phrase = "seed $tmpseed try $tmptry run $run";
                    random_set_seed_from_phrase($phrase);
                }

                my %options_hash = %{$self->_get_run_options(run_id => $run)};

                for my $key (keys %options_hash) {
                    delete $options_hash{$key} unless defined $options_hash{$key};
                }

                #careful here, option maxevals is set on commandline, but model->maxeval()
                #without s is
                #array of values actually set in modelfile
                my $do_restart = $self -> restart_needed(%options_hash,
                                                          queue_info  => $queue_info{$run},
                                                          run_no      => $run,
                                                          maxevals        => $candidate_model->maxevals);

                if ($self->abort_on_fail) {
                    my $tries = \$queue_info{$run}->{'tries'};
                    if (defined $queue_info{$run}->{'run_results'}->[${$tries}]->{'failed'} or
                        defined $queue_info{$run}->{'run_results'}->[${$tries}]->{'nonmem_run_failed'}) {
                        $do_abort = 1;
                        $failed_run_message = 'run in NM_run'.($run+1).' failed';
                        if (defined $queue_info{$run}->{'run_results'}->[${$tries}]->{'nonmem_run_failed'}) {
                            $failed_run_message .= ': '.$queue_info{$run}->{'run_results'}->[${$tries}]->{'nonmem_run_failed'};
                        }
                        $do_restart = 0;
                        @queue = ();
                    }
                }

                if ($do_restart) {
                    unshift(@queue, $run);
                    delete($queue_map{$pid});
                    chdir('..');
                } else {
                    $self->select_best_model(run_no => $run,
                                             queue_info_ref => $queue_info{$run});

                    # Print finishing messages

                    if( scalar @queue == 0 ) {
                        if( $all_jobs_started == 0 ) {

                            ui -> print( category => 'all', message => "Waiting for all NONMEM runs to finish:", newline => 1 )
                                if ($self->parent_threads <= 1 and $threads > 1 and not $self->verbose);

                            $all_jobs_started = 1;
                        }

                        my $modulus = (($#models+1) <= 10) ? 1 : (($#models+1) / 10)+1;

                        if ( $run % $modulus == 0 or $run == 0 or $run == $#models ) {
                            ui -> print( category => 'all',
                                         message  => 'F:'.($run+1).' .. ',
                                         wrap => 0,
                                         newline => 0)
                                unless ( $self->parent_threads > 1 or $self->verbose );
                        }
                    }

                    chdir( '..' );

                    # cleaning and done file

                    if( $self->clean >= 3 ){
                        unlink( <$work_dir/worker*/*> );
                        my @removedir = <$work_dir/worker*>;
                        foreach my $remdir (@removedir){
                            rmdir ($remdir) if (-d $remdir);
                        }
                        unless ((-e $work_dir.'/'.$self->general_error_file) or (-e $work_dir.'/'.$self->nmtran_error_file)){

                            #leave if error message,
                            unlink( <$work_dir/*> )  ;
                            unless( rmdir( $work_dir ) ){ warn "Unable to remove $work_dir directory: $! ."};
                        }
                    } else {
                        1;

                    }

                    $self -> print_finish_message( candidate_model => $candidate_model,
                                                   run => $run );

                    if( $threads <= 1 ) {
                        $self->prepared_models([]) unless defined $self->prepared_models;
                        push( @{$self->prepared_models->[$run]{'own'}}, $candidate_model );
                    }

                    delete( $queue_info{$run} );
                    delete( $queue_map{$pid} );
                }


            }
            #If we get this far then we found a $pid so just continue, this loop will break now
        } # end while (not $pid).

    } #end while loop over jobs left to start or jobs left running
    #  Print finish message

    ui -> print( category => 'all', message  => " done",newline => 1 )
        if( $self->parent_threads <= 1 and $threads > 1 and not $self->verbose);

        # local execution

    $self->prepare_raw_results();

    $self->print_raw_results();

    unless(chdir($cwd)){
        croak("Failed chdir to cwd: $cwd"."\n system error $!");
    }

    # clean $self -> directory
    if( $self->clean >= 2 ){
        unlink($self->directory . '/model_NMrun_translation.txt');
        unlink($self->directory . '/modelfit.log');
    }
    if( not $self->top_tool and $self->clean >= 3 ){
        my $dir = $self->directory;
        unlink( <$dir/NM_run*/worker*/*> );
        my @removedir = <$dir/NM_run*/worker*>;
        foreach my $remdir (@removedir){
            rmdir ($remdir) if (-d $remdir);
        }
        my $keep_this=0;
        foreach my $work_dir (<$dir/NM_run*>){
            if ((-e $work_dir.'/'.$self->general_error_file) or (-e $work_dir.'/'.$self->nmtran_error_file)){
                #leave if error message,
                $keep_this=1;
            }else{
                unlink( <$work_dir/*> );
                rmdir($work_dir);
            }
        }
        unlink( <$dir/raw_results_structure> );
        unless ($keep_this){
            unlink( <$dir/*> );
            rmdir( $dir );
        }
    }

    if ($do_abort){
        croak("abort_on_fail is set and ".$failed_run_message.". Aborting");
    }

    if ($self->run_on_slurm) {
        $SIG{'INT'} = $old_inthandler;
    }

    return \@results;
}

sub make_delay{
    my %parm = validated_hash(\@_,
                              threads => { isa => 'Int', optional => 0 },
                              run => { isa => 'Int', optional => 0 },
                              min_fork_delay => { isa => 'Maybe[Num]', optional => 0 },
                              max_fork_delay => { isa => 'Maybe[Num]', optional => 0 },
        );
    my $threads = $parm{'threads'};
    my $run = $parm{'run'};
    my $min_fork_delay = $parm{'min_fork_delay'}; #100000
    my $max_fork_delay = $parm{'max_fork_delay'}; #1

    if ($threads > 1) {
        if ($run > 0) {
            my $start_sleep = Time::HiRes::time();

            my ($min_sleep, $max_sleep); # min_sleep is in microseconds and max_sleep is in seconds.

            if( defined $min_fork_delay ) {
                $min_sleep = $min_fork_delay;
            } else {
                $min_sleep = 0;
            }

            if( defined $max_fork_delay ) {
                $max_sleep = $max_fork_delay;
            } else {
                $max_sleep = 0;
            }

            while ((not(-e 'NM_run' . ($run) . '/psn.lst')) and
                   (Time::HiRes::time() - $start_sleep) < $max_sleep) {
                Time::HiRes::usleep($min_sleep);
            }
        }

    }
}

sub select_best_retry
{
    #static
    my %parm = validated_hash(\@_,
        run_results => { isa => 'ArrayRef', optional => 0 },
        accepted_ofv_difference => { isa => 'Num', optional => 0 },
    );
    my $run_results = $parm{'run_results'};
    my $accepted_ofv_difference = $parm{'accepted_ofv_difference'};
    my $selected_index;

    if ($accepted_ofv_difference < 0){
        croak("accepted_ofv_difference must not be negative, but is $accepted_ofv_difference");
    }

    # Rule for selecting best model:
    # pick global best ofv, except  except accepted_ofv_difference preference for picky if reached OR
    # otherwise accepted_ofv_difference preference for minimization successful if found
    # If no ofv value is produced it will be the basic model.

    my $best_picky_index = -1;
    my $best_minsucc_index = -1;
    my $best_any_index = -1;

    my $best_picky_ofv=999999999;
    my $best_minsucc_ofv=999999999;
    my $best_any_ofv = 999999999;
    my $warning;

    for(my $i = 0; $i < scalar @{$run_results}; $i++ ){
        if( defined( $run_results -> [$i] -> {'ofv'} )  ) {
            if ($run_results -> [$i] -> {'ofv'} < $best_any_ofv ){
                $best_any_ofv = $run_results -> [$i] -> {'ofv'};
                $best_any_index = $i;
            }
            if ($run_results->[$i]->{'minimization_successful'}){
                if ($run_results -> [$i] -> {'ofv'} < $best_minsucc_ofv){
                    $best_minsucc_ofv = $run_results -> [$i] -> {'ofv'};
                    $best_minsucc_index = $i;
                }
                if ( $run_results -> [$i] -> {'pass_picky'}
                     and $run_results -> [$i] -> {'ofv'} < $best_picky_ofv){
                    $best_picky_ofv = $run_results -> [$i] -> {'ofv'};
                    $best_picky_index = $i;
                }
            }
        }
    }

    if ($best_picky_index >= 0 and ($best_picky_ofv <= $best_any_ofv + $accepted_ofv_difference)){
        #picky set and reached, not more than accepted_diff worse than best any ofv
        $selected_index = $best_picky_index;
    }elsif ($best_minsucc_index >= 0 and ($best_minsucc_ofv <= $best_any_ofv + $accepted_ofv_difference)){
        #minim successful exists, not more than accepted_diff worse than best any ofv
        $selected_index = $best_minsucc_index;
    }elsif ($best_any_index >= 0){
        #have any ofv
        $selected_index = $best_any_index;
    }else{
        #no ofv at all, use first
        $selected_index = 0;
    }


    #add 1 to get order number which is needed below
    return ($selected_index+1);
}

sub select_best_model
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        run_no => { isa => 'Int', optional => 1 },
        queue_info_ref => { isa => 'HashRef', optional => 0 },
    );
    my $run_no = $parm{'run_no'};
    my $queue_info_ref = $parm{'queue_info_ref'};


    # -------------- Notes about Final model selection -----------------

    # Since we have reruns with pertubation and now also forced (or
    # automatic) pertubation the final model is not equal to the
    # original model.


    my $run_results = $queue_info_ref -> {'run_results'};
    my $model = $queue_info_ref -> {'model'};
    my $candidate_model = $queue_info_ref -> {'candidate_model'};
    if (-e 'stats-runs.csv'){
        if ( defined $run_results -> [0] -> {'failed'} ){
            my @raw_row = [($run_no+1,'1','1','run failed: '.($run_results -> [0] -> {'failed'}))];
            $self->raw_results([]) unless defined $self->raw_results;
            push( @{$self->raw_results}, @raw_row );
            push( @{$self->raw_nonp_results}, @raw_row ) if (defined $self->raw_nonp_results);
        }else{
            my @raw_results_rows = @{$queue_info_ref -> {'raw_results'} -> [0]};
            foreach my $row ( @raw_results_rows ){
                shift( @{$row} );
                unshift( @{$row}, $run_no+1 );
            }
            $self->raw_results([]) unless defined $self->raw_results;
            push( @{$self->raw_results}, @raw_results_rows );
            push( @{$self->raw_nonp_results}, @{$queue_info_ref -> {'raw_nonp_results'} -> [0]} )
            if (defined $self->raw_nonp_results and defined $queue_info_ref -> {'raw_nonp_results'} -> [0]);
        }

    } else {
        my $selected = select_best_retry(run_results => $run_results,
                                         accepted_ofv_difference => $self->accepted_ofv_difference);

        open( STAT, '>stats-runs.csv' );
        print STAT Dumper \@{$run_results};
        print STAT "Selected $selected\n";
        close( STAT );

        if ( defined $run_results -> [$selected-1] -> {'failed'} ){
            my @raw_row = [($run_no+1,'1','1','run failed: '.($run_results -> [$selected-1] -> {'failed'}))];
            $self->raw_results([]) unless defined $self->raw_results;
            $self->raw_nonp_results([]) unless defined $self->raw_nonp_results;
            push( @{$self->raw_results}, @raw_row );
            push( @{$self->raw_nonp_results}, @raw_row );

            #partial cleaning
            # TODO update move_retry_files and use it here?
            foreach my $ext (@PsN::nm7_extensions,'.'.$self->modext,'.lst'){
                my $filename = 'psn'.$ext;
                my $use_name = get_retry_name( filename => $filename,
                                               retry => $selected-1);

                # move files to final files in NM_run, to be clear about which one was selected
                mv( $use_name, $filename ) if (-e $use_name);
            }

            if ($self->clean >= 1) {
                if (-e 'nonmem.exe'){
                    #we have observed on Windows 10 that removal of nonmem.exe sometimes fails due to file permissions
                    #hypothesis that removal will succeed after a while, so we try a few times
                    for (my $attempt = 0; $attempt < 10; $attempt++){
                        last if unlink 'nonmem.exe';
                        sleep(3);
                    }
                }

                unlink 'nonmem', 'nonmem5','nonmem6','nonmem7',
                'nonmem5_adaptive','nonmem6_adaptive','nonmem7_adaptive',
                'nonmem_mpi.exe','nonmem_mpi','NONMEM_MPI.exe','FDATA', 'FREPORT', 'FSUBS', 'FSUBS.f','FSUBS.f90',
                'FSUBS.for', 'LINK.LNK', 'FSTREAM', 'FCON.orig', 'FLIB', 'FCON',#'PRDERR', save for error check
                'nmprd4p.mod','nul',
                'fsubs','fsubs.f','fsubs.for','fsubs.f90','FSUBS2','FSUBS_MU.F90';

                unlink 'LINKC.LNK','compile.lnk','gfortran.txt','ifort.txt','garbage.out',
                'newline','nmexec.set','parafile.set','prcompile.set','prdefault.set',
                'prsame.set','psn.log','rundir.set','runpdir.set','temporaryfile.xml';
                unlink 'temp.out','trashfile.xxx','trskip.set','worker.set','xmloff.set';
                unlink 'prsizes.f90','licfile.set','background.set','FSIZES';
                #do not delete INTER, needed for saving data from crashed runs

                unlink(<worker*/*>);
                my @removedir = <worker*>;
                foreach my $remdir (@removedir){
                    rmdir ($remdir) if (-d $remdir);
                }

                if( $self->clean >= 2 ) {
                    unlink( <temp_dir/*> );
                    rmdir( 'temp_dir' );
                }
            }
            if ($self->clean == 5) {        # Copy even failed psn.lst up for clean=5
                my $outfilename;
                if (not $self->model_subdir) {
                    $outfilename = $model->outputs->[0]->full_name;
                } else {
                    $outfilename = $self->base_directory . $self->model_subdir_name . $model->outputs->[0]->filename;
                }
                copy("psn.lst", $outfilename);
            }
        } else {
            my @raw_results_rows = @{$queue_info_ref -> {'raw_results'} -> [$selected-1]};

            foreach my $row ( @raw_results_rows ){
                shift( @{$row} );
                unshift( @{$row}, $run_no+1 );
            }

            $self->raw_results([]) unless defined $self->raw_results;
            $self->raw_nonp_results([]) unless defined $self->raw_nonp_results;
            push( @{$self->raw_results}, @raw_results_rows );
            if (defined $self->raw_nonp_results and defined $queue_info_ref -> {'raw_nonp_results'} -> [$selected-1]){
                push( @{$self->raw_nonp_results}, @{$queue_info_ref -> {'raw_nonp_results'} -> [$selected-1]} );
            }

            $self -> move_model_and_output( final_model   => $candidate_model,
                                            model         => $model,
                                            use_run       => $selected ? $selected : '' );
        }
    }
}

sub get_retry_name
{
    #static no shift
    my %parm = validated_hash(\@_,
                              retry => { isa => 'Int', optional => 0 },
                              crash => { isa => 'Maybe[Int]', optional => 1 },
                              filename => { isa => 'Str', optional => 0 },
        );
    my $retry = $parm{'retry'};
    my $crash = $parm{'crash'};
    my $filename = $parm{'filename'};
    #input retry is actually the try index, increase with 1 to get retry number

    $retry++;

    my ($base,$msftype,$extension) = model::problem::msfi::get_basename_msftype_extension(filename => $filename);

    my $crashstring='';
    if (defined $crash){
        $crashstring = "-step$crash";
    }
    $filename = "$base-$retry".$crashstring.$msftype.$extension;

    return $filename;
}

sub set_msfo_to_msfi
{
    #static
    my %parm = validated_hash(\@_,
                              candidate_model => { isa => 'model', optional => 1 },
                              retry => { isa => 'Int', optional => 1 },
                              queue_info => { isa => 'HashRef', optional => 1 },
                              base_msfo_name => { isa => 'Maybe[Str]', optional => 1 },
    );
    my $candidate_model = $parm{'candidate_model'};
    my $retry = $parm{'retry'};
    my $queue_info = $parm{'queue_info'};
    my $base_msfo_name = $parm{'base_msfo_name'};

    #this assumes prob index 0, record index 0
    my $arr = $queue_info->{'model'}->problems->[0]->get_msfo_filenames;

    my $filename = $arr->[0] if (scalar(@{$arr})>0);
    $filename = $base_msfo_name if (defined $base_msfo_name);
    unless (defined $filename) {
        ui -> print( category => 'all',  message  => "Warning, no MSFO option in model, ".
            "set_msfo_to_msfi will fail (used when handling option maxevals)",
            newline => 1);
    }

    my $msfo = get_retry_name('filename' => $filename,
                              'retry' => $retry);

    my $msfi;

    if ($candidate_model->outputs->[0]->msfo_has_terminated) {
        $msfi = $msfo . '-step' . ($queue_info->{'crashes'} - 1);
        $candidate_model->remove_records(type => 'estimation');
    } else {
        $msfi = get_retry_name(
            'filename' => $filename,
            'retry' => $retry,
            crash => $queue_info->{'crashes'}    );
    }

    unless (-e $msfi) {
        ui -> print( category => 'all', message  => "Warning, MSFO file $msfi does not exist, ".
            "set_msfo_to_msfi will fail (used when handling option maxevals)",
            newline => 1);
    }

    $candidate_model->set_first_problem_msfi(msfiname => $msfi);

    $candidate_model->_write;
}

sub reset_msfo
{
    my %parm = validated_hash(\@_,
        candidate_model => { isa => 'model', optional => 1 },
        basic_model => { isa => 'model', optional => 1 },
        base_msfo_name => { isa => 'Maybe[Str]', optional => 1 },
    );
    my $candidate_model = $parm{'candidate_model'};
    my $basic_model = $parm{'basic_model'};
    my $base_msfo_name = $parm{'base_msfo_name'};

    my $model_modified = 0;

    if (defined $candidate_model ->problems->[0]->msfis and
        scalar(@{$candidate_model ->problems->[0]->msfis})>0){
        $candidate_model->remove_records(type=>'msfi',problem_numbers => [1]);
        foreach my $param ( 'thetas', 'omegas', 'sigmas' ) {
            $candidate_model -> problems->[0] -> $param( Storable::dclone( $basic_model -> problems->[0]-> $param ) );
        }
        $model_modified = 1;
    }

    my $filename = $basic_model -> problems->[0]-> get_msfo_filenames->[0];
    $filename = $base_msfo_name if (defined $base_msfo_name);

    if (defined $filename){
        $candidate_model-> rename_msfo(name => $filename);
    }

    return $model_modified;
}

sub cut_thetas
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        candidate_model => { isa => 'model', optional => 1 },
        cutoff_thetas => { isa => 'ArrayRef[Num]', optional => 1 },
        output_file => { isa => 'output', optional => 1 }
    );
    my $candidate_model = $parm{'candidate_model'};
    my @cutoff_thetas = defined $parm{'cutoff_thetas'} ? @{$parm{'cutoff_thetas'}} : ();
    my $output_file = $parm{'output_file'};

    $candidate_model -> update_inits( from_output => $output_file,
        update_omegas => 1,
        update_sigmas => 1,
        update_thetas => 1);

    foreach my $th_num ( @cutoff_thetas ) {
        my $init_val = $candidate_model ->
        initial_values( parameter_type    => 'theta',
            parameter_numbers => [[$th_num]])->[0][0];
        if (abs($init_val)<=$self->cutoff) {
            $candidate_model->initial_values(parameter_type => 'theta',
                parameter_numbers => [[$th_num]],
                new_values =>[[0]]);
            $candidate_model->fixed(parameter_type => 'theta',
                parameter_numbers => [[$th_num]],
                new_values => [[1]] );
        }
    }
}

sub prepare_raw_results
{
    my $self = shift;

    my @new_header;

    # As of PsN version 2.1.8, we don't handle problems and
    # subproblems in any of the tools but modelfit.

    unless (defined $self->raw_results) {
        croak("Failed to collect raw results from lst-file(s).\n".
            "Check lst-file or ".$self->general_error_file." or ".$self->nmtran_error_file." in ".
              "NM_run directory/-ies for errors.\n");
    }

    #need to sort on $PROB also? this is only by modelnum. sse requires sort by prob
    @{$self->raw_results} = sort( {$a->[0] <=> $b->[0]} @{$self->raw_results} );
    if( defined $self->raw_nonp_results ) {
        @{$self->raw_nonp_results} = sort( {$a->[0] <=> $b->[0]} @{$self->raw_nonp_results} );
    }
    my %max_hash = %{$self->max_hash};

    #in scm we change the header. need to fix max hash also
    &{$self->_raw_results_callback}( $self, \%max_hash ) if ( defined $self->_raw_results_callback );

    # ---------------------------  Create a header  ----------------------------

    # header

    my %param_names;
    my @params = ( 'theta', 'omega', 'sigma' );
    foreach my $param ( @params ) {
        #set header from first model, will be errors if labels different order in other models
        my $labels = $self -> models -> [0] -> labels( parameter_type => $param );
        #we just assume single $PROBLEM here
        $param_names{$param} = $labels -> [0] if ( defined $labels );
    }

    $self->raw_results_header([]) unless defined $self->raw_results_header;
    foreach my $name ( @{$self->raw_results_header} ) {
        my $success;
        my @params = ( 'theta', 'omega', 'sigma', 'npomega', 'shrinkage_eta', 'shrinkage_iwres','eigen' );
        foreach my $param ( @params ) {
            if ( $name eq $param ){
                if ( $name eq 'shrinkage_eta' ){
                    for ( my $i = 1; $i <= $max_hash{ $name }; $i++ ) {
                        push ( @new_header, 'shrinkage_eta'.$i.'(%)' );
                    }
                } elsif ( $name eq 'shrinkage_iwres' ){
                    push ( @new_header, 'shrinkage_iwres(%)' );
                } else {
                    for ( my $i = 1; $i <= $max_hash{ $name }; $i++ ) {

                        my $label = undef;
                        if( defined $param_names{$name} -> [$i-1] ){
                            $label = $param_names{$name} -> [$i-1] ;
                        }
                        if( defined $label ){
                            push( @new_header, $label );
                        } else {
                            push( @new_header, uc(substr($name,0,2)).$i );
                        }
                    }
                }
                $success = 1;
                last;
            } elsif ( $name eq 'se'.$param ) {
                for ( my $i = 1; $i <= $max_hash{ $name }; $i++ ) {
                    my $label = undef;

                    if( defined $param_names{$param} -> [$i-1] ){
                        $label = $param_names{$param} -> [$i-1] ;
                    }
                    if( defined $label ){
                        push( @new_header, 'se' . $label );
                    } else {
                        push( @new_header, 'se' . uc(substr($name,2,2)).$i );
                    }
                }
                $success = 1;
                last;
            }
        }
        unless( $success ){
            push( @new_header, $name ) unless ((defined $max_hash{$name}) and $max_hash{$name}== 0); #only print nburn_set if saem/bayes
        }
    }

    $self->raw_results_header(\@new_header);

    # ---------------------------  Create a nonp header  ----------------------------

    my @new_nonp_header;
    $self->raw_nonp_results_header([]) unless defined $self->raw_nonp_results_header;
    foreach my $name ( @{$self->raw_nonp_results_header} ) {
        my $success;
        my @params = ( 'npeta','npomega','npofv');
        if ( $name eq 'npeta' ){
            for ( my $i = 1; $i <= $max_hash{ $name }; $i++ ) {
                push ( @new_nonp_header, 'EXP(ETA'.$i.')' );
            }
            $success = 1;
        }elsif ( $name eq 'npofv' ){
            push ( @new_nonp_header, 'npofv' );
            $success = 1;
        }elsif ( $name eq 'npomega' ){
            my $row=1;
            my $col=1;
            for ( my $i = 1; $i <= $max_hash{ $name }; $i++ ) {
                push ( @new_nonp_header,'OMEGA('.$row.','.$col.')' );
                $col++;
                if ($col>$row){
                    $row++;
                    $col=1;
                }
            }
            $success = 1;
        }
        unless( $success ){
            push( @new_nonp_header, $name );
        }
    }

    $self->raw_nonp_results_header(\@new_nonp_header);
}

sub print_raw_results
{
    my $self = shift;

  ## print raw_results array

  my ($dir,$file);
  my $raw_file;
  if ( defined $self->raw_results ) {
        $raw_file = $self->raw_results_file->[0];

    ($dir,$file) = OSspecific::absolute_path( $self->directory, $raw_file );

    my $append = $self->raw_results_append ? '>>' : '>';
    open( RRES, $append.$dir.$file );

    if( (not $self->raw_results_append) and $PsN::output_header ){
      #To avoid problems if cells contain commas
      print RRES join(",",map {s/\"/\"\"/g; '"'.$_.'"'} @{$self->raw_results_header} ),"\n";
    }

    for ( my $i = 0; $i < scalar @{$self->raw_results}; $i++ ) {
      my @result_array = @{$self->raw_results->[$i]};
      map( $_ = defined $_ ? $_ : $PsN::out_miss_data, @result_array );
      print RRES join(',',@result_array ),"\n";
    }
    close( RRES );
  }

  ## print raw_nonp_results

  if( defined $self->raw_nonp_results and $self->any_nonparametric_step) {
    my ($npdir,$npfile) = OSspecific::absolute_path( $self->directory,
                         $self->raw_nonp_file->[0] );
    my $append = $self->raw_results_append ? '>>' : '>';
    open( RRES, $append.$dir.$npfile ); #use the same dir as raw_results, only new filename

    if( (not $self->raw_results_append) and $PsN::output_header ){
      #To avoid problems if cells contain commas
            $self->raw_nonp_results_header([]) unless defined $self->raw_nonp_results_header;
      print RRES join(",",map {s/\"/\"\"/g; '"'.$_.'"'} @{$self->raw_nonp_results_header} ),"\n";
    }

    for ( my $i = 0; $i < scalar @{$self->raw_nonp_results}; $i++ ) {
      my @result_array;
      if ( defined $self->raw_nonp_results->[$i] ) {
                @result_array = @{$self->raw_nonp_results->[$i]};
                map( $_ = defined $_ ? $_ : $PsN::out_miss_data, @result_array );
      }
      print RRES join(',',@result_array ),"\n";
    }
    close( RRES );
  }

  ## raw_line_structure should be printed to disk here for fast
  ## resumes. In the future

  $self->raw_line_structure->write( $dir.'raw_results_structure' );
}

sub _get_run_options
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         run_id => { isa => 'Int', optional => 1 },
         work_dir => { isa => 'Str', optional => 1 }
    );
    my $run_id = $parm{'run_id'};
    my %options_hash;
    my $work_dir = $parm{'work_dir'};

    %options_hash = ( 'cut_thetas_rounding_errors' => undef,
        'cut_thetas_maxevals' => undef,
        'handle_hessian_npd' => undef,
        'cutoff_thetas' => 'ARRAY',
        'tweak_inits' => undef,
        'retries' => undef,
        'picky' => undef,
        'significant_digits_accept' => undef,
        'nm_version' => undef );

    foreach my $option ( keys %options_hash ) {

    # This loops allows run specific parameters to be
    # specified. We check that the parameter given is an
    # array we take out one element of that array and pass it
    # as a run specific parameter. If the option is specified
    # as being an "ARRAY" type in the has above, but there are
    # no subarrays, we send the entire array as an parameter.

        if ( ref( $self -> {$option} ) eq 'ARRAY' ) {
            if ( ref( $self -> {$option} -> [$run_id] ) ne 'ARRAY' and $options_hash{$option} eq 'ARRAY' ) {
                $options_hash{$option} = $self -> {$option};
            } else {
                $options_hash{$option} = $self -> {$option} -> [$run_id];
      }
        } else {
      $options_hash{$option} = $self -> {$option};
    }
    }

    return \%options_hash;
}

sub run_nonmem
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         nm_version => { isa => 'Str', optional => 1 },
         queue_info => { isa => 'Ref', optional => 1 },
         run_no => { isa => 'Int', optional => 0 },
         queue_map => { isa => 'Ref', optional => 1 }
    );
    my $nm_version = $parm{'nm_version'};
    my $queue_info = $parm{'queue_info'};
    my $run_no = $parm{'run_no'};
    my $queue_map = $parm{'queue_map'};

    my $candidate_model = $queue_info->{'candidate_model'};
    my $tries = $queue_info->{'tries'};
    my $model = $queue_info->{'model'};

    #an ls might make files sync and become visible
    my $dirt = `ls -la 2>&1` unless ($Config{osname} eq 'MSWin32');


    if (-e $self->nmtran_error_file) {
        #give fake pid and go directly to restart needed. Do not copy or move anything
        $queue_map->{'rerun_'.$run_no} = $run_no; #Fake pid
        return;
    } elsif (-e 'stats-runs.csv') {
        #possible reasons:
        #a) Restart after -clean > 1. Then we do not know the true restart number of psn.lst
        #b) Restart after completed run -clean <= 1. Best retry is copied to psn.lst, don't know which.

        #give fake pid and go directly to restart needed. Do not copy or move anything
        $queue_map->{'rerun_'.$run_no} = $run_no; #Fake pid
        return;
    } elsif (-e 'psn.lst') {
        #possible reason
        #Restart after main PsN process killed before sge/slurm NONMEM run finished.

        # Then check for ($tries+1).lst. If it exists, then *move*
        #psn.mod, psn.lst etc to psn-prevrun.lst etc
        # to save it for later and continue to next if -clause.
        # If $tries+1 does not exist then do not run anything, want to go directly to restart needed.
        # Create a fake pid and return, do not copy or move files.

        if( -e 'psn-' . ( $tries + 1 ) . '.lst'){
            foreach my $filename (@{$candidate_model->output_files}, 'psn.' . $self->modext, $self->base_msfo_name) {
                next unless (defined $filename);
                my $new_name = $filename;
                unless( $new_name =~ s/\.([^.]+)$/-prevrun.$1/ ){
                    $new_name .= "-prevrun";
                }
                mv($filename,$new_name);
            }
        } else {
            $queue_map->{'rerun_'.$run_no} = $run_no; #Fake pid
            return;
        }
    }elsif(-e 'psn-prevrun.lst'){
        #did not have psn.lst but do have prevrun.lst
        # Then check for ($tries+1).lst. If it exists, do nothing.
        # If it does not exist then make it look like this was just run: Move all files to psn.lst etc
        # Then let restart_needed move files to numbered retry files.
        # Create a fake pid and return, do not copy or move any more files.
        unless( -e 'psn-' . ( $tries + 1 ) . '.lst'){
            foreach my $filename (@{$candidate_model->output_files}, 'psn.' . $self->modext, $self->base_msfo_name) {
                next unless (defined $filename);
                my $new_name = $filename;
                unless( $new_name =~ s/\.([^.]+)$/-prevrun.$1/ ){
                    $new_name .= "-prevrun";
                }
                mv($new_name,$filename);
            }
            $queue_map->{'rerun_'.$run_no} = $run_no; #Fake pid
            return;
        }
    }

    # We do not expect any values of rerun lower than 1 here. (a bug otherwise...)
    if (not -e 'psn-' . ($tries + 1) . '.lst' ) {
        #missing psn-1.lst etc

        # Execution step
        my $nonmem_run;

        if ($self->run_local) {
            if ($Config{osname} eq 'MSWin32') {
                $nonmem_run = nonmemrun::localwindows->new(full_path_runscript => $self->full_path_runscript);
            } elsif ($self->run_on_mosix) {
                $nonmem_run = nonmemrun::mosix->new(full_path_runscript => $self->full_path_runscript);
            } else {
                $nonmem_run = nonmemrun::localunix->new(full_path_runscript => $self->full_path_runscript);
            }
            $nonmem_run->display_iterations($self->display_iterations);
        } elsif ($self->run_on_lsf ) {
            $nonmem_run = nonmemrun::lsf->new(
                full_path_runscript => $self->full_path_runscript,
                lsf_job_name => $self->lsf_job_name,
                lsf_project_name => $self->lsf_project_name,
                lsf_queue => $self->lsf_queue,
                lsf_resources => $self->lsf_resources,
                lsf_ttl => $self->lsf_ttl,
                lsf_sleep => $self->lsf_sleep,
                lsf_options => $self->lsf_options,
            );
        } elsif ($self->run_on_ud) {
            $nonmem_run = nonmemrun::ud->new(
                full_path_runscript => $self->full_path_runscript,
                directory => $self->directory,
                run_no => $run_no,
            );
        } elsif ($self->run_on_sge ) {
            $nonmem_run = nonmemrun::sge->new(
                prepend_flags => $self->sge_prepend_flags,
                full_path_runscript => $self->full_path_runscript,
                resource => $self->sge_resource,
                queue => $self->sge_queue,
            );
        } elsif ($self->run_on_slurm) {
            $nonmem_run = nonmemrun::slurm->new(
                full_path_runscript => $self->full_path_runscript,
                email_address => $self->email_address,
                send_email => $queue_info->{'send_email'},
                prepend_flags => $self->slurm_prepend_flags,
                max_runtime => $self->max_runtime,
                partition => $self->slurm_partition,
                account => $self->slurm_account,
                cluster => $self->slurm_cluster,
            );
        } elsif ($self->run_on_torque) {
            $nonmem_run = nonmemrun::torque->new(
                prepend_flags => $self->torque_prepend_flags,
                full_path_runscript => $self->full_path_runscript,
                torque_queue => $self->torque_queue,
            );
        } elsif ($self->run_on_zink) {
            $nonmem_run = nonmemrun::zink->new(
                full_path_runscript => $self->full_path_runscript,
            );
        }

        $nonmem_run->model($queue_info->{'model'});
        $nonmem_run->nodes($self->nodes);
        $nonmem_run->parafile($self->parafile);
        $nonmem_run->nmfe_options($self->nmfe_options);

        $queue_info->{'nonmemrun'} = $nonmem_run;
        my $jobId = $nonmem_run->submit;
        if ($jobId == -1) {
            $queue_map->{'fail_' . $run_no} = $run_no;
        } else {
            $queue_map->{$jobId} = $run_no;
        }

    } else {
        #psn-(tries+1).lst exists

        # we want to recheck the
        # output files for errors. Therefore we put a fake entry in
        # queue_map to trigger "restart_needed()".

        #Make it look like the retry has just been run, and not yet
        #moved to numbered retry files in restart_needed

        # TODO update move_retry_files and use it here?

        foreach my $filename (@{$candidate_model->output_files}, 'psn.' . $self->modext, $self->base_msfo_name) {
            next unless (defined $filename);

            my $retry_name = get_retry_name(filename => $filename,
                                            retry => $tries);
            mv($retry_name, $filename) if (-e $retry_name);
        }

        my $fname = get_retry_name(filename => 'psn.lst',
                                   retry => $tries);
        $queue_map->{'rerun_' . $run_no} = $run_no; #Fake pid
    } # end of "not -e psn-$tries.lst or rerun"
}

sub diagnose_lst_errors
{
    #static no shift
    my %parm = validated_hash(\@_,
                              missing => { isa => 'Bool', optional => 0 },
                              have_stats_runs => { isa => 'Bool', optional => 0 },
                              parsed_successfully => { isa => 'Bool', optional => 1 },
                              interrupted => { isa => 'Maybe[Bool]', optional => 1 },
                              run_no  => { isa => 'Int', optional => 0 },
                              modext  => { isa => 'Str', optional => 0 },
                              run_local => { isa => 'Bool', optional => 0 },
                              nmtran_error_file => { isa => 'Str', optional => 0 },
        );
    my $missing = $parm{'missing'};
    my $have_stats_runs = $parm{'have_stats_runs'};
    my $parsed_successfully  = $parm{'parsed_successfully'};
    my $interrupted  = $parm{'interrupted'};
    my $run_no  = $parm{'run_no'};
    my $modext  = $parm{'modext'};
    my $run_local  = $parm{'run_local'};
    my $nmtran_error_file  = $parm{'nmtran_error_file'};

    my $failure;
    my $failure_mess;
    my $restart_possible=0;
    my $store_general_error=0;

    if (not (-e 'FDATA')) {
        if (-e 'locfile.set' or -e 'maxlim.set' or -e 'background.set' or -e 'licfile.set' or -e 'nmexec.set' or -e 'rundir.set' or -e 'runpdir.set' or -e 'worker.set'){
            $failure = 'There was an error when running nmfe, NMtran could not be initiated (the NMtran output file FDATA is missing)';
            $failure_mess = "\nThere was an error when running nmfe, NMtran could not be initiated for model ".($run_no+1).' ';
            $failure_mess .= ' - check that nmfe-related options are correct, if used (-nmfe_options, -parafile...)';

        } else{
            $failure = 'NMtran could not be initiated (the NMtran output file FDATA is missing)';
            $failure_mess = "\nNMtran could not be initiated (the NMtran output file FDATA is missing). There is no output for model ".($run_no+1).'.';
            if (-e 'job_submission_error'){
                open( MESS, '<job_submission_error' );
                $restart_possible = 1 if ($run_local); #will only restart if handle_crashes and crash_restarts > 0
                $failure = '';
                $failure_mess = "Job submission error:\n";
                while(<MESS>) {
                    chomp;
                    $failure .= $_.' ';
                    $failure_mess .= $_."\n";
                }
                close( MESS );
            }elsif ($run_local) {
                $failure .= ' - check that the nmfe script can be run independent of PsN';
                $failure_mess .= ' - check that the nmfe script can be run independent of PsN';
            }else{
                if ($missing and (not $have_stats_runs) and (not -e 'nmfe_output.txt')){
                    $restart_possible = 1; #will only restart if handle_crashes and crash_restarts > 0
                    $failure .= ' - this could be a cluster file sync error';
                    $failure_mess = "\nNMtran could not be initiated (the NMtran output file FDATA is missing in NM_run".($run_no+1).'). '.
                        ' - this could be a cluster file sync error';
                }else{
                    $failure .= ' - check cluster status and cluster settings in psn.conf';
                }
            }
        }
        $store_general_error=1 unless ($have_stats_runs);
    } elsif (not(-e 'FREPORT')) {
        $failure = 'NMtran failed';
        $failure_mess="NMtran failed. There is no output for model ".($run_no+1) ;
        if (($run_no < 2) and (-e 'FMSG' or -e $nmtran_error_file)){
            #if low run number then add nmtran messages to failure_mess, which will be printed to screen later
            #do not do this for high run numbers (we do not want 100 prints for e.g. a bootstrap)
            my $fname = 'FMSG';
            $fname = $nmtran_error_file if (-e $nmtran_error_file);
            open( FILE, "$fname" ) ||    warn "Could not open $fname for reading";
            my @lines = <FILE>;
            close( FILE );
            $failure_mess .= ". Contents of FMSG:\n";
            foreach my $string (@lines){
                chomp($string);
                $failure_mess .= $string."\n";
            }
        }
        $store_general_error=1 unless ($missing);
    } elsif (not(-e 'nonmem.exe' or -e 'NONMEM_MPI.exe' or -e 'nonmem_mpi.exe' or -e 'nonmem_mpi' or -e 'nonmem' or -e 'nonmem5' or -e 'nonmem6' or -e 'nonmem7' )){
        $failure = 'It seems like the compilation failed';
        $failure_mess="It seems like the compilation failed." ;

        unless ($have_stats_runs){
            if (defined $PsN::config -> {'_'} -> {'transient_compilation_errors'} and
                   $PsN::config -> {'_'} -> {'transient_compilation_errors'} > 0){
                $restart_possible = 1; #will only restart if handle_crashes and crash_restarts > 0
                $failure .= ' - could be transient';
                $failure_mess = "It seems like Fortran compilation by NONMEM's nmfe script failed in NM_run".($run_no+1).", - could be transient.";
                sleep($PsN::config -> {'_'} -> {'transient_compilation_errors'}); #long sleep before doing something new, hope this solves the issue
            }else{
                $failure_mess = "It seems like Fortran compilation by NONMEM's nmfe script failed. Cannot start NONMEM.\n".
                    "Go to the NM_run".($run_no+1)." subdirectory and run psn.".$modext." with NONMEM's nmfe script to diagnose the problem.";
            }
        }
        $store_general_error=1 unless ($have_stats_runs);
    } elsif (not $missing and (defined $interrupted) and $interrupted){
        $restart_possible = 1;
        $failure = 'NONMEM iterations interrupted';
        $failure_mess = "It seems NONMEM iterations were interrupted before finishing";
    } else{
        if ($missing){
            $failure = 'the lst-file does not exist in NM_run'.($run_no+1);
            $failure_mess = 'NONMEM run failed: the lst-file does not exist in NM_run'.($run_no+1);
            $store_general_error=1 unless ($have_stats_runs);
        }else{
            $failure = 'NONMEM run failed';
            $failure_mess = "NONMEM run failed. Check the lst-file in NM_run" . ($run_no+1) . " for errors";
            $store_general_error=1 unless ($have_stats_runs);
        }
    }

    return [$failure,$failure_mess,$restart_possible,$store_general_error];
}

sub general_error
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              message => { isa => 'Str', optional => 1 }
        );
    my $message = $parm{'message'};
    open( FILE, '>>' . $self->general_error_file );
    print FILE "\n" . $message . "\n";
    close(FILE);
}

sub passed_picky
{
    #static, no shift
    my %parm = validated_hash(\@_,
                              minimization_successful => { isa => 'ArrayRef', optional => 0 },
                              minimization_message => { isa => 'ArrayRef', optional => 0 },
                              picky =>  { isa => 'Bool', optional => 0 },
                              probnum  =>  { isa => 'Int', optional => 0 },
        );

    my $minimization_successful = $parm{'minimization_successful'};
    my $minimization_message = $parm{'minimization_message'};
    my $picky = $parm{'picky'};
    my $probnum = $parm{'probnum'};

    #probnum is from output->get_estimation_evaluation_problem_number, can be negative
    my $passed_picky = 0;

    if ($picky and ($probnum > 0)){
        #if not picky set, or no probnum to check, then always return false
        croak ("probnum is $probnum but number of problems is ".scalar(@{$minimization_successful}))
            unless (scalar(@{$minimization_successful}) >= $probnum);
        if ((defined $minimization_successful -> [$probnum-1] ) and
            $minimization_successful -> [$probnum-1][0] ) {
            $passed_picky = 1;
            if ((defined $minimization_message -> [$probnum-1]) and
                (defined $minimization_message -> [$probnum-1][0])){
                for ( @{$minimization_message -> [$probnum-1][0]} ) {
                    if ( /0COVARIANCE STEP ABORTED/ or
                         /0PROGRAM TERMINATED BY OBJ/ or
                         /0ESTIMATE OF THETA IS NEAR THE BOUNDARY AND/ or
                         /0PARAMETER ESTIMATE IS NEAR ITS BOUNDARY/ or
                         /0R MATRIX ALGORITHMICALLY SINGULAR/ or
                         /0S MATRIX ALGORITHMICALLY SINGULAR/ ) {
                        $passed_picky = 0;
                        last;

                    }
                }
            }
        }
    }
    return $passed_picky;

}

sub store_results_old_run
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         retries => { isa => 'Int', optional => 1 },
         picky => { isa => 'Bool', default => 0, optional => 1 },
         run_no => { isa => 'Int', optional => 0 },
         tries => { isa => 'Int', optional => 0 },
         queue_info_ref => { isa => 'HashRef', optional => 0 }
    );
    my $retries = $parm{'retries'};
    my $picky = $parm{'picky'};
    my $run_no = $parm{'run_no'};
    my $tries = $parm{'tries'};
    my $queue_info_ref = $parm{'queue_info_ref'};

    my $model = $queue_info_ref->{'model'};
    my $candidate_model = $queue_info_ref->{'candidate_model'};
    my $run_results = $queue_info_ref->{'run_results'};


    my $failure;
    my $failure_mess;
    my ($raw_results_row, $nonp_row);
    #problem here if got rid of theta omega sigma as part of handle maxevals.
    #candidate model has no labels in that case, send label_model along
    ($raw_results_row, $nonp_row) =
        $self -> create_raw_results_rows( max_hash => $self->max_hash,
                                          model => $candidate_model,
                                          label_model => $model,
                                          model_number => $run_no + 1,
                                          raw_line_structure => $self->raw_line_structure);

    if (-e 'psn.lst'){
        my $output_file = $candidate_model -> outputs -> [0];
        #is this needed?
        $output_file -> _read_problems;

        my $evaluation_probnum = $output_file->get_estimation_evaluation_problem_number(); #if neg then no est step run

        $queue_info_ref -> {'raw_results'} -> [$tries] = $raw_results_row;
        $queue_info_ref -> {'raw_nonp_results'} -> [$tries] = $nonp_row;
        foreach my $category ( 'minimization_successful', 'covariance_step_successful',
                               'covariance_step_warnings', 'estimate_near_boundary',
                               'significant_digits', 'ofv' ){
            my $index = (abs($evaluation_probnum)-1);
            $run_results -> [$tries] -> {$category} = $output_file -> get_single_value(attribute => $category,
                                                                                       problem_index => $index);
        }

        $run_results -> [$tries] -> {'pass_picky'} = 0;

        if( $output_file->parsed_successfully() and not defined $output_file->problems ) {
            # This should not happen if we are able to parse the output file correctly
            $run_results -> [$tries] -> {'failed'} = 'lst-file file exists but could not be parsed correctly';
            $self->general_error(message => 'lst-file file exists but could not be parsed correctly');
        }else{
            $run_results -> [$tries] -> {'pass_picky'} = passed_picky(minimization_successful => $output_file -> minimization_successful(),
                                                                      minimization_message => $output_file -> minimization_message(),
                                                                      probnum => $evaluation_probnum,
                                                                      picky => $picky);
        }
        $output_file -> flush;

    } else {
        #we do not have any psn.lst. Cannot happen if nmfe
        #and overtime kill by system, at least model and NMtran mess
        my $ref = diagnose_lst_errors(missing => 1,
                                      run_no => $run_no,
                                      have_stats_runs => 1,
                                      modext  => $self->modext,
                                      run_local => $self->run_local,
                                      nmtran_error_file => $self->nmtran_error_file);

        $failure = $ref->[0];
        $failure_mess = $ref->[1];
        $self->general_error(message => $failure_mess) if ($ref->[3]); #if store_general_error is true
        $run_results -> [$tries] -> {'failed'} = $failure; #different texts for different causes
    }

}

sub maxeval_exceeded
{
    #static no shift
    my %parm = validated_hash(\@_,
                              output => { isa => 'output', optional => 0 },
                              probnum => { isa => 'Int', optional => 0 }
        );
    my $output = $parm{'output'};
    my $probnum = $parm{'probnum'};

    my $value = 0;

    if ($output-> parsed_successfully() and (defined $output-> problems) and
        ($probnum > 0)
        and (defined $output -> minimization_message()) and
        (defined $output -> minimization_message()->[($probnum-1)]) and
        (defined $output -> minimization_message()->[($probnum-1)][0])
        ){
        for ( @{$output -> minimization_message() -> [($probnum-1)][0]} ) {
            if ( /\s*MAX. NO. OF FUNCTION EVALUATIONS EXCEEDED\s*/) {
                $value = $output -> get_single_value(attribute=> 'feval',
                                                     problem_index => ($probnum-1),
                                                     subproblem_index => 0);
                last;
            }
        }
    }
    return $value;
}

sub hessian_error
{
    #static no shift
    my %parm = validated_hash(\@_,
                              output => { isa => 'output', optional => 0 },
                              probnum => { isa => 'Int', optional => 0 }
        );
    my $output = $parm{'output'};
    my $probnum = $parm{'probnum'};

    my $value = 0;

    if ($output-> parsed_successfully() and (defined $output-> problems) and
        ($probnum > 0 )
        and (defined $output -> minimization_message()) and
        (defined $output -> minimization_message()->[($probnum-1)]) and
        (defined $output -> minimization_message()->[($probnum-1)][0])
        ){
        my $line = join(' ',@{$output -> minimization_message() -> [($probnum-1)][0]});
        if ($line =~ /\s*NUMERICAL HESSIAN OF OBJ. FUNC. FOR COMPUTING CONDITIONAL ESTIMATE\s*IS NON POSITIVE DEFINITE\s*/){
            $value = 1;
        }
    }
    return $value;
}

sub significant_digits_accepted
{
    #static
    my %parm = validated_hash(\@_,
                              run_results => { isa => 'ArrayRef', optional => 0 },
                              significant_digits_accept => { isa => 'Maybe[Num]', optional => 0 },
                              try => { isa => 'Int', optional => 0 },
    );
    my $run_results = $parm{'run_results'};
    my $significant_digits_accept = $parm{'significant_digits_accept'};
    my $try = $parm{'try'};

    croak("input error try $try in significant_digits_accepted") unless ($try >= 0);
    croak("input error signficant_digits_accept $significant_digits_accept in significant_digits_accepted") if
        (defined $significant_digits_accept and ($significant_digits_accept < 0));

    my $accept = 0;
    $accept = 1 if ((not $run_results -> [$try] -> {'minimization_successful'}) and
                    (defined $significant_digits_accept and ($significant_digits_accept > 0)) and
                    (defined $run_results -> [$try] -> {'significant_digits'}) and
                    ($run_results -> [$try] -> {'significant_digits'} >= $significant_digits_accept));

    return $accept;
}

sub local_minimum
{
    #static
    my %parm = validated_hash(\@_,
                              run_results => { isa => 'ArrayRef', optional => 0 },
                              accepted_ofv_difference => { isa => 'Num', optional => 0 },
                              reduced_model_ofv => { isa => 'Maybe[Num]', optional => 0 },
                              have_accepted_run => { isa => 'Bool', optional => 0 },
                              try => { isa => 'Int', optional => 0 },
                              probnum=> { isa => 'Int', optional => 0 },
    );
    my $run_results = $parm{'run_results'};
    my $accepted_ofv_difference = $parm{'accepted_ofv_difference'};
    my $reduced_model_ofv = $parm{'reduced_model_ofv'};
    my $have_accepted_run = $parm{'have_accepted_run'};
    my $try = $parm{'try'};
    my $probnum = $parm{'probnum'};

    croak("input error local_minimum accepted_ofv_difference $accepted_ofv_difference") if ($accepted_ofv_difference < 0);

    my $is_local_minimum = 0;

    if ($probnum > 0){
        if ((defined $run_results->[$try]) and  (defined $run_results->[$try]->{'ofv'})){
            if ((defined $reduced_model_ofv)  and
                ($reduced_model_ofv < ($run_results->[$try]->{'ofv'} - $accepted_ofv_difference))){
                $is_local_minimum = 1;
            }else {
                if (($try > 0) and (not $have_accepted_run)){
                    for (my $tr=0; $tr < $try; $tr++){
                        if (defined $run_results -> [$tr] -> {'ofv'}
                            and ($run_results -> [$tr] -> {'ofv'}<
                                 ($run_results->[$try]->{'ofv'} - $accepted_ofv_difference))){
                            $is_local_minimum = 2;
                            last;
                        }
                    }
                }
            }
        }
    }
    return $is_local_minimum;
}

sub move_retry_files
{
    #static no shift
    my %parm = validated_hash(\@_,
                              retry => { isa => 'Int', optional => 0 },
                              crash => { isa => 'Int', optional => 1 },
                              filenames => { isa => 'ArrayRef', optional => 0 },
                              nm_major_version => { isa => 'Int', optional => 0 },
                              nm_minor_version => { isa => 'Maybe[Num]', optional => 1 }
        );
    my $retry = $parm{'retry'};
    my $crash = $parm{'crash'};
    my $filenames = $parm{'filenames'};
    my $nm_major_version = $parm{'nm_major_version'};
    my $nm_minor_version = $parm{'nm_minor_version'};
    #input retry is actually the try index, is increased with 1 in get_retry_name to get retry number

    croak("empty array into move_retry_files") unless (scalar(@{$filenames})>0);

    my $eta_shrinkage_name;
    my $iwres_shrinkage_name;

    my $stopmess ='moved ';
    foreach my $filename ( @{$filenames}){
        next unless (defined $filename);
        my $old_name = $filename;
        if (defined $crash){
            #name without crash
            $old_name = get_retry_name( filename => $filename,
                                        retry => $retry);
        }

        my $new_name = get_retry_name( filename => $filename,
                                       retry => $retry,
                                       crash => $crash);
        if ($filename =~ /psn\_etas/){
            $eta_shrinkage_name = $new_name;
        }
        if ($filename =~ /psn\_wres/){
            $iwres_shrinkage_name = $new_name;
        }
        mv( $old_name, $new_name );
        $stopmess .= "$old_name to $new_name, ";

    }
    return ($stopmess,$eta_shrinkage_name,$iwres_shrinkage_name);
}

sub retries_decide_what_to_do
{
    #static no shift
    my %parm = validated_hash(\@_,
                              estimation_step_run => { isa => 'Bool', optional => 0 },
                              minimization_successful => { isa => 'Bool', optional => 0 },
                              local_minimum => { isa => 'Int', optional => 0 },
                              hessian_error => { isa => 'Bool', optional => 0 },
                              handle_hessian_npd => { isa => 'Bool', optional => 0 },
                              round_error => { isa => 'Bool', optional => 0 },
                              cut_thetas_rounding_errors => { isa => 'Bool', optional => 0 },
                              maxevals_exceeded => { isa => 'Int', optional => 0 },
                              cut_thetas_maxevals => { isa => 'Bool', optional => 0 },
                              sigdigs_accepted => { isa => 'Bool', optional => 0 },
                              pass_picky => { isa => 'Bool', optional => 0 },
                              picky => { isa => 'Bool', optional => 0 },
                              handle_msfo => { isa => 'Bool', optional => 0 },
                              tweak_inits => { isa => 'Bool', optional => 0 },
                              min_retries => { isa => 'Int', optional => 0 },
                              max_retries => { isa => 'Int', optional => 0 },
                              try => { isa => 'Int', optional => 0 },
                              have_accepted_run => { isa => 'Bool', optional => 0 },
        );
    my $estimation_step_run = $parm{'estimation_step_run'};
    my $minimization_successful = $parm{'minimization_successful'};
    my $local_minimum = $parm{'local_minimum'};
    my $hessian_error = $parm{'hessian_error'};
    my $handle_hessian_npd = $parm{'handle_hessian_npd'};
    my $round_error = $parm{'round_error'};
    my $cut_thetas_rounding_errors = $parm{'cut_thetas_rounding_errors'};
    my $maxevals_exceeded = $parm{'maxevals_exceeded'};
    my $cut_thetas_maxevals = $parm{'cut_thetas_maxevals'};
    my $sigdigs_accepted = $parm{'sigdigs_accepted'};
    my $pass_picky = $parm{'pass_picky'};
    my $picky = $parm{'picky'};
    my $handle_msfo = $parm{'handle_msfo'};
    my $tweak_inits = $parm{'tweak_inits'};
    my $min_retries = $parm{'min_retries'};
    my $max_retries = $parm{'max_retries'};
    my $try = $parm{'try'};
    my $have_accepted_run = $parm{'have_accepted_run'};


    my $do_cut_thetas = 0;
    my $do_tweak_inits = 0;
    my $do_retry = 0;
    my $do_reset_msfo = 0;
    my $run_is_accepted = 0;
    my $reason;

    croak("impossible input picky") if ($pass_picky and not $minimization_successful);
    croak("impossible input pass_picky picky") if ($pass_picky and not $picky);
    croak("impossible input round") if ($round_error and $minimization_successful);
    croak("impossible input sigdigs") if ($sigdigs_accepted and $minimization_successful);
    croak("impossible input hessian") if ($hessian_error and $minimization_successful);
    croak("impossible input maxevals") if ($maxevals_exceeded and $minimization_successful);
    croak("impossible input min_retries") if ($min_retries < 0);
    croak("impossible input max_retries") if ($max_retries < 0);
    croak("impossible input have_accepted_run") if ($have_accepted_run and ($try > $min_retries) );

    for (my $i=0 ; $i<1; $i++){
        #single round loop to be able to use last when some condition is met

        unless ($estimation_step_run){
            #unless estimation step is run we have no estimation to evaluate
            $reason = 'no estimation to evaluate';
            last;
        }
        if (($try >= $max_retries) and ($try >=  $min_retries)){
            #not allowed to do more retries
            $reason = 'reached max_retries';
            last;
        }
        if ($have_accepted_run and ($try >=  $min_retries)){
            #have good run from before and have now reached minretries
            $reason = 'have accepted run from before and have reached min_retries';
            last;
        }
        if ( ($round_error && $cut_thetas_rounding_errors) or ($hessian_error && $handle_hessian_npd)
             or ($maxevals_exceeded && $cut_thetas_maxevals)) {
            $do_retry = 1;
            $do_cut_thetas = 1;
            $do_reset_msfo = 1;
            $do_tweak_inits = 1 if ($tweak_inits and ($try > 0)); #do not perturb first time
            $reason = 'hessian/round/maxevals termination and handling';
            last;
        }
        unless ($tweak_inits){
            #do nothing if tweak_inits functionality not turned on
            $reason = 'tweak_inits turned off';
            last;
        }

        if ($picky and (not $pass_picky)){
            $do_retry = 1;
            $do_tweak_inits = 1;
            $reason = 'not pass picky';
            last;
        }
        unless ($sigdigs_accepted or $minimization_successful){
            $do_retry = 1;
            $do_tweak_inits = 1;
            $reason = 'minimization not successful and not sigdigs accepted';
            last;
        }
        if ($local_minimum){
            $do_retry = 1;
            $do_tweak_inits = 1;
            $reason = 'local minimum';
            last;
        }

        #if we get this far then run is accepted, but may still need retry because of min_retries
        $run_is_accepted = 1;
        $reason = 'run is accepted';
    }

    #min_retries has precedence over everything, check outside loop so that is not skipped by 'last' break above
    unless ($do_retry){
        if ($try <  $min_retries){
            $do_retry = 1;
            $do_tweak_inits = 1;
            $reason = 'min_retries not reached';
        }
    }

    if ($do_retry and (not ($do_cut_thetas or $do_tweak_inits))){
        croak("bug in retries_what_to_do, retry without modify model");
    }


    $do_reset_msfo = 1 if ($do_retry and $handle_msfo);

    if (($do_cut_thetas or $do_tweak_inits or $do_reset_msfo) and (not $do_retry)){
        croak("bug in retry what to do, modify model without retry");
    }

    my $message;
    if ($do_retry){
        $message = 'Doing retry because of '.$reason;
    }else{
        $message = 'Not doing retry because '.$reason;
    }

    my %hash;
    $hash{'cut_thetas'} = $do_cut_thetas;
    $hash{'tweak_inits'} = $do_tweak_inits;
    $hash{'retry'} = $do_retry;
    $hash{'reset_msfo'} = $do_reset_msfo;
    $hash{'run_is_accepted'} = $run_is_accepted;
    $hash{'message'} = $message;

    return \%hash;

}

sub restart_needed
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         retries => { isa => 'Int', optional => 1 },
         cut_thetas_rounding_errors => { isa => 'Bool', default => 0, optional => 1 },
         cut_thetas_maxevals => { isa => 'Bool', default => 0, optional => 1 },
         handle_hessian_npd => { isa => 'Bool', default => 0, optional => 1 },
         tweak_inits => { isa => 'Bool', default => 1, optional => 1 },
         maxevals => { isa => 'Maybe[Int]', default => 0, optional => 1 },
         picky => { isa => 'Bool', default => 0, optional => 1 },
         cutoff_thetas => { isa => 'Maybe[ArrayRef[Num]]', optional => 1 },
         significant_digits_accept => { isa => 'Num', optional => 1 },
         run_no => { isa => 'Int', optional => 1 },
         nm_version => { isa => 'Str', optional => 1 },
         queue_info => { isa => 'HashRef', optional => 0 }
    );
    my $retries = $parm{'retries'};
    my $marked_for_rerun = 0;
    my $cut_thetas_rounding_errors = $parm{'cut_thetas_rounding_errors'};
    my $cut_thetas_maxevals = $parm{'cut_thetas_maxevals'};
    my $handle_hessian_npd = $parm{'handle_hessian_npd'};
    my $tweak_inits = $parm{'tweak_inits'};
    my $maxevals = $parm{'maxevals'};
    $maxevals = 0 unless (defined $maxevals);
    my $picky = $parm{'picky'};
    my @cutoff_thetas = defined $parm{'cutoff_thetas'} ? @{$parm{'cutoff_thetas'}} : ();
    my $significant_digits_accept = $parm{'significant_digits_accept'};
    my $run_no = $parm{'run_no'};
    my $nm_version = $parm{'nm_version'};
    my %queue_info = defined $parm{'queue_info'} ? %{$parm{'queue_info'}} : ();


    # -------------- Notes about automatic pertubation and retries -----------------

    # see documentation in common_options


  # We need the trail of files to select the most appropriate at the end
  # (see move_model_and_output)

    unless (defined $parm{'queue_info'}) {
        # The queue_info must be defined here!
        croak("Internal run queue corrupt\n" );
    }
    my $queue_info_ref = $parm{'queue_info'};
    my $run_results = $queue_info_ref->{'run_results'};
    my $tries = \$queue_info_ref->{'tries'};
    my $model = $queue_info_ref->{'model'};
    my $candidate_model = $queue_info_ref->{'candidate_model'};

    my @outputfilelist = (@{$candidate_model->output_files}, 'psn.' . $self->modext, $self->base_msfo_name);

    #this is 1 for regular one $PROB with estimation
    # 2 if two $PROB with $PRIOR TNPRI in first
    # 0 if no estimation (no $EST, or $EST MAXEVAL=0)

    my $failure;
    my $failure_mess;
    my $eta_shrinkage_name;
    my $iwres_shrinkage_name;
    my $stopmess;
    if (-e 'stats-runs.csv') {
        #this is a rerun, we should not do anything here, no copying or anything except
        #reading raw results to memory
        $self->store_results_old_run( retries => $retries,picky => $picky,
                                      run_no => $run_no,
                                      tries => ${$tries},
                                      queue_info_ref => $queue_info_ref);
        return(0); #no restart needed when stats-runs exist
    }
    unless (-e 'psn.lst'){
        my $ref = diagnose_lst_errors(missing => 1,
                                      have_stats_runs => 0,
                                      run_no => $run_no,
                                      modext  => $self->modext,
                                      run_local => $self->run_local,
                                      nmtran_error_file => $self->nmtran_error_file);

        $failure = $ref->[0];
        $failure_mess = $ref->[1];
        my $restart_possible = $ref->[2];
        if ($restart_possible and ($self->handle_crashes and $queue_info_ref->{'crashes'} < $self->crash_restarts)){
            $queue_info_ref->{'crashes'}++;
            update_crash_number(modext => $self->modext,
                                queue_info => $queue_info_ref,
                                retry => ${$tries},
                                nm_major_version => $PsN::nm_major_version,
                                nm_minor_version => $PsN::nm_minor_version);

            ui -> print( category => 'all',
                         message  => $failure_mess.', doing new submit '.$queue_info_ref -> {'crashes'},
                         newline => 1 );
            #here we do not move retry files, since no output generated. reuse same psn.mod. assume no
            #special cleanup needed
            return(1);
        }else{
            $self->general_error(message => $failure_mess) if ($ref->[3]); #if store_general_error is true
            ui -> print( category => 'all', message  => $failure_mess,newline => 1 );
            $run_results -> [${$tries}] -> {'failed'} = $failure; #different texts for different causes
            return(0); #must always return 0 unless either modified model or increased crash number
        }
    }

    #now we know psn.lst exists.
    if ( -e 'psn-' . ( ${$tries} + 1 ) . '.lst' ) {
        #should never enter here.
        #ui -> print( category => 'all', message  => "\n\nERROR IN RESTART NEEDED\n", newline => 1);
        croak("\n\nERROR IN RESTART NEEDED, existing tries files, please report this bug including files\n");
    }

    my ( $output_file );
    $output_file = $candidate_model -> outputs -> [0];
    $output_file -> _read_problems;
    my $evaluation_probnum = $output_file->get_estimation_evaluation_problem_number(); #if neg then no est step run

    my $model_crashed = 0;
    my $iterations_interrupted = 0;
    if (( $output_file -> parsed_successfully() and
                             not defined $output_file -> problems ) or
                           (not $output_file -> parsed_successfully()) ){
        $model_crashed = 1;
        $iterations_interrupted = $output_file->iterations_interrupted;
        if (-e 'OUTPUT' and $output_file->could_append_OUTPUT){
            my $tmp_output_file = output->new(filename => $output_file->filename,
                                              append_nm_OUTPUT => 1);
            $iterations_interrupted = $tmp_output_file->iterations_interrupted;
            print "tried append output, $iterations_interrupted\n";
        }
    }


    ($stopmess,$eta_shrinkage_name,$iwres_shrinkage_name) = move_retry_files(filenames => \@outputfilelist,
                                                                             retry => ${$tries},
                                                                             nm_major_version => $PsN::nm_major_version,
                                                                             nm_minor_version => $PsN::nm_minor_version );

    # Create intermediate raw results

    my ($raw_results_row, $nonp_row);
    if ( ($maxevals > 0) and ($queue_info_ref -> {'crashes'} > 0)) {
        #problem here if got rid of theta omega sigma as part of handle maxevals.
        #candidate model has no labels
        ($raw_results_row, $nonp_row) =
            $self -> create_raw_results_rows( max_hash => $self->max_hash,
                                              model => $candidate_model,
                                              label_model => $model,
                                              model_number => $run_no + 1,
                                              raw_line_structure => $self->raw_line_structure,
                                              eta_shrinkage_file => $eta_shrinkage_name,
                                              iwres_shrinkage_file => $iwres_shrinkage_name);

    }else{
        ($raw_results_row, $nonp_row) =
            $self -> create_raw_results_rows( max_hash => $self->max_hash,
                                              model => $candidate_model,
                                              model_number => $run_no + 1,
                                              raw_line_structure => $self->raw_line_structure,
                                              eta_shrinkage_file => $eta_shrinkage_name,
                                              iwres_shrinkage_file => $iwres_shrinkage_name);

    }

    $queue_info_ref -> {'raw_results'} -> [${$tries}] = $raw_results_row;
    $queue_info_ref -> {'raw_nonp_results'} -> [${$tries}] = $nonp_row;
    no warnings qw(uninitialized);
    # write intermediate raw results, append to existing file
    open( INTERMED, '>>'.'intermediate_raw_results.csv' );
    foreach my $row ( @{$raw_results_row} ){
        next unless (defined $row);
        print INTERMED 'try '.(${$tries}+1).',';
        print INTERMED join( ',', @{$row} )."\n";
    }
    close( INTERMED );
    if (defined $nonp_row and scalar(@{$nonp_row})>0){
        open( INTERMEDNONP, '>>intermediate_nonp_results.csv' );
        foreach my $row ( @{$nonp_row} ){
            next unless (defined $row);
            print INTERMEDNONP 'try '.(${$tries}+1).',';
            print INTERMEDNONP join( ',', @{$row} )."\n";
        }
        close( INTERMEDNONP );
    }

    # Check for minimization successfull and try to find out if lst file is truncated


    my $restart_possible = 0;
    my $maxevals_exceeded = 0;

    $maxevals_exceeded = maxeval_exceeded(output => $output_file,
                                          probnum => $evaluation_probnum); #return 0 if not, actual evals if yes

    if (($maxevals > 0) and (not $cut_thetas_maxevals) and $maxevals_exceeded){
        if (defined $queue_info_ref -> {'evals'}){
            $queue_info_ref -> {'evals'} += $maxevals_exceeded;
        }else {
            $queue_info_ref -> {'evals'} = $maxevals_exceeded;
        }
    }


    if($model_crashed){
        #here we do have a lst-file, but perhaps is completely empty
        #check for signs of NMtran error, compilation error.

        my $ref = diagnose_lst_errors(missing => 0,
                                      have_stats_runs => 0,
                                      parsed_successfully => 0,
                                      interrupted => $iterations_interrupted,
                                      run_no => $run_no,
                                      modext  => $self->modext,
                                      run_local => $self->run_local,
                                      nmtran_error_file => $self->nmtran_error_file);
        $failure = $ref->[0];
        $failure_mess = $ref->[1];
        $restart_possible = $ref->[2];
        $self->general_error(message => $failure_mess) if ($ref->[3]); #if store_general_error is true
    }

    my $do_crash_type_restart = 0;
    $do_crash_type_restart = 1 if (
        ($maxevals_exceeded and ( $maxevals > 0 ) and (not $cut_thetas_maxevals) and ($maxevals > $queue_info_ref->{'evals'})) or
        ($model_crashed and $restart_possible and ($self->handle_crashes and $queue_info_ref->{'crashes'} < $self->crash_restarts))
        );

    if ($do_crash_type_restart){
        #either models_crashed or handle maxevals exceeded
        $queue_info_ref->{'crashes'}++;

        if($model_crashed){
            #not if handle maxevals exceeded
            update_crash_number(modext => $self->modext,
                                queue_info => $queue_info_ref,
                                retry => ${$tries},
                                nm_major_version => $PsN::nm_major_version,
                                nm_minor_version => $PsN::nm_minor_version);

            my $message = "\nModel in NM_run".($run_no+1)." crashed, restart attempt nr ". ($queue_info_ref -> {'crashes'} );
            ui -> print( category => 'all',  message  => $message,
                         newline => 1);
        }

        ($stopmess,$eta_shrinkage_name,$iwres_shrinkage_name) = move_retry_files(filenames => \@outputfilelist,
                                                                                 retry => ${$tries},
                                                                                 crash => $queue_info_ref -> {'crashes'},
                                                                                 nm_major_version => $PsN::nm_major_version,
                                                                                 nm_minor_version => $PsN::nm_minor_version );


        if( $self->handle_msfo or ($maxevals > 0)){
            set_msfo_to_msfi( candidate_model => $candidate_model,
                              retry => ${$tries},
                              queue_info => $queue_info_ref,
                              base_msfo_name => $self->base_msfo_name);
        } else {
            my $new_name = get_retry_name( filename => 'psn.'.$self->modext,
                                           retry => ${$tries},
                                           crash => $queue_info_ref -> {'crashes'});
            copy( $new_name, 'psn.'.$self->modext );
        }

        $output_file -> flush;
        return(1); # Return a one (1) to make run() rerun the model.

    } #end if $do_crash_type_restart

    if($model_crashed){
        ui -> print( category => 'all', message  => $failure_mess."\nNot restarting this model.",newline => 1 );

        $run_results -> [${$tries}] -> {'failed'} = $failure;
        $output_file -> flush;
        return(0);
    }

    #here we know the model did not crash
    # (re)set the $crashes variable and continue
    $queue_info_ref -> {'crashes'} = 0;
    #reset number of evals
    $queue_info_ref -> {'evals'}=0;




    # log the stats of this run

    foreach my $category ( 'minimization_successful', 'covariance_step_successful',
                           'covariance_step_warnings', 'estimate_near_boundary',
                           'significant_digits', 'ofv' ){
        my $index = (abs($evaluation_probnum)-1);
        $run_results -> [${$tries}] -> {$category} = $output_file -> get_single_value(attribute => $category,
                                                                                      problem_index => $index);
    }

    $run_results -> [${$tries}] -> {'pass_picky'} = passed_picky(minimization_successful => $output_file -> minimization_successful(),
                                                                 minimization_message => $output_file -> minimization_message(),
                                                                 probnum => $evaluation_probnum,
                                                                 picky => $picky);

    my $round_error = 0;
    $round_error = 1 if (($evaluation_probnum > 0) and
                         $output_file -> get_single_value(attribute => 'rounding_errors',
                                                          problem_index => (abs($evaluation_probnum)-1)));

    my $sigdigs_accepted = significant_digits_accepted(run_results => $run_results,
                                                       significant_digits_accept => $significant_digits_accept,
                                                       try => ${$tries});

    my $hessian_error = hessian_error(output => $output_file,  probnum => $evaluation_probnum);

    my $local_minimum = local_minimum(run_results => $run_results,
                                      reduced_model_ofv => $self->reduced_model_ofv,
                                      accepted_ofv_difference => $self->accepted_ofv_difference,
                                      have_accepted_run => $queue_info_ref -> {'have_accepted_run'},
                                      try => ${$tries},
                                      probnum => $evaluation_probnum);

    my $do_this = retries_decide_what_to_do( estimation_step_run => ($evaluation_probnum > 0),
                                             minimization_successful => $run_results -> [${$tries}] -> {'minimization_successful'},
                                             local_minimum => $local_minimum,
                                             hessian_error => $hessian_error,
                                             handle_hessian_npd => $handle_hessian_npd,
                                             round_error => $round_error,
                                             cut_thetas_rounding_errors => $cut_thetas_rounding_errors,
                                             maxevals_exceeded => $maxevals_exceeded,
                                             cut_thetas_maxevals => $cut_thetas_maxevals,
                                             sigdigs_accepted => $sigdigs_accepted,
                                             pass_picky => $run_results -> [${$tries}] -> {'pass_picky'},
                                             picky => $picky,
                                             handle_msfo => ($self->handle_msfo || ($maxevals > 0)),
                                             tweak_inits => $tweak_inits,
                                             min_retries => $self->min_retries,
                                             max_retries => $retries,
                                             try => ${$tries},
                                             have_accepted_run => $queue_info_ref -> {'have_accepted_run'}
        );




    $queue_info_ref->{'have_accepted_run'}= 1 if ($do_this->{'run_is_accepted'}); #do not overwrite old if this run not accepted

    if ($do_this->{'reset_msfo'}){
        reset_msfo( basic_model => $model,
                    candidate_model => $candidate_model,
                    base_msfo_name => $self->base_msfo_name);
    }
    if ($do_this->{'cut_thetas'}){
        $self -> cut_thetas( candidate_model => $candidate_model,
                             cutoff_thetas => \@cutoff_thetas,
                             output_file => $output_file );
    }
    if ($do_this->{'tweak_inits'}){
        my $did_tweak = 0;
        #loop backwards until found params to tweak
        for (my $index = (abs($evaluation_probnum)-1); $index >= 0; $index--){
            $did_tweak = $candidate_model -> problems->[$index] -> set_random_inits ( degree => $self->degree ,
                                                                                      basic_model => $model,
                                                                                      problem_index => $index);
            if ($did_tweak){
                last;
            }
        }
        unless ($did_tweak){
            #found no params to tweak. Unless did cut thetas or reset msfo, i.e. modified model, then turn off retry
            unless ($do_this->{'cut_thetas'} or $do_this->{'reset_msfo'}){
                $do_this->{'retry'} = 0;
            }
        }
    }

    if ($do_this->{'retry'}){
        ${$tries}++;
        if( ${$tries} >= $self->min_retries and $self->verbose ){
            my $message = "R:".($run_no+1).":". (${$tries}+1) . " ";
            ui -> print( category => 'all',  message  => $message,
                         newline => 0);
        }
        #know must have modified model
        $candidate_model->_write;
        $marked_for_rerun=1;
    }else{
        $marked_for_rerun = 0;

    }

    my ($failed,$reason) = $output_file->nonmem_run_failed;
    if ($failed){
        $run_results -> [${$tries}] -> {'nonmem_run_failed'} = $reason;
    }
    $output_file -> flush;
    return $marked_for_rerun;

}

sub update_crash_number {
    my %parm = validated_hash(\@_,
                              modext => { isa => 'Str', optional => 0 },
                              queue_info => { isa => 'HashRef', optional => 0 },
                              retry =>  { isa => 'Int', optional => 0 },
                              nm_major_version => { isa => 'Int', optional => 0 },
                              nm_minor_version => { isa => 'Maybe[Num]', optional => 1 }
        );
    my $modext = $parm{'modext'};
    my $queue_info = $parm{'queue_info'};
    my $retry = $parm{'retry'};
    my $nm_major_version = $parm{'nm_major_version'};
    my $nm_minor_version = $parm{'nm_minor_version'};

    #now we could be restarting after the main PsN process has been killed, and
    #the crashes counter has been reset. Therefore check existence of files from
    #previous crashes
    my $crash_no = $queue_info -> {'crashes'};
    while (-e get_retry_name( filename => 'psn.'.$modext,
                              retry => $retry,
                              crash => $crash_no)){
        $crash_no++;
    }
    $queue_info -> {'crashes'} = $crash_no;
}

sub compute_iofv
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         run_no => { isa => 'Int', optional => 1 },
         queue_info => { isa => 'HashRef', optional => 0 }
    );
    my $run_no = $parm{'run_no'};
    my %queue_info = defined $parm{'queue_info'} ? %{$parm{'queue_info'}} : ();

  my $queue_info_ref = $parm{'queue_info'};
    croak "queue_info is not defined" unless defined $queue_info_ref;

  my $model = $queue_info_ref->{'model'};

  if ( defined $model->iofv_modules ) {
    $model->iofv_modules->[0]->post_run_process;
  }
}

sub run_nmtran
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              check_verbatim => { isa => 'Bool', optional => 1 , default => 0},
                              model => { isa => 'model', optional => 1}
        );
    my $check_verbatim = $parm{'check_verbatim'};
    my $model = $parm{'model'};
    my $ok;

    $ok = 1;
    if ($self->check_nmtran and defined $self->full_path_nmtran and (length($self->full_path_nmtran)>0)){
        my $command = $self->full_path_nmtran.'  < psn.'.$self->modext.' > FMSG';
        system($command);
        unlink('FCON','FSIZES','FSTREAM','prsizes.f90','FSUBS','FSUBS2','FSUBS.f90');
        unlink('FSUBS_MU.F90','FLIB','LINK.LNK','FWARN');
        if (not -e 'FREPORT'){
            $ok=0;
            mv('FMSG',$self->nmtran_error_file);
            #leave FDATA for correct diagnosis in restart_needed
        }else{
            #everything ok, cleanup
            unlink('FDATA','FREPORT','FMSG');
            if ($check_verbatim){
                my @code_records = ('aesinitial','aes','des','error','infn','mix','pk','pred','thetai','thetar');
                #check if have verbatim
                my $testmodel = $model -> copy( filename => 'dummy.'.$self->modext,
                                                copy_datafile => 0,
                                                copy_output => 0,
                                                write_copy => 0);
                my $have_verb = 0;
                foreach my $prob (@{$testmodel->problems()}){
                    $prob -> shrinkage_module -> disable;
                    $prob -> cwres(0);
                    $prob -> mirror_plots(0);
                    $prob -> {'cwres_modules'}=undef;
                    $prob -> {'mirror_plot_modules'}=undef;
                    foreach my $coderec (@code_records){
                        my $accessor = $coderec.'s';
                        if ( defined $prob->$accessor ) {
                            foreach my $record ( @{$prob->$accessor} ){
                                if ((defined $record->verbatim_first ) or (defined $record->verbatim_last)){
                                    $have_verb=1;
                                    $record->{'verbatim_first'}=undef;
                                    $record->{'verbatim_last'}=undef;
                                }
                            }
                        }
                    }
                }
                if ($have_verb){
                    $testmodel->remove_records(type => 'table');
                    $testmodel->_write();
                    my $command = $self->full_path_nmtran.'  < dummy.'.$self->modext.' > FMSG';
                    system($command);
                    if (not -e 'FREPORT'){
                        open( ERR, 'FMSG');
                        my @read_file = <ERR>;
                        close( ERR );
                        for(my $i=0; $i<scalar(@read_file); $i++){
                            if ($read_file[$i] =~/UNDEFINED VARIABLE/){
                                if ($read_file[$i-1] =~/:\s+(.+)$/){
                                    print "\nWarning: Double check that variable $1 is defined in the model\n";
                                    print "         (if you defined it in verbatim code then everything is ok)\n";
                                }
                            }
                        }
                        mv('FMSG','nmtran_error_without_verbatim_code.txt');
                    }
                    unlink('dummy.'.$self->modext);
                    unlink('FCON','FSIZES','FSTREAM','prsizes.f90','FSUBS','FSUBS2','FSUBS.f90');
                    unlink('FSUBS_MU.F90','FLIB','LINK.LNK','FWARN');
                    unlink('FDATA','FREPORT','FMSG');
                }
            }
        }
    }

    return $ok;
}

sub compute_cwres
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         run_no => { isa => 'Int', optional => 1 },
         queue_info => { isa => 'HashRef', optional => 0 }
    );
    my $run_no = $parm{'run_no'};
    my %queue_info = defined $parm{'queue_info'} ? %{$parm{'queue_info'}} : ();

    my $queue_info_ref = $parm{'queue_info'};
    croak "queue_info is not defined" unless defined $queue_info_ref;

    my $model = $queue_info_ref->{'model'};

    #check in PsN config, or try R --version
    my $Rexec = PsN::get_R_exec();

    if (defined $Rexec and length($Rexec)>0){
        my $probs = $model->problems();
        if ( defined $probs ) {
            foreach my $prob ( @{$probs} ) {
                if( $prob->cwres_modules ) {
                    my $sdno = $prob->cwres_modules->[0]->sdno();

                    my $sim = $prob->cwres_modules->[0]->mirror_plots ? ',sim.suffix="sim"' : '';
                    # Create the short R-script to compute the CWRES.
                    open( RSCRIPT, ">compute_cwres.R" );
                    print RSCRIPT "library(xpose4)\ncompute.cwres($sdno $sim)\n";
                    close( RSCRIPT );

                    # Execute the script
                    system( $Rexec." CMD BATCH compute_cwres.R" );
                }
            }
        }
    }

}

sub get_selected
{
    my $file = shift;
    my @lines = utils::file::slurp_file($file);
    my $selected;
    for (my $i=1; $i<= scalar(@lines); $i++){
        if ($lines[-$i] =~ /\s*Selected\s*(\d+)/){
            $selected = $1;
            last;
        }
    }
    return $selected;
}

sub copy_model_and_input
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         model => { isa => 'model', optional => 0 },
         source => { isa => 'Str', optional => 1 },
         run_nmtran => { isa => 'Bool', default => 0, optional => 1 },
         check_verbatim => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $model = $parm{'model'};
    my $source = $parm{'source'};
    my $run_nmtran = $parm{'run_nmtran'};
    my $check_verbatim = $parm{'check_verbatim'};
    my $candidate_model;

    my $tries = 0;
    if (-e 'stats-runs.csv' and $self->add_retries) {
        #possible reasons:
        #a) Restart after -clean > 1. Then we do not know the true restart number of psn.lst
        #b) Restart after completed run -clean <= 1. Best retry is copied to psn.lst, don't know which.

        #read selected retry from stats-runs
        my $selected_retry = get_selected('stats-runs.csv');
        #remove stats-runs here.
        unlink 'stats-runs.csv';

        #Then pick highest retry file in directory as
        #candidate model, otherwise psn.mod
        #If do not have any retry file must copy input again, was removed during clean=2

        if ((-e 'psn-1.'.$self->modext ) or (-e 'psn-2.'.$self->modext)) {

            #clean 1. Data is left. Move psn.mod and psn.lst to selected retry number. #HERE
            #use last retry as candidate model. move last retry to psn.mod and
            # .lst, as if had just been run.
            foreach my $ext (@PsN::nm7_extensions,'.'.$self->modext,'.lst'){
                mv('psn'.$ext,'psn-'.$selected_retry.$ext) if (-e 'psn'.$ext);
            }

            my $last_retry = 1;
            while (-e 'psn-'.($last_retry+1).'.'.$self->modext) {
                $last_retry++;
            }
            $tries = $last_retry-1;
            foreach my $ext (@PsN::nm7_extensions,'.'.$self->modext,'.lst'){
                mv('psn-'.$last_retry.$ext,'psn'.$ext) if (-e 'psn-'.$last_retry.$ext);
            }

            $candidate_model = model->new(
                outputfile                  => 'psn.lst',
                filename                    => 'psn.'.$self->modext,
                ignore_missing_output_files    => 1,
                ignore_missing_data                    => 0
            );
        } else {
            #clean 2.
            #must copy input again
            $tries = $selected_retry-1;

            if ($model->tbs or $model->dtbs){
                $self->write_tbs_files(thetanum => $model->tbs_thetanum());
            }
            #use psn.mod as candidate model
            foreach my $file( @{$model->input_files} ){
                #this does not include data files
                # $file is a ref to an array with two elements, the first is a
                # path, the second is a name.

                copy( $file->[0] . $file -> [1], $file -> [1] ); #FIXME symlink if on linux?

            }

            #it is an error if data is missing here, but we ignore it and let
            #nonmem crash due to missing data, will be handled better that way than having croak in data.pm
            #TODO mark the model as failed even before NMrun if data is missing, so do not waste nm call

            $candidate_model =  model -> new (outputfile                  => 'psn.lst',
                                              filename                    => 'psn.'.$self->modext,
                                              ignore_missing_output_files => 1,
                                              ignore_missing_data => 1);

            unlink 'psn.'.$self->modext;

            $candidate_model -> shrinkage_modules( $model -> shrinkage_modules );
            $candidate_model->_write;

        }

    }elsif (-e 'stats-runs.csv'){
        $candidate_model =  model -> new (outputfile                  => 'psn.lst',
                                          filename                    => 'psn.'.$self->modext,
                                          ignore_missing_output_files => 1,
                                          ignore_missing_data => 1);

        #psn.lst may be missing if e.g. nmtran failed

    }else{

        if ($model->tbs() or $model->dtbs()){
            $self->write_tbs_files(thetanum => $model->tbs_thetanum());
        }

        # Set the table names to a short version
        my @new_table_names = ();
        my @table_names = @{$model -> table_names( ignore_missing_files => 1 )};
        # Loop the problems
        for ( my $i = 0; $i <= $#table_names; $i++ ) {
            my @new_arr;
            # Loop the table files within each problem
            for ( my $j = 0; $j < scalar @{$table_names[$i]}; $j++ ) {
                my ( $dir, $filename ) = OSspecific::absolute_path( '.', $table_names[$i][$j] );
                push( @new_arr,  $filename );
            }
            push( @new_table_names, \@new_arr );
        }


        # Copy input files ( msfo, msfi, subroutines and extra at the
        # moment)

        foreach my $file( @{$model -> input_files} ){

            # $file is a ref to an array with two elements, the first is a
            # path, the second is a name.

            copy( $file->[0] . $file -> [1], $file -> [1] ); #FIXME symlink if unix?

        }


        # Copy the model object. Set the new (shorter) data file names.
        # datafiles are copied by model
        #data_file_names are only used if copy_data=1

        #it is an error if data is missing here, but we ignore it and let
        #nonmem crash due to missing data, will be handled better that way than having croak in data.pm
        #TODO mark the model as failed even before NMrun if data is missing, so do not waste nm call

        $candidate_model = $model -> copy( filename => 'psn.'.$self->modext,
                                           copy_datafile => ($self->copy_data or $self->always_datafile_in_nmrun),
                                           copy_output => 0,
                                           write_copy => 0);

        $candidate_model -> shrinkage_modules( $model -> shrinkage_modules );

        if ( $self->handle_msfo ) {

            # Initialize sequence of msfi/msfo files.

            my $msfo_names = $candidate_model -> msfo_names;
            my $msfi_names = $candidate_model -> msfi_names(absolute_path => 1);
            my $msfi_in;

            if( defined $msfo_names and (defined $msfo_names -> [0])){
                $msfi_in = $msfo_names -> [0];
            } elsif ( defined $msfi_names and (defined $msfi_names -> [0])){
                $msfi_in = $msfi_names -> [0];
            }

            my $basename = 'psn_msfo';
            if( defined $msfo_names and (defined $msfo_names -> [0])){
                $basename = $msfo_names -> [0];
            }

            if( defined $msfi_in and (-s $msfi_in) ){
                #-s returns true if file is non-empty
                #assume original model has msfi, assume thetas already removed.
                copy( $msfi_in, $basename.'-0' ); #move or copy... this is from calling directory
                #assume only one $MSFI per $PROB.
                if (defined $candidate_model->problems->[0]->msfis){
                    $candidate_model->problems->[0]->msfis->[0]->set_filename(filename =>$basename.'-0');
                }
            } else {
                #
                1;
            }

            if( defined $candidate_model -> problems->[0]->estimations or
                defined $candidate_model -> problems->[0]->nonparametrics ){
                $self->base_msfo_name($basename);

                $candidate_model -> rename_msfo(name => $basename,
                                                add_if_absent => 1);

            }
        }

        $candidate_model -> table_names( new_names            => \@new_table_names,
                                         ignore_missing_files => 1 );
        $candidate_model -> _write(copy_msfi =>1);
        $candidate_model -> store_inits;
        $self->run_nmtran(check_verbatim => $check_verbatim,
                          model => $candidate_model) if ($run_nmtran);

    }

    return ($candidate_model,$tries);
}

sub write_tbs_files
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        thetanum => { isa => 'Int', optional => 0 }
    );
    my $thetanum = $parm{'thetanum'};

    my $file ='contr.txt';
    open( FILE, '>'.$file );
    print FILE '      subroutine contr (icall,cnt,ier1,ier2)'."\n".
    '      double precision cnt'."\n".
    '      call ncontr (cnt,ier1,ier2,l2r)'."\n".
    '      return'."\n".
    '      end'."\n";
    close(FILE);

    $file = 'ccontra_nm7.txt';
    if ($PsN::nm_major_version < 7){
        $file = 'ccontra_nm6.txt';
    }
    open( FILE, '>'.$file );
    if ($PsN::nm_major_version < 7){
        print FILE '      subroutine ccontr (icall,c1,c2,c3,ier1,ier2)'."\n".
        '      parameter (lth=40,lvr=30,no=50)'."\n".
        '      common /rocm0/ theta (lth)'."\n".
        '      common /rocm4/ y'."\n".
        '      double precision c1,c2,c3,theta,y,w,one,two'."\n".
        '      dimension c2(*),c3(lvr,*)'."\n".
        '      data one,two/1.,2./'."\n".
        '      if (icall.le.1) return'."\n".
        '      w=y'."\n".
        "\n".
        '         if(theta('.$thetanum.').eq.0) y=log(y)'."\n".
        '         if(theta('.$thetanum.').ne.0) y=(y**theta('.$thetanum.')-one)/theta('.$thetanum.')'."\n".
        "\n".
        "\n".
        '      call cels (c1,c2,c3,ier1,ier2)'."\n".
        '      y=w'."\n".
        '      c1=c1-two*(theta('.$thetanum.')-one)*log(y)'."\n".
        "\n".
        '      return'."\n".
        '      end'."\n";
    }else{
        print FILE '      subroutine ccontr (icall,c1,c2,c3,ier1,ier2)'."\n".
        '      USE ROCM_REAL,   ONLY: theta=>THETAC,y=>DV_ITM2'."\n".
        '      USE NM_INTERFACE,ONLY: CELS'."\n".
        '!      parameter (lth=40,lvr=30,no=50)'."\n".
        '!      common /rocm0/ theta (lth)'."\n".
        '!      common /rocm4/ y'."\n".
        '!      double precision c1,c2,c3,theta,y,w,one,two'."\n".
        '      double precision c1,c2,c3,w,one,two'."\n".
        '      dimension c2(:),c3(:,:)'."\n".
        '      data one,two/1.,2./'."\n".
        '      if (icall.le.1) return'."\n".
        '      w=y(1)'."\n".
        "\n".
        '         if(theta('.$thetanum.').eq.0) y(1)=log(y(1))'."\n".
        '         if(theta('.$thetanum.').ne.0) y(1)=(y(1)**theta('.$thetanum.')-one)/theta('.$thetanum.')'."\n".
        "\n".
        "\n".
        '      call cels (c1,c2,c3,ier1,ier2)'."\n".
        '      y(1)=w'."\n".
        '      c1=c1-two*(theta('.$thetanum.')-one)*log(y(1))'."\n".
        "\n".
        '      return'."\n".
        '      end'."\n";
    }
    close(FILE);
}

sub move_model_and_output
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              final_model => { isa => 'model', optional => 0 },
                              model => { isa => 'model', optional => 0 },
                              use_run => { isa => 'Str', default => '', optional => 1 }
        );
    my $final_model = $parm{'final_model'};
    my $model = $parm{'model'};
    my $use_run = $parm{'use_run'};

    my $final_lst;
    my $outfilename;
    if (not $self->model_subdir) {
        $outfilename = $model->outputs->[0]->full_name;
    } else {
        $outfilename = $self->base_directory . $self->model_subdir_name . $model->outputs->[0]->filename;
    }

    my ($dir, $model_filename) = OSspecific::absolute_path($model -> directory,
                                                           $model -> filename );

    # This is used with 'prepend_model_file_name'
    #this regex must be the same as used when finding lst-file name in model.pm, for consistency
    my $dotless_model_filename = $model_filename;
    $dotless_model_filename =~ s/\.[^.]+$//;

    my @output_files = @{$final_model -> output_files};

    #Kajsa 2008-09-16 Prepend modelfile $use_run-1 to lst-file $use_run-1
    #prepend options file to lst
    if ( $self->prepend_options_to_lst ) {
        my @out_array;
        my $fname;

        if ($self->prepend_options_to_lst) {
            #read option file to memory
            $fname = $self->directory . "/version_and_option_info.txt";
            if ( open (OPTFILE,$fname)){
                while (my $inline = <OPTFILE>){
                    chomp ($inline);
                    push (@out_array,$inline);
                }
                close (OPTFILE);
                push (@out_array,'');
            }
        }

        #then read final lst-file to memory, append to same array
        $fname = get_retry_name( filename => 'psn.lst',
                                 retry => $use_run-1);
        open(LSTFILE, $fname);
        while (my $inline = <LSTFILE>){
            chomp ($inline);
            push (@out_array,$inline);
        }
        close (LSTFILE);
        #finally open lstfile for overwriting, print model+lst
        open(LSTFILE, "> ", $fname);
        foreach my $inline (@out_array){
            print LSTFILE $inline."\n";
        }
        close (LSTFILE);
    }

    my @nmout;
    @nmout = split( /,/ ,$self->nm_output()) if (defined $self->nm_output());

    # TODO update move_retry_files and use it here?

    $self->metadata->{'copied_files'} = [];

    foreach my $filename (@output_files, 'psn.' . $self->modext) {

        my $use_name = get_retry_name( filename => $filename,
                                       retry => $use_run-1);

        # Copy $use_run files to final files in NM_run, to be clear about which one was selected
        mv( $use_name, $filename ) if (-e $use_name);
        next if( $filename eq 'psn.'.$self->modext );

        # Don't prepend the model file name to psn.lst, but use the name
        # from the $model object.
        if ($filename eq 'psn.lst') {
            my $success = copy($filename, $outfilename);
            $final_lst = $outfilename;
            (undef, undef, my $lst_name) = File::Spec->splitpath($outfilename);
            if ($success) {
                push @{$self->metadata->{'copied_files'}}, $lst_name;
            }
            next;
        }
        my $found_ext = 0;
        foreach my $ext (@PsN::nm7_extensions){
            if( $filename eq 'psn'.$ext ){
                $found_ext = 1;
                foreach my $out (@nmout){
                    $out =~ s/^\.//;
                    if ('.'.$out eq $ext){
                        my $success;
                        if ($self->model_subdir) {
                            $success = copy($filename, $self->base_directory . $self->model_subdir_name . $dotless_model_filename . $ext);
                        } else {
                            $success = copy($filename, $dir . $dotless_model_filename . $ext);
                        }
                        if ($success) {
                            push @{$self->metadata->{'copied_files'}}, $dotless_model_filename . $ext;
                        }
                        last;
                    }
                }
                last;
            }
        }
        next if ($found_ext);

        my $destination;
        if ($self->model_subdir) {
            $destination = $self->base_directory . $self->model_subdir_name;
        } else {
            $destination = $dir;
        }
        if ($self->prepend_model_file_name) {
            $destination .= "$dotless_model_filename.";
        }
        $destination .= $filename;
        copy($filename, $destination);
        push @{$self->metadata->{'copied_files'}}, $filename;
    }

    if ($self->clean >= 1) {
        unlink 'nonmem', 'nonmem5','nonmem6','nonmem7',
        'nonmem5_adaptive','nonmem6_adaptive','nonmem7_adaptive',
        'nonmem.exe','FDATA', 'FREPORT', 'FSUBS', 'FSUBS.f','FSUBS.f90',
        'FSUBS.for', 'LINK.LNK', 'FSTREAM', 'FCON.orig', 'FLIB', 'FCON','PRDERR',
        'nmprd4p.mod','nul',
        'fsubs','fsubs.f','fsubs.for','fsubs.f90','FSUBS2','FSUBS_MU.F90';

        unlink 'LINKC.LNK','compile.lnk','gfortran.txt','ifort.txt','garbage.out',
        'newline','nmexec.set','parafile.set','prcompile.set','prdefault.set',
        'prsame.set','psn.log','rundir.set','runpdir.set','temporaryfile.xml';
        unlink 'temp.out','trashfile.xxx','trskip.set','worker.set','xmloff.set';
        unlink 'prsizes.f90','licfile.set','background.set','FMSG','FSIZES';
        #do not delete INTER, needed for saving data from crashed runs
        unlink 'modelname';

        unlink( <worker*/*> );
        my @removedir = <worker*>;
        foreach my $remdir (@removedir){
            rmdir ($remdir) if (-d $remdir);
        }

        if( defined $final_model -> extra_files ){
            foreach my $x_file( @{$final_model -> extra_files} ){
                my ( $dir, $filename ) = OSspecific::absolute_path( $final_model -> directory,
                                                                    $x_file );
                unlink( $filename );
            }
        }
        if( $self->clean >= 2 ){
            unlink( <temp_dir/*> );
            rmdir( 'temp_dir' );
            my $msfo=$final_model -> problems->[0]->get_msfo_filenames->[0];
            if (defined $msfo){
                $msfo = get_retry_name( filename => $msfo,
                                        retry => $use_run-1);
            }
            my $max_retry = $self->retries;
            $max_retry = $self->min_retries if ($self->min_retries > $max_retry);
            $max_retry++; #first run with number 1 is not a retry
            for ( my $i = 1; $i <= $max_retry; $i++ ) {
                foreach my $filename (@output_files, 'psn.' . $self->modext) {

                    my $use_name = get_retry_name( filename => $filename,
                                                   retry => $i-1);
                    unlink( $use_name );
                    my $crash=1;
                    my $del_name = get_retry_name( filename => $filename,
                                                   retry => $i-1,
                                                   crash => $crash);

                    while (-e $del_name){
                        $crash++;
                        my $next_name = get_retry_name( filename => $filename,
                                                        retry => $i-1,
                                                        crash => $crash);
                        unlink( $del_name ) unless (($use_name eq $msfo) and
                                                    (not -e $next_name));
                        $del_name = $next_name;
                    }
                }
            }
            unlink( @{$model -> datafiles} );
        }
    }

    if ( $self->clean >= 3 ) {
        # Do nothing. "run_nonmem" will remove entire work directory
        # before returning.
    } else {
        system('tar cz --remove-files -f nonmem_files.tgz *')
            if ( $self->compress and $Config{osname} ne 'MSWin32' );
        system('compact /c /s /q > NUL')
            if ( $self->compress and $Config{osname} eq 'MSWin32' );
    }

    if ($self->so) {
        if (not eval("require so; require so::parsers::nmoutput;")) {
            ui->print(category=> 'all',
                      message=> "Unable to create the standard output: the option -so needs the XML::LibXML module to be installed");
        }else{
            my $so = so->new();
            my $nm_parser = so::parsers::nmoutput->new(so => $so, lst_file => $final_lst);
            $so->write();
            copy($so->filename, $model->directory);
            unlink($so->filename);
        }
    }
}

sub calculate_raw_results_width
{
    my $self = shift;

    # 2008-01-24
    # This code comes largely from "prepare_raw_results" which should
    # be split over several functions to fit the serialized version of
    # PsN.

    # Some column in "raw_results_header" are meta-columns, they
    # will be replaced by several columns. For example, the
    # 'theta' column will be replaced with TH1, TH2, TH3 in the
    # general case. (Actually it will be replaced with the
    # thetas labels from the model file. Here we search the meta
    # column of the raw_results_header to find the maximum
    # number of real columns.

    my %max_hash;
    my $saem=0;
    my $bayes=0;

    foreach my $model ( @{$self->models} ) {

        #figure out if last est is saem or bayes
        unless ($bayes and $saem){
            for (my $i=0;$i< scalar(@{$model ->problems()}); $i++){
                #get ref of array of methods
                my $methref = $model -> get_option_value(record_name => 'estimation', option_name => 'METHOD',
                    problem_index => $i, record_index => 'all');
                if (defined $methref){
                    my $j= scalar(@{$methref})-1;
                    if (defined $methref->[$j]){
                        $saem = 1 if ($methref->[$j] eq 'SAEM' or
                            (index('SAEM',$methref->[$j])==0));
                        $bayes = 1 if ($methref->[$j] eq 'BAYES' or
                            (index('BAYES',$methref->[$j])==0));
                    }
                }
                last if ($bayes and $saem);
            }
        }
        if ($bayes){
            $max_hash{'set_dic'}=1;
        }else{
            $max_hash{'set_dic'}=0;
        }
        foreach my $category ( @{$self->raw_results_header},'npeta','npomega' ) {
            if ( $category eq 'setheta' or $category eq 'seomega' or $category eq 'sesigma' ){
                next;
            }elsif ( $category eq 'nburn_set' or $category eq 'burn_in_iter' or $category eq 'burn_in_conv'){
                my $numpar=0;
                if ($saem or $bayes){
                    $numpar = 1;
                }
                $max_hash{ $category } = $numpar; #$saem and $bayes are never unset, so can never set 1 to 0

            }elsif ( $category eq 'model_run_time'
                    or $category eq 'subprob_est_time' or $category eq 'subprob_cov_time'){
                my $numpar=0;
                if ($PsN::nm_major_version >= 7 ){
                    $numpar = 1;
                }
                $max_hash{ $category } = $numpar; #$saem and $bayes are never unset, so can never set 1 to 0

            } elsif ( $category eq 'theta' or $category eq 'omega' or $category eq 'sigma' or
                $category eq 'npomega' or $category eq 'shrinkage_eta' or $category eq 'eigen' or
                $category eq 'npeta') {
                my $numpar = 0;
                if( $category eq 'npomega' or $category eq 'shrinkage_eta' or $category eq 'npeta') {
                    my $nomegas = $model -> nomegas(with_correlations => 0, with_same => 1);
                    if( defined $nomegas ) {
                        for( my $j = 0; $j < scalar @{$nomegas}; $j++ ) {
                            if( $category eq 'npomega' ) {
                                my $npar = $nomegas -> [$j];
                                $npar = $npar*($npar+1)/2;
                                $numpar = $numpar >= $npar ? $numpar : $npar;
                            } elsif ( $category eq 'npeta' ){
                                $numpar = $numpar >= $nomegas -> [$j] ? $numpar : $nomegas -> [$j];
                            } else {
                                $numpar = $numpar >= $nomegas -> [$j] ? $numpar : $nomegas -> [$j];
                            }
                        }
                    }
                } elsif( $category eq 'eigen') {

                    # This is an upper limit on the number of eigenvalues in
                    # the output file. The accessors should not count "SAME"
                    # but should count offdiagonals. It also counts "FIX"
                    # which is not ideal.

                    my $max_sigmas = 0;
                    foreach my $prob( @{$model -> nsigmas(with_correlations => 1 )} ) {
                        if( defined $prob and ($prob > $max_sigmas) ){
                            $max_sigmas = $prob;
                        }
                    }

                    my $max_omegas = 0;
                    foreach my $prob( @{$model -> nomegas(with_correlations => 1 )} ) {
                        if( $prob > $max_omegas ){
                            $max_omegas = $prob;
                        }
                    }

                    $numpar = $model -> nthetas() + $max_omegas + $max_sigmas;

                } else {

                    # Here we assume that $category is 'theta', 'omega' or
                    # 'sigma'. SAME records will also get space.


                    my $accessor = 'n'.$category.'s';
                    if( $category eq 'theta' ){
                        #this call assumes only one $PROBLEM, bug otherwise
                        $numpar = $model -> $accessor();
                    } else {
                        # Here accessor must be omega or sigma.
                        $numpar = $model -> $accessor(with_correlations => 1); #with_same is default
                    }

                    # omega and sigma is an array, we find the biggest member

                    if( ref($numpar) eq 'ARRAY' ){
                        my $max = 0;
                        foreach my $prob ( @{$numpar} ){
                            if( defined $prob and ($prob > $max) ){
                                $max = $prob;
                            }
                        }
                        $numpar = $max;
                    }

                }
                if ( (not defined $max_hash{ $category }) or ($max_hash{ $category } < $numpar) ) {
                    $max_hash{ $category } = $numpar;
                    $max_hash{ 'se'.$category } = $numpar;
                }
            } else {
                $max_hash{ $category } = 1;
            }
        }
    }

    $self->max_hash(\%max_hash);
}

sub print_finish_message
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         run => { isa => 'Int', optional => 0 },
         candidate_model => { isa => 'model', optional => 1 }
    );
    my $run = $parm{'run'};
    my $candidate_model = $parm{'candidate_model'};

    my $ui_text;
    # Log the run
    $ui_text .= sprintf("%3s",$run+1) . sprintf("%25s",$self->models->[$run]->filename);
    my $log_text = $run+1 . ',' . $self->models->[$run]->filename . ',';
    if( ($self->verbose or $self->quick_summarize) and (not $self->clean > 2)){
        foreach my $param ( 'ofv', 'covariance_step_successful', 'minimization_message' ) {
            if( $param eq 'minimization_message' ){
                $ui_text .= "\n";
            }
            if( defined $candidate_model ){
                my $ests = $candidate_model -> outputs -> [0] -> $param;
                # Loop the problems
                for ( my $j = 0; $j < scalar @{$ests}; $j++ ) {
                    if ( ref( $ests -> [$j][0] ) ne 'ARRAY' ) {
                        $ests -> [$j][0] =~ s/^\s*//;
                        $ests -> [$j][0] =~ s/\s*$//;
                        $log_text .= $ests -> [$j][0] .',';
                        if( $param eq 'minimization_message' ){
                            $ui_text .= sprintf("%70s",' ').$ests -> [$j][0];
                        }elsif( $param eq 'covariance_step_successful' ){
                            $ui_text .= sprintf("%12s",$ests -> [$j][0]);
                        }else{#ofv
                            $ui_text .= sprintf("%12.7g",$ests -> [$j][0]);
                        }
                    } else {

                        # Loop the parameter numbers (skip sub problem level)
                        for ( my $num = 0; $num < scalar @{$ests -> [$j][0]}; $num++ ) {
                            $log_text .= $ests -> [$j][0][$num] .',';
                            if( $param eq 'minimization_message' ){
                                $ui_text .= sprintf("%70s",' ');
                            }
                            $ui_text .= sprintf("%12s",$ests -> [$j][0][$num]);
                        }
                    }
                }
            }
        }
        ui -> print( category => 'all',
                     message  => $ui_text,
                     wrap     => 0,
                     newline => 0);
    }

    open( LOG, ">>".$self->logfile->[0] );
    print LOG $log_text;
    print LOG "\n";
    close LOG;
}

sub create_sub_dir
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         subDir => { isa => 'Str', optional => 0 },
         modelname => { isa => 'Str', optional => 0 }
    );
    my $subDir = $parm{'subDir'};
    my $modelname = $parm{'modelname'};
    my $tmp_dir;

    my $file;
    ($tmp_dir, $file) = OSspecific::absolute_path( $self->directory . '/' .  $subDir, '');

    unless( -e $tmp_dir ){
        unless (mkdir( $tmp_dir )){
            croak("Error in create_sub_dir: Failed to create $tmp_dir\n system error $! \n");
        }
    }
    open( FILE, '>'.$tmp_dir.'/modelname' );
    print FILE "$modelname\n";
    close(FILE);

    return $tmp_dir;
}

sub create_R_plots_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        rplot => { isa => 'rplots', optional => 0 }
    );
    my $rplot = $parm{'rplot'};
    #only to be used from execute, single model
    #TODO update inits add xpose tables, partially interactive

    # Find a table with residuals. Needed for the OFV_i vs #OBS_i plot
    my $res_table_file = '';
    my $tables = $self->models->[0]->problems->[0]->tables;
    if (defined $tables) {
        my $found = 0;
        my $filename;
        for my $table (@{$self->models->[0]->problems->[0]->tables}) {
            for my $option (@{$table->options}) {
                if ($option->name eq 'RES' or $option->name eq 'WRES' or $option->name eq 'CWRES') {
                    $found = 1;
                }
                if ($option->name =~ 'FIL|FILE') {
                    $filename = $option->value;
                }
            }
            if ($found and defined $filename) {
                $res_table_file = $filename;
            } else {
                $found = 0;
                $filename = undef;
            }
        }
    }

    $rplot->add_preamble(code => [ "res.table <- '$res_table_file'" ] );
}

1;
