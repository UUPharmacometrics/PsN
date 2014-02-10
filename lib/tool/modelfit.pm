package tool::modelfit;

#use Carp;
use include_modules;
use Config;
use Cwd;
use Data::Dumper;
use File::Copy qw/cp mv/;
use File::Path;
use File::Glob;
use FindBin qw($Bin);
use Storable;
use Math::Random;
use nonmem;
use POSIX ":sys_wait_h";
use output;
use OSspecific;
use ui;
use Time::HiRes;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

my @nm7_extensions = ('.ext','.cov','.cor','.coi','.phi','.phm',
		      '.shk','.grd','.xml','.smt','.rmt');

has 'data_path' => ( is => 'rw', isa => 'Str' );
has 'tail_output' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nmtran_skip_model' => ( is => 'rw', isa => 'Int', default => 10000 );
has 'full_path_nmtran' => ( is => 'rw', isa => 'Str' );
has 'nmtran_error_file' => ( is => 'rw', isa => 'Str', default => 'nmtran_error.txt' );
has 'general_error_file' => ( is => 'rw', isa => 'Str', default => 'psn_nonmem_error_messages.txt' );
has 'base_msfo_name' => ( is => 'rw', isa => 'Str' );
has 'max_hash' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'full_path_nmfe' => ( is => 'rw', isa => 'Str' );
has 'wintail_exe' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'wintail_command' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'cutoff' => ( is => 'rw', isa => 'Num' );
has 'cutoff_thetas' => ( is => 'rw', isa => 'ArrayRef' );
has 'cut_thetas_rounding_errors' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'cut_thetas_maxevals' => ( is => 'rw', isa => 'Bool', default => 0);
has 'handle_hessian_npd' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['modelfit.log'] } );
has '_raw_results_callback' => ( is => 'rw' );



# No method, just documentation
#start description
    #
    # In PsN versions < 2.0, the functionality for actually running
    # NONMEM on models and data PsN objects was provided by the model
    # class. As of PsN versions 2.0 and higher, this functinality has
    # been moved to the separate class I<modelfit> in order to make the
    # class responsibilities clearer.
    #
    # Fitting a model can be viewed as a special case of a more
    # general set of tools for population PK/PD. In PsN, the
    # modelfit class is therefore a specialization of a general PsN
    # tool class. The tool class itself is not capable of much at
    # all but to define a common structure for all PsN tools.
    #
    # All attributes and (class) methods specified for the general
    # tool class are inherited in the modelfit class. Some (class) methods
    # are defined in both classes (e.g. the L</run>) and in these
    # cases it is the modelfit version that will be used.
    #
    # =begin html
    #
    # <tr>Please look at the documentation for the <a
    # href="../tool.html">general tool class</a> as well as the <a
    # href="#examples">examples</a> section of this document for
    # descriptions of the setting that are available for all
    # tools.</tr>
    #
    # =end html
    # 
    # =begin man
    #
    # Please look at the documentation for the general I<tool> class
    # as well as the L</examples> section of this document for
    # descriptions of the setting that are available for all tools.
    #
    # =end man
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
    #   my $data = $model -> datas -> [0];
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

#start see_also
    # =begin html
    #
    # <a HREF="../data.html">data</a>, <a
    # HREF="../model.html">model</a> <a
    # HREF="../output.html">output</a>, <a
    # HREF="../tool.html">tool</a>
    #
    # =end html
    #
    # =begin man
    #
    # data, model, output, tool
    #
    # =end man
#end see_also

sub BUILDARGS
{
	my $self = shift;
	return $self->SUPER::BUILDARGS(@_);
}

sub BUILD
{
	my $this  = shift;

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

	if ( defined $this->logfile ) {
		$this->logfile([join('', OSspecific::absolute_path( $this->directory, $this->logfile->[0]) ) ]);
	}

	if ($this->nmqual) {
		unless (defined $this->nmqual_xml){
			croak("No nmqual_xml defined. Required for nmqual. Exiting\n");
		}
		$this->nonmem_options($this->nmqual_xml .
			','.$this->nmfe_options);
	} elsif ($this->nmfe or $this->run_on_sge_nmfe) {
		$this->nonmem_options($this->nmfe_options);
		unless (defined $this->full_path_nmtran()){
			$this->nmfe_setup(nm_version => $this->nm_version);
		}
	}

	if( $this->run_on_lsf or $this->run_on_ud or $this->run_on_zink or
		$this->run_on_torque or $this->run_on_slurm or $this->run_on_lsf_nmfe or
		$this->run_on_sge_nmfe or $this->run_on_sge ) {
		$this->run_local(0);
	} else {
		$this->run_local(1);
	}

	if ( $this->handle_msfo ) {
		$this->handle_crashes(1);
	}

	if ( $this->handle_crashes and $this->crash_restarts > 0 ) {
		$this->crash_restarts($this->crash_restarts + 1);
	}

	$this->calculate_raw_results_width();

	$this->raw_line_structure(ext::Config::Tiny -> new());
}

sub run
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		resuming => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $resuming = $parm{'resuming'};
	my @results;

	my $cwd = getcwd();
	my $started_all_models = 0;
	my $started_all_models_print = 0;
	chdir( $self->directory );
	$self->stop_motion_call(tool => 'modelfit', message => "Changed directory to " . $self->directory)
	if ($self->stop_motion());

	# {{{ sanity checks

	my @models;
	if ( defined $self->models ) {
		@models = @{ $self->models };
	} else {
		croak("Have no models!" );
	}

	my $threads = $self->threads;
	$threads = $#models + 1 if ( $threads > $#models + 1); 

	# }}}

	# {{{ print starting messages 
	ui -> print( category => 'all',
		message  => 'Starting ' . scalar(@models) . ' NONMEM executions. '. $threads .' in parallel.',newline => 1 ) 
	unless $self->parent_threads > 1;
	ui -> print( category => 'all',
		message  => "Run number\tModel name\tOFV\tCovariance step successful.",
		newline => 1)  if $self->verbose;

	# }}}

	# {{{ Print model-NM_run translation file

	open( MNT, ">model_NMrun_translation.txt");
	for ( my $run = 0; $run <= $#models; $run ++ ) {
		print MNT sprintf("%-40s",$models[$run]->filename),"NM_run",($run+1),"\n";
	}
	close( MNT );

	# }}}


	# {{{ Local execution

	# %queue_map is a mapping from nonmem.pm pID to run number.

	my %queue_map;

	# %queue_info is keyed on run number and contains information
	# about each nonmem run.

	my %queue_info;

	# @queue is an array of NM_run directory numbers. If "X" is in
	# @queue, then psn.mod in "NM_runX" is scheduled to be run. It
	# initialized to all models in the tool. Note that if X is in
	# the queue, it doesn't mean NM_runX exists.

	my @queue = (0..$#models);
	my $all_jobs_started = 0;

	# We loop while there is content in the queue (which shrinks when jobs are submitted and grows when restarts are needed)
	# and while we have jobs running, i.e. scalar keys %queue_map > 0 (represented in the queue_info)

	while( (scalar(@queue) > 0) or (scalar keys %queue_map > 0) ){

		if ( (scalar @queue > 0) and (scalar keys %queue_map < $threads) ){
			#we may start a new job here 
			# This is where we initiate a new job:
			
			my $run = shift(@queue);
			$self->stop_motion_call(tool => 'modelfit', message => "Prepare to start the next job in the run queue")
				if ($self->stop_motion() > 1);
			
			# {{{ check for no run conditions. (e.g. job already run)
			
			if ( -e $self->models->[$run]->outputs->[0]->full_name and $self->rerun < 1 ) {
				
				if( not -e './NM_run' . ($run+1) . '/done' ){
					# here we have an .lst file, no done file and we are not
					# rerunning NONMEM. Which means we must create fake NM_run and
					# "done" files. (Usually this case occurs if we want to
					# use execute to create a summary or run table).
					
					mkdir( "./NM_run" . ($run+1) );
					open( DONE, ">./NM_run". ($run+1) ."/done.1" );
					print DONE "This is faked\nseed: 1 1\n" ;
					close( DONE );

					my ($raw_results_row, $nonp_row) = $self->create_raw_results_rows(
						max_hash => $self->max_hash,
						model => $self->models->[$run],
						model_number => $run + 1,
						raw_line_structure => $self->raw_line_structure );

					$self->raw_results([]) unless defined $self->raw_results;
					$self->raw_nonp_results([]) unless defined $self->raw_nonp_results;

					push( @{$self->raw_results}, @{$raw_results_row} );
					push( @{$self->raw_nonp_results}, @{$nonp_row} );

					# TODO Must copy tablefiles if they exist.

					# We use the existing .lst file as the final product.
					cp( $self->models->[$run]->outputs->[0]->full_name, './NM_run' . ($run+1) .'/psn.lst' );
					#copy NM7 files also...
				}

				# TODO Should check for tablefiles.

				my $modulus = (($#models+1) <= 10) ? 1 : (($#models+1) / 10)+1;

				if ( $run % $modulus == 0 or $run == 0 or $run == $#models ) {
					ui -> print( category => 'all', wrap => 0, newline => 0,
								 message  => 'D:'.( $run + 1 ).' .. ' )
						unless( $self->parent_threads > 1 or $self->verbose );
				}

				$queue_info{$run}{'candidate_model'} =
					model -> new( filename => "./NM_run" . ($run + 1) . "/psn.mod",
								  target               => 'disk',
								  ignore_missing_files => 1,
								  quick_reload         => 1,
								  cwres                => $models[$run] -> cwres() );
				$self->print_finish_message( candidate_model => $queue_info{$run}{'candidate_model'}, run => $run );

				$self->prepared_models([]) unless defined $self->prepared_models;
				push( @{$self->prepared_models->[$run]{'own'}}, $queue_info{$run}{'candidate_model'} );

				next; # We are done with this model. It has already been run. Go back to main while loop.
			}

			# }}}

			# {{{ delay code (to avoid overload of new processes)

			if ($threads > 1) {

				if ($run > 0) {
					my $start_sleep = Time::HiRes::time();

					my ($min_sleep, $max_sleep); # min_sleep is in microseconds and max_sleep is in seconds.

					if( defined $PsN::config -> {'_'} -> {'min_fork_delay'} ) {
						$min_sleep = $PsN::config -> {'_'} -> {'min_fork_delay'};
					} else {
						$min_sleep = 0;
					}

					if( defined $PsN::config -> {'_'} -> {'max_fork_delay'} ) {
						$max_sleep = $PsN::config -> {'_'} -> {'max_fork_delay'};
					} else {
						$max_sleep = 0;
					}

					if( $min_sleep > $max_sleep * 1000000 ){
						$max_sleep = $min_sleep;
					}

					while( (not( -e 'NM_run'.($run).'/psn.lst' )) and 
						   (Time::HiRes::time() - $start_sleep) < $max_sleep ) {
						Time::HiRes::usleep($min_sleep);
					}
				}

			}

			# }}}

			# {{{ Call to run_nonmem 

			# This will stop nasty prints from model, output and data
			# which are set to print for the scm.
			my $old_category = ui -> category();

			ui -> category('modelfit');

			my $stoptmp = '';
			$stoptmp = "Created NM_run".($run+1)."." unless (-d 'NM_run'.($run+1));
			$self -> create_sub_dir( subDir => '/NM_run'.($run+1),
									 modelname => $models[$run]->filename);
			chdir( 'NM_run'.($run+1) );
			$self->stop_motion_call(tool=>'modelfit',message => $stoptmp." Moved to NM_run".($run+1).".")
				if ($self->stop_motion()> 1);

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

			unless( exists $queue_info{$run} ){
				#if stats-runs.csv exists then copy_model_and_input does not do anything
				#but read psn.mod into candidate_model object
				my $run_nmtran = 0;
				if ($self->check_nmtran and 
					(($run+1) < $self->nmtran_skip_model)){
					$run_nmtran = 1;
				}
				$queue_info{$run}{'candidate_model'} = $self -> copy_model_and_input(model=>$models[$run],
																					 source => '../',
																					 run_nmtran => $run_nmtran);
				$queue_info{$run}{'model'} = $models[$run];
				$queue_info{$run}{'modelfile_tainted'} = 1;
				$queue_info{$run}{'have_accepted_run'} = 0;
				$queue_info{$run}{'tries'} = 0;
				$queue_info{$run}{'crashes'} = 0;
				$queue_info{$run}{'evals'} = 0;
				$queue_info{$run}{'run_results'} = [];
				$queue_info{$run}{'raw_results'} = [];
				$queue_info{$run}{'raw_nonp_results'} = [];
				$queue_info{$run}{'start_time'} = 0;
				$queue_info{$run}{'send_email'} = 0;

				# {{{ printing progress

				# We don't want to print all starting models if they are
				# more than ten. But we always want to print the first
				# and last

				my $modulus = (($#models+1) <= 10) ? 1 : (($#models+1) / 10);

				if ($self->send_email()){
					my $mail_modulus = (($#models+1) <= 3) ? 1 : (($#models+1) / 5);
					if ( $run == 0 ) {
						$queue_info{$run}{'send_email'} = 2 ; #ALL
					}elsif ( $run % $mail_modulus == 0 or $run == $#models ) {
						$queue_info{$run}{'send_email'} = 1; #END
					}
				}

				if ( $run % $modulus == 0 or $run == 0 or $run == $#models ) {
					#only slurm_submit uses email info right now
					# The unless checks if tool owning the modelfit is
					# running more modelfits, in wich case we should be
					# silent to avoid confusion. The $done check is made do
					# diffrentiate between allready run processes.

					ui -> print( category => 'all', wrap => 0, newline => 0,
								 message  => 'S:'.($run+1).' .. ' )
						unless ( $self->parent_threads > 1 or $self->verbose );
				}
				$started_all_models = 1 if ($run == $#models);

				# }}}
			} else {
				$self->stop_motion_call(tool => 'modelfit', message => "Did not have to copy model and input, this is a retry.")
					if ($self->stop_motion() > 1);
			}

			my %options_hash = %{$self -> _get_run_options(run_id => $run)}; 

			$self -> run_nonmem ( run_no          => $run,
								  nm_version      => $options_hash{'nm_version'},
								  queue_info      => $queue_info{$run},
								  queue_map       => \%queue_map);

			ui -> print( category => 'all', message  => "\nAll executions started.",newline => 1 )
				if ($started_all_models and $self->parent_threads <= 1  and not $self->verbose and not $started_all_models_print);
			$started_all_models_print = 1;
			chdir('..');
			$self->stop_motion_call(tool=>'modelfit',message => "change directory one level up")
				if ($self->stop_motion > 1);

			ui -> category( $old_category );

			# }}}



			next; #go back to main while loop to check if there is another job that can be started
		}
	

		# We could not start a new job, so we look for jobs that have been started and
		# finished. If we find one, we set $pid to that job.
		#we must loop here until a job is finished, because we know we cannot start new job
		# until one is finished

		my $pid = 0;

		# {{{ Get potiential finished pid

		while (not $pid){
			# (sleep to make polling less demanding).
					
			if( defined $PsN::config -> {'_'} -> {'job_polling_interval'} and
				$PsN::config -> {'_'} -> {'job_polling_interval'} > 0 ) {
				sleep($PsN::config -> {'_'} -> {'job_polling_interval'});
			} else {
				sleep(1);
			}

			foreach my $check_pid( keys %queue_map ){
				
				if( $check_pid =~ /^rerun_/ ){
					
					# A pid that starts with "rerun" is a rerun and is always
					# "finished".
					
					$pid = $check_pid;
					last;
				}elsif( $check_pid =~ /^fail_/ ){
					
					# A pid fail_ is a failed grid submit and is always
					# "finished".
					
					$pid = $check_pid;
					last;
				}
				
				# Diffrent environments have diffrent ways of reporting
				# job status. Here we check which environment we are in
				# and act accordingly.
				
				if ( $self->run_on_ud ) {
					
					$pid = $self -> ud_monitor( jobId => $check_pid );
					
					if( $pid ){
						$self -> ud_retrieve( jobId => $check_pid,
											  run_no => $queue_map{$check_pid} );
					}
					
				} elsif ( $self->run_on_sge ) {
					
					$pid = $self -> sge_monitor( jobId => $check_pid );
					
				} elsif ( $self->run_on_sge_nmfe ) {
					
					$pid = $self -> sge_nmfe_monitor( jobId => $check_pid );
					
				} elsif ( $self->run_on_slurm ) {
					
					$pid = $self -> slurm_monitor( jobId => $check_pid );

				} elsif ( $self->run_on_zink ) {

					$pid = $self -> zink_monitor( jobId => $check_pid );

				} elsif ( $self->run_on_lsf ) {

					$pid = $self -> lsf_monitor( jobId => $check_pid );

				} elsif ( $self->run_on_lsf_nmfe ) {

					$pid = $self -> lsf_nmfe_monitor( jobId => $check_pid );

				} elsif ( $self->run_on_torque ) {

					$pid = $self -> torque_monitor( jobId => $check_pid );

				} else { # Local process poll

					if( $Config{osname} eq 'MSWin32' ){

						my $exit_code;

						# GetExitCode is supposed to return a value indicating
						# if the process is still running, however it seems to
						# allways return 0. $exit_code however is update and
						# seems to be nonzero if the process is running.

						$queue_info{$queue_map{$check_pid}}{'winproc'}->GetExitCode($exit_code);

						if( $exit_code == 0 ){
							$pid = $check_pid;
						}

					} else {

						$pid = waitpid($check_pid, WNOHANG);

						# Waitpid will return $check_pid if that process has
						# finished and 0 if it is still running.

						if( $pid == -1 ){
							# If waitpid return -1 the child has probably finished
							# and has been "Reaped" by someone else. We treat this
							# case as the child has finished. If this happens many
							# times in the same NM_runX directory, there is probably
							# something wrong and we die(). (I [PP] suspect that we
							# never/seldom reach 10 reruns in one NM_runX directory)

							my $run = $queue_map{$check_pid};

							$queue_info{$run}{'nr_wait'}++;
							if( $queue_info{$run}{'nr_wait'} > 10 ){
								croak("Nonmem run was lost\n");
							}
							$pid = $check_pid;
						}

					}
				}

				last if $pid; #we found a finished run, do not loop over more running pid

			} #end loop over running pid


			if( not $pid ){

				# No process has finished. 
				# we cannot start another job
				next; # Return to polling for finished jobs.
			
			} else { 

				# {{{ Here, a process has finished and we check for restarts.

				my $run = $queue_map{$pid};

				my $candidate_model = $queue_info{$run}{'candidate_model'};

				my $work_dir = 'NM_run' . ($run+1) ;
				chdir($work_dir);
				$self->stop_motion_call(tool=>'modelfit',message => "A NONMEM run has finished (system process with id $pid ".
										"has disappeared).\n".
										"Changed to directory $work_dir of this process to check results.")
					if ($self->stop_motion() > 1);

				$self->compute_cwres(queue_info => $queue_info{$run}, run_no => $run);

				$self->compute_iofv(queue_info => $queue_info{$run}, run_no => $run);

				# Make sure that each process gets a unique random sequence:
				my $tmpseed = defined $self->seed() ? $self->seed() : random_uniform_integer(1,1,99999999);
				my $tmptry  = exists $queue_info{$run}{'tries'} ? $queue_info{$run}{'tries'} : 0;
				#have two alternatives: first for backward reproducability of sequences
				#second to prevent bug when very large number of models
				if ($run < 5000){
					random_set_seed(($tmpseed+100000*($run+1)),($tmptry+1));
				}else{
					my $phrase = "seed $tmpseed try $tmptry run $run";
					random_set_seed_from_phrase($phrase);
				}

				my %options_hash = %{$self -> _get_run_options(run_id => $run)};

				for my $key (keys %options_hash) {			# ADDED
					delete $options_hash{$key} unless defined $options_hash{$key};
				}

				#careful here, option maxevals is set on commandline, but model->maxeval() is 
				#array of values actually set in modelfile
				my $do_restart = $self -> restart_needed( %options_hash,
														  queue_info  => $queue_info{$run},
														  run_no      => $run,
														  maxevals		=> $candidate_model->maxevals);

				if ($self->abort_on_fail) {
					my $tries = \$queue_info{$run} -> {'tries'};
					if ($queue_info{$run}->{'run_results'}->[${$tries}]->{'failed'}) {
						$do_restart = 0;
						@queue = ();
					}
				}

				if ($do_restart) {
					unshift(@queue, $run);
					delete($queue_map{$pid});
					chdir('..');	    
					$self->stop_motion_call(tool => 'modelfit', message => "Had to do restart, put job in queue.\nChange directory one level up ")
						if ($self->stop_motion() > 1);
				} else {
					$self->stop_motion_call(tool => 'modelfit', message => "did not have to restart this model")
						if ($self->stop_motion() > 1);
					$self -> select_best_model(run_no          => $run,
											   nm_version      => $options_hash{'nm_version'},
											   queue_info      => $queue_info{$run});
					
					# {{{ Print finishing messages
					
					if( scalar @queue == 0 ) {
						if( $all_jobs_started == 0 ) {

							ui -> print( category => 'all',
										 message  => "Waiting for all NONMEM runs to finish:",
										 newline => 1 ) 
								if( $self->parent_threads <= 1 and $threads > 1 and not $self->verbose );
							
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

					# }}}	      

					chdir( '..' );
					$self->stop_motion_call(tool=>'modelfit',message => "changed directory one level up")
						if ($self->stop_motion()> 1);

					# {{{ cleaning and done file
					
					if( $self->clean >= 3 ){
						unlink( <$work_dir/worker*/*> );
						my @removedir = <$work_dir/worker*>;
						foreach my $remdir (@removedir){
							rmdir ($remdir) if (-d $remdir);
						}
						unless ((-e $work_dir.'/'.$self->general_error_file) or (-e $work_dir.'/'.$self->nmtran_error_file)){ 
							
							#leave if error message,
							unlink( <$work_dir/*> )  ; 
							unless( rmdir( $work_dir ) ){debug -> warn( message => "Unable to remove $work_dir directory: $! ." )};
							$self->stop_motion_call(tool=>'modelfit',message => "clean level is >=3, removed $work_dir")
								if ($self->stop_motion()> 1);
						}
			#			sleep(2);
					} else {
						1;

					}

					# }}}

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
	
	# }}}

		# }}} local execution
	
	$self->prepare_raw_results();

	$self->print_raw_results();

	chdir($cwd);
	$self->stop_motion_call(tool=>'modelfit', message => "changed directory to $cwd")
		if ($self->stop_motion() > 1);

		# {{{ clean $self -> directory 
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
			$self->stop_motion_call(tool=>'modelfit',message => "clean level is >=3, removing $dir completely")
			if ($self->stop_motion()> 1);
		}
	}
	# }}}

	return \@results;
}

sub select_best_model
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 run_no => { isa => 'Int', optional => 1 },
		 nm_version => { isa => 'Str', optional => 1 },
		 queue_info => { isa => 'HashRef', optional => 0 }
	);
	my $run_no = $parm{'run_no'};
	my $nm_version = $parm{'nm_version'};
	my %queue_info = defined $parm{'queue_info'} ? %{$parm{'queue_info'}} : ();

  # -------------- Notes about Final model selection -----------------
  
  # Since we have reruns with pertubation and now also forced (or
  # automatic) pertubation the final model is not equal to the
  # original model. We consider three implicit subsets. Those that pass
  # the picky test, those that don't pass the picky test but have ofv
  # and, finally, those that doesn't produce an ofv. 


  #pass picky will only be defined if picky option is set in run
  #The final model will be the one that passes picky,
  # otherwise it will be the model that has the lowest ofv value,
  # unless a model with minimization successful has an ofv where
  # ofv_successful-accepted_ofv_difference is the lowest. 
  # If two models are equal based on ofv with minimization-succcessful correction
  # then significant digits will decide which is best.
  # If no ofv value is produced it will be the basic model.
      
  # Get all runs that passed the picky test (if picky is used)

  my $queue_info_ref = $parm{'queue_info'};
  my $run_results = $queue_info_ref -> {'run_results'};
  my $model = $queue_info_ref -> {'model'};
  my $candidate_model = $queue_info_ref -> {'candidate_model'};
  if (-e 'stats-runs.csv'){
    $self->stop_motion_call(tool=>'modelfit',message => "Have previously copied best model to psn.mod.")
	if ($self->stop_motion()> 1);
    if ( $run_results -> [0] -> {'failed'} ){
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

  }else{
    $self->stop_motion_call(tool=>'modelfit',message => "check which run was the best ")
	if ($self->stop_motion()> 1);
    
    unless( defined $parm{'queue_info'} ){
      # The queue_info must be defined here!
			croak("Internal run queue corrupt\n" );
		}

    my $selected;
    my $best_significant_digits = 0;
    my $best_passed_picky = 0;
    my $best_successful = 0;

    my $have_any_ofv=0;
    my $best_corrected_ofv=999999999;
    my $best_actual_picky_ofv=999999999;
    my $accepted_ofv_diff = $self->accepted_ofv_difference;
    my $warning;

    for(my $i = 0; $i < scalar @{$run_results}; $i++ ){
      if( defined( $run_results -> [$i] -> {'ofv'} )  ) {
	$have_any_ofv=1;
	if ($run_results->[$i]->{'minimization_successful'}){
	  if (($run_results -> [$i] -> {'ofv'}-$accepted_ofv_diff) < $best_corrected_ofv){
	    $best_corrected_ofv = ($run_results -> [$i] -> {'ofv'}-$accepted_ofv_diff);
	  }
	  if ( $run_results -> [$i] -> {'pass_picky'}
	       and $run_results -> [$i] -> {'ofv'} < $best_actual_picky_ofv){
	    $best_actual_picky_ofv = $run_results -> [$i] -> {'ofv'};
	  } 
	}else{
	  if ($run_results -> [$i] -> {'ofv'} < $best_corrected_ofv){
	    $best_corrected_ofv = $run_results -> [$i] -> {'ofv'};
	  }
	}
      }
    }
    
    for(my $i = 0; $i < scalar @{$run_results}; $i++ ){
      if (defined($run_results->[$i]->{'ofv'})){
				my $corrected = $run_results->[$i]->{'ofv'};
				$corrected = ($run_results->[$i]->{'ofv'} - $accepted_ofv_diff) 
					if ($run_results->[$i]->{'minimization_successful'});

	#picky always precedence, but might need to warn
	if ( $run_results -> [$i] -> {'pass_picky'}){ 
	  if (($run_results->[$i]->{'ofv'} <= $best_actual_picky_ofv)
	      and (not $best_passed_picky)){
	    #if more than 1 has best actual picky ofv then pick 1st of them
	    $selected = ($i+1);
	    $best_passed_picky = 1 ;
	    $best_successful = 1 ;
	    $best_significant_digits = $run_results -> [$i] -> {'significant_digits'};
	    $warning = "Warning: Could be a local minimum\n" if ($corrected > $best_corrected_ofv)
	  }
	}elsif ($corrected <= $best_corrected_ofv and (not $best_passed_picky)){
	  #never get here if pass picky
	  if (defined $selected){
	    #we have two equal corrected ofv
	    if ( $run_results -> [$i] -> {'minimization_successful'} 
					and ((not $best_successful) or
						($best_successful and 
						 ($best_significant_digits<$run_results->[$i]->{'significant_digits'})))){
	      $selected = ($i+1);
	      $best_passed_picky = 0;
	      $best_successful = 1 ;
	      $best_significant_digits = $run_results -> [$i] -> {'significant_digits'};
	    }elsif ( (not $best_successful) and
		     ($best_significant_digits<$run_results->[$i]->{'significant_digits'})){
	      $selected = ($i+1);
	      $best_passed_picky = 0 ;
	      $best_successful = 0 ;
	      $best_significant_digits = $run_results -> [$i] -> {'significant_digits'};
	    }
	  } else {
	    $selected = ($i+1);
	    $best_passed_picky = 0 ;
	    if ( $run_results -> [$i] -> {'minimization_successful'}){
	      $best_successful = 1 ;
	    } else {
	      $best_successful = 0 ;
	    }
	    $best_significant_digits = $run_results -> [$i] -> {'significant_digits'};
	  }
	}
      }
    }

    $selected = defined $selected ? $selected : 1; #if none has defined ofv then select 1st
    
    open( STAT, '>stats-runs.csv' );
    print STAT Dumper \@{$run_results};
    print STAT "Selected $selected\n";
    print STAT "$warning" if (defined $warning);
    close( STAT );

    if ( $run_results -> [$selected-1] -> {'failed'} ){
      my @raw_row = [($run_no+1,'1','1','run failed: '.($run_results -> [$selected-1] -> {'failed'}))];
			$self->raw_results([]) unless defined $self->raw_results;
      push( @{$self->raw_results}, @raw_row );
      push( @{$self->raw_nonp_results}, @raw_row ) if (defined $self->raw_nonp_results);
 
	  #partial cleaning

	  if ( $self->clean >= 1 and $PsN::warnings_enabled == 0 ) {
		  unlink 'nonmem', 'nonmem5','nonmem6','nonmem7',
		  'nonmem5_adaptive','nonmem6_adaptive','nonmem7_adaptive', 
		  'nonmem.exe','nonmem_mpi.exe','nonmem_mpi','NONMEM_MPI.exe','FDATA', 'FREPORT', 'FSUBS', 'FSUBS.f','FSUBS.f90', 
		  'FSUBS.for', 'LINK.LNK', 'FSTREAM', 'FCON.orig', 'FLIB', 'FCON','PRDERR',
		  'nmprd4p.mod','nul',
		  'fsubs','fsubs.f','fsubs.for','fsubs.f90','FSUBS2','FSUBS_MU.F90';

		  unlink 'LINKC.LNK','compile.lnk','gfortran.txt','ifort.txt','garbage.out',
		  'newline','nmexec.set','parafile.set','prcompile.set','prdefault.set',
		  'prsame.set','psn.log','rundir.set','runpdir.set','temporaryfile.xml';
		  unlink 'temp.out','trashfile.xxx','trskip.set','worker.set','xmloff.set';
		  unlink 'prsizes.f90','licfile.set','background.set','FSIZES';
		  #do not delete INTER, needed for saving data from crashed runs

		  unlink( <worker*/*> );
		  my @removedir = <worker*>;
		  foreach my $remdir (@removedir){
			  rmdir ($remdir) if (-d $remdir);
		  }

		  if( $self->clean >= 2 ){
			  unlink( <temp_dir/*> );
			  rmdir( 'temp_dir' );
		  }
	  }
    }else{
		my @raw_results_rows = @{$queue_info_ref -> {'raw_results'} -> [$selected-1]};
		
		foreach my $row ( @raw_results_rows ){
			shift( @{$row} );
			unshift( @{$row}, $run_no+1 );
		}
		
		$self->raw_results([]) unless defined $self->raw_results;
		push( @{$self->raw_results}, @raw_results_rows );
		push( @{$self->raw_nonp_results}, @{$queue_info_ref -> {'raw_nonp_results'} -> [$selected-1]} )
			if (defined $self->raw_nonp_results and defined $queue_info_ref -> {'raw_nonp_results'} -> [$selected-1]);
		
		
		$self -> copy_model_and_output( final_model   => $candidate_model,
										model         => $model,
										use_run       => $selected ? $selected : '' );
		
    }

  }

  if ( $self->nonparametric_etas and ( not -e 'np_etas.lst' or $self->rerun >=2 ) ) {
    
    # ---------------------  Create nonp eta model  -----------------------------
    
    # {{{ nonp eta model

    if( not -e 'np_etas.lst' or $self->rerun >= 2 ){
      
      ui -> print( category => 'execute',
		   message  => 'Creating NPETA model',newline => 1 );
      
      my $np_eta_mod = $candidate_model ->
	  copy( filename    => $self->directory .
		'NM_run'.($run_no+1).'/np_etas.mod',
		target      => 'mem',
		copy_data   => 0,
		copy_output => 0);
      

      my $nprob = 0;
      if ( defined $candidate_model->problems ) {
				$nprob = scalar(@{$candidate_model->problems});
      } else {
				carp("No problems defined in candidate_model" );
      }
      # We should have an MSFO file here
      for( my $i = 0; $i < $nprob; $i++ ) {
				my @msfo_arr = @{$candidate_model ->  get_option_value( option_name  => 'MSFO',
								record_name  => 'estimation',
								record_index => 'all',
								problem_index => $i)};
				my $msfi;
				while (not defined $msfi){
					$msfi = pop(@msfo_arr);
				}
				$np_eta_mod -> set_records( problem_numbers => [($i+1)],
						type            =>'msfi',
						record_strings  => ["$msfi"]); 
				$np_eta_mod -> set_records( problem_numbers => [($i+1)],
						type           => 'nonparametric',
						record_strings => [ 'ETAS UNCONDITIONAL '.
						'MSFO=npmsfo'.($i+1) ] );
			}
			$np_eta_mod -> remove_option( record_name => 'estimation',
					option_name => 'MSFO' );
			$np_eta_mod -> remove_records( type => 'theta' );
			$np_eta_mod -> remove_records( type => 'omega' );
			$np_eta_mod -> remove_records( type => 'sigma' );
			$np_eta_mod -> remove_records( type => 'table' );
			my @nomegas = @{$candidate_model -> nomegas};

			for( my $i = 0; $i <= $#nomegas; $i++ ) {
				my $marg_str = 'ID';
				for( my $j = 1; $j <= $nomegas[$i]; $j++ ) {
					$marg_str = $marg_str.' ETA'.$j;
				}
				$marg_str = $marg_str.' FILE='.$model -> filename.'.nonp_etas'.
					' NOAPPEND ONEHEADER NOPRINT FIRSTONLY';
				$np_eta_mod -> add_records( problem_numbers => [($i+1)],
						type            => 'table',
						record_strings  => [ $marg_str ] );	  
			}

			$np_eta_mod -> set_maxeval_zero(print_warning => 0,need_ofv => 0,
					niter_eonly => $self->niter_eonly,
					last_est_complete => $self->last_est_complete);

			$np_eta_mod -> _write;

      # }}} nonp eta model
      
      # ----------------------  run nonp eta model  -------------------------------
      
      # {{{ run eta model

      ui -> print( category => 'execute',
		   message  => 'Running NPETA model',newline => 1 );
      
      my $nonmem_object = nonmem -> new(
				adaptive => $self->adaptive,
				modelfile => 'np_etas.mod',
				version => $nm_version,
				nice => $self->nice,
				show_version => 0,
				display_iterations => $self->display_iterations(),
				parafile => $self->parafile(),
				nodes => $self->nodes(),
				nonmem_options => $self->nonmem_options()
				);

			if ($self->nmfe) {
				unless ($nonmem_object -> run_with_nmfe())
				{
					croak($nonmem_object -> error_message );
				}
			}elsif ($self->nmqual){
				unless ($nonmem_object -> run_with_nmqual())
				{
					croak($nonmem_object -> error_message );
				}
			} else {
				$nonmem_object -> fsubs( $np_eta_mod -> subroutine_files );

				unless( $nonmem_object -> compile() ){
					croak("NONMEM compilation failed:\n" . $nonmem_object -> error_message );
				}
				if( $nonmem_object -> nmtran_message =~ /WARNING/s and $self->verbose ){
					ui -> print(category => 'all',
							message => "NONMEM Warning: " . $nonmem_object -> nmtran_message,
							newline => 1);
				}
				$nonmem_object -> execute();
			}
      
      foreach my $table_files( @{$np_eta_mod -> table_names} ){
				foreach my $table_file( @{$table_files} ){
					my $dir = $model -> directory;
					cp( $table_file, $dir  );
				}
			}

			unlink 'nonmem','nonmem_mpi' ,'nonmem6', 'nonmem5','nonmem.exe','nonmem_mpi.exe','NONMEM_MPI.exe','nonmem6_adaptive', 'nonmem5_adaptive';
			unlink 'nonmem7', 'nonmem7_adaptive';
		}

    # }}} run eta model
    
  }
}

sub get_retry_name
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 retry => { isa => 'Int', optional => 1 },
		 crash => { isa => 'Int', optional => 1 },
		 filename => { isa => 'Str', optional => 1 }
	);
	my $retry = $parm{'retry'};
	my $crash = $parm{'crash'};
	my $filename = $parm{'filename'};

  $retry++;
  if (defined $crash){
    unless( $filename =~ s/\.([^.]+)$/-$retry-step$crash.$1/ ){
      $filename .= "-$retry-step$crash";
    }
  }else{
    unless( $filename =~ s/\.([^.]+)$/-$retry.$1/ ){
      $filename .= "-$retry";
    }
  }

	return $filename;
}

sub set_msfo_to_msfi
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		candidate_model => { isa => 'model', optional => 1 },
		retry => { isa => 'Int', optional => 1 },
		queue_info => { isa => 'Ref', optional => 1 }
	);
	my $candidate_model = $parm{'candidate_model'};
	my $retry = $parm{'retry'};
	my $queue_info = $parm{'queue_info'};

	my $filename = $queue_info -> {'model'} -> get_option_value(record_name => 'estimation',
		option_name => 'MSFO');

	$filename = $self->base_msfo_name if (defined $self->base_msfo_name);
	unless (defined $filename){
		ui -> print( category => 'all',  message  => "Warning, no MSFO option in model, ".
			"set_msfo_to_msfi will fail (used when handling option maxevals)",
			newline => 1);
	}

	my $msfo = $self -> get_retry_name( 'filename' => $filename,
		'retry' => $retry );

	my $msfi;

	if( $candidate_model -> outputs -> [0] -> msfo_has_terminated() ){

		$msfi = $msfo . '-step' . ($queue_info -> {'crashes'}-1);

		$candidate_model->remove_records( type => 'estimation' );

	} else {
		$msfi = $self -> get_retry_name( 'filename' => $filename,
			'retry' => $retry,
			crash => $queue_info -> {'crashes'});
	}

	unless( -e $msfi ){
		ui -> print( category => 'all',  message  => "Warning, MSFO file $msfi does not exist, ".
			"set_msfo_to_msfi will fail (used when handling option maxevals)",
			newline => 1);
	}

	$candidate_model->set_records(type=>'msfi',
		record_strings => [$msfi]);

	$candidate_model->remove_records(type=>'theta');
	$candidate_model->remove_records(type=>'omega');
	$candidate_model->remove_records(type=>'sigma');
	$candidate_model->_write;

}

sub reset_msfo
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		candidate_model => { isa => 'model', optional => 1 },
		basic_model => { isa => 'model', optional => 1 }
	);
	my $candidate_model = $parm{'candidate_model'};
	my $basic_model = $parm{'basic_model'};
	my $model_modified = 0;

	my @data_ref = @{$candidate_model -> record( record_name => 'msfi' )};
	# Check if there is a msfi record and then delete it
	my @new_problems;
	if (scalar(@data_ref)!=0) {
		$candidate_model->remove_records(type=>'msfi');

		# Set the intial values + boundaries to the first  values (update theta, omega, sigma)

		my @old_problems = @{$basic_model -> problems};
		@new_problems = @{$candidate_model -> problems};
		for ( my $i=0; $i <= $#old_problems; $i++ ) {
			foreach my $param ( 'thetas', 'omegas', 'sigmas' ) {
				$new_problems[$i] -> $param( Storable::dclone( $old_problems[$i] -> $param ) );
			}
		}						 

		$model_modified = 1;
	}

	my $filename = $basic_model -> get_option_value(record_name => 'estimation',
		option_name => 'MSFO');
	$filename = $self->base_msfo_name if (defined $self->base_msfo_name);

	if (defined $filename){
		$candidate_model-> set_option(record_name => 'estimation',option_name => 'MSFO',
			fuzzy_match => 1, option_value => $filename,
			problem_numbers => [scalar(@new_problems)]);
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
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 },
		header => { isa => 'ArrayRef[Str]', optional => 1 },
		models => { isa => 'ArrayRef', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my @header = defined $parm{'header'} ? @{$parm{'header'}} : ();
	my @models = defined $parm{'models'} ? @{$parm{'models'}} : ();
	my @new_header;
	my @raw_results;

	# As of PsN version 2.1.8, we don't handle problems and
	# subproblems in any of the tools but modelfit.

	unless (defined $self->raw_results) {
		croak("Failed to collect raw results from lst-file(s).\n".
			"Check lst-file(s) and ".$self->general_error_file." or ".$self->nmtran_error_file." in ".
			"NM_run directory/-ies for errors.\n".
			"If lst-file and error file are missing\n".
			"check perl installation and perl settings in psn.conf. ".
			"If applicable, also check cluster settings.\n");
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

	# {{{ header

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
#dangerous to turn this on, some models really do not have omega/sigma/theta
#		if (($max_hash{ $name } == 0) and defined $param_names{$name}
#		    and scalar(@{$param_names{$name}})>0){
#		  #probably msfi, labels from output instead
#		  foreach my $label (@{$param_names{$name}}){
#		    push( @new_header, $label );
#		  }
#		}
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
#dangerous to turn this on, some models really do not have omega/sigma/theta
#	      if (($max_hash{ $name } == 0) and defined $param_names{$param}
#		  and scalar(@{$param_names{$param}})>0){
#		#probably msfi, labels from output instead
#		foreach my $label (@{$param_names{$param}}){
#		  push( @new_header, 'se'.$label );
#		}
#	      }
				$success = 1;
				last;
			} 
		}
		unless( $success ){
			push( @new_header, $name ) unless ((defined $max_hash{$name}) and $max_hash{$name}== 0); #only print nburn_set if saem/bayes
		}
	}

	$self->raw_results_header(\@new_header);

	# }}} header

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

	return \@new_header ,\@raw_results;
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
  
  if( defined $self->raw_nonp_results ) {
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
  
  $self->raw_line_structure->write( 'raw_results_structure' );
  $self->stop_motion_call(tool=>'modelfit',message => "Printed file $raw_file")
      if ($self->stop_motion());
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

sub ud_submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'model', optional => 0 }
	);
	my $model = $parm{'model'};
	my $jobId = -1;

  my $script;
  unless( defined $PsN::config -> {'_'} -> {'ud_nonmem'} ){
    if( $Config{osname} eq 'MSWin32' ) {
      $script = 'nonmem.bat';
    } else {
      $script = 'nonmem.sh';
    }
  } else {
    $script = $PsN::config -> {'_'} -> {'ud_nonmem'};
  }
  
  if( system( "$script -s " . $model -> filename . "> nonmem_sh_stdout"  ) ){
    croak("UD submit script failed, check that $script is in your PATH.\nSystem error message: $!" );
  }
  
  open(JOBFILE, "JobId") or croak("Couldn't open UD grid JobId file for reading: $!" );
  $jobId = <JOBFILE>;
  close(JOBFILE);
  
	return $jobId;
}

sub ud_monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 jobId => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};

	#this cannot possible work, but leave it here if any user wants to use ud. 
	# will be easy to fix this
  my $script;
  unless( defined $PsN::config -> {'_'} -> {'ud_nonmem'} ){
    if( $Config{osname} eq 'MSWin32' ) {
      $script = 'nonmem.bat';
    } else {
      $script = 'nonmem.sh';
    }
  } else {
    $script = $PsN::config -> {'_'} -> {'ud_nonmem'};
  }


  my $stdout; # Variable to store output from "nonmem.bat"

  my $response = `$script -l $jobId 2>&1`;

  carp("$response" );
  if( $response =~ /Job State:\s+Completed/ ){ # regexp to find finished jobs.
      carp("Returning $jobId" );
      return $jobId; # Return the jobId found.
  }

  return 0;
}

sub ud_retrieve
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 jobId => { isa => 'Int', optional => 1 },
		 run_no => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};
	my $run_no = $parm{'run_no'};

  my $script;
  unless( defined $PsN::config -> {'_'} -> {'ud_nonmem'} ){
    if( $Config{osname} eq 'MSWin32' ) {
      $script = 'nonmem.bat';
    } else {
      $script = 'nonmem.sh';
    }
  } else {
    $script = $PsN::config -> {'_'} -> {'ud_nonmem'};
  }

  my $subDir = "NM_run".($run_no+1);
  my ($tmp_dir, $file) = OSspecific::absolute_path( $self->directory . '/' .
						    $subDir, '');
  if( system("$script -b -c -d ".$tmp_dir." -r $jobId > nonmem_bat_stdout") ){
    croak("UD submit script failed.\nSystem error message:$!" );
  }
  
  if( $Config{osname} ne 'MSWin32' ){
    cp($tmp_dir.'/psn.LST',$tmp_dir.'/psn.lst');
    unlink($tmp_dir.'/psn.LST');
  }
}

sub ud_retrieve2
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 jobId => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};
}

sub sge_submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'model', optional => 1 },
		 nm_version => { isa => 'Str', optional => 1 },
		 queue_info => { isa => 'Ref', optional => 1 }
	);
	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $queue_info = $parm{'queue_info'};

  my $fsubs = join( ',' , @{$model -> subroutine_files} );

  my $execution = 1 + $self->nmfe + 2 * ($self->nmqual); 
  # if nmfe is set, then sum will be 2, which means nmfe in nonmem.pm. If -nmqual is set sum will be 3 -> nmqual
  my $jobname = $queue_info -> {'model'} -> filename;
  $jobname = 'psn_'.$jobname if ($jobname =~ /^[0-9]/);
  my $flags = ' -cwd -b y -N ';
  if (defined $self->sge_prepend_flags()){
    $flags = ' '.$self->sge_prepend_flags().$flags;
  }
  my $qsubstring = 'qsub '.$flags. 
      $jobname.
      ($self->sge_resource ? ' -l '.$self->sge_resource.' ' : ' ') .
      ($self->sge_queue ? '-q '.$self->sge_queue.' ' : ' ') .
      ($PsN::config -> {'_'} -> {'remote_perl'} ? ' ' . $PsN::config -> {'_'} -> {'remote_perl'} : ' perl ') . " -I" .
      $PsN::lib_dir ."/../ " . 
      $PsN::lib_dir . "/nonmem.pm" . 
      " psn.mod psn.lst " . 
      $self->nice . " ". 
      $nm_version . " " .
      1 . " " . # compilation
      $execution . " " . # execution
      $self->display_iterations() . ' ' .
      $self->nonmem_options() . ' ' .
      $self->parafile() . ' ' .
      $self->nodes() . ' ' .
      $fsubs . ' > JobId';
  if( system( $qsubstring ) ){
    croak("Grid submit failed.\nSystem error message: $!" );
  }
  
  open(JOBFILE, "JobId") or croak("Couldn't open grid JobId file for reading: $!" );
  while( <JOBFILE> ){
    if( /Your job (\d+)/ ){
      $jobId = $1;
    }
  }
  close(JOBFILE);

	return $jobId;
}

sub sge_nmfe_submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'model', optional => 1 },
		 nm_version => { isa => 'Str', optional => 1 },
		 nodes => { isa => 'Int', default => 0, optional => 1 },
		 queue_info => { isa => 'Ref', optional => 1 }
	);
	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $nodes = $parm{'nodes'};
	my $queue_info = $parm{'queue_info'};

  unless (defined $self->full_path_nmfe()){
    $self->nmfe_setup(nm_version => $nm_version);
  }
  #edit nmfe7, add which file it is and nmfe_error


  # clean up from old compile
  unless ($PsN::nm_major_version == 7 
	  and defined $PsN::nm_minor_version and $PsN::nm_minor_version >1){
    unlink( 'FMSG','FLIB','FCON', 'FDATA', 'FREPORT','FSUBS', 'FSUBS.f','FSUBS.f90','FSUBS2','nmprd4p.mod');
    unlink('fsubs','fsubs.f90');
    unlink('LINK.LNK','FSTREAM', 'PRDERR', 'nonmem.exe', 'nonmem','FSUBS.for');
    unlink('nonmem5', 'nonmem6', 'nonmem7','nonmem5_adaptive', 'nonmem6_adaptive','nonmem7_adaptive' );
    unlink('ifort.txt','g95.txt','gfortran.txt','gfcompile.bat','g95compile.bat');
  }
  unlink('psn.lst','nmfe_error','OUTPUT','output','job_submission_error');

  #only support nmfe here, not nmqual or PsN compile

  my $jobname = $queue_info -> {'model'} -> filename;
  $jobname = 'psn_'.$jobname if ($jobname =~ /^[0-9]/);
  my $background = '-background';
  $background = '' if ($PsN::nm_major_version == 6);


  #cwd default in slurm
  # -J jobname
  #need to check translation for -b y

  my $flags = ' -N '.$jobname.' -j y -cwd -b y';
  if (defined $self->sge_prepend_flags()){
    $flags = ' '.$self->sge_prepend_flags().$flags;
  }
  my $parastring = '';
  unless ($self->parafile() eq 'none'){
    $parastring = '"-parafile='.$self->parafile().'"';
  }
  if ($nodes > 0){
    $parastring .= ' "[nodes]='.$nodes.'"';
  }
  my $switches='';
  unless ($self->nmfe_options() eq 'none'){
    my @switches = split( /,/ ,$self->nmfe_options());
    foreach my $sw (@switches){
      $switches .= ' -'.$sw;
    }
  }

  my $submitstring = $flags. 
      ($self->sge_resource ? ' -l '.$self->sge_resource.' ' : ' ') .
      ($self->sge_queue ? '-q '.$self->sge_queue.' ' : ' ') .
      $self->full_path_nmfe(). ' '.
      " psn.mod psn.lst $background ".$parastring." ".$switches;

  unless ($Config{osname} eq 'MSWin32' or $Config{osname} eq 'MSWin64'){
      system('echo qsub '.$submitstring.' > qsubcommand');
  }

  my $outp = `qsub $submitstring`;
  #write error to job_submission_error instead, let jobid be -1, and continue?
  ($outp =~ /^Your job (\d+)/)
      or croak("Grid submit failed.\nSystem error message: $outp" );
  $jobId = $1;

	return $jobId;
}

sub lsf_nmfe_submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'model', optional => 1 },
		 nm_version => { isa => 'Str', optional => 1 },
		 nodes => { isa => 'Int', default => 0, optional => 1 },
		 queue_info => { isa => 'Ref', optional => 1 }
	);
	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $nodes = $parm{'nodes'};
	my $queue_info = $parm{'queue_info'};

  unless (defined $self->full_path_nmfe()){
    $self->nmfe_setup(nm_version => $nm_version);
  }


  # clean up from old compile
  unless ($PsN::nm_major_version == 7 
	  and defined $PsN::nm_minor_version and $PsN::nm_minor_version >1){
    unlink( 'FMSG','FLIB','FCON', 'FDATA', 'FREPORT','FSUBS', 'FSUBS.f','FSUBS.f90','FSUBS2','nmprd4p.mod');
    unlink('fsubs','fsubs.f90');
    unlink('LINK.LNK','FSTREAM', 'PRDERR', 'nonmem.exe', 'nonmem','FSUBS.for');
    unlink('nonmem5', 'nonmem6', 'nonmem7','nonmem5_adaptive', 'nonmem6_adaptive','nonmem7_adaptive' );
    unlink('ifort.txt','g95.txt','gfortran.txt','gfcompile.bat','g95compile.bat');
  }
  unlink('psn.lst','nmfe_error','OUTPUT','output','job_submission_error');
  unlink('lsf_stderr_stdout','lsf_jobscript');

  #only support nmfe here, not nmqual or PsN compile

  my $jobname = $queue_info -> {'model'} -> filename;
  $jobname = 'psn_'.$jobname if ($jobname =~ /^[0-9]/);
  $jobname = $self->lsf_job_name if (defined $self->lsf_job_name);
  my $background = '-background';
  $background = '' if ($PsN::nm_major_version == 6 or $PsN::nm_major_version == 5);

  open( SUB, '>lsf_jobscript' );
  print SUB ( "#BSUB -J $jobname\n" );
  print SUB ( "#BSUB -e lsf_stderr_stdout\n" );
  print SUB ( "#BSUB -o lsf_stderr_stdout\n" );
  print SUB ( "#BSUB -q ".$self->lsf_queue."\n" ) 
      if (defined $self->lsf_queue);
  print SUB ( "#BSUB -P ".$self->lsf_project_name."\n" ) 
      if (defined $self->lsf_project_name);
  print SUB ( "#BSUB -c ".$self->lsf_ttl."\n" ) 
      if (defined $self->lsf_ttl);
  print SUB ( "#BSUB -R ".$self->lsf_resources."\n" ) 
      if (defined $self->lsf_resources);

  my $parastring = '';
  unless ($self->parafile() eq 'none'){
    $parastring = '"-parafile='.$self->parafile().'"';
  }
  if ($nodes > 0){
    $parastring .= ' "[nodes]='.$nodes.'"';
  }
  my $switches='';
  unless ($self->nmfe_options() eq 'none'){
    my @switches = split( /,/ ,$self->nmfe_options());
    foreach my $sw (@switches){
      $switches .= ' -'.$sw;
    }
  }

  print SUB ($self->full_path_nmfe().' '.
	     " psn.mod psn.lst $background ".$parastring.' '.$switches."\n");
  close (SUB);

  my $submitstring = 'bsub '.$self->lsf_options().' < lsf_jobscript 2>&1'; 

  my $lsf_out = `$submitstring`;
  if ($lsf_out=~/Job \<(\d+)\> is submitted/) {
    $jobId=$1;
  } else{
    open( ERR, '>job_submission_error' );
    print ERR ('COMMAND: '.$submitstring."\n");
    print ERR ('SYSTEM RESPONSE: '.$lsf_out."\n");
    print ERR ("RESULT: bsub command was not successful, could not submit nmfe run\n");
    close (ERR);
    ui -> print( category => 'all', message  => $lsf_out,newline => 1 );
    ui -> print( category => 'all', message  => "bsub command was not successful, could not submit nmfe run",newline => 1 );
    #now jobId is -1, handled outside, setting job to failed and never running bjobs.
  }

  sleep($self->lsf_sleep()) if (defined $self->lsf_sleep());

	return $jobId;
}

sub sge_monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 jobId => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};

	my $response = `qstat -j $jobId 2>&1`;
	
	if( $response =~ /Following jobs do not exist/ ){ # regexp to find finished jobs.
		return $jobId;
	}
	
	return 0;
}

sub sge_nmfe_monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 jobId => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};

  my $response = `qstat -j $jobId 2>&1`;

  if($response =~ /Following jobs do not exist/ ){ # regexp to find finished jobs.
      return $jobId;
  }elsif($response =~ /^usage: qstat/ ){
      return $jobId;
  }

  return 0;
}

sub lsf_nmfe_monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		jobId => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};

	my $string = "bjobs $jobId 2>&1";
	my $answer = `$string`;

	# /m flag in regex:
	#Treat string as multiple lines. That is, change "^" and "$" from matching 
	#the start or end of the string to matching the start or end of any line 
	#anywhere within the string. Skip /m when looking for DONE, unless using also
	#line beginning

	if (($answer=~/^$jobId /m) and ($answer=~/ DONE /)) {
		return $jobId; # Return the jobId found.
	}elsif (($answer=~/is not found/) or
		($answer=~/illegal option/) or
		($answer=~/Illegal job ID/) or
		($answer=~/No unfinished job found/)){
		ui -> print( category => 'all', message  => $answer,newline => 1 );
		ui -> print( category => 'all', message  => "lsf run error, jobID $jobId not".
			"recognized by system:",newline => 1 );
		return $jobId; # Return the jobId so that do not get infinite loop, 
		#let restart_needed detect and handle error (no psn.lst, no NMtran etc)
	}else{
		return 0; #try again later
	}
}

sub slurm_submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 1 },
							  nm_version => { isa => 'Str', optional => 1 },
							  nodes => { isa => 'Int', default => 0, optional => 1 },
							  queue_info => { isa => 'Ref', optional => 1 }
		);
	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $nodes = $parm{'nodes'};
	my $queue_info = $parm{'queue_info'};

	unless (defined $self->full_path_nmfe()){
		$self->nmfe_setup(nm_version => $nm_version);
	}
	#edit nmfe7, add which file it is and nmfe_error


	# clean up from old compile
	unless ($PsN::nm_major_version == 7 
			and defined $PsN::nm_minor_version and $PsN::nm_minor_version >1){
		unlink( 'FMSG','FLIB','FCON', 'FDATA', 'FREPORT','FSUBS', 'FSUBS.f','FSUBS.f90','FSUBS2','nmprd4p.mod');
		unlink('fsubs','fsubs.f90');
		unlink('LINK.LNK','FSTREAM', 'PRDERR', 'nonmem.exe', 'nonmem','FSUBS.for');
		unlink('nonmem5', 'nonmem6', 'nonmem7','nonmem5_adaptive', 'nonmem6_adaptive','nonmem7_adaptive' );
		unlink('ifort.txt','g95.txt','gfortran.txt','gfcompile.bat','g95compile.bat');
	}
	unlink('psn.lst','nmfe_error','OUTPUT','output','job_submission_error');

	#only support nmfe here, not nmqual or PsN compile

	my $jobname = $queue_info -> {'model'} -> filename;
	$jobname = 'psn_'.$jobname if ($jobname =~ /^[0-9]/);
	my $background = '-background';
	$background = '' if ($PsN::nm_major_version == 6);

	my $parastring = '';
	unless ($self->parafile() eq 'none'){
		$parastring = '"-parafile='.$self->parafile().'"';
	}
	if ($nodes > 0){
		$parastring .= ' "[nodes]='.$nodes.'"';
	}
	my $switches='';
	unless ($self->nmfe_options() eq 'none'){
		my @switches = split( /,/ ,$self->nmfe_options());
		foreach my $sw (@switches){
			$switches .= ' -'.$sw;
		}
	}

	#cwd default in slurm
	# -J jobname
	#need to check translation for -b y

	my $flags = ' -J '.$jobname;
	$flags .= ' -o nmfe.output -e nmfe.output ';
	if (defined $self->slurm_account()){
		$flags .= ' -A '.$self->slurm_account() ;
	}else{
		if( $PsN::config -> {'default_options'} -> {'uppmax'}){
			croak("slurm account must be defined on uppmax");
		}
	}
	if (defined $self->max_runtime()){
		#Acceptable time formats include #minutes", 
		#minutes:seconds", #hours:minutes:seconds", #days-hours", 
		#days-hours:minutes¡ and ´days-hours:minutes:seconds". 
		unless (($self->max_runtime() =~ /^[0-9]+$/) or
				($self->max_runtime() =~ /^[0-9]+\:[0-9]+\:[0-9]+$/) or
				($self->max_runtime() =~ /^[0-9]+\-[0-9]+$/)){
			croak("max_runtime must have format minutes, ".
				  "hours:minutes:seconds, or days-hours");
		}
		$flags .= ' -t '.$self->max_runtime() ;
	}
	if (defined $self->slurm_partition()){
		$flags .= ' -p '.$self->slurm_partition() ;
	}
#at most 3GB RAM 
	if( $PsN::config -> {'default_options'} -> {'uppmax'}){
		$flags .= ' -p core -n 1 '; #single core
	}

	if ($queue_info -> {'send_email'} and defined $self->email_address()){
		if ($queue_info -> {'send_email'}  == 2){
			$flags .= ' --mail-user='.$self->email_address().' --mail-type=ALL ';
		}else{
			$flags .= ' --mail-user='.$self->email_address().' --mail-type=END ';
		}
	}

	#-t "hours:minutes:seconds", "days-hours"

#sbatch -J psn:pheno.mod -o nmfe.output -e nmfe.output -p core -n 1 -t 0:3:0 -A p2011021 /bubo/sw/apps/nonmem/nm_7.1.0_g_reg/run/nmfe7 pheno.mod pheno.lst -background

	if (defined $self->slurm_prepend_flags()){
		$flags = ' '.$self->slurm_prepend_flags().$flags;
	}
	my $submitstring = $flags .' '. 
		$self->full_path_nmfe(). ' '.
		" psn.mod psn.lst $background ".$parastring." ".$switches;

	unless ($Config{osname} eq 'MSWin32' or $Config{osname} eq 'MSWin64'){
		system('echo sbatch '.$submitstring.' "2>&1" > sbatchcommand');
	}
	sleep(1); #wait to let other nodes sync files here?

	my $outp = `sbatch $submitstring 2>&1`;
	#write error to job_submission_error instead, set jobid to -1, and continue?
	($outp =~ /Submitted batch job (\d+)/)
		or croak("Slurm submit failed.\nSystem error message: $outp" );
	$jobId = $1;
	
	unless ($Config{osname} eq 'MSWin32' or $Config{osname} eq 'MSWin64'){
		system('echo sbatch '.$jobId.' "2>&1" > jobId');
	}
	return $jobId;
}

sub nmfe_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 nm_version => { isa => 'Str', optional => 1 }
	);
	my $nm_version = $parm{'nm_version'};

  #in $nm_version
  # set $self->full_path_nmfe();
  # set $self->full_path_nmtran();
  #anropa i submit ifall full_path inte redan definierat

  PsN::set_nonmem_info($nm_version);
  my $nmdir = $PsN::nmdir;
  unless( defined $nmdir ){
    my $mess = "Unknown NONMEM version $nm_version specified.\n";
#    $this->store_message('message' =>$mess);
    croak($mess);
  }
  my $minor = $PsN::nm_minor_version;
  my $major = $PsN::nm_major_version;
  
  unless (defined $major){
    croak("No nonmem major version, error config.\n");
  }

  my $nmtr = "$nmdir/tr/nmtran.exe"; 
  if( -x $nmtr ){
	  $self->full_path_nmtran($nmtr);
  }
  my $found_nonmem=0;

  my $windows = 0;
  $windows=1 if ($Config{osname} eq 'MSWin32');
  my $suffix='';
  $suffix='.bat' if ($windows);
  my @check_paths=('/run/','/util/','/');
  if ($major == 7){
    if (not defined $minor){
      #Try to figure out the subversion
	my $found = 0;
	foreach my $subv (('1','2','3','4','5','6','7','8','9')){
	    last if $found;
	    foreach my $path (@check_paths){
		if( -x "$nmdir$path"."nmfe7$subv$suffix" ){
		    $minor=$subv;
		    $found_nonmem=1;
		    $self->full_path_nmfe("$nmdir$path"."nmfe7$subv$suffix");
		    $found = 1;
		    last;
		} 
	    }
	}
    }
    if (defined $minor){
      #PsN.pm makes sure this is only one character, no dots
      #only want subversion number if subversion >1
      $minor='' unless ($minor>1 );
    }else{
      $minor='';
    }
  }
  
  unless ($found_nonmem){
    foreach my $path (@check_paths){
      if( -x "$nmdir$path"."nmfe$major$minor$suffix" ){
	$self->full_path_nmfe("$nmdir$path"."nmfe$major$minor$suffix");
	$found_nonmem=1;
	last;
      } 
    }
  }

  #check if $nmdir is in fact name of executable script, then take that as nmfe (wrapper)
  unless ($found_nonmem){
    if( (-x "$nmdir") and (not -d "$nmdir") ){
      $self->full_path_nmfe("$nmdir");
      $found_nonmem=1;
    
      croak("NONMEM major version (5,6 or 7) is not defined in psn.conf ".
		  "for $nm_version") unless (defined $major);
      if ($major == 7){
	if (defined $minor){
	  #PsN.pm makes sure this is only one character, no dots
	  #only want subversion number if subversion >1
	  $minor='' unless ($minor>1 );
	}else{
	  $minor='';
	}
      }else{
	$minor='';
      }
    }
  }

  unless ($found_nonmem){
    my $looked_in= join ' or ',@check_paths;
    my $err_version = ( defined $nmdir and $nmdir ne '' ) ? $nmdir : '[not configured]';
    my $mess = "Unable to find executable nmfe$major$minor$suffix ".
	"in any of the subdirectories\n".
	"$looked_in of the NONMEM installation directory.\n".
	"The NONMEM installation directory is $err_version for version [".
	$nm_version."] according to psn.conf.";
    croak($mess);
  }
}

sub slurm_monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  jobId => { isa => 'Int', optional => 1 }
		);
	my $jobId = $parm{'jobId'};

	#squeue -j 12345, --jobs

	#list only completed, cancelled... job with right id without header
	my $outp = `squeue -h --states CA,CD,F,NF,TO -j $jobId 2>&1`;
	if (defined $outp){
		if ($outp =~ /(i|I)nvalid/){
			#this is either because the job finished so long ago (MinJobAge)
			#that is has disappeared, 
			#or due to some Slurm error. We sleep for 3 sec to make sure there was not an error
			#due to too early polling, and then try again. If message persists then assume job
			#id will never be valid, i.e. finished. That definitely can happen.
			sleep(3);
			my $outp2 = `squeue -h --states CA,CD,F,NF,TO -j $jobId 2>&1`;
			if (defined $outp2){
				if ($outp2 =~ /(i|I)nvalid/){
					return $jobId; # Give up. This job is finished since not in queue
				}elsif ($outp2 =~ /^\s*$jobId\s/){
					#assume jobId first item in string, possibly with leading whitespace
					#job is finished
					return $jobId;
				}else{
					return 0; # Assume some error message, not finished
				}
			}else{
				return 0; # not finished since empty output when asking for finished jobs
			}
		}elsif ($outp =~ /^\s*$jobId\s/){
			#assume jobId first item in string, possibly with leading whitespace
			#job in set of finished ones
			return $jobId;
		}else{
			return 0; #Assume some error message, not finished
		}
	}else{
		return 0; # Not finished since empty output
	}
}

sub zink_submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'model', optional => 1 },
		 nm_version => { isa => 'Str', optional => 1 },
		 queue_info => { isa => 'Ref', optional => 1 }
	);
	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $queue_info = $parm{'queue_info'};

  require File::Temp;# qw/tempfile tempdir/;
  require Sys::Hostname;
  require LockFile::Simple;# qw/lock unlock trylock/;  # Non-standard module
  
  ###################################################################################################
  ###### JobSpecification code and variables                                           ##############
  ###################################################################################################

  ## Specifies the top level directory of the Zink directory structure. Should be specified via psn.conf
  my $ZinkDir = $PsN::config -> {'_'} -> {'zink_dir'};

  ## Specify the queing directory/Job Specification drop zone. I suggest this is hardcoded and not specifiable in psn.conf.
  my $ZinkJobDir =$ZinkDir."/ZinkJobs";
  
  ## $JobName: Name of job. Default could be model file name.
  ## $JobPriority: Priority of job. Between 0-5 (0=low). Default should be 3.
  ## $ExePath: Directory in which the run is to be executed.
  ## $Command: String with command to be executed, e.g. "nmfe6 run1.mod run1.lst"
  
  my $host = Sys::Hostname::hostname();
  my $fsubs = join( ',' , @{$model -> subroutine_files} );

  my $execution = 1 + $self->nmfe + 2 * ($self->nmqual); 
  # if nmfe is set, then sum will be 2, which means nmfe in nonmem.pm. If -nmqual is set sum will be 3 -> nmqual

  my $command = ($PsN::config -> {'_'} -> {'remote_perl'} ? ' ' . $PsN::config -> {'_'} -> {'remote_perl'} : ' perl ') . " -I" .
                 $PsN::lib_dir ."/../ " . 
		 $PsN::lib_dir . "/nonmem.pm" . 
		 " psn.mod psn.lst " . 
		 $self->nice . " ". 
		 $nm_version . " " .
		 1 . " " . # compilation
		 $execution . " " . # execution
		 $self->display_iterations() . ' ' .
		 $self->nonmem_options() . ' ' .
		 $self->parafile() . ' ' .
		 $self->nodes() . ' ' .
		 $fsubs;

  my $path = getcwd();
  my $jobname = $queue_info -> {'model'} -> filename;
  (my $FH,$jobId) = File::Temp::tempfile("$host-XXXXXXXXXXXXXXXXX",DIR => $ZinkJobDir,SUFFIX=>'.znk');
  LockFile::Simple::lock $jobId;
  print $FH "SUBMITHOST: $host\n";
  print $FH "JOBNAME:  $jobname\n";
  print $FH "PRIORITY: 3\n";
  print $FH "EXEPATH: $path\n";
  print $FH "COMMAND: $command\n";
  close $FH;
  LockFile::Simple::unlock $jobId;
  $jobId = OSspecific::nopath($jobId);

	return $jobId;
}

sub zink_monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 jobId => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};

  ## Specifies the top level directory of the Zink directory structure. Should be specified via psn.conf
  my $ZinkDir = $PsN::config -> {'_'} -> {'zink_dir'};
  
  ## Specify the queing directory/Job Specification drop zone. I suggest this is hardcoded and not specifiable in psn.conf.
  my $ZinkDoneDir =$ZinkDir."/ZinkDone";

  if( -e "$ZinkDoneDir/$jobId" ){
    return $jobId;
  } else {
    return 0;
  }
}

sub lsf_monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		jobId => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};

	my $string = "bjobs $jobId 2>&1";
	my $stdout = `$string`;

	# /m flag in regex:
	#Treat string as multiple lines. That is, change "^" and "$" from matching 
	#the start or end of the string to matching the start or end of any line 
	#anywhere within the string. Skip /m when looking for DONE, unless using also
	#jobID?

	if ($stdout=~/DONE/m) {
		return $jobId; # Return the jobId found.
	}elsif (($stdout=~/is not found/) or
		($stdout=~/illegal option/) or
		($stdout=~/Illegal job ID/) or
		($stdout=~/No unfinished job found/)){
		ui -> print( category => 'all', message  => $stdout,newline => 1 );
		ui -> print( category => 'all', message  => "lsf run error, jobID $jobId not".
			"recognized by system:",newline => 1 );
		return $jobId; # Return the jobId so that do not get infinite loop, 
		#let restart_needed detect and handle error (no psn.lst, no NMtran etc)
	}else{
		return 0;
	}
}

sub torque_submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'model', optional => 1 },
		 nm_version => { isa => 'Str', optional => 1 },
		 queue_info => { isa => 'Ref', optional => 1 }
	);
	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $queue_info = $parm{'queue_info'};

  my $fsubs = join( ',' , @{$model -> subroutine_files} );

  my $execution = 1 + $self->nmfe + 2 * ($self->nmqual); 
  # if nmfe is set, then sum will be 2, which means nmfe in nonmem.pm. If -nmqual is set sum will be 3 -> nmqual

  open(JOBSCRIPT, ">JobScript") or croak("Couldn't open Torque JobScript file for writing: $!" );
  print JOBSCRIPT
      "cd ".getcwd()."\n".
		($PsN::config -> {'_'} -> {'remote_perl'} ? ' ' . $PsN::config -> {'_'} -> {'remote_perl'} : ' perl ') . " -I" .
		$PsN::lib_dir ."/../ " . 
		$PsN::lib_dir . "/nonmem.pm" . 
		" psn.mod psn.lst " .
		$self->nice . " ".
		$nm_version . " " .
		1 . " " . # compilation
		$execution . " " . # execution
		$self->display_iterations() . ' ' .
		$self->nonmem_options() . ' ' .
		$self->parafile().' ' .
		$self->nodes() . ' ' .
		$fsubs;
  close(JOBSCRIPT);

  my $jobname= "psn:" . $queue_info -> {'model'} -> filename;
  $jobname =~ s/\ /_/g;
  my $prepend = '';
  if (defined $self->torque_prepend_flags()){
    $prepend = ' '.$self->torque_prepend_flags().' ';
  }

  my $queue_string = ' ';
  $queue_string = ' -q '.$PsN::config->{'_'}->{'torque_queue'}.' ' 
      if ($PsN::config -> {'_'}  -> {'torque_queue'});
  $queue_string = ' -q '.$self->torque_queue().' ' if (defined $self->torque_queue());
  if( system( 'qsub '.
	      $prepend.
	      ' -N '.$jobname .
	      $queue_string .
	      ' JobScript > JobId' ) ){
    croak("Torque submit failed.\nSystem error message: $!" );
  }
  
  open(JOBFILE, "JobId") or croak("Couldn't open torque JobId file for reading: $!" );
  while( <JOBFILE> ){
    if( /(\d+.[0-9A-Za-z\-\.]*)/ ){
      $jobId = $1;
    }
  }
  close(JOBFILE);

	return $jobId;
}

sub torque_monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		jobId => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};

	carp("Checking Torque queue for $jobId");
	my $response = `qstat $jobId 2>&1`;

	carp("Result: (OUT+ERR) $response");
	if($response =~ /Unknown Job Id/ ){ # regexp to find finished jobs.
		# The job is completed by default
		return $jobId; # Return the jobId found.
	}
	elsif ($response =~ /Job id/) { # regexp to find running jobs
		# The job is not completed
		return 0;
	}
	else {
		# something else happened, FIXME: there should probably be something different here
		return 0;
	}
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

	my $candidate_model = $queue_info -> {'candidate_model'};
	my $tries = $queue_info -> {'tries'};
	my $model = $queue_info -> {'model'};

	#an ls might make files sync and become visible
	my $dirt = `ls -la 2>&1` unless ($Config{osname} eq 'MSWin32');

	if ($self->rerun >= 2 ) {
		unlink ('psn.lst') if (-e 'psn.lst');
		unlink ('psn-prevrun.lst') if (-e 'psn-prevrun.lst');
	}

	if (-e $self->nmtran_error_file){
		#give fake pid and go directly to restart needed. Do not copy or move anything
		$queue_map->{'rerun_'.$run_no} = $run_no; #Fake pid
		
		$self->stop_motion_call(tool=>'modelfit',
								message => "nmtran error file exists, syntax check failed. Give fake pid of ".
								'rerun_'.$run_no.' and do not run anything.')
			if ($self->stop_motion());
		return;
		
		
	}elsif (-e 'stats-runs.csv') {
		#possible reasons: 
		#a) Restart after -clean > 1. Then we do not know the true restart number of psn.lst
		#b) Restart after completed run -clean <= 1. Best retry is copied to psn.lst, don't know which.

		#give fake pid and go directly to restart needed. Do not copy or move anything
		$queue_map->{'rerun_'.$run_no} = $run_no; #Fake pid

		$self->stop_motion_call(tool=>'modelfit',
								message => "stats-runs.csv exists, this is a rerun. Give fake pid of ".
								'rerun_'.$run_no.' and do not run anything.')
			if ($self->stop_motion());
		return;
	
	}elsif (-e 'psn.lst'){
		#possible reason 
		#Restart after main PsN process killed before sge/slurm NONMEM run finished. 
		
		# If rerun >= 2 then removed it above.
		# Then check for ($tries+1).lst. If it exists, then *move* 
		#psn.mod, psn.lst etc to psn-prevrun.lst etc
		# to save it for later and continue to next if -clause.
		# If $tries+1 does not exist then do not run anything, want to go directly to restart needed.
		# Create a fake pid and return, do not copy or move files.
		
		if( -e 'psn-' . ( $tries + 1 ) . '.lst'){
			foreach my $filename ( @{$candidate_model -> output_files},'psn.mod','compilation_output.txt',
								   $self->base_msfo_name){
				next unless (defined $filename);
				my $new_name = $filename;
				unless( $new_name =~ s/\.([^.]+)$/-prevrun.$1/ ){
					$new_name .= "-prevrun";
				}
				mv($filename,$new_name);
			}
		}else{
			$queue_map->{'rerun_'.$run_no} = $run_no; #Fake pid
			$self->stop_motion_call(tool=>'modelfit',
									message => "psn.lst exists and not retry. Give fake pid of ".'rerun_'.$run_no.
									' to make it look like psn.mod is run, '.
									'so that output will be checked the usual way.')
				if ($self->stop_motion());
			return;
		}
	}elsif(-e 'psn-prevrun.lst'){
		#did not have psn.lst but do have prevrun.lst
		# If rerun >= 2 then removed it above.
		# Then check for ($tries+1).lst. If it exists, do nothing.
		# If it does not exist then make it look like this was just run: Move all files to psn.lst etc
		# Then let restart_needed move files to numbered retry files.
		# Create a fake pid and return, do not copy or move any more files.
		unless( -e 'psn-' . ( $tries + 1 ) . '.lst'){
			foreach my $filename ( @{$candidate_model -> output_files},'psn.mod','compilation_output.txt',
								   $self->base_msfo_name){
				next unless (defined $filename);
				my $new_name = $filename;
				unless( $new_name =~ s/\.([^.]+)$/-prevrun.$1/ ){
					$new_name .= "-prevrun";
				}
				mv($new_name,$filename);
			}
			$queue_map->{'rerun_'.$run_no} = $run_no; #Fake pid
			$self->stop_motion_call(tool=>'modelfit',
									message => "psn-prevrun.lst exists and not retry or psn.lst. ".
									"Move to psn.lst and give fake pid of ".'rerun_'.$run_no.
									' to make it look like psn.mod is run, '.
									'so that output will be checked the usual way.')
				if ($self->stop_motion());
			return;
		}
	}
	
	# We do not expect any values of rerun lower than 1 here. (a bug otherwise...)
	if ( not -e 'psn-' . ( $tries + 1 ) . '.lst' or $self->rerun >= 2 ) {
		#missing psn-1.lst etc or force rerun
		
		# {{{ Execution step 
		
		if ( $self->run_local ) {
			
			# Normal local execution
			
			my $fsubs = join( ',' , @{$candidate_model -> subroutine_files} );
			
			my $execution = 1 + $self->nmfe + 2 * ($self->nmqual); 
			# if nmfe is set, then sum will be 2, which means nmfe in nonmem.pm. If -nmqual is set sum will be 3
			my $command_line_options = " -I" .
				$PsN::lib_dir ."/../ " . 
				$PsN::lib_dir . "/nonmem.pm" . 
				" psn.mod psn.lst " . 
				$self->nice . " ". 
				$nm_version . " " .
				1 . " " . # compilation
				$execution . " " . # execution
				$self->display_iterations() . ' ' .
				$self->nonmem_options() . ' ' .
				$self->parafile() . ' '.
				$self->nodes() . ' ' .
				$fsubs;
			$self->stop_motion_call(tool=>'modelfit',message => "About to start NONMEM run with command\n".
									"$command_line_options")
				if ($self->stop_motion()> 1);
			
			if( $Config{osname} eq 'MSWin32' ){

				# {{{ Windows execution

				my $perl_bin = ($PsN::config -> {'_'} -> {'perl'} ? $PsN::config -> {'_'} -> {'perl'} : 'C:\Perl\bin\perl.exe');

				unless (-e $perl_bin){
					if (defined $PsN::config -> {'_'} -> {'perl'}){
						ui->print(category=> 'all', message=>  
								  "\nWarning: Perl binary ".$perl_bin." set in psn.conf cannot be found. ".
								  "Check setting of\n".
								  "perl= ...\n".
								  "in psn.conf, see psn_configuration.pdf.",
								  newline => 1);
					}else{
						ui -> print( category => 'all', message  => 
									 "\nWarning: Perl binary ".$perl_bin." (the default) cannot be found. ".
									 "You need to set\n".
									 "perl= ...\n".
									 "in psn.conf, see psn_configuration.pdf.",
									 newline => 1);

					}
				}

				require Win32::Process;
				require Win32;
				sub ErrorReport{ print Win32::FormatMessage( Win32::GetLastError() ); }
				my $proc;
				Win32::Process::Create($proc,$perl_bin,$perl_bin . $command_line_options,0,$Win32::Process::NORMAL_PRIORITY_CLASS,'.') || die ErrorReport();
				
				$queue_info->{'winproc'}=$proc;
				$queue_map->{$proc->GetProcessID()} = $run_no;

				# }}}
				
			} else { #Asume *nix
				
				# {{{ Unix execution

				my $perl_bin = ($PsN::config -> {'_'} -> {'perl'} ? $PsN::config -> {'_'} -> {'perl'} : ' perl ');

				my $pid = fork();
				if( $pid == 0 ){
					my $execstring = $perl_bin.$command_line_options;
					$execstring = 'mosenv -e '.$execstring if ($self->run_on_mosix());
					exec($execstring);
					exit; # Die Here if exec failed. Probably happens very rarely.
				}
				$queue_map->{$pid} = $run_no;

				# }}}
				
			}

			$queue_info->{'start_time'} = time;

		} elsif ( $self->run_on_lsf ) {
			
			# lsf_submit will call the "nonmem" module that will figure
			# out that we want to run remotely. If we are also compiling
			# remotely, it will be done from here as well.
			
			my $jobId = $self -> lsf_submit( model => $candidate_model,
											 nm_version => $nm_version,
											 work_dir   => $self->directory . "/NM_run$run_no/");
			
			if ($jobId == -1){
				$queue_map->{'fail_'.$run_no} = $run_no;
			}else{
				$queue_map->{$jobId} = $run_no;
			}
			
		} elsif ( $self->run_on_lsf_nmfe ) {

			my $jobId = $self -> lsf_nmfe_submit( model => $candidate_model,
												  queue_info => $queue_info,
												  nm_version => $nm_version,
												  nodes => $self->nodes());
			if ($jobId == -1){
				$queue_map->{'fail_'.$run_no} = $run_no;
			}else{
				$queue_map->{$jobId} = $run_no;
			}
			
		} elsif ( $self -> run_on_ud() ) {
			carp("Submitting to the UD system" );  
			my $jobId = $self -> ud_submit( model => $candidate_model );
			
			if ($jobId == -1){
				$queue_map->{'fail_'.$run_no} = $run_no;
			}else{
				$queue_map->{$jobId} = $run_no;
			}
			
		} elsif ( $self -> run_on_sge() ) {
			my $jobId = $self -> sge_submit( model => $candidate_model,
											 queue_info => $queue_info,
											 nm_version => $nm_version );
			
			if ($jobId == -1){
				$queue_map->{'fail_'.$run_no} = $run_no;
			}else{
				$queue_map->{$jobId} = $run_no;
			}
		} elsif ( $self -> run_on_sge_nmfe() ) {
			my $jobId = $self -> sge_nmfe_submit( model => $candidate_model,
												  queue_info => $queue_info,
												  nm_version => $nm_version,
												  nodes => $self->nodes());
			
			if ($jobId == -1){
				$queue_map->{'fail_'.$run_no} = $run_no;
			}else{
				$queue_map->{$jobId} = $run_no;
			}
		} elsif ( $self -> run_on_slurm() ) {
			my $jobId = $self -> slurm_submit( model => $candidate_model,
											   queue_info => $queue_info,
											   nm_version => $nm_version,
											   nodes => $self->nodes());
			if ($jobId == -1){
				$queue_map->{'fail_'.$run_no} = $run_no;
			}else{
				$queue_map->{$jobId} = $run_no;
			}
		} elsif ( $self -> run_on_torque() ) {
			my $jobId = $self -> torque_submit( model => $candidate_model,
												queue_info => $queue_info,
												nm_version => $nm_version );
			
			if ($jobId == -1){
				$queue_map->{'fail_'.$run_no} = $run_no;
			}else{
				$queue_map->{$jobId} = $run_no;
			}
		} elsif ( $self -> run_on_zink() ) {
			my $jobId = $self -> zink_submit( model => $candidate_model,
											  queue_info => $queue_info,
											  nm_version => $nm_version );
			
			if ($jobId == -1){
				$queue_map->{'fail_'.$run_no} = $run_no;
			}else{
				$queue_map->{$jobId} = $run_no;
			}
#	  print $jobId."\n";
		}

		# }}}
		
	} elsif ( $self->rerun >= 1 ) {
		#psn-(tries+1).lst exists, and rerun < 2
		
		# We are not forcing a rerun, but we want to recheck the
		# output files for errors. Therefore we put a fake entry in
		# queue_map to trigger "restart_needed()". 

		#Make it look like the retry has just been run, and not yet 
		#moved to numbered retry files in restart_needed

		foreach my $filename ( @{$candidate_model -> output_files},'psn.mod','compilation_output.txt',
							   $self->base_msfo_name) {
			next unless (defined $filename);

			my $retry_name = $self -> get_retry_name( filename => $filename,
													  retry => $tries );
			mv( $retry_name,$filename );
		}

		my $fname = $self -> get_retry_name( filename => 'psn.lst',
											 retry => $tries );
		$queue_map->{'rerun_'.$run_no} = $run_no; #Fake pid
		$self->stop_motion_call(tool=>'modelfit',
								message => "Moved $fname to psn.lst in run_nonmem and ".
								"give fake pid of ".'rerun_'.$run_no. 'to make it look like psn.mod is run '.
								'so that output will be checked the usual way.')
			if ($self->stop_motion());
	} # end of "not -e psn-$tries.lst or rerun"
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
	my $picky = $parm{'picky'};
	my @cutoff_thetas = defined $parm{'cutoff_thetas'} ? @{$parm{'cutoff_thetas'}} : ();
	my $significant_digits_accept = $parm{'significant_digits_accept'};
	my $run_no = $parm{'run_no'};
	my $nm_version = $parm{'nm_version'};
	my %queue_info = defined $parm{'queue_info'} ? %{$parm{'queue_info'}} : ();

	# -------------- Notes about automatic pertubation and retries -----------------
  
  # Automatic pertubation of initial estimates are useful for two
  # purposes. One reason is when nonmem failes to produce a successful
  # minimization. In this case, we can try to direct the search by
  # selecting other estimates. It is also possible to get a successful
  # minimization in a local minima. In this case, we have no way of
  # knowing that it is a local minima without trying other initial
  # estimates. Two modelfit members govern the pertubation process;
  # "retries" which is a number giving the maximum number of retries
  # when nonmem fails and "min_retries" which is the number of extra runs
  # we want to do to get a global minima. If min_retries is 2 and
  # retries is 5 we will stop after 3 runs if we have reached a
  # successful minimization but continue until 6 if necessary.
  
  # It is important to set $marked_for_rerun to 1 if $tries is
  # incremented!  Otherwise $tries can be incremented twice for
  # one run. The opposite is not true however, for instance a reset
  # of maxevals is not a retry but sets $marked_for_rerun to 1.
  
  # We need the trail of files to select the most appropriate at the end
  # (see copy_model_and_output)


	sub general_error
	{
		my $message = shift;
		open( FILE, '>>' . $self->general_error_file );
		print FILE "\n" . $message . "\n";
		close(FILE);
	}

  
	unless( defined $parm{'queue_info'} ){
		# The queue_info must be defined here!
		croak("Internal run queue corrupt\n" );
	}
	my $queue_info_ref = $parm{'queue_info'};
	my $run_results = $queue_info_ref -> {'run_results'};
	my $tries = \$queue_info_ref -> {'tries'};
	my $model = $queue_info_ref -> {'model'};
	my $candidate_model = $queue_info_ref -> {'candidate_model'};
	my $modelfile_tainted = \$queue_info_ref -> {'modelfile_tainted'};
	my $nmqual = 'nmqual_messages.txt';
	#if missing psn.lst is due to NMtran or compilation failure we should see a file 
	#$self->general_error_file or $self->nmtran_error_file in the directory. Then do not wait, continue directly with
	#storing failure messages. On the other hand, if we do not see psn.lst due to
	#a file sync delay, there will be no $self->general_error_file. Wait for a long time
	#to see if $self->general_error_file appears. Then check quickly for psn.lst again and then
	#exit with failure message

	#neither psn_nonmem_error_messages.txt nor psn.lst will appear if run killed or
	#it is a perl settings problem. No hurry in those cases. Can always wait for a long
	#time if neither file has appeared.

	#according to NFS documentaion max cache (delay) time is 60 sec.
	my $dirt;
	for (my $i=0; $i<20; $i++){
		#an ls might make files sync and become visible
		$dirt = `ls -la 2>&1` unless ($Config{osname} eq 'MSWin32');
		last if((-e 'stats-runs.csv') or (-e 'psn.lst') or (-e 'job_submission_error')
				or (-e $self->general_error_file)
				or (-e $self->nmtran_error_file));#psn.lst exists or will never appear
		sleep(6);
	}

	my $failure;
	my $failure_mess;
	my $lstsuccess = 0;
	my $eta_shrinkage_name;
	my $iwres_shrinkage_name;
	if( -e 'stats-runs.csv' ){
		#this is a rerun, we should not do anything here, no copying or anything except
		#reading raw results to memory
		#candidate model should have psn.lst as output file

		my ($raw_results_row, $nonp_row);
		if ($candidate_model->nthetas(problem_number=>1)==0 ){

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

		$self->stop_motion_call(tool=>'modelfit',
								message => "Found stats-runs.csv, will read output psn.lst if it exists to put in raw_results.")
			if ($self->stop_motion()> 1);

		if (-e 'psn.lst'){
			my ( $output_file );
			$output_file = $candidate_model -> outputs -> [0];
			#is this needed? 
			$output_file -> _read_problems;    
			$self->stop_motion_call(tool=>'modelfit',message => "parsed NONMEM output file ".$output_file->filename())
				if ($self->stop_motion()> 1);
			
			$queue_info_ref -> {'raw_results'} -> [${$tries}] = $raw_results_row;
			$queue_info_ref -> {'raw_nonp_results'} -> [${$tries}] = $nonp_row;
			foreach my $category ( 'minimization_successful', 'covariance_step_successful',
								   'covariance_step_warnings', 'estimate_near_boundary',
								   'significant_digits', 'ofv' ){
				my $res = $output_file -> $category;
				$run_results -> [${$tries}] -> {$category} = defined $res ? $res -> [0][0] : undef;
			}

			$run_results -> [${$tries}] -> {'pass_picky'} = 0;

			if( $output_file->parsed_successfully() and not defined $output_file->problems ) {
				# This should not happen if we are able to parse the output file correctly
				$run_results -> [${$tries}] -> {'failed'} = 'lst-file file exists but could not be parsed correctly';
				general_error('lst-file file exists but could not be parsed correctly');
				return(0);
			}

			my $minimization_successful = $output_file -> minimization_successful();
			my $minimization_message    = $output_file -> minimization_message();
			my @problems = @{$candidate_model -> problems};
			for ( my $problem = 1; $problem <= scalar @problems; $problem++ ) {
				if ( $candidate_model -> is_estimation( problem_number => $problem ) ){
					if ( $minimization_successful -> [$problem-1][0] ) {
						if ( $picky ) {
							$run_results -> [${$tries}] -> {'pass_picky'} = 1;
							for ( @{$minimization_message -> [$problem-1][0]} ) {
								if ( /0COVARIANCE STEP ABORTED/ or
									 /0PROGRAM TERMINATED BY OBJ/ or
									 /0ESTIMATE OF THETA IS NEAR THE BOUNDARY AND/ or
									 /0PARAMETER ESTIMATE IS NEAR ITS BOUNDARY/ or
									 /0R MATRIX ALGORITHMICALLY SINGULAR/ or
									 /0S MATRIX ALGORITHMICALLY SINGULAR/ ) {
									$run_results -> [${$tries}] -> {'pass_picky'} = 0;
									last;
								}
							}
						}
					}
				}
			}

			$output_file -> flush;

		} else {
			$self->stop_motion_call(tool=>'modelfit',message => "psn.lst does not exist. Previous run must have failed")	    if ($self->stop_motion()> 1);
			#we do not have any psn.lst. Cannot happen if nmfe 
			#and overtime kill by system, at least model and NMtran mess
			if (not (-e 'FDATA')){
				$failure = 'File system problem or NMtran could not be initiated (the NMtran output file FDATA is missing)';
				if ($self->run_local) {
					$failure .= ' - check perl settings in psn.conf and perl installation';
				} elsif ($self->run_on_sge_nmfe) {
					$failure .= ' - check cluster status and cluster settings in psn.conf';
				} elsif ($self->run_on_lsf_nmfe) {
					
					if (-e 'job_submission_error'){
						open( MESS, '<job_submission_error' );
						$failure = '';
						while(<MESS>) {
							chomp;
							$failure .= $_.' ';
						}
						close( MESS );
						
					}else{
						$failure .= ' - check cluster status and cluster settings in psn.conf';
					}
				}else{
					$failure .= ' - check cluster status and cluster settings and remote_perl in psn.conf';
				}
			}elsif (not(-e 'FREPORT')){
				$failure = 'NMtran failed';
			}elsif (not(-e 'nonmem.exe' or -e 'nonmem' or -e 'nonmem_mpi.exe' or -e 'NONMEM_MPI.exe' or -e 'nonmem_mpi' or -e 'nonmem5' or -e 'nonmem6' or -e 'nonmem7' )){
				$failure = 'Compilation failed';
			}elsif (-e 'OUTPUT' or -e 'output'){
				$failure = 'NONMEM run interrupted';
			}else{
				$failure = 'the lst-file does not exist';
			}

			$run_results -> [${$tries}] -> {'failed'} = $failure; #different texts for different causes
		}
		return(0); #no restart needed when stats-runs exist
	}
	if( -e 'psn.lst' ){

		my ( $output_file );
		if ( not -e 'psn-' . ( ${$tries} + 1 ) . '.lst' or $self->rerun >= 2 ) {

			$output_file = $candidate_model -> outputs -> [0];
			$output_file -> abort_on_fail($self->abort_on_fail);
			$output_file -> _read_problems;    
			$self->stop_motion_call(tool=>'modelfit',message => "parsed NONMEM output file ".$output_file->filename())
				if ($self->stop_motion()> 1);

			my $stopmess ='moved ';
			my $extramess;
			foreach my $filename ( @{$candidate_model -> output_files},'psn.mod','compilation_output.txt',
								   $self->base_msfo_name) {

				next unless (defined $filename);
				my $new_name = $self -> get_retry_name( filename => $filename,
														retry => ${$tries} );
				if ($filename =~ /psn\_etas/){
					$eta_shrinkage_name = $new_name;
				}
				if ($filename =~ /psn\_wres/){
					$iwres_shrinkage_name = $new_name;
				}
				mv( $filename, $new_name );
				$stopmess .= "$filename to $new_name, ";
				
			}
			$self->stop_motion_call(tool=>'modelfit',message => $stopmess. "so that new retry with psn.mod will not overwrite this runs results.")
				if ($self->stop_motion()> 1);
			if (-e $nmqual){
				my $new_name = $self -> get_retry_name( filename => $nmqual, retry => ${$tries} );
				mv( $nmqual, $new_name );
			}
			
		} else {
			#should never enter here.
			ui -> print( category => 'all', message  => "\n\nERROR IN RESTART NEEDED\n", newline => 1);
		}

		# {{{ Create intermediate raw results

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

		# write intermediate raw results, append to existing file
		open( INTERMED, '>>'.'intermediate_raw_results.csv' );
		foreach my $row ( @{$raw_results_row} ){
			next unless (defined $row);
			print INTERMED 'try '.(${$tries}+1).',';
			print INTERMED join( ',', @{$row} ), "\n";
		}
		close( INTERMED );
		if (defined $nonp_row and scalar(@{$nonp_row})>0){
			open( INTERMEDNONP, '>>intermediate_nonp_results.csv' );
			foreach my $row ( @{$nonp_row} ){
				next unless (defined $row);
				print INTERMED 'try '.(${$tries}+1).',';
				print INTERMEDNONP join( ',', @{$row} ), "\n";
			}
			close( INTERMEDNONP );
		}



		# }}}

		# {{{ Check for minimization successfull and try to find out if lst file is truncated

		my ( $minimization_successful, $minimization_message );


		if(( $output_file -> parsed_successfully() and
			 not defined $output_file -> problems ) or
		   (not $output_file -> parsed_successfully()) ){
			#here we do have a lst-file, but perhaps is completely empty
			#check for signs of NMtran error, compilation error. Do not handle that as crash

			if (not (-e 'FDATA')) {
				$failure = 'File system problem or NMtran could not be initiated (the NMtran output file FDATA is missing)';
				$failure_mess="\nFile system problem or NMtran could not be initiated (the NMtran output file FDATA is missing). There is no output for model ".($run_no+1);
				if ($self->run_local) {
					$failure_mess .= " It is recommended to check the perl installation, and perl settings in psn.conf." ;
					$failure .= ' - check perl settings in psn.conf and perl installation';
				} elsif ($self->run_on_sge_nmfe) {
					$failure_mess .= " It is recommended to check cluster status, and cluster settings in psn.conf." ;
					$failure .= ' - check cluster status and cluster settings in psn.conf';
				} elsif ($self->run_on_lsf_nmfe) {

					if (-e 'job_submission_error') {
						open( MESS, '<job_submission_error' );
						$failure = '';
						$failure_mess = "Job submission error:\n";
						while(<MESS>) {
							chomp;
							$failure .= $_ . ' ';
							$failure_mess .= $_."\n";
						}
						close( MESS );
					}else{
						$failure .= ' - check cluster status and cluster settings in psn.conf';
					}
				}else{
					$failure_mess .= " It is recommended to check cluster status, and cluster settings and remote_perl in psn.conf." ;
					$failure .= ' - check cluster status, and cluster settings and remote_perl in psn.conf';
				}
				ui -> print( category => 'all', message  => $failure_mess,newline => 1 );
				$run_results -> [${$tries}] -> {'failed'} = $failure;
				$output_file -> flush;
				return(0);
			}elsif (not(-e 'FREPORT')){
				$failure = 'NMtran failed';
				$failure_mess="NMtran failed. There is no output for model ".($run_no+1) ;
				general_error($failure_mess);
				ui -> print( category => 'all', message  => $failure_mess,newline => 1 );
				$run_results -> [${$tries}] -> {'failed'} = $failure;
				$output_file -> flush;
				return(0);
			}elsif (not(-e 'nonmem.exe' or -e 'nonmem' or -e 'nonmem_mpi.exe' or -e 'NONMEM_MPI.exe' or -e 'nonmem_mpi' or -e 'nonmem5' or -e 'nonmem6' or -e 'nonmem7' )){
				$failure = 'Compilation failed';
				$failure_mess="Compilation failed. There is no output for model ".($run_no+1) ;
				ui -> print( category => 'all', message  => $failure_mess,newline => 1 );
				$run_results -> [${$tries}] -> {'failed'} = $failure;
				$output_file -> flush;
				return(0);
			}elsif (not $output_file -> lst_interrupted()){
				$failure = 'NONMEM run failed';
				$failure_mess="NONMEM run failed. Check the lst-file in NM_run".($run_no+1).
					" for errors" ;
				general_error($failure_mess);
				ui -> print( category => 'all', message  => $failure_mess,newline => 1 );
				$run_results -> [${$tries}] -> {'failed'} = $failure;
				$output_file -> flush;
				return(0);
			} elsif( $self->handle_crashes and 
					 $queue_info_ref -> {'crashes'} < $self -> crash_restarts() ) {

				# If the lst file is interrupted (no end time printed by nmfe), this is
				# a sign of a crashed run. This is not a NONMEM error as such
				# but probably an operating system problem. To handle this, we
				# mark this for rerunning but do not increase the $tries
				# variable but instead increase $crashes and check whether
				# this value is below or equal to $crash_restarts.
				carp("Restarting crashed run ".
					 $output_file -> full_name().
					 "\n".$output_file -> parsing_error_message() );
				
				$queue_info_ref -> {'crashes'}++;

				#now we could be restarting after the main PsN process has been killed, and
				#the crashes counter has been reset. Therefore check existence of files from 
				#previous crashes

				my $crash_no = $queue_info_ref -> {'crashes'};
				while (-e $self -> get_retry_name( filename => 'psn.mod',
												   retry => ${$tries},
												   crash => $crash_no)){
					$crash_no++;
				}	  
				$queue_info_ref -> {'crashes'} = $crash_no;

				my $message = "\nModel in NM_run".($run_no+1)." crashed, restart attempt nr ". ($queue_info_ref -> {'crashes'} );
				ui -> print( category => 'all',  message  => $message,
							 newline => 1);

				foreach my $filename ( @{$candidate_model -> output_files},'psn.mod','compilation_output.txt',
									   $self->base_msfo_name) {
					next unless (defined $filename);
					my $old_name = $self -> get_retry_name( filename => $filename,
															retry => ${$tries} );
					my $new_name = $self -> get_retry_name( filename => $filename,
															retry => ${$tries},
															crash => $queue_info_ref -> {'crashes'});
					mv( $old_name, $new_name );
				}

				if( $self->handle_msfo ){
					$self -> set_msfo_to_msfi( candidate_model => $candidate_model,
											   retry => ${$tries},
											   queue_info => $queue_info_ref);
				} else {
					my $new_name = $self -> get_retry_name( filename => 'psn.mod',
															retry => ${$tries},
															crash => $queue_info_ref -> {'crashes'});
					cp( $new_name, 'psn.mod' );
				}

				$output_file -> flush;
				return(1); # Return a one (1) to make run() rerun the
				# model. By returning here, we avoid the
				# perturbation of the initial estimates later on in
				# this method.
			} else {
				my $message = "\nModel in NM_run".($run_no+1)." crashed ".(($queue_info_ref -> {'crashes'}+1)." times. Not restarting." );
				ui -> print( category => 'all',  message  => $message,
							 newline => 1);

				$output_file -> flush;
				
				return(0);
			}
		}elsif (( $maxevals > 0 ) and (not $cut_thetas_maxevals)){
			my $exceeded=0;
			for ( @{$output_file -> minimization_message() -> [0][0]} ) {
				if ( /\s*MAX. NO. OF FUNCTION EVALUATIONS EXCEEDED\s*/) {
					$exceeded=1;
					# To handle this, we
					# mark this for rerunning but do not increase the $tries
					# variable but instead increase $crashes
					if (defined $queue_info_ref -> {'evals'}){
						$queue_info_ref -> {'evals'} += $output_file -> get_single_value(attribute=> 'feval');
					}else {
						$queue_info_ref -> {'evals'} = $output_file -> get_single_value(attribute=> 'feval');
					}
					
					if( $maxevals > $queue_info_ref -> {'evals'} ){
						$queue_info_ref -> {'crashes'}++;	      
						my $stopmess ='moved ';
						foreach my $filename ( @{$candidate_model -> output_files},'psn.mod','compilation_output.txt',
											   $self->base_msfo_name) {
							next unless (defined $filename);
							my $old_name = $self -> get_retry_name( filename => $filename,
																	retry => ${$tries} );
							my $new_name = $self -> get_retry_name( filename => $filename,
																	retry => ${$tries},
																	crash => $queue_info_ref -> {'crashes'});
							mv( $old_name, $new_name );

							$stopmess .= "$old_name to $new_name, ";
						}
						$self -> set_msfo_to_msfi( candidate_model => $candidate_model,
												   retry => ${$tries},
												   queue_info => $queue_info_ref);
						#set msfo to msfi does model print
						${$modelfile_tainted} = 1;

						$self->stop_motion_call(tool=>'modelfit',
												message => "max no evaluations exceeded and option maxevals set, ".
												"must rerun this model with msfo set to msfi.\n".
												"Total evals so far is ".$queue_info_ref -> {'evals'}."\n".
												"$stopmess\n"."so that intermediate run for this try will be ".
												"distinguished from later runs for this try.")
							if ($self->stop_motion());
						
						my $old_name = $self -> get_retry_name( filename => $nmqual,
																retry => ${$tries} );
						if (-e $old_name){
							my $new_name = $self -> get_retry_name( filename => $nmqual,
																	retry => ${$tries},
																	crash => $queue_info_ref -> {'crashes'});
							mv( $old_name, $new_name );
						}

						$output_file -> flush;
						return(1); # Return a one (1) to make run() rerun the
						# model. By returning here, we avoid the
						# perturbation of the initial estimates later on in
						# this method.
					}	    
					#if we passed the total number of evals, continue through to below, parse normally
				}
			}
		}
		# If the output file was parsed successfully and not handle maxevals, we (re)set the $crashes
		# variable and continue
		$queue_info_ref -> {'crashes'} = 0;
		#reset number of evals
		$queue_info_ref -> {'evals'}=0;

		$minimization_successful = $output_file -> minimization_successful();
		$minimization_message    = $output_file -> minimization_message();

		unless( defined $minimization_successful ) {
			croak("No minimization status found in " . $output_file ->filename );
		}
		
		# {{{ log the stats of this run

		foreach my $category ( 'minimization_successful', 'covariance_step_successful',
							   'covariance_step_warnings', 'estimate_near_boundary',
							   'significant_digits', 'ofv' ){
			my $res = $output_file -> $category;
			$run_results -> [${$tries}] -> {$category} = defined $res ? $res -> [0][0] : undef;
		}
		$run_results -> [${$tries}] -> {'pass_picky'} = 0;
		# {{{ Check for failed problems and possibly check for picky errors.

		my $round_error = 0;
		my $hessian_error = 0;
		my $maxeval_error = 0;
		if ( (not $marked_for_rerun) and ( $tweak_inits || $cut_thetas_rounding_errors || $handle_hessian_npd || $cut_thetas_maxevals)) {

			$self->stop_motion_call(tool=>'modelfit',message => "check if run needs restart")
				if ($self->stop_motion() > 1);
			
			my @reruns;
			my @problems = @{$candidate_model -> problems};
			for ( my $problem = 1; $problem <= scalar @problems; $problem++ ) {
				if ( $candidate_model -> is_estimation( problem_number => $problem ) ){
					if ( $minimization_successful -> [$problem-1][0] ) {
						my $check_local_opt=1;
						if ( $picky ) {
							$run_results -> [${$tries}] -> {'pass_picky'} = 1;
							for ( @{$minimization_message -> [$problem-1][0]} ) {
								if ( /0COVARIANCE STEP ABORTED/ or
									 /0PROGRAM TERMINATED BY OBJ/ or
									 /0ESTIMATE OF THETA IS NEAR THE BOUNDARY AND/ or
									 /0PARAMETER ESTIMATE IS NEAR ITS BOUNDARY/ or
									 /0R MATRIX ALGORITHMICALLY SINGULAR/ or
									 /0S MATRIX ALGORITHMICALLY SINGULAR/ ) {
									push( @reruns, $problem );
									$run_results -> [${$tries}] -> {'pass_picky'} = 0;
									$check_local_opt=0;
									$self->stop_motion_call(tool=>'modelfit',message => "Run did not pass picky test, must restart.")
										if ($self->stop_motion()> 1);
									last;
								}
							}
							if ($check_local_opt){
								if ((${$tries} > 0) and (not $queue_info_ref -> {'have_accepted_run'})){
									for (my $tr=0; $tr < ${$tries}; $tr++){
										if (defined $run_results -> [$tr] -> {'ofv'}
											and not $run_results -> [$tr] -> {'pass_picky'}
											and ($run_results -> [$tr] -> {'ofv'}<
												 ($run_results->[${$tries}]->{'ofv'} - $self->accepted_ofv_difference))){
											push( @reruns, $problem );
											$self->stop_motion_call(tool=>'modelfit',message => "Run picky ok but had higher ".
																	"corrected ofv than previous without picky ok, local min, must restart.")
												if ($self->stop_motion()> 1);
											last;
										}
									}
								}
							}
						}else{
							#not picky
							#if successful but higher ofv than any previous and previous not successful then rerun
							if ((${$tries} > 0) and (not $queue_info_ref -> {'have_accepted_run'})){
								for (my $tr=0; $tr < ${$tries}; $tr++){
									if (defined $run_results -> [$tr] -> {'ofv'}
										and defined $run_results -> [$tr] -> {'minimization_successful'}
										and not $run_results -> [$tr] -> {'minimization_successful'}
										and ($run_results -> [$tr] -> {'ofv'}<
											 ($run_results->[${$tries}]->{'ofv'} - $self->accepted_ofv_difference))){
										push( @reruns, $problem );
										$self->stop_motion_call(tool=>'modelfit',message => "Run minim success but had higher ".
																"corrected ofv than previous without successful, local min, must restart.")
											if ($self->stop_motion()> 1);
										last;
									}
								}
							}
						}
					}else{
						#not successful
						$round_error = 1 if ($output_file -> get_single_value(attribute => 'rounding_errors',
																			  problem_index => ($problem-1)));
						if ($handle_hessian_npd){
							for ( @{$minimization_message -> [$problem-1][0]} ) {
								if ( /\s*NUMERICAL HESSIAN OF OBJ. FUNC. FOR COMPUTING CONDITIONAL ESTIMATE IS NON POSITIVE DEFINITE\s*/){
									$hessian_error = 1 ;
									last;
								}
							}
						} 
						if ($cut_thetas_maxevals){
							for ( @{$minimization_message -> [$problem-1][0]} ) {
								if ( /\s*MAX. NO. OF FUNCTION EVALUATIONS EXCEEDED\s*/) {
									$maxeval_error = 1 ;
									last;
								}
							}
						} 
						unless ($significant_digits_accept and 
								(defined $output_file -> get_single_value(attribute =>'significant_digits',
																		  problem_index => ($problem-1))) and
								$output_file -> get_single_value(attribute =>'significant_digits',
																 problem_index => ($problem-1))>= $significant_digits_accept)  {
							push( @reruns, $problem );
							$self->stop_motion_call(tool=>'modelfit',message => "Minimization not successful, must restart.")
								if ($self->stop_motion() > 1);
						}
					}
				} # end  is_estimation
			}		

			if( ${$tries} < ($retries) and scalar @reruns > 0 
				and (not $queue_info_ref -> {'have_accepted_run'})) {
				$marked_for_rerun = 1;
				${$tries} ++;
				
				if( ${$tries} >= $self->min_retries and $self->verbose ){
					my $message = "R:".($run_no+1).":". (${$tries}+1) . " ";
					ui -> print( category => 'all',  message  => $message,
								 newline => 0);
				}
				
				my $degree = 0.1*${$tries};
				if ( ($round_error && $cut_thetas_rounding_errors) or ($hessian_error && $handle_hessian_npd) 
					 or ($maxeval_error && $cut_thetas_maxevals)) {
					#If got rid of theta omega sigma as part of handle maxevals then reset msfo fixes that
					#cut thetas does a update inits, needs the parameters!
					$self -> reset_msfo( basic_model => $model,
										 candidate_model => $candidate_model );
					
					$self -> cut_thetas( candidate_model => $candidate_model,
										 cutoff_thetas => \@cutoff_thetas,
										 output_file => $output_file );
					$self->stop_motion_call(tool=>'modelfit',message => "done cut_thetas")
						if ($self->stop_motion());

					if ($tweak_inits and ${$tries}>0){ #do not perturb first time, since we cut first thetas then
						$degree = 0.1;
					}else{
						$degree = 0;
					}
				}elsif( $self->handle_msfo or ($maxevals > 0)) {
					# This code must be adjusted for multiple problems??
					#If got rid of theta omega sigma as part of handle maxevals then reset msfo fixes that
					$self -> reset_msfo( basic_model => $model,
										 candidate_model => $candidate_model );
					#do not set degree 0 here, if we got here it means we do not simply continue if maxevals reached
					
				}

				if ($degree > 0){
					foreach my $prob ( @reruns ) {
						$problems[$prob-1] -> set_random_inits ( degree => $degree );
					}
				}
				$candidate_model->_write;
				$self->stop_motion_call(tool=>'modelfit',
										message => "this run needed restart, have tweaked inits and ".
										"written to ".$candidate_model->filename())
					if ($self->stop_motion() > 1);
				
				${$modelfile_tainted} = 1;	    
			}
		} #end if (not $marked_for_rerun)

		# }}}

		# {{{ Perturb estimates if min_retries not reached

		# This "if" block should conceptually be last, since it is
		# something we should only do if the model succeeds. In
		# practise it could swap places with at least the tweak inits
		# block, but for simplicities sake, lets leave it at the
		# bottom.
		
		if( not $marked_for_rerun and ${$tries} < $self->min_retries ) {
			#Here we force pertubation when the model is successful.
			#If got rid of theta omega sigma as part of handle maxevals then reset msfo fixes that
			${$tries} ++;
			$marked_for_rerun = 1; 	
			$queue_info_ref -> {'have_accepted_run'}=1;#would have been content now if were not for min_retries

			my $degree = 0.1 * ${$tries};
			if ($maxevals > 0) {
				$self -> reset_msfo( basic_model => $model,
									 candidate_model => $candidate_model );
				
				foreach my $prob ( @{$candidate_model -> problems} ) {
					$prob -> set_random_inits ( degree => $degree );
				}
				
				$candidate_model->_write;
				$self->stop_motion_call(tool=>'modelfit',message => "Have not reached minimum number of retries,".
										" must restart. Have tweaked initial estimates in ".
										$candidate_model->filename()." and reset msfo to prepare for rerun")
					if ($self->stop_motion()> 1);
				
			} else {
				foreach my $prob ( @{$candidate_model -> problems} ) {
					$prob -> set_random_inits ( degree => $degree );
				}
				
				$candidate_model->_write;
				$self->stop_motion_call(tool=>'modelfit',message => "Have not reached minimum number of retries, must restart. Have tweaked initial estimates in ".$candidate_model->filename()." to prepare for rerun")
					if ($self->stop_motion()> 1);
			}
			
			${$modelfile_tainted} = 1;
		}

		# }}}

		$output_file -> flush;
		
		$lstsuccess = 1; # We did find the lst file.

	} else {
		#we do not have any psn.lst. Cannot happen if nmfe 
		#and overtime kill by system, at least model and NMtran mess
		my $nmrundir = 'the model in NM_run'.($run_no+1);
		if (not (-e 'FDATA')){
			$failure = 'File system problem or NMtran could not be initiated (the NMtran output file FDATA is missing)';
			$failure_mess="\nFile system problem or NMtran could not be initiated (the NMtran output file FDATA is missing). There is no lst-file for $nmrundir.";
			if ($self->run_local) {
				$failure_mess .= " It is recommended to check the perl installation, and perl settings in psn.conf." ;
				$failure .= ' - check perl settings in psn.conf and perl installation';
			} elsif ($self->run_on_sge_nmfe) {
				$failure_mess .= " It is recommended to check cluster status, and cluster settings in psn.conf." ;
				$failure .= ' - check cluster status and cluster settings in psn.conf';
			} elsif ($self->run_on_lsf_nmfe) {
				
				if (-e 'job_submission_error'){
					open( MESS, '<job_submission_error' );
					$failure = '';
					$failure_mess = "Job submission error:\n";
					while(<MESS>) {
						chomp;
						$failure .= $_.' ';
						$failure_mess .= $_."\n";
					}
					close( MESS );
				general_error($failure_mess);
				}else{
					$failure .= ' - check cluster status and cluster settings in psn.conf';
				}
			}else{
				$failure_mess .= " It is recommended to check cluster status, and cluster settings and remote_perl in psn.conf." ;
				$failure .= ' - check cluster status, and cluster settings and remote_perl in psn.conf';
			}
		}elsif (not(-e 'FREPORT')){
			$failure = 'NMtran failed';
			if (-e $self->nmtran_error_file){
				$failure_mess="NMtran failed. NMtran output:\n";
				open( MESS, '<'.$self->nmtran_error_file);
				while(<MESS>) {
					chomp;
					$failure_mess .= $_."\n";
				}
				$failure_mess .= "Due to the NMtran errors there is no output for $nmrundir";
			}else{
				$failure_mess = "NMtran failed. There is no output for $nmrundir";
				general_error($failure_mess);
			}
		}elsif (not(-e 'nonmem.exe' or -e 'NONMEM_MPI.exe' or -e 'nonmem_mpi.exe' or -e 'nonmem_mpi' or -e 'nonmem' or -e 'nonmem5' or -e 'nonmem6' or -e 'nonmem7' )){
			$failure = 'Compilation failed';
			if ($self->nmqual){
				$failure_mess = "Fortran compilation by NMQual failed. Cannot start NONMEM.\n".
					"Go to the NM_run".($run_no+1)." subdirectory and run psn.mod with NMQual to diagnose the problem.";
			}else{
				$failure_mess = "Fortran compilation by the NONMEM's nmfe script failed. Cannot start NONMEM.\n".
					"Go to the NM_run".($run_no+1)." subdirectory and run psn.mod with NONMEM's nmfe script to diagnose the problem.";
			}
			general_error($failure_mess);
		}elsif (-e 'OUTPUT' or -e 'output'){
			$failure = 'NONMEM run interrupted';
			$failure_mess="NONMEM run interrupted. There is no lst-file for $nmrundir";
		}else{
			$failure = 'the lst-file does not exist';
			$failure_mess="There is no lst-file for $nmrundir" ;
		}

		ui -> print( category => 'all', message  => $failure_mess,newline => 1 );
	} # Did the lst file exist?

	unless( $lstsuccess ) { # psn.lst doesn't exist.
		$run_results -> [${$tries}] -> {'failed'} = $failure; #different texts for different causes
	}
	
	return $marked_for_rerun;
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
	my $ok;

	$ok=1;
	if ($self->check_nmtran and defined $self->full_path_nmtran and (length($self->full_path_nmtran)>0)){
		my $command = $self->full_path_nmtran.'  < psn.mod > FMSG';
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
	if ( defined $PsN::config -> {'_'} -> {'R'} ) {
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
					system( $PsN::config -> {'_'} -> {'R'}." CMD BATCH compute_cwres.R" );
				}
			}
		}
	}

}

sub copy_model_and_input
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'model', optional => 0 },
		 source => { isa => 'Str', optional => 1 },
		 run_nmtran => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $model = $parm{'model'};
	my $source = $parm{'source'};
	my $run_nmtran = $parm{'run_nmtran'};
	my $candidate_model;

	if (-e 'stats-runs.csv' and $self->add_retries()) {
		#possible reasons: 
		#a) Restart after -clean > 1. Then we do not know the true restart number of psn.lst
		#b) Restart after completed run -clean <= 1. Best retry is copied to psn.lst, don't know which.
		
		#remove stats-runs here. Then pick highest retry file in directory as
		#candidate model, otherwise psn.mod
		#If do not have any retry file must copy input again, was removed during clean=2
		$self->stop_motion_call(tool=>'modelfit',
								message => "have stats-runs but doing add_retries")
			if ($self->stop_motion()> 1);
		
		
		unlink 'stats-runs.csv';
		if (-e 'psn-1.mod'){
			$self->stop_motion_call(tool=>'modelfit',
									message => "old clean<2")
				if ($self->stop_motion()> 1);
			
			#clean 1. Data is left. Remove psn.mod and psn.lst
			#use last retry as candidate model. move last retry to psn.mod and
			# .lst, as if had just been run.
			unlink 'psn.lst';
			unlink 'psn.mod';
			unlink 'psn.cov';
			unlink 'psn.coi';
			unlink 'psn.cor';
			
			my $last_retry=1;
			while (-e 'psn-'.($last_retry+1).'.mod'){
				$last_retry++;
			}
			mv('psn-'.($last_retry).'.mod','psn.mod');
			mv('psn-'.($last_retry).'.lst','psn.lst') if (-e 'psn-'.($last_retry).'.lst');
			mv('psn-'.($last_retry).'.cov','psn.cov') if (-e 'psn-'.($last_retry).'.cov');
			mv('psn-'.($last_retry).'.cor','psn.cor') if (-e 'psn-'.($last_retry).'.cor');
			mv('psn-'.($last_retry).'.coi','psn.coi') if (-e 'psn-'.($last_retry).'.coi');
			
			$candidate_model =  model -> new (
				outputfile                  => 'psn.lst',
				filename                    => 'psn.mod',
				ignore_missing_output_files	=> 1,
				ignore_missing_data					=> 0);
		} else{
			$self->stop_motion_call(tool=>'modelfit',
									message => "old clean>1")
				if ($self->stop_motion()> 1);
			#clean 2. 
			#must copy input again
			
			if ($model->tbs() or $model->dtbs()){
				$self->write_tbs_files(thetanum => $model->tbs_thetanum());
			}
			#use psn.mod as candidate model
			foreach my $file( @{$model->input_files} ){
				#this does not include data files
				# $file is a ref to an array with two elements, the first is a
				# path, the second is a name.
				
				cp( $file->[0] . $file -> [1], $file -> [1] );
				
			}
			
			my @problems = @{$model->problems};
			
			unless (defined $self->data_path) {
				#copy data true
				# Fix new short names (i.e. No path)
				my @data_file_names;
				
				if( defined $model->datas ){
					foreach my $data ( @{$model->datas} ) {
						my $filename = $data -> filename;
						$filename = $self->data_path . $filename 
							if (defined $self->data_path);
						push( @data_file_names, $filename );
					}
				} else {
					croak('No datafiles set in modelfile.' );
				}
				# save references to own data and output objects
				my $datas   = $model->datas;
				my @problems = @{$model->problems};
				
				my ( @new_datas, @new_outputs );
				
				$model -> synchronize if not $model->synced;
				
				# Copy the data objects if so is requested
				if ( defined $datas ) {
					my $i = 0;
					foreach my $data ( @{$datas} ) {
						#this copies datafile with local name to NM_run
						push( @new_datas, $data ->
							  copy( filename => $data_file_names[$i]) ); #attribute skip_parsing is copied
						
						$i++;
					}
				}
				
				foreach my $data ( @new_datas ) {
					$data -> _write;
				}
				
			}
			
			$candidate_model =  model -> new (outputfile                  => 'psn.lst',
											  filename                    => 'psn.mod',
											  ignore_missing_output_files => 1,
											  ignore_missing_data => 0);
			
			$candidate_model -> shrinkage_modules( $model -> shrinkage_modules );
			
			$model -> flush_data;


		}
		
	}elsif (-e 'stats-runs.csv'){
		$self->stop_motion_call(tool=>'modelfit',message => "Did not copy anything to current directory".
								" because file stats-runs.csv already here. Must be a rerun.")
			if ($self->stop_motion()> 1);
		
		$candidate_model =  model -> new (outputfile                  => 'psn.lst',
										  filename                    => 'psn.mod',
										  ignore_missing_output_files => 1,
										  ignore_missing_data => 1);
		
		#psn.lst may be missing if e.g. nmtran failed
		
	}else{
		
		if ($model->tbs() or $model->dtbs()){
			$self->write_tbs_files(thetanum => $model->tbs_thetanum());
		}
		
		my $copy_data=1;
		$copy_data = 0 if (defined $self->data_path);
		
		# Fix new short names (i.e. No path)
		my @new_data_names;
		
		if( defined $model -> datas ){
			foreach my $data ( @{$model -> datas} ) {
				my $filename = $data -> filename;
				$filename = $self->data_path . $filename if (defined $self->data_path);
				push( @new_data_names, $filename );
			}
		} else {
			croak('No datafiles set in modelfile.' );
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
			
			cp( $file->[0] . $file -> [1], $file -> [1] );
			
		}
		

		# Copy the model object. Set the new (shorter) data file names.
		# datafiles are copied by model
		#data_file_names are only used if copy_data=1
		$candidate_model = $model -> copy( filename              => 'psn.mod',
										   data_file_names       => \@new_data_names,
										   copy_data             => $copy_data );
		
		$candidate_model -> shrinkage_modules( $model -> shrinkage_modules );
		
		$model -> flush_data;
		
		if ( $self->handle_msfo ) {
			
			# Initialize sequence of msfi/msfo files.
			
			my $msfo_names = $candidate_model -> msfo_names;
			my $msfi_names = $candidate_model -> msfi_names;
			my $msfi_in;
			
			if( defined $msfo_names ){
				$msfi_in = $msfo_names -> [0][0];
			} elsif ( defined $msfi_names ){
				$msfi_in = $msfi_names -> [0][0];
			}
			
			my $basename = 'psn_msfo';
			if( scalar @{$candidate_model -> record(record_name => 'estimation')} > 0 ){
				my $setnames = $candidate_model -> get_option_value(record_name => 'estimation',
																	option_name => 'MSFO',
																	record_index => 'all');
				if (defined $setnames){
					foreach my $set (@{$setnames}){
						$basename = $set if (defined $set);
					}
				}
			}

			if( -s $msfi_in ){
				#-s returns true if file is non-empty
				cp( $msfi_in, $basename.'-0' ); #move or copy... this is from calling directory
				$candidate_model->set_records(type=>'msfi',
											  record_strings => [$basename.'-0']);
				$candidate_model->remove_records(type=>'theta');
				$candidate_model->remove_records(type=>'omega');
				$candidate_model->remove_records(type=>'sigma');
				
			} else {
				# 
				1;
			}
			
			if( scalar @{$candidate_model -> record(record_name => 'estimation')} > 0 ){
				#record_number 0 means all, -1 means last
				$self->base_msfo_name($basename);

				$candidate_model -> remove_option( record_name => 'estimation',
												   record_number => 0,
												   fuzzy_match => 1,
												   option_name => 'MSFO',
												   problem_numbers => [scalar(@{$candidate_model -> problems})]);
				
				$candidate_model -> add_option( record_name => 'estimation',
												record_number => -1,
												option_name => 'MSFO',
												option_value=> $self->base_msfo_name,
												problem_numbers => [scalar(@{$candidate_model -> problems})]);
			}
		}
		
		$candidate_model -> table_names( new_names            => \@new_table_names,
										 ignore_missing_files => 1 );
		$candidate_model -> drop_dropped if ( $self->drop_dropped );
		$candidate_model -> _write( filename   => 'psn.mod' );# write_data => 1 );  #Kolla denna, den funkar inte utan wrap!!
		$candidate_model -> flush_data;
		$candidate_model -> store_inits;
		$self->run_nmtran if ($run_nmtran);
		
		$self->stop_motion_call(tool=>'modelfit',message => "Copied ".$model->filename().
								" to psn.mod in current directory. Modified table names.")
			if ($self->stop_motion()> 1);
		
	}

	return $candidate_model;
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

sub copy_model_and_output
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

  my $outfilename = $model -> outputs -> [0] -> full_name;

  my ($dir, $model_filename) = OSspecific::absolute_path($model -> directory,
							 $model -> filename );

  # This is used with 'prepend_model_file_name'
  my $dotless_model_filename = $model_filename;
  $dotless_model_filename =~ s/\.[^.]+$//;

  #add an ls here to try to force file sync
  my $checkname = $self -> get_retry_name( filename => 'psn.lst',
					   retry => $use_run-1 );
  my $dirt;
  for (my $i=0; $i<12; $i++){
    #an ls might make files sync and become visible
    $dirt = `ls -la 2>&1` unless ($Config{osname} eq 'MSWin32');
    last if((-e $checkname) or (-e $self->general_error_file) or (-e $self->nmtran_error_file));#psn.lst exists or will never appear
    sleep(6);
  }

  my @output_files = @{$final_model -> output_files};

  #Kajsa 2008-09-16 Prepend modelfile $use_run-1 to lst-file $use_run-1
  #prepend options file to lst
  if ($self->prepend_model_to_lst || $self->prepend_options_to_lst ) {
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

    if ($self->prepend_model_to_lst) {
      #read final modelfile to memory
      $fname = $self -> get_retry_name( filename => 'psn.mod',
					   retry => $use_run-1 );
      open (MODFILE,$fname);
      while (my $inline = <MODFILE>){
	chomp ($inline);
	push (@out_array,$inline);
      }
      close (MODFILE);
    }
    #then read final lst-file to memory, append to same array
    $fname = $self -> get_retry_name( filename => 'psn.lst',
				      retry => $use_run-1 );
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
  #end Kajsa 2008-09-16

  my @nmout;
  @nmout = split( /,/ ,$self->nm_output()) if (defined $self->nm_output());

  foreach my $filename ( @output_files, 'compilation_output.txt','psn.mod','nmqual_messages.txt' ){

    my $use_name = $self -> get_retry_name( filename => $filename,
					    retry => $use_run-1 );

    # Copy $use_run files to final files in NM_run, to be clear about which one was selected
    cp( $use_name, $filename ) if (-e $use_name);
    next if( $filename eq 'psn.mod' );
    next if( $filename eq 'nmqual_messages.txt' );

    # Don't prepend the model file name to psn.lst, but use the name
    # from the $model object.
    if( $filename eq 'psn.lst' ){
      cp( $use_name, $outfilename );
      next;
    }
    my $found_ext = 0;
    foreach my $ext (@nm7_extensions){
      if( $filename eq 'psn'.$ext ){
	$found_ext = 1;
	foreach my $out (@nmout){
	  $out =~ s/^\.//;
	  if ('.'.$out eq $ext){
	    my $ok = cp( $use_name, $dir.$dotless_model_filename.$ext);
#      print "ok=$ok $use_name ".$dir.$dotless_model_filename.'.'.$1."\n";
	    last;
	  }
	}
	last;
      }
    }
    next if ($found_ext);

    if ( $self->prepend_model_file_name ) {
      cp( $use_name, $dir . $dotless_model_filename . '.' . $filename );
    } else {
      cp( $use_name, $dir .$filename );
    }


  }

  $self->stop_motion_call(tool=>'modelfit',message => "Best retry is $use_run.\nCopied psn-".
			  $use_run.".mod to psn.mod, psn-$use_run".".lst to psn.lst etc.\n".
			  "Copied psn.lst and other output to this models 'home directory' $dir ".
			  "using filestems for $model_filename")
      if ($self->stop_motion());
  
  if ( $self->clean >= 1 and $PsN::warnings_enabled == 0 ) {
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
    $self->stop_motion_call(tool=>'modelfit',message => "Clean level is >=1. Removed NONMEM intermediate files ".
			    "like FDATA and such")
	if ($self->stop_motion()> 1);


    if( $self->clean >= 2 ){
		unlink( <temp_dir/*> );
		rmdir( 'temp_dir' );
		my $msfo=$final_model -> get_option_value(record_name => 'estimation',
												  option_name => 'MSFO');
		if (defined $msfo){
			$msfo = $self -> get_retry_name( filename => $msfo,
											 retry => $use_run-1 );
		}
		my $max_retry = $self->retries;
		$max_retry = $self->min_retries if ($self->min_retries > $max_retry);
		$max_retry++; #first run with number 1 is not a retry
		for ( my $i = 1; $i <= $max_retry; $i++ ) {
			foreach my $filename ( @output_files,'psn.mod','compilation_output.txt','nmqual_messages.txt'){

				my $use_name = $self -> get_retry_name( filename => $filename,
														retry => $i-1 );
				unlink( $use_name );
				my $crash=1;
				my $del_name = $self -> get_retry_name( filename => $filename,
														retry => $i-1,
														crash => $crash);
				while (-e $del_name){
					$crash++;
					my $next_name = $self -> get_retry_name( filename => $filename,
															 retry => $i-1,
															 crash => $crash);
					unlink( $del_name ) unless (($use_name eq $msfo) and 
												(not -e $next_name));
					$del_name = $next_name;
				}
			}
		}
		unlink( @{$model -> datafiles} );
		unlink 'psn.nmqual_out';
		$self->stop_motion_call(tool=>'modelfit',message => "Clean level is >=2. Removed all numbered retry files")
			if ($self->stop_motion()> 1);
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
}

sub lsf_submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model => { isa => 'model', optional => 0 },
		nm_version => { isa => 'Str', optional => 0 },
		work_dir => { isa => 'Str', optional => 1 }
	);
	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $work_dir = $parm{'work_dir'};

	# This method will submit the nonmem.pm file as a script to the
	# LSF system.

	my $fsubs = join( ',' , @{$model -> subroutine_files} );

	my $execution = 1 + $self->nmfe + 2 * ($self->nmqual); 
	# if nmfe is set, then sum will be 2, which means nmfe in nonmem.pm. If -nmqual is set sum will be 3
	for( my $i = 1; $i <= 5; $i++ ){
		my $str = "bsub -e stderr -o stdout " .
		($self->lsf_queue ? " -q " . $self->lsf_queue : ' ') .
		($self->lsf_project_name ? " -P " . $self->lsf_project_name : ' ') .
		($self->lsf_job_name ? " -J " . $self->lsf_job_name : ' ') .
		($self->lsf_ttl ? " -c " . $self->lsf_ttl : ' ') .
		($self->lsf_resources ? " -R " . $self->lsf_resources : ' ') .
		$self->lsf_options . " \"sleep 3 && " .
		($PsN::config -> {'_'} -> {'remote_perl'} ? ' ' . $PsN::config -> {'_'} -> {'remote_perl'} : ' perl ') . " -I" .
		$PsN::lib_dir ."/../ " . 
		$PsN::lib_dir . "/nonmem.pm" . 
		" psn.mod psn.lst " . 
		$self->nice . " ". 
		$nm_version . " " .
		1 . ' ' .
		$execution . ' ' .
		$self->display_iterations() . ' ' .
		$self->nonmem_options() . ' ' .
		$self->parafile() . ' '.
		$self->nodes() . ' ' .
		$fsubs . " \"";

		my $response = `$str 2>&1`;

		if ($response=~/Job \<(\d+)\> is submitted/) {
			$jobId=$1;
		} 


		unless( $response =~ /System call failed/ or
			$response =~ /Bad argument/ or
			$response =~ /Request aborted by esub/ or
			$response =~ /Bad user ID/ ) {
			last;
		}

#	print "$response\n";
		if( $response =~ /Bad argument/ or
			$response =~ /Request aborted by esub/ or
			$response =~ /Bad user ID/ ) {
			sleep(($i+1)**2);
		} else {
			chdir( $work_dir );
		}
		ui -> print( category => 'all', message  => 
			"bsub command was not successful, trying ".(5-$i)." times more",
			newline => 1);
	}
	sleep($self->lsf_sleep());

	return $jobId;
}

sub umbrella_submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'model', optional => 0 },
		 nm_version => { isa => 'Str', optional => 0 },
		 prepare_jobs => { isa => 'Bool', default => 0, optional => 1 },
		 queue_info => { isa => 'Ref', optional => 1 }
	);
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $job_id = -1;
	my $prepare_jobs = $parm{'prepare_jobs'};
	my $queue_info = $parm{'queue_info'};

	return $job_id;
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
						if( $prob > $max_sigmas ){
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
							if( $prob > $max ){
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
				$ui_text .= "\n    ---------- Minimization Message ----------\n";
			}
			if( defined $candidate_model ){
				my $ests = $candidate_model -> outputs -> [0] -> $param;
				# Loop the problems
				for ( my $j = 0; $j < scalar @{$ests}; $j++ ) {
					if ( ref( $ests -> [$j][0] ) ne 'ARRAY' ) {
						$ests -> [$j][0] =~ s/^\s*//;
						$ests -> [$j][0] =~ s/\s*$//;
						$log_text .= $ests -> [$j][0] .',';
						$ui_text .= sprintf("%10s",$ests -> [$j][0]);
					} else {
						
						# Loop the parameter numbers (skip sub problem level)
						for ( my $num = 0; $num < scalar @{$ests -> [$j][0]}; $num++ ) {
							$log_text .= $ests -> [$j][0][$num] .',';
							if( $param eq 'minimization_message' ){
								$ui_text .= "    ";
							}
							$ui_text .= sprintf("%12s",$ests -> [$j][0][$num]);
						}
					}
				}
			}
			if( $param eq 'minimization_message' ){
				$ui_text .= "    ------------------------------------------\n\n";
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
	    mkdir( $tmp_dir );
	}
	open( FILE, '>'.$tmp_dir.'/modelname' );
	print FILE "$modelname\n";
	close(FILE);

	return $tmp_dir;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
