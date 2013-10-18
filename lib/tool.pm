use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool;
use Carp;
use ext::Parallel::ForkManager;
use strict;
use Cwd;
use File::Copy 'cp';
use OSspecific;
use Storable;
use Math::Random;
use ui;
use Data::Dumper;
use Config;
our $AUTOLOAD;
use debug;


#---------------------------------------------------------------------
#         Used Packages
#---------------------------------------------------------------------
use tool;
use model;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'models' => 'ARRAY', 'tools' => 'ARRAY', 'first_callback' => 'SCALAR',
			'adaptive' => 'SCALAR', 'raw_line_structure' => 'REF',
			'check_nmtran' => 'SCALAR', 'last_est_complete' => 'SCALAR',
			'add_retries' => 'SCALAR', 'niter_eonly' => 'SCALAR',
			'abort_on_fail' => 'SCALAR', 'accepted_ofv_difference' => 'SCALAR',
			'base_directory' => '', 'clean' => 'SCALAR',
			'compress' => 'SCALAR', 'cpu_time' => 'SCALAR',
			'diagnostic_parameters' => 'ARRAY', 'directory' => '',
			'drop_dropped' => 'SCALAR', 'stop_motion' => 'SCALAR',
			'grid_batch_size' => 'SCALAR', 'logfile' => 'REF',
			'max_runtime' => 'SCALAR', 'min_retries' => 'SCALAR',
			'missing_data_token' => 'SCALAR', 'model_number' => 'SCALAR',
			'nice' => 'SCALAR', 'nm_version' => 'SCALAR',
			'prepend_model_to_lst' => 'SCALAR', 'prepend_options_to_lst' => 'SCALAR',
			'tool_id' => 'SCALAR', 'parent_tool_id' => 'SCALAR',
			'parent_threads' => 'SCALAR', 'picky' => 'SCALAR',
			'prepared_models' => 'REF', 'prepend_model_file_name' => 'SCALAR',
			'raw_results' => 'ARRAY', 'raw_results_append' => 'SCALAR',
			'raw_results_file' => 'REF', 'raw_results_header' => 'ARRAY',
			'raw_nonp_results_header' => 'ARRAY', 'reference_object' => '',
			'results' => 'ARRAY', 'results_file' => 'SCALAR',
			'resume' => 'SCALAR', 'retries' => '', 'run_on_lsf' => 'SCALAR',
			'run_local' => 'SCALAR', 'run_on_mosix' => 'SCALAR',
			'seed' => '', 'significant_digits_accept' => 'SCALAR',
			'subtools' => 'ARRAY', 'subtool_arguments' => 'HASH',
			'threads' => '', 'parafile' => 'SCALAR', 'nodes' => 'SCALAR',
			'nmfe_options' => 'SCALAR', 'nm_output' => 'SCALAR',
			'nmqual_xml' => 'SCALAR', 'nonmem_options' => 'SCALAR',
			'tool_id' => 'SCALAR', 'tweak_inits' => 'SCALAR',
			'verbose' => 'SCALAR', '_raw_results_callback' => '',
			'correlation_limit' => 'SCALAR', 'condition_number_limit' => 'SCALAR',
			'near_bound_sign_digits' => 'SCALAR', 'near_zero_boundary_limit' => 'SCALAR',
			'sign_digits_off_diagonals' => 'SCALAR', 'large_theta_cv_limit' => 'SCALAR',
			'large_omega_cv_limit' => 'SCALAR', 'large_sigma_cv_limit' => 'SCALAR',
			'lsf_job_name' => 'SCALAR', 'lsf_project_name' => 'SCALAR',
			'lsf_queue' => 'SCALAR', 'lsf_resources' => 'SCALAR',
			'lsf_ttl' => 'SCALAR', 'lsf_sleep' => 'SCALAR',
			'lsf_options' => 'SCALAR', 'grid_poll_interval' => 'SCALAR',
			'nonparametric_etas' => 'SCALAR', 'nonparametric_marginals' => 'SCALAR',
			'shrinkage' => 'SCALAR', 'eigen_values' => 'SCALAR',
			'precision' => 'SCALAR', 'quick_summarize' => 'SCALAR',
			'rerun' => 'SCALAR', 'handle_crashes' => 'SCALAR',
			'handle_msfo' => 'SCALAR', 'raw_nonp_results' => 'ARRAY',
			'raw_nonp_file' => 'REF', 'sge_prepend_flags' => 'SCALAR',
			'nmfe' => 'SCALAR', 'nmqual' => 'SCALAR', 'top_tool' => 'SCALAR',
			'ud_native_retrieve' => 'SCALAR', 'ud_sleep' => 'SCALAR',
			'run_on_umbrella' => 'SCALAR', 'expected_run_time' => 'SCALAR',
			'umbrella_timeout' => 'SCALAR', 'crash_restarts' => 'SCALAR',
			'run_on_ud' => 'SCALAR', 'run_on_sge' => 'SCALAR',
			'run_on_sge_nmfe' => 'SCALAR', 'run_on_lsf_nmfe' => 'SCALAR',
			'run_on_slurm' => 'SCALAR', 'email_address' => 'SCALAR',
			'send_email' => 'SCALAR', 'run_on_zink' => 'SCALAR',
			'display_iterations' => 'SCALAR', 'slurm_project' => 'SCALAR',
			'slurm_prepend_flags' => 'SCALAR', 'sge_resource' => 'SCALAR',
			'sge_queue' => 'SCALAR', 'torque_prepend_flags' => 'SCALAR',
			'torque_queue' => 'SCALAR', 'run_on_torque' => 'SCALAR',
			'subtool_results' => 'ARRAY', 'summarize' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'first_callback'} = defined $parm{'first_callback'} ? $parm{'first_callback'} : 0 unless defined $this -> {'first_callback'};
	$this -> {'adaptive'} = defined $parm{'adaptive'} ? $parm{'adaptive'} : 0 unless defined $this -> {'adaptive'};
	$this -> {'check_nmtran'} = defined $parm{'check_nmtran'} ? $parm{'check_nmtran'} : 1 unless defined $this -> {'check_nmtran'};
	$this -> {'last_est_complete'} = defined $parm{'last_est_complete'} ? $parm{'last_est_complete'} : 0 unless defined $this -> {'last_est_complete'};
	$this -> {'add_retries'} = defined $parm{'add_retries'} ? $parm{'add_retries'} : 0 unless defined $this -> {'add_retries'};
	$this -> {'abort_on_fail'} = defined $parm{'abort_on_fail'} ? $parm{'abort_on_fail'} : 0 unless defined $this -> {'abort_on_fail'};
	$this -> {'accepted_ofv_difference'} = defined $parm{'accepted_ofv_difference'} ? $parm{'accepted_ofv_difference'} : 0.5 unless defined $this -> {'accepted_ofv_difference'};
	$this -> {'clean'} = defined $parm{'clean'} ? $parm{'clean'} : 1 unless defined $this -> {'clean'};
	$this -> {'compress'} = defined $parm{'compress'} ? $parm{'compress'} : 0 unless defined $this -> {'compress'};
	$this -> {'cpu_time'} = defined $parm{'cpu_time'} ? $parm{'cpu_time'} : 120 unless defined $this -> {'cpu_time'};
	$this -> {'diagnostic_parameters'} = defined $parm{'diagnostic_parameters'} ? $parm{'diagnostic_parameters'} : ['covariance_step_run','minimization_successful','covariance_step_successful','covariance_step_warnings','estimate_near_boundary','rounding_errors','zero_gradients','final_zero_gradients','hessian_reset','s_matrix_singular','significant_digits','condition_number','est_methods','model_run_time','subprob_est_time','subprob_cov_time'] unless defined $this -> {'diagnostic_parameters'};
	$this -> {'drop_dropped'} = defined $parm{'drop_dropped'} ? $parm{'drop_dropped'} : 0 unless defined $this -> {'drop_dropped'};
	$this -> {'stop_motion'} = defined $parm{'stop_motion'} ? $parm{'stop_motion'} : 0 unless defined $this -> {'stop_motion'};
	$this -> {'grid_batch_size'} = defined $parm{'grid_batch_size'} ? $parm{'grid_batch_size'} : 1 unless defined $this -> {'grid_batch_size'};
	$this -> {'logfile'} = defined $parm{'logfile'} ? $parm{'logfile'} : ['psn_logfile.csv'] unless defined $this -> {'logfile'};
	$this -> {'min_retries'} = defined $parm{'min_retries'} ? $parm{'min_retries'} : 0 unless defined $this -> {'min_retries'};
	$this -> {'missing_data_token'} = defined $parm{'missing_data_token'} ? $parm{'missing_data_token'} : "-99" unless defined $this -> {'missing_data_token'};
	$this -> {'nice'} = defined $parm{'nice'} ? $parm{'nice'} : 19 unless defined $this -> {'nice'};
	$this -> {'nm_version'} = defined $parm{'nm_version'} ? $parm{'nm_version'} : 'default' unless defined $this -> {'nm_version'};
	$this -> {'prepend_model_to_lst'} = defined $parm{'prepend_model_to_lst'} ? $parm{'prepend_model_to_lst'} : 0 unless defined $this -> {'prepend_model_to_lst'};
	$this -> {'prepend_options_to_lst'} = defined $parm{'prepend_options_to_lst'} ? $parm{'prepend_options_to_lst'} : 0 unless defined $this -> {'prepend_options_to_lst'};
	$this -> {'parent_threads'} = defined $parm{'parent_threads'} ? $parm{'parent_threads'} : 1 unless defined $this -> {'parent_threads'};
	$this -> {'picky'} = defined $parm{'picky'} ? $parm{'picky'} : 0 unless defined $this -> {'picky'};
	$this -> {'prepared_models'} = defined $parm{'prepared_models'} ? $parm{'prepared_models'} : [] unless defined $this -> {'prepared_models'};
	$this -> {'prepend_model_file_name'} = defined $parm{'prepend_model_file_name'} ? $parm{'prepend_model_file_name'} : 0 unless defined $this -> {'prepend_model_file_name'};
	$this -> {'raw_results_append'} = defined $parm{'raw_results_append'} ? $parm{'raw_results_append'} : 0 unless defined $this -> {'raw_results_append'};
	$this -> {'raw_results_file'} = defined $parm{'raw_results_file'} ? $parm{'raw_results_file'} : ['raw_results.csv'] unless defined $this -> {'raw_results_file'};
	$this -> {'raw_results_header'} = defined $parm{'raw_results_header'} ? $parm{'raw_results_header'} : ['model','problem','subproblem','covariance_step_run','minimization_successful','covariance_step_successful','covariance_step_warnings','estimate_near_boundary','rounding_errors','zero_gradients','final_zero_gradients','hessian_reset','s_matrix_singular','significant_digits','condition_number','est_methods','nburn_set','burn_in_iter','burn_in_conv','model_run_time','subprob_est_time','subprob_cov_time','ofv','theta','omega','sigma','setheta','seomega','sesigma','shrinkage_eta','shrinkage_iwres','eigen'] unless defined $this -> {'raw_results_header'};
	$this -> {'raw_nonp_results_header'} = defined $parm{'raw_nonp_results_header'} ? $parm{'raw_nonp_results_header'} : ['model','problem','subproblem','npofv','npeta','npomega'] unless defined $this -> {'raw_nonp_results_header'};
	$this -> {'results'} = defined $parm{'results'} ? $parm{'results'} : [] unless defined $this -> {'results'};
	$this -> {'results_file'} = defined $parm{'results_file'} ? $parm{'results_file'} : 'psn_results.csv' unless defined $this -> {'results_file'};
	$this -> {'resume'} = defined $parm{'resume'} ? $parm{'resume'} : 0 unless defined $this -> {'resume'};
	$this -> {'retries'} = defined $parm{'retries'} ? $parm{'retries'} : 0 unless defined $this -> {'retries'};
	$this -> {'run_on_lsf'} = defined $parm{'run_on_lsf'} ? $parm{'run_on_lsf'} : 0 unless defined $this -> {'run_on_lsf'};
	$this -> {'run_local'} = defined $parm{'run_local'} ? $parm{'run_local'} : 0 unless defined $this -> {'run_local'};
	$this -> {'run_on_mosix'} = defined $parm{'run_on_mosix'} ? $parm{'run_on_mosix'} : 0 unless defined $this -> {'run_on_mosix'};
	$this -> {'seed'} = defined $parm{'seed'} ? $parm{'seed'} : int(rand()*10000000) unless defined $this -> {'seed'};
	$this -> {'significant_digits_accept'} = defined $parm{'significant_digits_accept'} ? $parm{'significant_digits_accept'} : 0 unless defined $this -> {'significant_digits_accept'};
	$this -> {'subtools'} = defined $parm{'subtools'} ? $parm{'subtools'} : ['modelfit'] unless defined $this -> {'subtools'};
	$this -> {'threads'} = defined $parm{'threads'} ? $parm{'threads'} : 1 unless defined $this -> {'threads'};
	$this -> {'parafile'} = defined $parm{'parafile'} ? $parm{'parafile'} : 'none' unless defined $this -> {'parafile'};
	$this -> {'nodes'} = defined $parm{'nodes'} ? $parm{'nodes'} : 0 unless defined $this -> {'nodes'};
	$this -> {'nmfe_options'} = defined $parm{'nmfe_options'} ? $parm{'nmfe_options'} : 'none' unless defined $this -> {'nmfe_options'};
	$this -> {'nonmem_options'} = defined $parm{'nonmem_options'} ? $parm{'nonmem_options'} : 'none' unless defined $this -> {'nonmem_options'};
	$this -> {'tweak_inits'} = defined $parm{'tweak_inits'} ? $parm{'tweak_inits'} : 1 unless defined $this -> {'tweak_inits'};
	$this -> {'verbose'} = defined $parm{'verbose'} ? $parm{'verbose'} : 0 unless defined $this -> {'verbose'};
	$this -> {'correlation_limit'} = defined $parm{'correlation_limit'} ? $parm{'correlation_limit'} : 0.85 unless defined $this -> {'correlation_limit'};
	$this -> {'condition_number_limit'} = defined $parm{'condition_number_limit'} ? $parm{'condition_number_limit'} : 1000 unless defined $this -> {'condition_number_limit'};
	$this -> {'near_bound_sign_digits'} = defined $parm{'near_bound_sign_digits'} ? $parm{'near_bound_sign_digits'} : 2 unless defined $this -> {'near_bound_sign_digits'};
	$this -> {'near_zero_boundary_limit'} = defined $parm{'near_zero_boundary_limit'} ? $parm{'near_zero_boundary_limit'} : 0.01 unless defined $this -> {'near_zero_boundary_limit'};
	$this -> {'sign_digits_off_diagonals'} = defined $parm{'sign_digits_off_diagonals'} ? $parm{'sign_digits_off_diagonals'} : 2 unless defined $this -> {'sign_digits_off_diagonals'};
	$this -> {'large_theta_cv_limit'} = defined $parm{'large_theta_cv_limit'} ? $parm{'large_theta_cv_limit'} : 0.50 unless defined $this -> {'large_theta_cv_limit'};
	$this -> {'large_omega_cv_limit'} = defined $parm{'large_omega_cv_limit'} ? $parm{'large_omega_cv_limit'} : 0.8 unless defined $this -> {'large_omega_cv_limit'};
	$this -> {'large_sigma_cv_limit'} = defined $parm{'large_sigma_cv_limit'} ? $parm{'large_sigma_cv_limit'} : 0.8 unless defined $this -> {'large_sigma_cv_limit'};
	$this -> {'lsf_sleep'} = defined $parm{'lsf_sleep'} ? $parm{'lsf_sleep'} : 3 unless defined $this -> {'lsf_sleep'};
	$this -> {'nonparametric_etas'} = defined $parm{'nonparametric_etas'} ? $parm{'nonparametric_etas'} : 0 unless defined $this -> {'nonparametric_etas'};
	$this -> {'nonparametric_marginals'} = defined $parm{'nonparametric_marginals'} ? $parm{'nonparametric_marginals'} : 0 unless defined $this -> {'nonparametric_marginals'};
	$this -> {'shrinkage'} = defined $parm{'shrinkage'} ? $parm{'shrinkage'} : 0 unless defined $this -> {'shrinkage'};
	$this -> {'eigen_values'} = defined $parm{'eigen_values'} ? $parm{'eigen_values'} : 0 unless defined $this -> {'eigen_values'};
	$this -> {'quick_summarize'} = defined $parm{'quick_summarize'} ? $parm{'quick_summarize'} : 0 unless defined $this -> {'quick_summarize'};
	$this -> {'rerun'} = defined $parm{'rerun'} ? $parm{'rerun'} : 1 unless defined $this -> {'rerun'};
	$this -> {'handle_crashes'} = defined $parm{'handle_crashes'} ? $parm{'handle_crashes'} : 1 unless defined $this -> {'handle_crashes'};
	$this -> {'handle_msfo'} = defined $parm{'handle_msfo'} ? $parm{'handle_msfo'} : 0 unless defined $this -> {'handle_msfo'};
	$this -> {'raw_nonp_file'} = defined $parm{'raw_nonp_file'} ? $parm{'raw_nonp_file'} : ['raw_nonparametric_results.csv'] unless defined $this -> {'raw_nonp_file'};
	$this -> {'nmfe'} = defined $parm{'nmfe'} ? $parm{'nmfe'} : 0 unless defined $this -> {'nmfe'};
	$this -> {'nmqual'} = defined $parm{'nmqual'} ? $parm{'nmqual'} : 0 unless defined $this -> {'nmqual'};
	$this -> {'top_tool'} = defined $parm{'top_tool'} ? $parm{'top_tool'} : 0 unless defined $this -> {'top_tool'};
	$this -> {'ud_native_retrieve'} = defined $parm{'ud_native_retrieve'} ? $parm{'ud_native_retrieve'} : 0 unless defined $this -> {'ud_native_retrieve'};
	$this -> {'ud_sleep'} = defined $parm{'ud_sleep'} ? $parm{'ud_sleep'} : 30 unless defined $this -> {'ud_sleep'};
	$this -> {'run_on_umbrella'} = defined $parm{'run_on_umbrella'} ? $parm{'run_on_umbrella'} : 0 unless defined $this -> {'run_on_umbrella'};
	$this -> {'expected_run_time'} = defined $parm{'expected_run_time'} ? $parm{'expected_run_time'} : 5 unless defined $this -> {'expected_run_time'};
	$this -> {'umbrella_timeout'} = defined $parm{'umbrella_timeout'} ? $parm{'umbrella_timeout'} : 360 unless defined $this -> {'umbrella_timeout'};
	$this -> {'crash_restarts'} = defined $parm{'crash_restarts'} ? $parm{'crash_restarts'} : 4 unless defined $this -> {'crash_restarts'};
	$this -> {'run_on_ud'} = defined $parm{'run_on_ud'} ? $parm{'run_on_ud'} : 0 unless defined $this -> {'run_on_ud'};
	$this -> {'run_on_sge'} = defined $parm{'run_on_sge'} ? $parm{'run_on_sge'} : 0 unless defined $this -> {'run_on_sge'};
	$this -> {'run_on_sge_nmfe'} = defined $parm{'run_on_sge_nmfe'} ? $parm{'run_on_sge_nmfe'} : 0 unless defined $this -> {'run_on_sge_nmfe'};
	$this -> {'run_on_lsf_nmfe'} = defined $parm{'run_on_lsf_nmfe'} ? $parm{'run_on_lsf_nmfe'} : 0 unless defined $this -> {'run_on_lsf_nmfe'};
	$this -> {'run_on_slurm'} = defined $parm{'run_on_slurm'} ? $parm{'run_on_slurm'} : 0 unless defined $this -> {'run_on_slurm'};
	$this -> {'send_email'} = defined $parm{'send_email'} ? $parm{'send_email'} : 0 unless defined $this -> {'send_email'};
	$this -> {'run_on_zink'} = defined $parm{'run_on_zink'} ? $parm{'run_on_zink'} : 0 unless defined $this -> {'run_on_zink'};
	$this -> {'display_iterations'} = defined $parm{'display_iterations'} ? $parm{'display_iterations'} : 0 unless defined $this -> {'display_iterations'};
	$this -> {'run_on_torque'} = defined $parm{'run_on_torque'} ? $parm{'run_on_torque'} : 0 unless defined $this -> {'run_on_torque'};
	$this -> {'summarize'} = defined $parm{'summarize'} ? $parm{'summarize'} : 0 unless defined $this -> {'summarize'};

	bless $this, $class;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 23 "lib/tool_subs.pm" 
{ 
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
    # If I<debug> is set to 1(true), (many!) debug messages will be
    # printed.
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

	if( $PsN::config -> {'default_options'} -> {'lsf_pfizer'}){
		my $apath  = cwd();
		my $default_jobname;
		if ($apath =~ m/\/(AnalysisStep\d+)/) {
			$default_jobname = 'PsN-'.$1;
		}else {
			$default_jobname = 'PsN-'.time();
		}
		$this->lsf_job_name(defined $parm{'lsf_job_name'} ? $parm{'lsf_job_name'} : $default_jobname) unless defined $this->lsf_job_name;
	}
	$this->seed(defined $parm{'seed'} ? $parm{'seed'} : random_uniform_integer(1,0,10000000));
  
	#Initiate the random generator if a seed is given (which it is, see above)
	random_set_seed_from_phrase( $this->seed );

	# The base_directory refers to the directory where the tool should place its own
	# directory
  if ( defined $parm{'base_directory'} ) {
    $this->base_directory($parm{'base_directory'});
  } else {
    my ($uniquePath, $file) = OSspecific::absolute_path( '', '' );
    $this->base_directory($uniquePath);
  }
  
  my @tool_name_full = split( '::', ref $this );
  my $tool_name = $tool_name_full[$#tool_name_full];
  
  # The directory is the folder where the tools stores temporary data and 
  # runs subtools (or in the modelfit case, runs NONMEM)
  if ( defined $parm{'directory'} ) {
    my $dummy;
		my $dir;
    ( $dir, $dummy ) = OSspecific::absolute_path( $parm{'directory'}, '');
		$this->directory($dir);
  } else {
    my $file;
    $this->directory(OSspecific::unique_path( $tool_name.'_dir', $this->base_directory ));
  }
  if (ui->silent() and not defined ui->logfile()){
    ui->logfile($this->directory . 'run_messages.txt');
    #debug->logfile($this->directory . 'run_messages.txt');
  }
  

  # Create my temporary directory
  $this -> _make_dir;


	
  croak("No model specified!" )
      unless ( defined $this->models and scalar @{$this->models} > 0 );
  foreach my $mod ( @{$this->models} ) {
    croak("Supplied argument model is not defined" )
			unless defined $mod;
  }
  # Make sure that the filenames are absolute and collect model_ids
  my $first =1;
  foreach my $model ( @{$this->models} ) {
    my $datas = $model->datas;

    my ($directory, $filename) = OSspecific::absolute_path( $model->directory, $model->filename );
    $model->filename( $filename );
    $model->directory( $directory );
    $filename =~ s/(\.ctl|\.mod)$//;

    foreach my $attribute ( 'raw_results_file','raw_nonp_file' ) {
      if ( $this->top_tool() and $first  ) {
				$first =0;
				my $name = $this->$attribute->[0];
				unless ($name =~ /$filename/){
	    if ( $name =~ /\./ ) {
				$name =~ s/\./_$filename\./;
	    } else {
				$name = $name.'_'.$filename;
	    }
	    $this -> $attribute->[0] = $name;
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
    if ( defined $model->datas ) {
      my @datas = @{$model->datas};
      my $counter=0;
      foreach my $data ( @datas ) {
				my ($directory, $filename) = OSspecific::absolute_path( $datas[$counter] -> directory, $datas[$counter] -> filename );
				$data -> filename( $filename );
				$data -> directory( $directory );
				$counter++;
      }
    }
  }
}
# line 409 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'models'} = $parm;
	} else {
		return $self -> {'models'};
	}
}

sub tools {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tools'} = $parm;
	} else {
		return $self -> {'tools'};
	}
}

sub first_callback {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'first_callback'} = $parm;
	} else {
		return $self -> {'first_callback'};
	}
}

sub adaptive {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'adaptive'} = $parm;
	} else {
		return $self -> {'adaptive'};
	}
}

sub raw_line_structure {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_line_structure'} = $parm;
	} else {
		return $self -> {'raw_line_structure'};
	}
}

sub check_nmtran {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'check_nmtran'} = $parm;
	} else {
		return $self -> {'check_nmtran'};
	}
}

sub last_est_complete {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'last_est_complete'} = $parm;
	} else {
		return $self -> {'last_est_complete'};
	}
}

sub add_retries {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'add_retries'} = $parm;
	} else {
		return $self -> {'add_retries'};
	}
}

sub niter_eonly {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'niter_eonly'} = $parm;
	} else {
		return $self -> {'niter_eonly'};
	}
}

sub abort_on_fail {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'abort_on_fail'} = $parm;
	} else {
		return $self -> {'abort_on_fail'};
	}
}

sub accepted_ofv_difference {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'accepted_ofv_difference'} = $parm;
	} else {
		return $self -> {'accepted_ofv_difference'};
	}
}

sub base_directory {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'base_directory'} = $parm;
	} else {
		return $self -> {'base_directory'};
	}
}

sub clean {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'clean'} = $parm;
	} else {
		return $self -> {'clean'};
	}
}

sub compress {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'compress'} = $parm;
	} else {
		return $self -> {'compress'};
	}
}

sub cpu_time {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cpu_time'} = $parm;
	} else {
		return $self -> {'cpu_time'};
	}
}

sub diagnostic_parameters {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'diagnostic_parameters'} = $parm;
	} else {
		return $self -> {'diagnostic_parameters'};
	}
}

sub directory {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'directory'} = $parm;
	} else {
		return $self -> {'directory'};
	}
}

sub drop_dropped {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'drop_dropped'} = $parm;
	} else {
		return $self -> {'drop_dropped'};
	}
}

sub stop_motion {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'stop_motion'} = $parm;
	} else {
		return $self -> {'stop_motion'};
	}
}

sub grid_batch_size {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'grid_batch_size'} = $parm;
	} else {
		return $self -> {'grid_batch_size'};
	}
}

sub logfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'logfile'} = $parm;
	} else {
		return $self -> {'logfile'};
	}
}

sub max_runtime {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'max_runtime'} = $parm;
	} else {
		return $self -> {'max_runtime'};
	}
}

sub min_retries {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'min_retries'} = $parm;
	} else {
		return $self -> {'min_retries'};
	}
}

sub missing_data_token {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'missing_data_token'} = $parm;
	} else {
		return $self -> {'missing_data_token'};
	}
}

sub model_number {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'model_number'} = $parm;
	} else {
		return $self -> {'model_number'};
	}
}

sub nice {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nice'} = $parm;
	} else {
		return $self -> {'nice'};
	}
}

sub nm_version {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nm_version'} = $parm;
	} else {
		return $self -> {'nm_version'};
	}
}

sub prepend_model_to_lst {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prepend_model_to_lst'} = $parm;
	} else {
		return $self -> {'prepend_model_to_lst'};
	}
}

sub prepend_options_to_lst {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prepend_options_to_lst'} = $parm;
	} else {
		return $self -> {'prepend_options_to_lst'};
	}
}

sub tool_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tool_id'} = $parm;
	} else {
		return $self -> {'tool_id'};
	}
}

sub parent_tool_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parent_tool_id'} = $parm;
	} else {
		return $self -> {'parent_tool_id'};
	}
}

sub parent_threads {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parent_threads'} = $parm;
	} else {
		return $self -> {'parent_threads'};
	}
}

sub picky {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'picky'} = $parm;
	} else {
		return $self -> {'picky'};
	}
}

sub prepared_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prepared_models'} = $parm;
	} else {
		return $self -> {'prepared_models'};
	}
}

sub prepend_model_file_name {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prepend_model_file_name'} = $parm;
	} else {
		return $self -> {'prepend_model_file_name'};
	}
}

sub raw_results {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_results'} = $parm;
	} else {
		return $self -> {'raw_results'};
	}
}

sub raw_results_append {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_results_append'} = $parm;
	} else {
		return $self -> {'raw_results_append'};
	}
}

sub raw_results_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_results_file'} = $parm;
	} else {
		return $self -> {'raw_results_file'};
	}
}

sub raw_results_header {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_results_header'} = $parm;
	} else {
		return $self -> {'raw_results_header'};
	}
}

sub raw_nonp_results_header {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_nonp_results_header'} = $parm;
	} else {
		return $self -> {'raw_nonp_results_header'};
	}
}

sub reference_object {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'reference_object'} = $parm;
	} else {
		return $self -> {'reference_object'};
	}
}

sub results {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'results'} = $parm;
	} else {
		return $self -> {'results'};
	}
}

sub results_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'results_file'} = $parm;
	} else {
		return $self -> {'results_file'};
	}
}

sub resume {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'resume'} = $parm;
	} else {
		return $self -> {'resume'};
	}
}

sub retries {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'retries'} = $parm;
	} else {
		return $self -> {'retries'};
	}
}

sub run_on_lsf {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_lsf'} = $parm;
	} else {
		return $self -> {'run_on_lsf'};
	}
}

sub run_local {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_local'} = $parm;
	} else {
		return $self -> {'run_local'};
	}
}

sub run_on_mosix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_mosix'} = $parm;
	} else {
		return $self -> {'run_on_mosix'};
	}
}

sub seed {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'seed'} = $parm;
	} else {
		return $self -> {'seed'};
	}
}

sub significant_digits_accept {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'significant_digits_accept'} = $parm;
	} else {
		return $self -> {'significant_digits_accept'};
	}
}

sub subtools {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subtools'} = $parm;
	} else {
		return $self -> {'subtools'};
	}
}

sub subtool_arguments {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subtool_arguments'} = $parm;
	} else {
		return $self -> {'subtool_arguments'};
	}
}

sub threads {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'threads'} = $parm;
	} else {
		return $self -> {'threads'};
	}
}

sub parafile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parafile'} = $parm;
	} else {
		return $self -> {'parafile'};
	}
}

sub nodes {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nodes'} = $parm;
	} else {
		return $self -> {'nodes'};
	}
}

sub nmfe_options {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nmfe_options'} = $parm;
	} else {
		return $self -> {'nmfe_options'};
	}
}

sub nm_output {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nm_output'} = $parm;
	} else {
		return $self -> {'nm_output'};
	}
}

sub nmqual_xml {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nmqual_xml'} = $parm;
	} else {
		return $self -> {'nmqual_xml'};
	}
}

sub nonmem_options {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonmem_options'} = $parm;
	} else {
		return $self -> {'nonmem_options'};
	}
}

sub tool_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tool_id'} = $parm;
	} else {
		return $self -> {'tool_id'};
	}
}

sub tweak_inits {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tweak_inits'} = $parm;
	} else {
		return $self -> {'tweak_inits'};
	}
}

sub verbose {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'verbose'} = $parm;
	} else {
		return $self -> {'verbose'};
	}
}

sub _raw_results_callback {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'_raw_results_callback'} = $parm;
	} else {
		return $self -> {'_raw_results_callback'};
	}
}

sub correlation_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'correlation_limit'} = $parm;
	} else {
		return $self -> {'correlation_limit'};
	}
}

sub condition_number_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'condition_number_limit'} = $parm;
	} else {
		return $self -> {'condition_number_limit'};
	}
}

sub near_bound_sign_digits {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'near_bound_sign_digits'} = $parm;
	} else {
		return $self -> {'near_bound_sign_digits'};
	}
}

sub near_zero_boundary_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'near_zero_boundary_limit'} = $parm;
	} else {
		return $self -> {'near_zero_boundary_limit'};
	}
}

sub sign_digits_off_diagonals {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sign_digits_off_diagonals'} = $parm;
	} else {
		return $self -> {'sign_digits_off_diagonals'};
	}
}

sub large_theta_cv_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'large_theta_cv_limit'} = $parm;
	} else {
		return $self -> {'large_theta_cv_limit'};
	}
}

sub large_omega_cv_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'large_omega_cv_limit'} = $parm;
	} else {
		return $self -> {'large_omega_cv_limit'};
	}
}

sub large_sigma_cv_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'large_sigma_cv_limit'} = $parm;
	} else {
		return $self -> {'large_sigma_cv_limit'};
	}
}

sub lsf_job_name {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lsf_job_name'} = $parm;
	} else {
		return $self -> {'lsf_job_name'};
	}
}

sub lsf_project_name {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lsf_project_name'} = $parm;
	} else {
		return $self -> {'lsf_project_name'};
	}
}

sub lsf_queue {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lsf_queue'} = $parm;
	} else {
		return $self -> {'lsf_queue'};
	}
}

sub lsf_resources {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lsf_resources'} = $parm;
	} else {
		return $self -> {'lsf_resources'};
	}
}

sub lsf_ttl {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lsf_ttl'} = $parm;
	} else {
		return $self -> {'lsf_ttl'};
	}
}

sub lsf_sleep {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lsf_sleep'} = $parm;
	} else {
		return $self -> {'lsf_sleep'};
	}
}

sub lsf_options {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lsf_options'} = $parm;
	} else {
		return $self -> {'lsf_options'};
	}
}

sub grid_poll_interval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'grid_poll_interval'} = $parm;
	} else {
		return $self -> {'grid_poll_interval'};
	}
}

sub nonparametric_etas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonparametric_etas'} = $parm;
	} else {
		return $self -> {'nonparametric_etas'};
	}
}

sub nonparametric_marginals {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonparametric_marginals'} = $parm;
	} else {
		return $self -> {'nonparametric_marginals'};
	}
}

sub shrinkage {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'shrinkage'} = $parm;
	} else {
		return $self -> {'shrinkage'};
	}
}

sub eigen_values {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'eigen_values'} = $parm;
	} else {
		return $self -> {'eigen_values'};
	}
}

sub precision {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'precision'} = $parm;
	} else {
		return $self -> {'precision'};
	}
}

sub quick_summarize {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'quick_summarize'} = $parm;
	} else {
		return $self -> {'quick_summarize'};
	}
}

sub rerun {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'rerun'} = $parm;
	} else {
		return $self -> {'rerun'};
	}
}

sub handle_crashes {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'handle_crashes'} = $parm;
	} else {
		return $self -> {'handle_crashes'};
	}
}

sub handle_msfo {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'handle_msfo'} = $parm;
	} else {
		return $self -> {'handle_msfo'};
	}
}

sub raw_nonp_results {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_nonp_results'} = $parm;
	} else {
		return $self -> {'raw_nonp_results'};
	}
}

sub raw_nonp_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_nonp_file'} = $parm;
	} else {
		return $self -> {'raw_nonp_file'};
	}
}

sub sge_prepend_flags {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sge_prepend_flags'} = $parm;
	} else {
		return $self -> {'sge_prepend_flags'};
	}
}

sub nmfe {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nmfe'} = $parm;
	} else {
		return $self -> {'nmfe'};
	}
}

sub nmqual {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nmqual'} = $parm;
	} else {
		return $self -> {'nmqual'};
	}
}

sub top_tool {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'top_tool'} = $parm;
	} else {
		return $self -> {'top_tool'};
	}
}

sub ud_native_retrieve {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ud_native_retrieve'} = $parm;
	} else {
		return $self -> {'ud_native_retrieve'};
	}
}

sub ud_sleep {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ud_sleep'} = $parm;
	} else {
		return $self -> {'ud_sleep'};
	}
}

sub run_on_umbrella {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_umbrella'} = $parm;
	} else {
		return $self -> {'run_on_umbrella'};
	}
}

sub expected_run_time {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'expected_run_time'} = $parm;
	} else {
		return $self -> {'expected_run_time'};
	}
}

sub umbrella_timeout {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'umbrella_timeout'} = $parm;
	} else {
		return $self -> {'umbrella_timeout'};
	}
}

sub crash_restarts {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'crash_restarts'} = $parm;
	} else {
		return $self -> {'crash_restarts'};
	}
}

sub run_on_ud {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_ud'} = $parm;
	} else {
		return $self -> {'run_on_ud'};
	}
}

sub run_on_sge {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_sge'} = $parm;
	} else {
		return $self -> {'run_on_sge'};
	}
}

sub run_on_sge_nmfe {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_sge_nmfe'} = $parm;
	} else {
		return $self -> {'run_on_sge_nmfe'};
	}
}

sub run_on_lsf_nmfe {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_lsf_nmfe'} = $parm;
	} else {
		return $self -> {'run_on_lsf_nmfe'};
	}
}

sub run_on_slurm {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_slurm'} = $parm;
	} else {
		return $self -> {'run_on_slurm'};
	}
}

sub email_address {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'email_address'} = $parm;
	} else {
		return $self -> {'email_address'};
	}
}

sub send_email {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'send_email'} = $parm;
	} else {
		return $self -> {'send_email'};
	}
}

sub run_on_zink {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_zink'} = $parm;
	} else {
		return $self -> {'run_on_zink'};
	}
}

sub display_iterations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'display_iterations'} = $parm;
	} else {
		return $self -> {'display_iterations'};
	}
}

sub slurm_project {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'slurm_project'} = $parm;
	} else {
		return $self -> {'slurm_project'};
	}
}

sub slurm_prepend_flags {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'slurm_prepend_flags'} = $parm;
	} else {
		return $self -> {'slurm_prepend_flags'};
	}
}

sub sge_resource {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sge_resource'} = $parm;
	} else {
		return $self -> {'sge_resource'};
	}
}

sub sge_queue {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sge_queue'} = $parm;
	} else {
		return $self -> {'sge_queue'};
	}
}

sub torque_prepend_flags {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'torque_prepend_flags'} = $parm;
	} else {
		return $self -> {'torque_prepend_flags'};
	}
}

sub torque_queue {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'torque_queue'} = $parm;
	} else {
		return $self -> {'torque_queue'};
	}
}

sub run_on_torque {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_on_torque'} = $parm;
	} else {
		return $self -> {'run_on_torque'};
	}
}

sub add_tool {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_tool given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'tools'}},
		tool -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_model {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_model given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'models'}},
		model -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> analyze');
# line 862 "lib/tool_subs.pm" 
{
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
# line 2120 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> analyze');
	# End of Non-Dia code #

}

sub stop_motion_call {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR', 'tool' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->stop_motion_call: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->stop_motion_call: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->stop_motion_call: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->stop_motion_call: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->stop_motion_call: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = $parm{'message'};
	my $tool = $parm{'tool'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> stop_motion_call');
# line 509 "lib/tool_subs.pm" 
{
	print "\nPsN stop-motion: $tool\n".$message."\n(hit return to continue)";
	my $dirt = getc;
}
# line 2163 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> stop_motion_call');
	# End of Non-Dia code #

}

sub _make_dir {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _make_dir');
# line 518 "lib/tool_subs.pm" 
       {
	   unless ( -e $self->directory ) {
	       mkdir( $self->directory ) ;
	       $self->stop_motion_call(tool => 'tool',message => "created ".$self->directory)
		   if ($self->stop_motion() > 1);
	   }
       }
# line 2182 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _make_dir');
	# End of Non-Dia code #

}

sub post_fork_analyze {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> post_fork_analyze');
# line 472 "lib/tool_subs.pm" 
{
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
# line 2206 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> post_fork_analyze');
	# End of Non-Dia code #

}

sub post_subtool_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->post_subtool_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->post_subtool_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->post_subtool_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->post_subtool_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->post_subtool_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> post_subtool_analyze');
# line 1371 "lib/tool_subs.pm" 
    {
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
# line 2255 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> post_subtool_analyze');
	# End of Non-Dia code #

}

sub pre_fork_setup {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> pre_fork_setup');
# line 267 "lib/tool_subs.pm" 
{
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
# line 2279 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> pre_fork_setup');
	# End of Non-Dia code #

}

sub print_raw_results {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->print_raw_results: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->print_raw_results: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->print_raw_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->print_raw_results: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->print_raw_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub print_results {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> print_results');
# line 286 "lib/tool_subs.pm" 
{

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
				if ( /.*\D+.*/ or $nodot) { #non-digit or no decimal point
					return sprintf("%14s", $val) . ',';
				} else {
					return sprintf("%14.5f", $val) . ',';
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

    croak("No results_file defined" )
			unless ( defined $self->results_file );

    unless ( defined $self->results and scalar(@{$self->results}) == 1
	     and not defined $self->results->[0]{'own'}) {
			open ( RES, ">" . $self->directory . $self->results_file );
			$self->stop_motion_call(tool => 'tool', message => "prepare to print ".
				$self->directory . $self->results_file)
	    if ($self->stop_motion());
    }

    #the unless is here to prevent empty file from being produced, especially for mcmp

    if ( defined $self->results ) {
      my @all_results = @{$self->results};

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
  } else {
    carp("No subtools defined".
		   ", using default printing routine" );
  }
}
# line 2504 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> print_results');
	# End of Non-Dia code #

}

sub read_raw_results {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->read_raw_results: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->read_raw_results: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->read_raw_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->read_raw_results: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->read_raw_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> read_raw_results');
# line 899 "lib/tool_subs.pm" 
      {

	undef $self -> {'raw_results_header'};	# FIXME: Do in moose
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
# line 2581 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> read_raw_results');
	# End of Non-Dia code #

}

sub run {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->run: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->run: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->run: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->run: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->run: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @results;
	my @prepared_models;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> run');
# line 669 "lib/tool_subs.pm" 
    {
	my $return_dir = getcwd();
	chdir( $self->directory );
	$self->stop_motion_call(tool=> 'tool', message => "Changed directory to " . $self->directory)
	    if ($self->stop_motion());

	$self -> pre_fork_setup;

	my @models = @{$self->models};
	# Use the thread number of this tool level:
	my $threads = ref( $self->threads ) eq 'ARRAY' ?  $self->threads->[0] : $self->threads;

	# No point in using more threads than models
	$threads = $#models + 1 if ( $threads > $#models + 1);

	# Currently parallel execution is not supported on windows platforms
	$threads = 1 if( $Config{osname} eq 'MSWin32' );

	# Create new forkmanager
	my $pm = ext::Parallel::ForkManager -> new($threads) if ( $threads > 1 );
	my $aborting = 0;
	$pm -> run_on_finish( sub { my ( $pid, $exit_code, $ident ) = @_;
				    if( $exit_code ){
				      croak("Subtool died, exiting." );
				    }
				  } ) if ( $threads > 1 );
      
	# Store some globals for single-thread mode to make each loop
	# over the models see the same (fresh) prepared attributes as
	# in the parallel mode.
 	my @pre_fork_tools;

	# Loop over the models
	for ( my $i = 1; $i <= scalar @models; $i++ ) {
	    # Spawn new processes
	    $pm -> start and next if ( $threads > 1 );

	    # model_number is a member that tells the tool which model
	    # it is currently working on.
	    $self -> model_number( $i );
	    
	    # Make sure that each process gets a unique random sequence:
	    random_set_seed_from_phrase(random_uniform_integer(1,0,10000*$i));

	    # First, run setup
	    $self -> setup( model_number => $i );

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
					} else {
						carp("inside " . ref($self) . " but no prep_models defined from $tool $i");
					}
					$self -> post_subtool_analyze;
				}

	    } else {
				carp("No tool object to run from tool object." );
	    }

	    $self->results->[$i-1]{'subtools'} = \@tool_results;
	    $self->prepared_models->[$i - 1]{'subtools'} = \@tool_models;

	    # Analyze the results
	    $self -> analyze( model_number => $i );

	    Storable::store( $self->prepared_models, $self->directory . "/m$i/prepared_models.log" );
	    if ( $threads > 1 ) {
		Storable::store( $self->results, $self->directory . "/m$i/results.log" );
		  # Maybe redundant to transfer back both prepared_models as well as tools

	          # Actually, by principle everything interesting for
	          # a parent should be placed in "results" or possibly
	          # "prepared_models".
		}
	    $pm -> finish if ( $threads > 1 );
	}
	$pm -> wait_all_children if ( $threads > 1 );

	for( my $i = 1; $i <= scalar @{$self->models}; $i++ ) {
	  my @prepared_models = @{Storable::retrieve( $self->directory . "/m$i/prepared_models.log" )};
	  unless ($self->clean == 0) {
	    unlink( $self->directory . "/m$i/prepared_models.log" );
	  }
	  $self->prepared_models->[$i-1] = $prepared_models[$i-1];
	}

	if ( $threads > 1 ) {
	    for( my $i = 1; $i <= scalar @{$self->models}; $i++ ) {
		my @model_results = @{Storable::retrieve( $self->directory .  "/m$i/results.log" )};
# It is important to keep the number of dimensions: push the first value, not the
# whole array!
		$self->results->[$i - 1] = $model_results[$i - 1];

		# Read comment aboud tools.log near storable above.
	    }
	  }

	
	# Perform analyses that need to be done after all models have
	# been run and processed. Also write a result file if one is
	# defined.
	$self -> post_fork_analyze;

	chdir($return_dir);
	$self->stop_motion_call(tool=>'tool',message => "Changed directory to ".$return_dir)
	    if ($self->stop_motion());
	
	if( $self->clean >= 3 and not $self->top_tool ) {
	  
	  my $top_dir = $self->directory;
	  foreach my $dir ( <$top_dir/m*> ){
	    if( $dir =~ /m[0123456789]+$/ ){
	      unlink( <$dir/*> );
	      rmdir( $dir );
	    }
	  }

	  my $dir = $self->directory;
	  unlink( <$dir/*> );
	  rmdir( $dir );
	}
      }
# line 2754 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> run');
	# End of Non-Dia code #

	return \@results ,\@prepared_models;
}

sub setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> setup');
# line 491 "lib/tool_subs.pm" 

$self -> _prepare_model( model_number => $model_number );

# Run the setup specific for the subtool
if (defined $self->subtools) {
	my $sub_setup = $self->subtools->[0];
	if ( defined $sub_setup ) {
		$sub_setup = $sub_setup.'_setup';
		$self -> $sub_setup( model_number => $model_number );
	}
}
# line 2804 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> setup');
	# End of Non-Dia code #

}

sub register_in_database {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'force' => 'SCALAR', 'execute_id' => 'SCALAR',
			'cdd_id' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->register_in_database: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->register_in_database: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->register_in_database: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->register_in_database: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->register_in_database: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $force = defined $parm{'force'} ? $parm{'force'} : 0;
	my $tool_id;
	my $execute_id = defined $parm{'execute_id'} ? $parm{'execute_id'} : 1;
	my $cdd_id = defined $parm{'cdd_id'} ? $parm{'cdd_id'} : 1;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $tool_id;
}

sub register_tm_relation {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_ids' => 'm_ARRAY', 'prepared_models' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->register_tm_relation: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->register_tm_relation: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->register_tm_relation: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->register_tm_relation: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->register_tm_relation: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @model_ids = defined $parm{'model_ids'} ? @{$parm{'model_ids'}} : ();
	my $prepared_models = defined $parm{'prepared_models'} ? $parm{'prepared_models'} : 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub log_object {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> log_object');
# line 228 "lib/tool_subs.pm" 
{
  open( OLOG, '>',$self->directory . 'object.txt' );
  $Data::Dumper::Maxdepth = 1;
  print OLOG Dumper $self;
  $Data::Dumper::Maxdepth = 0;
  close( OLOG );
}
# line 2897 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> log_object');
	# End of Non-Dia code #

}

sub read_log {
	my $self = shift;
	my $found_log = 0;
	my $found_tool_id = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> read_log');
# line 241 "lib/tool_subs.pm" 
{
  if( -e $self->directory . 'object.txt' ) {
    $found_log = 1;
    open( OLOG, '<'.$self->directory . 'object.txt' );
    my @olog = <OLOG>;
    my $str = "(";
    for ( my $i = 1; $i < $#olog; $i++ ) {
      $str = $str.$olog[$i];
    }
    $str = $str.")";
    my %tmp = eval( $str );
    
    if( exists $tmp{'tool_id'} ) {
      $self->tool_id($tmp{'tool_id'});
      $found_tool_id = 1;
    }
    close( OLOG );
  }
}
# line 2930 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> read_log');
	# End of Non-Dia code #

	return $found_log ,$found_tool_id;
}

sub harvest_output {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'search_models' => 'SCALAR', 'search_output' => 'SCALAR',
			'search_data' => 'SCALAR', 'search_subtools' => 'SCALAR',
			'search_original_models' => 'SCALAR', 'accessor_parameters' => 'HASH',
			'accessors' => 'm_ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->harvest_output: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->harvest_output: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->harvest_output: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->harvest_output: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->harvest_output: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $search_models = defined $parm{'search_models'} ? $parm{'search_models'} : 0;
	my $search_output = defined $parm{'search_output'} ? $parm{'search_output'} : 0;
	my $search_data = defined $parm{'search_data'} ? $parm{'search_data'} : 0;
	my $search_subtools = defined $parm{'search_subtools'} ? $parm{'search_subtools'} : 0;
	my $search_original_models = defined $parm{'search_original_models'} ? $parm{'search_original_models'} : 0;
	my %accessor_parameters = defined $parm{'accessor_parameters'} ? %{$parm{'accessor_parameters'}} : ();
	my @accessors = defined $parm{'accessors'} ? @{$parm{'accessors'}} : ();
my %result;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> harvest_output');
# line 1389 "lib/tool_subs.pm" 
    {

      # harvest_output is a complement to AUTOLOAD below. AUTOLOAD is
      # currently used to find the AUTOLOAD:ed accessor in any
      # existing subtool, model, data or outputobject. It is
      # inefficient in that it will have to be called for once for
      # each accessor. harvest_output will take a list of accessors
      # that it will search for in each object, saving time and
      # foremost; memory. Also it will take arguments such as
      # "search_models", "search_subtools" that will make things more
      # efficient if you know where to search.
      
      unless( $search_models + $search_output + $search_data <= 1 ){
	croak("This is a PsN bug: Only one of the 'search_' options can and must be specified.".
			"\t search_models: $search_models\n".
			"\t search_data: $search_data\n".
			"\t search_output: $search_output");
      }
      
      if ( $search_subtools ) {
	carp("\n\nSearching subtools, which is a very untested functionality!!\n\n" );
	
      } else {
	
	sub models_traverse2 {
	  my %parameters = @_;
	  my @models = $parameters{'models'} ? @{$parameters{'models'}} : ();
	  my $search_models = $parameters{'search_models'};
	  my $search_output = $parameters{'search_output'};
	  my $search_data   = $parameters{'search_data'};
	  my $accessor_parameters = $parameters{'accessor_parameters'};
	  my $accessors = $parameters{'accessors'};
	  my %results;
	  
	  for( my $i = 0; $i < scalar (@models); $i++ ){
	    
	    foreach my $model ( @{$models[$i]{'own'}} ) {
	      
	      foreach my $accessor( @{$accessors} ) {
		
		if( $search_models and $model -> can( $accessor ) ) {
		  push( @{$results{$accessor}[$i]{'own'}}, $model -> $accessor( %{$accessor_parameters} ) );
		  
		} elsif( $search_data and $model -> datas -> [0] -> can( $accessor ) ) {
		  push( @{$results{$accessor}[$i]{'own'}}, $model -> datas -> [0] -> $accessor( %{$accessor_parameters} ) );
		  
		} elsif( $search_output and $model -> outputs -> [0] -> can( $accessor ) ) {
		  push( @{$results{$accessor}[$i]{'own'}}, $model -> outputs -> [0] -> $accessor( %{$accessor_parameters} ) );
		  
		} else {
		  croak("Neither model, data, output have a method for $accessor" );
		}
		
		if ( defined $models[$i]{'subtools'} ) {
		  push( @{$results{$accessor}[$i]{'subtools'}}, models_traverse2( models => $models[$i]{'subtools'} ) );
		}
	      }
	      
	      if( $search_data ){
		$model -> datas -> [0] -> flush();
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
				carp("Trying @accessors, but no prepared models available" );
				return {};
      }
      
      %result = %{models_traverse2( models => \@models,
				   search_models => $search_models,
				   search_output => $search_output,
				   search_data => $search_data,
				   accessor_parameters => \%accessor_parameters,
				   accessors => \@accessors )};
      
    }
# line 3069 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> harvest_output');
	# End of Non-Dia code #

	return \%result;
}

sub create_raw_results_rows {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'label_model' => 'model',
			'raw_line_structure' => 'REF', 'max_hash' => 'REF',
			'model_number' => 'SCALAR', 'eta_shrinkage_file' => 'SCALAR',
			'iwres_shrinkage_file' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->create_raw_results_rows: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->create_raw_results_rows: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->create_raw_results_rows: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->create_raw_results_rows: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->create_raw_results_rows: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model = $parm{'model'};
	my $label_model = $parm{'label_model'};
	my @return_rows;
	my @nonp_return_rows;
	my $raw_line_structure = $parm{'raw_line_structure'};
	my $max_hash = $parm{'max_hash'};
	my $model_number = defined $parm{'model_number'} ? $parm{'model_number'} : 0;
	my $eta_shrinkage_file = $parm{'eta_shrinkage_file'};
	my $iwres_shrinkage_file = $parm{'iwres_shrinkage_file'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> create_raw_results_rows');
# line 943 "lib/tool_subs.pm" 
  {
      #note that local variable hash raw_line_structure is different from attribute raw_line_structure
     #do this also for unsuccessful models
    unless( $model -> outputs -> [0] -> parsed ){
      $model -> outputs -> [0] -> abort_on_fail(0);
      $model -> outputs -> [0] -> _read_problems;
    }
    my $data_stored=0;
    my @probs;
    my $np=0;
    my $model_row = 0;
    if( $model -> outputs -> [0] -> parsed_successfully ){
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
      $self->stop_motion_call(tool => 'tool', message => "prepare to collect raw results from output object ")
	  if ($self->stop_motion() > 1);

      my $saem=0;
      my $bayes=0;
			$self->raw_results_header([]) unless defined $self->raw_results_header;
      foreach my $category ( @{$self->raw_results_header} ){
				next if( $category eq 'model' or $category eq 'problem' or $category eq 'subproblem'
						or $category eq 'method' );
				my ( $accessor, $res );

	# {{{ Get the values for the category

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
	    $saem = 1 if ($string_arr[$#string_arr] eq 'SAEM' or 
			  (index('SAEM',$string_arr[$#string_arr]==0)));
	    $bayes = 1 if ($string_arr[$#string_arr] eq 'BAYES' or 
			   (index('BAYES',$string_arr[$#string_arr]==0)));
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
#	  Shrinkage in NM 7.1 is incorrect
	    # Shrinkage does not work for subproblems right now.
	    #compute shrinkage
	    $res = $model -> eta_shrinkage(eta_filename => $eta_shrinkage_file);
	} elsif ( $category eq 'shrinkage_iwres' ) {
	  # Shrinkage does not work for subproblems right now.
	  $res = $model -> iwres_shrinkage(iwres_filename => $iwres_shrinkage_file);
	} else {
	  #covariance_step_run comes here
	  $accessor = $category;
	  $res = $model->outputs->[0]->$accessor;
	}

	# {{{ Create entry in raw_line_structure
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

	# }}}

	# }}} Get the values for the category
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
			push( @{$return_array_ref -> [$row]}, @{$res -> [$j][$k]} );
			push( @{$return_array_ref -> [$row]},
			      (undef) x ($max_hash -> {$category} - scalar @{$res -> [$j][$k]})  );
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
		
		# {{{ Push undefs for missing subprobs
		
		for( my $k = 0; $k < $ns; $k++ ) {
		  my $row = $model_row++;
		  push( @{$return_array_ref -> [$row]},
			(undef) x $max_hash -> {$category}  );
		}
		
		# }}} Push undefs for missing subprobs
	      
	      }
	    }
	  }
	} else {
	  
	  # {{{ Push undefs for missing probs/subprobs

	  for( my $j = 0; $j < $np; $j++ ) {
	    my $ns = $probs[$j]; # #subprobs
	    for( my $k = 0; $k < $ns; $k++ ) {
	      my $row = $model_row++;
	      push( @{$return_array_ref -> [$row]},
		    (undef) x $max_hash -> {$category}  );
	    }
	  }

	  # }}} Push undefs for missing probs/subprobs
	  
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
		      push( @{$return_array_ref -> [$row]},
			    (undef) x ($max_hash -> {$category} - scalar @{$res -> [$j][$k]})  );
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

      } #end foreach category


      #end nonp

    } else {
      # not $model->outputs->[0]->parsed_successfully.
      # or not data_stored (0 problems or 0 subproblems in all problems)

      my $mes = "run failed - Could not parse the output file: ".
				$model->outputs->[0]->filename;
      push( @{$return_rows[0]}, ($model_number,(1),(1),($mes)) );
      push( @{$nonp_return_rows[0]}, ($model_number,(1),(1),($mes)) );
      $raw_line_structure -> {$model_number} -> {'line_numbers'} = scalar @return_rows;
    }    
  }
# line 3541 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> create_raw_results_rows');
	# End of Non-Dia code #

	return \@return_rows ,\@nonp_return_rows;
}

sub _prepare_model {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->_prepare_model: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->_prepare_model: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->_prepare_model: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->_prepare_model: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->_prepare_model: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _prepare_model');
# line 809 "lib/tool_subs.pm" 
      {

	  my ($newdir, $newfile) = OSspecific::absolute_path( $self->directory .  '/m'.$model_number, '' );
	  carp("Making directory\t\t" . $newdir );
	  mkdir( $newdir );
	  $self->stop_motion_call(tool=>'tool',message => "Created directory $newdir ")
	      if ($self->stop_motion());
	  if ( defined $self -> models() ) {
	    my @models = @{$self -> models()};
	    if ( defined $models[$model_number - 1] ) {
	      my $model = $models[$model_number - 1];
	      # copy the msfi files


	      if (1){
		#fixed bug in msfi_names, skips existing extra options in $MSFI. AWFUL coding!!!
		# TODO, this is probably totally unneccessary. Review this.
		
		my @newarr;
		if( defined $model -> msfi_names() ){
		  foreach my $msfi_files( @{$model -> msfi_names()} ){
		    #loop $PROB
		    my @new_names;
		    if (defined $msfi_files){
		      foreach my $msfi_file( @{$msfi_files} ){
			#loop instances
			if ( defined $msfi_file ) {
			  my ( $dir, $filename ) = OSspecific::absolute_path($model -> directory,
									     $msfi_file );
			  cp( $dir.$filename, $newdir.$filename );
			  push( @new_names, $filename );
			} else {
			  push( @new_names, undef );
			}
		      }
		      push(@newarr,\@new_names);
		    }else{
		      push(@newarr,undef);
		    }
		  }
		  $model -> msfi_names( new_names => \@newarr);
		}
	      }
	    } 
	  }
      }
# line 3626 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _prepare_model');
	# End of Non-Dia code #

}

sub print_options {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'toolname' => 'm_SCALAR', 'cmd_line' => 'SCALAR',
			'directory' => 'SCALAR', 'local_options' => 'REF',
			'common_options' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool->print_options: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool->print_options: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool->print_options: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->print_options: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool->print_options: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $toolname = $parm{'toolname'};
	my $cmd_line = $parm{'cmd_line'};
	my $directory = $parm{'directory'};
	my $local_options = $parm{'local_options'};
	my $common_options = $parm{'common_options'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> print_options');
# line 1610 "lib/tool_subs.pm" 
{

  #input string directory
  #input string cmd_line
  #input string toolname
  #input ref to array of option names local_options
  #input ref to array of option names common_options
  #no return param

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
  if ($cmd_line){
    open(CMD, ">>", $dir . "/command.txt");
    print CMD $cmd_line, "\n";
    close(CMD);
  }



  open(CMD, "> ", $option_file);
  my @datearr=localtime;
  my $theDate=sprintf "%4.4d-%2.2d-%2.2d",($datearr[5]+1900),($datearr[4]+1),($datearr[3]);
  my $theTime=sprintf "%2.2d:%2.2d",($datearr[2]),($datearr[1]);
  my $info_line = "PsN version: ".$PsN::version."\nRun started: $theDate at $theTime\n";
  print CMD "$info_line";
  print CMD "version_and_option_info.txt is overwitten if the run is restarted later using option -directory.\n";
  print CMD "The original file from the first call is saved as original_version_and_option_info.txt.\n\n";
  
  if ($cmd_line){
    print CMD "Command:\n".$cmd_line. "\n\n";
  }
    
  print CMD "Actual values optional $toolname options (undefined values not listed):\n";
  foreach my $opt (sort(@{$local_options})){
    $opt =~ s/[!:|].*//g; #get rid of :s |? :i etcetera
    if (defined $self->{$opt}){
      if (not ref($self->{$opt})){
	print CMD "-$opt=".$self->{$opt}."\n";
      } elsif ( ref($self->{$opt}) eq "ARRAY") {
	if (not ref($self->{$opt}->[0])){
	  print CMD "-$opt=".(join ',',@{$self->{$opt}})."\n";
	}
      }
    }
  }

  print CMD "\nActual values optional PsN (common) options (undefined values not listed):\n";
  print CMD "-silent=1\n" if (ui->silent());
  foreach my $opt (sort(@{$common_options})){
    $opt =~ s/[!:|].*//g; #get rid of :s |? :i etcetera
    if (defined $self->{$opt}){
      if (not ref($self->{$opt})){
	print CMD "-$opt=".$self->{$opt}."\n";
      } elsif ( $opt eq 'threads') {
	print CMD "-$opt=".$self->{$opt}->[1]."\n";
      } elsif ( ref($self->{$opt}) eq "ARRAY") {
	print CMD "-$opt=".(join ',',@{$self->{$opt}})."\n";
      } elsif ( ref($self->{$opt}) eq "HASH") {
	print CMD "-$opt=".(join ',',@{$self->{$opt}})."\n";
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

}
# line 3755 libgen/tool.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> print_options');
	# End of Non-Dia code #

}

1;

