use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool;

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
	# End of Non-Dia code #

}

sub _make_dir {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub post_fork_analyze {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

sub pre_fork_setup {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

sub read_log {
	my $self = shift;
	my $found_log = 0;
	my $found_tool_id = 0;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

1;

