use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::bootstrap;

#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(tool);

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'bca_print_order' => 'ARRAY', 'result_parameters' => 'REF',
			'bca_calculation_order' => 'ARRAY', 'bootstrap_raw_results' => 'REF',
			'jackknife_raw_results' => 'REF', 'bootstrap_diagnostics' => 'REF',
			'jackknife_diagnostics' => 'REF', 'bootstrap_estimates' => 'REF',
			'jackknife_estimates' => 'REF', 'skip_minimization_terminated' => 'SCALAR',
			'allow_ignore_id' => 'SCALAR', 'copy_data' => 'SCALAR',
			'jackknife' => 'REF', 'skip_covariance_step_terminated' => 'SCALAR',
			'skip_with_covstep_warnings' => 'SCALAR',
			'skip_estimate_near_boundary' => 'SCALAR',
			'calculation_order' => 'ARRAY', 'print_order' => 'ARRAY',
			'samples' => 'SCALAR', 'dofv' => 'SCALAR', 'dofv_samples' => 'REF',
			'mceta' => 'SCALAR', 'subjects' => 'REF', 'stratify_on' => 'SCALAR',
			'type' => 'SCALAR', 'confidence_limits' => 'HASH',
			'parameters' => 'ARRAY', 'logfile' => 'REF',
			'results_file' => 'SCALAR', 'minimization_successful_limit' => 'SCALAR',
			'covariance_step_successful_limit' => 'SCALAR',
			'covariance_step_warnings_limit' => 'SCALAR',
			'estimate_near_boundary_limit' => 'SCALAR',
			'se_confidence_intervals_check' => 'SCALAR',
			'se_confidence_intervals_level' => 'SCALAR',
			'percentile_confidence_intervals_check' => 'SCALAR',
			'percentile_confidence_intervals_level' => 'SCALAR',
			'bca_confidence_intervals_check' => 'SCALAR',
			'bca_confidence_intervals_level' => 'SCALAR',
			'large_bias_limit' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
	}
	foreach my $givenp ( keys %parm ) {
		$superParms{$givenp} = $parm{$givenp} and next unless( defined $valid_parm{$givenp});

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'bca_print_order'} = defined $parm{'bca_print_order'} ? $parm{'bca_print_order'} : ['diagnostic_means','means','bias','bca_confidence_intervals','standard_error_confidence_intervals','standard_errors','medians','jackknife_means','percentile_confidence_intervals'] unless defined $this -> {'bca_print_order'};
	$this -> {'result_parameters'} = defined $parm{'result_parameters'} ? $parm{'result_parameters'} : {} unless defined $this -> {'result_parameters'};
	$this -> {'bca_calculation_order'} = defined $parm{'bca_calculation_order'} ? $parm{'bca_calculation_order'} : ['diagnostic_means','means','medians','percentile_confidence_intervals','standard_errors','standard_error_confidence_intervals','jackknife_means','bca_confidence_intervals'] unless defined $this -> {'bca_calculation_order'};
	$this -> {'bootstrap_raw_results'} = defined $parm{'bootstrap_raw_results'} ? $parm{'bootstrap_raw_results'} : [] unless defined $this -> {'bootstrap_raw_results'};
	$this -> {'jackknife_raw_results'} = defined $parm{'jackknife_raw_results'} ? $parm{'jackknife_raw_results'} : [] unless defined $this -> {'jackknife_raw_results'};
	$this -> {'bootstrap_diagnostics'} = defined $parm{'bootstrap_diagnostics'} ? $parm{'bootstrap_diagnostics'} : [] unless defined $this -> {'bootstrap_diagnostics'};
	$this -> {'jackknife_diagnostics'} = defined $parm{'jackknife_diagnostics'} ? $parm{'jackknife_diagnostics'} : [] unless defined $this -> {'jackknife_diagnostics'};
	$this -> {'bootstrap_estimates'} = defined $parm{'bootstrap_estimates'} ? $parm{'bootstrap_estimates'} : [] unless defined $this -> {'bootstrap_estimates'};
	$this -> {'jackknife_estimates'} = defined $parm{'jackknife_estimates'} ? $parm{'jackknife_estimates'} : [] unless defined $this -> {'jackknife_estimates'};
	$this -> {'skip_minimization_terminated'} = defined $parm{'skip_minimization_terminated'} ? $parm{'skip_minimization_terminated'} : 1 unless defined $this -> {'skip_minimization_terminated'};
	$this -> {'allow_ignore_id'} = defined $parm{'allow_ignore_id'} ? $parm{'allow_ignore_id'} : 0 unless defined $this -> {'allow_ignore_id'};
	$this -> {'copy_data'} = defined $parm{'copy_data'} ? $parm{'copy_data'} : 0 unless defined $this -> {'copy_data'};
	$this -> {'skip_covariance_step_terminated'} = defined $parm{'skip_covariance_step_terminated'} ? $parm{'skip_covariance_step_terminated'} : 0 unless defined $this -> {'skip_covariance_step_terminated'};
	$this -> {'skip_with_covstep_warnings'} = defined $parm{'skip_with_covstep_warnings'} ? $parm{'skip_with_covstep_warnings'} : 0 unless defined $this -> {'skip_with_covstep_warnings'};
	$this -> {'skip_estimate_near_boundary'} = defined $parm{'skip_estimate_near_boundary'} ? $parm{'skip_estimate_near_boundary'} : 0 unless defined $this -> {'skip_estimate_near_boundary'};
	$this -> {'calculation_order'} = defined $parm{'calculation_order'} ? $parm{'calculation_order'} : ['diagnostic_means','means','medians','percentile_confidence_intervals','standard_errors','standard_error_confidence_intervals'] unless defined $this -> {'calculation_order'};
	$this -> {'print_order'} = defined $parm{'print_order'} ? $parm{'print_order'} : ['diagnostic_means','means','bias','standard_error_confidence_intervals','standard_errors','medians','percentile_confidence_intervals'] unless defined $this -> {'print_order'};
	$this -> {'samples'} = defined $parm{'samples'} ? $parm{'samples'} : 200 unless defined $this -> {'samples'};
	$this -> {'dofv'} = defined $parm{'dofv'} ? $parm{'dofv'} : 0 unless defined $this -> {'dofv'};
	$this -> {'dofv_samples'} = defined $parm{'dofv_samples'} ? $parm{'dofv_samples'} : [] unless defined $this -> {'dofv_samples'};
	$this -> {'mceta'} = defined $parm{'mceta'} ? $parm{'mceta'} : 0 unless defined $this -> {'mceta'};
	$this -> {'type'} = defined $parm{'type'} ? $parm{'type'} : 'bootstrap' unless defined $this -> {'type'};
	$this -> {'confidence_limits'} = defined $parm{'confidence_limits'} ? $parm{'confidence_limits'} : {'0.1'=>3.2905,'1'=>2.5758,'5'=>1.96,'10'=>1.6449} unless defined $this -> {'confidence_limits'};
	$this -> {'parameters'} = defined $parm{'parameters'} ? $parm{'parameters'} : ['ofv','theta','omega','sigma'] unless defined $this -> {'parameters'};
	$this -> {'logfile'} = defined $parm{'logfile'} ? $parm{'logfile'} : ['bootstraplog.csv'] unless defined $this -> {'logfile'};
	$this -> {'results_file'} = defined $parm{'results_file'} ? $parm{'results_file'} : 'bootstrap_results.csv' unless defined $this -> {'results_file'};
	$this -> {'minimization_successful_limit'} = defined $parm{'minimization_successful_limit'} ? $parm{'minimization_successful_limit'} : 0.80 unless defined $this -> {'minimization_successful_limit'};
	$this -> {'covariance_step_successful_limit'} = defined $parm{'covariance_step_successful_limit'} ? $parm{'covariance_step_successful_limit'} : 0.80 unless defined $this -> {'covariance_step_successful_limit'};
	$this -> {'covariance_step_warnings_limit'} = defined $parm{'covariance_step_warnings_limit'} ? $parm{'covariance_step_warnings_limit'} : 0.20 unless defined $this -> {'covariance_step_warnings_limit'};
	$this -> {'estimate_near_boundary_limit'} = defined $parm{'estimate_near_boundary_limit'} ? $parm{'estimate_near_boundary_limit'} : 0.2 unless defined $this -> {'estimate_near_boundary_limit'};
	$this -> {'se_confidence_intervals_check'} = defined $parm{'se_confidence_intervals_check'} ? $parm{'se_confidence_intervals_check'} : 0 unless defined $this -> {'se_confidence_intervals_check'};
	$this -> {'se_confidence_intervals_level'} = defined $parm{'se_confidence_intervals_level'} ? $parm{'se_confidence_intervals_level'} : 5 unless defined $this -> {'se_confidence_intervals_level'};
	$this -> {'percentile_confidence_intervals_check'} = defined $parm{'percentile_confidence_intervals_check'} ? $parm{'percentile_confidence_intervals_check'} : 0.5 unless defined $this -> {'percentile_confidence_intervals_check'};
	$this -> {'percentile_confidence_intervals_level'} = defined $parm{'percentile_confidence_intervals_level'} ? $parm{'percentile_confidence_intervals_level'} : 5 unless defined $this -> {'percentile_confidence_intervals_level'};
	$this -> {'bca_confidence_intervals_check'} = defined $parm{'bca_confidence_intervals_check'} ? $parm{'bca_confidence_intervals_check'} : 0 unless defined $this -> {'bca_confidence_intervals_check'};
	$this -> {'bca_confidence_intervals_level'} = defined $parm{'bca_confidence_intervals_level'} ? $parm{'bca_confidence_intervals_level'} : 5 unless defined $this -> {'bca_confidence_intervals_level'};
	$this -> {'large_bias_limit'} = defined $parm{'large_bias_limit'} ? $parm{'large_bias_limit'} : 0.05 unless defined $this -> {'large_bias_limit'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub bca_print_order {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bca_print_order'} = $parm;
	} else {
		return $self -> {'bca_print_order'};
	}
}

sub result_parameters {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'result_parameters'} = $parm;
	} else {
		return $self -> {'result_parameters'};
	}
}

sub bca_calculation_order {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bca_calculation_order'} = $parm;
	} else {
		return $self -> {'bca_calculation_order'};
	}
}

sub bootstrap_raw_results {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bootstrap_raw_results'} = $parm;
	} else {
		return $self -> {'bootstrap_raw_results'};
	}
}

sub jackknife_raw_results {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'jackknife_raw_results'} = $parm;
	} else {
		return $self -> {'jackknife_raw_results'};
	}
}

sub bootstrap_diagnostics {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bootstrap_diagnostics'} = $parm;
	} else {
		return $self -> {'bootstrap_diagnostics'};
	}
}

sub jackknife_diagnostics {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'jackknife_diagnostics'} = $parm;
	} else {
		return $self -> {'jackknife_diagnostics'};
	}
}

sub bootstrap_estimates {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bootstrap_estimates'} = $parm;
	} else {
		return $self -> {'bootstrap_estimates'};
	}
}

sub jackknife_estimates {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'jackknife_estimates'} = $parm;
	} else {
		return $self -> {'jackknife_estimates'};
	}
}

sub skip_minimization_terminated {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'skip_minimization_terminated'} = $parm;
	} else {
		return $self -> {'skip_minimization_terminated'};
	}
}

sub allow_ignore_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'allow_ignore_id'} = $parm;
	} else {
		return $self -> {'allow_ignore_id'};
	}
}

sub copy_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'copy_data'} = $parm;
	} else {
		return $self -> {'copy_data'};
	}
}

sub jackknife {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'jackknife'} = $parm;
	} else {
		return $self -> {'jackknife'};
	}
}

sub skip_covariance_step_terminated {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'skip_covariance_step_terminated'} = $parm;
	} else {
		return $self -> {'skip_covariance_step_terminated'};
	}
}

sub skip_with_covstep_warnings {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'skip_with_covstep_warnings'} = $parm;
	} else {
		return $self -> {'skip_with_covstep_warnings'};
	}
}

sub skip_estimate_near_boundary {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'skip_estimate_near_boundary'} = $parm;
	} else {
		return $self -> {'skip_estimate_near_boundary'};
	}
}

sub calculation_order {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'calculation_order'} = $parm;
	} else {
		return $self -> {'calculation_order'};
	}
}

sub print_order {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'print_order'} = $parm;
	} else {
		return $self -> {'print_order'};
	}
}

sub samples {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'samples'} = $parm;
	} else {
		return $self -> {'samples'};
	}
}

sub dofv {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'dofv'} = $parm;
	} else {
		return $self -> {'dofv'};
	}
}

sub dofv_samples {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'dofv_samples'} = $parm;
	} else {
		return $self -> {'dofv_samples'};
	}
}

sub mceta {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'mceta'} = $parm;
	} else {
		return $self -> {'mceta'};
	}
}

sub subjects {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subjects'} = $parm;
	} else {
		return $self -> {'subjects'};
	}
}

sub stratify_on {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'stratify_on'} = $parm;
	} else {
		return $self -> {'stratify_on'};
	}
}

sub type {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'type'} = $parm;
	} else {
		return $self -> {'type'};
	}
}

sub confidence_limits {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'confidence_limits'} = $parm;
	} else {
		return $self -> {'confidence_limits'};
	}
}

sub parameters {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parameters'} = $parm;
	} else {
		return $self -> {'parameters'};
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

sub minimization_successful_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'minimization_successful_limit'} = $parm;
	} else {
		return $self -> {'minimization_successful_limit'};
	}
}

sub covariance_step_successful_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariance_step_successful_limit'} = $parm;
	} else {
		return $self -> {'covariance_step_successful_limit'};
	}
}

sub covariance_step_warnings_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariance_step_warnings_limit'} = $parm;
	} else {
		return $self -> {'covariance_step_warnings_limit'};
	}
}

sub estimate_near_boundary_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimate_near_boundary_limit'} = $parm;
	} else {
		return $self -> {'estimate_near_boundary_limit'};
	}
}

sub se_confidence_intervals_check {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'se_confidence_intervals_check'} = $parm;
	} else {
		return $self -> {'se_confidence_intervals_check'};
	}
}

sub se_confidence_intervals_level {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'se_confidence_intervals_level'} = $parm;
	} else {
		return $self -> {'se_confidence_intervals_level'};
	}
}

sub percentile_confidence_intervals_check {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'percentile_confidence_intervals_check'} = $parm;
	} else {
		return $self -> {'percentile_confidence_intervals_check'};
	}
}

sub percentile_confidence_intervals_level {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'percentile_confidence_intervals_level'} = $parm;
	} else {
		return $self -> {'percentile_confidence_intervals_level'};
	}
}

sub bca_confidence_intervals_check {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bca_confidence_intervals_check'} = $parm;
	} else {
		return $self -> {'bca_confidence_intervals_check'};
	}
}

sub bca_confidence_intervals_level {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bca_confidence_intervals_level'} = $parm;
	} else {
		return $self -> {'bca_confidence_intervals_level'};
	}
}

sub large_bias_limit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'large_bias_limit'} = $parm;
	} else {
		return $self -> {'large_bias_limit'};
	}
}

sub _sampleTools {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'samples' => 'SCALAR', 'subjects' => 'SCALAR',
			'target' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->_sampleTools: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->_sampleTools: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->_sampleTools: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->_sampleTools: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->_sampleTools: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $samples = defined $parm{'samples'} ? $parm{'samples'} : 200;
	my $subjects = $parm{'subjects'};
	my @newModels;
	my $target = defined $parm{'target'} ? $parm{'target'} : 'disk';

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@newModels;
}

sub modelfit_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub cleanup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->cleanup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->cleanup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->cleanup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->cleanup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->cleanup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub llp_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->llp_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->llp_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->llp_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->llp_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->llp_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub resample {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'target' => 'SCALAR', 'model' => 'model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->resample: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->resample: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->resample: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->resample: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->resample: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $target = defined $parm{'target'} ? $parm{'target'} : 'disk';
	my @resample_models;
	my $model = $parm{'model'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@resample_models;
}

sub within_se_confidence_limits {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'original_models' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->within_se_confidence_limits: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->within_se_confidence_limits: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->within_se_confidence_limits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->within_se_confidence_limits: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->within_se_confidence_limits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @original_models = defined $parm{'original_models'} ? @{$parm{'original_models'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_diagnostic_means {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'parameter_names' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->calculate_diagnostic_means: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_diagnostic_means: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_diagnostic_means: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_diagnostic_means: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_diagnostic_means: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_means {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'parameter_names' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->calculate_means: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_means: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_means: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_means: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_means: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_bca_confidence_intervals {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'parameter_names' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->calculate_bca_confidence_intervals: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_bca_confidence_intervals: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_bca_confidence_intervals: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_bca_confidence_intervals: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_bca_confidence_intervals: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_jackknife_means {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'parameter_names' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->calculate_jackknife_means: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_jackknife_means: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_jackknife_means: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_jackknife_means: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_jackknife_means: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_medians {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'parameter_names' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->calculate_medians: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_medians: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_medians: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_medians: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_medians: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_standard_errors {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'parameter_names' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_errors: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_errors: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_errors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_errors: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_errors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_standard_error_confidence_intervals {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'parameter_names' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_error_confidence_intervals: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_error_confidence_intervals: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_error_confidence_intervals: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_error_confidence_intervals: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_standard_error_confidence_intervals: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_percentile_confidence_intervals {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'parameter_names' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->calculate_percentile_confidence_intervals: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_percentile_confidence_intervals: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_percentile_confidence_intervals: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_percentile_confidence_intervals: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->calculate_percentile_confidence_intervals: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub general_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'class' => 'SCALAR',
			'subm_threads' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->general_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->general_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->general_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->general_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->general_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $class = $parm{'class'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub modelfit_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub modelfit_post_fork_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_post_fork_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_post_fork_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_post_fork_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_post_fork_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->modelfit_post_fork_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _modelfit_raw_results_callback {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->_modelfit_raw_results_callback: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->_modelfit_raw_results_callback: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->_modelfit_raw_results_callback: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \&subroutine;
}

sub _dofv_raw_results_callback {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->_dofv_raw_results_callback: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->_dofv_raw_results_callback: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->_dofv_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->_dofv_raw_results_callback: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->_dofv_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \&subroutine;
}

sub _jackknife_raw_results_callback {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::bootstrap->_jackknife_raw_results_callback: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::bootstrap->_jackknife_raw_results_callback: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::bootstrap->_jackknife_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->_jackknife_raw_results_callback: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::bootstrap->_jackknife_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \&subroutine;
}

sub bca_read_raw_results {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub prepare_results {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub create_matlab_scripts {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub create_R_scripts {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

1;

