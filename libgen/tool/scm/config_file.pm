use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::scm::config_file;

#---------------------------------------------------------------------
#         Used Packages
#---------------------------------------------------------------------
use file;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'file' => 'REF', 'abort_on_fail' => 'SCALAR',
			'base_criteria_values' => 'CODE', 'categorical_covariates' => 'ARRAY',
			'error_code' => 'SCALAR', 'code' => 'HASH',
			'compress' => 'SCALAR', 'continuous_covariates' => 'ARRAY',
			'cpu_time' => 'SCALAR', 'debug' => 'SCALAR',
			'directory' => 'SCALAR', 'do_not_drop' => 'ARRAY',
			'extra_files' => 'ARRAY', 'global_init' => 'SCALAR',
			'gof' => 'SCALAR', 'grid_batch_size' => 'SCALAR',
			'included_relations' => 'HASH', 'logfile' => 'SCALAR',
			'lower_bounds' => 'HASH', 'missing_data_token' => 'SCALAR',
			'model' => 'SCALAR', 'nm_version' => 'SCALAR',
			'nmfe_options' => 'SCALAR', 'ofv_backward' => 'HASH',
			'ofv_change' => 'REF', 'ofv_forward' => 'HASH',
			'p_backward' => 'SCALAR', 'p_forward' => 'SCALAR',
			'p_value' => 'SCALAR', 'picky' => 'SCALAR',
			'relations' => 'HASH', 'retries' => 'SCALAR',
			'linearize' => 'SCALAR', 'noabort' => 'SCALAR',
			'max_steps' => 'SCALAR', 'epsilon' => 'SCALAR',
			'only_successful' => 'SCALAR', 'parallel_states' => 'SCALAR',
			'update_derivatives' => 'SCALAR', 'error' => 'SCALAR',
			'logit' => 'ARRAY', 'time_varying' => 'ARRAY',
			'second_order' => 'SCALAR', 'foce' => 'SCALAR',
			'lst_file' => 'SCALAR', 'derivatives_data' => 'SCALAR',
			'search_direction' => 'SCALAR', 'seed' => 'SCALAR',
			'subtools' => 'ARRAY', 'subtool_arguments' => 'CODE',
			'test_relations' => 'HASH', 'threads' => 'SCALAR',
			'tweak_inits' => 'SCALAR', 'upper_bounds' => 'HASH',
			'valid_array_options' => 'HASH', 'valid_code_options' => 'HASH',
			'valid_hash_options' => 'HASH', 'valid_scalar_options' => 'HASH',
			'valid_states' => 'HASH', 'inits' => 'HASH',
			'drop_dropped' => 'SCALAR', 'grid_poll_interval' => 'SCALAR',
			'run_on_lsf' => 'SCALAR', 'lsf_resources' => 'SCALAR',
			'lsf_queue' => 'SCALAR', 'lsf_project_name' => 'SCALAR',
			'lsf_job_name' => 'SCALAR', 'lsf_ttl' => 'SCALAR',
			'lsf_options' => 'SCALAR', 'run_on_torque' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm::config_file->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm::config_file->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm::config_file->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm::config_file->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm::config_file->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'abort_on_fail'} = defined $parm{'abort_on_fail'} ? $parm{'abort_on_fail'} : \'1,0' unless defined $this -> {'abort_on_fail'};
	$this -> {'base_criteria_values'} = defined $parm{'base_criteria_values'} ? $parm{'base_criteria_values'} : sub {} unless defined $this -> {'base_criteria_values'};
	$this -> {'categorical_covariates'} = defined $parm{'categorical_covariates'} ? $parm{'categorical_covariates'} : [] unless defined $this -> {'categorical_covariates'};
	$this -> {'error_code'} = defined $parm{'error_code'} ? $parm{'error_code'} : \'' unless defined $this -> {'error_code'};
	$this -> {'code'} = defined $parm{'code'} ? $parm{'code'} : {'ARRAY'} unless defined $this -> {'code'};
	$this -> {'compress'} = defined $parm{'compress'} ? $parm{'compress'} : \'1,0' unless defined $this -> {'compress'};
	$this -> {'continuous_covariates'} = defined $parm{'continuous_covariates'} ? $parm{'continuous_covariates'} : [] unless defined $this -> {'continuous_covariates'};
	$this -> {'cpu_time'} = defined $parm{'cpu_time'} ? $parm{'cpu_time'} : \'' unless defined $this -> {'cpu_time'};
	$this -> {'debug'} = defined $parm{'debug'} ? $parm{'debug'} : \'1,2,3' unless defined $this -> {'debug'};
	$this -> {'directory'} = defined $parm{'directory'} ? $parm{'directory'} : \'' unless defined $this -> {'directory'};
	$this -> {'do_not_drop'} = defined $parm{'do_not_drop'} ? $parm{'do_not_drop'} : [] unless defined $this -> {'do_not_drop'};
	$this -> {'extra_files'} = defined $parm{'extra_files'} ? $parm{'extra_files'} : [] unless defined $this -> {'extra_files'};
	$this -> {'global_init'} = defined $parm{'global_init'} ? $parm{'global_init'} : \'' unless defined $this -> {'global_init'};
	$this -> {'gof'} = defined $parm{'gof'} ? $parm{'gof'} : \'ofv,p_value' unless defined $this -> {'gof'};
	$this -> {'grid_batch_size'} = defined $parm{'grid_batch_size'} ? $parm{'grid_batch_size'} : \'' unless defined $this -> {'grid_batch_size'};
	$this -> {'included_relations'} = defined $parm{'included_relations'} ? $parm{'included_relations'} : {'ARRAY'} unless defined $this -> {'included_relations'};
	$this -> {'logfile'} = defined $parm{'logfile'} ? $parm{'logfile'} : \'' unless defined $this -> {'logfile'};
	$this -> {'lower_bounds'} = defined $parm{'lower_bounds'} ? $parm{'lower_bounds'} : {'ARRAY'} unless defined $this -> {'lower_bounds'};
	$this -> {'missing_data_token'} = defined $parm{'missing_data_token'} ? $parm{'missing_data_token'} : \'' unless defined $this -> {'missing_data_token'};
	$this -> {'model'} = defined $parm{'model'} ? $parm{'model'} : \'' unless defined $this -> {'model'};
	$this -> {'nm_version'} = defined $parm{'nm_version'} ? $parm{'nm_version'} : \'' unless defined $this -> {'nm_version'};
	$this -> {'nmfe_options'} = defined $parm{'nmfe_options'} ? $parm{'nmfe_options'} : \'' unless defined $this -> {'nmfe_options'};
	$this -> {'ofv_backward'} = defined $parm{'ofv_backward'} ? $parm{'ofv_backward'} : {} unless defined $this -> {'ofv_backward'};
	$this -> {'ofv_change'} = defined $parm{'ofv_change'} ? $parm{'ofv_change'} : {} unless defined $this -> {'ofv_change'};
	$this -> {'ofv_forward'} = defined $parm{'ofv_forward'} ? $parm{'ofv_forward'} : {} unless defined $this -> {'ofv_forward'};
	$this -> {'p_backward'} = defined $parm{'p_backward'} ? $parm{'p_backward'} : \'' unless defined $this -> {'p_backward'};
	$this -> {'p_forward'} = defined $parm{'p_forward'} ? $parm{'p_forward'} : \'' unless defined $this -> {'p_forward'};
	$this -> {'p_value'} = defined $parm{'p_value'} ? $parm{'p_value'} : \'' unless defined $this -> {'p_value'};
	$this -> {'picky'} = defined $parm{'picky'} ? $parm{'picky'} : \'1,0' unless defined $this -> {'picky'};
	$this -> {'retries'} = defined $parm{'retries'} ? $parm{'retries'} : \'' unless defined $this -> {'retries'};
	$this -> {'linearize'} = defined $parm{'linearize'} ? $parm{'linearize'} : \'1,0' unless defined $this -> {'linearize'};
	$this -> {'noabort'} = defined $parm{'noabort'} ? $parm{'noabort'} : \'1,0' unless defined $this -> {'noabort'};
	$this -> {'max_steps'} = defined $parm{'max_steps'} ? $parm{'max_steps'} : \'' unless defined $this -> {'max_steps'};
	$this -> {'epsilon'} = defined $parm{'epsilon'} ? $parm{'epsilon'} : \'1,0' unless defined $this -> {'epsilon'};
	$this -> {'only_successful'} = defined $parm{'only_successful'} ? $parm{'only_successful'} : \'1,0' unless defined $this -> {'only_successful'};
	$this -> {'parallel_states'} = defined $parm{'parallel_states'} ? $parm{'parallel_states'} : \'1,0' unless defined $this -> {'parallel_states'};
	$this -> {'update_derivatives'} = defined $parm{'update_derivatives'} ? $parm{'update_derivatives'} : \'1,0' unless defined $this -> {'update_derivatives'};
	$this -> {'error'} = defined $parm{'error'} ? $parm{'error'} : \'add,prop,propadd,exp,user' unless defined $this -> {'error'};
	$this -> {'logit'} = defined $parm{'logit'} ? $parm{'logit'} : [] unless defined $this -> {'logit'};
	$this -> {'time_varying'} = defined $parm{'time_varying'} ? $parm{'time_varying'} : [] unless defined $this -> {'time_varying'};
	$this -> {'second_order'} = defined $parm{'second_order'} ? $parm{'second_order'} : \'1,0' unless defined $this -> {'second_order'};
	$this -> {'foce'} = defined $parm{'foce'} ? $parm{'foce'} : \'1,0' unless defined $this -> {'foce'};
	$this -> {'lst_file'} = defined $parm{'lst_file'} ? $parm{'lst_file'} : \'' unless defined $this -> {'lst_file'};
	$this -> {'derivatives_data'} = defined $parm{'derivatives_data'} ? $parm{'derivatives_data'} : \'' unless defined $this -> {'derivatives_data'};
	$this -> {'search_direction'} = defined $parm{'search_direction'} ? $parm{'search_direction'} : \'forward,backward,both' unless defined $this -> {'search_direction'};
	$this -> {'seed'} = defined $parm{'seed'} ? $parm{'seed'} : \'' unless defined $this -> {'seed'};
	$this -> {'subtools'} = defined $parm{'subtools'} ? $parm{'subtools'} : [] unless defined $this -> {'subtools'};
	$this -> {'subtool_arguments'} = defined $parm{'subtool_arguments'} ? $parm{'subtool_arguments'} : sub {} unless defined $this -> {'subtool_arguments'};
	$this -> {'test_relations'} = defined $parm{'test_relations'} ? $parm{'test_relations'} : {'ARRAY'} unless defined $this -> {'test_relations'};
	$this -> {'threads'} = defined $parm{'threads'} ? $parm{'threads'} : \'' unless defined $this -> {'threads'};
	$this -> {'tweak_inits'} = defined $parm{'tweak_inits'} ? $parm{'tweak_inits'} : \'1,0' unless defined $this -> {'tweak_inits'};
	$this -> {'upper_bounds'} = defined $parm{'upper_bounds'} ? $parm{'upper_bounds'} : {'ARRAY'} unless defined $this -> {'upper_bounds'};
	$this -> {'valid_states'} = defined $parm{'valid_states'} ? $parm{'valid_states'} : {'ARRAY'} unless defined $this -> {'valid_states'};
	$this -> {'inits'} = defined $parm{'inits'} ? $parm{'inits'} : {'ARRAY'} unless defined $this -> {'inits'};
	$this -> {'drop_dropped'} = defined $parm{'drop_dropped'} ? $parm{'drop_dropped'} : \'1,0' unless defined $this -> {'drop_dropped'};
	$this -> {'grid_poll_interval'} = defined $parm{'grid_poll_interval'} ? $parm{'grid_poll_interval'} : \'' unless defined $this -> {'grid_poll_interval'};
	$this -> {'run_on_lsf'} = defined $parm{'run_on_lsf'} ? $parm{'run_on_lsf'} : \'' unless defined $this -> {'run_on_lsf'};
	$this -> {'lsf_resources'} = defined $parm{'lsf_resources'} ? $parm{'lsf_resources'} : \'' unless defined $this -> {'lsf_resources'};
	$this -> {'lsf_queue'} = defined $parm{'lsf_queue'} ? $parm{'lsf_queue'} : \'' unless defined $this -> {'lsf_queue'};
	$this -> {'lsf_project_name'} = defined $parm{'lsf_project_name'} ? $parm{'lsf_project_name'} : \'' unless defined $this -> {'lsf_project_name'};
	$this -> {'lsf_job_name'} = defined $parm{'lsf_job_name'} ? $parm{'lsf_job_name'} : \'' unless defined $this -> {'lsf_job_name'};
	$this -> {'lsf_ttl'} = defined $parm{'lsf_ttl'} ? $parm{'lsf_ttl'} : \'' unless defined $this -> {'lsf_ttl'};
	$this -> {'lsf_options'} = defined $parm{'lsf_options'} ? $parm{'lsf_options'} : \'' unless defined $this -> {'lsf_options'};
	$this -> {'run_on_torque'} = defined $parm{'run_on_torque'} ? $parm{'run_on_torque'} : \'1,0' unless defined $this -> {'run_on_torque'};

	bless $this, $class;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'file'} = $parm;
	} else {
		return $self -> {'file'};
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

sub base_criteria_values {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'base_criteria_values'} = $parm;
	} else {
		return $self -> {'base_criteria_values'};
	}
}

sub categorical_covariates {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'categorical_covariates'} = $parm;
	} else {
		return $self -> {'categorical_covariates'};
	}
}

sub error_code {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'error_code'} = $parm;
	} else {
		return $self -> {'error_code'};
	}
}

sub code {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'code'} = $parm;
	} else {
		return $self -> {'code'};
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

sub continuous_covariates {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'continuous_covariates'} = $parm;
	} else {
		return $self -> {'continuous_covariates'};
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

sub debug {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'debug'} = $parm;
	} else {
		return $self -> {'debug'};
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

sub do_not_drop {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'do_not_drop'} = $parm;
	} else {
		return $self -> {'do_not_drop'};
	}
}

sub extra_files {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'extra_files'} = $parm;
	} else {
		return $self -> {'extra_files'};
	}
}

sub global_init {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'global_init'} = $parm;
	} else {
		return $self -> {'global_init'};
	}
}

sub gof {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'gof'} = $parm;
	} else {
		return $self -> {'gof'};
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

sub included_relations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'included_relations'} = $parm;
	} else {
		return $self -> {'included_relations'};
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

sub lower_bounds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lower_bounds'} = $parm;
	} else {
		return $self -> {'lower_bounds'};
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

sub model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'model'} = $parm;
	} else {
		return $self -> {'model'};
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

sub ofv_backward {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ofv_backward'} = $parm;
	} else {
		return $self -> {'ofv_backward'};
	}
}

sub ofv_change {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ofv_change'} = $parm;
	} else {
		return $self -> {'ofv_change'};
	}
}

sub ofv_forward {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ofv_forward'} = $parm;
	} else {
		return $self -> {'ofv_forward'};
	}
}

sub p_backward {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'p_backward'} = $parm;
	} else {
		return $self -> {'p_backward'};
	}
}

sub p_forward {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'p_forward'} = $parm;
	} else {
		return $self -> {'p_forward'};
	}
}

sub p_value {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'p_value'} = $parm;
	} else {
		return $self -> {'p_value'};
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

sub relations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'relations'} = $parm;
	} else {
		return $self -> {'relations'};
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

sub linearize {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'linearize'} = $parm;
	} else {
		return $self -> {'linearize'};
	}
}

sub noabort {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'noabort'} = $parm;
	} else {
		return $self -> {'noabort'};
	}
}

sub max_steps {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'max_steps'} = $parm;
	} else {
		return $self -> {'max_steps'};
	}
}

sub epsilon {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'epsilon'} = $parm;
	} else {
		return $self -> {'epsilon'};
	}
}

sub only_successful {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'only_successful'} = $parm;
	} else {
		return $self -> {'only_successful'};
	}
}

sub parallel_states {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parallel_states'} = $parm;
	} else {
		return $self -> {'parallel_states'};
	}
}

sub update_derivatives {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'update_derivatives'} = $parm;
	} else {
		return $self -> {'update_derivatives'};
	}
}

sub error {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'error'} = $parm;
	} else {
		return $self -> {'error'};
	}
}

sub logit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'logit'} = $parm;
	} else {
		return $self -> {'logit'};
	}
}

sub time_varying {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'time_varying'} = $parm;
	} else {
		return $self -> {'time_varying'};
	}
}

sub second_order {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'second_order'} = $parm;
	} else {
		return $self -> {'second_order'};
	}
}

sub foce {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'foce'} = $parm;
	} else {
		return $self -> {'foce'};
	}
}

sub lst_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lst_file'} = $parm;
	} else {
		return $self -> {'lst_file'};
	}
}

sub derivatives_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'derivatives_data'} = $parm;
	} else {
		return $self -> {'derivatives_data'};
	}
}

sub search_direction {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'search_direction'} = $parm;
	} else {
		return $self -> {'search_direction'};
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

sub test_relations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'test_relations'} = $parm;
	} else {
		return $self -> {'test_relations'};
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

sub upper_bounds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'upper_bounds'} = $parm;
	} else {
		return $self -> {'upper_bounds'};
	}
}

sub valid_array_options {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'valid_array_options'} = $parm;
	} else {
		return $self -> {'valid_array_options'};
	}
}

sub valid_code_options {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'valid_code_options'} = $parm;
	} else {
		return $self -> {'valid_code_options'};
	}
}

sub valid_hash_options {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'valid_hash_options'} = $parm;
	} else {
		return $self -> {'valid_hash_options'};
	}
}

sub valid_scalar_options {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'valid_scalar_options'} = $parm;
	} else {
		return $self -> {'valid_scalar_options'};
	}
}

sub valid_states {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'valid_states'} = $parm;
	} else {
		return $self -> {'valid_states'};
	}
}

sub inits {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'inits'} = $parm;
	} else {
		return $self -> {'inits'};
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

sub add_file {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_file given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'files'}},
		file -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _check_various {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'slave' => 'SCALAR', 'master' => 'm_SCALAR',
			'header' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm::config_file->_check_various: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm::config_file->_check_various: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm::config_file->_check_various: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm::config_file->_check_various: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm::config_file->_check_various: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $slave = $parm{'slave'};
	my $master = $parm{'master'};
	my $header = $parm{'header'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _check_code {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _check_included_relations {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub write {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'filename' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm::config_file->write: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm::config_file->write: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm::config_file->write: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm::config_file->write: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm::config_file->write: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $filename = $parm{'filename'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub parse_config {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'config_tiny' => 'ext::config::tiny' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm::config_file->parse_config: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm::config_file->parse_config: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm::config_file->parse_config: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm::config_file->parse_config: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm::config_file->parse_config: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $config_tiny = $parm{'config_tiny'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

1;

