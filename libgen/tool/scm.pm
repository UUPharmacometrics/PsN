use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::scm;

#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(tool);

#---------------------------------------------------------------------
#         Used Packages
#---------------------------------------------------------------------
use tool::scm::config_file;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'config_file' => 'REF', 'append_log' => 'SCALAR',
			'base_criteria_values' => 'REF', 'format' => 'SCALAR',
			'medians' => 'REF', 'means' => 'REF', 'initial_estimates_model' => 'model',
			'derivatives_base_model' => 'model', 'filtered_data_model' => 'model',
			'derivatives_output' => 'output', 'update_derivatives' => 'SCALAR',
			'copy_data' => 'SCALAR', 'max_data_items' => 'SCALAR',
			'error' => 'SCALAR', 'best_step' => '', 'bounds' => 'HASH',
			'categorical_covariates' => 'ARRAY', 'error_code' => 'ARRAY',
			'config_file_name' => 'SCALAR', 'continuous_covariates' => 'ARRAY',
			'final_model_directory' => 'SCALAR', 'data_items' => 'SCALAR',
			'sizes_pd' => 'SCALAR', 'covariate_statistics' => 'REF',
			'global_covariate_statistics' => 'REF', 'do_not_drop' => 'ARRAY',
			'global_init' => 'SCALAR', 'gof' => 'SCALAR',
			'included_relations' => 'REF', 'logfile' => 'REF',
			'ofv_change' => 'REF', 'ofv_backward' => '',
			'p_value' => 'SCALAR', 'p_backward' => '', 'parameters' => 'ARRAY',
			'prev_best' => '', 'relations' => 'REF', 'resulting_model' => 'model',
			'max_steps' => 'SCALAR', 'linearize' => 'SCALAR',
			'basename' => 'SCALAR', 'noabort' => 'SCALAR',
			'skip_filtering' => 'SCALAR', 'xv_pred_data' => 'REF',
			'xv_results' => 'REF', 'xv_results_file' => 'SCALAR',
			'epsilon' => 'SCALAR', 'derivatives_data' => 'SCALAR',
			'have_Math_CDF' => 'SCALAR', 'have_run_included' => 'SCALAR',
			'run_linearized_base' => 'SCALAR', 'return_after_derivatives_done' => 'SCALAR',
			'only_successful' => 'SCALAR', 'parallel_states' => 'SCALAR',
			'logit' => 'ARRAY', 'sum_covariates_hash' => 'REF',
			'time_varying' => 'ARRAY', 'second_order' => 'SCALAR',
			'parameter_eta' => 'REF', 'parameter_relation' => 'REF',
			'foce' => 'SCALAR', 'lst_file' => 'SCALAR',
			'search_direction' => 'SCALAR', 'both_directions' => 'SCALAR',
			'step_number' => 'SCALAR', 'step_relations' => 'ARRAY',
			'test_relations' => 'REF', 'valid_states' => 'HASH',
			'work_queue' => 'ARRAY', 'covariate_statistics_file' => 'SCALAR',
			'relations_file' => 'SCALAR', 'short_logfile' => 'REF',
			'spec_code' => 'HASH' );

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
			'debug' -> die( message => "ERROR in tool::scm->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'append_log'} = defined $parm{'append_log'} ? $parm{'append_log'} : 0 unless defined $this -> {'append_log'};
	$this -> {'base_criteria_values'} = defined $parm{'base_criteria_values'} ? $parm{'base_criteria_values'} : {} unless defined $this -> {'base_criteria_values'};
	$this -> {'medians'} = defined $parm{'medians'} ? $parm{'medians'} : {} unless defined $this -> {'medians'};
	$this -> {'means'} = defined $parm{'means'} ? $parm{'means'} : {} unless defined $this -> {'means'};
	$this -> {'update_derivatives'} = defined $parm{'update_derivatives'} ? $parm{'update_derivatives'} : 0 unless defined $this -> {'update_derivatives'};
	$this -> {'copy_data'} = defined $parm{'copy_data'} ? $parm{'copy_data'} : 0 unless defined $this -> {'copy_data'};
	$this -> {'max_data_items'} = defined $parm{'max_data_items'} ? $parm{'max_data_items'} : 50 unless defined $this -> {'max_data_items'};
	$this -> {'data_items'} = defined $parm{'data_items'} ? $parm{'data_items'} : 0 unless defined $this -> {'data_items'};
	$this -> {'sizes_pd'} = defined $parm{'sizes_pd'} ? $parm{'sizes_pd'} : 0 unless defined $this -> {'sizes_pd'};
	$this -> {'global_init'} = defined $parm{'global_init'} ? $parm{'global_init'} : 0.001 unless defined $this -> {'global_init'};
	$this -> {'gof'} = defined $parm{'gof'} ? $parm{'gof'} : 'p_value' unless defined $this -> {'gof'};
	$this -> {'included_relations'} = defined $parm{'included_relations'} ? $parm{'included_relations'} : {} unless defined $this -> {'included_relations'};
	$this -> {'logfile'} = defined $parm{'logfile'} ? $parm{'logfile'} : ['scmlog.txt'] unless defined $this -> {'logfile'};
	$this -> {'relations'} = defined $parm{'relations'} ? $parm{'relations'} : {} unless defined $this -> {'relations'};
	$this -> {'linearize'} = defined $parm{'linearize'} ? $parm{'linearize'} : 0 unless defined $this -> {'linearize'};
	$this -> {'noabort'} = defined $parm{'noabort'} ? $parm{'noabort'} : 0 unless defined $this -> {'noabort'};
	$this -> {'skip_filtering'} = defined $parm{'skip_filtering'} ? $parm{'skip_filtering'} : 0 unless defined $this -> {'skip_filtering'};
	$this -> {'epsilon'} = defined $parm{'epsilon'} ? $parm{'epsilon'} : 1 unless defined $this -> {'epsilon'};
	$this -> {'have_Math_CDF'} = defined $parm{'have_Math_CDF'} ? $parm{'have_Math_CDF'} : 0 unless defined $this -> {'have_Math_CDF'};
	$this -> {'have_run_included'} = defined $parm{'have_run_included'} ? $parm{'have_run_included'} : 0 unless defined $this -> {'have_run_included'};
	$this -> {'run_linearized_base'} = defined $parm{'run_linearized_base'} ? $parm{'run_linearized_base'} : 1 unless defined $this -> {'run_linearized_base'};
	$this -> {'return_after_derivatives_done'} = defined $parm{'return_after_derivatives_done'} ? $parm{'return_after_derivatives_done'} : 0 unless defined $this -> {'return_after_derivatives_done'};
	$this -> {'only_successful'} = defined $parm{'only_successful'} ? $parm{'only_successful'} : 0 unless defined $this -> {'only_successful'};
	$this -> {'parallel_states'} = defined $parm{'parallel_states'} ? $parm{'parallel_states'} : 0 unless defined $this -> {'parallel_states'};
	$this -> {'sum_covariates_hash'} = defined $parm{'sum_covariates_hash'} ? $parm{'sum_covariates_hash'} : {} unless defined $this -> {'sum_covariates_hash'};
	$this -> {'second_order'} = defined $parm{'second_order'} ? $parm{'second_order'} : 0 unless defined $this -> {'second_order'};
	$this -> {'foce'} = defined $parm{'foce'} ? $parm{'foce'} : 1 unless defined $this -> {'foce'};
	$this -> {'both_directions'} = defined $parm{'both_directions'} ? $parm{'both_directions'} : 0 unless defined $this -> {'both_directions'};
	$this -> {'step_number'} = defined $parm{'step_number'} ? $parm{'step_number'} : 1 unless defined $this -> {'step_number'};
	$this -> {'step_relations'} = defined $parm{'step_relations'} ? $parm{'step_relations'} : [] unless defined $this -> {'step_relations'};
	$this -> {'valid_states'} = defined $parm{'valid_states'} ? $parm{'valid_states'} : {'continuous' => [1,2,3], 'categorical' => [1,2]} unless defined $this -> {'valid_states'};
	$this -> {'covariate_statistics_file'} = defined $parm{'covariate_statistics_file'} ? $parm{'covariate_statistics_file'} : 'covariate_statistics.txt' unless defined $this -> {'covariate_statistics_file'};
	$this -> {'relations_file'} = defined $parm{'relations_file'} ? $parm{'relations_file'} : 'relations.txt' unless defined $this -> {'relations_file'};
	$this -> {'short_logfile'} = defined $parm{'short_logfile'} ? $parm{'short_logfile'} : ['short_scmlog.txt'] unless defined $this -> {'short_logfile'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub config_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'config_file'} = $parm;
	} else {
		return $self -> {'config_file'};
	}
}

sub append_log {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'append_log'} = $parm;
	} else {
		return $self -> {'append_log'};
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

sub format {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'format'} = $parm;
	} else {
		return $self -> {'format'};
	}
}

sub medians {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'medians'} = $parm;
	} else {
		return $self -> {'medians'};
	}
}

sub means {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'means'} = $parm;
	} else {
		return $self -> {'means'};
	}
}

sub initial_estimates_model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'initial_estimates_model'} = $parm;
	} else {
		return $self -> {'initial_estimates_model'};
	}
}

sub derivatives_base_model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'derivatives_base_model'} = $parm;
	} else {
		return $self -> {'derivatives_base_model'};
	}
}

sub filtered_data_model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'filtered_data_model'} = $parm;
	} else {
		return $self -> {'filtered_data_model'};
	}
}

sub derivatives_output {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'derivatives_output'} = $parm;
	} else {
		return $self -> {'derivatives_output'};
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

sub max_data_items {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'max_data_items'} = $parm;
	} else {
		return $self -> {'max_data_items'};
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

sub best_step {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'best_step'} = $parm;
	} else {
		return $self -> {'best_step'};
	}
}

sub bounds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bounds'} = $parm;
	} else {
		return $self -> {'bounds'};
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

sub config_file_name {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'config_file_name'} = $parm;
	} else {
		return $self -> {'config_file_name'};
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

sub final_model_directory {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'final_model_directory'} = $parm;
	} else {
		return $self -> {'final_model_directory'};
	}
}

sub data_items {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'data_items'} = $parm;
	} else {
		return $self -> {'data_items'};
	}
}

sub sizes_pd {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sizes_pd'} = $parm;
	} else {
		return $self -> {'sizes_pd'};
	}
}

sub covariate_statistics {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariate_statistics'} = $parm;
	} else {
		return $self -> {'covariate_statistics'};
	}
}

sub global_covariate_statistics {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'global_covariate_statistics'} = $parm;
	} else {
		return $self -> {'global_covariate_statistics'};
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

sub prev_best {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prev_best'} = $parm;
	} else {
		return $self -> {'prev_best'};
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

sub resulting_model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'resulting_model'} = $parm;
	} else {
		return $self -> {'resulting_model'};
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

sub basename {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'basename'} = $parm;
	} else {
		return $self -> {'basename'};
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

sub skip_filtering {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'skip_filtering'} = $parm;
	} else {
		return $self -> {'skip_filtering'};
	}
}

sub xv_pred_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'xv_pred_data'} = $parm;
	} else {
		return $self -> {'xv_pred_data'};
	}
}

sub xv_results {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'xv_results'} = $parm;
	} else {
		return $self -> {'xv_results'};
	}
}

sub xv_results_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'xv_results_file'} = $parm;
	} else {
		return $self -> {'xv_results_file'};
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

sub have_Math_CDF {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'have_Math_CDF'} = $parm;
	} else {
		return $self -> {'have_Math_CDF'};
	}
}

sub have_run_included {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'have_run_included'} = $parm;
	} else {
		return $self -> {'have_run_included'};
	}
}

sub run_linearized_base {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_linearized_base'} = $parm;
	} else {
		return $self -> {'run_linearized_base'};
	}
}

sub return_after_derivatives_done {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'return_after_derivatives_done'} = $parm;
	} else {
		return $self -> {'return_after_derivatives_done'};
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

sub sum_covariates_hash {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sum_covariates_hash'} = $parm;
	} else {
		return $self -> {'sum_covariates_hash'};
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

sub parameter_eta {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parameter_eta'} = $parm;
	} else {
		return $self -> {'parameter_eta'};
	}
}

sub parameter_relation {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parameter_relation'} = $parm;
	} else {
		return $self -> {'parameter_relation'};
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

sub both_directions {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'both_directions'} = $parm;
	} else {
		return $self -> {'both_directions'};
	}
}

sub step_number {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'step_number'} = $parm;
	} else {
		return $self -> {'step_number'};
	}
}

sub step_relations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'step_relations'} = $parm;
	} else {
		return $self -> {'step_relations'};
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

sub work_queue {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'work_queue'} = $parm;
	} else {
		return $self -> {'work_queue'};
	}
}

sub covariate_statistics_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariate_statistics_file'} = $parm;
	} else {
		return $self -> {'covariate_statistics_file'};
	}
}

sub relations_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'relations_file'} = $parm;
	} else {
		return $self -> {'relations_file'};
	}
}

sub short_logfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'short_logfile'} = $parm;
	} else {
		return $self -> {'short_logfile'};
	}
}

sub add_config_file {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_config_file given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'config_files'}},
		tool::scm::config_file -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_scm_file {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _raw_results_callback {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->_raw_results_callback: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->_raw_results_callback: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->_raw_results_callback: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \&subroutine;
}

sub modelfit_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->modelfit_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->modelfit_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->modelfit_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub linearize_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'original_model' => 'model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->linearize_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->linearize_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->linearize_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->linearize_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->linearize_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $original_model = $parm{'original_model'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $original_model;
}

sub modelfit_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->modelfit_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->modelfit_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->modelfit_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub gof_ofv {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub gof_pval {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_categorical_statistics {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'data' => 'data', 'model' => 'model', 'covariate' => 'm_SCALAR',
			'column_number' => 'm_SCALAR', 'factors' => 'HASH',
			'have_missing_data' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->calculate_categorical_statistics: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->calculate_categorical_statistics: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->calculate_categorical_statistics: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->calculate_categorical_statistics: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->calculate_categorical_statistics: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $data = $parm{'data'};
	my $model = $parm{'model'};
	my $covariate = $parm{'covariate'};
	my $column_number = $parm{'column_number'};
	my %factors = defined $parm{'factors'} ? %{$parm{'factors'}} : {};
	my $have_missing_data = $parm{'have_missing_data'};
	my $median;
	my $min;
	my $max;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $median ,$min ,$max;
}

sub calculate_continuous_statistics {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'data' => 'data', 'model' => 'model', 'covariate' => 'm_SCALAR',
			'column_number' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->calculate_continuous_statistics: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->calculate_continuous_statistics: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->calculate_continuous_statistics: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->calculate_continuous_statistics: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->calculate_continuous_statistics: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $data = $parm{'data'};
	my $model = $parm{'model'};
	my $covariate = $parm{'covariate'};
	my $column_number = $parm{'column_number'};
	my $median;
	my $min;
	my $max;
	my $mean;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $median ,$min ,$max ,$mean;
}

sub _create_models {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'parallel_states' => 'SCALAR',
			'orig_model' => 'model', 'initial_estimates_model' => 'model',
			'relations' => 'HASH', 'included_relations' => 'HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->_create_models: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->_create_models: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->_create_models: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->_create_models: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->_create_models: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $parallel_states = defined $parm{'parallel_states'} ? $parm{'parallel_states'} : 0;
	my $orig_model = $parm{'orig_model'};
	my $initial_estimates_model = $parm{'initial_estimates_model'};
	my %relations = defined $parm{'relations'} ? %{$parm{'relations'}} : {};
	my %included_relations = defined $parm{'included_relations'} ? %{$parm{'included_relations'}} : {};
	my @new_models;
	my @step_relations;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@new_models ,\@step_relations;
}

sub create_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'state' => 'SCALAR', 'start_theta' => 'SCALAR',
			'model_number' => 'SCALAR', 'parameter' => 'SCALAR',
			'covariate' => 'SCALAR', 'continuous' => 'SCALAR',
			'statistics' => 'HASH', 'missing_data_token' => 'SCALAR',
			'sum_covariates' => 'SCALAR', 'code' => 'ARRAY',
			'inits' => 'ARRAY', 'bounds' => 'HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->create_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->create_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->create_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $state = $parm{'state'};
	my $start_theta = $parm{'start_theta'};
	my $model_number = $parm{'model_number'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : {};
	my $missing_data_token = $parm{'missing_data_token'};
	my $sum_covariates = defined $parm{'sum_covariates'} ? $parm{'sum_covariates'} : 0;
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub add_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter' => 'SCALAR', 'covariate' => 'SCALAR',
			'sum_covariates' => 'SCALAR', 'nthetas' => 'SCALAR',
			'definition_code' => 'ARRAY', 'bounds' => 'HASH',
			'inits' => 'ARRAY', 'applicant_model' => 'model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->add_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->add_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->add_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->add_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->add_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $sum_covariates = defined $parm{'sum_covariates'} ? $parm{'sum_covariates'} : 0;
	my $nthetas = $parm{'nthetas'};
	my @definition_code = defined $parm{'definition_code'} ? @{$parm{'definition_code'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : {};
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my $applicant_model = $parm{'applicant_model'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $applicant_model;
}

sub add_code_linearize {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter' => 'SCALAR', 'covariate' => 'SCALAR',
			'sum_covariates' => 'SCALAR', 'nthetas' => 'SCALAR',
			'definition_code' => 'ARRAY', 'bounds' => 'HASH',
			'inits' => 'ARRAY', 'applicant_model' => 'model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->add_code_linearize: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->add_code_linearize: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->add_code_linearize: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->add_code_linearize: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->add_code_linearize: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $sum_covariates = defined $parm{'sum_covariates'} ? $parm{'sum_covariates'} : 0;
	my $nthetas = $parm{'nthetas'};
	my @definition_code = defined $parm{'definition_code'} ? @{$parm{'definition_code'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : {};
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my $applicant_model = $parm{'applicant_model'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $applicant_model;
}

sub add_code_gfunc {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_G' => 'HASH', 'parameter_relation' => 'HASH',
			'applicant_model' => 'model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->add_code_gfunc: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->add_code_gfunc: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->add_code_gfunc: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->add_code_gfunc: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->add_code_gfunc: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my %parameter_G = defined $parm{'parameter_G'} ? %{$parm{'parameter_G'}} : {};
	my %parameter_relation = defined $parm{'parameter_relation'} ? %{$parm{'parameter_relation'}} : {};
	my $applicant_model = $parm{'applicant_model'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $applicant_model;
}

sub run_xv_pred_step {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'estimation_model' => 'model', 'derivatives_run' => 'SCALAR',
			'model_name' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->run_xv_pred_step: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->run_xv_pred_step: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->run_xv_pred_step: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->run_xv_pred_step: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->run_xv_pred_step: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $estimation_model = $parm{'estimation_model'};
	my $derivatives_run = defined $parm{'derivatives_run'} ? $parm{'derivatives_run'} : 0;
	my $model_name = $parm{'model_name'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub format_inits_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'string' => 'm_SCALAR', 'continuous' => 'm_SCALAR',
			'is_bound' => 'm_SCALAR', 'statistics' => 'm_HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->format_inits_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->format_inits_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->format_inits_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->format_inits_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->format_inits_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $string = $parm{'string'};
	my $continuous = $parm{'continuous'};
	my $is_bound = $parm{'is_bound'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : {};
	my $value;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $value;
}

sub create_linear_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'start_theta' => 'SCALAR', 'parameter' => 'SCALAR',
			'covariate' => 'SCALAR', 'continuous' => 'SCALAR',
			'statistics' => 'HASH', 'sum_covariates' => 'SCALAR',
			'missing_data_token' => 'SCALAR', 'code' => 'ARRAY',
			'inits' => 'ARRAY', 'bounds' => 'HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->create_linear_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->create_linear_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->create_linear_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_linear_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_linear_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : {};
	my $sum_covariates = defined $parm{'sum_covariates'} ? $parm{'sum_covariates'} : 0;
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub create_hockey_stick_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'start_theta' => 'SCALAR', 'parameter' => 'SCALAR',
			'covariate' => 'SCALAR', 'continuous' => 'SCALAR',
			'statistics' => 'HASH', 'sum_covariates' => 'SCALAR',
			'missing_data_token' => 'SCALAR', 'code' => 'ARRAY',
			'inits' => 'ARRAY', 'bounds' => 'HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->create_hockey_stick_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->create_hockey_stick_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->create_hockey_stick_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_hockey_stick_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_hockey_stick_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : {};
	my $sum_covariates = defined $parm{'sum_covariates'} ? $parm{'sum_covariates'} : 0;
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub write_log {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub drop_undrop_covariates {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'applicant_model' => 'model', 'used_covariates' => 'ARRAY',
			'all_covariates' => 'ARRAY', 'do_not_drop' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->drop_undrop_covariates: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->drop_undrop_covariates: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->drop_undrop_covariates: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->drop_undrop_covariates: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->drop_undrop_covariates: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $applicant_model = $parm{'applicant_model'};
	my @used_covariates = defined $parm{'used_covariates'} ? @{$parm{'used_covariates'}} : ();
	my @all_covariates = defined $parm{'all_covariates'} ? @{$parm{'all_covariates'}} : ();
	my @do_not_drop = defined $parm{'do_not_drop'} ? @{$parm{'do_not_drop'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $applicant_model;
}

sub write_final_models {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'final_model' => 'model', 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->write_final_models: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->write_final_models: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->write_final_models: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->write_final_models: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->write_final_models: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $final_model = $parm{'final_model'};
	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $final_model;
}

sub modelfit_post_fork_analyze {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub read_config_file {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub create_exponential_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'start_theta' => 'SCALAR', 'parameter' => 'SCALAR',
			'covariate' => 'SCALAR', 'continuous' => 'SCALAR',
			'statistics' => 'HASH', 'sum_covariates' => 'SCALAR',
			'missing_data_token' => 'SCALAR', 'code' => 'ARRAY',
			'inits' => 'ARRAY', 'bounds' => 'HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->create_exponential_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->create_exponential_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->create_exponential_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_exponential_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_exponential_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : {};
	my $sum_covariates = defined $parm{'sum_covariates'} ? $parm{'sum_covariates'} : 0;
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub create_power_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'start_theta' => 'SCALAR', 'parameter' => 'SCALAR',
			'covariate' => 'SCALAR', 'continuous' => 'SCALAR',
			'statistics' => 'HASH', 'sum_covariates' => 'SCALAR',
			'missing_data_token' => 'SCALAR', 'code' => 'ARRAY',
			'inits' => 'ARRAY', 'bounds' => 'HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->create_power_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->create_power_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->create_power_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_power_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_power_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : {};
	my $sum_covariates = defined $parm{'sum_covariates'} ? $parm{'sum_covariates'} : 0;
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub create_user_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'start_theta' => 'SCALAR', 'parameter' => 'SCALAR',
			'covariate' => 'SCALAR', 'continuous' => 'SCALAR',
			'statistics' => 'HASH', 'sum_covariates' => 'SCALAR',
			'missing_data_token' => 'SCALAR', 'code' => 'ARRAY',
			'inits' => 'ARRAY', 'bounds' => 'HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->create_user_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->create_user_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->create_user_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_user_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_user_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : {};
	my $sum_covariates = defined $parm{'sum_covariates'} ? $parm{'sum_covariates'} : 0;
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub create_state1_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'start_theta' => 'SCALAR', 'parameter' => 'SCALAR',
			'covariate' => 'SCALAR', 'continuous' => 'SCALAR',
			'statistics' => 'HASH', 'sum_covariates' => 'SCALAR',
			'missing_data_token' => 'SCALAR', 'code' => 'ARRAY',
			'inits' => 'ARRAY', 'bounds' => 'HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->create_state1_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->create_state1_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->create_state1_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_state1_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->create_state1_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $start_theta = $parm{'start_theta'};
	my $parameter = $parm{'parameter'};
	my $covariate = $parm{'covariate'};
	my $continuous = $parm{'continuous'};
	my %statistics = defined $parm{'statistics'} ? %{$parm{'statistics'}} : {};
	my $sum_covariates = defined $parm{'sum_covariates'} ? $parm{'sum_covariates'} : 0;
	my $missing_data_token = $parm{'missing_data_token'};
	my @code = defined $parm{'code'} ? @{$parm{'code'}} : ();
	my $end_theta;
	my @inits = defined $parm{'inits'} ? @{$parm{'inits'}} : ();
	my %bounds = defined $parm{'bounds'} ? %{$parm{'bounds'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@code ,$end_theta ,\@inits ,\%bounds;
}

sub prepare_raw_results {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->prepare_raw_results: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->prepare_raw_results: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->prepare_raw_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->prepare_raw_results: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->prepare_raw_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub preprocess_data {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'REF', 'filter' => 'SCALAR', 'test_relations' => 'REF',
			'time_varying' => 'REF', 'directory' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->preprocess_data: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->preprocess_data: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->preprocess_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->preprocess_data: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->preprocess_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model = $parm{'model'};
	my $filter = $parm{'filter'};
	my $test_relations = $parm{'test_relations'};
	my $time_varying = $parm{'time_varying'};
	my $directory = $parm{'directory'};
	my $filtered_data_model;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $filtered_data_model;
}

sub register_in_database {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'force' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::scm->register_in_database: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::scm->register_in_database: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::scm->register_in_database: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->register_in_database: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::scm->register_in_database: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $force = defined $parm{'force'} ? $parm{'force'} : 0;
	my $scm_id;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $scm_id;
}

1;

