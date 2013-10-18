use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::npc;

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
	my %valid_parm = ( 'is_vpc' => 'SCALAR', 'searchdir' => 'SCALAR',
			'refstrat' => 'SCALAR', 'fine_pi' => 'SCALAR',
			'confidence_interval' => 'SCALAR', 'covariance_file' => 'SCALAR',
			'rawres_input' => 'SCALAR', 'offset_rawres' => 'SCALAR',
			'have_nwpri' => 'SCALAR', 'have_tnpri' => 'SCALAR',
			'copy_data' => 'SCALAR', 'simprobnum' => 'SCALAR',
			'origprobnum' => 'SCALAR', 'boxcox_lambda' => 'SCALAR',
			'auto_bin_mode' => 'SCALAR', 'min_no_bins' => 'SCALAR',
			'max_no_bins' => 'SCALAR', 'min_points_in_bin' => 'SCALAR',
			'predcorr' => 'SCALAR', 'lnDV' => 'SCALAR',
			'lower_bound' => 'SCALAR', 'bound_variable' => 'SCALAR',
			'npc_alert_written' => 'SCALAR', 'detection_censored' => 'SCALAR',
			'run_the_original' => 'SCALAR', 'run_the_sim' => 'SCALAR',
			'data_matrix' => 'REF', 'censor_data_matrix' => 'REF',
			'n_simulations' => 'SCALAR', 'n_observations' => 'SCALAR',
			'strata_matrix' => 'REF', 'strata_labels' => 'REF',
			'strata_variable_vector' => 'REF', 'stratified_data' => 'REF',
			'idv_array' => 'REF', 'pred_array' => 'REF',
			'bound_array' => 'REF', 'id_array' => 'REF',
			'binned_data' => 'REF', 'censor_binned_data' => 'REF',
			'bin_ceilings' => 'REF', 'bin_floors' => 'REF',
			'binned_id' => 'REF', 'binned_idv' => 'REF',
			'binned_strt' => 'REF', 'vpctab_filename' => 'SCALAR',
			'mirror_labels' => 'REF', 'mirror_set' => 'REF',
			'vpctab_header' => 'SCALAR', 'censor_stratified_data' => 'REF',
			'varcorr' => 'SCALAR', 'noprediction' => 'SCALAR',
			'samples' => 'SCALAR', 'extra_table_parameters' => 'REF',
			'dv' => 'SCALAR', 'orig_table' => 'SCALAR',
			'sim_table' => 'SCALAR', 'keep_estimation' => 'SCALAR',
			'dv_table_name' => 'SCALAR', 'n_simulation_models' => 'SCALAR',
			'idv' => 'SCALAR', 'bin_by_count' => 'SCALAR',
			'no_of_bins' => 'SCALAR', 'single_bin_size' => 'SCALAR',
			'overlap_percent' => 'SCALAR', 'bin_array' => 'ARRAY',
			'categorized' => 'SCALAR', 'levels' => 'ARRAY',
			'lloq' => 'SCALAR', 'uloq' => 'SCALAR', 'stratify_on' => 'SCALAR',
			'censor' => 'SCALAR', 'tte' => 'SCALAR', 'sim_model' => 'SCALAR',
			'flip_comments' => 'SCALAR', 'no_of_strata' => 'SCALAR',
			'lst_file' => 'SCALAR', 'msfo_file' => 'SCALAR',
			'mirrors' => 'SCALAR', 'simulation_models' => 'REF',
			'original_model' => 'model', 'logfile' => 'REF',
			'results_file' => 'SCALAR' );

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
			'debug' -> die( message => "ERROR in tool::npc->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'is_vpc'} = defined $parm{'is_vpc'} ? $parm{'is_vpc'} : 0 unless defined $this -> {'is_vpc'};
	$this -> {'fine_pi'} = defined $parm{'fine_pi'} ? $parm{'fine_pi'} : 0 unless defined $this -> {'fine_pi'};
	$this -> {'confidence_interval'} = defined $parm{'confidence_interval'} ? $parm{'confidence_interval'} : 95 unless defined $this -> {'confidence_interval'};
	$this -> {'offset_rawres'} = defined $parm{'offset_rawres'} ? $parm{'offset_rawres'} : 1 unless defined $this -> {'offset_rawres'};
	$this -> {'have_nwpri'} = defined $parm{'have_nwpri'} ? $parm{'have_nwpri'} : 0 unless defined $this -> {'have_nwpri'};
	$this -> {'have_tnpri'} = defined $parm{'have_tnpri'} ? $parm{'have_tnpri'} : 0 unless defined $this -> {'have_tnpri'};
	$this -> {'copy_data'} = defined $parm{'copy_data'} ? $parm{'copy_data'} : 1 unless defined $this -> {'copy_data'};
	$this -> {'simprobnum'} = defined $parm{'simprobnum'} ? $parm{'simprobnum'} : 1 unless defined $this -> {'simprobnum'};
	$this -> {'origprobnum'} = defined $parm{'origprobnum'} ? $parm{'origprobnum'} : 1 unless defined $this -> {'origprobnum'};
	$this -> {'boxcox_lambda'} = defined $parm{'boxcox_lambda'} ? $parm{'boxcox_lambda'} : 0 unless defined $this -> {'boxcox_lambda'};
	$this -> {'predcorr'} = defined $parm{'predcorr'} ? $parm{'predcorr'} : 0 unless defined $this -> {'predcorr'};
	$this -> {'lnDV'} = defined $parm{'lnDV'} ? $parm{'lnDV'} : 0 unless defined $this -> {'lnDV'};
	$this -> {'strata_matrix'} = defined $parm{'strata_matrix'} ? $parm{'strata_matrix'} : [] unless defined $this -> {'strata_matrix'};
	$this -> {'strata_labels'} = defined $parm{'strata_labels'} ? $parm{'strata_labels'} : [] unless defined $this -> {'strata_labels'};
	$this -> {'strata_variable_vector'} = defined $parm{'strata_variable_vector'} ? $parm{'strata_variable_vector'} : [] unless defined $this -> {'strata_variable_vector'};
	$this -> {'stratified_data'} = defined $parm{'stratified_data'} ? $parm{'stratified_data'} : [] unless defined $this -> {'stratified_data'};
	$this -> {'idv_array'} = defined $parm{'idv_array'} ? $parm{'idv_array'} : [] unless defined $this -> {'idv_array'};
	$this -> {'pred_array'} = defined $parm{'pred_array'} ? $parm{'pred_array'} : [] unless defined $this -> {'pred_array'};
	$this -> {'bound_array'} = defined $parm{'bound_array'} ? $parm{'bound_array'} : [] unless defined $this -> {'bound_array'};
	$this -> {'id_array'} = defined $parm{'id_array'} ? $parm{'id_array'} : [] unless defined $this -> {'id_array'};
	$this -> {'binned_data'} = defined $parm{'binned_data'} ? $parm{'binned_data'} : [] unless defined $this -> {'binned_data'};
	$this -> {'bin_ceilings'} = defined $parm{'bin_ceilings'} ? $parm{'bin_ceilings'} : [] unless defined $this -> {'bin_ceilings'};
	$this -> {'bin_floors'} = defined $parm{'bin_floors'} ? $parm{'bin_floors'} : [] unless defined $this -> {'bin_floors'};
	$this -> {'binned_id'} = defined $parm{'binned_id'} ? $parm{'binned_id'} : [] unless defined $this -> {'binned_id'};
	$this -> {'binned_idv'} = defined $parm{'binned_idv'} ? $parm{'binned_idv'} : [] unless defined $this -> {'binned_idv'};
	$this -> {'binned_strt'} = defined $parm{'binned_strt'} ? $parm{'binned_strt'} : [] unless defined $this -> {'binned_strt'};
	$this -> {'mirror_labels'} = defined $parm{'mirror_labels'} ? $parm{'mirror_labels'} : [] unless defined $this -> {'mirror_labels'};
	$this -> {'mirror_set'} = defined $parm{'mirror_set'} ? $parm{'mirror_set'} : [] unless defined $this -> {'mirror_set'};
	$this -> {'varcorr'} = defined $parm{'varcorr'} ? $parm{'varcorr'} : 0 unless defined $this -> {'varcorr'};
	$this -> {'noprediction'} = defined $parm{'noprediction'} ? $parm{'noprediction'} : 0 unless defined $this -> {'noprediction'};
	$this -> {'dv'} = defined $parm{'dv'} ? $parm{'dv'} : 'DV' unless defined $this -> {'dv'};
	$this -> {'keep_estimation'} = defined $parm{'keep_estimation'} ? $parm{'keep_estimation'} : 0 unless defined $this -> {'keep_estimation'};
	$this -> {'n_simulation_models'} = defined $parm{'n_simulation_models'} ? $parm{'n_simulation_models'} : 1 unless defined $this -> {'n_simulation_models'};
	$this -> {'categorized'} = defined $parm{'categorized'} ? $parm{'categorized'} : 0 unless defined $this -> {'categorized'};
	$this -> {'flip_comments'} = defined $parm{'flip_comments'} ? $parm{'flip_comments'} : 0 unless defined $this -> {'flip_comments'};
	$this -> {'logfile'} = defined $parm{'logfile'} ? $parm{'logfile'} : ['npc.log'] unless defined $this -> {'logfile'};
	$this -> {'results_file'} = defined $parm{'results_file'} ? $parm{'results_file'} : 'npc_results.csv' unless defined $this -> {'results_file'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub is_vpc {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'is_vpc'} = $parm;
	} else {
		return $self -> {'is_vpc'};
	}
}

sub searchdir {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'searchdir'} = $parm;
	} else {
		return $self -> {'searchdir'};
	}
}

sub refstrat {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'refstrat'} = $parm;
	} else {
		return $self -> {'refstrat'};
	}
}

sub fine_pi {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'fine_pi'} = $parm;
	} else {
		return $self -> {'fine_pi'};
	}
}

sub confidence_interval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'confidence_interval'} = $parm;
	} else {
		return $self -> {'confidence_interval'};
	}
}

sub covariance_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariance_file'} = $parm;
	} else {
		return $self -> {'covariance_file'};
	}
}

sub rawres_input {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'rawres_input'} = $parm;
	} else {
		return $self -> {'rawres_input'};
	}
}

sub offset_rawres {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'offset_rawres'} = $parm;
	} else {
		return $self -> {'offset_rawres'};
	}
}

sub have_nwpri {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'have_nwpri'} = $parm;
	} else {
		return $self -> {'have_nwpri'};
	}
}

sub have_tnpri {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'have_tnpri'} = $parm;
	} else {
		return $self -> {'have_tnpri'};
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

sub simprobnum {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'simprobnum'} = $parm;
	} else {
		return $self -> {'simprobnum'};
	}
}

sub origprobnum {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'origprobnum'} = $parm;
	} else {
		return $self -> {'origprobnum'};
	}
}

sub boxcox_lambda {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'boxcox_lambda'} = $parm;
	} else {
		return $self -> {'boxcox_lambda'};
	}
}

sub auto_bin_mode {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'auto_bin_mode'} = $parm;
	} else {
		return $self -> {'auto_bin_mode'};
	}
}

sub min_no_bins {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'min_no_bins'} = $parm;
	} else {
		return $self -> {'min_no_bins'};
	}
}

sub max_no_bins {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'max_no_bins'} = $parm;
	} else {
		return $self -> {'max_no_bins'};
	}
}

sub min_points_in_bin {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'min_points_in_bin'} = $parm;
	} else {
		return $self -> {'min_points_in_bin'};
	}
}

sub predcorr {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'predcorr'} = $parm;
	} else {
		return $self -> {'predcorr'};
	}
}

sub lnDV {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lnDV'} = $parm;
	} else {
		return $self -> {'lnDV'};
	}
}

sub lower_bound {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lower_bound'} = $parm;
	} else {
		return $self -> {'lower_bound'};
	}
}

sub bound_variable {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bound_variable'} = $parm;
	} else {
		return $self -> {'bound_variable'};
	}
}

sub npc_alert_written {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'npc_alert_written'} = $parm;
	} else {
		return $self -> {'npc_alert_written'};
	}
}

sub detection_censored {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'detection_censored'} = $parm;
	} else {
		return $self -> {'detection_censored'};
	}
}

sub run_the_original {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_the_original'} = $parm;
	} else {
		return $self -> {'run_the_original'};
	}
}

sub run_the_sim {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_the_sim'} = $parm;
	} else {
		return $self -> {'run_the_sim'};
	}
}

sub data_matrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'data_matrix'} = $parm;
	} else {
		return $self -> {'data_matrix'};
	}
}

sub censor_data_matrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'censor_data_matrix'} = $parm;
	} else {
		return $self -> {'censor_data_matrix'};
	}
}

sub n_simulations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'n_simulations'} = $parm;
	} else {
		return $self -> {'n_simulations'};
	}
}

sub n_observations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'n_observations'} = $parm;
	} else {
		return $self -> {'n_observations'};
	}
}

sub strata_matrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'strata_matrix'} = $parm;
	} else {
		return $self -> {'strata_matrix'};
	}
}

sub strata_labels {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'strata_labels'} = $parm;
	} else {
		return $self -> {'strata_labels'};
	}
}

sub strata_variable_vector {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'strata_variable_vector'} = $parm;
	} else {
		return $self -> {'strata_variable_vector'};
	}
}

sub stratified_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'stratified_data'} = $parm;
	} else {
		return $self -> {'stratified_data'};
	}
}

sub idv_array {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'idv_array'} = $parm;
	} else {
		return $self -> {'idv_array'};
	}
}

sub pred_array {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'pred_array'} = $parm;
	} else {
		return $self -> {'pred_array'};
	}
}

sub bound_array {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bound_array'} = $parm;
	} else {
		return $self -> {'bound_array'};
	}
}

sub id_array {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'id_array'} = $parm;
	} else {
		return $self -> {'id_array'};
	}
}

sub binned_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'binned_data'} = $parm;
	} else {
		return $self -> {'binned_data'};
	}
}

sub censor_binned_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'censor_binned_data'} = $parm;
	} else {
		return $self -> {'censor_binned_data'};
	}
}

sub bin_ceilings {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bin_ceilings'} = $parm;
	} else {
		return $self -> {'bin_ceilings'};
	}
}

sub bin_floors {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bin_floors'} = $parm;
	} else {
		return $self -> {'bin_floors'};
	}
}

sub binned_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'binned_id'} = $parm;
	} else {
		return $self -> {'binned_id'};
	}
}

sub binned_idv {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'binned_idv'} = $parm;
	} else {
		return $self -> {'binned_idv'};
	}
}

sub binned_strt {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'binned_strt'} = $parm;
	} else {
		return $self -> {'binned_strt'};
	}
}

sub vpctab_filename {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'vpctab_filename'} = $parm;
	} else {
		return $self -> {'vpctab_filename'};
	}
}

sub mirror_labels {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'mirror_labels'} = $parm;
	} else {
		return $self -> {'mirror_labels'};
	}
}

sub mirror_set {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'mirror_set'} = $parm;
	} else {
		return $self -> {'mirror_set'};
	}
}

sub vpctab_header {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'vpctab_header'} = $parm;
	} else {
		return $self -> {'vpctab_header'};
	}
}

sub censor_stratified_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'censor_stratified_data'} = $parm;
	} else {
		return $self -> {'censor_stratified_data'};
	}
}

sub varcorr {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'varcorr'} = $parm;
	} else {
		return $self -> {'varcorr'};
	}
}

sub noprediction {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'noprediction'} = $parm;
	} else {
		return $self -> {'noprediction'};
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

sub extra_table_parameters {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'extra_table_parameters'} = $parm;
	} else {
		return $self -> {'extra_table_parameters'};
	}
}

sub dv {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'dv'} = $parm;
	} else {
		return $self -> {'dv'};
	}
}

sub orig_table {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'orig_table'} = $parm;
	} else {
		return $self -> {'orig_table'};
	}
}

sub sim_table {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sim_table'} = $parm;
	} else {
		return $self -> {'sim_table'};
	}
}

sub keep_estimation {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'keep_estimation'} = $parm;
	} else {
		return $self -> {'keep_estimation'};
	}
}

sub dv_table_name {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'dv_table_name'} = $parm;
	} else {
		return $self -> {'dv_table_name'};
	}
}

sub n_simulation_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'n_simulation_models'} = $parm;
	} else {
		return $self -> {'n_simulation_models'};
	}
}

sub idv {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'idv'} = $parm;
	} else {
		return $self -> {'idv'};
	}
}

sub bin_by_count {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bin_by_count'} = $parm;
	} else {
		return $self -> {'bin_by_count'};
	}
}

sub no_of_bins {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'no_of_bins'} = $parm;
	} else {
		return $self -> {'no_of_bins'};
	}
}

sub single_bin_size {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'single_bin_size'} = $parm;
	} else {
		return $self -> {'single_bin_size'};
	}
}

sub overlap_percent {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'overlap_percent'} = $parm;
	} else {
		return $self -> {'overlap_percent'};
	}
}

sub bin_array {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bin_array'} = $parm;
	} else {
		return $self -> {'bin_array'};
	}
}

sub categorized {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'categorized'} = $parm;
	} else {
		return $self -> {'categorized'};
	}
}

sub levels {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'levels'} = $parm;
	} else {
		return $self -> {'levels'};
	}
}

sub lloq {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lloq'} = $parm;
	} else {
		return $self -> {'lloq'};
	}
}

sub uloq {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'uloq'} = $parm;
	} else {
		return $self -> {'uloq'};
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

sub censor {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'censor'} = $parm;
	} else {
		return $self -> {'censor'};
	}
}

sub tte {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tte'} = $parm;
	} else {
		return $self -> {'tte'};
	}
}

sub sim_model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sim_model'} = $parm;
	} else {
		return $self -> {'sim_model'};
	}
}

sub flip_comments {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'flip_comments'} = $parm;
	} else {
		return $self -> {'flip_comments'};
	}
}

sub no_of_strata {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'no_of_strata'} = $parm;
	} else {
		return $self -> {'no_of_strata'};
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

sub msfo_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'msfo_file'} = $parm;
	} else {
		return $self -> {'msfo_file'};
	}
}

sub mirrors {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'mirrors'} = $parm;
	} else {
		return $self -> {'mirrors'};
	}
}

sub simulation_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'simulation_models'} = $parm;
	} else {
		return $self -> {'simulation_models'};
	}
}

sub original_model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'original_model'} = $parm;
	} else {
		return $self -> {'original_model'};
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

sub modelfit_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->modelfit_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->modelfit_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->modelfit_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
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
		'debug' -> die( message => "ERROR in tool::npc->_modelfit_raw_results_callback: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->_modelfit_raw_results_callback: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->_modelfit_raw_results_callback: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \&subroutine;
}

sub modelfit_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->modelfit_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->modelfit_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->modelfit_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub create_unique_values_hash {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'data_column' => 'm_ARRAY', 'reference' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->create_unique_values_hash: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->create_unique_values_hash: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->create_unique_values_hash: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->create_unique_values_hash: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->create_unique_values_hash: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

my %value_hash;
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my $reference = $parm{'reference'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \%value_hash;
}

sub get_bin_boundaries_overlap_count {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'value_hash' => 'HASH', 'overlap_percent' => 'm_SCALAR',
			'count' => 'm_SCALAR', 'data_column' => 'm_ARRAY',
			'data_indices' => 'm_ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_count: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_count: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_count: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_count: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_count: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my %value_hash = defined $parm{'value_hash'} ? %{$parm{'value_hash'}} : {};
	my $overlap_percent = $parm{'overlap_percent'};
	my $count = $parm{'count'};
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my @bin_floors = ();
	my @bin_ceilings = ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@bin_floors ,\@bin_ceilings;
}

sub get_bin_ceilings_from_count {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'value_hash' => 'HASH', 'n_bins' => 'SCALAR',
			'single_bin_size' => 'SCALAR', 'list_counts' => 'ARRAY',
			'data_column' => 'm_ARRAY', 'data_indices' => 'm_ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_count: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_count: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_count: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_count: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_count: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my %value_hash = defined $parm{'value_hash'} ? %{$parm{'value_hash'}} : {};
	my $n_bins = $parm{'n_bins'};
	my $single_bin_size = $parm{'single_bin_size'};
	my @list_counts = defined $parm{'list_counts'} ? @{$parm{'list_counts'}} : ();
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my @bin_ceilings = ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@bin_ceilings;
}

sub get_bin_boundaries_overlap_value {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'data_column' => 'm_ARRAY', 'data_indices' => 'm_ARRAY',
			'width' => 'm_SCALAR', 'overlap_percent' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_value: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_value: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_value: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_value: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_bin_boundaries_overlap_value: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my $width = $parm{'width'};
	my $overlap_percent = $parm{'overlap_percent'};
	my @bin_floors = ();
	my @bin_ceilings = ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@bin_floors ,\@bin_ceilings;
}

sub get_bin_ceilings_from_value {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'data_column' => 'm_ARRAY', 'data_indices' => 'm_ARRAY',
			'n_bins' => 'SCALAR', 'single_bin_size' => 'SCALAR',
			'list_boundaries' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_value: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_value: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_value: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_value: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_bin_ceilings_from_value: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my $n_bins = $parm{'n_bins'};
	my $single_bin_size = $parm{'single_bin_size'};
	my @list_boundaries = defined $parm{'list_boundaries'} ? @{$parm{'list_boundaries'}} : ();
	my @bin_ceilings = ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@bin_ceilings;
}

sub index_matrix_binned_values {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'value_hash' => 'm_HASH', 'reference_index' => 'SCALAR',
			'data_column' => 'm_ARRAY', 'data_indices' => 'm_ARRAY',
			'bin_ceilings' => 'ARRAY', 'bin_floors' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->index_matrix_binned_values: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->index_matrix_binned_values: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->index_matrix_binned_values: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->index_matrix_binned_values: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->index_matrix_binned_values: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my %value_hash = defined $parm{'value_hash'} ? %{$parm{'value_hash'}} : {};
	my $reference_index = $parm{'reference_index'};
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my @bin_ceilings = defined $parm{'bin_ceilings'} ? @{$parm{'bin_ceilings'}} : ();
	my @bin_floors = defined $parm{'bin_floors'} ? @{$parm{'bin_floors'}} : ();
	my @index_matrix;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@index_matrix;
}

sub round {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'number' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->round: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->round: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->round: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->round: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->round: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $number = $parm{'number'};
	my $integer_out;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $integer_out;
}

sub ceil {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'number' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->ceil: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->ceil: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->ceil: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->ceil: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->ceil: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $number = $parm{'number'};
	my $integer_out;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $integer_out;
}

sub median {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'sorted_array' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->median: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->median: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->median: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->median: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->median: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $sorted_array = $parm{'sorted_array'};
	my $result;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $result;
}

sub mean {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'array' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->mean: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->mean: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->mean: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->mean: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->mean: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $array = $parm{'array'};
	my $mean;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $mean;
}

sub standard_deviation {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'array' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->standard_deviation: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->standard_deviation: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->standard_deviation: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->standard_deviation: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->standard_deviation: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $array = $parm{'array'};
	my $result;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $result;
}

sub get_data_matrix {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->get_data_matrix: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->get_data_matrix: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->get_data_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_data_matrix: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_data_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub get_tte_data {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->get_tte_data: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->get_tte_data: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->get_tte_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_tte_data: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_tte_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub cleanup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->cleanup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->cleanup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->cleanup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->cleanup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->cleanup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub create_binned_data {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->create_binned_data: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->create_binned_data: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->create_binned_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->create_binned_data: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->create_binned_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub do_predcorr_and_varcorr {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'pred_array' => 'REF', 'bound_array' => 'REF',
			'data_array' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->do_predcorr_and_varcorr: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->do_predcorr_and_varcorr: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->do_predcorr_and_varcorr: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->do_predcorr_and_varcorr: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->do_predcorr_and_varcorr: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $pred_array = $parm{'pred_array'};
	my $bound_array = $parm{'bound_array'};
	my $data_array = $parm{'data_array'};
	my @corrected_data_array;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@corrected_data_array;
}

sub create_mirror_and_plot_data {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->create_mirror_and_plot_data: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->create_mirror_and_plot_data: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->create_mirror_and_plot_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->create_mirror_and_plot_data: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->create_mirror_and_plot_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub reprint_mirror_and_plot_data {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->reprint_mirror_and_plot_data: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->reprint_mirror_and_plot_data: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->reprint_mirror_and_plot_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->reprint_mirror_and_plot_data: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->reprint_mirror_and_plot_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub create_stratified_data {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->create_stratified_data: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->create_stratified_data: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->create_stratified_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->create_stratified_data: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->create_stratified_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub vpc_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->vpc_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->vpc_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->vpc_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->vpc_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->vpc_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub npc_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->npc_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->npc_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->npc_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->npc_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->npc_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub get_npc_indices {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'pred_intervals' => 'REF', 'ci' => 'SCALAR',
			'no_sim' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->get_npc_indices: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->get_npc_indices: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->get_npc_indices: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_npc_indices: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_npc_indices: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $pred_intervals = $parm{'pred_intervals'};
	my $ci = $parm{'ci'};
	my $no_sim = $parm{'no_sim'};
	my @lower_index;
	my @upper_index;
	my $low_ind;
	my $high_ind;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@lower_index ,\@upper_index ,$low_ind ,$high_ind;
}

sub get_npc_result_labels {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'pred_intervals' => 'REF', 'ci' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->get_npc_result_labels: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->get_npc_result_labels: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->get_npc_result_labels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_npc_result_labels: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->get_npc_result_labels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $pred_intervals = $parm{'pred_intervals'};
	my $ci = $parm{'ci'};
	my @result_column_labels;
	my @result_row_labels;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@result_column_labels ,\@result_row_labels;
}

sub subset_npc_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'bin_index' => 'SCALAR', 'strata_index' => 'SCALAR',
			'pred_intervals' => 'REF', 'lower_index' => 'REF',
			'upper_index' => 'REF', 'low_ind' => 'SCALAR',
			'high_ind' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::npc->subset_npc_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::npc->subset_npc_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::npc->subset_npc_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->subset_npc_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::npc->subset_npc_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $bin_index = $parm{'bin_index'};
	my $strata_index = $parm{'strata_index'};
	my $pred_intervals = $parm{'pred_intervals'};
	my $lower_index = $parm{'lower_index'};
	my $upper_index = $parm{'upper_index'};
	my $low_ind = $parm{'low_ind'};
	my $high_ind = $parm{'high_ind'};
	my @result_values;
	my @real_positions;
	my @stats_warnings;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@result_values ,\@real_positions ,\@stats_warnings;
}

1;

