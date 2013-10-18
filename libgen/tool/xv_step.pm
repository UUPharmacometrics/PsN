use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::xv_step;

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
	my %valid_parm = ( 'nr_validation_groups' => 'SCALAR', 'stratify_on' => 'SCALAR',
			'cutoff' => 'SCALAR', 'n_model_thetas' => 'SCALAR',
			'estimation_data' => 'ARRAY', 'prediction_data' => 'ARRAY',
			'init' => 'CODE', 'post_analyze' => 'CODE',
			'cont' => 'SCALAR', 'own_parameters' => 'HASH',
			'estimation_models' => 'ARRAY', 'prediction_models' => 'ARRAY',
			'prediction_is_run' => 'SCALAR', 'warnings' => 'SCALAR',
			'estimate_only' => 'SCALAR', 'predict_only' => 'SCALAR',
			'last_est_complete' => 'SCALAR' );

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
			'debug' -> die( message => "ERROR in tool::xv_step->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv_step->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'nr_validation_groups'} = defined $parm{'nr_validation_groups'} ? $parm{'nr_validation_groups'} : 5 unless defined $this -> {'nr_validation_groups'};
	$this -> {'n_model_thetas'} = defined $parm{'n_model_thetas'} ? $parm{'n_model_thetas'} : 0 unless defined $this -> {'n_model_thetas'};
	$this -> {'estimation_data'} = defined $parm{'estimation_data'} ? $parm{'estimation_data'} : [] unless defined $this -> {'estimation_data'};
	$this -> {'prediction_data'} = defined $parm{'prediction_data'} ? $parm{'prediction_data'} : [] unless defined $this -> {'prediction_data'};
	$this -> {'estimation_models'} = defined $parm{'estimation_models'} ? $parm{'estimation_models'} : [] unless defined $this -> {'estimation_models'};
	$this -> {'prediction_models'} = defined $parm{'prediction_models'} ? $parm{'prediction_models'} : [] unless defined $this -> {'prediction_models'};
	$this -> {'prediction_is_run'} = defined $parm{'prediction_is_run'} ? $parm{'prediction_is_run'} : 0 unless defined $this -> {'prediction_is_run'};
	$this -> {'warnings'} = defined $parm{'warnings'} ? $parm{'warnings'} : 0 unless defined $this -> {'warnings'};
	$this -> {'estimate_only'} = defined $parm{'estimate_only'} ? $parm{'estimate_only'} : 0 unless defined $this -> {'estimate_only'};
	$this -> {'predict_only'} = defined $parm{'predict_only'} ? $parm{'predict_only'} : 0 unless defined $this -> {'predict_only'};
	$this -> {'last_est_complete'} = defined $parm{'last_est_complete'} ? $parm{'last_est_complete'} : 0 unless defined $this -> {'last_est_complete'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub nr_validation_groups {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nr_validation_groups'} = $parm;
	} else {
		return $self -> {'nr_validation_groups'};
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

sub cutoff {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cutoff'} = $parm;
	} else {
		return $self -> {'cutoff'};
	}
}

sub n_model_thetas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'n_model_thetas'} = $parm;
	} else {
		return $self -> {'n_model_thetas'};
	}
}

sub estimation_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimation_data'} = $parm;
	} else {
		return $self -> {'estimation_data'};
	}
}

sub prediction_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prediction_data'} = $parm;
	} else {
		return $self -> {'prediction_data'};
	}
}

sub init {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'init'} = $parm;
	} else {
		return $self -> {'init'};
	}
}

sub post_analyze {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'post_analyze'} = $parm;
	} else {
		return $self -> {'post_analyze'};
	}
}

sub cont {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cont'} = $parm;
	} else {
		return $self -> {'cont'};
	}
}

sub own_parameters {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'own_parameters'} = $parm;
	} else {
		return $self -> {'own_parameters'};
	}
}

sub estimation_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimation_models'} = $parm;
	} else {
		return $self -> {'estimation_models'};
	}
}

sub prediction_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prediction_models'} = $parm;
	} else {
		return $self -> {'prediction_models'};
	}
}

sub prediction_is_run {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prediction_is_run'} = $parm;
	} else {
		return $self -> {'prediction_is_run'};
	}
}

sub warnings {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'warnings'} = $parm;
	} else {
		return $self -> {'warnings'};
	}
}

sub estimate_only {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimate_only'} = $parm;
	} else {
		return $self -> {'estimate_only'};
	}
}

sub predict_only {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'predict_only'} = $parm;
	} else {
		return $self -> {'predict_only'};
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

sub die {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::xv_step->die: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::xv_step->die: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv_step->die: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->die: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->die: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = defined $parm{'message'} ? $parm{'message'} : "Error message Undefined \n";

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub warn {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::xv_step->warn: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::xv_step->warn: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv_step->warn: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->warn: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->warn: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = defined $parm{'message'} ? $parm{'message'} : "Warning message Undefined\n";

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub modelfit_setup {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub modelfit_analyze {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub create_data_sets {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub modelfit_post_subtool_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

1;

