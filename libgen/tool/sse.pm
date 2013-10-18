use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::sse;

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
	my %valid_parm = ( 'samples' => 'SCALAR', 'alternative_models' => 'ARRAY',
			'mc_models' => 'ARRAY', 'initial_values' => 'REF',
			'first_alternative' => 'SCALAR', 'bayes' => 'SCALAR',
			'simulation_rawres' => 'SCALAR', 'in_filter' => 'ARRAY',
			'out_filter' => 'ARRAY', 'covariance_file' => 'SCALAR',
			'rawres_input' => 'SCALAR', 'offset_rawres' => 'SCALAR',
			'add_models' => 'SCALAR', 'recompute' => 'SCALAR',
			'ref_ofv' => 'SCALAR', 'parallel_simulations' => 'SCALAR',
			'estimate_simulation' => 'SCALAR', 'have_nwpri' => 'SCALAR',
			'have_tnpri' => 'SCALAR', 'probnum' => 'SCALAR',
			'logfile' => 'REF', 'results_file' => 'SCALAR' );

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
			'debug' -> die( message => "ERROR in tool::sse->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'samples'} = defined $parm{'samples'} ? $parm{'samples'} : 100 unless defined $this -> {'samples'};
	$this -> {'alternative_models'} = defined $parm{'alternative_models'} ? $parm{'alternative_models'} : [] unless defined $this -> {'alternative_models'};
	$this -> {'mc_models'} = defined $parm{'mc_models'} ? $parm{'mc_models'} : [] unless defined $this -> {'mc_models'};
	$this -> {'bayes'} = defined $parm{'bayes'} ? $parm{'bayes'} : 0 unless defined $this -> {'bayes'};
	$this -> {'offset_rawres'} = defined $parm{'offset_rawres'} ? $parm{'offset_rawres'} : 1 unless defined $this -> {'offset_rawres'};
	$this -> {'add_models'} = defined $parm{'add_models'} ? $parm{'add_models'} : 0 unless defined $this -> {'add_models'};
	$this -> {'estimate_simulation'} = defined $parm{'estimate_simulation'} ? $parm{'estimate_simulation'} : 1 unless defined $this -> {'estimate_simulation'};
	$this -> {'have_nwpri'} = defined $parm{'have_nwpri'} ? $parm{'have_nwpri'} : 0 unless defined $this -> {'have_nwpri'};
	$this -> {'have_tnpri'} = defined $parm{'have_tnpri'} ? $parm{'have_tnpri'} : 0 unless defined $this -> {'have_tnpri'};
	$this -> {'probnum'} = defined $parm{'probnum'} ? $parm{'probnum'} : 1 unless defined $this -> {'probnum'};
	$this -> {'logfile'} = defined $parm{'logfile'} ? $parm{'logfile'} : ['sse.log'] unless defined $this -> {'logfile'};
	$this -> {'results_file'} = defined $parm{'results_file'} ? $parm{'results_file'} : 'sse_results.csv' unless defined $this -> {'results_file'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

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

sub alternative_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'alternative_models'} = $parm;
	} else {
		return $self -> {'alternative_models'};
	}
}

sub mc_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'mc_models'} = $parm;
	} else {
		return $self -> {'mc_models'};
	}
}

sub initial_values {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'initial_values'} = $parm;
	} else {
		return $self -> {'initial_values'};
	}
}

sub first_alternative {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'first_alternative'} = $parm;
	} else {
		return $self -> {'first_alternative'};
	}
}

sub bayes {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bayes'} = $parm;
	} else {
		return $self -> {'bayes'};
	}
}

sub simulation_rawres {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'simulation_rawres'} = $parm;
	} else {
		return $self -> {'simulation_rawres'};
	}
}

sub in_filter {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'in_filter'} = $parm;
	} else {
		return $self -> {'in_filter'};
	}
}

sub out_filter {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'out_filter'} = $parm;
	} else {
		return $self -> {'out_filter'};
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

sub add_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'add_models'} = $parm;
	} else {
		return $self -> {'add_models'};
	}
}

sub recompute {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'recompute'} = $parm;
	} else {
		return $self -> {'recompute'};
	}
}

sub ref_ofv {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ref_ofv'} = $parm;
	} else {
		return $self -> {'ref_ofv'};
	}
}

sub parallel_simulations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parallel_simulations'} = $parm;
	} else {
		return $self -> {'parallel_simulations'};
	}
}

sub estimate_simulation {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimate_simulation'} = $parm;
	} else {
		return $self -> {'estimate_simulation'};
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

sub probnum {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'probnum'} = $parm;
	} else {
		return $self -> {'probnum'};
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

sub general_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'class' => 'SCALAR',
			'subm_threads' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->general_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->general_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->general_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->general_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->general_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $class = $parm{'class'};
	my $subm_threads = $parm{'subm_threads'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub modelfit_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->modelfit_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->modelfit_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->modelfit_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub cdd_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->cdd_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->cdd_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->cdd_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->cdd_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->cdd_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
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
		'debug' -> die( message => "ERROR in tool::sse->_modelfit_raw_results_callback: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->_modelfit_raw_results_callback: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->_modelfit_raw_results_callback: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
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
		'debug' -> die( message => "ERROR in tool::sse->modelfit_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->modelfit_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->modelfit_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub prepare_results {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->prepare_results: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->prepare_results: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->prepare_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->prepare_results: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->prepare_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub format_initials {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'initials_object' => 'REF', 'initial_value' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->format_initials: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->format_initials: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->format_initials: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->format_initials: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->format_initials: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $initials_object = $parm{'initials_object'};
	my $initial_value = $parm{'initial_value'};
my %initials_hash;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \%initials_hash;
}

sub compute_rmse {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'use_runs' => 'm_ARRAY', 'column_index' => 'm_SCALAR',
			'start_row_index' => 'SCALAR', 'end_row_index' => 'SCALAR',
			'initial_values' => 'm_ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->compute_rmse: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->compute_rmse: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->compute_rmse: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->compute_rmse: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->compute_rmse: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @use_runs = defined $parm{'use_runs'} ? @{$parm{'use_runs'}} : ();
	my $column_index = $parm{'column_index'};
	my $start_row_index = defined $parm{'start_row_index'} ? $parm{'start_row_index'} : 0;
	my $end_row_index = $parm{'end_row_index'};
	my @initial_values = defined $parm{'initial_values'} ? @{$parm{'initial_values'}} : ();
	my $rmse;
	my $relative_rmse_percent;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $rmse ,$relative_rmse_percent;
}

sub compute_bias {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'use_runs' => 'm_ARRAY', 'column_index' => 'm_SCALAR',
			'start_row_index' => 'SCALAR', 'end_row_index' => 'SCALAR',
			'initial_values' => 'm_ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->compute_bias: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->compute_bias: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->compute_bias: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->compute_bias: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->compute_bias: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @use_runs = defined $parm{'use_runs'} ? @{$parm{'use_runs'}} : ();
	my $column_index = $parm{'column_index'};
	my $start_row_index = defined $parm{'start_row_index'} ? $parm{'start_row_index'} : 0;
	my $end_row_index = $parm{'end_row_index'};
	my @initial_values = defined $parm{'initial_values'} ? @{$parm{'initial_values'}} : ();
	my $absolute_bias;
	my $relative_absolute_bias_percent;
	my $relative_bias_percent;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $absolute_bias ,$relative_absolute_bias_percent ,$relative_bias_percent;
}

sub median {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'use_runs' => 'm_ARRAY', 'column_index' => 'm_SCALAR',
			'start_row_index' => 'SCALAR', 'end_row_index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->median: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->median: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->median: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->median: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->median: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @use_runs = defined $parm{'use_runs'} ? @{$parm{'use_runs'}} : ();
	my $column_index = $parm{'column_index'};
	my $start_row_index = defined $parm{'start_row_index'} ? $parm{'start_row_index'} : 0;
	my $end_row_index = $parm{'end_row_index'};
	my $median;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $median;
}

sub skewness_and_kurtosis {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'use_runs' => 'm_ARRAY', 'column_index' => 'm_SCALAR',
			'start_row_index' => 'SCALAR', 'end_row_index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->skewness_and_kurtosis: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->skewness_and_kurtosis: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->skewness_and_kurtosis: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->skewness_and_kurtosis: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->skewness_and_kurtosis: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @use_runs = defined $parm{'use_runs'} ? @{$parm{'use_runs'}} : ();
	my $column_index = $parm{'column_index'};
	my $start_row_index = defined $parm{'start_row_index'} ? $parm{'start_row_index'} : 0;
	my $end_row_index = $parm{'end_row_index'};
	my $skewness;
	my $kurtosis;
	my $mean;
	my $stdev;
	my $warn = 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $skewness ,$kurtosis ,$mean ,$stdev ,$warn;
}

sub max_and_min {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'use_runs' => 'm_ARRAY', 'column_index' => 'm_SCALAR',
			'start_row_index' => 'SCALAR', 'end_row_index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->max_and_min: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->max_and_min: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->max_and_min: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->max_and_min: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->max_and_min: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @use_runs = defined $parm{'use_runs'} ? @{$parm{'use_runs'}} : ();
	my $column_index = $parm{'column_index'};
	my $start_row_index = defined $parm{'start_row_index'} ? $parm{'start_row_index'} : 0;
	my $end_row_index = $parm{'end_row_index'};
	my $maximum;
	my $minimum;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $maximum ,$minimum;
}

sub cleanup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::sse->cleanup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::sse->cleanup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::sse->cleanup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->cleanup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::sse->cleanup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

1;

