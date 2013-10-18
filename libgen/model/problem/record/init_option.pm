use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem::record::init_option;

#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(model::problem::record::option);

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'lobnd' => 'SCALAR', 'upbnd' => 'SCALAR', 'init' => 'SCALAR',
			'fix' => 'SCALAR', 'label' => 'SCALAR', 'prior' => 'SCALAR',
			'unit' => 'SCALAR', 'stored_init' => 'SCALAR',
			'coordinate_string' => 'm_SCALAR', 'on_diagonal' => 'SCALAR',
			'sd' => 'SCALAR', 'chol' => 'SCALAR', 'corr' => 'SCALAR' );

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
			'debug' -> die( message => "ERROR in model::problem::record::init_option->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record::init_option->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::init_option->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::init_option->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'upbnd'} = defined $parm{'upbnd'} ? $parm{'upbnd'} : undef unless defined $this -> {'upbnd'};
	$this -> {'init'} = defined $parm{'init'} ? $parm{'init'} : undef unless defined $this -> {'init'};
	$this -> {'fix'} = defined $parm{'fix'} ? $parm{'fix'} : 0 unless defined $this -> {'fix'};
	$this -> {'label'} = defined $parm{'label'} ? $parm{'label'} : undef unless defined $this -> {'label'};
	$this -> {'prior'} = defined $parm{'prior'} ? $parm{'prior'} : 0 unless defined $this -> {'prior'};
	$this -> {'unit'} = defined $parm{'unit'} ? $parm{'unit'} : undef unless defined $this -> {'unit'};
	$this -> {'stored_init'} = defined $parm{'stored_init'} ? $parm{'stored_init'} : undef unless defined $this -> {'stored_init'};
	$this -> {'sd'} = defined $parm{'sd'} ? $parm{'sd'} : 0 unless defined $this -> {'sd'};
	$this -> {'chol'} = defined $parm{'chol'} ? $parm{'chol'} : 0 unless defined $this -> {'chol'};
	$this -> {'corr'} = defined $parm{'corr'} ? $parm{'corr'} : 0 unless defined $this -> {'corr'};

	bless $this, $class;
	model::problem::record::option::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub lobnd {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lobnd'} = $parm;
	} else {
		return $self -> {'lobnd'};
	}
}

sub upbnd {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'upbnd'} = $parm;
	} else {
		return $self -> {'upbnd'};
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

sub fix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'fix'} = $parm;
	} else {
		return $self -> {'fix'};
	}
}

sub label {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'label'} = $parm;
	} else {
		return $self -> {'label'};
	}
}

sub prior {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prior'} = $parm;
	} else {
		return $self -> {'prior'};
	}
}

sub unit {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'unit'} = $parm;
	} else {
		return $self -> {'unit'};
	}
}

sub stored_init {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'stored_init'} = $parm;
	} else {
		return $self -> {'stored_init'};
	}
}

sub coordinate_string {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'coordinate_string'} = $parm;
	} else {
		return $self -> {'coordinate_string'};
	}
}

sub on_diagonal {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'on_diagonal'} = $parm;
	} else {
		return $self -> {'on_diagonal'};
	}
}

sub sd {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sd'} = $parm;
	} else {
		return $self -> {'sd'};
	}
}

sub chol {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'chol'} = $parm;
	} else {
		return $self -> {'chol'};
	}
}

sub corr {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'corr'} = $parm;
	} else {
		return $self -> {'corr'};
	}
}

sub store_init {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub restore_init {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub set_random_init {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'degree' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::record::init_option->set_random_init: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::record::init_option->set_random_init: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record::init_option->set_random_init: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::init_option->set_random_init: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::init_option->set_random_init: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $degree = defined $parm{'degree'} ? $parm{'degree'} : 0.1;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub check_and_set_init {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_value' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::record::init_option->check_and_set_init: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::record::init_option->check_and_set_init: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record::init_option->check_and_set_init: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::init_option->check_and_set_init: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::init_option->check_and_set_init: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $success = 0;
	my @error_code = [0,0,0];
	my $new_value = $parm{'new_value'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $success ,\@error_code ,$new_value;
}

sub _read_option {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _format_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'len' => 'SCALAR', 'number_format' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::record::init_option->_format_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::record::init_option->_format_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record::init_option->_format_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::init_option->_format_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::init_option->_format_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $len = $parm{'len'};
	my $formatted;
	my $number_format = $parm{'number_format'};
	my $no_break = 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $formatted ,$no_break;
}

1;

