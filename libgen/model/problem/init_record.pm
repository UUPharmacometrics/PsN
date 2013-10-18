use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem::init_record;

#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(model::problem::record);

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'SCALAR', 'prior' => 'SCALAR', 'size' => 'SCALAR',
			'same' => 'SCALAR', 'fix' => 'SCALAR', 'sd' => 'SCALAR',
			'chol' => 'SCALAR', 'corr' => 'SCALAR', 'n_previous_rows' => 'SCALAR' );

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
			'debug' -> die( message => "ERROR in model::problem::init_record->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::init_record->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::init_record->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::init_record->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'type'} = defined $parm{'type'} ? $parm{'type'} : undef unless defined $this -> {'type'};
	$this -> {'prior'} = defined $parm{'prior'} ? $parm{'prior'} : 0 unless defined $this -> {'prior'};
	$this -> {'size'} = defined $parm{'size'} ? $parm{'size'} : undef unless defined $this -> {'size'};
	$this -> {'same'} = defined $parm{'same'} ? $parm{'same'} : 0 unless defined $this -> {'same'};
	$this -> {'fix'} = defined $parm{'fix'} ? $parm{'fix'} : 0 unless defined $this -> {'fix'};
	$this -> {'sd'} = defined $parm{'sd'} ? $parm{'sd'} : 0 unless defined $this -> {'sd'};
	$this -> {'chol'} = defined $parm{'chol'} ? $parm{'chol'} : 0 unless defined $this -> {'chol'};
	$this -> {'corr'} = defined $parm{'corr'} ? $parm{'corr'} : 0 unless defined $this -> {'corr'};
	$this -> {'n_previous_rows'} = defined $parm{'n_previous_rows'} ? $parm{'n_previous_rows'} : 0 unless defined $this -> {'n_previous_rows'};

	bless $this, $class;
	model::problem::record::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

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

sub size {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'size'} = $parm;
	} else {
		return $self -> {'size'};
	}
}

sub same {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'same'} = $parm;
	} else {
		return $self -> {'same'};
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

sub n_previous_rows {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'n_previous_rows'} = $parm;
	} else {
		return $self -> {'n_previous_rows'};
	}
}

sub store_inits {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub restore_inits {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub set_random_inits {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'degree' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::init_record->set_random_inits: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::init_record->set_random_inits: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::init_record->set_random_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::init_record->set_random_inits: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::init_record->set_random_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $degree = defined $parm{'degree'} ? $parm{'degree'} : 0.1;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_options {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _add_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'option_string' => 'SCALAR', 'fix' => 'SCALAR',
			'comment' => 'SCALAR', 'coordinate_string' => 'm_SCALAR',
			'on_diagonal' => 'SCALAR', 'sd' => 'SCALAR',
			'chol' => 'SCALAR', 'corr' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::init_record->_add_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::init_record->_add_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::init_record->_add_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::init_record->_add_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::init_record->_add_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $option_string = $parm{'option_string'};
	my $fix = defined $parm{'fix'} ? $parm{'fix'} : 0;
	my $comment = $parm{'comment'};
	my $coordinate_string = $parm{'coordinate_string'};
	my $on_diagonal = $parm{'on_diagonal'};
	my $sd = defined $parm{'sd'} ? $parm{'sd'} : 0;
	my $chol = defined $parm{'chol'} ? $parm{'chol'} : 0;
	my $corr = defined $parm{'corr'} ? $parm{'corr'} : 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _format_record {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'nonparametric_code' => 'SCALAR', 'number_format' => 'SCALAR',
			'shrinkage_code' => 'SCALAR', 'eigen_value_code' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::init_record->_format_record: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::init_record->_format_record: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::init_record->_format_record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::init_record->_format_record: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::init_record->_format_record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $nonparametric_code = $parm{'nonparametric_code'};
	my $number_format = $parm{'number_format'};
	my $shrinkage_code = $parm{'shrinkage_code'};
	my $eigen_value_code = $parm{'eigen_value_code'};
	my @formatted;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@formatted;
}

1;

