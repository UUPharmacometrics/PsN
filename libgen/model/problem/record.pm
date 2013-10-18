use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem::record;

#---------------------------------------------------------------------
#         Used Packages
#---------------------------------------------------------------------
use model::problem::record::option;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'options' => 'ARRAY', 'record_arr' => 'ARRAY',
			'comment' => 'ARRAY', 'print_order' => 'ARRAY' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::record->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::record->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}


	bless $this, $class;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub options {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'options'} = $parm;
	} else {
		return $self -> {'options'};
	}
}

sub record_arr {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'record_arr'} = $parm;
	} else {
		return $self -> {'record_arr'};
	}
}

sub comment {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'comment'} = $parm;
	} else {
		return $self -> {'comment'};
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

sub add_option {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_option given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'options'}},
		model::problem::record::option -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub remove_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'name' => 'SCALAR', 'fuzzy_match' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::record->remove_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::record->remove_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record->remove_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record->remove_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record->remove_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $name = $parm{'name'};
	my $fuzzy_match = defined $parm{'fuzzy_match'} ? $parm{'fuzzy_match'} : 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _add_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'option_string' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::record->_add_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::record->_add_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record->_add_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record->_add_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record->_add_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $option_string = $parm{'option_string'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_options {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _format_record {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'nonparametric_code' => 'SCALAR', 'number_format' => 'SCALAR',
			'shrinkage_code' => 'SCALAR', 'eigen_value_code' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::record->_format_record: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::record->_format_record: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record->_format_record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record->_format_record: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record->_format_record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
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

