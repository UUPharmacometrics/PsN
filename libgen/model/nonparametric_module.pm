use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::nonparametric_module;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'SCALAR', 'model' => 'model', 'problem' => 'model::problem',
			'temp_problem_number' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::nonparametric_module->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'enabled'} = defined $parm{'enabled'} ? $parm{'enabled'} : 0 unless defined $this -> {'enabled'};

	bless $this, $class;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub enabled {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'enabled'} = $parm;
	} else {
		return $self -> {'enabled'};
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

sub problem {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'problem'} = $parm;
	} else {
		return $self -> {'problem'};
	}
}

sub temp_problem_number {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'temp_problem_number'} = $parm;
	} else {
		return $self -> {'temp_problem_number'};
	}
}

sub etas_tablename {
	my $self = shift;
	my $filename;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $filename;
}

sub marginals_tablename {
	my $self = shift;
	my $filename;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $filename;
}

sub format_etas_table {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@formatted;
}

sub format_marginals_table {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@formatted;
}

sub format_table {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};
	my @formatted;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@formatted;
}

sub problem_number {
	my $self = shift;
	my $problem_number;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $problem_number;
}

sub enable {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub disable {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub etas_table_exists {
	my $self = shift;
	my $exists = 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $exists;
}

sub marginals_table_exists {
	my $self = shift;
	my $exists = 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $exists;
}

sub format_nonparametric {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};
	my @formatted;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@formatted;
}

sub format_etas_nonparametric {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@formatted;
}

sub format_marginals_nonparametric {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@formatted;
}

sub format_etas_msfi {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@formatted;
}

1;

