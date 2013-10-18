use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package nonmem;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'error_message' => 'SCALAR', 'fsubs' => 'ARRAY',
			'display_iterations' => 'SCALAR', 'full_path_executable' => 'SCALAR',
			'nmqual' => 'SCALAR', 'nm_directory' => 'SCALAR',
			'nm_minor_version' => 'SCALAR', 'nm_major_version' => 'SCALAR',
			'parafile' => 'SCALAR', 'nodes' => 'SCALAR',
			'nonmem_options' => 'SCALAR', 'modelfile' => 'm_SCALAR',
			'nice' => 'SCALAR', 'nmtran_message' => '',
			'outputfile' => 'SCALAR', 'version' => 'SCALAR',
			'show_version' => 'SCALAR', 'adaptive' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in nonmem->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in nonmem->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in nonmem->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in nonmem->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in nonmem->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'display_iterations'} = defined $parm{'display_iterations'} ? $parm{'display_iterations'} : 0 unless defined $this -> {'display_iterations'};
	$this -> {'nmqual'} = defined $parm{'nmqual'} ? $parm{'nmqual'} : 0 unless defined $this -> {'nmqual'};
	$this -> {'nodes'} = defined $parm{'nodes'} ? $parm{'nodes'} : 0 unless defined $this -> {'nodes'};
	$this -> {'nice'} = defined $parm{'nice'} ? $parm{'nice'} : 19 unless defined $this -> {'nice'};
	$this -> {'version'} = defined $parm{'version'} ? $parm{'version'} : 5 unless defined $this -> {'version'};
	$this -> {'show_version'} = defined $parm{'show_version'} ? $parm{'show_version'} : 1 unless defined $this -> {'show_version'};
	$this -> {'adaptive'} = defined $parm{'adaptive'} ? $parm{'adaptive'} : 0 unless defined $this -> {'adaptive'};

	bless $this, $class;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub error_message {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'error_message'} = $parm;
	} else {
		return $self -> {'error_message'};
	}
}

sub fsubs {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'fsubs'} = $parm;
	} else {
		return $self -> {'fsubs'};
	}
}

sub display_iterations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'display_iterations'} = $parm;
	} else {
		return $self -> {'display_iterations'};
	}
}

sub full_path_executable {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'full_path_executable'} = $parm;
	} else {
		return $self -> {'full_path_executable'};
	}
}

sub nmqual {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nmqual'} = $parm;
	} else {
		return $self -> {'nmqual'};
	}
}

sub nm_directory {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nm_directory'} = $parm;
	} else {
		return $self -> {'nm_directory'};
	}
}

sub nm_minor_version {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nm_minor_version'} = $parm;
	} else {
		return $self -> {'nm_minor_version'};
	}
}

sub nm_major_version {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nm_major_version'} = $parm;
	} else {
		return $self -> {'nm_major_version'};
	}
}

sub parafile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parafile'} = $parm;
	} else {
		return $self -> {'parafile'};
	}
}

sub nodes {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nodes'} = $parm;
	} else {
		return $self -> {'nodes'};
	}
}

sub nonmem_options {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonmem_options'} = $parm;
	} else {
		return $self -> {'nonmem_options'};
	}
}

sub modelfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'modelfile'} = $parm;
	} else {
		return $self -> {'modelfile'};
	}
}

sub nice {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nice'} = $parm;
	} else {
		return $self -> {'nice'};
	}
}

sub nmtran_message {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nmtran_message'} = $parm;
	} else {
		return $self -> {'nmtran_message'};
	}
}

sub outputfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'outputfile'} = $parm;
	} else {
		return $self -> {'outputfile'};
	}
}

sub version {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'version'} = $parm;
	} else {
		return $self -> {'version'};
	}
}

sub show_version {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'show_version'} = $parm;
	} else {
		return $self -> {'show_version'};
	}
}

sub adaptive {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'adaptive'} = $parm;
	} else {
		return $self -> {'adaptive'};
	}
}

sub store_message {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR', 'die_after' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in nonmem->store_message: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in nonmem->store_message: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in nonmem->store_message: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in nonmem->store_message: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in nonmem->store_message: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = $parm{'message'};
	my $die_after = defined $parm{'die_after'} ? $parm{'die_after'} : 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub run_with_nmfe {
	my $self = shift;
	my $return_value = 1;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $return_value;
}

sub run_with_nmqual {
	my $self = shift;
	my $return_value = 1;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $return_value;
}

1;

