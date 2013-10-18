use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem::record::theta_option;


#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(model::problem::record::init_option);

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'lobnd' => 'SCALAR' );

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
			'debug' -> die( message => "ERROR in model::problem::record::theta_option->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record::theta_option->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::theta_option->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::theta_option->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'lobnd'} = defined $parm{'lobnd'} ? $parm{'lobnd'} : -1000000 unless defined $this -> {'lobnd'};

	bless $this, $class;
	model::problem::record::init_option::new($this,%superParms);

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

sub _format_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'number_format' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::record::theta_option->_format_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::record::theta_option->_format_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::record::theta_option->_format_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::theta_option->_format_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::record::theta_option->_format_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $formatted;
	my $number_format = $parm{'number_format'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _format_option');
# line 45 "lib/model/problem/record/theta_option_subs.pm" 
      {
	my $to_long = 0;
	my $modifier = 0;
	my $init = $self->init;
	$formatted = '';

	if (defined $number_format and $number_format < 15 and ($PsN::nm_major_version > 6)){
	  my $form = '%.'.$number_format.'G';
	  $init = sprintf("$form",$init);
	}

	if ( ( defined $self->upbnd ) or ( defined $self->lobnd ) ) {
	  $formatted = $formatted."(". $self->lobnd.",";
	  $formatted = $formatted.$init;
	  $formatted = $formatted.",".$self->upbnd if ( defined $self->upbnd );
	  $formatted = $formatted.")";
	} else {
	  $formatted = $formatted.$init;
	}

	if ( $self->fix ) {
	  $formatted = $formatted." FIX";
	}

	$formatted = $formatted." ;" if ( ( defined $self->label ) or
					  ( defined $self->unit ) );
	if ( defined $self->label ) {
	  $formatted = $formatted." ".$self->label; #don't truncate
	}

	if ( defined $self->unit ) {
	  $formatted = $formatted."; ".$self->unit; #don't truncate
	}

      }
# line 144 libgen/model/problem/record/theta_option.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _format_option');
	# End of Non-Dia code #

	return $formatted;
}

sub option_count {
	my $self = shift;
	my $return_value = 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $return_value;
}

sub _read_option {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_option');
# line 2 "lib/model/problem/record/theta_option_subs.pm" 
      {
	my ( $line, $comment ) = split( ";", $self->option_string, 2 );

	## Split and store labels and units
	if ( defined $comment ) {
	  my ($label,$unit) = split( ";",$comment,2 );
	  chomp $label if $label;
	  chomp $unit if $unit;
	  $self->label($label);
	  $self->unit($unit);
	}

	# $line should be one theta now
	chomp( $line );
	$line =~ s/\)//g;
	$line =~ s/\(//g;
	$line =~ s/\s+//g;

	## Find fix
	$self->fix($line =~ s/FIX\w*// ? 1 : 0);

	my @inits = split( ",", $line );
	if ( $#inits <= 0) {
	  $self->init($inits[0]);
	  $self -> {'lobnd'} = undef;		# FIXME: Fix with Moose
	  $self -> {'upbnd'} = undef;
	} else {
	  $self->lobnd($inits[0]);
	  $self->init($inits[1]);
	  $self->upbnd($inits[2]);
	}
	
	if( defined $self->lobnd and $self->lobnd =~ /INF/ ) {
	  $self->lobnd($PsN::config -> {'low_INF'});
	}

	if( defined $self->upbnd and $self->upbnd =~ /INF/ ){
	  $self->upbnd($PsN::config -> {'high_INF'});
	}
      }
# line 207 libgen/model/problem/record/theta_option.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_option');
	# End of Non-Dia code #

}

1;

