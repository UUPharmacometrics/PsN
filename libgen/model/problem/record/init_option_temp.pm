use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem::record::init_option;
use Carp;
use Math::Random;
use debug;


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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 3 "lib/model/problem/record/init_option_subs.pm" 
      {
	if ( defined $this->on_diagonal ) {
	  # on_diagonal is only defined for omegas and sigmas
	  # The default lower boundary value is 0. For off-diagonal
	  # elements this must be changed to undef.
	  if ( not $this->on_diagonal ) {
	    $this -> {'lobnd'} = undef;		# FIXME: Fix with Moose
	  }
	}
      }
# line 85 libgen/model/problem/record/init_option.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> store_init');
# line 174 "lib/model/problem/record/init_option_subs.pm" 
      {
	$self->stored_init($self->init);
      }
# line 283 libgen/model/problem/record/init_option.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> store_init');
	# End of Non-Dia code #

}

sub restore_init {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> restore_init');
# line 182 "lib/model/problem/record/init_option_subs.pm" 
{
	$self->init($self->stored_init)
		if ( defined $self->stored_init );
}
# line 299 libgen/model/problem/record/init_option.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> restore_init');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> set_random_init');
# line 192 "lib/model/problem/record/init_option_subs.pm" 
      {
	  # Degree is 0.1*n where n is the retry number.

	my ( $sign, $est, $form );
	unless ( $self->fix or ($self->init == 0) ) {
	  my $lobnd = $self->lobnd;
	  my $upbnd = $self->upbnd;
	  my $init  = $self->init;
	  my $change = abs($degree*$init);

	  if ((not defined $lobnd) and $self->on_diagonal) {
	    $lobnd = 0; #diagonal omegas and sigmas must not be negative
	  }
	  if ( defined $lobnd ) {
	    $lobnd = $init-$change < $lobnd ? $lobnd : $init-$change;
	  } else {
	    $lobnd = $init-$change;
	  }
	  if ( defined $upbnd ) {
	    $upbnd = $init+$change > $upbnd ? $upbnd : $init+$change;
	  } else {
	    $upbnd = $init+$change;
	  }
	  $lobnd = 0.01 if ( ( $lobnd < 0.01 and $lobnd > -0.01)
			     and $upbnd >= 0.01001 );
	  $upbnd = -0.01 if ( ( $upbnd < 0.01 and $upbnd > -0.01)
			      and $lobnd <= -0.01001 );
	  
	  
	  if ( $lobnd <= -0.01 and $upbnd >= 0.01 ) {
	    $est = random_uniform(1, $lobnd + 0.02, $upbnd);
	    $est = $est - 0.02 if ( $est <0.01 );
	  } else {
	    $est = random_uniform(1, $lobnd, $upbnd ); #bug
	  }
	  $form  = "%6.4f" if $est < 1000 and $est > -999;
	  $form  = "%6.1f" if $est >= 1000 or $est <=-999;
	  $self->init(sprintf $form, $est);
	  if ($self->init == 0) { 
	    $self->init('0.0001');
	  }
	} else {
	  carp("Init is FIXED or zero, leaving it unchanged" );
	}                                                                         
      }
# line 382 libgen/model/problem/record/init_option.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> set_random_init');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> check_and_set_init');
# line 26 "lib/model/problem/record/init_option_subs.pm" 
      {
	# Error codes:
	# [0,0,0]:    Success
	# [1,0,0]:    Value truncated
	# [0,1,0]:    Value below lower boundary
	# [0,0,1]:    Value above upper boundary

	if ( defined $new_value ) {
	  $success = 1;
	  my $original=$new_value;
	  if ($PsN::nm_major_version < 7){
	    if ( 
		 (
		  (($new_value < 0.000001)and ($new_value > 0) )
		  or
		  (($new_value > -0.00001)and ($new_value < 0) )
		  )
		 and #either THETA or diagonal OMEGA/SIGMA
		 ((not defined $self->on_diagonal) or ($self->on_diagonal) ) ){
	      #replace with 0.000001 or -0.00001, smallest nonzero number
	      if ($new_value > 0){
		$new_value = "0.000001";
	      }else{ 
		$new_value = "-0.00001";
	      }
	      carp($original." is too small, setting $new_value instead ");
	      $error_code[1] = 1;
	    }else{
	      #still NM6, but absolute value large enough
	      $new_value = sprintf("%.6f",$new_value); #six decimal places
	      $new_value = substr($new_value,0,8); #max 8 characters
	      if (eval($new_value) != eval($original)){
					carp($original . " is too long, using the truncated " . $new_value . " instead");
					$error_code[1] = 1;
	      }
	    }
	  } else {
	    #NM7, E notation or regular as needed, precision as in default raw output
	    #do not allow larger than 15, doc says double has 15 sigdig
	    #$temp_value = sprintf("%.".$prec."G",$new_value);
	    $new_value = sprintf("%.15G",$new_value);
	  }
	  if ( defined $self->lobnd and $new_value <= $self->lobnd ) {
	    $success = 0;
	    $error_code[2] = 1;
	  }
	  if ( defined $self->upbnd and $new_value >= $self->upbnd ) {
	    $success = 0;
	    $error_code[3] = 1;
	  }
	  if ( $success ) {
	    $self->init($new_value);
	  }
	}
      }
# line 477 libgen/model/problem/record/init_option.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> check_and_set_init');
	# End of Non-Dia code #

	return $success ,\@error_code ,$new_value;
}

sub _read_option {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_option');
# line 88 "lib/model/problem/record/init_option_subs.pm" 
      {
	my $optstr = $self->option_string;

	## Find fix unless it's already defined

	unless ( defined $self->fix ) {
	  $self->fix($optstr =~ s/FIX\w*// ? 1 : 0);
	}

	## Split initials from comments
	my ($init,$comment)  = split ";",$optstr,2;

	## Split and store names and units
	my ($label,$unit)     = split ";",$comment,2 if $comment;
	chomp $label if $label;
	chomp $unit if $unit;
	$self->label($label);
	$self->unit($unit);

	## Should only be one value now
	$init =~ s/\(//  ;
	$init =~ s/\)//  ;
	$init =~ s/\s*//g;
	$self->init($init);
      }
# line 515 libgen/model/problem/record/init_option.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_option');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _format_option');
# line 120 "lib/model/problem/record/init_option_subs.pm" 
      {
	my $init = $self->init;
	my $label= $self->label;
	my $unit = $self->unit;
	my $fix  = $self->fix;

	$formatted = "";
	my $str2   = "";
	my $str3   = "";
	# If FIX
	  $init =~ s/\s*//g;
	if ( defined $self -> init() ) {
	  $formatted = "$init";

	  if (defined $number_format and $number_format < 15 and ($PsN::nm_major_version > 6)){
	    my $form = '%.'.$number_format.'G';
	    $formatted = sprintf("$form",$formatted);
	  }

	  if ( $self -> fix() ) {
	    $formatted = $formatted.sprintf("%5s",'FIX');
	  }
	  if ( $self -> sd() ) {
	    $formatted = $formatted.sprintf("%10s",'STANDARD');
	  }
	  if ( $self -> corr() ) {
	    $formatted = $formatted.sprintf("%13s",'CORRELATION');
	  }
	  if ( $self -> chol() ) {
	    $formatted = $formatted.sprintf("%10s",'CHOLESKY');
	  }
	} else {
	  $formatted = "";
	}

	## Pad with spaces

	if ( defined $label or defined $unit ) {
	  $str2 = "$label" if defined $label;
	  $str2 = "  ; ".sprintf("%10s",$str2) if defined $label;
	} elsif (not $self->on_diagonal()){
	  $no_break=1;
	}
	if ( defined $unit ) {
	  $str3 = "  ; ".sprintf("%10s",$unit);
	}
	$formatted = $formatted.$str2.$str3;
      }
# line 604 libgen/model/problem/record/init_option.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _format_option');
	# End of Non-Dia code #

	return $formatted ,$no_break;
}

1;

