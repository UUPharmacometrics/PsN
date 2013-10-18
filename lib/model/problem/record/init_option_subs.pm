# {{{ new
start new
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
end new
#}}}

# {{{ include
start include statements
use Carp;
use Math::Random;
end include statements
# }}}

# {{{ check_and_set_init

start check_and_set_init
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
end check_and_set_init

# }}} check_and_set_init

# {{{ _read_option

start _read_option
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
end _read_option

# }}} _read_option

# {{{ _format_option

start _format_option
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
end _format_option

# }}} _format_option

# {{{ store_init
start store_init
      {
	$self->stored_init($self->init);
      }
end store_init
# }}} store_init

# {{{ restore_init
start restore_init
{
	$self->init($self->stored_init)
		if ( defined $self->stored_init );
}
end restore_init
# }}} restore_init

# {{{ set_random_init

start set_random_init
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
end set_random_init

# }}} set_random_init
