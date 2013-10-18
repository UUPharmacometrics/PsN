use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem::init_record;
use Carp;
use model::problem::record::init_option;
use debug;


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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> store_inits');
# line 40 "lib/model/problem/init_record_subs.pm" 
      {
	if ( defined $self->options ) {
	  foreach my $option ( @{$self->options} ){
	    if( $option -> can( 'store_init' ) ){
	      $option -> store_init;
	    }
	  }
	}
      }
# line 216 libgen/model/problem/init_record.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> store_inits');
	# End of Non-Dia code #

}

sub restore_inits {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> restore_inits');
# line 13 "lib/model/problem/init_record_subs.pm" 
{
	if ( defined $self->options ) {
	  foreach my $option ( @{$self->options} ) {
	    $option->restore_init;
	  }
	}
}
# line 235 libgen/model/problem/init_record.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> restore_inits');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> set_random_inits');
# line 26 "lib/model/problem/init_record_subs.pm" 
      {
	if ( defined $self -> options and not $self->same and 
	     not $self->fix and not $self->prior) {
	  foreach my $option ( @{$self -> options} ){
	    $option -> set_random_init( degree => $degree )
		unless ($option->prior());
	  }
	}
      }
# line 282 libgen/model/problem/init_record.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> set_random_inits');
	# End of Non-Dia code #

}

sub _read_options {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_options');
# line 147 "lib/model/problem/init_record_subs.pm" 
      {
#	my @inits = ();
	my @row   = ();
	my @digits = ();
	my @fix = ();
	my @sds = ();
	my @chols = ();
	my @corrs = ();
	my @comments = ();
	my $warn_standar_chol = 0;
	my ( $any_fixed, $any_sd, $any_corr, $block_sd, $block_corr, $block_fixed, $any_chol, $block_chol ) = ( 0, 0, 0, 0, 0, 0, 0, 0 );
	my @class_names = split('::',ref($self));
	my $parameter = uc(pop(@class_names));

	if ( defined $self->record_arr ) {
	  for ( @{$self->record_arr} ) {
	    my $whole_row = $_;
	    chomp;
	    s/^\s+//; #leading spaces
	    s/\s+$//; #trailing spaces
	    s/^\s*\$\w+//; #the record name
	    next unless( length($_) > 0 );
	    if( /^\s*\;/ ) {
	      # This is a comment row
				$self->comment([]) unless defined $self->comment;
	      push( @{$self->comment}, $_ . "\n" );
	    } else {
	      # Make sure that the labels and units are in one string
	      s/\;\s+/\;/g; #spaces after ;
	      # Get rid of unwanted spaces
	      s/\s*\)/\)/g; #spaces inside parentheses
	      s/\(\s*/\(/g;
	      my ( $line, $line_comment ) = split( ";", $_, 2 );
	      $_ = $line;
	      $any_fixed++ if /FIX/;
	      $any_sd++    if /SD/;
	      $warn_standar_chol = 1 if /SD/;
	      $any_sd++    if /STANDARD/;
	      $warn_standar_chol = 1 if /STANDARD/;
	      $any_chol++    if /CHOLESKY/;
	      $warn_standar_chol = 1 if /CHOLESKY/;
	      $any_corr++  if /CORRELATION/;
	      $warn_standar_chol = 1 if /CORRELATION/;
	      if (/DIAG\w*/){
		$self->type('DIAGONAL');
		if (s/^\s*(DIAG)\w*\s*\((\d+)\)\s*//){
		  $self->size($2);
		}else {
		  croak("Error parsing matrix record: size (n) is mandatory with DIAGONAL.");
		}
	      } elsif (/BLOCK/){
		$self->type('BLOCK'); #either size or SAME mandatory with block
		if (/SAME\((\d+)\)/){
		    croak("Model parsing error: PsN does not support SAME(".$1.") in ". 
				    '$'.$parameter);
		}
		if (s/\s*(BLOCK)\s*\((\d+)\)\s*//){
		  $self->size($2);
		  if (s/SAME//){
		    $self->same(1);
		  }
		}elsif (s/\s*(BLOCK)\s*(SAME)\s*//){
		  $self->same(1);
		}else {
		  croak("Error parsing matrix record: size (n) or SAME is mandatory with BLOCK.");
		}
		#if block then remove all FIX etc here, will be set since any_fixed etc
		( s/STANDARD// );
		( s/SD// );
		( s/FIX// );
		( s/CHOLESKY// );
		( s/CORRELATION// );
		( s/COVARIANCE// );
		( s/VARIANCE// );
	      }

	      my $print_debug=0;
	      while (/\w/) {
		  if ( /\)(x\d+)/){
		      croak("Model parsing error: PsN does not support ".$1." notation in ". 
				      '$'.$parameter);
		  }
		( s/^\s+// );
		if ( s/^\(([\w \.+\-]*)\)// ) {
		  #one set of (init options) or (options init), parentheses enclose
		  my @opt = split( " ",$1 );
		  my ( $digit, $comment, $fixed, $sd, $corr, $chol ) = ( undef, undef, 0, 0, 0, 0 );
		  for ( my $i = 0; $i <= $#opt; $i++ ) {
		    print $opt[$i]."," if ($print_debug);
		    if ( $opt[$i] =~ /\d+/ ) {
		      $digit = $opt[$i];
		    } elsif ( $opt[$i] =~ /FIX/ ) {
		      $fixed = 1;
		    } elsif ( $opt[$i] =~ /SD/ or $opt[$i] =~ /STANDARD/ ) {
		      $sd = 1;
		      $warn_standar_chol = 1;
		    } elsif ( $opt[$i] =~ /CORRELATION/ ) {
		      $corr = 1;
		      $warn_standar_chol = 1;
		    } elsif ( $opt[$i] =~ /CHOLESKY/ ) {
		      $chol = 1;
		      $warn_standar_chol = 1;
		    } else {
		      croak("Model parsing error: Unknown option $_" )
			  unless ($opt[$i] =~ /COVARIANCE/ or $opt[$i] =~ /VARIANCE/);
		    }
		  }
		  $comment = (/\w/) ? ' ' : $line_comment;
		  if ( defined $digit ) {
		    push( @digits, $digit );
		    push( @fix   , $fixed );
		    push( @sds    , $sd );
		    push( @corrs  , $corr );
		    push( @chols  , $chol );
		    push( @comments, $comment );
		  }
		} else {
		  # all inits  and options up to ( or end of string
		  unless ( s/^([^\(]+)// ) {
		    croak("Model parsing error: unknown string $_" );
		  }
		  @row = split( " ", $1 );
		  my ( $digit, $comment, $fixed, $sd, $corr, $chol ) = ( undef, undef, 0, 0, 0, 0 );
		  for ( my $i = 0; $i <= $#row; $i++ ) {
		    # In this code we find all records coded like: init options init options ...
		    if ( $row[$i] =~ /\d+/ ) {
		      #we find a new digit
		      if ( defined $digit ) {
			#if we had one from before, store the old one
			push( @digits, $digit );
			push( @fix   , $fixed );
			push( @sds    , $sd );
			push( @corrs  , $corr );
			push( @chols  , $chol );
			push( @comments, $comment );
		      }elsif($sd or $chol or $fixed or $corr){
			croak("Model parsing error: ".
					"Found option before ".
					"digit ".$row[$i]." on\n $whole_row".
					"Options must come after inits unless parentheses are used.");
		      }
		      ( $fixed, $sd, $corr, $chol ) = ( 0, 0, 0, 0 );
		      $digit = $row[$i]; #read the new one
		    } elsif ( $row[$i] =~ /FIX/ and not $fixed ) {
		      $fixed = 1;
		    } elsif ( $row[$i] =~ /STANDARD/ ) {
		      $sd = 1;
		    } elsif ( $row[$i] =~ /SD/) {
		      $sd = 1;
		    } elsif ( $row[$i] =~ /CORRELATION/ ) {
		      $corr = 1;
		    } elsif ( $row[$i] =~ /CHOLESKY/ ) {
		      $chol = 1;
		    } else {
			if ($row[$i] =~ /VALUES/){
			    croak("Model parsing error: PsN does not support VALUES option in ". 
					    '$'.$parameter);
			}else{
			    croak("Model parsing error: Unknown option ".$row[$i] )
				unless ($row[$i] =~ /COVARIANCE/ or $row[$i] =~ /VARIANCE/);
			}
		    }
		    if ($i == $#row){
		      if (/\w/){
			$comment = ' ';
		      } else {
			$comment = $line_comment;
		      }
		      if ( defined $digit ) {
			#store the last digit, above we only stored when found a new one
			push( @digits, $digit );
			push( @fix   , $fixed );
			push( @sds    , $sd );
			push( @corrs  , $corr );
			push( @chols  , $chol );
			push( @comments, $comment );
		      } elsif ($fixed and ($self->type ne 'BLOCK')){
			my $mes = "parsing error: FIX in \n$whole_row".
			    "could not be coupled to an initial value.";
			$mes .=" Please check the parentheses." if ($#digits == 0 and ($whole_row =~ /[\(\)]/));
			croak($mes );
		      } elsif ($sd and ($self->type ne 'BLOCK')){
			my $mes = "parsing error: STANDARD/SD in \n$whole_row".
			    "could not be coupled to an initial value.";
			$mes .=" Please check the parentheses." if ($#digits == 0 and ($whole_row =~ /[\(\)]/));
			croak($mes );
		      } elsif ($corr and ($self->type ne 'BLOCK')){
			my $mes = "parsing error: CORRELATION in \n$whole_row".
			    "could not be coupled to an initial value.";
			$mes .=" Please check the parentheses." if ($#digits == 0 and ($whole_row =~ /[\(\)]/));
			croak($mes );
		      } elsif ($chol and ($self->type ne 'BLOCK')){
			my $mes = "parsing error: CHOLESKY in \n$whole_row".
			    "could not be coupled to an initial value.";
			$mes .=" Please check the parentheses." if ($#digits == 0 and ($whole_row =~ /[\(\)]/));
			croak($mes );
		      }
		    }
		  }
		} #end else
	      } #end while string not empty
	    } # end unless comment on its own
	  } #end foreach line in recordarr
	} #end if defined record arr

	if ( $self->type eq 'BLOCK' ) {
	  $self -> fix(1)  if ($any_fixed);
	  $self -> sd(1)   if ($any_sd);
	  $self -> corr(1) if ($any_corr);
	  $self -> chol(1) if ($any_chol);
	}


	my $global_row= $self->n_previous_rows()+1;
	my $global_column= $self->n_previous_rows()+1;
	my $row = 1;
	for ( my $i = 0; $i <= $#digits; $i++ ) {
	  my $com_str = $comments[$i] =~ /\w/ ? ';'.$comments[$i] : '';
	  if ( $self->type eq 'BLOCK' ) {
	    if ( $i+1 == $row*($row+1)/2 ) {
	      $self -> _add_option( option_string => $digits[$i].$com_str,
				    on_diagonal   => 1,
				    coordinate_string => $parameter.'('.$global_row.','.$global_column.')');
	      $row++;
	      $global_row++;
	      $global_column=$self->n_previous_rows()+1;
	    } else {
	      $self -> _add_option( option_string => $digits[$i].$com_str,
				    on_diagonal   => 0,
				    coordinate_string => $parameter.'('.$global_row.','.$global_column.')');
	      $global_column++;
	      if ($global_column > $global_row){
		croak("error setting coordinate_string");
	      }
	    }
	  } else {
	    $self -> _add_option( option_string => $digits[$i].$com_str,
				  fix           => $fix[$i],
				  sd            => $sds[$i],
				  corr          => $corrs[$i],
				  chol          => $chols[$i],
				  on_diagonal   => 1,
				  coordinate_string => $parameter.'('.$global_row.','.$global_column.')');
	    $global_row++;
	    $global_column++;
	  }
	}

	if ($warn_standar_chol){
	  print "\nWarning:\n Found STANDARD/CORRELATION/CHOLESKY in control stream.\n".
	      "This is not yet supported by PsN. Errors will be introduced when\n".
	      "updating initial estimates to final estimates from previous run\n".
	      "and sumo output will be wrong.\n";
	}

      }
# line 550 libgen/model/problem/init_record.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_options');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _add_option');
# line 55 "lib/model/problem/init_record_subs.pm" 
{
	my $opt_obj = 
	  model::problem::record::init_option ->
	      new ( option_string => $option_string,
		    on_diagonal   => $on_diagonal,
		    sd            => $sd,
		    corr          => $corr,
		    chol          => $chol,
		    fix           => $fix,
		    coordinate_string => $coordinate_string);
	$self->options([]) unless defined $self->options;
	push( @{$self->options}, $opt_obj ) if( $opt_obj ); 
}
# line 611 libgen/model/problem/init_record.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _add_option');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _format_record');
# line 75 "lib/model/problem/init_record_subs.pm" 
{
	my @class_names = split('::',ref($self));
	my $fname = uc(pop(@class_names));
	$formatted[0] = "\$".$fname." ";
	my $len;

	my $otype = $self->type;
	my $same  = $self->same;
	my $fix   = $self->fix;
	my $size  = $self->size;

	if ( defined $otype ) {
	  $formatted[0] = $formatted[0]." $otype";
	  if ( defined $size ) {
	    $formatted[0] = $formatted[0]."($size)";
	  }
	  if ( $same) {
	    $formatted[0] = $formatted[0]." SAME";
	  }
	  if ( $self -> sd() ) {
	    $formatted[0] = $formatted[0]." STANDARD";
	  }
	  if ( $self -> corr() ) {
	    $formatted[0] = $formatted[0]." CORRELATION";
	  }
	  if ( $self -> chol() ) {
	    $formatted[0] = $formatted[0]." CHOLESKY";
	  }
	  if ($fix) {
	    $formatted[0] = $formatted[0]." FIX";
	  }
	  $formatted[0] = $formatted[0]."\n";
	}
	my $i = 0;
	$len = length $formatted[0];
	if ( defined $self->options ) {
	  foreach my $option ( @{$self->options} ) {
	    my ($form,$no_break) = $option -> _format_option(number_format => $number_format);
	    if (($len+length(' '.$form)) > 150){
 	      #must add linebreak if very long lines. Assume NM7 if we get 
	      #this problem, allow 150 characters
 	      $formatted[0] = $formatted[0]."\n";
	      $len = 0;
 	    }
	    $formatted[0] = $formatted[0].' '.$form;
	    $len += length(' '.$form);
	    if ($no_break){
	      1;
	      #$formatted[0] = $formatted[0].' ';
	      #$len += length(' ');
	    }else{
	      $formatted[0] = $formatted[0]."\n";
	      $len = 0;
	    }
#	    $formatted[0] = $formatted[0].' '.
#	      $option -> _format_option( len => $len)."\n";
	  }
	} else {
	  $formatted[0] = $formatted[0]."\n";
	}

	if ( defined $self->comment ) {
	  push( @formatted, @{$self->comment} );
	  $formatted[$#formatted] = $formatted[$#formatted];
	}
      }
# line 720 libgen/model/problem/init_record.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _format_record');
	# End of Non-Dia code #

	return \@formatted;
}

1;

