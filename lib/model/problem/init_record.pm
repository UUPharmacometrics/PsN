package model::problem::init_record;
#use Carp;
use include_modules;
use model::problem::record::init_option;
use linear_algebra;
use Math::Random;
use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

has 'type' => ( is => 'rw', isa => 'Str' );
has 'prior' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'size' => ( is => 'rw', isa => 'Str' );
has 'same' => ( is => 'rw', isa => 'Bool', default => '0' );
has 'fix' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'sd' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'chol' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'corr' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'n_previous_rows' => ( is => 'rw', isa => 'Maybe[Int]', default => 0 );

sub store_inits
{
	my $self = shift;

	if ( defined $self->options ) {
	  foreach my $option ( @{$self->options} ){
	    if( $option -> can( 'store_init' ) ){
	      $option -> store_init;
	    }
	  }
	}
}

sub restore_inits
{
	my $self = shift;

	if ( defined $self->options ) {
	  foreach my $option ( @{$self->options} ) {
	    $option->restore_init;
	  }
	}
}

sub set_random_inits
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 degree => { isa => 'Num', default => 0.1, optional => 1 },
		 bound_record => { isa => 'model::problem::init_record', optional => 1 }
	);
	#theta.pm has overloaded set_random_inits. This one is only for omega sigma
	my $degree = $parm{'degree'};
	my $bound_record = $parm{'bound_record'};

	if (($degree >= 1) or ($degree <= 0)){
		croak("Illegal input to init_record->set_random_inits, degree $degree is not between 0 and 1");
	}

	return if ($self->fix or $self->prior or $self->same);

	unless (defined $bound_record){
		$bound_record = $self;
	}
	my $do_cholesky=0;
	if ($self->type eq 'BLOCK' and (defined $self->size) and $self->size > 1){
		$do_cholesky=1;
	}
	my $nopt = scalar(@{$bound_record->options});
	unless (defined $self->options and scalar(@{$self->options})==$nopt){
		croak("bug in init_record->set_random_inits: bound_record does not match self" );
	}
	for (my $attempt=0; $attempt<100; $attempt++){
		my $matrix=[];
		if ($do_cholesky){
			for (my $k=0; $k< $self->size; $k++){
				push(@{$matrix},[(0) x $self->size]);
			}
		}
		my $row=0;
		my $col=0;
		for (my $j=0; $j< $nopt; $j++){
			my $option = $bound_record->options->[$j];
			if ($option->fix ){
				#this can happen if diagonal omega/sigma, there options are fix/unfix individually
				if ($self->type eq 'BLOCK'){
					print "";
				}
				$col++;
				if ($col>$row){
					$row++;
					$col=0;
				}
				next;
			}
			if ($option->init == 0){
				$col++;
				if ($col>$row){
					$row++;
					$col=0;
				}
				next;
			}
			my $range = $option->get_range(degree => $degree);

			my $val;
			for (my $k=0; $k<1000; $k++){
				$val = random_uniform(1, $range->[0], $range->[1] );
				last unless ($val == 0);
			}
			if ($do_cholesky){
				$matrix->[$row]->[$col]=$val;
				$matrix->[$col]->[$row]=$val;
				$col++;
				if ($col>$row){
					$row++;
					$col=0;
				}
			}
			$self->options->[$j]->check_and_set_init(new_value=>$val);
		}#end loop over options
		my $accept=1;
		if ($do_cholesky){
			#if get numerical error on cholesky then do not accept
			my $err = linear_algebra::cholesky($matrix );
			if ($err == 1){
				$accept = 0;
			}
		}
		last if $accept;
	} #end loop attempts

}

sub _read_options
{
	my $self = shift;

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

sub _add_option
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 option_string => { isa => 'Str', optional => 1 },
		 fix => { isa => 'Bool', default => 0, optional => 1 },
		 comment => { isa => 'Str', optional => 1 },
		 coordinate_string => { isa => 'Str', optional => 0 },
		 on_diagonal => { isa => 'Bool', optional => 1 },
		 sd => { isa => 'Bool', default => 0, optional => 1 },
		 chol => { isa => 'Bool', default => 0, optional => 1 },
		 corr => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $option_string = $parm{'option_string'};
	my $fix = $parm{'fix'};
	my $comment = $parm{'comment'};
	my $coordinate_string = $parm{'coordinate_string'};
	my $on_diagonal = $parm{'on_diagonal'};
	my $sd = $parm{'sd'};
	my $chol = $parm{'chol'};
	my $corr = $parm{'corr'};

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

sub _format_record
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_format => { isa => 'Maybe[Int]', optional => 1 },
	);
	my $number_format = $parm{'number_format'};
	my @formatted;

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
	    }else{
	      $formatted[0] = $formatted[0]."\n";
	      $len = 0;
	    }
	  }
	} else {
	  $formatted[0] = $formatted[0]."\n";
	}

	if ( defined $self->comment ) {
	  push( @formatted, @{$self->comment} );
	  $formatted[$#formatted] = $formatted[$#formatted];
	}

	return \@formatted;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
