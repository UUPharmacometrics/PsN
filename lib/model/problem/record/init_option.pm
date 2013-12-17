package model::problem::record::init_option;

#use Carp;
use include_modules;
use Math::Random;
use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record::option';

has 'init' => ( is => 'rw', isa => 'Str' );
has 'fix' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'label' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'prior' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'unit' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'stored_init' => ( is => 'rw', isa => 'Str' );
has 'coordinate_string' => ( is => 'rw', isa => 'Str' );
has 'on_diagonal' => ( is => 'rw', isa => 'Bool' );
has 'sd' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'chol' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'corr' => ( is => 'rw', isa => 'Bool', default => 0 );

sub store_init
{
	my $self = shift;

	$self->stored_init($self->init);
}

sub restore_init
{
	my $self = shift;

	$self->init($self->stored_init)
		if ( defined $self->stored_init );
}

sub set_random_init
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 degree => { isa => 'Num', default => 0.1, optional => 1 }
	);
	my $degree = $parm{'degree'};

	# Degree is 0.1*n where n is the retry number.

	my ( $sign, $est, $form );
	unless ( $self->fix or ($self->init == 0) ) {


		my $lobnd;
		if ($self->can('lobnd')) {
			# This is a theta
			$lobnd = $self->lobnd;
		} elsif (defined $self->on_diagonal and $self->on_diagonal) {		# This is an omega or a sigma off diagonal
			$lobnd = 0;
		}

		my $upbnd;
		if ($self->can('upbnd')) {
			$upbnd = $self->upbnd;
		}


		my $init  = $self->init;
	  my $change = abs($degree*$init);

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
	  $lobnd = 0.01 if ( ( $lobnd < 0.01 and $lobnd > -0.01) and $upbnd >= 0.01001 );
	  $upbnd = -0.01 if ( ( $upbnd < 0.01 and $upbnd > -0.01) and $lobnd <= -0.01001 );
	  
	  
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

sub check_and_set_init
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 new_value => { isa => 'Maybe[Num]', optional => 1 }
	);
	my $success = 0;
	my @error_code = [0,0,0];
	my $new_value = $parm{'new_value'};

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
	    } else {
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

		my $lobnd;
		if ($self->can('lobnd')) {
			# This is a theta
			$lobnd = $self->lobnd;
		} elsif (defined $self->on_diagonal and $self->on_diagonal) {		# This is an omega or a sigma off diagonal
			$lobnd = 0;
		}

		my $upbnd;
		if ($self->can('upbnd')) {
			$upbnd = $self->upbnd;
		}

	  if ( defined $lobnd and $new_value <= $lobnd ) {
	    $success = 0;
	    $error_code[2] = 1;
	  }
	  if ( defined $upbnd and $new_value >= $upbnd ) {
	    $success = 0;
	    $error_code[3] = 1;
	  }
	  if ( $success ) {
	    $self->init($new_value);
	  }
	}

	return $success, \@error_code, $new_value;
}

sub _read_option
{
	my $self = shift;

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

sub _format_option
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 len => { isa => 'Num', optional => 1 },
		 number_format => { isa => 'Maybe[Int]', optional => 1 }
	);
	my $len = $parm{'len'};
	my $formatted;
	my $number_format = $parm{'number_format'};
	my $no_break = 0;

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
	$formatted = $formatted . $str2 . $str3;

	return $formatted, $no_break;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
