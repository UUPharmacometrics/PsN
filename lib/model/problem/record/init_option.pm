package model::problem::record::init_option;

use include_modules;
use Mouse;
use MouseX::Params::Validate;
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

sub get_range
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         degree => { isa => 'Num', optional => 0 },
    );
    #helper routine to set_random_inits
    my $degree = $parm{'degree'};
    if ($degree <= 0) {
        croak("Illegal input to theta_option->get_range, degree $degree must be a positive number");
    }

    my $low = $self->init - abs($degree *$self->init) ;
    if($self->on_diagonal and $low <= 0){
        $low = 1e-10; #should not end up here if init>0 and sensible degree
    }elsif ((not $self->on_diagonal) and $low <= -1000000){
        $low = -1000000 + 1;
    }
    my $high = $self->init + abs($degree *$self->init) ;
    if($high >= 1000000){
        $high = 1000000  - 1;
    }

    if ($low >= $high){
        croak("bug in init_option get range: init ".$self->init." degree $degree low $low high $high\nPlease report this\n");
    }

    return [$low,$high];
}

sub check_and_set_init
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         new_value => { isa => 'Maybe[Num]', optional => 1 }
    );
    my $success = 0;
    my @error_code = (0,0,0);
    my $new_value = $parm{'new_value'};

    # Error codes:
    # [0,0,0]:    Success
    # [1,0,0]:    Value truncated
    # [0,1,0]:    Value below lower boundary
    # [0,0,1]:    Value above upper boundary

    if ( defined $new_value ) {
      $success = 1;
      my $original=$new_value;
      if (defined $PsN::nm_major_version and (($PsN::nm_major_version == 5) or ($PsN::nm_major_version == 6))){
        if ( (($new_value < 0.000001)and ($new_value > 0) )
               or
               (($new_value > -0.00001)and ($new_value < 0) )
            ){
            #replace with 0.000001 or -0.00001, smallest nonzero number
            if ($new_value > 0){
                $new_value = "0.000001";
            }else{
                $new_value = "-0.00001";
            }
            $error_code[1] = 1;
        } else {
            #still NM6, but absolute value large enough
            #we do not handle here input that is larger than max allowed by NM6
            my $rounded;
            my $truncated;
            my $ok = 0;
            for (my $dec=6; $dec>= 0; $dec--){
                $rounded = sprintf("%.".$dec."f",$new_value);
                if (length($rounded)<=8){
                    $ok=1;
                    #remove trailing zeros, and decimal point if 0 decimals left
                    $new_value = $rounded;
                    $new_value =~ s/\.?0*$// if ($new_value =~ /\./);
                    last;
                }
            }

        }
      } else {
        #NM7, E notation or regular as needed, use all info from input
        #do not allow larger than 15, doc says double has 15 sigdig
        #$temp_value = sprintf("%.".$prec."G",$new_value);
        $new_value = sprintf("%.15G",$new_value);
      }

        my $lobnd;
        if ($self->can('lobnd')) {
            # This is a theta
            $lobnd = $self->lobnd;
        } elsif (defined $self->on_diagonal and $self->on_diagonal) {        # This is an omega or a sigma diagonal
            $lobnd = 0;
        }

        my $upbnd;
        if ($self->can('upbnd')) {
            $upbnd = $self->upbnd;
        }

      if ( defined $lobnd and $new_value <= $lobnd ) {
        $success = 0;
        $error_code[1] = 1;
      }
      if ( defined $upbnd and $new_value >= $upbnd ) {
        $success = 0;
        $error_code[2] = 1;
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
    if ($label) {
        chomp $label ;
        $label =~ s/^\s*//;
        $label =~ s/\s*$//;
    }
    if ($unit){
        chomp $unit;
        $unit =~ s/^\s*//;
        $unit =~ s/\s*$//;
    }
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
         number_format => { isa => 'Maybe[Int]', optional => 1 },
         is_block => { isa => 'Bool', optional => 1, default => 0 },
    );
    my $len = $parm{'len'};
    my $is_block = $parm{'is_block'};
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

      if (defined $number_format and $number_format < 15 and (not ($PsN::nm_major_version == 5 or $PsN::nm_major_version == 6))){
        my $form = '%.'.$number_format.'G';
        $formatted = sprintf("$form",$formatted);
      }
      $formatted =~ s/e/E/;     # The default stringification of floats always use lowercase e. NONMEM 7.1 and earlier need uppercase.

      unless ($is_block) {
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

1;
