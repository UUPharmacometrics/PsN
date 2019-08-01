package model::problem::record::theta_option;

use Moose;
use MooseX::Params::Validate;
use include_modules;

extends 'model::problem::record::init_option';

has 'upbnd' => ( is => 'rw', isa => 'Maybe[Str]', clearer => 'clear_upbnd' );
has 'lobnd' => ( is => 'rw', isa => 'Maybe[Str]', default => '-1000000', clearer => 'clear_lobnd' );

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
	if (defined $self->lobnd and $low <= $self->lobnd){
		$low = $self->lobnd + (1e-10);
	}elsif($low <= -1000000){
		$low = -1000000+1;
	}
	my $high = $self->init + abs($degree *$self->init) ;
	if (defined $self->upbnd
		and $high >= $self->upbnd){
		$high = $self->upbnd - (1e-10);
	}elsif ($high >= 1000000){
		$high = 1000000-1;
	}
	return [$low,$high];

}

sub _format_option
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_format => { isa => 'Maybe[Int]', optional => 1 },
		 is_block => { isa => 'Bool', optional => 1 }
	);
	#is_block is always ignored, only relevant for omega/sigma
	my $formatted;
	my $number_format = $parm{'number_format'};

	my $to_long = 0;
	my $modifier = 0;
	my $init = $self->init;
	$formatted = '';

	if (defined $number_format and $number_format < 15 and (not ($PsN::nm_major_version == 5 or $PsN::nm_major_version == 6))) {
	  my $form = '%.'.$number_format.'G';
	  $init = sprintf("$form",$init);
	}

	if ((defined $self->upbnd and $self->upbnd ne "") or (defined $self->lobnd and $self->lobnd ne "")) {
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

	$formatted = $formatted." ;" if ( ( defined $self->label ) or ( defined $self->unit ) );
	if ( defined $self->label ) {
	  $formatted = $formatted." ".$self->label; #don't truncate
	}

	if ( defined $self->unit ) {
	  $formatted = $formatted."; ".$self->unit; #don't truncate
	}

	return $formatted;
}

sub _read_option
{
	my $self = shift;

	my ( $line, $comment ) = split( ";", $self->option_string, 2 );

	## Split and store labels and units
	if ( defined $comment ) {
	  my ($label,$unit) = split( ";",$comment,2 );
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
	}

	# $line should be one theta now
	chomp( $line );
	#check that both opening and closing parenthesis, or neither
	$line =~ s/(.*)\((.*)\)/$1$2/;
	if ($line =~ /[()]/){
		croak("unmatched parenthesis in THETA:\n$line\nThis would trigger an NMTRAN error, aborting.\n");
	}
#	$line =~ s/\)//g;
#	$line =~ s/\(//g;
	$line =~ s/\s+//g;

	## Find fix
	$self->fix($line =~ s/FIX\w*// ? 1 : 0);

	my @inits = split( ",", $line );
	if ( $#inits <= 0) {
	  $self->init($inits[0]);
	  $self->lobnd(undef);
	  $self->upbnd(undef);
	} else {
	  $self->lobnd($inits[0]);
	  $self->init($inits[1]);
	  $self->upbnd($inits[2]);
	}

	if ( defined $self->lobnd and $self->lobnd =~ /INF/ ) {
	  $self->lobnd($PsN::config -> {'low_INF'});
	}

	if ( defined $self->upbnd and $self->upbnd =~ /INF/ ) {
	  $self->upbnd($PsN::config -> {'high_INF'});
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
