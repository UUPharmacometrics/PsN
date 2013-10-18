package model::problem::record::theta_option;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record::init_option';

has 'upbnd' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'lobnd' => ( is => 'rw', isa => 'Maybe[Str]', default => '-1000000' );

sub _format_option
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_format => { isa => 'Maybe[Int]', optional => 1 }
	);
	my $formatted;
	my $number_format = $parm{'number_format'};

	my $to_long = 0;
	my $modifier = 0;
	my $init = $self->init;
	$formatted = '';

	if (defined $number_format and $number_format < 15 and ($PsN::nm_major_version > 6)) {
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

	$formatted = $formatted." ;" if ( ( defined $self->label ) or ( defined $self->unit ) );
	if ( defined $self->label ) {
	  $formatted = $formatted." ".$self->label; #don't truncate
	}

	if ( defined $self->unit ) {
	  $formatted = $formatted."; ".$self->unit; #don't truncate
	}

	return $formatted;
}

sub option_count
{
	my $self = shift;
	my $return_value = 0;

	return $return_value;
}

sub _read_option
{
	my $self = shift;

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
