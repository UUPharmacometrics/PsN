package model::problem::record::option;

use Moose;
use MooseX::Params::Validate;

has 'option_string' => ( is => 'rw', isa => 'Maybe[Str]', clearer => 'clear_option_string' );
has 'name' => ( is => 'rw', isa => 'Str' );
has 'value' => ( is => 'rw', isa => 'Maybe[Str]' );

sub BUILD
{
	my $self  = shift;

	if ( defined $self->option_string ) {
		$self->_read_option;
		$self->clear_option_string;
	}
}

sub _read_option
{
	my $self = shift;

	#this gets strange for $PRIOR which has  NWPRI NTHETA=4,NETA=4,NTHP=4,NETP=4
	my $line = $self->option_string;
	chomp( $line );
	$line =~ s/^\s+//;
	$line =~ s/\s+$//;
	my @option = split( "=", $line ); # NTHETA    4,NETA    4,NTHP   4,NETP   4
	$self->name(shift( @option ));
	$self->value(join( "=", @option )); #4,NETA=4,NTHP=4,NETP=4
}

sub _format_option
{
	my $self = shift;
	my $formatted = $self->name;

	if (defined $self->value and $self->value ne '') {
		$formatted = $formatted . '=' . $self->value; #NTHETA=4,NETA=4,NTHP=4,NETP=4
	}

	return $formatted;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
