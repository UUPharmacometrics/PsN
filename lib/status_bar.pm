package status_bar;

use Moose;
use MooseX::Params::Validate;

has 'width' => ( is => 'rw', isa => 'Int', default => 40 );
has 'sofar' => ( is => 'rw', isa => 'Int', default => 0 );
has 'step_length' => ( is => 'rw', isa => 'Int', default => 5 );
has 'steps' => ( is => 'rw', required => 1, isa => 'Int' );

sub print_step
{
	my $self = shift;
	my $output = '';

	if ( $self->sofar == 0 ) {
		$output = '|';
	} else {
		if (( $self->steps / $self->width < 1 ) and ($self->steps > 0)) {
			$output = '.' x int($self->width / $self->steps);
		} else {
	  	$output = '.';
		}
	}

	if ( $self->sofar >= $self->steps ) {
		$output .= '|';
	}

	return $output;
}

sub tick
{
	my $self = shift;
	my $return = 0;

	$self->sofar($self->sofar + 1);

	if ( $self->sofar >= $self->steps ) {
		return 1;
  }

	if ( $self->steps / $self->width < 1 ) {
		$return = 1;
  } elsif ( $self->sofar % int($self->steps / $self->width) ) {
		$return = 0;
	} else {
		$return = 1;
	}

	return $return;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
