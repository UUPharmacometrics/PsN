package model::problem::input;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

sub get_nonskipped_columns
{
	my $self = shift;

	my @option_list = ();

	foreach my $option (@{$self->options}) {
		if ($option->name ne 'DROP' && $option->name ne 'SKIP' && $option->value ne 'SKIP' && $option->value ne 'DROP') {
			push @option_list, $option->name; 
		}
	}

	return \@option_list;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
