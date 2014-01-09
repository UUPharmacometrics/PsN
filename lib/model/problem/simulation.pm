package model::problem::simulation;

use Moose;
use MooseX::Params::Validate;

use model::problem::record::option;

extends 'model::problem::record';

has 'seed1' => (is => 'rw', isa => 'Int');
has 'seed2' => (is => 'rw', isa => 'Int');

sub get_or_set_seed
{
	my $self = shift;
	my $seed_number = shift;
	my $seed = shift;

	my @seed_option;
	my @current_seed;

	foreach my $option (@{$self->options}) {
		if ($option->name =~ /(\d+)/) {
			if (not defined $seed_option[0]) {
				$current_seed[0] = $1;
				$seed_option[0] = $option;
			} elsif (not defined $seed_option[1]) {
				$current_seed[1] = $1;
				$seed_option[1] = $option;
				last;
			}
		}
	}

	if (defined $seed) {
		my $option = $seed_option[$seed_number - 1];
		if (defined $option) {
			my $name = $option->name;
			$name =~ s/\d+/$seed/;
			$option->name($name);
		} else {
			my $option = model::problem::record::option->new(name => "($seed)");
			if ($seed_number == 1) {
				unshift @{$self->options}, $option;
			} else {
				if (defined $seed_option[0]) {
					# Insert after closing paren
					my $pos;
					for ($pos = 0; $pos < @{$self->options}; $pos++) {
						last if $self->options->[$pos]->name =~ /\)/;
					}
					splice @{$self->options}, $pos + 1, 0, $option;
				} else {
					croak("Seed2 need to be set after seed1");
				}
			}
		}
	} else {
		return $current_seed[$seed_number - 1];
	}
}

around 'seed1' => sub
{
	my $orig = shift;
	my $self = shift;
	my $seed = shift;

	$self->get_or_set_seed(1, $seed);
};

around 'seed2' => sub
{
	my $orig = shift;
	my $self = shift;
	my $seed = shift;

	$self->get_or_set_seed(2, $seed);
};

no Moose;
__PACKAGE__->meta->make_immutable;
1;
