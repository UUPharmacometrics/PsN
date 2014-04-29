package model::problem::input;

use Moose;
use MooseX::Params::Validate;
use ext::Carp;

extends 'model::problem::record';

sub BUILD
{
	my $self = shift;

	# Check if there are any reserved words containing lower case. Starting with NONMEM 7.2 this is allowed but PsN does not support it
	foreach my $option (@{$self->options}) {
		foreach my $string ($option->name, $option->value) {
			if ($string =~ /ID|L1|L2|DV|MDV|RAW_|MRG_|RPT_|TIME|DATE|DAT1|DAT2|DAT3|DROP|SKIP|EVID|AMT|RATE|SS|II|ADDL|CMT|PCMT|CALL|CONT/i) {
				if ($string =~ /[a-z]/) {
					croak("\$INPUT contains a NONMEM reserved word \"$string\" containing lowercase letters. This is not yet supported by PsN." .
						" Please use all uppercase letters.");
				}
			}
		}
	}
}


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
