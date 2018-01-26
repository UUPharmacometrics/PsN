package code;

# Module to generate (and potentially parse) abbreviated code

use strict;
use warnings;
use include_modules;
use MooseX::Params::Validate;

sub generate_sum
{
    # Generate a sum of numbers as abbreviated code
	my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
        terms => { isa => 'ArrayRef' },
    );
    my $name = $parm{'name'};
    my $terms = $parm{'terms'};

    return "$name = " . join(' + ', @$terms);
}

1;
