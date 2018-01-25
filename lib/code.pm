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

    my @lines;
    my $current = "$name=";

    for (my $i = 0; $i < scalar(@$terms); $i++) {
        if (length($current) + length($terms->[$i]) + 1 > 131) {
            push @lines, "$current&";
            $current = "";
        }
        $current .= $terms->[$i];
        if ($i < scalar(@$terms) - 1) {
            $current .= "+";
        }
    }
    push @lines, $current;
    
    return \@lines;
}

1;
