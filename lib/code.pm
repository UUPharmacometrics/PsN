package code;

# Module to generate abbreviated code

use strict;
use warnings;
use include_modules;
use MouseX::Params::Validate;
use model;

sub append_verbatim_code
{
    # Append verbatim code at verbatim section specified (LAST or FIRST)
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        code_record => { isa => 'Str' },
        pos => { isa => 'Str' },
        code => { isa => 'ArrayRef[Str]' },
    );
    my $model = $parm{'model'};
    my $code_record = $parm{'code_record'};
    my $pos = $parm{'pos'};
    my $code = $parm{'code'};

    if (not ($pos eq 'LAST' or $pos eq 'FIRST')) {
        croak("pos must be one of LAST or FIRST");
    }

    my $code_array_name = $code_record . 's';
    my $code_array = $model->problems->[0]->$code_array_name;

    my $verbatim_code;
    my $record;
    if ($pos eq 'FIRST') {
        $record = $code_array->[0];
        $verbatim_code = $record->verbatim_first;
    } else {
        $record = $code_array->[scalar(@$code_array) - 1];
        $verbatim_code = $record->verbatim_last;
    }

    if (not defined $verbatim_code) {
        $verbatim_code = [];
    }

    for my $row (@$code) {
        push @$verbatim_code, '"' . $row;
    }

    if ($pos eq 'FIRST') {
        $record->verbatim_first($verbatim_code);
    } else {
        $record->verbatim_last($verbatim_code);
    }
}

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
