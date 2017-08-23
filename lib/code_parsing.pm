package code_parsing;

use strict;
use warnings;
use include_modules;
use Cwd;
use model;
use PsN;
use MooseX::Params::Validate;
use utils::file;

sub find_assignments
{
    # Find all assignments in a pk/pred record and return a hash of symbol over rhs
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my %assignments;

    (undef, my $code) = $model->get_pk_or_pred_code();

    for my $line (@$code) {
        if ($line =~ /^\s*(\w+)\s*=\s*(.*)/) {
            $assignments{$1} = $2;
        }
    }

    return \%assignments;
}

sub merge_assignments_and_expression
{
    # Merge one level of assignments into expression
   	my %parm = validated_hash(\@_,
        expression => { isa => 'Str' },
        assignments => { isa => 'HashRef' },
    );
    my $expression = $parm{'expression'};
    my $assignments = $parm{'assignments'};

    for my $symbol (keys %$assignments) {
        my $replacement = $assignments->{$symbol};
        $expression =~ s/\b$symbol\b/$replacement/;
    }

    return $expression;
}

1;
