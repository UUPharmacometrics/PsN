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

sub check_additive_eta
{
    # Check if we have an additive relationship between theta and eta in expression
    # The check will check if the expression is on the form EXPR + ETA(n) or ETA(n) + EXPR where EXPR contains at least one THETA
    # Return 0 if not an additive relationship and the number of the involved eta otherwise (which is a true value)
	my %parm = validated_hash(\@_,
        expression => { isa => 'Str' },
    );
    my $expression = $parm{'expression'};

    if ($expression =~ /\bTHETA\(\d+\).*\+\s*ETA\((\d+)\)/) {
        return $1;
    }
    if ($expression =~ /\bETA\((\d+)\)\s*\+.*THETA\(\d+\)/) {
        return $1;
    }

    return 0;
}

sub defined_symbol
{
    # Is this symbol defined in any code record?
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        symbol => { isa => 'Str' },
    );
    my $model = $parm{'model'};
    my $symbol = $parm{'symbol'};

    for my $record (('pk', 'pred', 'error', 'des', 'aes', 'aesinitial', 'mix', 'infn')) {
        if ($model->has_code(record => $record)) {  
            my $code = $model->get_code(record => $record);
            for my $line (@$code) {
                if ($line =~ /^\s*(\w+)\s*=/) {
                    if ($1 eq $symbol) {
                        return 1;
                    }
                }
            }
        }
    }
    return 0;
}

sub used_symbol
{
    # Check if a symbol is used in any LHS or RHS
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        symbol => { isa => 'Str' },
    );
    my $model = $parm{'model'};
    my $symbol = $parm{'symbol'};

    my $symbol_escaped = quotemeta($symbol);
    if (substr($symbol_escaped, -1) =~ /\w/) {      # Need word boundary here. Cannot add for ex ETA(1)
        $symbol_escaped .= '\b';
    }

    for my $record (('pk', 'pred', 'error', 'des', 'aes', 'aesinitial', 'mix', 'infn')) {
        if ($model->has_code(record => $record)) {  
            my $code = $model->get_code(record => $record);
            for my $line (@$code) {
                my @a = split /;/, $line;       # Don't match in comments
                if ($a[0] =~ /\b$symbol_escaped/) {
                    return 1;
                }
            }
        }
    }
    return 0;
}

1;
