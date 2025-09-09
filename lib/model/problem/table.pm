package model::problem::table;

use Mouse;
use MouseX::Params::Validate;

extends 'model::problem::record';

sub renumber_file{
    my $self = shift;
    my %parm = validated_hash(\@_,
        numberstring => { isa => 'Str', optional => 0 }
        );
    my $numberstring = $parm{'numberstring'};

    my @options = defined($self->options) ? @{$self->options} : ();
    foreach my $opt (@options){
        if ($opt->name =~ /^\s*FIL/i  and (defined $opt->value and $opt->value ne '')){
            my $line = $opt->value;
            #everything up to but not including optional dot
            $line =~ s/[0-9]+[^0-9.]*/$numberstring/ ;
            $opt->value($line);
        }
    }

}

sub columns
{
    my $self = shift;

    my @columns;

    my @options = defined($self->options) ? @{$self->options} : ();
    foreach my $opt (@options) {
        if ($opt->name !~ /PRINT|NOPRINT|FILE|NOHEADER|ONEHEADER|NOTITLE|NOLABEL|
            FIRSTONLY|NOFORWARD|FORWARD|APPEND|NOAPPEND|FORMAT|LFORMAT|RFORMAT|
            ESAMPLE|WRESCHOL|SEED|RANMETHOD|UNCONDITIONAL|CONDITIONAL|OMITTED/x) {
            push @columns, $opt->name;
        }
    }

    return \@columns;
}

1;
