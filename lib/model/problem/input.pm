package model::problem::input;

use Moose;
use MooseX::Params::Validate;
use Carp;

extends 'model::problem::record';

sub BUILD
{
    my $self = shift;

    # Check if there are any reserved words containing lower case. Starting with NONMEM 7.2 this is allowed but PsN does not support it
    foreach my $option (@{$self->options}) {
        foreach my $string ($option->name, $option->value) {
            if ((defined $string) and
                ($string =~ /^(ID|L1|L2|DV|MDV|RAW_\w+|MRG_\w+|RPT_\w+|TIME|DATE|DAT1|DAT2|DAT3|DROP|SKIP|EVID|AMT|RATE|SS|II|ADDL|CMT|PCMT|CALL|CONT)$/i)) {
                if ($string =~ /[a-z]/) {
                    croak("\$INPUT contains a NONMEM reserved word \"$string\" containing lowercase letters. This is not yet supported by PsN." .
                        " Please use all uppercase letters.");
                }
            }
        }
    }
}

sub remove_drop_column_names
{
    my $self = shift;
    foreach my $option (@{$self->options}) {
        if ($option->name eq 'DROP' or $option->name eq 'SKIP' or (defined $option->value and ($option->value eq 'SKIP' or $option->value eq 'DROP'))) {
            $option->name('DROP');
            $option->clear_option_value;
        }
    }

}

sub get_nonskipped_columns
{
    my $self = shift;

    my @option_list = ();

    foreach my $option (@{$self->options}) {
        if ($option->name ne 'DROP' && $option->name ne 'SKIP' && ((not defined $option->value) or ($option->value ne 'SKIP' && $option->value ne 'DROP'))) {
            push @option_list, $option->name;
        }
    }

    return \@option_list;
}

sub have_column
{
    # Do we have a certain symbol?
    my $self = shift;
    my %parm = validated_hash(\@_,
        column => { isa => 'Str' },
    );
    my $column = $parm{'column'};


    my $cols = $self->get_nonskipped_columns();

    for my $col (@$cols) {
        return 1 if ($col eq $column)
    }
    return 0;
}

sub get_filter_table_names
{
    my $self = shift;

    my @filter_table_names = ();
    my $first_undropped;
    my $time_in_input = 0;
    my $datx_in_input = 0;
    my $time_added = 0;


    #not including DROP SKIP
    #my @reserved = qw(ID L1 L2 DV MDV RAW_ MRG_ RPT_ TIME DATE DAT1 DAT2 DAT3 EVID AMT RATE SS II ADDL CMT PCMT CALL CONT);

    foreach my $option (@{$self->options}) {
        if ($option->name ne 'DROP' && $option->name ne 'SKIP' and
            ((not defined $option->value) or ($option->value ne 'SKIP' and $option->value ne 'DROP'))) {
            unless (defined $first_undropped){
                $first_undropped =  $option->name;
            }
            unless ($time_in_input){
                if (($option->name eq 'TIME') or (defined $option->value and ($option->value eq 'TIME'))){
                    $time_in_input=1;
                }
            }
        }
        unless ($datx_in_input){
            #regardless if drop or not
            foreach my $col ('DATE','DAT1','DAT2','DAT3') {
                if (($option->name eq $col) or ((defined $option->value) and $option->value eq $col)){
                    $datx_in_input=1;
                    last;
                }
            }
        }
    }
    unless (defined $first_undropped){
        return (undef,0); #croak("found no undropped columns in model");
    }
    #FIXME what about synonyms? do we want reserved label or synonym here?
    foreach my $option (@{$self->options}) {
        if ($option->name ne 'DROP' and $option->name ne 'SKIP' and
            ((not defined $option->value) or ($option->value ne 'SKIP' && $option->value ne 'DROP'))) {
            push (@filter_table_names, $option->name);
        }else{
            push (@filter_table_names,$first_undropped);
        }
    }

    unless ($time_in_input){
        if ($datx_in_input){
            push (@filter_table_names,'TIME');
            $time_added=1;
        }
    }
    return (\@filter_table_names,$time_added);
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
