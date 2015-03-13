package standardised_output::matrix;

# Class representing a generic SO matrix

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use standardised_output::xml;

has 'RowNames' => ( is => 'rw', isa => 'ArrayRef' );
has 'ColumnNames' => ( is => 'rw', isa => 'ArrayRef' );
has 'MatrixRow' => ( is => 'rw', isa => 'ArrayRef' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = standardised_output::xml::get_xpc();

    my @row_names = $xpc->findnodes('ct:Matrix/ct:RowNames/*', $node);
    $self->RowNames([]);
    foreach my $row (@row_names) {
        push @{$self->RowNames}, $row->textContent;
    }

    my @col_names = $xpc->findnodes('ct:Matrix/ct:ColumnNames/*', $node);
    $self->ColumnNames([]);
    foreach my $col (@col_names) {
        push @{$self->ColumnNames}, $col->textContent;
    }

    my @rows = $xpc->findnodes('ct:Matrix/ct:MatrixRow', $node);
    $self->MatrixRow([]);
    foreach my $row (@rows) {
        my $current_row = [];
        push @{$self->MatrixRow}, $current_row;
        my @cols = $xpc->findnodes('ct:Real', $row);
        foreach my $col (@cols) {
            push @{$current_row}, $col->textContent;
        }
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
