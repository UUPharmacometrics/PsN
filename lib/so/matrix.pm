package so::matrix;

# Class representing a generic SO matrix

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::xml;

has 'name' => ( is => 'rw', isa => 'Str' );
has 'RowNames' => ( is => 'rw', isa => 'ArrayRef' );
has 'ColumnNames' => ( is => 'rw', isa => 'ArrayRef' );
has 'MatrixRow' => ( is => 'rw', isa => 'ArrayRef' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

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

sub xml
{
    my $self = shift;

    my $matrix_xml = XML::LibXML::Element->new($self->name);

    if (defined $self->RowNames and defined $self->ColumnNames and defined $self->MatrixRow) {
        $matrix_xml = XML::LibXML::Element->new("ct:Matrix");
        $matrix_xml->setAttribute(matrixType => "Any");

        my $rownames_xml = XML::LibXML::Element->new("ct:RowNames");
        $matrix_xml->appendChild($rownames_xml);
        foreach my $name (@{$self->RowNames}) {
            my $string = XML::LibXML::Element->new("ct:String");
            $string->appendTextNode($name);
            $rownames_xml->appendChild($string);
        }

        my $colnames_xml = XML::LibXML::Element->new("ct:ColumnNames");
        $matrix_xml->appendChild($colnames_xml);
        foreach my $name (@{$self->ColumnNames}) {
            my $string = XML::LibXML::Element->new("ct:String");
            $string->appendTextNode($name);
            $colnames_xml->appendChild($string);
        }

        for my $row (@{$self->MatrixRow}) {
            my $matrix_row = XML::LibXML::Element->new("ct:MatrixRow");
            $matrix_xml->appendChild($matrix_row);
            for my $element (@$row) {
                my $real = XML::LibXML::Element->new("ct:Real");
                $real->appendTextNode($element);
                $matrix_row->appendChild($real);
            }
        }
    }

    return $matrix_xml;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
