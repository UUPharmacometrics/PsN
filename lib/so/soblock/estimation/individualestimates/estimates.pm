package so::soblock::estimation::individualestimates::estimates;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;

has 'Mean' => ( is => 'rw', isa => 'so::table' );
has 'Median' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $mean) = $xpc->findnodes('x:Mean', $node);
    if (defined $mean) {
        my $table = so::table->new();
        $table->parse($mean);
        $self->Mean($table);
    }

    (my $median) = $xpc->findnodes('x:Median', $node);
    if (defined $median) {
        my $table = so::table->new();
        $table->parse($median);
        $self->Median($table);
    }
}

sub xml
{
    my $self = shift;

    my $mean;
    if (defined $self->Mean) {
        $mean = $self->Mean->xml();
    }

    my $median;
    if (defined $self->Median) {
        $median = $self->Median->xml();
    }

    my $est;
    if (defined $mean or defined $median) {
        $est = XML::LibXML::Element->new("Estimates");

        if (defined $mean) {
            $est->appendChild($mean);
        }

        if (defined $median) {
            $est->appendChild($median);
        }
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
