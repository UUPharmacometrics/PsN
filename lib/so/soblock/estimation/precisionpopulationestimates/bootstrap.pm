package so::soblock::estimation::precisionpopulationestimates::bootstrap;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;

has 'PercentilesCI' => ( is => 'rw', isa => 'so::table' );
has 'AsymptoticCI' => ( is => 'rw', isa => 'so::table' );
has 'StandardError' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $perc) = $xpc->findnodes('x:PercentilesCI', $node);
    if (defined $perc) {
        my $table  = so::table->new();
        $table->parse($perc);
        $self->PercentilesCI($table);
    }

    (my $prec) = $xpc->findnodes('x:AsymptoticCI', $node);
    if (defined $prec) {
        my $table = so::table->new();
        $table->parse($prec);
        $self->AsymptoticCI($table);
    }

    (my $prec) = $xpc->findnodes('x:StandardError', $node);
    if (defined $prec) {
        my $table = so::table->new();
        $table->parse($prec);
        $self->StandardError($table);
    }
}

sub xml
{
    my $self = shift;

    my $bootstrap;

    if (defined $self->PercentilesCI or defined $self->AsymptoticCI or defined $self->StandardError) {
        $bootstrap = XML::LibXML::Element->new("OtherMethod");
        $bootstrap->setAttribute("method", "Bootstrap");
    }

    if (defined $self->StandardError) {
        my $xml = $self->StandardError->xml();
        $bootstrap->appendChild($xml);
    }

    if (defined $self->AsymptoticCI) {
        my $xml = $self->AsymptoticCI->xml();
        $bootstrap->appendChild($xml);
    }

    if (defined $self->PercentilesCI) {
        my $xml = $self->PercentilesCI->xml();
        $bootstrap->appendChild($xml);
    }

    return $bootstrap;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
