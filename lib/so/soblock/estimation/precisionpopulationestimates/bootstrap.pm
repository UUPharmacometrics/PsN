package so::soblock::estimation::precisionpopulationestimates::bootstrap;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;

has 'Percentiles' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $perc) = $xpc->findnodes('x:Percentiles', $node);
    $self->Percentiles->parse($perc) if (defined $perc);
}

sub xml
{
    my $self = shift;

    my $est;

    if (defined $self->Percentiles) {
        $est = XML::LibXML::Element->new("Bootstrap");
        my $xml = $self->Percentiles->xml();
        $est->appendChild($xml);
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
