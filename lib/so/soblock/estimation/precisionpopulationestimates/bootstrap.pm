package so::soblock::estimation::precisionpopulationestimates::bootstrap;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;

has 'Percentiles' => ( is => 'rw', isa => 'so::table' );
has 'PrecisionEstimates' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $perc) = $xpc->findnodes('x:Percentiles', $node);
    if (defined $perc) {
        my $table  = so::table->new();
        $table->parse($perc);
        $self->Percentiles($table);
    }

    (my $prec) = $xpc->findnodes('x:PrecisionEstimates', $node);
    if (defined $prec) {
        my $table = so::table->new();
        $table->parse($prec);
        $self->PrecisionEstimates($table);
    }
}

sub xml
{
    my $self = shift;

    my $bootstrap;

    if (defined $self->Percentiles or defined $self->PrecisionEstimates) {
        $bootstrap = XML::LibXML::Element->new("Bootstrap");
    }

    if (defined $self->PrecisionEstimates) {
        my $xml = $self->PrecisionEstimates->xml();
        $bootstrap->appendChild($xml);
    }

    if (defined $self->Percentiles) {
        my $xml = $self->Percentiles->xml();
        $bootstrap->appendChild($xml);
    }

    return $bootstrap;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
