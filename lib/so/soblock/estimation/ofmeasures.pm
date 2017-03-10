package so::soblock::estimation::ofmeasures;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'Deviance' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'IndividualContributionToLL' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $dev) = $xpc->findnode('x:Deviance', $node);
    $self->Deviance($dev->textContent) if (defined $dev);

    (my $ictll) = $xpc->findnodes('x:IndividualContributionToLL', $node);
    if (defined $ictll) {
        my $table = so::table->new();
        $table->parse($ictll);
        $self->IndividualContributionToLL($table);
    }
}

sub xml
{
    my $self = shift;

    my $l;

    my $dev;
    if (defined $self->Deviance) {
        $dev = XML::LibXML::Element->new("Deviance");
        $dev->appendTextNode($self->Deviance);
    }

    my $ictll;
    if (defined $self->IndividualContributionToLL) {
        $ictll = $self->IndividualContributionToLL->xml();
    }

    if (defined $dev or defined $ictll) {
        $l = XML::LibXML::Element->new("OFMeasures");
        if (defined $dev) {
            $l->appendChild($dev);
        }
        if (defined $ictll) {
            $l->appendChild($ictll);
        }
    }

    return $l;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
