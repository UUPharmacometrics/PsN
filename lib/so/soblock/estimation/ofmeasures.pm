package so::soblock::estimation::ofmeasures;

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'Deviance' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'IndividualContribToLL' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $dev) = $xpc->findnode('x:Deviance', $node);
    $self->Deviance($dev->textContent) if (defined $dev);

    (my $ictll) = $xpc->findnodes('x:IndividualContribToLL', $node);
    if (defined $ictll) {
        my $table = so::table->new();
        $table->parse($ictll);
        $self->IndividualContribToLL($table);
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
    if (defined $self->IndividualContribToLL) {
        $ictll = $self->IndividualContribToLL->xml();
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

1;
