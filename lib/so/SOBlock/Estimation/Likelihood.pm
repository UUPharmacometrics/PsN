package so::SOBlock::Estimation::Likelihood;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'Deviance' => ( is => 'rw', isa => 'Str' );

sub xml
{
    my $self = shift;

    my $l;

    if (defined $self->Deviance) {
        $l = XML::LibXML::Element->new("Likelihood");
        my $dev = XML::LibXML::Element->new("Deviance");
        $dev->appendTextNode($self->Deviance);
        $l->appendChild($dev);
    }

    return $l;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
