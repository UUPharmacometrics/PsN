package so::SOBlock::Estimation::IndividualEstimates;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'Estimates' => ( is => 'rw', isa => 'so::SOBlock::Estimation::IndividualEstimates::Estimates' );
has 'RandomEffects' => ( is => 'rw', isa => 'so::SOBlock::Estimation::IndividualEstimates::RandomEffects' );

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("IndividualEstimates");
    
    if (defined $self->Estimates) {
        my $xml = $self->Estimates->xml();
        $est->appendChild($xml);
    }

    if (defined $self->RandomEffects) {
        my $xml = $self->RandomEffects->xml();
        $est->appendChild($xml);
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
