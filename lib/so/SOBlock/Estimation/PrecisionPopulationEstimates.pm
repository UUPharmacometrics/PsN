package so::SOBlock::Estimation::PrecisionPopulationEstimates;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'MLE' => ( is => 'rw', isa => 'so::SOBlock::Estimation::PrecisionPopulationEstimates::MLE' );
has 'Bootstrap' => ( is => 'rw', isa => 'so::SOBlock::Estimation::PrecisionPopulationEstimates::Bootstrap' );

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("PrecisionPopulationEstimates");
    
    if (defined $self->MLE) {
        my $xml = $self->MLE->xml();
        $est->appendChild($xml);
    }

    if (defined $self->Bootstrap) {
        my $xml = $self->Bootstap->xml();
        $est->appendChild($xml);
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
