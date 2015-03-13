package so::SOBlock::Estimation;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'PopulationEstimates' => ( is => 'rw', isa => 'so::SOBlock::Estimation::PopulationEstimates' );
has 'PrecisionPopulationEstimates' => ( is => 'rw', isa => 'so::SOBlock::Estimation::PrecisionPopulationEstimates' );
has 'IndividualEstimates' => ( is => 'rw', isa => 'so::SOBlock::Estimation::IndividualEstimates' );
has 'PrecisionIndividualEstimates' => ( is => 'rw', isa => 'so::SOBlock::Estimation::PrecisionIndividualEstimates' );
has 'Residual' => ( is => 'rw', isa => 'so::SOBlock::Estimation::Residual' );
has 'Predictions' => ( is => 'rw', isa => 'so::table' );
has 'Likelihood' => ( is => 'rw', isa => 'so::SOBlock::Estimation::Likelihood' );

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("Estimation");
    
    if (defined $self->PopulationEstimates) {
        my $xml = $self->PopulationEstimates->xml();
        $est->appendChild($xml);
    }

    if (defined $self->PrecisionPopulationEstimates) {
        my $xml = $self->PrecisionPopulationEstimates->xml();
        $est->appendChild($xml);
    }

    if (defined $self->IndividualEstimates) {
        my $xml = $self->IndividualEstimates->xml();
        $est->appendChild($xml);
    }

    if (defined $self->PrecisionIndividualEstimates) {
        my $xml = $self->PrecisionIndividualEstimates->xml();
        $est->appendChild($xml);
    }

    if (defined $self->Residual) {
        my $xml = $self->Residual->xml();
        $est->appendChild($xml);
    }

    if (defined $self->Predictions) {
        my $xml = $self->Predictions->xml();
        $est->appendChild($xml);
    }

    if (defined $self->Likelihood) {
        my $xml = $self->Likelihood->xml();
        $est->appendChild($xml);
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
