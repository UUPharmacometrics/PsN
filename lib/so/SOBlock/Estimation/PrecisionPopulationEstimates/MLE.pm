package so::SOBlock::Estimation::PrecisionPopulationEstimates::MLE;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'CovarianceMatrix' => ( is => 'rw', isa => 'so::matrix' );
has 'CorrelationMatrix' => ( is => 'rw', isa => 'so::matrix' );
has 'StandardError' => ( is => 'rw', isa => 'so::table' );
has 'RelativeStandardError' => ( is => 'rw', isa => 'so::table' );


sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("MLE");
    
    if (defined $self->CovarianceMatrix) {
        my $xml = $self->CovarianceMatrix->xml();
        $est->appendChild($xml);
    }

    if (defined $self->CorrelationMatrix) {
        my $xml = $self->CorrelationMatrix->xml();
        $est->appendChild($xml);
    }

    if (defined $self->StandardError) {
        my $xml = $self->StandardError->xml();
        $est->appendChild($xml);
    }

    if (defined $self->RelativeStandardError) {
        my $xml = $self->RelativeStandardError->xml();
        $est->appendChild($xml);
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
