package so::SOBlock::Estimation::PopulationEstimates;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'MLE' => ( is => 'rw', isa => 'so::SOBlock::Estimation::PopulationEstimates::MLE' );

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("PopulationEstimates");
    
    if (defined $self->MLE) {
        my $xml = $self->MLE->xml();
        $est->appendChild($xml);
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
