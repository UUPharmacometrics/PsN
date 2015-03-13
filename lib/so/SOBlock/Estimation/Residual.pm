package so::SOBlock::Estimation::Residual;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'ResidualTable' => ( is => 'rw', isa => 'so::table' );

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("IndividualEstimates");
    
    if (defined $self->ResidualTable) {
        my $xml = $self->ResidualTable->xml();
        $est->appendChild($xml);
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
