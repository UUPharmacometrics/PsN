package so::SOBlock::Estimation::IndividualEstimates::Estimates;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'Mean' => ( is => 'rw', isa => 'so::table' );
has 'Median' => ( is => 'rw', isa => 'so::table' );

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("Estimates");
    
    if (defined $self->Mean) {
        my $xml = $self->Mean->xml();
        $est->appendChild($xml);
    }

    if (defined $self->Median) {
        my $xml = $self->Median->xml();
        $est->appendChild($xml);
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
