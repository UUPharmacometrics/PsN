package so::soblock::estimation::individualestimates::randomeffects;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;

has 'EffectMean' => ( is => 'rw', isa => 'so::table' );
has 'EffectMedian' => ( is => 'rw', isa => 'so::table' );

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("RandomEffects");
    
    if (defined $self->EffectMean) {
        my $xml = $self->EffectMean->xml();
        $est->appendChild($xml);
    }

    if (defined $self->EffectMedian) {
        my $xml = $self->EffectMedian->xml();
        $est->appendChild($xml);
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
