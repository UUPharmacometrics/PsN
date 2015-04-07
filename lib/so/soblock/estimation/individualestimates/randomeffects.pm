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

    my $mean;
    if (defined $self->EffectMean) {
        $mean = $self->EffectMean->xml();
    }

    my $median;
    if (defined $self->EffectMedian) {
        $median = $self->EffectMedian->xml();
    }

    my $est;
    if (defined $mean or defined $median) {
        $est = XML::LibXML::Element->new("RandomEffects");

        if (defined $mean) {
            $est->appendChild($mean);
        }

        if (defined $median) {
            $est->appendChild($median);
        }
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
