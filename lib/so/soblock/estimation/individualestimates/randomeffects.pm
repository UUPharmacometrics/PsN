package so::soblock::estimation::individualestimates::randomeffects;

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;

has 'EffectMean' => ( is => 'rw', isa => 'so::table' );
has 'EffectMedian' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $em) = $xpc->findnodes('x:EffectMean', $node);
    if (defined $em) {
        my $table = so::table->new();
        $table->parse($em);
        $self->EffectMean($table);
    }

    (my $emed) = $xpc->findnodes('x:EffectMedian', $node);
    if (defined $emed) {
        my $table = so::table->new();
        $table->parse($emed);
        $self->EffectMedian($table);
    }
}

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

1;
