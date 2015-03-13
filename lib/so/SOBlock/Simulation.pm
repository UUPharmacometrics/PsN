package so::SOBlock::Simulation;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'SimulationBlock' => ( is => 'rw', isa => 'ArrayRef[so::SOBlock::Simulation::SimulationBlock]' );

sub xml
{
    my $self = shift;

    my $sim;
    if (defined $self->SimulationBlock) {
        my $sim = XML::LibXML::Element->new("Simulation");
    
        foreach my $block (@{$self->SimulationBlock}) {
            my $xml = $block->xml(); 
            $sim->appendChild($xml);
        }
    }

    return $sim;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
