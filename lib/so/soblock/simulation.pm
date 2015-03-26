package so::soblock::simulation;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::soblock::simulation::simulationblock;

has 'SimulationBlock' => ( is => 'rw', isa => 'ArrayRef[so::soblock::simulation::simulationblock]' );

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
