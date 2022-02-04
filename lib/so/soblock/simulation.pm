package so::soblock::simulation;

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::soblock::simulation::simulationblock;

has 'SimulationBlock' => ( is => 'rw', isa => 'ArrayRef[so::soblock::simulation::simulationblock]', default => sub { [] } );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    my @blocks = $xpc->findnodes('x:SimulationBlock', $node);

    foreach my $block (@blocks) {
        my $sb = so::soblock::simulation::simulationblock->new();
        $sb->parse($block);
        push @{$self->SimulationBlock}, $sb;
    }
}

sub xml
{
    my $self = shift;

    my $sim;
    if (scalar(@{$self->SimulationBlock}) > 0) {
        $sim = XML::LibXML::Element->new("Simulation");

        foreach my $block (@{$self->SimulationBlock}) {
            my $xml = $block->xml();
            $sim->appendChild($xml);
        }
    }

    return $sim;
}

1;
