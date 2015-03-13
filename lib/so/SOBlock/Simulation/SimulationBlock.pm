package so::SOBlock::Simulation::SimulationBlock;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'replicate' => ( is => 'rw', isa => 'Str' );
has 'SimulatedProfiles' => ( is => 'rw', isa => 'so::table' );
has 'IndivParameters' => ( is => 'rw', isa => 'so::table' );
has 'RandomEffects' => ( is => 'rw', isa => 'so::table' );
has 'Covariates' => ( is => 'rw', isa => 'so::table' );
has 'PopulationParameters' => ( is => 'rw', isa => 'so::table' );
has 'Dosing' => ( is => 'rw', isa => 'so::table' );

sub xml
{
    my $self = shift;

    my $block = XML::LibXML::Element->new("SimulationBlock");
    $block->setAttribute("replicate", $self->replicate);

    my @attributes = ("SimulatedProfiles", "IndivParameters", "RandomEffects", "Covariates", "PopulationParameters", "Dosing");
    foreach my $attr (@attributes) {
        if (defined $self->$attr) {
            my $xml = $self->$attr->xml();
            $block->appendChild($xml);
        }
    }

    return $sim;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
