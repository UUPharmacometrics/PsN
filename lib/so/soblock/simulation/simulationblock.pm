package so::soblock::simulation::simulationblock;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;

has 'replicate' => ( is => 'rw', isa => 'Str' );
has 'SimulatedProfiles' => ( is => 'rw', isa => 'so::table' );
has 'IndivParameters' => ( is => 'rw', isa => 'so::table' );
has 'RandomEffects' => ( is => 'rw', isa => 'so::table' );
has 'Covariates' => ( is => 'rw', isa => 'so::table' );
has 'PopulationParameters' => ( is => 'rw', isa => 'so::table' );
has 'Dosing' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    my @attributes = ("SimulatedProfiles", "IndivParameters", "RandomEffects", "Covariates", "PopulationParameters", "Dosing");
    foreach my $attr (@attributes) {
        (my $subnode) = $xpc->findnodes("x:$attr", $node);
        if (defined $subnode) {
            my $table = so::table->new();
            $table->parse($subnode);
            $self->$attr($table);
        }
    }
}

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

    return $block;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
