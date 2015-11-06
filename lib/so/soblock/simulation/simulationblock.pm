package so::soblock::simulation::simulationblock;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;
use so::soblock::simulation::simulationblock::simulationtable;

has 'replicate' => ( is => 'rw', isa => 'Str' );
has 'SimulatedProfiles' => ( is => 'rw', isa => 'ArrayRef[so::soblock::simulation::simulationblock::simulationtable]', default => sub { [] } );
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

    my @subnodes = $xpc->findnodes("x:SimulatedProfiles", $node);
    foreach my $a (@subnodes) {
        my $table = so::soblock::simulation::simulationblock::simulationtable->new();
        $table->parse($a);
        push @{$self->SimulatedProfiles}, $table;
    }

    my @attributes = ("IndivParameters", "RandomEffects", "Covariates", "PopulationParameters", "Dosing");
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

    if (defined $self->SimulatedProfiles) {
        foreach my $a (@{$self->SimulatedProfiles}) {
            my $xml = $a->xml();
            $block->appendChild($xml); 
        }
    }

    my @attributes = ("IndivParameters", "RandomEffects", "Covariates", "PopulationParameters", "Dosing");
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
