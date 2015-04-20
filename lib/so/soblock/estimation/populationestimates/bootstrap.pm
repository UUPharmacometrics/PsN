package so::soblock::estimation::populationestimates::bootstrap;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

use so::table;

has 'version' => ( is => 'rw', isa => 'Num', required => 1 );

has 'Mean' => ( is => 'rw', isa => 'so::table' );
has 'Median' => ( is => 'rw', isa => 'so::table' ); 

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $mean) = $xpc->findnodes('x:Mean', $node);
    if (defined $mean) {
        my $table = so::table->new();
        $table->parse($mean);
        $self->Mean($table);
    }

    (my $median) = $xpc->findnodes('x:Median', $node);
    if (defined $median) {
        my $table = so::table->new();
        $table->parse($median);
        $self->Median($table);
    }
}

sub xml
{
    my $self = shift;

    my $bootstrap;

    if ($self->version >= 0.2) {
        my $mean;
        if (defined $self->Mean) {
            $mean = $self->Mean->xml();
        }
        my $median;
        if (defined $self->Median) {
            $median = $self->Median->xml();
        }

        if (defined $mean or defined $median) {
            $bootstrap = XML::LibXML::Element->new("Bootstrap");
            if (defined $mean) {
                $bootstrap->appendChild($mean);
            }
            if (defined $median) {
                $bootstrap->appendChild($median);
            }
        }
    }

    return $bootstrap;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
