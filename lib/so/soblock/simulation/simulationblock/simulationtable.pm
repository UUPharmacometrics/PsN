package so::soblock::simulation::simulationblock::simulationtable;

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;

extends 'so::table';

has 'attr_name' => ( is => 'rw', isa => 'Str' );

sub parse
{
    my $self = shift;
    my $node = shift;

    $self->SUPER::parse($node);

    my $xpc = so::xml::get_xpc();
}

sub xml
{
    my $self = shift;

    my $xml = $self->SUPER::xml();
    if (defined $self->attr_name) {
       $xml->setAttribute("name", $self->attr_name);
    }

    return $xml;
}

1;
