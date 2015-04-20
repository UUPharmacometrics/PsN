package so::soblock::taskinformation::runtime;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'Description' => ( is => 'rw', isa => 'Str' );
has 'Real' => ( is => 'rw', isa => 'Str' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $desc) = $xpc->findnodes('ct:Description');
    $self->Description($desc);

    (my $real) = $xpc->findnodes('ct:Real');
    $self->Real($real);
}

sub xml
{
    my $self = shift;

    my $rt;
    if (defined $self->Real) {
        $rt = XML::LibXML::Element->new("RunTime");

        if (defined $self->Description) {
            my $desc = XML::LibXML::Element->new("ct:Description");
            $desc->appendTextNode($self->Description);
            $rt->appendChild($desc);
        }

        my $real = XML::LibXML::Element->new("ct:Real");
        $real->appendTextNode($self->Real);

        $rt->appendChild($real);
    }

    return $rt;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
