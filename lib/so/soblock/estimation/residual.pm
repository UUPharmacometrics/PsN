package so::soblock::estimation::residual;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::table;

has 'ResidualTable' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();
   
    (my $rt)= $xpc->findnodes($node);
    $self->ResidualTable->parse($rt) if (defined $rt);
}

sub xml
{
    my $self = shift;

    my $res;
    if (defined $self->ResidualTable) {
        $res = $self->ResidualTable->xml();
    }

    my $est;
    if (defined $res) {
        $est = XML::LibXML::Element->new("Residual");
    
        if (defined $res) {
            $est->appendChild($res);
        }
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
