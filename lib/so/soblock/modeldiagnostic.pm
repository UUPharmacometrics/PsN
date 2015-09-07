package so::soblock::modeldiagnostic;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

use so::table;

has 'VPC' => ( is => 'rw', isa => 'so::table' );

sub BUILD
{
    my $self = shift;

}

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $vpc) = $xpc->findnodes('x:VPC', $node);
    if (defined $vpc) {
        my $table = so::table->new();
        $table->parse($vpc);
        $self->VPC($table);
    }
}

sub xml
{
    my $self = shift;

    my $md;

    my $vpc;
    if (defined $self->VPC) {
        $vpc = $self->VPC->xml();
    }

    if (defined $vpc) {
        $md = XML::LibXML::Element->new("ModelDiagnostic");
        $md->appendChild($vpc);
    }

    return $md;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
