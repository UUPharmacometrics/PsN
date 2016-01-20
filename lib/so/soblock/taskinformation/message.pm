package so::soblock::taskinformation::message;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'type' => ( is => 'rw', isa => 'Str' );
has 'Toolname' => ( is => 'rw', isa => 'Str' );
has 'Name' => ( is => 'rw', isa => 'Str' );
has 'Content' => ( is => 'rw', isa => 'Str' );
has 'Severity' => ( is => 'rw', isa => 'Int' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $type = $node->getAttribute('type');
    $self->type($type);

    my $xpc = so::xml::get_xpc();

    (my $toolname) = $xpc->findnodes('x:Toolname', $node);
    $self->Toolname($toolname);

    (my $name) = $xpc->findnodes('x:Name', $node);
    $self->Name($name);

    (my $content) = $xpc->findnodes('x:Content', $node);
    $self->Content($content);

    (my $severity) = $xpc->findnodes('x:Severity', $node);
    $self->Severity($severity);

}

sub xml
{
    my $self = shift;

    my $message = XML::LibXML::Element->new("Message");
    $message->setAttribute('type', $self->type);

    my $toolname = XML::LibXML::Element->new("Toolname");
    $toolname->appendTextNode($self->Toolname);

    my $name = XML::LibXML::Element->new("Name");
    $name->appendTextNode($self->Name);

    my $content = XML::LibXML::Element->new("Content");
    $content->appendTextNode($self->Content);

    my $severity = XML::LibXML::Element->new("Severity");
    $severity->appendTextNode($self->Severity);

    $message->appendChild($toolname);
    $message->appendChild($name);
    $message->appendChild($content);
    $message->appendChild($severity);

    return $message;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
