package so::soblock::modeldiagnostic;

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use include_modules;
use XML::LibXML;

use so::soblock::modeldiagnostic::diagnosticstructuralmodel;
use so::table;

has 'DiagnosticStructuralModel' => ( is => 'rw', isa => 'so::soblock::modeldiagnostic::diagnosticstructuralmodel');

sub BUILD
{
    my $self = shift;

    my $dsm = so::soblock::modeldiagnostic::diagnosticstructuralmodel->new();
    $self->DiagnosticStructuralModel($dsm);
}

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $dsm) = $xpc->findnodes('x:DiagnosticStructuralModel', $node);
    $self->DiagnosticStructuralModel->parse($dsm) if (defined $dsm);
}

sub xml
{
    my $self = shift;

    my $md;

    my $dsm;
    if (defined $self->DiagnosticStructuralModel) {
        $dsm = $self->DiagnosticStructuralModel->xml();
    }

    if (defined $dsm) {
        $md = XML::LibXML::Element->new("ModelDiagnostic");
        $md->appendChild($dsm);
    }

    return $md;
}

1;
