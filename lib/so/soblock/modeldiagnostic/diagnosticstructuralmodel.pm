package so::soblock::modeldiagnostic::diagnosticstructuralmodel;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

use so::table;

has 'IndivObservationPrediction' => ( is => 'rw', isa => 'so::table');
has 'VPC' => ( is => 'rw', isa => 'so::table');

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $iop) = $xpc->findnodes('x:IndivObservationPrediction', $node);
    if (defined $iop) {
        my $table = so::table->new();
        $table->parse($iop);
        $self->IndivObservationPrediction($table);
    }

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

    my $dsm;

    my $iop;
    if (defined $self->IndivObservationPrediction) {
        $iop = $self->IndivObservationPrediction->xml();
    }
    my $vpc;
    if (defined $self->VPC) {
        $vpc = $self->VPC->xml();
    }

    if (defined $iop or defined $vpc) {
        $dsm = XML::LibXML::Element->new("DiagnosticStructuralModel");
		if (defined $iop) {
        	$dsm->appendChild($iop);
		}
		if (defined $vpc) {
			$dsm->appendChild($vpc);
		}
    }

    return $dsm;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
