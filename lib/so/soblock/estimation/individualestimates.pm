package so::soblock::estimation::individualestimates;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::soblock::estimation::individualestimates::estimates;
use so::soblock::estimation::individualestimates::randomeffects;

has 'Estimates' => ( is => 'rw', isa => 'so::soblock::estimation::individualestimates::estimates' );
has 'RandomEffects' => ( is => 'rw', isa => 'so::soblock::estimation::individualestimates::randomeffects' );

sub BUILD
{
    my $self = shift;

    my $est = so::soblock::estimation::individualestimates::estimates->new();
    $self->Estimates($est);

    my $rand = so::soblock::estimation::individualestimates::randomeffects->new();
    $self->RandomEffects($rand);
}

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $est) = $xpc->findnodes('x:Estimates', $node);
    $self->Estimates->parse($est) if (defined $est);

    (my $re) = $xpc->findnodes('x:RandomEffects', $node);
    $self->RandomEffects->parse($re) if (defined $re);
}

sub xml
{
    my $self = shift;

    my $ind_est;

    my $est = $self->Estimates->xml();
    my $rand = $self->RandomEffects->xml();

    if (defined $est or defined $rand) {
        $ind_est = XML::LibXML::Element->new("IndividualEstimates");
        if (defined $est) {
            $ind_est->appendChild($est);
        }
        if (defined $rand) {
            $ind_est->appendChild($rand);
        }
    }

    return $ind_est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
