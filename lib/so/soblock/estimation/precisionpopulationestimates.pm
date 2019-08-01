package so::soblock::estimation::precisionpopulationestimates;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::soblock::estimation::precisionpopulationestimates::mle;
use so::soblock::estimation::precisionpopulationestimates::bootstrap;

has 'version' => ( is => 'rw', isa => 'Num', required => 1 );

has 'MLE' => ( is => 'rw', isa => 'so::soblock::estimation::precisionpopulationestimates::mle' );
has 'Bootstrap' => ( is => 'rw', isa => 'so::soblock::estimation::precisionpopulationestimates::bootstrap' );

sub BUILD
{
    my $self = shift;

    my $mle = so::soblock::estimation::precisionpopulationestimates::mle->new(version => $self->version);
    $self->MLE($mle);
    my $bs = so::soblock::estimation::precisionpopulationestimates::bootstrap->new();
    $self->Bootstrap($bs);
}

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $mle) = $xpc->findnodes('x:MLE', $node);
    $self->MLE->parse($mle) if (defined $mle);

    (my $bootstrap) = $xpc->findnodes('x:OtherMethod', $node);
    $self->Bootstrap->parse($bootstrap) if (defined $bootstrap);
}

sub xml
{
    my $self = shift;

    my $mle;
    if (defined $self->MLE) {
        $mle = $self->MLE->xml();
    }

    my $bootstrap;
    if (defined $self->Bootstrap) {
        $bootstrap = $self->Bootstrap->xml();
    }

    my $est;
    if (defined $mle or defined $bootstrap) {
        $est = XML::LibXML::Element->new("PrecisionPopulationEstimates");
        if (defined $mle) {
            $est->appendChild($mle);
        }
        if (defined $bootstrap) {
            $est->appendChild($bootstrap);
        }
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
