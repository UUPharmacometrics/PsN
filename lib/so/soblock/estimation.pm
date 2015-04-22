package so::soblock::estimation;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

use so::table;
use so::soblock::estimation::populationestimates;
use so::soblock::estimation::precisionpopulationestimates;
use so::soblock::estimation::individualestimates;
use so::soblock::estimation::residual;
use so::soblock::estimation::likelihood;

has 'version' => ( is => 'rw', isa => 'Num', required => 1 );

has 'PopulationEstimates' => ( is => 'rw', isa => 'so::soblock::estimation::populationestimates' );
has 'PrecisionPopulationEstimates' => ( is => 'rw', isa => 'so::soblock::estimation::precisionpopulationestimates' );
has 'IndividualEstimates' => ( is => 'rw', isa => 'so::soblock::estimation::individualestimates' );
has 'Residual' => ( is => 'rw', isa => 'so::soblock::estimation::residual' );
has 'Predictions' => ( is => 'rw', isa => 'so::table' );
has 'Likelihood' => ( is => 'rw', isa => 'so::soblock::estimation::likelihood' );

sub BUILD
{
    my $self = shift;

    my $pe = so::soblock::estimation::populationestimates->new(version => $self->version);
    $self->PopulationEstimates($pe);
    my $ppe = so::soblock::estimation::precisionpopulationestimates->new(version => $self->version);
    $self->PrecisionPopulationEstimates($ppe);
    my $ie = so::soblock::estimation::individualestimates->new();
    $self->IndividualEstimates($ie);
    my $res = so::soblock::estimation::residual->new();
    $self->Residual($res);
    my $l = so::soblock::estimation::likelihood->new();
    $self->Likelihood($l);
}

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $pe) = $xpc->findnodes('x:PopulationEstimates', $node);
    $self->PopulationEstimates->parse($pe) if (defined $pe);

    (my $ppe) = $xpc->findnodes('x:PrecisionPopulationEstimates', $node);
    $self->PrecisionPopulationEstimates->parse($ppe) if (defined $ppe);

    (my $ie) = $xpc->findnodes('x:IndividualEstimates', $node);
    $self->IndividualEstimates->parse($ie) if (defined $ie);

    (my $res) = $xpc->findnodes('x:Residual', $node);
    $self->Residual->parse($res) if (defined $res);

    (my $pred) = $xpc->findnodes('x:Predictions', $node);
    if (defined $pred) {
        my $table = so::table->new();
        $table->parse($pred);
        $self->Predictions($table);
    }

    (my $ll) = $xpc->findnodes('x:Likelihood', $node);
    $self->Likelihodd->parse($ll) if (defined $ll);
}

sub xml
{
    my $self = shift;

    my $est;

    my $pe = $self->PopulationEstimates->xml();
    my $ppe = $self->PrecisionPopulationEstimates->xml();
    my $ie = $self->IndividualEstimates->xml();
    my $res = $self->Residual->xml();
    my $pred;
    if (defined $self->Predictions) {
        $pred = $self->Predictions->xml();
    }
    my $l = $self->Likelihood->xml();

    if (defined $pe or defined $ppe or defined $ie or defined $res or defined $pred or defined $l) {
        $est = XML::LibXML::Element->new("Estimation");

        if (defined $pe) {
            $est->appendChild($pe);
        }

        if (defined $ppe) {
            $est->appendChild($ppe);
        }

        if (defined $ie) {
            $est->appendChild($ie);
        }

        if (defined $res) {
            $est->appendChild($res);
        }

        if (defined $pred) {
            $est->appendChild($pred);
        }

        if (defined $l) {
            $est->appendChild($l);
        }
    }

    return $est;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
