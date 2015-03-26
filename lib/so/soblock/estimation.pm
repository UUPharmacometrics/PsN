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


has 'PopulationEstimates' => ( is => 'rw', isa => 'so::soblock::estimation::populationestimates' );
has 'PrecisionPopulationEstimates' => ( is => 'rw', isa => 'so::soblock::estimation::precisionpopulationestimates' );
has 'IndividualEstimates' => ( is => 'rw', isa => 'so::soblock::estimation::individualestimates' );
has 'Residual' => ( is => 'rw', isa => 'so::soblock::estimation::residual' );
has 'Predictions' => ( is => 'rw', isa => 'so::table' );
has 'Likelihood' => ( is => 'rw', isa => 'so::soblock::estimation::likelihood' );


sub BUILD
{
    my $self = shift;

    my $pe = so::soblock::estimation::populationestimates->new();
    $self->PopulationEstimates($pe);
    my $ppe = so::soblock::estimation::precisionpopulationestimates->new();
    $self->PrecisionPopulationEstimates($ppe);
    my $ie = so::soblock::estimation::individualestimates->new();
    $self->IndividualEstimates($ie);
    my $res = so::soblock::estimation::residual->new();
    $self->Residual($res);
    my $l = so::soblock::estimation::likelihood->new();
    $self->Likelihood($l);
}

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("Estimation");
    
    if (defined $self->PopulationEstimates) {
        my $xml = $self->PopulationEstimates->xml();
        $est->appendChild($xml);
    }

    if (defined $self->PrecisionPopulationEstimates) {
        my $xml = $self->PrecisionPopulationEstimates->xml();
        $est->appendChild($xml);
    }

    if (defined $self->IndividualEstimates) {
        my $xml = $self->IndividualEstimates->xml();
        $est->appendChild($xml);
    }

    if (defined $self->Residual) {
        my $xml = $self->Residual->xml();
        $est->appendChild($xml);
    }

    if (defined $self->Predictions) {
        my $xml = $self->Predictions->xml();
        $est->appendChild($xml);
    }

    if (defined $self->Likelihood) {
        my $xml = $self->Likelihood->xml();
        $est->appendChild($xml);
    }

    return $est;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
