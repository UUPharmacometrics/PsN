package standardised_output::so::SOBlock;

# Class containing an SOBlock

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use standardised_output::xml;
use standardised_output::table;
use standardised_output::matrix;

has 'blkId' => ( is => 'rw', isa => 'Str' );
has 'DataFile' => ( is => 'rw', isa => 'ArrayRef[HashRef]' );     # Array of Hashes with 'Description' and 'path'
has 'PopulationEstimates' => ( is => 'rw', isa => 'standardised_output::table' );
has 'StandardError' => ( is => 'rw', isa => 'standardised_output::table' );
has 'RelativeStandardError' => ( is => 'rw', isa => 'standardised_output::table' );
has 'CovarianceMatrix' => ( is => 'rw', isa => 'standardised_output::matrix' );
has 'CorrelationMatrix' => ( is => 'rw', isa => 'standardised_output::matrix' );
has 'Deviance' => ( is => 'rw', isa => 'Num' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $blk_id = $node->getAttribute('blkId');
    $self->blkId($blk_id);

    my $xpc = standardised_output::xml::get_xpc();

    my @datafiles = $xpc->findnodes('x:RawResults/x:DataFile', $node);
    foreach my $datafile (@datafiles) {
        (my $desc) = $xpc->findnodes('ct:Description', $datafile);
        (my $path) = $xpc->findnodes('ds:path', $datafile);
        $self->DataFile([]) unless defined $self->DataFile;
        push @{$self->DataFile}, { Description => $desc->textContent(), path => $path->textContent() }; 
    }

    (my $mle) = $xpc->findnodes('x:Estimation/x:PopulationEstimates/x:MLE', $node);
    my $popest = standardised_output::table->new();
    $popest->parse($mle);
    $self->PopulationEstimates($popest);

    (my $se_node) = $xpc->findnodes('x:Estimation/x:PrecisionPopulationEstimates/x:MLE/x:StandardError', $node);
    my $ses = standardised_output::table->new();
    $ses->parse($se_node);
    $self->StandardError($ses);

    (my $rse_node) = $xpc->findnodes('x:Estimation/x:PrecisionPopulationEstimates/x:MLE/x:RelativeStandardError', $node);
    my $rses = standardised_output::table->new();
    $rses->parse($rse_node);
    $self->RelativeStandardError($rses);

    (my $cov_node) = $xpc->findnodes('x:Estimation/x:PrecisionPopulationEstimates/x:MLE/x:CovarianceMatrix', $node);
    my $cov = standardised_output::matrix->new();
    $cov->parse($cov_node);
    $self->CovarianceMatrix($cov);

    (my $cor_node) = $xpc->findnodes('x:Estimation/x:PrecisionPopulationEstimates/x:MLE/x:CorrelationMatrix', $node);
    my $cor = standardised_output::matrix->new();
    $cor->parse($cor_node);
    $self->CorrelationMatrix($cor);

    (my $dev) = $xpc->findnodes('x:Estimation/x:Likelihood/x:Deviance', $node);
    $self->Deviance($dev->textContent());
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
