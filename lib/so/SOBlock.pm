package so::SOBlock;

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
has 'RawResults' => ( is => 'rw', isa => 'so::SOBlock::RawResults' );
has 'TaskInformation' => ( is => 'rw', isa => 'so::SOBlock::TaskInformation' );
has 'Estimation' => ( is => 'rw', isa => 'so::SOBlock::Estimation' );
has 'Simulation' => ( is => 'rw', isa => 'so::SOBlock::Simulation' );

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
    if (defined $mle) {
        my $popest = standardised_output::table->new();
        $popest->parse($mle);
        $self->PopulationEstimates($popest);
    }

    (my $se_node) = $xpc->findnodes('x:Estimation/x:PrecisionPopulationEstimates/x:MLE/x:StandardError', $node);

    if (defined $se_node) {
        my $ses = standardised_output::table->new();
        $ses->parse($se_node);
        $self->StandardError($ses);
    }

    (my $rse_node) = $xpc->findnodes('x:Estimation/x:PrecisionPopulationEstimates/x:MLE/x:RelativeStandardError', $node);
    if (defined $rse_node) {
        my $rses = standardised_output::table->new();
        $rses->parse($rse_node);
        $self->RelativeStandardError($rses);
    }

    (my $cov_node) = $xpc->findnodes('x:Estimation/x:PrecisionPopulationEstimates/x:MLE/x:CovarianceMatrix', $node);
    if (defined $cov_node) {
        my $cov = standardised_output::matrix->new();
        $cov->parse($cov_node);
        $self->CovarianceMatrix($cov);
    }

    (my $cor_node) = $xpc->findnodes('x:Estimation/x:PrecisionPopulationEstimates/x:MLE/x:CorrelationMatrix', $node);
    if (defined $cor_node) {
        my $cor = standardised_output::matrix->new();
        $cor->parse($cor_node);
        $self->CorrelationMatrix($cor);
    }

    (my $dev) = $xpc->findnodes('x:Estimation/x:Likelihood/x:Deviance', $node);
    if (defined $dev) {
        $self->Deviance($dev->textContent());
    }

    (my $res_node) = $xpc->findnodes('x:Estimation/x:Residuals/x:ResidualTable', $node);
    if (defined $res_node) {
        my $res = standardised_output::table->new();
        $res->parse($res_node);
        $self->Residuals($res);
    }

    (my $pred_node) = $xpc->findnodes('x:Estimation/x:Predictions', $node);
    if (defined $pred_node) {
        my $pred = standardised_output::table->new();
        $pred->parse($pred_node);
        $self->Predictions($pred);
    }
}

sub create_sdtab
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        filename => { isa => 'Str' },
    );
    my $filename = $parm{'filename'};

    my @colnames = ( 'ID', 'TIME' );
    my @table = ( $self->Predictions->columns->[0], $self->Predictions->columns->[1]);  # FIXME: Assumes too much!

    if (defined $self->Predictions) {
        for (my $i = 0; $i < scalar(@{$self->Predictions->columnId}); $i++) {
            my $column_id = $self->Predictions->columnId->[$i];
            if ($column_id ne 'ID' and $column_id ne 'TIME') {
                push @colnames, $column_id;
                push @table, $self->Predictions->columns->[$i];
            }
        }
    }
    if (defined $self->Residuals) {
        for (my $i = 0; $i < scalar(@{$self->Residuals->columnId}); $i++) {
            my $column_id = $self->Residuals->columnId->[$i];
            if ($column_id ne 'ID' and $column_id ne 'TIME') {
                push @colnames, $column_id;
                push @table, $self->Residuals->columns->[$i];
            }
        }
    }

    open my $fh, '>', $filename;
    print $fh "TABLE NO.  1\n ";

    for (my $i = 0; $i < scalar(@colnames) - 1; $i++) {
        print $fh $colnames[$i] . ' ' x (12 - length($colnames[$i]));
    }
    print $fh $colnames[-1] . "\n";
    for (my $i = 0; $i < scalar(@{$table[0]}); $i++) {
        foreach my $col (@table) {
            printf $fh ' % .4E', $col->[$i]
        }
        print $fh "\n";
    }

    close $fh;
}

sub xml
{
    my $self = shift;

    my $block = XML::LibXML::Element->new("SOBlock");
    $block->setAttribute("blkId", $self->blkId);

    my @attributes = ( "RawResults", "TaskInformation", "Estimation", "Simulation" );
    foreach my $attr (@attributes) {
        if (defined $self->$attr) {
            my $xml = $self->$attr->xml();
            $block->appendChild($xml);
        }
    }

    return $block;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
