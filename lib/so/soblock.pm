package so::soblock;

# Class containing an SOBlock

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::xml;
use so::table;
use so::matrix;
use so::soblock::rawresults;
use so::soblock::taskinformation;
use so::soblock::estimation;
use so::soblock::simulation;

has 'version' => ( is => 'rw', isa => 'Num', required => 1 );

has 'blkId' => ( is => 'rw', isa => 'Str' );
has 'RawResults' => ( is => 'rw', isa => 'so::soblock::rawresults' );
has 'TaskInformation' => ( is => 'rw', isa => 'so::soblock::taskinformation' );
has 'Estimation' => ( is => 'rw', isa => 'so::soblock::estimation' );
has 'Simulation' => ( is => 'rw', isa => 'so::soblock::simulation' );

sub BUILD
{
    my $self = shift;

    my $rr = so::soblock::rawresults->new();
    $self->RawResults($rr);

    my $ti = so::soblock::taskinformation->new();
    $self->TaskInformation($ti);

    my $est = so::soblock::estimation->new(version => $self->version);
    $self->Estimation($est);

    my $sim = so::soblock::simulation->new();
    $self->Simulation($sim);
}

sub parse
{
    my $self = shift;
    my $node = shift;

    my $blk_id = $node->getAttribute('blkId');
    $self->blkId($blk_id);

    my $xpc = so::xml::get_xpc();

    (my $ti) = $xpc->findnodes('x:TaskInformation', $node);
    $self->TaskInformation->parse($ti) if (defined $ti);

    (my $rr) = $xpc->findnodes('x:RawResults', $node);
    $self->RawResults->parse($rr) if (defined $rr);

    (my $est) = $xpc->findnodes('x:Estimation', $node);
    $self->Estimation->parse($est) if (defined $est);

    (my $sim) = $xpc->findnodes('x:Simulation', $node);
    $self->Simulation->parse($sim) if (defined $sim);
}

sub create_sdtab
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        filename => { isa => 'Str' },
    );
    my $filename = $parm{'filename'};

    my @colnames = ( 'ID', 'TIME' );
    my $pred = $self->Estimation->Predictions;

    my @table = ( $pred->columns->[0], $pred->columns->[1] );  # FIXME: Assumes too much!

    if (defined $pred) {
        for (my $i = 0; $i < scalar(@{$pred->columnId}); $i++) {
            my $column_id = $pred->columnId->[$i];
            if ($column_id ne 'ID' and $column_id ne 'TIME') {
                push @colnames, $column_id;
                push @table, $pred->columns->[$i];
            }
        }
    }
    my $res = $self->Estimation->Residual->ResidualTable;
    if (defined $res) {
        for (my $i = 0; $i < scalar(@{$res->columnId}); $i++) {
            my $column_id = $res->columnId->[$i];
            if ($column_id ne 'ID' and $column_id ne 'TIME') {
                push @colnames, $column_id;
                push @table, $res->columns->[$i];
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

    my @attributes = ( "RawResults", "TaskInformation", "Estimation", "Simulation" );
    my @elements;
    foreach my $attr (@attributes) {
        if (defined $self->$attr) {
            my $xml = $self->$attr->xml();
            if (defined $xml) {
                push @elements, $xml;
            }
        }
    }

    my $block;
    if (scalar(@elements) > 0) {
        $block = XML::LibXML::Element->new("SOBlock");
        $block->setAttribute("blkId", $self->blkId);
        foreach my $e (@elements) {
            $block->appendChild($e);
        }
    }

    return $block;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
