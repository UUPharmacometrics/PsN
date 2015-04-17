package so::soblock::estimation::precisionpopulationestimates::mle;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::matrix;
use so::table;

has 'CovarianceMatrix' => ( is => 'rw', isa => 'so::matrix' );
has 'CorrelationMatrix' => ( is => 'rw', isa => 'so::matrix' );
has 'StandardError' => ( is => 'rw', isa => 'so::table' );
has 'RelativeStandardError' => ( is => 'rw', isa => 'so::table' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $cov) = $xpc->findnodes('x:CovarianceMatrix', $node);
    if (defined $cov) {
        my $matrix = so::matrix->new();
        $matrix->parse($cov);
        $self->CovarianceMatrix($matrix);
    }

    (my $cor) = $xpc->findnodes('x:CorrelationMatrix', $node);
    if (defined $cor) {
        my $matrix = so::matrix->new();
        $matrix->parse($cor);
        $self->CorrelationMatrix($matrix);
    }

    (my $se) = $xpc->findnodes('x:StandardError', $node);
    if (defined $se) {
        my $table = so::table->new();
        $table->parse($se);
        $self->StandardError($table);
    }

    (my $rse) = $xpc->findnodes('x:RelativeStandardError', $node);
    if (defined $rse) {
        my $table = so::table->new();
        $table->parse($rse);
        $self->RelativeStandardError($table);
    }
}

sub xml
{
    my $self = shift;

    my $cov;
    if (defined $self->CovarianceMatrix) {
        $cov = $self->CovarianceMatrix->xml();
    }

    my $cor;
    if (defined $self->CorrelationMatrix) {
        $cor = $self->CorrelationMatrix->xml();
    }

    my $se;
    if (defined $self->StandardError) {
        $se = $self->StandardError->xml();
    }

    my $rse;
    if (defined $self->RelativeStandardError) {
        $rse = $self->RelativeStandardError->xml();
    }

    my $est;
    if (defined $cov or defined $cor or defined $se or defined $rse) {
        $est = XML::LibXML::Element->new("MLE");

        if (defined $cov) {
            $est->appendChild($cov);
        }

        if (defined $cor) {
            $est->appendChild($cor);
        }

        if (defined $se) {
            $est->appendChild($se);
        }

        if (defined $rse) {
            $est->appendChild($rse);
        }
    }

    return $est;
}

sub create
{
    # Helper method to create the MLE

    my $self = shift;
    my %parm = validated_hash(\@_,
        labels => { isa => 'ArrayRef', optional => 1 },
        standard_errors => { isa => 'ArrayRef', optional => 1 },
        relative_standard_errors => { isa => 'ArrayRef', optional => 1 },
        correlation_matrix => { isa => 'ArrayRef[ArrayRef]', optional => 1 },
        covariance_matrix => { isa => 'ArrayRef[ArrayRef]', optional => 1 },
    );
    my @labels = defined $parm{'labels'} ? @{$parm{'labels'}} : ();
    my @standard_errors = defined $parm{'standard_errors'} ? @{$parm{'standard_errors'}}: ();
    my @relative_standard_errors = defined $parm{'relative_standard_errors'} ? @{$parm{'relative_standard_errors'}} : ();
    my $correlation_matrix = $parm{'correlation_matrix'};
    my $covariance_matrix = $parm{'covariance_matrix'};

    if (scalar(@labels) > 0) {
        my $se_table = so::table->new(name => 'StandardError');
        $se_table->parameter_table(name => 'SE', labels => \@labels, values => \@standard_errors);
        $self->StandardError($se_table);

        my $rse_table = so::table->new(name => 'RelativeStandardError');
        $rse_table->parameter_table(name => 'RSE', labels => \@labels, values => \@relative_standard_errors);
        $self->RelativeStandardError($rse_table);        

        my $cor = so::matrix->new(name => 'CorrelationMatrix', RowNames => \@labels, ColumnNames => \@labels, MatrixRow => $correlation_matrix);
        $self->CorrelationMatrix($cor);

        my $cov = so::matrix->new(name => 'CovarianceMatrix', RowNames => \@labels, ColumnNames => \@labels, MatrixRow => $covariance_matrix);
        $self->CovarianceMatrix($cov);
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
