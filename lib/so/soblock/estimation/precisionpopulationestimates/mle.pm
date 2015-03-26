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

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("MLE");
    
    if (defined $self->CovarianceMatrix) {
        my $xml = $self->CovarianceMatrix->xml();
        $est->appendChild($xml);
    }

    if (defined $self->CorrelationMatrix) {
        my $xml = $self->CorrelationMatrix->xml();
        $est->appendChild($xml);
    }

    if (defined $self->StandardError) {
        my $xml = $self->StandardError->xml();
        $est->appendChild($xml);
    }

    if (defined $self->RelativeStandardError) {
        my $xml = $self->RelativeStandardError->xml();
        $est->appendChild($xml);
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
