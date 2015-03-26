package so::soblock::estimation::populationestimates;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

use so::table;

has 'MLE' => ( is => 'rw', isa => 'so::table' );

sub xml
{
    my $self = shift;

    my $est = XML::LibXML::Element->new("PopulationEstimates");
    
    if (defined $self->MLE) {
        my $xml = $self->MLE->xml();
        $est->appendChild($xml);
    }

    return $est;
}

sub create_MLE
{
    # Helper method to create the MLE table
    my $self = shift;
    my %parm = validated_hash(\@_,
        labels => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my @labels = @{$parm{'labels'}};
    my @values = @{$parm{'values'}};

    my $table = so::table->new(name => "MLE");
    $table->columnId(\@labels);
    $table->single_row(values => \@values);
    $self->MLE($table); 
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
