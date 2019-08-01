package so::soblock::estimation::populationestimates;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

use so::table;
use so::soblock::estimation::populationestimates::bootstrap;

has 'version' => ( is => 'rw', isa => 'Num', required => 1 );

has 'MLE' => ( is => 'rw', isa => 'so::table' );
has 'Bootstrap' => ( is => 'rw', isa => 'so::soblock::estimation::populationestimates::bootstrap' );    # Added in SO 0.2


sub BUILD
{
    my $self = shift;

    my $bootstrap = so::soblock::estimation::populationestimates::bootstrap->new(version => $self->version);
    $self->Bootstrap($bootstrap);
}

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    (my $mle) = $xpc->findnodes('x:MLE', $node);
    if (defined $mle) {
        my $table = so::table->new();
        $table->parse($mle);
        $self->MLE($table);
    }

    (my $bootstrap) = $xpc->findnodes('x:OtherMethod', $node);
    $self->Bootstrap->parse($bootstrap) if (defined $bootstrap);
}

sub xml
{
    my $self = shift;

    my $est;

    my $mle;
    if (defined $self->MLE) {
        $mle = $self->MLE->xml();
    }
    my $bootstrap = $self->Bootstrap->xml();

    if (defined $mle or defined $bootstrap) {
        $est = XML::LibXML::Element->new("PopulationEstimates");
        if (defined $mle) {
            $est->appendChild($mle);
        }
        if (defined $bootstrap) {
            $est->appendChild($bootstrap);
        }
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
        types => { isa => 'ArrayRef' },
    );
    my @labels = @{$parm{'labels'}};
    my @values = @{$parm{'values'}};
    my @types = @{$parm{'types'}};

    my $table = so::table->new(name => "MLE");
    $table->columnId(\@labels);
    $table->single_row(values => \@values, types => \@types);
    $self->MLE($table);
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
