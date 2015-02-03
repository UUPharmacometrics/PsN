package standardised_output::table;

# Class representing a generic SO table

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use standardised_output::xml;

has 'columnId' => ( is => 'rw', isa => 'ArrayRef' );
has 'columnType' => ( is => 'rw', isa => 'ArrayRef' );
has 'valueType' => ( is => 'rw', isa => 'ArrayRef' ); 
has 'columns' => ( is => 'rw', isa => 'ArrayRef' );


sub BUILD
{
    my $self = shift;

}

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = standardised_output::xml::get_xpc();

    my @columns = $xpc->findnodes('ds:Definition/ds:Column', $node);

    $self->columnId([]);
    $self->columnType([]);
    $self->valueType([]);

    foreach my $column (@columns) {
        push @{$self->columnId},  $column->getAttribute('columnId');
        push @{$self->columnType},  $column->getAttribute('columnType');
        push @{$self->valueType},  $column->getAttribute('valueType');
    }

    my @rows = $xpc->findnodes('ds:Table/ds:Row', $node);

    $self->columns([ ([]) x scalar(@columns) ]);
    foreach my $row (@rows) {
        my @values = $row->getChildrenByTagName('*');
        for (my $i = 0; $i < scalar(@values); $i++) {
            push @{$self->columns->[$i]}, $values[$i]->textContent;
        }
    }
    use Data::Dumper;
    print Dumper($self);
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
