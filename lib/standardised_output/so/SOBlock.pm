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

has 'blkId' => ( is => 'rw', isa => 'Str' );
has 'PopulationEstimates' => (is => 'rw', isa => 'Ref' );

sub BUILD
{
    my $self = shift;

}

sub parse
{
    my $self = shift;
    my $node = shift;

    my $blk_id = $node->getAttribute('blkId');
    $self->blkId($blk_id);

    my $xpc = standardised_output::xml::get_xpc();
    (my $mle) = $xpc->findnodes('x:Estimation/x:PopulationEstimates/x:MLE', $node);

    my $table = standardised_output::table->new();
    $table->parse($mle);
    $self->PopulationEstimates($table);
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
