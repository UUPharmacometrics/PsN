package standardised_output::so;

# Class for reading an so file

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use standardised_output::xml;
use standardised_output::so::SOBlock;

has 'filename' => ( is => 'rw', isa => 'Str' );
has 'SOBlock' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has '_document' => ( is => 'rw', isa => 'Ref' );
has '_xpc' => ( is => 'rw', isa => 'Ref' );

sub BUILD
{
    my $self = shift;

    my $doc = XML::LibXML->load_xml(location => $self->filename);
    my $xpc = standardised_output::xml::get_xpc();

    my @SOBlocks = $xpc->findnodes('/x:SO/x:SOBlock', $doc);

    foreach my $node (@SOBlocks) {
        my $so_block = standardised_output::so::SOBlock->new();
        $so_block->parse($node);
        push @{$self->SOBlock}, $so_block;
    }

    $self->_document($doc);
    $self->_xpc($xpc);
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
