package so;

# Class representing an so file

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::soblock;

has 'filename' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'pretty' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'exclude_elements' => ( is => 'rw', isa => 'Maybe[ArrayRef[Str]]' );
has 'only_include_elements' => ( is => 'rw', isa => 'Maybe[ArrayRef[Str]]' ); 
has 'version' => ( is => 'rw', isa => 'Num', default => 0.2 );
has 'message' => ( is => 'rw', isa => 'Maybe[Str]' );

has 'PharmMLRef' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'SOBlock' => ( is => 'rw', isa => 'ArrayRef[so::soblock]', default => sub { [] } );

has '_duplicate_blocknames' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );


sub parse
{
    my $self = shift;

    my $doc = XML::LibXML->load_xml(location => $self->filename);
    my $xpc = so::xml::get_xpc();

    (my $PharmMLRef) = $xpc->findnodes('/x:SO/x:PharmMLRef');
    $self->PharmMLRef($PharmMLRef->textContent) if (defined $PharmMLRef);

    my @SOBlocks = $xpc->findnodes('/x:SO/x:SOBlock', $doc);

    foreach my $node (@SOBlocks) {
        my $so_block = so::soblock->new();
        $so_block->parse($node);
        push @{$self->SOBlock}, $so_block;
    }
}

sub create_block
{
    # Create a new SOBlock, set the id and add it to this SO 
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
    );
    my $name = $parm{'name'};
    
    my $block = so::soblock->new(version => $self->version);

    if (not exists $self->_duplicate_blocknames->{$name}) {
        $block->blkId($name);
    } else {
        $block->blkId($name . '_' . ($self->_duplicate_blocknames->{$name} + 1));
    }
    $self->_duplicate_blocknames->{$name}++;

    push @{$self->SOBlock}, $block;

    return $block;
}

sub _create_filename
{
    # Check if filename was set else try to infer it from SOBlock names
    my $self = shift;

    my $so_filename;
    if (defined $self->SOBlock->[0] and defined $self->SOBlock->[0]->blkId) {
        $self->filename($self->SOBlock->[0]->blkId . '.SO.xml');
    } else {
        $self->filename("unnamed.SO.xml");
    }
}

sub write
{
    my $self = shift;

    my $doc = XML::LibXML::Document->new('1.0', 'utf-8');

    my $SO = $doc->createElement("SO");
    $SO->setAttribute('xmlns' => "http://www.pharmml.org/so/0.1/StandardisedOutput");
    $SO->setAttribute('xmlns:xsi' => "http://www.w3.org/2001/XMLSchema-instance");
    $SO->setAttribute('xmlns:ds' => "http://www.pharmml.org/pharmml/0.6/Dataset");
    $SO->setAttribute('xmlns:ct' => "http://www.pharmml.org/pharmml/0.6/CommonTypes");
    $SO->setAttribute('xsi:schemaLocation' => "http://www.pharmml.org/so/0.1/StandardisedOutput");
    $SO->setAttribute('implementedBy' => "MJS");
    $SO->setAttribute('writtenVersion' => "0.1");
    $SO->setAttribute('id' => "i1");

    if (defined $self->PharmMLRef) {
        my $ref = XML::LibXML::Element->new("PharmMLRef");
        $ref->setAttribute("name", $self->PharmMLRef);
        $SO->appendChild($ref);
    }

    # Add the user defined message to the first block
    if (defined $self->message and defined $self->SOBlock->[0]) {
        $self->SOBlock->[0]->TaskInformation->add_message(
            type => "INFORMATION",
            toolname => "nmoutput2so",
            name => "User specified message",
            content => $self->message,
            severity => 0,
        );
    }

    foreach my $block (@{$self->SOBlock}) {
        my $xml = $block->xml();
        $self->_exclude_elements($xml);
        $SO->appendChild($xml);
    }

    $doc->setDocumentElement($SO);
    if (not defined $self->filename) {
        $self->_create_filename();
    }

    $doc->toFile($self->filename, $self->pretty);
}

sub _exclude_elements
{
    # Exclude elements in exclude_elements or not in only_include_elements

    my $self = shift;
    my $xml = shift;

    if (defined $self->exclude_elements) {
        foreach my $xpath (@{$self->exclude_elements}) {
            my @nodes = $xml->findnodes($xpath);
            foreach my $node (@nodes) {
                $node->unbindNode();
            }
        }
    }

    if (defined $self->only_include_elements) {
        my @include_nodes;
        foreach my $xpath (@{$self->only_include_elements}) {
            my @nodes = $xml->findnodes($xpath);
            push @include_nodes, @nodes;
        }

        my @keep;
        # Find all ancestors and descendants of the only_include nodes
        foreach my $node (@include_nodes) {
            push @keep, $node;
            my $ancestor = $node;
            while ($ancestor = $ancestor->parentNode) {
                push @keep, $ancestor;
            }
            my @descendants = $node->findnodes("descendant::node()");
            push @keep, @descendants;
        }

        my @all_nodes = $xml->findnodes("//*");

        # Remove nodes not in the @keep
        foreach my $node (@all_nodes) {
            if (not grep { $_->isSameNode($node) } @keep) {
                $node->unbindNode();
            }
        }

    }

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
