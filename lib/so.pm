package standardised_output::so;

# Class representing an so file

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::SOBlock;

has 'filename' => ( is => 'rw', isa => 'Str' );
has 'pretty' => ( is => 'rw', isa => 'Bool', default => 0 );

has 'PharmMLRef' => ( is => 'rw', isa => 'Str' );
has 'SOBlock' => ( is => 'rw', isa => 'ArrayRef[so::SOBlock]', default => sub { [] } );

has '_document' => ( is => 'rw', isa => 'Ref' );
has '_xpc' => ( is => 'rw', isa => 'Ref' );

sub BUILD
{
    my $self = shift;

    my $so_xml = standardised_output::xml->new(precision => $self->precision, verbose => $self->verbose);
    $self->_xml($so_xml);
    $self->_document($self->_xml->_document);
}

sub parse
{
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

#FIXME: Rewrite to handle duplicate names only
sub create_block
{
    # Create a new SOBlock and set the id 
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
    );
    my $name = $parm{'name'};
    
    my $doc = $self->_document;
    my %duplicates;
    if (defined $self->_duplicate_blocknames) {
        %duplicates = %{$self->_duplicate_blocknames};
    }

    my $block = $doc->createElement("SOBlock");
    if (not exists $duplicates{$name}) {
        $block->setAttribute(blkId => $name);
    } else {
        print "$duplicates{$name}";
        $block->setAttribute(blkId => $name . $duplicates{$name});
        $self->_duplicate_blocknames->{$name}++;
    }

    return $block;
}

sub _create_filename
{
    # Check if filename was set else try to infer it from SOBlock names
    my $self = shift;

    my $so_filename;
    if (defined $self->SOBlock->[0]->blkId) {
        $self->filename($self->SOBlock->[0]->blkId . 'SO.xml';
    } else {
        foreach my $block (@{$self->SOBlock}) {
            if (defined $block->Estimation and defined $block->Estimation->PrecisionPopulationEstimates
                    and defined $block->Estimation->PrecisionPopulationEstimates->Bootstrap) {
                $self->filename("bootstrap.SO.xml");
            }
        }
    }
    if (not defined $self->filename) {
        $self->filename("unnamed.SO.xml");
    }
}

sub write
{
    my $self = shift;

    my $doc = $self->_document;

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
        $ref->addAttribute("name", $self->PharmMLRef);
        $SO->appendChild($ref);
    }

    if (defined $self->SOBlock) {
        foreach my $block (@{$self->SOBlock}) {
            my $xml = $block->xml();
            $SO->appendChild($xml);
        }
    }

    $doc->setDocumentElement($SO);
    if (not defined $self->filename) {
        $self->_create_filename();
    }
    $doc->toFile($self->filename, $self->pretty);
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
