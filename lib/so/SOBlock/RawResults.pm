package so::SOBlock::RawResults;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use standardised_output::xml;
use standardised_output::table;
use standardised_output::matrix;

has 'DataFile' => ( is => 'rw', isa => 'ArrayRef[so::SOBlock::RawResults::DataFile]' );

sub xml
{
    my $self = shift;

    my $rr = XML::LibXML::Element->new("RawResults");

    if (defined $self->DataFile) {
        foreach my $file (@{$self->DataFile}) {
            my $file_xml = $file->xml();
            $rr->appendChild($file_xml);
        }
    }

    return $rr;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
