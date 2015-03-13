package so::SOBlock::TaskInformation;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'Message' => ( is => 'rw', isa => 'ArrayRef[so::SOBlock::TaskInformation::Message]' );
has 'RunTime' => ( is => 'rw', isa => 'so::SOBlock::TaskInformation::RunTime' );

sub xml
{
    my $self = shift;

    my $ti = XML::LibXML::Element->new("TaskInformation");

    if (defined $self->Message) {
        foreach my $msg (@{$self->Message}) {
            my $msg_xml = $msg->xml();
            $ti->appendChild($msg_xml);
        }
    }

    if (defined $self->RunTime) {
        my $rt = $self->RunTime->xml();
        $ti->appendChild($rt);
    }

    return $ti;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
