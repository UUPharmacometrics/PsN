package so::soblock::taskinformation;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::soblock::taskinformation::message;
use so::soblock::taskinformation::runtime;

has 'Message' => ( is => 'rw', isa => 'ArrayRef[so::soblock::taskinformation::message]' );
has 'RunTime' => ( is => 'rw', isa => 'so::soblock::taskinformation::runtime' );

sub BUILD
{
    my $self = shift;

    my $rt = so::soblock::taskinformation::runtime->new();
    $self->RunTime($rt);
}

sub xml
{
    my $self = shift;

    my $ti;
    my $rt = $self->RunTime->xml();

    if (defined $rt or defined $self->Message) {
        $ti = XML::LibXML::Element->new("TaskInformation");

        if (defined $self->Message) {
            foreach my $msg (@{$self->Message}) {
                my $msg_xml = $msg->xml();
                $ti->appendChild($msg_xml);
            }
        }

        if (defined $rt) {
            $ti->appendChild($rt);
        }
    }

    return $ti;
}

sub add_message
{
    # Helper method to add a message
    my $self = shift;
    my %parm = validated_hash(\@_,
        type => { isa => 'Str' },
        toolname => { isa => 'Str' },
        name => { isa => 'Str' },
        content => { isa => 'Str' },
        severity => { isa => 'Int' },
    );
    my $type = $parm{'type'};
    my $toolname = $parm{'toolname'};
    my $name = $parm{'name'};
    my $content = $parm{'content'};
    my $severity = $parm{'severity'};

    $self->Message([]) if not defined $self->Message;

    my $msg = so::soblock::taskinformation::message->new(
        type => $type,
        Toolname => $toolname,
        Name => $name,
        Content => $content,
        Severity => $severity,    
    );

    push @{$self->Message}, $msg;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
