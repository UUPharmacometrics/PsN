package so::soblock::taskinformation;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use so::soblock::taskinformation::message;

has 'Message' => ( is => 'rw', isa => 'ArrayRef[so::soblock::taskinformation::message]' );
has 'RunTime' => ( is => 'rw', isa => 'Maybe[Num]' );

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    my @messages = $xpc->findnodes('x:Message', $node);
    foreach my $msg (@messages) {
        $self->Message([]) if not defined $self->Message;
        my $message = so::soblock::taskinformation::message->new();
        $message->parse($msg);
        push @{$self->Message}, $message;
    }

    (my $runtime) = $xpc->findnodes('x:RunTime', $node);
    $self->RunTime($runtime);
}

sub xml
{
    my $self = shift;

    my $ti;

    if (defined $self->RunTime or defined $self->Message) {
        $ti = XML::LibXML::Element->new("TaskInformation");

        if (defined $self->Message) {
            foreach my $msg (@{$self->Message}) {
                my $msg_xml = $msg->xml();
                $ti->appendChild($msg_xml);
            }
        }

        if (defined $self->RunTime) {
            my $rt = XML::LibXML::Element->new("RunTime");
            $rt->appendTextNode($self->RunTime);
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
