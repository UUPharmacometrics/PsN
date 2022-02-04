package so::soblock::rawresults;

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use include_modules;
use XML::LibXML;
use File::Spec;
use utils::file;
use so::soblock::rawresults::datafile;

has 'DataFile' => ( is => 'rw', isa => 'ArrayRef[so::soblock::rawresults::datafile]' );
has '_next_oid' => ( is => 'rw', isa => 'Int', default => 1 );  # The next description oid to use by add_datafile

sub parse
{
    my $self = shift;
    my $node = shift;

    my $xpc = so::xml::get_xpc();

    my @datafiles = $xpc->findnodes('x:DataFile', $node);
    foreach my $datafile (@datafiles) {
        $self->DataFile([]) unless defined $self->DataFile;
        my $file = so::soblock::rawresults::datafile->new();
        $file->parse($datafile);
        push @{$self->DataFile}, $file;
    }
}

sub xml
{
    my $self = shift;

    my $rr;

    if (defined $self->DataFile) {
        $rr = XML::LibXML::Element->new("RawResults");
        foreach my $file (@{$self->DataFile}) {
            my $file_xml = $file->xml();
            $rr->appendChild($file_xml);
        }
    }

    return $rr;
}

sub add_datafile
{
    #Helper method to add a datafile
    #Will remove path and overwrite if file was already added
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
        description => { isa => 'Str' },
        oid => { isa => 'Str', optional => 1 },     # Will be used as oid if specified. Otherwise a default numbered oid will be used
    );
    my $name = $parm{'name'};
    my $description = $parm{'description'};
    my $oid = $parm{'oid'};

    (undef, undef, $name) = File::Spec->splitpath($name);

    # Check if file already added
    if (defined $self->DataFile) {
        foreach my $df (@{$self->DataFile}) {
            if ($df->path eq $name) {
                $df->Description($description);
                return;
            }
        }
    }

    if (not defined $oid) {
        $oid = 'd' . $self->_next_oid;
        $self->_next_oid($self->_next_oid + 1);
    }

    my $df = so::soblock::rawresults::datafile->new(Description => $description, path => $name, oid => $oid);

    $self->DataFile([]) if not defined $self->DataFile;
    push @{$self->DataFile}, $df;
}

1;
