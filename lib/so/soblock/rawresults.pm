package so::soblock::rawresults;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use utils::file;
use so::soblock::rawresults::datafile;

has 'DataFile' => ( is => 'rw', isa => 'ArrayRef[so::soblock::rawresults::datafile]' );
has '_next_oid' => ( is => 'rw', isa => 'Int', default => 1 );  # The next description oid to use by add_datafile

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

sub add_datafile
{
    #Helper method to add a datafile
    #Will remove path and overwrite if file was already added
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
        description => { isa => 'Str' },
    );
    my $name = $parm{'name'};
    my $description = $parm{'description'};

    $name = utils::file::remove_path($name);

    # Check if file already added
    if (defined $self->DataFile) {
        foreach my $df (@{$self->DataFile}) {
            if ($df->path eq $name) {
                $df->Description($description);
                return;
            }
        }
    }

    my $df = so::soblock::rawresults::datafile->new(Description => $description, path => $name, oid => 'd' . $self->_next_oid);
    $self->_next_oid($self->_next_oid + 1);

    $self->DataFile([]) if not defined $self->DataFile;
    push @{$self->DataFile}, $df;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
