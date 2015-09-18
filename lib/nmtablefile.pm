package nmtablefile;

# A class representing a nonmem table file

use include_modules;
use Moose;
use MooseX::Params::Validate;
use nmtable;

has 'filename' => ( is => 'rw', isa => 'Str' );
has 'tables' => ( is => 'rw', isa => 'ArrayRef[nmtable]', default => sub { [] } );

sub BUILD
{
    my $self = shift;

    if (defined $self->filename) {
        $self->read_nmtable(filename => $self->filename);
    } 
}

sub read_nmtable
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		filename => { isa => 'Str' },
	);
    my $filename = $parm{'filename'};

    $self->filename($filename);

    open my $fh, '<', $filename;

    my $table_row = <$fh>;

    TABLE: while (1) {
        my $table = nmtable->new();
        $table->read_table_row(row => $table_row);
        my $header_row = <$fh>;
        if ($header_row =~ /^TABLE NO./) {   #Header row is actually new table row
            $table_row = $header_row;
            push @{$self->tables}, $table;
            next TABLE;
        }
        $table->set_header(header => $header_row);

        my $row;

        ROW: while (1) {
            $row = <$fh>;
            if (not defined $row) {
                push @{$self->tables}, $table;
                last TABLE;
            }
            if ($row =~ /^TABLE NO./) {
                $table_row = $row;
                push @{$self->tables}, $table;
                last ROW;
            }
            $table->add_row(row => $row);
        }
    }
    
    close $fh;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
