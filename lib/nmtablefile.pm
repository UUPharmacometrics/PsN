package nmtablefile;

# A class representing a nonmem table file

use include_modules;
use Mouse;
use MouseX::Params::Validate;
use nmtable;
use math qw(trinum);


has 'filename' => ( is => 'rw', isa => 'Str' );
has 'tables' => ( is => 'rw', isa => 'ArrayRef[nmtable]', default => sub { [] } );
has 'is_ext_file' => ( is => 'rw', isa => 'Bool', default => 0);
has 'has_evaluation' => ( is => 'rw', isa => 'Bool', default => 0);
has 'table_lookup' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'problem_lookup' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );


sub BUILD
{
    my $self = shift;

    if (defined $self->filename) {
        $self->read_nmtable(filename => $self->filename);
    }
}

sub get_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        index => { isa => 'Int', optional => 1 },
        number => { isa => 'Int', optional => 1 },
        problem => { isa => 'Int', optional => 1 },
        subproblem => { isa => 'Int', optional => 1 },
    );
    my $index = $parm{'index'};
    my $number = $parm{'number'};
    my $problem = $parm{'problem'};
    my $subproblem = $parm{'subproblem'};

    if (defined $problem and defined $subproblem and
        defined $self->problem_lookup->{$problem} and
        defined $self->problem_lookup->{$problem}->{$subproblem}){
        return $self->tables->[$self->problem_lookup->{$problem}->{$subproblem}];
    }
    if (defined $number and defined $self->table_lookup->{$number}){
        return $self->tables->[$self->table_lookup->{$number}];
    }
    if (defined $index and $index < scalar(@{$self->tables})){
        #works for -1 (last table) also
        return $self->tables->[$index];
    }
    return undef;
}

sub add_table
{
    my $self = shift;
    my $table = shift;

    push @{$self->tables}, $table;
    if (defined $table->table_number){
        $self->table_lookup->{$table->table_number} = (scalar(@{$self->tables})-1);
    }
    if (defined $table->problem and defined $table->subproblem){
        #if multiple $EST they will have same subproblem number, we will store the last one only
        $self->problem_lookup->{$table->problem}->{$table->subproblem} = (scalar(@{$self->tables})-1);
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

    open my $fh, '<', $filename or die("Could not open table $filename\n");

    my $table_row = <$fh>;

    TABLE: while (defined $table_row) {
        my $table = nmtable->new();
        if ($self->is_ext_file){
            if (($table_row =~ /\(Evaluation\)/) or ($table_row =~ /\(EVALUATION\)/)){
                $self->has_evaluation(1);
            }
        }
        $table->read_table_row(row => $table_row);
        my $header_row = <$fh>;
        if (not defined $header_row){ #table row without header row following
            $self->add_table($table);
            last TABLE;
        }elsif ($header_row =~ /^TABLE NO./) {   #Header row is actually new table row
            $table_row = $header_row;
            $self->add_table($table); #previously read table
            next TABLE;
        }
        $table->set_header(header => $header_row);

        my $row;

        ROW: while (1) {
            $row = <$fh>;
            if (not defined $row) {
                $self->add_table($table);
                last TABLE;
            }
            if ($row =~ /^TABLE NO./) {
                $table_row = $row;
                $self->add_table($table);
                last ROW;
            }
            if ($self->is_ext_file){
                next unless ($row =~ /\s*-100000000[0-6]/);
            }
            $table->add_row(row => $row);
        }
    }

    close $fh;
}

sub _replace_names_in_header
{
    # Replace all names in a replacement hash in a table header string
    my %parm = validated_hash(\@_,
        header => { isa => 'Str' },
        replacements => { isa => 'HashRef' },
    );
    my $header = $parm{'header'};
    my $replacements = $parm{'replacements'};

    for my $key (keys %$replacements) {
        my $replacement = $replacements->{$key};
        $header =~ s/\b$replacement\b/$key/;
    }

    return $header;
}

sub rename_column_names
{
    # Function to rename column names of table using a hash of names => replacements
    # This can be used as a workaround for the NONMEM caveat causing synonyms to always show up in table headers
    my %parm = validated_hash(\@_,
        filename => { isa => 'Str' },
        replacements => { isa => 'HashRef' },
    );
    my $filename = $parm{'filename'};
    my $replacements = $parm{'replacements'};

    open my $sh, '<', $filename;
    open my $dh, '>', "$filename.new";
    my $first_line = <$sh>;
    print $dh $first_line;
    my $header = <$sh>;
    $header = _replace_names_in_header(header => $header, replacements => $replacements);
    print $dh $header;
    while (my $line = <$sh>) {
        print $dh $line;
    }
    close $dh;
    close $sh;
    unlink $filename;
    rename "$filename.new", $filename;
}

sub write
{
    # Write the NONMEM tablefile to disk. Supports phi-files and regular tables
    # WARNING: Assuming that columns contain the original strings
    my $self = shift;
    my %parm = validated_hash(\@_,
        path => { isa => 'Str' },
        phi => { isa => 'Bool', default => 0 },     # Is it a phi-file
        colsize => { isa => 'Int', default => 13 },     # The size in characters of one column
    );
    my $path = $parm{'path'};
    my $phi = $parm{'phi'};
    my $colsize = $parm{'colsize'};

    open my $fh, '>', $path or croak "Could not open $path for writing";

    for my $nmtable (@{$self->tables}) {
        if ($phi) {
            printf $fh "TABLE NO.%6s: %s: Problem=%d Subproblem=%d Superproblem1=%d Iteration1=%d Superproblem2=%d Iteration2=%d\n",
                $nmtable->table_number, $nmtable->method, $nmtable->problem, $nmtable->subproblem, $nmtable->superproblem1,
                $nmtable->iteration1, $nmtable->superproblem2, $nmtable->iteration2;
        } else {
            printf $fh "TABLE NO.%3s\n", $nmtable->table_number;
        }

        my @header = @{$nmtable->header_array};
        print $fh ' ';
        for my $colname (@header[0..($#header - 1)]) {
            printf $fh "%-${colsize}s", $colname;
        }
        print $fh $header[-1], "\n";

        for (my $row = 0; $row < scalar(@{$nmtable->columns->[0]}); $row++) {
            for (my $col = 0; $col < scalar(@{$nmtable->columns}); $col++) {
                my $item = $nmtable->columns->[$col]->[$row];
                if ($col <= 1 and $phi) {    # Integer format for phi file
                    printf $fh "%${colsize}d", $item;
                } elsif (not $phi or $col < scalar(@{$nmtable->columns}) - 1) {
                    if ($item > 0 or ($item == 0 and substr($item, 0, 1) ne '-')) {     # Account for negative zero
                        print $fh "  ";
                    } else {
                        print $fh " ";
                    }
                    printf $fh $item;
                } else {        # OBJ column
                    if ($item < 0 and $item > -1) {
                        print $fh "  ";
                    } elsif ($item > 1) {
                        print $fh "    ";
                    } else {
                        print $fh "   ";
                    }
                    print $fh $item;
                }
            }
            print $fh "\n";
        }
    }

    close $fh;
}

sub rearrange_etas
{
    # Will rearrange all ETAs in a phi file using a hash from old eta numbers to new.
    # Reordering hash must be complete, i.e. cover all etas
    # This is specific to phi files and could be put in a subclass in the future
    my $self = shift;
    my %parm = validated_hash(\@_,
        order => { isa => 'HashRef' },
    );
    my $order = $parm{'order'};

    my %new_to_old = reverse %$order;
    my $netas = scalar(keys %new_to_old);

    for my $nmtable (@{$self->tables}) {
        my $old_columns = $nmtable->columns;
        my @new_columns = ($nmtable->columns->[0], $nmtable->columns->[1]);     # SUBJECT_NO and ID
        for (my $new_eta = 1; $new_eta <= $netas; $new_eta++) {                 # ETA columns
            my $old_eta = $new_to_old{$new_eta};
            push @new_columns, $nmtable->columns->[1 + $old_eta];
        }
        for (my $new_row = 1; $new_row <= $netas; $new_row++) {                 # ETC columns
            for (my $new_col = 1; $new_col <= $new_row; $new_col++) {
                my $old_row = $new_to_old{$new_row};
                my $old_col = $new_to_old{$new_col};
                if ($old_col > $old_row) {              # Did we end up in upper triangle?
                    ($old_row, $old_col) = ($old_col, $old_row);
                }
                my $index = trinum($old_row - 1) + $old_col - 1;
                push @new_columns, $nmtable->columns->[2 + $netas + $index];
            }
        }
        push @new_columns, $nmtable->columns->[-1];
        $nmtable->columns(\@new_columns);
    }
}

sub renumber_l2_column
{
    # Renumbers a L2 column to use unique numbers for each level starting from 1 for each ID
    my $self = shift;
    my %parm = validated_hash(\@_,
        column => { isa => 'Int' },        # The index of the L2 column
        format => { isa => 'Str', default => '%.8E' },
    );
    my $column = $parm{'column'};
    my $format = $parm{'format'};

    for my $nmtable (@{$self->tables}) {
        my $l2_index = $column;
        my $l2_column = $nmtable->columns->[$l2_index];
        my $id_index = $nmtable->header->{'ID'};
        my $id_column = $nmtable->columns->[$id_index];
        my @new_column;
        my $new_l2 = 1;
        my $current_id = $id_column->[0];
        my $current_l2 = $l2_column->[0];
        for (my $i = 0; $i < scalar(@$id_column); $i++) {
            if ($current_id != $id_column->[$i]) {
                $current_id = $id_column->[$i];
                $current_l2 = $l2_column->[$i];
                $new_l2 = 1;
            }
            if ($current_l2 != $l2_column->[$i]) {
                $current_l2 = $l2_column->[$i];
                $new_l2++;
            }
            push @new_column, sprintf($format, $new_l2);
        }
        $nmtable->columns->[$l2_index] = \@new_column;
    }
}

1;
