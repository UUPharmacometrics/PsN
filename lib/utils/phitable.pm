package phitable;

# A class representing a phi file to modify it (used by FREM for filling zeroes and reorganizing)

use include_modules;
use nmtablefile;
use Moose;
use MooseX::Params::Validate;

use Digest::file qw(digest_file_hex);

has 'path' => ( is => 'rw', isa => 'Str' );
has 'filename' => ( is => 'rw', isa => 'Maybe[Str]', default => undef );
has 'table' => ( is => 'rw', isa => 'nmtable' );
has 'valid' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'num_rows' => ( is => 'rw', isa => 'Int', default => 0 );
has 'num_etas' => ( is => 'rw', isa => 'Int', default => 0 );

# TODO: extends nmtable;

sub BUILD
{
    my $self = shift;

    my $valid = $self->validate_phi(path => $self->path);
    $self->valid( $valid );
    my $num_etas = $self->num_etas;
    my $num_rows = $self->num_rows;

    print "num_etas=$num_etas and num_rows=$num_rows\n";
}

sub validate_phi
{
    # read and validate that phi file is according to expected spec
    my $self = shift;
    my %parm = validated_hash(\@_,
        path => { isa => 'Str', optional => 0 },
    );
    my $path = $parm{'path'};
    my ($dir, $filename) = OSspecific::absolute_path("", $path);
    print "dir=$dir and filename=$filename\n";
    $self->filename( $filename );

    # load table
    my $tables = nmtablefile->new(filename => $path);
    my $num_tables = scalar @{$tables->tables};
    if ($num_tables != 1) {
        croak "phi file must contain exactly 1 table ($filename has $num_tables)";
    }
    my $table = $tables->get_table(index => 0);
    $self->table( $table );

    # load as matrix and header
    my $size = scalar(@{$table->columns});
    my $table_mat = [];
    for (my $i=0; $i<$size; $i++) {
        my $col = $table->get_column(index => $i);
        if (scalar @{$col} == 0) {
            croak "phi file ($filename) has no rows";
        }
        push @{$table_mat}, $col;
    }
    my $header = $table->get_header;
    unless (scalar @{$header} == $size) {
        croak "phi file ($filename) error: header missing or wrong length";
    }

    # validate all columns as expected
    unless ($header->[0] eq "SUBJECT_NO" and $header->[1] eq "ID") {
        croak "phi file ($filename) SUBJECT_NO and ID cols must be first cols";
    }
    unless ($header->[-1] eq "OBJ") {
        croak "phi file ($filename) OBJ col must be last col";
    }
    my $eta_count = 1;
    my $etc_x_count = 1;
    my $etc_y_count = 1;
    foreach my $name (@{$header}[2..($size-1)]) {
        if ($name =~ /^ETA\(\d+\)$/) {
            my ($num) = $name =~ /(\d+)/;
            if ($num != $eta_count) {
                my $missing = $eta_count-1;
                croak "phi file ($filename) error: missing ETA($missing) col";
            } elsif ($etc_x_count > 1 or $etc_y_count > 1) {
                croak "phi file ($filename) error: ETA col $name after ETC col";
            }

            $eta_count++;
        } elsif ($name =~ /^ETC\(\d+,\d+\)$/) {
            my @num = $name =~ /(\d+)/g;
            if ($num[0] != $etc_x_count || $num[1] != $etc_y_count) {
                croak "phi file ($filename) error: missing ETC($etc_x_count,$etc_y_count) col";
            } elsif ($num[0] > $eta_count || $num[1] > $eta_count) {
                croak "phi file ($filename) error: $name col larger indices than ETA cols";
            }

            if ($etc_y_count < $etc_x_count) {
                $etc_y_count++;
            } else {
                $etc_x_count++;
                $etc_y_count = 1;
            }
        } elsif ($name eq "OBJ") {
            last;
        } else {
            croak "phi file ($filename) error: unknown column $name";
        }
    }

    # now we know phi file is ordered exactly as expected (important for modifications)
    $self->num_rows( scalar @{$table_mat->[0]} );
    $self->num_etas( $eta_count-1 );
    return 1;
}

sub swap_etas
{
    # swap two ETA columns (1-indexed as in header), renumber and shift + renumber ETC columns
    my $self = shift;
    my %parm = validated_hash(\@_,
        eta_num_a => { isa => 'Int', optional => 0 },
        eta_num_b => { isa => 'Int', optional => 0 },
    );
    my $eta_num_a = $parm{'eta_num_a'};
    my $eta_num_b = $parm{'eta_num_b'};
    my $netas = $self->num_etas;
    croak "eta_num_a out of bounds ($netas ETA cols)" if ($eta_num_a <= 0 || $eta_num_a > $netas);
    croak "eta_num_b out of bounds ($netas ETA cols)" if ($eta_num_b <= 0 || $eta_num_b > $netas);
    croak "can't modify non-valid phi" unless ($self->valid);

    # get old columns, header array and hash (before reordering)
    my @columns = @{$self->table->columns};
    my @header_array = @{$self->table->header_array};
    my %header_hash = %{$self->table->header};

    # reorder array: store new index for each old index position (before reordering)
    my @reorder;
    for (my $i=0; $i<(scalar @header_array); $i++) {
        # keep old index as default
        $reorder[$i] = $i;

        my $name = $header_array[$i];
        if ($name =~ /^ETA\(\d+\)$/) {
            my ($old_eta) = $name =~ /(\d+)/;
            my $swap_eta;
            $swap_eta = $eta_num_a if ($eta_num_b == $old_eta);
            $swap_eta = $eta_num_b if ($eta_num_a == $old_eta);
            if (defined $swap_eta) {
                # eta index match index to swap: new index becomes old index of ETA to swap with
                my $swap_name = "ETA($swap_eta)";
                $reorder[$i] = $header_hash{$swap_name};
                print "MOVED $header_array[$i] [$i] => $swap_name [$reorder[$i]]\n";
            }
        } elsif ($name =~ /^ETC\(\d+,\d+\)$/) {
            my @old_etas = $name =~ /(\d+)/g;
            my ($swap_eta_1, $swap_eta_2);
            $swap_eta_1 = $eta_num_a if ($old_etas[0] == $eta_num_b);
            $swap_eta_1 = $eta_num_b if ($old_etas[0] == $eta_num_a);
            $swap_eta_2 = $eta_num_a if ($old_etas[1] == $eta_num_b);
            $swap_eta_2 = $eta_num_b if ($old_etas[1] == $eta_num_a);
            if (defined $swap_eta_1 or defined $swap_eta_2) {
                # at least one of the indices match one of the indices to swap, if any is non-defined
                # (e.g. 2 of ETC(2,1) where ETA 1 and 3 to swap) assign old index
                $swap_eta_1 //= $old_etas[0];
                $swap_eta_2 //= $old_etas[1];

                # now we can generate the new name: new index becomes old index of ETC to swap with
                my $swap_name;
                if ($swap_eta_1 >= $swap_eta_2) {
                    $swap_name = "ETC($swap_eta_1,$swap_eta_2)";
                } else {
                    $swap_name = "ETC($swap_eta_2,$swap_eta_1)";
                }
                $reorder[$i] = $header_hash{$swap_name};
                print "MOVED $header_array[$i] [$i] => $swap_name [$reorder[$i]]\n";
            }
        }
    }

    # create the new column array (note: header and header hash do NOT change; ETAs get "renumbered")
    my $new_columns = [];
    for (my $i=0; $i<(scalar @reorder); $i++) {
        # get new index and column and push
        my $new_index = $reorder[$i];
        my $new_col = $columns[$new_index];
        push @{$new_columns}, $new_col;
    }

    # update object
    $self->table->columns( $new_columns );
}

sub add_zero_etas
{
    # appends correctly numbered ETA columns with 0s to (proper) end of phi table,
    # and adds new ETC values for each new ETA column (also filled with 0s)
    my $self = shift;
    my %parm = validated_hash(\@_,
        num_etas => { isa => 'Int', optional => 0 },
    );
    my $num_zero_etas = $parm{'num_etas'};
    croak "num_etas < 0" if ($num_zero_etas < 0);

    croak "can't modify non-valid phi" unless ($self->valid);
    my $nrows = $self->num_rows;
    my $netas = $self->num_etas;
    my $columns = $self->table->columns;
    my $header_hash = $self->table->header;
    my $header_array = $self->table->header_array;

    # push new zero ETA and ETC columns and new header entries
    my $eta_cols;
    my @eta_names;
    for (my $i=1; $i<=$num_zero_etas; $i++) {
        my $idx = $i+$netas;
        my $col = [ (0) x $nrows ];
        my $name = "ETA($idx)";

        # 2 offset for SUBJECT_NO and ID and -1 for 1-indexing
        splice @{$columns},      (2 + $netas + $i-1), 0, $col;
        splice @{$header_array}, (2 + $netas + $i-1), 0, $name;

        for (my $j=1; $j<=($netas+$i); $j++) {
            my $x = $netas + $i;
            my $y = $j;
            my $col = [ (0) x $nrows ];
            my $name = "ETC($x,$y)";

            # current size offset for all added ETA cols and -1 for 1-indexing
            my $cur_size = scalar @{$columns};
            splice @{$columns},      ($cur_size - 1), 0, $col;
            splice @{$header_array}, ($cur_size - 1), 0, $name;
        }
    }
    $netas += $num_zero_etas;

    # update column hash
    for (my $i=0; $i<scalar(@{$header_array}); $i++) {
        $header_hash->{ $header_array->[$i] } = $i;
    }

    # update object
    $self->num_etas( $netas );
    $self->table->columns( $columns );
    $self->table->header( $header_hash );
    $self->table->header_array( $header_array );
}

sub write
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        path => {isa => 'Str', default => 1, optional => 0},
    );
    my $path = $parm{'path'};
    croak "can't write non-valid phi" unless ($self->valid);
    my $tab = $self->table;
    my $nrow = $self->num_rows;

    # output TABLE header
    open(my $fh, '>', $path) or croak("could not open $path for phi write");
    my ($tabno,$meth,$prob,$sub,$sup1,$it1,$sup2,$it2) = (
        $tab->table_number,
        $tab->method,
        $tab->problem,
        $tab->subproblem,
        $tab->superproblem1,
        $tab->iteration1,
        $tab->superproblem2,
        $tab->iteration2,
    );
    printf $fh "TABLE NO. %5s: ", $tabno;
    print $fh "$meth: Problem=$prob Subproblem=$sub Superproblem1=$sup1 Iteration1=$it1 Superproblem2=$sup2 Iteration2=$it2\n";

    # output table header in neat columns as NONMEM
    my @header = @{$tab->header_array};
    my $ncol = scalar @header;
    my $format = "%-12s " x $ncol;
    printf $fh " $format\n", @header;

    # transpose columns vector into row vector (for output)
    my $columns = $tab->columns;
    my @rows;
    for (my $r=0; $r<$nrow; $r++) {
        my $row = [];
        for (my $c=0; $c<$ncol; $c++) {
            my $val = $columns->[$c]->[$r];
            push @{$row}, $val;
        }
        push @rows, $row;
    }

    # output all rows in neat columns as NONMEM
    for (my $row=0; $row<$nrow; $row++) {
        my $rvec = $rows[$row];
        # format: first 2 cols are SUBJECT_NO & ID (output as is), then fix precision and last is OBJ (full precision)
        my $format = ("%12s " x 2).( "%12.5E " x ($ncol-3) )."%s";
        printf $fh " $format\n", @{$rvec};
    }
    close $fh;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
