package phitable;

# A class representing a phi file to modify it (used by FREM for filling zeroes)
# Should be merged into a table subclass

use include_modules;
use nmtablefile;
use Mouse;
use MouseX::Params::Validate;


has 'path' => ( is => 'rw', isa => 'Str' );
has 'filename' => ( is => 'rw', isa => 'Maybe[Str]', default => undef );
has 'table' => ( is => 'rw', isa => 'nmtable' );
has 'valid' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'num_rows' => ( is => 'rw', isa => 'Int', default => 0 );
has 'num_etas' => ( is => 'rw', isa => 'Int', default => 0 );

# contains ("ETA","ETC") or ("PHI","PHC") after validate_phi:
has 'types' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );


sub BUILD
{
    my $self = shift;

    my $valid = $self->validate_phi(path => $self->path);
    $self->valid($valid);
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
    $self->filename( $filename );

    # load table
    my $tables = nmtablefile->new(filename => $path);
    my $num_tables = scalar @{$tables->tables};
    my $table = $tables->get_table(index => $num_tables - 1);
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
    my @types = ();
    foreach my $name (@{$header}[2..($size-1)]) {
        # get ETA/ETC or PHI/PHC type (estimation method dependent)
        if (scalar @types == 0) {
            my ($type) = $name =~ /(.+)\(/;
            if ($type eq "ETA" or $type eq "ETC") {
                @types = ("ETA", "ETC");
                # print "ETA type detected\n";
            } else {
                @types = ("PHI", "PHC");
                # print "PHI type detected\n";
            }
        }

        if ($name =~ /$types[0]\(\d+\)/) {
            my ($num) = $name =~ /(\d+)/;
            if ($num != $eta_count) {
                my $missing = $eta_count-1;
                croak "phi file ($filename) error: missing $types[0]($missing) col";
            } elsif ($etc_x_count > 1 or $etc_y_count > 1) {
                croak "phi file ($filename) error: $types[0] col $name after shrinkage col $types[1]";
            }

            $eta_count++;
        } elsif ($name =~ /$types[1]\(\d+,\d+\)/) {
            my @num = $name =~ /(\d+)/g;
            if ($num[0] != $etc_x_count || $num[1] != $etc_y_count) {
                croak "phi file ($filename) error: missing expected $types[1]($etc_x_count,$etc_y_count) col";
            } elsif ($num[0] > $eta_count || $num[1] > $eta_count) {
                croak "phi file ($filename) error: $name col larger indices than parsed $types[0] cols";
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
    $self->types( \@types );
    return 1;
}

sub add_zero_etas
{
    # appends correctly numbered ETA/PHI columns with 0s to (proper) end of phi table,
    # and adds new ETC/PHC values for each new ETA/PHI column (also filled with 0s)
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
    my @types = @{ $self->types };

    # push new zero ETA/PHI and ETC/PHC columns and new header entries
    my $eta_cols;
    my @eta_names;
    for (my $i=1; $i<=$num_zero_etas; $i++) {
        my $idx = $i+$netas;
        my $col = [ (0) x $nrows ];
        my $name = "$types[0]($idx)";

        # 2 offset for SUBJECT_NO and ID and -1 for 1-indexing
        splice @{$columns},      (2 + $netas + $i-1), 0, $col;
        splice @{$header_array}, (2 + $netas + $i-1), 0, $name;

        for (my $j=1; $j<=($netas+$i); $j++) {
            my $x = $netas + $i;
            my $y = $j;
            my $col = [ (0) x $nrows ];
            my $name = "$types[1]($x,$y)";

            # current size offset for all added ETA/PHI cols and -1 for 1-indexing
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

1;
