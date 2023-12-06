package tool::proseval;

use strict;
use Mouse;
use MouseX::Params::Validate;
use File::Copy 'copy';
use include_modules;
use data;
use filter_data;
use tool::modelfit;
use array;
use model_transformations;

extends 'tool';

has 'ignore' => ( is => 'rw', isa => 'Str' );
has 'model' => ( is => 'rw', isa => 'model' );
has 'evid_column' => ( is => 'rw', isa => 'Int' );
has 'numobs_indiv' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'sorted_indiv_numobs' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'columns' => ( is => 'rw', isa => 'ArrayRef' );

sub BUILD
{
    my $self = shift;

    my $model = $self->models()->[0];
    my $evid_column = $model->problems->[0]->find_data_column(column_name => 'EVID', ignore_dropped => 0);
    if ($evid_column == -1) {
        die "Error: There is no EVID column in the dataset\n";
    }

    $self->evid_column($evid_column);
    $self->model($model);
}

sub modelfit_setup
{
    my $self = shift;

    my $model = filter_data::filter_dataset(model => $self->model);

    my $n = 1;
    my $continue = 1;

    $model->set_option(record_name => 'estimation', option_name => 'MAXEVALS', fuzzy_match => 1, option_value => '0');
    my @models_to_run;

    my $mdv_column = $model->problems->[0]->find_data_column(column_name => 'MDV');
    if ($mdv_column == -1) {
        $mdv_column = undef;
    }

    my $datafile;
    if (-e "preprocess_data_dir/filtered.dta") {
        $datafile = "preprocess_data_dir/filtered.dta";
    } else {
        $datafile = $model->problems->[0]->datas->[0]->get_absolute_filename();
    }

    while ($continue) {
        my $data = data->new(
            filename => $datafile,
            ignoresign => '@',
            missing_data_token => $self->missing_data_token,
            idcolumn => $self->model->idcolumn(),
        );

        $continue = $self->set_evid(dataset => $data, numzeros => $n, evid_column => $self->evid_column, mdv_column => $mdv_column);

        my $data_filename = "m1/proseval_$n.dta";
        my $model_filename = $self->directory . "m1/proseval_$n.mod";
        $data->_write(filename => $data_filename);

        my $modified_model = $model->copy(filename => $model_filename, output_same_directory => 1);
        $modified_model->problems->[0]->datas->[0]->set_filename(filename => $data_filename);

        # Remove all tables except the first
        my $first_table = $modified_model->problems->[0]->tables->[0];
        $modified_model->problems->[0]->tables([ $first_table ]);
        $modified_model->set_option(record_name => 'table', option_name => 'FILE', option_value => "proseval_$n.tab");
        $modified_model->remove_option(record_name => 'table', option_name => 'PRINT');
        $modified_model->set_option(record_name => 'table', option_name => 'NOPRINT');
        $modified_model->remove_option(record_name => 'table', option_name => 'APPEND');
        $modified_model->set_option(record_name => 'table', option_name => 'NOAPPEND');
        $modified_model->remove_option(record_name => 'table', option_name => 'NOHEADER');
        $modified_model->set_option(record_name => 'table', option_name => 'ONEHEADER');
        my $columns = $first_table->columns();
        $self->columns($columns);

        # Add the number of observation to the model
        model_transformations::prepend_code(model => $modified_model, code => [ "NUMOBS=$n"]);

        $modified_model->_write(filename => $model_filename, relative_data_path => 1);
        push @models_to_run, $modified_model;
        $n++;

        if (not $continue) {
            # Final iteration. Sort numobs before dataset goes out of scope
            for my $individual (@{$data->individuals}) {
                push @{$self->sorted_indiv_numobs}, $self->numobs_indiv->{$individual->idnumber};
            }
        }
    }

    my $modelfit = tool::modelfit->new(
        %{common_options::restore_options(@common_options::tool_options)},
        models => \@models_to_run,
        base_dir => $self->directory . 'm1/',
        directory => undef,
        top_tool => 0,
        copy_data => 0,
    );

    $self->tools([]) unless defined $self->tools;
    push(@{$self->tools}, $modelfit);
}

sub modelfit_analyze
{
    # Collect all tables into one results table

    my $self = shift;

    open my $fh, '>', "results.csv";
    print $fh join(',', @{$self->columns}), ",OBS\n";

    my $numtabs = array::max($self->sorted_indiv_numobs);

    # Open all tables at the same time. Trading memory for speed
    my @tables;

    for my $obsnum (1 .. $numtabs) {
        $tables[$obsnum - 1] = data->new(
            filename => "m1/proseval_$obsnum.tab",
            missing_data_token => $self->missing_data_token,
            idcolumn => 1,
        );
    }

    for my $ind (0 .. scalar(@{$self->sorted_indiv_numobs}) - 1) {
        for my $numobs (1 .. $self->sorted_indiv_numobs->[$ind]) {
            my $individual = $tables[$numobs - 1]->individuals->[$ind];
            for my $line (@{$individual->subject_data}) {
                print $fh "$line,$numobs\n";
            }
        }
    }

    close $fh;
}

sub set_evid
{
    # Leave the $numzeros first zeros for each individual
    # Change the rest of the zeros to twos
    # Report if no zeros were changed
    my $self = shift;
    my %parm = validated_hash(\@_,
        dataset => { isa => 'data', optional => 0 },
        numzeros => { isa => 'Int', optional => 0 },
        evid_column => { isa => 'Int', optional => 0 },
        mdv_column => { isa => 'Maybe[Int]', optional => 1 },
    );
    my $dataset = $parm{'dataset'};
    my $numzeros = $parm{'numzeros'};
    my $evid_column = $parm{'evid_column'};
    my $mdv_column = $parm{'mdv_column'};

    my $set_evid_two = 0;       # Did we set any evid to two?

    for my $individual (@{$dataset->individuals}) {
        my $skipped_zeros = 0;
        my $indiv_set_evid_two = 0;     # Did we set any evid to two for this individual?
        for my $row (@{$individual->subject_data}) {
            my @a = split /,/, $row;
            if ($a[$evid_column] == 0 and not $self->ignore_row(row => \@a)) {
                if ($skipped_zeros < $numzeros) {
                    $skipped_zeros++;
                } else {
                    $set_evid_two = 1;
                    $indiv_set_evid_two = 1;
                    $a[$evid_column] = 2;
                    if (defined $mdv_column) {      # If EVID=2 must set MDV=1
                        $a[$mdv_column] = 1;
                    }
                    $row = join ',', @a;
                }
            }
        }
        if (not $indiv_set_evid_two and not exists $self->numobs_indiv->{$individual->idnumber}) {
            $self->numobs_indiv->{$individual->idnumber} = $numzeros;
        }
    }

    return $set_evid_two;
}

sub ignore_row
{
    # Decide if row should be ignored or not based on the ignore attribute
    my $self = shift;
    my %parm = validated_hash(\@_,
        row => { isa => 'ArrayRef' },
    );
    my $row = $parm{'row'};

    if (not defined $self->ignore) {
        return 0;
    }

    my @ignore = split /,/, $self->ignore;
    foreach my $expr (@ignore) {
        (my $colname, my $value) = split /==/, $expr;

        my $column = $self->model->problems->[0]->find_data_column(column_name => $colname, ignore_dropped => 0);
        if ($column == -1) {
            die "Error: There is no colum $colname to ignore\n";
        }

        if ($row->[$column] == $value) {
            return 1;
        }
    }

    return 0;
}

1;
