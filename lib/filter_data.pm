package filter_data;

# Package for filtering a NONMEM dataset according to model IGNORE

use strict;
use warnings;
use File::Copy qw(copy);
use MouseX::Params::Validate;
use data;
use model;
use tool::modelfit;
use code_parsing;
use nmtablefile;
use math;
use model_transformations;
use utils::file;
use include_modules;


require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ('all' => [ qw(filter_dataset add_derived_columns) ]);
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );


sub filter_dataset
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        force => { isa => 'Bool', default => 0 },
    );
    my $model = $parm{'model'};
    my $force = $parm{'force'};

    if ($force or $model->need_data_filtering()) {

        my $preprocessdir = 'preprocess_data_dir';
        if (not -d $preprocessdir) {
            mkdir($preprocessdir);
        }

        my $filtered_data_model = $model->copy(
            filename => 'filter_data.mod',
            directory => $preprocessdir,
            copy_datafile => 0,
            copy_output => 0,
            write_copy => 0,
            output_same_directory => 1,
        );
        my @filter_table_header;
        if (defined $filtered_data_model->problems->[0]->inputs and
            defined $filtered_data_model->problems->[0]->inputs->[0]->options) {
            my ($arr, $time_added) = $filtered_data_model->problems->[0]->inputs->[0]->get_filter_table_names();
            croak("found no undropped data column in \$INPUT ") unless (defined $arr);
            croak("automatic filtering cannot yet handle \$INPUT with DATX but without TIME") if ($time_added);
            @filter_table_header = @{$arr};
        } else {
            die("Trying to construct table for filtering data".
                " but no headers were found in \$INPUT");
        }

        foreach my $remove_rec ('abbreviated', 'msfi', 'contr', 'subroutine', 'prior', 'model', 'tol', 'infn', 'omega',
            'pk','aesinitial','aes','des','error','pred','mix','theta','sigma','simulation','estimation','covariance',
            'nonparametric','table','scatter') {
            $filtered_data_model->remove_records(type => $remove_rec);
        }

        $filtered_data_model->add_records(type => 'pred', record_strings => ['Y=THETA(1)+ETA(1)+EPS(1)']);
        $filtered_data_model->add_records(type => 'theta', record_strings => ['1']);
        $filtered_data_model->add_records(type => 'omega', record_strings => ['1']);
        $filtered_data_model->add_records(type => 'sigma', record_strings => ['1']);
        $filtered_data_model->add_records(type => 'estimation', record_strings => ['MAXEVALS=0 METHOD=ZERO']);

        my $datafile = 'filtered.dta';
        $filtered_data_model->add_records(
            type => 'table',
            record_strings => [ join(' ', @filter_table_header) .  ' NOAPPEND NOPRINT ONEHEADER FILE=' . $datafile ]
        );

        $filtered_data_model->_write();

        my $filter_fit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            base_directory => $preprocessdir,
            directory => $preprocessdir.'/data_filtering_dir/',
            models => [ $filtered_data_model ],
            top_tool => 0,
            clean => 2);
        $filter_fit->run;

        my $adjusted_model = $model->copy(
            filename => 'adjusted_model.mod',
            directory => '.',
            copy_datafile => 0,
            copy_output => 0,
            write_copy => 0,
            output_same_directory => 1,
        );

        $adjusted_model->remove_option(record_name => 'data', option_name => 'IGNORE', fuzzy_match => 1);
        $adjusted_model->remove_option(record_name => 'data', option_name => 'ACCEPT', fuzzy_match => 1);
        $adjusted_model->problems->[0]->datas->[0]->set_filename(filename => $datafile);
        $adjusted_model->problems->[0]->datas->[0]->ignoresign('@');

        return $adjusted_model;

    } else {
        return $model;
    }
}

sub derived_covariates_columns
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        columns => { isa => 'ArrayRef' }, 
    );
    my $model = $parm{'model'};
    my $columns = $parm{'columns'};

    my @needed_columns;

    for my $colname (@$columns) {
        if ($model->problems->[0]->find_data_column(column_name => $colname) == -1) {        # Is column in $INPUT?
            if (code_parsing::defined_symbol(model => $model, symbol => $colname)) {
                push @needed_columns, $colname;
            } else {
                croak("The symbol $colname requested, but not available in model.");
            }
        }
    }

    return \@needed_columns;
}

sub add_derived_columns
{
    # Look for provided columns in model. If column is missing, but can be calculated by model
    # run the model to calculate the column. Create a new model that will use the data column instead
    # of the calculation.
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        directory => { isa => 'Str' },
        columns => { isa => 'ArrayRef' }, 
    );
    my $model = $parm{'model'};
    my $directory = $parm{'directory'};
    my $columns = $parm{'columns'};

    my $needed_columns_ref = derived_covariates_columns(model => $model, columns => $columns);
    my @needed_columns = @$needed_columns_ref;

    if (scalar(@needed_columns) == 0) {        # All columns could be found in $INPUT
        return $model;
    }

    my $workdir = "$directory/derived_columns";
    if (not -d $workdir) {
        mkdir($workdir);
    }

    my $deriving_model = $model->copy(
        filename => 'derive_columns.mod',
        directory => $workdir,
        copy_datafile => 1,
        copy_output => 0,
        write_copy => 0,
        output_same_directory => 1,
    );

    # Add column with record number to dataset
    my $data = data->new(
        filename => $deriving_model->datafiles()->[0],
        directory => $workdir,
        idcolumn => $model->idcolumn(),
        ignoresign => $model->ignoresigns->[0]
    );
    my $recno = 0;
    for my $ind (@{$data->individuals}) {
        for my $row (@{$ind->subject_data}) {
            $row .= ",$recno";
            $recno++;
        }
    }
    $data->_write(overwrite => 1);

    $deriving_model->remove_records(type => 'estimation');
    $deriving_model->remove_records(type => 'covariance');
    $deriving_model->remove_records(type => 'simulation');
    $deriving_model->remove_records(type => 'table');

    # Add record number column to $INPUT
    my $recno_name = 'RECNO__';
    my $input_records = $deriving_model->problems->[0]->inputs;
    my $final_input_record = scalar(@$input_records) - 1; 
    $deriving_model->problems->[0]->add_option(record_number => $final_input_record, record_name => 'input', option_name => $recno_name); 

    my $output_table = 'derive_columns.tab';
    $deriving_model->add_records(
        type => 'table',
        record_strings => [ $recno_name . ' ' . join(' ', @needed_columns) . ' NOAPPEND NOPRINT ONEHEADER FILE=' . $output_table ],
    );

    $deriving_model->_write();

    my $deriving_run = tool::modelfit->new(
        %{common_options::restore_options(@common_options::tool_options)},
        base_directory => $workdir,
        directory => $workdir.'/derived_columns_run/',
        models => [ $deriving_model ],
        top_tool => 0,
        clean => 2
    );
    $deriving_run->run;

    # Update the original dataset with derived columns for recnos not removed
    # For removed recnos put 0 in all derived columns
    my $table_file = nmtablefile->new(filename => "$workdir/$output_table"); 
    my $table = $table_file->tables->[0];
    my $table_columns = $table->columns;
    my $currow = 0;

    for my $ind (@{$data->individuals}) {
        for my $row (@{$ind->subject_data}) {
            my @a = split ',', $row;
            my $recno = pop @a;
            if ($table_columns->[0]->[$currow] == $recno) {
                for (my $i = 1; $i < scalar(@$table_columns); $i++) {
                    push @a, $table_columns->[$i]->[$currow];
                }
                $currow++;
                if ($currow >= scalar(@{$table_columns->[0]})) {
                    $recno = math::inf();
                }
            } else {
                push @a, (0) x (scalar(@$table_columns) - 1);
            }
 
            $row = join ',', @a;
        }
    }

    $data->_write(overwrite => 1);

    my $result_model = $model->copy(
        filename => 'result.mod',
        directory => $workdir,
        copy_datafile => 0,
        copy_output => 0,
        write_copy => 0,
        output_same_directory => 1,
    );

    $result_model->problems->[0]->datas->[0]->set_filename(filename => $data->filename, directory => $data->directory); 
    for my $col (@needed_columns) {
        $result_model->problems->[0]->add_option(record_number => $final_input_record, record_name => 'input', option_name => $col); 
    }

    model_transformations::remove_symbol_definition(model => $result_model, symbols => \@needed_columns);

    $result_model->_write();

    my $model_name = $model->full_name();
    cp utils::file::replace_extension($model_name, 'ext'), "$workdir/result.ext";
    cp utils::file::replace_extension($model_name, 'phi'), "$workdir/result.phi";
    cp utils::file::replace_extension($model_name, 'lst'), "$workdir/result.lst";
    return $result_model;
}

1;
