package tool::boot_randtest;

use strict;
use include_modules;
use Mouse;
use MouseX::Params::Validate;
use OSspecific;
use tool::modelfit;
use data;
use tool::randtest;
use model_transformations;

extends 'tool';

has 'samples' => ( is => 'rw', isa => 'Int' );
has 'subjects' => ( is => 'rw', isa => 'HashRef' );
has 'base_model' => ( is => 'rw', isa => 'model' );
has 'stratify_on' => ( is => 'rw', isa => 'Str' );
has 'random_column' => ( is => 'rw', isa => 'Str' );     # Column to replace with a dichotomous column for randomizing
has 'summarize' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'replacement' => ( is => 'rw', isa => 'Bool', default => 0 );


sub BUILD
{
    my $self  = shift;

    my $repl_text = " no";
    if ($self->replacement) {
        $repl_text = "";
    }
    print "boot_randtest is bootstrapping with$repl_text replacement.\n";

    #Find column index of rand column
    #Find column index of strat column
    #my $counter = 0;
    #foreach my $opt (@{$self->models->[0]->problems->[0]->inputs->[0]->options()}){
#        $self->rand_index($counter) if ($opt->name() eq $self->randomization_column());
#        $self->strat_index($counter) if ((defined $self->stratify_on()) and ($opt->name() eq $self->stratify_on()));
#        $counter++;
#    }
    #croak("Could not find randomization column " . $self->randomization_column() ." in \$INPUT")
    #unless (defined $self->rand_index);
    #croak("Could not find stratification column " . $self->stratify_on() . " in \$INPUT")
    #unless ((not defined $self->stratify_on) or (defined $self->strat_index));

    if (not $self->summarize) {
        croak("Number of samples must be larger than 0") unless ($self->samples() > 0);
    }
}

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    if ($self->summarize) {
        return;
    }

    my $model = $self->models->[$model_number - 1];

    my $stratify_on;
    if (defined $self->stratify_on) {
        $stratify_on = $model->problems->[0]->find_data_column(column_name => $self->stratify_on);
        if ($stratify_on == -1) {
            croak("Could not find -stratify_on column ". $self->stratify_on . " in \$INPUT of model.\n");
        }
    }

    my ( $new_datas, $incl_ids, $incl_keys, $new_subjects, $orig_count_ind, $all_individuals )
        = data::bootstrap_create_datasets(
            output_directory => $self->directory() . '/m' . $model_number,
            name_stub => 'bs',
            samples => $self->samples,
            subjects => $self->subjects,
            stratify_on => $stratify_on + 1,    # Counted from 1
            input_filename => $model->problems->[0]->datas->[0]->get_absolute_filename(),
            ignoresign => $model->ignoresigns->[0],
            idcolumn => $model->problems->[0]->find_data_column(column_name => 'ID') + 1,
            missing_data_token => $self->missing_data_token,
            replacement => $self->replacement,
        );

    if (defined $self->random_column) {
        # Add column with 0 and 1 for each individual 50-50
        for (my $i = 1; $i <= $self->samples; $i++) {
            my $data = data->new(
                filename => "bs_$i.dta",
                directory => 'm1/',
                ignoresign => '@',
                missing_data_token => $self->missing_data_token,
                idcolumn => $model->idcolumn(),
            );
            my $ninds = scalar(@{$data->individuals});
            my $nzeroes = int($ninds / 2);
            for (my $j = 0; $j < $nzeroes; $j++) {
                for my $row (@{$data->individuals->[$j]->subject_data}) {
                    $row .= ",0";
                }
            }
            for (my $j = $nzeroes; $j < $ninds; $j++) {
                for my $row (@{$data->individuals->[$j]->subject_data}) {
                    $row .= ",1";
                }
            }
            $data->_write(overwrite => 1);
        }

        $model->add_option(record_name => 'input', option_name => 'NEW_');
        model_transformations::rename_symbol(model => $model, from => $self->random_column, to => 'NEW_');
    }

    for (my $i = 0; $i < $self->samples; $i++) {
        my $base = $self->base_model;
        $base->directory('m1/');
        $base->filename("base_" . ($i + 1) . ".mod");
        $base->problems->[0]->datas->[0]->set_filename(filename => 'bs_' . ($i + 1) . '.dta', directory => "m1/");
        $base->_write();
        $base->outputs->[0]->directory("m1/");
        $base->outputs->[0]->filename("base_" . ($i + 1) . ".lst");
        $base->outputs->[0]->problems([]);

        my $orig = $model;
        $orig->directory('m1/');
        $orig->filename("orig_" . ($i + 1) . ".mod");
        $orig->problems->[0]->datas->[0]->set_filename(filename => 'bs_' . ($i + 1) . '.dta', directory => "m1/");
        $orig->_write();

        my $randomization_column = $self->stratify_on;
        if ($self->random_column) {
            $randomization_column = 'NEW_';
        }

        my %restored_options = %{common_options::restore_options(@common_options::tool_options)};
        delete $restored_options{'directory'};

        my $clean_level = $self->clean;
        if ($self->clean > 2) {     # Cap clean level at 2 for randtests
            $clean_level = 2;
        }
        eval {
            my $rand = tool::randtest->new(
                %restored_options,
                top_tool => 0,
                prepend_model_file_name => 1,
                models => [ $orig ],
                samples    => 1,
                base_model => $base,
                randomization_column => $randomization_column,
                update_inits => 0,
                clean => $clean_level,
            );

            $rand->run();
            $rand->prepare_results();
            $rand->print_results();
        };
        if ($@) {
            print "error in randtest run $i: $@\n";
        }
    }
}

sub modelfit_analyze
{
    my $self = shift;

    my $samples;
    if ($self->summarize) {
        my @randtestdirs = glob 'randtest_dir*';
        $samples = scalar(@randtestdirs);
    } else {
        $samples = $self->samples;
    }

    my @dofv;
    open my $dh, '>', 'raw_results.csv';
    for (my $i = 0; $i < $samples; $i++) {
        open my $fh, '<', 'randtest_dir' . ($i + 1) . '/raw_results.csv' or next;
        my $line = <$fh>;
        if ($i == 0) {
            print $dh $line;
        }
        <$fh>;
        <$fh>;
        $line = <$fh>;
        my @a = split ',', $line;
        push @dofv, $a[20];
        $a[0] = $i + 1;
        $line = join ',', @a;
        print $dh $line;
        close $fh;
    }
    close $dh;
    tool::randtest::print_dofv_results(dofv => \@dofv, filename => 'boot_randtest_results.csv');
}

1;
