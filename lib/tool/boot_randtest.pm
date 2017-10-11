package tool::boot_randtest;

use include_modules;
use strict;
use data;
use log;
use OSspecific;
use tool::modelfit;
use Moose;
use MooseX::Params::Validate;
use tool::randtest;

extends 'tool';

has 'samples' => ( is => 'rw', required => 1, isa => 'Int' );
has 'subjects' => ( is => 'rw', isa => 'HashRef' );
has 'base_model' => ( is => 'rw', isa => 'model' );
has 'stratify_on' => ( is => 'rw', isa => 'Str' );


sub BUILD
{
	my $self  = shift;

	#Find column index of rand column
	#Find column index of strat column
    #my $counter = 0;
    #foreach my $opt (@{$self->models->[0]->problems->[0]->inputs->[0]->options()}){
#		$self->rand_index($counter) if ($opt->name() eq $self->randomization_column());
#		$self->strat_index($counter) if ((defined $self->stratify_on()) and ($opt->name() eq $self->stratify_on()));
#		$counter++;
#	}
    #croak("Could not find randomization column " . $self->randomization_column() ." in \$INPUT")
    #unless (defined $self->rand_index);
    #croak("Could not find stratification column " . $self->stratify_on() . " in \$INPUT")
    #unless ((not defined $self->stratify_on) or (defined $self->strat_index));

	croak("Number of samples must be larger than 0") unless ($self->samples() > 0);
}

sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	my $model = $self->models->[$model_number - 1];
    
    my $stratify_on;
    if (defined $self->stratify_on) {
        my $stratify_on = $model->problems->[0]->find_data_column(column_name => $self->stratify_on);
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
            stratify_on => $stratify_on,
            input_filename => $model->problems->[0]->datas->[0]->get_absolute_filename(),
            ignoresign => $model->ignoresigns->[0],
            idcolumn => $model->problems->[0]->find_data_column(column_name => 'ID') + 1,
            missing_data_token => $self->missing_data_token,
        );

    for (my $i = 0; $i < $self->samples; $i++) {
        my $base = $self->base_model;
        $base->directory('m1/');
        $base->filename("base_" . ($i + 1) . ".mod");
        $base->problems->[0]->datas->[0]->set_filename(filename => 'bs_' . ($i + 1) . '.dta', directory => "m1/");
        $base->_write();
        my $orig = $model;
        $orig->directory('m1/');
        $orig->filename("orig_" . ($i + 1) . ".mod");
        $orig->problems->[0]->datas->[0]->set_filename(filename => 'bs_' . ($i + 1) . '.dta', directory => "m1/");
        $orig->_write();

        my $rand = tool::randtest->new(
            %{common_options::restore_options(@common_options::tool_options)},
            top_tool => 0,
            prepend_model_file_name => 1,
            models => [ $orig ],
            samples	=> 1,
            base_model => $base,
            randomization_column => $self->stratify_on,
        );

        #$rand->print_options (cmd_line => $cmd_line,
        #    toolname => 'randtest',
        #    local_options => [keys %optional_options],
        #    common_options => \@common_options::tool_options);

        $rand->run();
        $rand->prepare_results();
        $rand->print_results();
    }

    open my $dh, '>', 'raw_results.csv';
    for (my $i = 0; $i < $self->samples; $i++) {
        open my $fh, '<', 'randtest_dir' . ($i + 1) . '/raw_results.csv' or die "Could not open rand_test raw_results file";
        my $line = <$fh>;
        if ($i == 0) {
            print $dh $line;
        }
        <$fh>;
        <$fh>;
        $line = <$fh>;
        my @a = split ',', $line;
        $a[0] = $i + 1;
        $line = join ',', @a;
        print $dh $line;
        close $fh;
    }
    close $dh;
}

sub modelfit_analyze
{
    my $self = shift;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
