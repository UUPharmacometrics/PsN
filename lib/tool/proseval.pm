package tool::proseval;

use include_modules;
use strict;
use File::Copy 'cp';
use data;
use log;
use tool::modelfit;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'model' => ( is => 'rw', isa => 'model' ); 
has 'evid_column' => ( is => 'rw', isa => 'Int' ); 

sub BUILD
{
    my $self = shift;

	my $model = $self->models()->[0]; 
    my $evid_column = $model->problems->[0]->find_data_column(column_name => 'EVID'); 
    if ($evid_column == -1) {
        die "Error: There is no EVID column in the dataset\n";
    }

    $self->evid_column($evid_column);
    $self->model($model);
}

sub modelfit_setup
{
	my $self = shift;

    my $orig_data_file = $self->model->datafiles(absolute_path => 1, problem_numbers => [1])->[0];
    my $ignoresign = defined $self->model->ignoresigns ? $self->model->ignoresigns->[0] : '@';

    my $n = 1;
    my $continue;


    $self->model->set_option(record_name => 'estimation', option_name => 'MAXEVALS', fuzzy_match => 1, option_value => '0');
	$self->tools([]) unless defined $self->tools;
    my @models_to_run;

    while (1) {
        my $data = data->new(
            filename => $orig_data_file,
            ignoresign => $ignoresign,
            missing_data_token => $self->missing_data_token,
            idcolumn => $self->model->idcolumn(),
        );

        $continue = $self->set_evid(dataset => $data, numzeros => $n, evid_column => $self->evid_column);
        last if not $continue;

        my $data_filename = "m1/proseval_$n.dta";
        my $model_filename = $self->directory . "m1/proseval_$n.mod";
        $data->_write(filename => $data_filename);

        my $modified_model = $self->model->copy(filename => $model_filename, output_same_directory => 1);
        $modified_model->problems->[0]->datas->[0]->set_filename(filename => $data_filename);

        # Remove all tables
        $modified_model->problems->[0]->tables([]);

		$modified_model->add_records(type => 'table',
            record_strings => ['ID', 'TIME', 'EVID', 'WRES', 'IWRES', 'NOPRINT', 'NOAPPEND', 'ONEHEADER', "FILE=proseval_$n.tab"]);

        $modified_model->_write(filename => $model_filename, relative_data_path => 1);
        push @models_to_run, $modified_model;
        $n++;
    }

	my $modelfit = tool::modelfit->new(
		%{common_options::restore_options(@common_options::tool_options)},
		models => \@models_to_run, 
		base_dir => $self->directory . 'm1/',
		directory => undef,
		top_tool => 0,
        copy_data => 0,
	);

	push(@{$self->tools}, $modelfit);
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
	);
	my $dataset = $parm{'dataset'};
	my $numzeros = $parm{'numzeros'};
	my $evid_column = $parm{'evid_column'};

    my $set_evid_two = 0;       # Did we set any evid to two?

    for my $individual (@{$dataset->individuals}) {
        my $skipped_zeros = 0;
        for my $row (@{$individual->subject_data}) {
            my @a = split /,/, $row;
            if ($a[$evid_column] == 0) {
                if ($skipped_zeros < $numzeros) {
                    $skipped_zeros++;
                } else {
                    $set_evid_two = 1;
                    $a[$evid_column] = 2;
                    $row = join ',', @a;
                }
            }
        }
    }

    return $set_evid_two;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
