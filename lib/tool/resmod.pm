package tool::resmod;

use strict;
use Moose;
use MooseX::Params::Validate;
use List::Util qw(max);
use include_modules;
use log;
use model;
use model::problem;
use tool::modelfit;

extends 'tool';

has 'model' => ( is => 'rw', isa => 'model' );
has 'idv' => ( is => 'rw', isa => 'Str', default => 'TIME' );

# This array of hashes represent the different models to be tested. The 0th is the base model
our @residual_models =
(
	{
		name => 'base',
		pred => 'Y = THETA(1) + ETA(1) + ERR(1)',
	}, {
		name => 'omega_on_epsilon',
		pred => 'Y = THETA(1) + ETA(1) + ERR(1) * EXP(ETA(2))',
		extra_omegas => '$OMEGA 0.01',
	},
);


sub BUILD
{
    my $self = shift;

	my $model = $self->models()->[0]; 
    $self->model($model);
}

sub modelfit_setup
{
	my $self = shift;

	# Find a table with ID, TIME and CWRES
    my $cwres_table = $self->model->problems->[0]->find_table(columns => [ 'ID', $self->idv, 'CWRES' ], get_object => 1);
    my $cwres_table_name = $self->model->problems->[0]->find_table(columns => [ 'ID', $self->idv, 'CWRES' ]);
	if (not defined $cwres_table) {
		die "Error original model has no table containing ID, IDV and CWRES\n";
	}

	# Create $INPUT
	my $input_columns;
	my $found_id;
	my $found_idv;
	my $found_cwres;
	for my $option (@{$cwres_table->options}) {
		if ($option->name eq 'ID') {
			$found_id = 1;
			$input_columns .= $option->name;
		} elsif ($option->name eq $self->idv) {
			$found_idv = 1;
			$input_columns .= $option->name;
		} elsif ($option->name eq 'DV') {
			$found_cwres = 1;
			$input_columns .= $option->name;
		} else {
			$input_columns .= 'DROP';
		}
		last if ($found_id and $found_idv and $found_cwres);
		$input_columns .= ' ';
	}

	my @models_to_run;
	for my $model_properties (@residual_models) {
		my $sh_mod = model::shrinkage_module->new(
			nomegas => 1,
			directory => 'm1/',
			problem_number => 1);
		my @prob_arr = (
			'$PROBLEM CWRES base model',
			"\$INPUT $input_columns",
			"\$DATA ../$cwres_table_name IGNORE=@",
			'$PRED',
			$model_properties->{'pred'},
			'$THETA  .1',
			'$OMEGA  0.01',
		);
		if (exists $model_properties->{'extra_omegas'}) {
			push @prob_arr, $model_properties->{'extra_omegas'};
		}
		push @prob_arr, (
			'$SIGMA 1',
			'$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
		);
		my $cwres_problem = model::problem->new(
			prob_arr => \@prob_arr,
			shrinkage_module => $sh_mod,
		);
		my $cwres_model = model->new(directory => 'm1/', filename => $model_properties->{'name'} . "_cwres.mod", problems => [ $cwres_problem ]);
		$cwres_model->_write();
		push @models_to_run, $cwres_model;
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
    my $self = shift;

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
