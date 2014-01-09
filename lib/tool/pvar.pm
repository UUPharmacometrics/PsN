package tool::pvar;

use Math::Random;
use Moose;
use MooseX::Params::Validate;

use tool::modelfit;

extends 'newtool';

has 'parameters' => (is => 'rw', isa => 'ArrayRef[Str]');
has 'samples' => (is => 'rw', isa => 'Int');


sub BUILD
{
	my $self = shift;

	foreach my $model (@{$self->models}) {
		if (@{$model->problems} > 1) {
			croak("More than one problem per model is currently not supported");
		}
	}
}


sub modelfit_setup
{
	my $self = shift;

	my @modified_models;

	for (my $i = 0; $i < scalar(@{$self->models}); $i++) {
		my $model = $self->models->[$i];
		foreach my $variant ('epv', 'pv') {
			my $new_model = $model->copy(output_same_directory => 0);

			if ($new_model->is_run) {
				$new_model->update_inits(from_model => $model);
			}

			if ($variant eq 'epv') {
				$new_model->set_all_omegas_to_zero();
			}

			$new_model->remove_records(type => 'estimation');
			$new_model->remove_records(type => 'covariance');
			$new_model->remove_records(type => 'nonparametric');

			my $simulation;
			if (defined $new_model->problems->[0]->simulations) {
				$simulation = $new_model->problem->[0]->simulations->[0];
			}
			if (not defined $simulation) {
				my $samples = $self->samples;
				$simulation = $new_model->problems->[0]->add_records(type => 'simulation', record_strings => [ "SUBPROBLEMS=$samples", "ONLYSIM" ]);
			}

			$simulation->seed1(random_uniform_integer(1, 1, 2**30));

			$new_model->add_records(type => 'table', record_strings => [@{$self->parameters}, 'NOPRINT','NOAPPEND','FIRSTONLY', 'ONEHEADER', "FILE=$variant$i.tab"]);

			$new_model->_write(filename => "$variant$i.mod");

			push(@modified_models, $new_model);
		}
	}

	my $modelfit = tool::modelfit->new(
		%{common_options::restore_options(@common_options::tool_options)},
		models => \@modified_models, 
		base_dir => $self->directory,
		directory => undef,
		top_tool => 0,
	);

	$self->tools([]) unless defined $self->tools;
	push(@{$self->tools}, $modelfit);
}


sub modelfit_analyze
{


}


sub get_models_from_scm_directory
{
	my $this = shift;
	my $directory = shift;

	my @model_files;

	return @model_files;
}

no Moose;
__PACKAGE__->meta->make_immutable;
