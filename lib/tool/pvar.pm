package tool::pvar;

use Moose;
use MooseX::Params::Validate;

extends 'newtool';

has 'parameters' => (is => 'rw', isa => 'ArrayRef[Str]');

sub BUILD
{
	foreach my $model (@models) {
		if (@{$model->problems} > 1) {
			croak("More than one problem per model is currently not supported");
		}
	}
}

=item modelfit_setup

modelfit_setup - Create the models for modelfit 

=cut
sub modelfit_setup
{
	my $self = shift;

	for (my $i = 0; $i < scalar($self->models); $i++) {
		my $model = $self->models->[$i];
		foreach $variant ('epv', 'pv') {
			my $new_model = $model->copy(output_same_directory => 0);

			if ($new_model->is_run) {
				$new_model->update_inits(from_model => $model);
			}

			if ($variant eq 'epv') {
				$new_model->set_all_omegas_to_zero();
			}

			$new_model->remove_records(type => 'estimation');

			my $simulation;
			if (defined $new_model->problems->[0]->simulations) {
				$simulation = $new_model->problem->[0]->simulations->[0];
			}
			if (not defined $simulation) {
				;
			}

			$new_model->add_records(type => 'table', record_strings => [@{$self->parameters}, 'NOPRINT','NOAPPEND','FIRSTONLY', 'ONEHEADER', "FILE=$variant$i.tab"]);
		}
	}
}


sub modelfit_analyze
{


}

=item get_models_from_scm_directory

get_models_from_scm_directory - 

@model_file_names = get_models_from_scm_directory($directory_name);

Description here

=cut
sub get_models_from_scm_directory
{
	my $this = shift;
	my $directory = shift;

	my @model_files;

	return @model_files;
}

no Moose;
__PACKAGE__->meta->make_immutable;
