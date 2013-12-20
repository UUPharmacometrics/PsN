package tool::pvar;

use Moose;
use MooseX::Params::Validate;

extends 'newtool';

has 'models' => (is => 'rw', isa => 'ArrayRef[model]');

sub BUILD
{
	# Check that each model only has one problem
	foreach my $model (@models) {
		if (@{$model->problems} != 1) {
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

	foreach $model ($self->models) {
		foreach $variant ('epv', 'pv') {
			my $new_model = $model->copy(output_same_directory => 0);
			if ($new_model->has_output) {
				$new_model->update_inits;
			}
			if ($variant eq 'epv') {
				# Set all omegas to 0 FIX
				foreach my $omega (@{$model->problems->[0]->omegas}) {
					foreach my $option (@{$omega->options}) {
						print "QQ", $option->name, "QQ", $option->value, "QQ\n";
					}
				}
			}
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
