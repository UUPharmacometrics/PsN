package tool::pvar;

use include_modules;
use Math::Random;
use File::Spec;
use Moose;
use MooseX::Params::Validate;

use array qw(:all);
use tool::modelfit;

extends 'tool';

has 'parameters' => (is => 'rw', isa => 'ArrayRef[Str]');
has 'samples' => (is => 'rw', isa => 'Int');
has 'pvar_models' => (is => 'rw', isa => 'ArrayRef[model]');

sub BUILD
{
	my $self = shift;

	foreach my $model (@{$self->pvar_models}) {
		if (@{$model->problems} > 1) {
			croak("More than one problem per model is currently not supported");
		}
	}
}

sub modelfit_setup
{
	my $self = shift;

	my @modified_models;

	for (my $i = 0; $i < scalar(@{$self->pvar_models}); $i++) {
		my $model = $self->pvar_models->[$i];

		foreach my $variant ('epv', 'pv') {
			my $new_model = $model->copy(
				output_same_directory => 1,
				filename => File::Spec->catfile(($self->directory, "m1"), "$variant$i.mod"),
				copy_datafile => 0,
				write_copy => 0,
				copy_output => 0
			);

			if ($model->is_run) {
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
				$simulation = $new_model->problems->[0]->simulations->[0];
			}
			if (not defined $simulation) {
				my $samples;
				if ($variant eq 'pv') {
					$samples = $self->samples;
				} else {
					$samples = 1;
				}
				$simulation = $new_model->problems->[0]->add_records(type => 'simulation', record_strings => [ "SUBPROBLEMS=$samples", "ONLYSIM" ]);
			}

			$simulation->seed1(random_uniform_integer(1, 1, 2**30));

			$new_model->add_records(type => 'table', record_strings => [@{$self->parameters}, 'NOPRINT','NOAPPEND','FIRSTONLY', 'ONEHEADER', "FILE=$variant$i.tab"]);

			$new_model->_write;

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
	my $self = shift;

	my $epv_array;
	my $pv_array;
	my @upv_array;

	open my $output_file, '>', "result.csv";

	print $output_file "Type,Model number,Model name,", join(',', @{$self->parameters}), "\n";; 

	for my $model_number (0 .. scalar(@{$self->pvar_models} - 1)) {
		$epv_array = $self->_get_epv($model_number);
		$pv_array = $self->_get_pv($model_number);
		foreach my $i (0 .. scalar(@$epv_array) - 1) {
			$upv_array[$i] = $pv_array->[$i] - $epv_array->[$i];
		}
		my $model_name = $self->pvar_models->[$model_number]->filename;
		print $output_file "EPV,", "$model_number,$model_name,", join(',', @$epv_array), "\n";
		print $output_file "UPV,", "$model_number,$model_name,", join(',', @upv_array), "\n";
		print $output_file "PV,", "$model_number,$model_name,", join(',', @$pv_array), "\n";
	}

	close $output_file;
}

sub _get_pv
{
	my $self = shift;
	my $model_number = shift;

	open my $table_file, '<', "m1/pv$model_number.tab";
	my $matrix = $self->_get_data($table_file, $model_number, $self->samples);
	close $table_file;

	my @pv_array;

	foreach my $i (0 .. scalar(@{$self->parameters}) - 1) {
		$pv_array[$i] = variance($matrix->[$i]);
	}

	return \@pv_array;
}

sub _get_epv
{
	my $self = shift;
	my $model_number = shift;

	open my $table_file, '<', "m1/epv$model_number.tab";
	my $matrix = $self->_get_data($table_file, $model_number, 1);
	close $table_file;

	my @epv_array;

	foreach my $i (0 .. scalar(@{$self->parameters}) - 1) {
		$epv_array[$i] = variance($matrix->[$i]);
	}

	return \@epv_array;
}

sub _get_data
{
	my $self = shift;
	my $file = shift;
	my $model_number = shift;
	my $number_of_samples = shift;

	# Find number of indivudals
	my $number_of_individuals = 0;
	<$file>;
	<$file>;
	my $row;
	while (defined($row = <$file>) && $row !~ /^TABLE/) {
		$number_of_individuals++;
	}
	seek $file, 0, 0;

	my @matrix;

	foreach my $sample (1 .. $number_of_samples) {
		<$file>;
		<$file>;

		foreach my $i (1 .. $number_of_individuals) {
			my $line = <$file>;
			my @row_array = split(/\s+/, $line);
			shift @row_array;
			foreach my $col (0 .. $#row_array) {
				push @{$matrix[$col]}, $row_array[$col];
			}
		}
	}

	return \@matrix;
}

sub _read_next_path
{
	my $logfile = shift;

	while (<$logfile>) {
		if (/^Model directory (.+)/) {
			return $1;
		}
	}

	return undef;
}

sub _read_next_model
{
	my $logfile = shift;

	while (<$logfile>) {
		if (/Parameter-covariate relation chosen in this \w+ step: (.+)/) {
			my $model = $1;
			$model =~ tr/\-//d;
			if ($model eq '') {
				return undef;
			} else {
				return $model;
			}
		}
	}

	return undef;
}

sub get_models_from_scm_directory
{
	my $self = shift;
	my $logfile_name = shift;

	my @model_files;

	# First add the base model
	my $base_model_name = 'base_model_with_included_relations.mod';
	(my $volume, my $directories, my $filename) = File::Spec->splitpath($logfile_name);
	my $base_model_full_path = "$volume$directories$base_model_name";
	if (-e $base_model_full_path) {
		push @model_files, "$base_model_full_path";
	}

	open my $logfile, '<', $logfile_name;

	my $path = _read_next_path($logfile);
	my $model_name;
	while (defined $path) {
		$model_name = _read_next_model($logfile);	
		next if not defined $model_name;
		$model_name = File::Spec->catpath(undef, $path, "$model_name.mod");
		push @model_files, $model_name;
	} continue {
		$path = _read_next_path($logfile);
	}

	close $logfile;

	return @model_files;
}

sub set_data_files
{
	my $self = shift;
	my $base_model = shift;
	my @models = @_;

	#datafiles returns an arrayref even if file for single $PROB is requested
	my $ref_data_filename_with_path = $base_model->datafiles(problem_numbers => [1], absolute_path => 1);

	for my $model (@models) {
		#new_names must be an arrayref
		$model->datafiles(problem_numbers => [1],
						  new_names => $ref_data_filename_with_path);
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
