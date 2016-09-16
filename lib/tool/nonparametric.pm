package tool::nonparametric;

use strict;
use Moose;
use MooseX::Params::Validate;
use File::Copy 'cp';
use include_modules;
use data;
use log;
use filter_data;
use tool::modelfit;

extends 'tool';

has 'npsupp' => ( is => 'rw', isa => 'ArrayRef');

sub BUILD
{
    my $self = shift;

	
}

sub modelfit_setup
{
	my $self = shift;
	my $modelfit;
	my $input_model = $self->models->[0];

my $filestem = $input_model ->filename();
#this regex must be the same as used in modelfit.pm, for consistency
$filestem =~ s/\.[^.]+$//; #last dot and extension

$input_model -> set_records(type => 'nonparametric',
							record_strings => ['UNCONDITIONAL']);
														
my @models_array=();
foreach my $value (@{$self->npsupp}){
	push(@models_array,$input_model -> copy( filename => $filestem.'_'.$value.'.mod',
											   copy_datafile => 0,
											   copy_output => 0,
											   write_copy => 0,
											   output_same_directory => 1,
											   directory => $self->directory.'/m1'));
	
	$models_array[-1] -> add_option(record_name => 'nonparametric',
									option_name => 'NPSUPP',
									option_value => $value);
	$models_array[-1] -> _write;
}

#basedirect $main_directory
$modelfit = 
	tool::modelfit->new( eval( $common_options::parameters ),
	  prepend_model_file_name => 1,
	  directory => undef,
	  min_retries => 0,
	  retries => 0,
	  copy_data => 0,
	  top_tool => 0,
	  models => \@models_array );
    
    
	$self->tools([]) unless (defined $self->tools);
	push(@{$self->tools}, $modelfit);
}

sub modelfit_analyze
{
    # Collect all tables into one results table

    my $self = shift;

    


}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
