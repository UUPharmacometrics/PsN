package tool::cdd::jackknife;

use Moose;
use MooseX::Params::Validate;

extends 'tool::cdd';

has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['jackknife.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'jackknife_results.csv' );

sub BUILD
{
	my $self = shift;

	my @mo_bins = ();
	foreach my $model (@{$self->models}) {
	  my @pr_bins = ();
	  foreach my $data (@{$model->datas}) {
	    push(@pr_bins, $data->count_ind);
	  }
	  push(@mo_bins, \@pr_bins);
	}
	$self->bins(\@mo_bins);
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
