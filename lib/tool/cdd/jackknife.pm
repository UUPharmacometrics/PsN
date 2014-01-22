package tool::cdd::jackknife;

use Moose;
use MooseX::Params::Validate;

extends 'tool::cdd';

has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['jackknife.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'jackknife_results.csv' );

sub BUILD
{
	my $this  = shift;
	my %parm  = @_;

	my @mo_bins = ();
	foreach my $model ( @{$this->models} ) {
	  my @pr_bins = ();
	  foreach my $data ( @{$model->datas}  ) {
	    push( @pr_bins, $data -> count_ind );
	  }
	  push( @mo_bins, \@pr_bins );
	}
	$this->bins(\@mo_bins);
}

sub model_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
