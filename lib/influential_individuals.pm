package influential_individuals;

use include_modules;
use Cwd;
use OSspecific;
use PsN;
use ui;
use model;
use MooseX::Params::Validate;
use array qw(max min mean variance);
use linear_algebra;
use Config;
use table;


sub get_eta_count
{
	my %parm = validated_hash(\@_,
							  header => {isa => 'ArrayRef', optional => 0},
		);
	my $header = $parm{'header'};

	my $count;
	#TODO

	return $count;
}

sub get_eta_means
{
	my %parm = validated_hash(\@_,
							  table => {isa => 'table', optional => 0},
							  eta_count => {isa => 'PositiveInt', optional => 0},
		);
	my $table = $parm{'table'};
	my $eta_count = $parm{'eta_count'};

	my @means=();
	#TODO
	# loop over 1 to eta_count
	#use $table->get_column using column name created from loop index (ETA(1), ETA(2) etc), examples in table.t
	#use array::mean , see array.t

	return \@means;
}

sub get_eta_covmatrix
{
	my %parm = validated_hash(\@_,
							  table => {isa => 'table', optional => 0},
							  eta_count => {isa => 'PositiveInt', optional => 0},
		);
	my $table = $parm{'table'};
	my $eta_count = $parm{'eta_count'};

	my $covmatrix=[];
	#TODO
	#compute covariance matrix of all eta columns. 
	# loop over 1 to eta_count
	#use $table->get_column using column name created from loop index (ETA(1), ETA(2) etc), examples in table.t
	# store all columns in one big ETA matrix , this is the input matrix for 
	# computing covariance matrix (subroutine in linear_algebra.pm)
	#See example in tool/frem.pm , part of sub get_correlation_matrix_from_phi
	
	return $covmatrix;
}

sub get_pred_code
{
	my %parm = validated_hash(\@_,
							  eta_count => {isa => 'PositiveInt', optional => 0},
		);
	my $eta_count = $parm{'eta_count'};

	my @code=();
	#TODO

	
	return \@code;
}

sub get_theta_code
{
	my %parm = validated_hash(\@_,
							  eta_means => {isa => 'ArrayRef', optional => 0},
		);
	my $eta_means = $parm{'eta_means'};

	my @code=();
	#TODO

	
	return \@code;
}
	


1;
