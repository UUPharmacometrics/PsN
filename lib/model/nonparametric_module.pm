package model::nonparametric_module;

use Moose;
use MooseX::Params::Validate;

has 'enabled' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'model' => ( is => 'rw', isa => 'model' );
has 'problem' => ( is => 'rw', isa => 'model::problem' );
has 'temp_problem_number' => ( is => 'rw', isa => 'Int' );

sub etas_tablename
{
	my $self = shift;
	my $filename;

	my $modname = $self->model->filename;
	my $probnum = $self->problem_number;

	$filename = $modname . '_' . $probnum . '.psn_nonp_etas';

	return $filename;
}

sub marginals_tablename
{
	my $self = shift;
	my $filename;

	my $modname = $self->model->filename;
	my $probnum = $self->problem_number;

	$filename = $modname.'_'.$probnum.'.psn_marginals';

	return $filename;
}

sub format_etas_table
{
	my $self = shift;

	my @formatted = @{$self -> format_table( type => 'etas' )} ;

	return \@formatted;
}

sub format_marginals_table
{
	my $self = shift;

	my @formatted = @{$self -> format_table( type => 'marginals' )} ;

	return \@formatted;
}

sub format_table
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 type => { isa => 'Str', optional => 1 }
	);
	my $type = $parm{'type'};

	my $nomegas = $self->problem->nomegas;

	my $table_str = 'ID';
	for( my $j = 1; $j <= $nomegas; $j++ ) {
		$table_str = $table_str.' ETA'.$j;
	}
	my $accessor = $type.'_tablename';
	$table_str = $table_str.' FILE='.$self -> $accessor.
		' NOAPPEND ONEHEADER NOPRINT FIRSTONLY';

	my $table = model::problem::table -> new ( record_arr => [$table_str] );

	my @formatted = @{$table -> _format_record};

	return \@formatted;
}

sub problem_number
{
	my $self = shift;
	my $problem_number;


	if ( defined $self->model and defined $self->model->problems ) {
		# This is the default
		my @modprobs = @{$self->model->problems};
		for( my $i = 0; $i <= $#modprobs; $i++ ) {
			$problem_number = ($i+1) if ( $modprobs[$i] eq $self->problem );
		}
	} else {
		# This happens when the problems are not yet set in the model
		$problem_number = $self->temp_problem_number;
	}

	return $problem_number;
}

sub enable
{
	my $self = shift;

	$self->enabled(1);
}

sub disable
{
	my $self = shift;

	$self->enabled(0);
}

sub etas_table_exists
{
	my $self = shift;
	my $exists = 0;

	my $directory = $self->model->directory;
	my $filename = $self->etas_tablename;

	$exists = -e $directory.$filename ? 1 : 0;

	return $exists;
}

sub marginals_table_exists
{
	my $self = shift;
	my $exists = 0;

	my $directory = $self->model->directory;
	my $filename = $self->marginals_tablename;

	$exists = -e $directory.$filename ? 1 : 0;

	return $exists;
}

sub format_nonparametric
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 type => { isa => 'Str', optional => 1 }
	);
	my $type = $parm{'type'};
	my @formatted;

	my $accessor = $type.'_msfoname';
	my $nonp_str = uc($type).' UNCONDITIONAL MSFO='.$self -> $accessor.
		' NOAPPEND ONEHEADER NOPRINT FIRSTONLY';

	my $nonp = model::problem::nonparametric -> new ( record_arr => [$nonp_str] );

	@formatted = @{$nonp -> _format_record};

	return \@formatted;
}

sub format_etas_nonparametric
{
	my $self = shift;
	my @formatted;

	@formatted = @{$self -> format_nonparametric( type => 'etas' )};

	return \@formatted;
}

sub format_marginals_nonparametric
{
	my $self = shift;
	my @formatted;

	@formatted = @{$self -> format_nonparametric( type => 'marginals' )};

	return \@formatted;
}

sub format_etas_msfi
{
	my $self = shift;

	my $msfi_str = $self -> marginals_msfoname;
	my $msfi = model::problem::msfi -> new ( record_arr => [$msfi_str] );
	my @formatted = @{$msfi -> _format_record};

	return \@formatted;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
