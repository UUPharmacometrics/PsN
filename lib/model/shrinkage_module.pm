package model::shrinkage_module;

#use Carp;
use include_modules;
use Moose;
use MooseX::Params::Validate;

has 'enabled' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nomegas' => ( is => 'rw', isa => 'Maybe[Num]', default => 0 );
has 'problem_number' => ( is => 'rw', isa => 'Num' );
has 'directory' => ( is => 'rw', isa => 'Str' );

sub eta_tablename
{
	my $self = shift;
	my $filename;

	my $probnum = $self->problem_number;
	$filename = 'prob'.'_'.$probnum.'.psn_etas';

	return $filename;
}

sub wres_tablename
{
	my $self = shift;
	my $filename;

	my $probnum = $self->problem_number;
	$filename = 'prob'.'_'.$probnum.'.psn_wres';

	return $filename;
}

sub format_shrinkage_tables
{
	my $self = shift;
	my @formatted;

	my $omegas = $self->nomegas;

	my $eta_str = 'ID';
	my $eps_str = 'ID IWRES EVID';

	for( my $j = 1; $j <= $omegas; $j++ ) {
		$eta_str = $eta_str.' ETA'.$j;
	}
	$eta_str = $eta_str.' FILE='.$self -> eta_tablename.
		' NOAPPEND ONEHEADER NOPRINT FIRSTONLY';
	$eps_str = $eps_str.' FILE='.$self -> wres_tablename.
		' NOAPPEND ONEHEADER NOPRINT'."\n";

	my $eta_table = model::problem::table -> new ( record_arr => [$eta_str] );
	my $eps_table = model::problem::table -> new ( record_arr => [$eps_str] );

	@formatted = ( @{$eta_table -> _format_record}, @{$eps_table -> _format_record} );

	return \@formatted;
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

sub eta_shrinkage
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { ' model', optional => 1 },
		 probnum => { isa => 'Num', optional => 1 },
		 directory => { isa => 'Str', optional => 1 },
		 eta_filename => { isa => 'Maybe[Str]', optional => 1 }
	);
	my @eta_shrinkage;
	my $model = $parm{'model'};
	my $probnum = $parm{'probnum'};
	my $directory = $parm{'directory'};
	my $eta_filename = $parm{'eta_filename'};

	# We do not handle subproblem eta shrinkage
    
	unless (defined($eta_filename)) {
		$eta_filename = $self -> eta_tablename;
	} else {
		1;
	}

	if ( $self->enabled ) {
		my $omegas  = $model -> outputs -> [0] -> omegas(); #may be many zeros here, off-diagonal
		my $omeganames  = $model -> outputs -> [0] -> omeganames();

		my @omega_indexes;
		if( defined $omeganames and 
				defined $omeganames->[0] and
				defined $omeganames->[0]->[0] ) {
			@omega_indexes = @{$omeganames->[0]->[0]};
		}
		my $defined_indexes = 0;
		foreach my $index ( @omega_indexes ) {
			$defined_indexes++ if defined $index;
		}

		if( defined $omegas and defined $omegas -> [$probnum-1] ) {
			if( scalar @{$omegas -> [$probnum-1]} == 1 ) { # One subprob
				if( $defined_indexes and (-e $directory.$eta_filename) ) {
					my $sh_table = data->new(
						directory            => $model->directory,
						filename             => $eta_filename,
						ignoresign					 => '@',
						ignore_missing_files => 1,
						idcolumn         => 1); #table with ID first
					my $next_diag_idx=1;
					for( my $j = 0; $j < scalar @{$omegas -> [$probnum-1][0]}; $j++ ) {
	  				# next unless diagonal
						$omega_indexes[$j] =~  /OMEGA\((\d+),(\d+)\)/ ;
						croak("unrecognized OMEGA index ".$omega_indexes[$j]) 
							unless (defined $1 and defined $2);
						next unless ($1 == $2);
						my $diag_omega_idx = $1-1; #want index to start at 0
							while ($next_diag_idx < $diag_omega_idx) {
								#must set undefs for omegas that are zero and not stored
								#(this might be unecessary in PsN-3.2.4 and up where zeros are stored)
								#since names are sorted we can set undef for all up to current diag_idx
								$eta_shrinkage[0][$next_diag_idx] = undef;
								$next_diag_idx++;
							}
						if ( defined $omegas -> [$probnum-1][0][$j] and defined $sh_table and
								$omegas -> [$probnum-1][0][$j] != 0 ) {
							my $omega = sqrt(abs($omegas -> [$probnum-1][0][$j]));
							#skip all values that are not !=0
							my $eta_sd = $sh_table -> sd( column => ($diag_omega_idx+2),
									subset_column => ($diag_omega_idx+2),
									subset_syntax => '!=0',
									global_sd     => 1 );
							my $shrinkage = ($omega - $eta_sd)/$omega;
							$eta_shrinkage[0][$diag_omega_idx] = 100*$shrinkage; #report percent
						} else {
							$eta_shrinkage[0][$diag_omega_idx] = undef;
						}
						$next_diag_idx++;
					}
				} else {
					$eta_shrinkage[0] = [];
				}
			} elsif( scalar @{$omegas -> [$probnum-1]} == 0 ) {
				my $mes = "\n".$model -> full_name. 
					"\nNo omegas found in output for problem $probnum. PsN cannot compute shrinkage.\n";
				carp($mes);
			} else {
				my $mes = "\n". $model -> full_name . "\nCall to output->omegas indicates that results ".
				"exists in multiple subproblems. PsN cannot compute shrinkage".
				" on the subproblem level\n";
				carp($mes);
			}
		}
	}

	return \@eta_shrinkage;
}

sub iwres_shrinkage
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { ' model', optional => 1 },
		 probnum => { isa => 'Int', optional => 1 },
		 directory => { isa => 'Str', optional => 1 },
		 iwres_filename => { isa => 'Maybe[Str]', optional => 1 }
	);
	my @iwres_shrinkage;
	my $model = $parm{'model'};
	my $probnum = $parm{'probnum'};
	my $directory = $parm{'directory'};
	my $iwres_filename = $parm{'iwres_filename'};

	# We do not handle subproblem iwres shrinkage
	unless (defined($iwres_filename)){
		$iwres_filename = $self -> wres_tablename;
	}

	if ( $self->enabled ) {

		my $ofv  = $model -> outputs -> [0] -> ofv; # Use ofv to test success

		if( defined $ofv ) {
			if( scalar @{$ofv -> [$probnum-1]} == 1 ) {
				my $sh_table;
				if( defined $ofv -> [$probnum-1][0] and (-e $directory.$iwres_filename) ) {
					$sh_table = data->new(
						directory            => $model -> directory,
						filename             => $iwres_filename,
						ignore_missing_files => 1,
						ignoresign					 =>'@',
						idcolumn         => 1); #table with ID first
				}
				if( defined $sh_table ) {
					my $iwres_sd = $sh_table -> sd( column        => 2,
							subset_column => 3,
							subset_syntax => '==0',
							global_sd     => 1 );
					my $shrinkage = 1 - $iwres_sd;
					$iwres_shrinkage[0] = 100*$shrinkage; #report percent
				} else {
					$iwres_shrinkage[0] = undef;
				}			  
			} elsif ( @{$ofv -> [$probnum-1]} < 1 ) {
				carp("There seems to be a problem with the results from ".
						$model -> filename().". Cannot compute shrinkage." );
			} else {
				my $mes =  "\n". $model -> full_name ."\nCall to output->ofv indicates that results ".
					"exists in multiple subproblems.PsN can not yet compute iwres_shrinkage".
					" on the subproblem level" ;
				carp($mes);
			}
		}
	}

	return \@iwres_shrinkage;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
