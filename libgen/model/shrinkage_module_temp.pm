use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::shrinkage_module;

use Carp;
use Data::Dumper;

use debug;


sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'SCALAR', 'nomegas' => 'SCALAR',
			'problem_number' => 'SCALAR', 'directory' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::shrinkage_module->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::shrinkage_module->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::shrinkage_module->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::shrinkage_module->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::shrinkage_module->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'enabled'} = defined $parm{'enabled'} ? $parm{'enabled'} : 0 unless defined $this -> {'enabled'};
	$this -> {'nomegas'} = defined $parm{'nomegas'} ? $parm{'nomegas'} : 0 unless defined $this -> {'nomegas'};

	bless $this, $class;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub enabled {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'enabled'} = $parm;
	} else {
		return $self -> {'enabled'};
	}
}

sub nomegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nomegas'} = $parm;
	} else {
		return $self -> {'nomegas'};
	}
}

sub problem_number {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'problem_number'} = $parm;
	} else {
		return $self -> {'problem_number'};
	}
}

sub directory {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'directory'} = $parm;
	} else {
		return $self -> {'directory'};
	}
}

sub eta_tablename {
	my $self = shift;
	my $filename;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> eta_tablename');
# line 62 "lib/model/shrinkage_module_subs.pm" 

my $probnum = $self -> problem_number;

$filename = 'prob'.'_'.$probnum.'.psn_etas';

# line 132 libgen/model/shrinkage_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> eta_tablename');
	# End of Non-Dia code #

	return $filename;
}

sub wres_tablename {
	my $self = shift;
	my $filename;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> wres_tablename');
# line 74 "lib/model/shrinkage_module_subs.pm" 

my $probnum = $self->problem_number;

$filename = 'prob'.'_'.$probnum.'.psn_wres';

# line 151 libgen/model/shrinkage_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> wres_tablename');
	# End of Non-Dia code #

	return $filename;
}

sub format_shrinkage_tables {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> format_shrinkage_tables');
# line 35 "lib/model/shrinkage_module_subs.pm" 

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

# line 185 libgen/model/shrinkage_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> format_shrinkage_tables');
	# End of Non-Dia code #

	return \@formatted;
}

sub enable {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> enable');
# line 15 "lib/model/shrinkage_module_subs.pm" 

$self->enabled(1);

# line 201 libgen/model/shrinkage_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> enable');
	# End of Non-Dia code #

}

sub disable {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> disable');
# line 25 "lib/model/shrinkage_module_subs.pm" 

$self->enabled(0);

# line 216 libgen/model/shrinkage_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> disable');
	# End of Non-Dia code #

}

sub eta_shrinkage {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'probnum' => 'SCALAR', 'directory' => 'SCALAR',
			'eta_filename' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::shrinkage_module->eta_shrinkage: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::shrinkage_module->eta_shrinkage: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::shrinkage_module->eta_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::shrinkage_module->eta_shrinkage: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::shrinkage_module->eta_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @eta_shrinkage;
	my $model = $parm{'model'};
	my $probnum = $parm{'probnum'};
	my $directory = $parm{'directory'};
	my $eta_filename = $parm{'eta_filename'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> eta_shrinkage');
# line 86 "lib/model/shrinkage_module_subs.pm" 

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
	my $sh_table = data -> new( directory            => $model -> directory,
				    filename             => $eta_filename,
				    ignore_missing_files => 1,
				    target               => 'mem',
				    table_file           => 1 );
	my $next_diag_idx=1;
	for( my $j = 0; $j < scalar @{$omegas -> [$probnum-1][0]}; $j++ ) {
	  # next unless diagonal
	  $omega_indexes[$j] =~  /OMEGA\((\d+),(\d+)\)/ ;
	  croak("unrecognized OMEGA index ".$omega_indexes[$j]) 
	      unless (defined $1 and defined $2);
	  next unless ($1 == $2);
	  my $diag_omega_idx = $1-1; #want index to start at 0
	  while ($next_diag_idx < $diag_omega_idx){
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

# line 337 libgen/model/shrinkage_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> eta_shrinkage');
	# End of Non-Dia code #

	return \@eta_shrinkage;
}

sub iwres_shrinkage {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'probnum' => 'SCALAR', 'directory' => 'SCALAR',
			'iwres_filename' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::shrinkage_module->iwres_shrinkage: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::shrinkage_module->iwres_shrinkage: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::shrinkage_module->iwres_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::shrinkage_module->iwres_shrinkage: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::shrinkage_module->iwres_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @iwres_shrinkage;
	my $model = $parm{'model'};
	my $probnum = $parm{'probnum'};
	my $directory = $parm{'directory'};
	my $iwres_filename = $parm{'iwres_filename'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> iwres_shrinkage');
# line 171 "lib/model/shrinkage_module_subs.pm" 

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
	$sh_table = data -> new( directory            => $model -> directory,
				 filename             => $iwres_filename,
				 ignore_missing_files => 1,
				 target               => 'mem',
				 table_file           => 1 );
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

# line 424 libgen/model/shrinkage_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> iwres_shrinkage');
	# End of Non-Dia code #

	return \@iwres_shrinkage;
}

1;

