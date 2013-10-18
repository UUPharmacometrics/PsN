use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package output;
# A Perl module for parsing NONMEM output files
use Carp;
use Digest::MD5 'md5_hex';
use OSspecific;
use Storable;
use Config;
use ext::Math::SigFigs;
use Data::Dumper;
use Time::Local;
use model::problem;
use array qw(:all);
use debug;


#---------------------------------------------------------------------
#         Used Packages
#---------------------------------------------------------------------
use output::problem;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'directory' => 'SCALAR',
			'control_stream_problems' => 'REF', 'filename_root' => 'SCALAR',
			'filename' => 'SCALAR', 'nonmem_version' => 'SCALAR',
			'output_id' => 'SCALAR', 'model_id' => 'SCALAR',
			'ignore_missing_files' => 'SCALAR', 'tablenames' => 'ARRAY',
			'target' => 'SCALAR', 'abort_on_fail' => 'SCALAR',
			'parsed_successfully' => 'SCALAR', 'msfo_has_terminated' => 'SCALAR',
			'runtime' => 'SCALAR', 'lst_interrupted' => 'SCALAR',
			'parsed' => 'SCALAR', 'parsing_error_message' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'ignore_missing_files'} = defined $parm{'ignore_missing_files'} ? $parm{'ignore_missing_files'} : 0 unless defined $this -> {'ignore_missing_files'};
	$this -> {'target'} = defined $parm{'target'} ? $parm{'target'} : 'mem' unless defined $this -> {'target'};
	$this -> {'abort_on_fail'} = defined $parm{'abort_on_fail'} ? $parm{'abort_on_fail'} : 1 unless defined $this -> {'abort_on_fail'};
	$this -> {'parsed_successfully'} = defined $parm{'parsed_successfully'} ? $parm{'parsed_successfully'} : 1 unless defined $this -> {'parsed_successfully'};
	$this -> {'msfo_has_terminated'} = defined $parm{'msfo_has_terminated'} ? $parm{'msfo_has_terminated'} : 0 unless defined $this -> {'msfo_has_terminated'};
	$this -> {'lst_interrupted'} = defined $parm{'lst_interrupted'} ? $parm{'lst_interrupted'} : 0 unless defined $this -> {'lst_interrupted'};
	$this -> {'parsed'} = defined $parm{'parsed'} ? $parm{'parsed'} : 0 unless defined $this -> {'parsed'};

	bless $this, $class;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 69 "lib/output_subs.pm" 
	# Usage:
	# 
	#   $outputObject -> new( filename => 'run1.lst' );
	#
	# The basic usage above creates a output object with the data
	# in file.out parsed into memory.
	#
	#   $outputObject -> new( filename => 'run1.lst',
	#                         target   => 'disk' );
	#
	# If I<target> is set to 'disk', the data in "run1.lst" will
	# be left on disk in an effort to preserve memory. The file
	# will be read if needed.

	carp("Initiating new\tNM::output object from file $parm{'filename'}" );

	if ( defined $this->filename and $this->filename ne '' ) {
		my $name;
		my $directory;
	  ( $directory, $name ) = OSspecific::absolute_path( $this->directory, $this->filename );
		$this->directory($directory);
		$this->filename($name);

	  if ( $name =~ /\.lst$/ ) {
	    $name =~ s/\.lst$//;
	    $this->filename_root($name);
	  } elsif(  $name =~ /\.res$/ ) {
	    $name =~ s/\.res$//;
	    $this->filename_root($name);
	  }

	  if ( -e $this->full_name ) { 
	    if ($this->target eq 'mem') {
	      $this->_read_problems;
			}
	  } else {
	    croak("The NONMEM output file " . $this -> full_name . " does not exist" )
					unless $this->ignore_missing_files;
	  }
	} else {
	  croak("No filename specified or filename equals empty string!" );
	  $this->filename('tempfile');
	}
# line 128 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub problems {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'problems'} = $parm;
	} else {
		return $self -> {'problems'};
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

sub control_stream_problems {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'control_stream_problems'} = $parm;
	} else {
		return $self -> {'control_stream_problems'};
	}
}

sub filename_root {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'filename_root'} = $parm;
	} else {
		return $self -> {'filename_root'};
	}
}

sub filename {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'filename'} = $parm;
	} else {
		return $self -> {'filename'};
	}
}

sub nonmem_version {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonmem_version'} = $parm;
	} else {
		return $self -> {'nonmem_version'};
	}
}

sub output_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'output_id'} = $parm;
	} else {
		return $self -> {'output_id'};
	}
}

sub model_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'model_id'} = $parm;
	} else {
		return $self -> {'model_id'};
	}
}

sub ignore_missing_files {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ignore_missing_files'} = $parm;
	} else {
		return $self -> {'ignore_missing_files'};
	}
}

sub tablenames {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tablenames'} = $parm;
	} else {
		return $self -> {'tablenames'};
	}
}

sub target {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'target'} = $parm;
	} else {
		return $self -> {'target'};
	}
}

sub abort_on_fail {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'abort_on_fail'} = $parm;
	} else {
		return $self -> {'abort_on_fail'};
	}
}

sub parsed_successfully {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parsed_successfully'} = $parm;
	} else {
		return $self -> {'parsed_successfully'};
	}
}

sub msfo_has_terminated {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'msfo_has_terminated'} = $parm;
	} else {
		return $self -> {'msfo_has_terminated'};
	}
}

sub runtime {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'runtime'} = $parm;
	} else {
		return $self -> {'runtime'};
	}
}

sub lst_interrupted {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lst_interrupted'} = $parm;
	} else {
		return $self -> {'lst_interrupted'};
	}
}

sub parsed {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parsed'} = $parm;
	} else {
		return $self -> {'parsed'};
	}
}

sub parsing_error_message {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parsing_error_message'} = $parm;
	} else {
		return $self -> {'parsing_error_message'};
	}
}

sub add_problem {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_problem given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'problems'}},
		output::problem -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub copy {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'filename' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->copy: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->copy: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->copy: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->copy: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->copy: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $filename = $parm{'filename'};
	my $new_output;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> copy');
# line 135 "lib/output_subs.pm" 
	$new_output = Storable::dclone( $self );
# line 439 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> copy');
	# End of Non-Dia code #

	return $new_output;
}

sub access_any {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'attribute' => 'm_SCALAR', 'problems' => 'ARRAY',
			'subproblems' => 'ARRAY', 'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->access_any: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->access_any: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->access_any: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->access_any: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->access_any: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $attribute = $parm{'attribute'};
	my @return_value;
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> access_any');
# line 1003 "lib/output_subs.pm" 
	# You should not really use access_any but instead the
	# specific selector for the information you want, such as
	# L</sigmas>, L</raw_tmatrix> or similar.


	# TODO: Add sanity checking of parameter values (more than
	# the automatic). e.g check that parameter_numbers is a two-
	# dimensional array.

	if ( $self -> have_output ) {
	  unless ( not_empty($self->problems) ) {
	    $self -> _read_problems;
	  }
	} else {
	  croak("Trying to access output object, that have no data on file(" . $self->full_name . ") or in memory" );
	}

	my @own_problems;
	if( defined $self->problems ) {
	  unless( scalar(@problems) > 0 ){
	    carp("Problems undefined, using all" );
	    @problems = (1 .. scalar @{$self->problems});
	  }
	  @own_problems = @{$self->problems};
	} else {
	  return \@return_value; #Return the empty array
	}

	foreach my $i ( @problems ) {
	  if ( defined $own_problems[$i - 1] ) {
	    if (( defined( $own_problems[$i - 1] -> can( $attribute ) ) ) and (not $attribute eq 'estimation_step_run')) {
	      carp("method $attribute defined on the problem level" );
	      my $meth_ret = $own_problems[$i - 1] -> $attribute;
	      if ( ref($meth_ret) eq "HASH" ) {
					push( @return_value, $meth_ret ) if defined $meth_ret;
	      } elsif ( ref ($meth_ret) ) {
					my @prob_attr = @{$meth_ret};
					if ( scalar @parameter_numbers > 0 ) {
		  			my @tmp_arr = ();
		  			foreach my $num ( @parameter_numbers ) {
		    			if ( $num > 0 and $num <= scalar @prob_attr ) {
		      			push( @tmp_arr, $prob_attr[$num - 1] );
		    			} else {
		      			croak("( $attribute ): no such parameter number $num!" . "(" . scalar @prob_attr . " exists)" );
		    			}
		  			}
		  			@prob_attr = @tmp_arr;
					}
					push( @return_value, \@prob_attr );
	      } else {
					push( @return_value, $meth_ret ) if defined $meth_ret;
	      }
	    } else {
	      carp("method $attribute defined on the subproblem level" );
	      my $problem_ret = $own_problems[$i - 1] -> access_any(
									attribute         => $attribute,
									subproblems       => \@subproblems,
									parameter_numbers => \@parameter_numbers );
	      push( @return_value, $problem_ret ) if defined $problem_ret;
	    }
	  } else {
	    croak("No such problem " . ($i - 1) );
	  }
	}
# line 547 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> access_any');
	# End of Non-Dia code #

	return \@return_value;
}

sub high_correlations {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'limit' => 'SCALAR', 'problems' => 'ARRAY',
			'subproblems' => 'ARRAY', 'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->high_correlations: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->high_correlations: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->high_correlations: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->high_correlations: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->high_correlations: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $limit = defined $parm{'limit'} ? $parm{'limit'} : 0.95;
	my @high_correlations;
	my @found_correlations;
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> high_correlations');
# line 1073 "lib/output_subs.pm" 
	my $correlation_matrix = $self -> correlation_matrix( problems    => \@problems,
							      subproblems => \@subproblems );
	my @matrix_headers = @{$self -> output_matrix_headers( problems    => \@problems,
							     subproblems => \@subproblems )};

	my $found_any = 0;
	for ( my $i = 0; $i < scalar @{$correlation_matrix}; $i++ ) {
	  for ( my $j = 0; $j < scalar @{$correlation_matrix -> [$i]}; $j++ ) {
	  	next unless (defined $correlation_matrix -> [$i][$j]);
	    if ((scalar @{$correlation_matrix -> [$i][$j]})>0) {
		  	$found_any = 1;
		  	last;
	    }
	  }
	  last if $found_any;
	}
	return unless $found_any;

	for ( my $i = 0; $i < scalar @{$correlation_matrix}; $i++ ) {
	  my ( @prob_corr, @pf_corr );
	  my @names = {$matrix_headers[$i]};

	  for ( my $j = 0; $j < scalar @{$correlation_matrix -> [$i]}; $j++ ) {
	    my ( @sp_corr, @spf_corr );;
	    my $idx = 0;
	    for ( my $row = 1; $row <= scalar @names; $row++ ) {
				for ( my $col = 1; $col <= $row; $col++ ) {
		    	if ( not ( $row == $col ) and $correlation_matrix -> [$i][$j][$idx] > $limit or $correlation_matrix -> [$i][$j][$idx] < -$limit ) {
		  			push( @sp_corr, $names[$row-1]."-".$names[$col-1] );
		  			push( @spf_corr, $correlation_matrix -> [$i][$j][$idx] );
					}
					$idx++;
	      }
	    }
	    push( @prob_corr, \@sp_corr );
	    push( @pf_corr, \@spf_corr );
	  }
	  push( @high_correlations, \@prob_corr );
	  push( @found_correlations, \@pf_corr );
	}
# line 632 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> high_correlations');
	# End of Non-Dia code #

	return \@high_correlations ,\@found_correlations;
}

sub large_standard_errors {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'theta_cv_limit' => 'SCALAR', 'omega_cv_limit' => 'SCALAR',
			'sigma_cv_limit' => 'SCALAR', 'problems' => 'ARRAY',
			'subproblems' => 'ARRAY', 'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->large_standard_errors: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->large_standard_errors: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->large_standard_errors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->large_standard_errors: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->large_standard_errors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $theta_cv_limit = defined $parm{'theta_cv_limit'} ? $parm{'theta_cv_limit'} : 0.95;
	my $omega_cv_limit = defined $parm{'omega_cv_limit'} ? $parm{'omega_cv_limit'} : 0.95;
	my $sigma_cv_limit = defined $parm{'sigma_cv_limit'} ? $parm{'sigma_cv_limit'} : 0.95;
	my @large_standard_errors_names;
	my @large_standard_errors_values;
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> large_standard_errors');
# line 1118 "lib/output_subs.pm" 
	foreach my $param ( 'theta', 'omega', 'sigma' ) {
	  my @cvs   = eval( '@{$self -> cvse'.$param.'s( problems    => \@problems,'.
			    'subproblems => \@subproblems )}' );
	  my @allnames   = eval( '@{$self -> '.$param.'names( problems    => \@problems,'.
			    'subproblems => \@subproblems )}' );
	  for ( my $i = 0; $i <= $#cvs; $i++ ) {
	    #problem
	    if ( $param eq 'theta' ) {
	      $large_standard_errors_names[$i] = [];
	    }
	    next unless( defined $cvs[$i] );
	    for ( my $j = 0; $j < scalar @{$cvs[$i]}; $j++ ) {
	      #subproblem
	      my (@large_values,@large_names);
	      if ( $param eq 'theta' ) {
					$large_standard_errors_names[$i][$j] = [] ;
	      }
	      next unless( defined $cvs[$i][$j] );
	      unless ( defined $allnames[$i][$j] ) {
					croak("no names matching  cvse:s");
	      }
	      my @values = @{$cvs[$i][$j]};
	      my @names = @{$allnames[$i][$j]};
	      unless ( scalar (@names) == scalar (@values) ) {
					croak("names do not match cvse:s");
	      }
	      for ( my $k = 0; $k < scalar (@values); $k++ ) {
					if ( abs($values[$k]) > eval('$'.$param.'_cv_limit')) {
		  			push (@large_values,$values[$k]);
		  			push (@large_names,$names[$k]);
					}
	      }
	      $large_standard_errors_names[$i][$j] = \@large_names ;
	      $large_standard_errors_values[$i][$j] = \@large_values ;
	    }
	  }
	}
# line 717 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> large_standard_errors');
	# End of Non-Dia code #

	return \@large_standard_errors_names ,\@large_standard_errors_values;
}

sub near_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'zero_limit' => 'SCALAR', 'significant_digits' => 'SCALAR',
			'off_diagonal_sign_digits' => 'SCALAR', 'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->near_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->near_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->near_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->near_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->near_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $zero_limit = defined $parm{'zero_limit'} ? $parm{'zero_limit'} : 0.01;
	my $significant_digits = defined $parm{'significant_digits'} ? $parm{'significant_digits'} : 2;
	my $off_diagonal_sign_digits = defined $parm{'off_diagonal_sign_digits'} ? $parm{'off_diagonal_sign_digits'} : 2;
	my @near_bounds;
	my @found_bounds;
	my @found_estimates;
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> near_bounds');
# line 1161 "lib/output_subs.pm" 
	sub test_sigdig {
	  my ( $number, $goal, $sigdig, $zerolim ) = @_;
	  $number = &FormatSigFigs($number, $sigdig );
	  my $test;
	  if ( $goal == 0 ) {
	    $test = abs($number) < $zerolim ? 1 : 0;
	  } else {
	    $goal = &FormatSigFigs($goal, $sigdig );
	    $test = $number eq $goal ? 1 : 0;
	  }
	  return $test;
	}
	sub cmp_coords {
	  if ($a =~ /THETA/){
	    return substr($a,5) <=> substr($b,5);
	  } else {
	    ($a.$b) =~ /\((\d+)\,(\d+)\)[A-Z]*\((\d+)\,(\d+)\)/;
	    return $2+(($1-1)*$1)/2 <=> $4+(($3-1)*$3)/2; 
	  }
	}

	my @init_problems;
	if ( not_empty($self->problems) ) {
	    foreach my $pr (@{$self->problems}) {
				push(@init_problems,$pr->input_problem());
	    }
	} else {
	    croak("No problems defined in output object in near_bounds");
	}


	foreach my $param ( 'theta', 'omega', 'sigma' ) {
	  my $setm =  eval( '$self -> '.$param.'s' );
	  my $nameref =  eval( '$self -> '.$param.'names' );
	  next unless( defined $setm and defined $nameref); 
	  my @estimates  = @{$setm};
	  #it is assumed here that 'fixed' has element for "full" lower triangular omega and sigma
	  # lower_theta_bounds etc have elements for all theta 1, 2, 3...
	  for ( my $i = 0; $i <= $#estimates; $i++ ) {
	    #loop problem level
	    next unless( defined $estimates[$i] );
	      
	    #prepare init data
	    next unless (scalar(@init_problems) > $i and defined ($init_problems[$i]));
	    my $accessor = $param.'s';
	    unless( $init_problems[$i]-> can($accessor) ) {
				croak("Error unknown parameter type: $param" );
	    }
	    my @records;
	    if (defined $init_problems[$i] -> $accessor()) {
				@records = @{$init_problems[$i] -> $accessor()};
	    }
	    next unless (scalar(@records) > 0); #no parameter in this problem


	    #loop through records and options of input problem
	    #if record is same or fix then skip
	    #name of own param is coord
	    #prior theta omega sigma always come after the regular inits in input model. Therefore still ok to match on coords
	    #for non-prior parameters

	    my %lobnd = {};
	    my %upbnd = {};
	    my %diagonal = {};
	    my %label = {};
	    foreach my $record (@records) {
				if  ($record->same() or $record->fix() or $record->prior()) {
		    	next;
				}
				unless (defined $record -> options()) {
		    	croak("$param record has no values in near_bounds in output object");
		 		}
				foreach my $option (@{$record -> options()}) {
		    	if ($option->fix() or $option->prior()) {
			 			next;
		    	}
		    	my $name = $option -> coordinate_string();
		    	if ( $param eq 'theta' ) {
			 			$diagonal{$name} = 0; #ensure hash entry defined even if theta
		  			$lobnd{$name} = $option ->lobnd();
		  			$lobnd{$name} = -1000000 unless (defined $lobnd{$name});
		  			$upbnd{$name} = $option ->upbnd();
		  			$upbnd{$name} = 1000000 unless (defined $upbnd{$name});
		  			if (defined $option ->label()) {
		      		$label{$name} = $option ->label();
		  			} else {
		      	$label{$name} = $name;
		  			}
		     	} else {    # on_diagonal is only defined for omegas and sigmas
		  			if ( (not $option -> on_diagonal()) and $option->init() == 0) {
		      		next;
		      		#do not check off-diagonal zeros
		  			}
		  			$diagonal{$name} = $option -> on_diagonal();
		     	}
		 		}
	    }

	    if ( $param eq 'theta' ) {
				#first round in loop over params
				$near_bounds[$i] = [];
				$found_bounds[$i] = [];
		 		$found_estimates[$i] = [];
	    }

	    for ( my $j = 0; $j < scalar @{$estimates[$i]}; $j++ ) {
				#loop subproblem level
				if ( $param eq 'theta' ) {
		    	#first round in loop over params
		     	$near_bounds[$i][$j] = [];
		     	$found_bounds[$i][$j] = [];
		    	$found_estimates[$i][$j] = [];
		 		}
		 		next unless( defined $estimates[$i][$j] and defined $nameref->[$i][$j]);
		 		my @values = @{$estimates[$i][$j]};
		 		my @names = @{$nameref->[$i][$j]};
		  
		 		my $debug_count = 0;
		 		for (my $k=0; $k< scalar(@values); $k++) {
		     	next unless (defined $diagonal{$names[$k]}); #defined also for theta as long as value to check
		     	$debug_count++;
		     	if ( $param eq 'theta' ) {
		  			if ( test_sigdig( $values[$k], $lobnd{$names[$k]}, $significant_digits, $zero_limit ) ) {
		      		push( @{$near_bounds[$i][$j]}, $names[$k] );
		      		push( @{$found_bounds[$i][$j]}, $lobnd{$names[$k]} );
		      		push( @{$found_estimates[$i][$j]}, $values[$k] );
		  			}
		  			if ( test_sigdig( $values[$k], $upbnd{$names[$k]}, $significant_digits, $zero_limit ) ) {
							push( @{$near_bounds[$i][$j]},  $names[$k] );
		      		push( @{$found_bounds[$i][$j]}, $upbnd{$names[$k]} );
		      		push( @{$found_estimates[$i][$j]}, $values[$k] );
		  			}
		     	} else {
		  			#omega or sigma
		  			my ( $upper, $lower, $sigdig );
		  			if ($diagonal{$names[$k]}) { # on diagonal
		      		( $lower, $upper, $sigdig ) = ( 0, 1000000, $significant_digits );
		  			} else {
		      		( $lower, $upper, $sigdig ) = ( -1, 1, $off_diagonal_sign_digits );
		  			}
		  			if ( test_sigdig( $values[$k], $lower, $sigdig, $zero_limit ) ) {
		      		push( @{$near_bounds[$i][$j]}, $names[$k] );
		     			push( @{$found_bounds[$i][$j]}, $lower ); #limit
		      		push( @{$found_estimates[$i][$j]}, $values[$k] );
		  			}
		  			if ( test_sigdig( $values[$k], $upper, $sigdig, $zero_limit ) ) {
		      		push( @{$near_bounds[$i][$j]}, $names[$k] );
		      		push( @{$found_bounds[$i][$j]}, $upper ); #limit
		      		push( @{$found_estimates[$i][$j]}, $values[$k] );
		  			}
		     	}
		 		}
			}
	 	}
	}
# line 918 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> near_bounds');
	# End of Non-Dia code #

	return \@near_bounds ,\@found_bounds ,\@found_estimates;
}

sub comegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->comegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->comegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->comegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->comegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->comegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @comegas = @{$self->access_any(attribute=>'comegas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> comegas');
# line 142 "lib/output_subs.pm" 
    # Since PsN output objects are read-only, once they are
    # initialized (probably through parsing a NONMEM output file) the
    # methods of the output class are only used to extract
    # information, not to set any.
    #
    # The general structure of the values returned by the methods
    # reflect the level where the attributes belong (problems or sub
    # problems) and of course also the structure of the attribute
    # itself (scalar (ofv), array (thetas) or matrix
    # (raw_cormatrix)). Taking ofv as example, this means that the
    # returned variable will be a (reference to a) two-dimensional
    # array, with the indexes problem and subproblem since ofv is a
    # scalar on the sub problem level.
    #
    # Most methods take two optional arguments, I<problems> and
    # I<subproblems>. These can be used to specify which problem or sub
    # problem that the method should extract the required information
    # from. problems and subproblems should be references to arrays of
    # numbers. Some methods that return information related to model
    # parameters also take I<parameter_numbers> as another optional
    # argument and this can be used to specify a subset of parameters.
    #
    # Example:
    #
    # Return the standard errors for omega 1 and 3 (in all problems
    # and sub problems)
    #
    #   @seomega = @{$output_object -> seomegas( parameter_numbers => [1,3] )};
    #
    #
    # comegas returns the standard deviation for elements on the
    # diagonal and correlation coefficients for off-diagonal elements.
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in omegacoordval.
# line 993 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> comegas');
	# End of Non-Dia code #

	return \@comegas;
}

sub condition_number {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->condition_number: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->condition_number: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->condition_number: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->condition_number: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->condition_number: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @condition_number = @{$self->access_any(attribute=>'condition_number',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> condition_number');
# line 179 "lib/output_subs.pm" 
    # condition_number returns the 2-norm condition number for the correlation matrix, i.e.
    # the largest eigen value divided by the smallest.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 1039 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> condition_number');
	# End of Non-Dia code #

	return \@condition_number;
}

sub covariance_step_run {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->covariance_step_run: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->covariance_step_run: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->covariance_step_run: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->covariance_step_run: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->covariance_step_run: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @covariance_step_run = @{$self->access_any(attribute=>'covariance_step_run',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> covariance_step_run');
# line 200 "lib/output_subs.pm" 
    # Returns 1 if the covariance step was run, 0 otherwise. See
    # L</comegas> for details.
    #
    # Level:  Problem
# line 1084 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> covariance_step_run');
	# End of Non-Dia code #

	return \@covariance_step_run;
}

sub covariance_step_successful {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->covariance_step_successful: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->covariance_step_successful: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->covariance_step_successful: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->covariance_step_successful: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->covariance_step_successful: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @covariance_step_successful = @{$self->access_any(attribute=>'covariance_step_successful',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> covariance_step_successful');
# line 234 "lib/output_subs.pm" 
    # Returns 1 if the covariance step was successful, 0
    # otherwise. See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
# line 1129 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> covariance_step_successful');
	# End of Non-Dia code #

	return \@covariance_step_successful;
}

sub estimate_near_boundary {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->estimate_near_boundary: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->estimate_near_boundary: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->estimate_near_boundary: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->estimate_near_boundary: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->estimate_near_boundary: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @estimate_near_boundary = @{$self->access_any(attribute=>'estimate_near_boundary',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@estimate_near_boundary;
}

sub covariance_step_warnings {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->covariance_step_warnings: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->covariance_step_warnings: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->covariance_step_warnings: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->covariance_step_warnings: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->covariance_step_warnings: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @covariance_step_warnings = @{$self->access_any(attribute=>'covariance_step_warnings',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> covariance_step_warnings');
# line 241 "lib/output_subs.pm" 
    # Returns 0 if there were no warnings or errors printed during the
    # covariance step, 1 otherwise. See L</comegas> for details on the
    # method arguments.
    #
    # Level:  Sub problem
# line 1212 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> covariance_step_warnings');
	# End of Non-Dia code #

	return \@covariance_step_warnings;
}

sub s_matrix_singular {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->s_matrix_singular: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->s_matrix_singular: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->s_matrix_singular: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->s_matrix_singular: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->s_matrix_singular: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @s_matrix_singular = @{$self->access_any(attribute=>'s_matrix_singular',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@s_matrix_singular;
}

sub csigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->csigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->csigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->csigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->csigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->csigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @csigmas = @{$self->access_any(attribute=>'csigmas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> csigmas');
# line 249 "lib/output_subs.pm" 
    # csigmas returns the standard deviation for elements on the
    # diagonal and correlation coefficients for off-diagonal elements.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in sigmacoordval.
# line 1297 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> csigmas');
	# End of Non-Dia code #

	return \@csigmas;
}

sub cvsethetas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->cvsethetas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->cvsethetas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->cvsethetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvsethetas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvsethetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvsethetas = @{$self->access_any(attribute=>'cvsethetas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> cvsethetas');
# line 277 "lib/output_subs.pm" 
    # cvsethetas returns the relative standard error for the thetas, i.e. SE/estimate.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in thetacoordval.
# line 1344 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> cvsethetas');
	# End of Non-Dia code #

	return \@cvsethetas;
}

sub cvseomegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->cvseomegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->cvseomegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->cvseomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvseomegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvseomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvseomegas = @{$self->access_any(attribute=>'cvseomegas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> cvseomegas');
# line 259 "lib/output_subs.pm" 
    # cvseomegas returns the relative standard error for the omegas, i.e. SE/estimate.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in omegacoordval.
# line 1391 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> cvseomegas');
	# End of Non-Dia code #

	return \@cvseomegas;
}

sub cvsesigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->cvsesigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->cvsesigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->cvsesigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvsesigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvsesigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvsesigmas = @{$self->access_any(attribute=>'cvsesigmas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> cvsesigmas');
# line 268 "lib/output_subs.pm" 
    # cvsesigmas returns the relative standard error for the sigmas, i.e. SE/estimate.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in sigmacoordval.
# line 1438 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> cvsesigmas');
	# End of Non-Dia code #

	return \@cvsesigmas;
}

sub shrinkage_eta {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->shrinkage_eta: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->shrinkage_eta: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->shrinkage_eta: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->shrinkage_eta: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->shrinkage_eta: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @shrinkage_eta = @{$self->access_any(attribute=>'shrinkage_eta',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@shrinkage_eta;
}

sub shrinkage_eps {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->shrinkage_eps: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->shrinkage_eps: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->shrinkage_eps: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->shrinkage_eps: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->shrinkage_eps: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @shrinkage_eps = @{$self->access_any(attribute=>'shrinkage_eps',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@shrinkage_eps;
}

sub eigens {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->eigens: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->eigens: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->eigens: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->eigens: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->eigens: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @eigens = @{$self->access_any(attribute=>'eigens',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> eigens');
# line 286 "lib/output_subs.pm" 
    # eigens returns the eigen values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 1557 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> eigens');
	# End of Non-Dia code #

	return \@eigens;
}

sub etabar {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->etabar: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->etabar: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->etabar: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->etabar: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->etabar: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @etabar = @{$self->access_any(attribute=>'etabar',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> etabar');
# line 293 "lib/output_subs.pm" 
    # etabar returns the ETABAR estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 1602 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> etabar');
	# End of Non-Dia code #

	return \@etabar;
}

sub feval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->feval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->feval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->feval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->feval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->feval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @feval = @{$self->access_any(attribute=>'feval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> feval');
# line 300 "lib/output_subs.pm" 
    # feval returns the number of function evaluations.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 1647 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> feval');
	# End of Non-Dia code #

	return \@feval;
}

sub finalparam {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->finalparam: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->finalparam: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->finalparam: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->finalparam: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->finalparam: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @finalparam = @{$self->access_any(attribute=>'finalparam',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> finalparam');
# line 307 "lib/output_subs.pm" 
    # finalparam returns the final parameter vector as it appears in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 1692 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> finalparam');
	# End of Non-Dia code #

	return \@finalparam;
}

sub final_gradients {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->final_gradients: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->final_gradients: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->final_gradients: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->final_gradients: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->final_gradients: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @final_gradients = @{$self->access_any(attribute=>'final_gradients',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> final_gradients');
# line 314 "lib/output_subs.pm" 
    # final_gradients returns the final gradient vector as it appears in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 1737 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> final_gradients');
	# End of Non-Dia code #

	return \@final_gradients;
}

sub fixedomegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->fixedomegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->fixedomegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->fixedomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->fixedomegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->fixedomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_index = $parm{'problem_index'};
	my @fixedomegas;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> fixedomegas');
# line 321 "lib/output_subs.pm" 
	# fixedomegas returns the a vector of booleans; 1's if
	# the parameters were fixed during the model fit, 0's
	# if they were not.

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->fixedomegas()) {
		@fixedomegas = @{$self->problems->[$problem_index]->fixedomegas()};
	}
# line 1785 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> fixedomegas');
	# End of Non-Dia code #

	return \@fixedomegas;
}

sub estimated_sigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->estimated_sigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->estimated_sigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->estimated_sigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->estimated_sigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->estimated_sigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @estimatedsigmas = @{$self->access_any(attribute=>'estimatedsigmas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@estimatedsigmas;
}

sub estimated_thetas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->estimated_thetas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->estimated_thetas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->estimated_thetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->estimated_thetas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->estimated_thetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @estimatedthetas = @{$self->access_any(attribute=>'estimatedthetas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@estimatedthetas;
}

sub estimated_omegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->estimated_omegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->estimated_omegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->estimated_omegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->estimated_omegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->estimated_omegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @estimatedomegas = @{$self->access_any(attribute=>'estimatedomegas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@estimatedomegas;
}

sub est_thetanames {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->est_thetanames: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->est_thetanames: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->est_thetanames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->est_thetanames: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->est_thetanames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @est_thetanames = @{$self->access_any(attribute=>'est_thetanames',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> est_thetanames');
# line 187 "lib/output_subs.pm" 
    # return thetanames from output_matrix_headers
# line 1938 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> est_thetanames');
	# End of Non-Dia code #

	return \@est_thetanames;
}

sub est_omeganames {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->est_omeganames: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->est_omeganames: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->est_omeganames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->est_omeganames: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->est_omeganames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @est_omeganames = @{$self->access_any(attribute=>'est_omeganames',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> est_omeganames');
# line 191 "lib/output_subs.pm" 
    # return omeganames from output_matrix headers
# line 1980 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> est_omeganames');
	# End of Non-Dia code #

	return \@est_omeganames;
}

sub est_sigmanames {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->est_sigmanames: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->est_sigmanames: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->est_sigmanames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->est_sigmanames: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->est_sigmanames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @est_sigmanames = @{$self->access_any(attribute=>'est_sigmanames',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> est_sigmanames');
# line 195 "lib/output_subs.pm" 
    # return sigmanames from output_matrix headers
# line 2022 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> est_sigmanames');
	# End of Non-Dia code #

	return \@est_sigmanames;
}

sub fixedsigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->fixedsigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->fixedsigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->fixedsigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->fixedsigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->fixedsigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_index = $parm{'problem_index'};
	my @fixedsigmas;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> fixedsigmas');
# line 332 "lib/output_subs.pm" 
	# fixedsigmas returns a vector of booleans; 1's if
	# the parameters were fixed during the model fit, 0's
	# if they were not.

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->fixedsigmas()) {
      @fixedsigmas = @{$self->problems->[$problem_index]->fixedsigmas()};
	}
# line 2070 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> fixedsigmas');
	# End of Non-Dia code #

	return \@fixedsigmas;
}

sub fixedthetas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->fixedthetas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->fixedthetas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->fixedthetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->fixedthetas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->fixedthetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_index = $parm{'problem_index'};
	my @fixedthetas;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> fixedthetas');
# line 343 "lib/output_subs.pm" 
	# fixedthetas returns a vector of booleans; 1's if
	# the parameters were fixed during the model fit, 0's
	# if they were not.

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->fixedthetas()) {
		@fixedthetas = @{$self->problems->[$problem_index]->fixedthetas()};
	}
# line 2118 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> fixedthetas');
	# End of Non-Dia code #

	return \@fixedthetas;
}

sub flush {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> flush');
# line 1396 "lib/output_subs.pm" 
	# flush is not an accessor method. As its name implies it flushes the
	# output objects memory by setting the I<problems> attribute to undef.
	# This method can be useful when many output objects are handled and
	# the memory is limited.

	# Flushes the object to save memory. There is no need to
	# synchronize the ouptut object before this since they are read-
	# only.

	$self->problems([]);
	#$self -> {'synced'} = 0;
# line 2142 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> flush');
	# End of Non-Dia code #

}

sub funcevalpath {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->funcevalpath: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->funcevalpath: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->funcevalpath: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->funcevalpath: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->funcevalpath: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @funcevalpath = @{$self->access_any(attribute=>'funcevalpath',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> funcevalpath');
# line 373 "lib/output_subs.pm" 
    # funcevalpath returns the number of function evaluations for each printed iteration in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 2186 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> funcevalpath');
	# End of Non-Dia code #

	return \@funcevalpath;
}

sub gradient_path {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->gradient_path: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->gradient_path: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->gradient_path: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->gradient_path: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->gradient_path: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @gradient_path = @{$self->access_any(attribute=>'gradient_path',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> gradient_path');
# line 380 "lib/output_subs.pm" 
    # gradient_path returns the gradients for each printed iteration in the monitoring of search section (returns a matrix for each sub problem).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 2231 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> gradient_path');
	# End of Non-Dia code #

	return \@gradient_path;
}

sub have_output {
	my $self = shift;
	my $return_value = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> have_output');
# line 748 "lib/output_subs.pm" 
	# have_output returns true if the output files exits or if there
	# is output data in memory.

	if ( -e $self->full_name || not_empty($self->problems) ) {
	  return 1;
	} else {
	  return 0;
	}
# line 2253 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> have_output');
	# End of Non-Dia code #

	return $return_value;
}

sub have_user_defined_prior {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->have_user_defined_prior: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->have_user_defined_prior: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->have_user_defined_prior: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->have_user_defined_prior: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->have_user_defined_prior: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @user_defined_prior = @{$self->access_any(attribute=>'user_defined_prior',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@user_defined_prior;
}

sub initgrad {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->initgrad: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->initgrad: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->initgrad: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->initgrad: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->initgrad: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @initgrad = @{$self->access_any(attribute=>'initgrad',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> initgrad');
# line 387 "lib/output_subs.pm" 
    # initgrad returns the initial gradient vector in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 2335 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> initgrad');
	# End of Non-Dia code #

	return \@initgrad;
}

sub initthetas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->initthetas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->initthetas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->initthetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->initthetas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->initthetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @initthetas = @{$self->access_any(attribute=>'initthetas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> initthetas');
# line 410 "lib/output_subs.pm" 
    # initthetas returns the initial theta values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 2381 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> initthetas');
	# End of Non-Dia code #

	return \@initthetas;
}

sub inverse_covariance_matrix {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @inverse_covariance_matrix = @{$self->access_any(attribute=>'inverse_covariance_matrix',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> inverse_covariance_matrix');
# line 548 "lib/output_subs.pm" 
    # inverse_covariance_matrix returns the inverse covariance matrix 
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 2426 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> inverse_covariance_matrix');
	# End of Non-Dia code #

	return \@inverse_covariance_matrix;
}

sub omega_indexes {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->omega_indexes: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->omega_indexes: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->omega_indexes: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omega_indexes: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omega_indexes: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @omega_indexes = @{$self->access_any(attribute=>'omega_indexes',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@omega_indexes;
}

sub sigma_indexes {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sigma_indexes: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sigma_indexes: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sigma_indexes: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigma_indexes: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigma_indexes: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sigma_indexes = @{$self->access_any(attribute=>'sigma_indexes',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@sigma_indexes;
}

sub iternum {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->iternum: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->iternum: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->iternum: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->iternum: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->iternum: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @iternum = @{$self->access_any(attribute=>'iternum',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> iternum');
# line 418 "lib/output_subs.pm" 
    # iternum returns a vector of the iteration numbers in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 2545 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> iternum');
	# End of Non-Dia code #

	return \@iternum;
}

sub labels {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->labels: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->labels: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->labels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->labels: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->labels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @labels;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> labels');
# line 1347 "lib/output_subs.pm" 
	if ( not defined $parameter_type or $parameter_type eq '' ) {
		my @thetanames = @{$self->thetanames};
		my @omeganames = @{$self->omeganames};
		my @sigmanames = @{$self->sigmanames};
		for ( my $i = 0; $i <= $#thetanames; $i++ ) {
			#problem
			if (defined $thetanames[$i]) {
				for ( my $j = 0; $j < scalar(@{$thetanames[$i]}); $j++ ) {
					#subproblems
					my @lab = ();
					if ( defined $thetanames[$i]->[$j] ) {
						push( @lab, @{$thetanames[$i]->[$j]});
					}
					if ( defined $omeganames[$i] ) {
						if( defined $omeganames[$i]->[$j] ) {
							push( @lab, @{$omeganames[$i]->[$j]}); 
						}
					}
					if ( defined $sigmanames[$i] ) {
						if ( defined $sigmanames[$i]->[$j] ) {
							push( @lab, @{$sigmanames[$i]->[$j]});
						}
					}
					push (@{$labels[$i]}, \@lab);
				}
			}
		}
	} else {
		my $access = $parameter_type . "names";
		my @names = @{$self -> $access};
		for ( my $i = 0; $i <= $#names; $i++ ) {
			#problems
			if ( defined $names[$i] ) {
				for ( my $j = 0; $j < scalar(@{$names[$i]}); $j++ ) {
					#subproblems
					my @lab = ();
					if ( defined $names[$i]->[$j] ) {
						push( @lab, @{$names[$i]->[$j]} );
					}
					push( @{$labels[$i]}, \@lab);
				}
			}
		}
	}
# line 2629 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> labels');
	# End of Non-Dia code #

	return \@labels;
}

sub nind {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->nind: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->nind: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->nind: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->nind: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->nind: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @nind = @{$self->access_any(attribute=>'nind',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nind');
# line 425 "lib/output_subs.pm" 
    # nind returns the number of individuals.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Problem
# line 2674 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nind');
	# End of Non-Dia code #

	return \@nind;
}

sub nobs {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->nobs: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->nobs: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->nobs: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->nobs: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->nobs: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @nobs = @{$self->access_any(attribute=>'nobs',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nobs');
# line 432 "lib/output_subs.pm" 
    # nobs returns the number of observations.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Problem
# line 2719 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nobs');
	# End of Non-Dia code #

	return \@nobs;
}

sub npofv {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->npofv: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->npofv: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->npofv: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->npofv: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->npofv: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @npofv = @{$self->access_any(attribute=>'npofv',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> npofv');
# line 439 "lib/output_subs.pm" 
    # npofv returns the non-parametric objective function value.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 2764 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> npofv');
	# End of Non-Dia code #

	return \@npofv;
}

sub nrecs {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->nrecs: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->nrecs: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->nrecs: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->nrecs: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->nrecs: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @nrecs = @{$self->access_any(attribute=>'nrecs',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nrecs');
# line 446 "lib/output_subs.pm" 
    # nrecs returns the number of records.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Problem
# line 2809 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nrecs');
	# End of Non-Dia code #

	return \@nrecs;
}

sub npomegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->npomegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->npomegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->npomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->npomegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->npomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @npomegas = @{$self->access_any(attribute=>'npomegas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> npomegas');
# line 453 "lib/output_subs.pm" 
    # npomegas returns the non-parametric omega estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 2854 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> npomegas');
	# End of Non-Dia code #

	return \@npomegas;
}

sub npetabars {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->npetabars: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->npetabars: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->npetabars: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->npetabars: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->npetabars: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @npetabars = @{$self->access_any(attribute=>'npetabars',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> npetabars');
# line 460 "lib/output_subs.pm" 
    # npthetas returns the non-parametric theta estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 2899 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> npetabars');
	# End of Non-Dia code #

	return \@npetabars;
}

sub nth {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->nth: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->nth: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->nth: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->nth: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->nth: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @nth = @{$self->access_any(attribute=>'nth',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nth');
# line 467 "lib/output_subs.pm" 
    # nth returns the number of thetas.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem

    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 2946 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nth');
	# End of Non-Dia code #

	return \@nth;
}

sub ofvpath {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->ofvpath: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->ofvpath: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->ofvpath: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->ofvpath: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->ofvpath: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @ofvpath = @{$self->access_any(attribute=>'ofvpath',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> ofvpath');
# line 476 "lib/output_subs.pm" 
    # ofvpath returns the objective [function] values in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 2991 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> ofvpath');
	# End of Non-Dia code #

	return \@ofvpath;
}

sub get_single_value {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'attribute' => 'm_SCALAR', 'problem_index' => 'SCALAR',
			'subproblem_index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->get_single_value: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->get_single_value: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->get_single_value: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->get_single_value: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->get_single_value: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $attribute = $parm{'attribute'};
	my $problem_index = defined $parm{'problem_index'} ? $parm{'problem_index'} : 0;
	my $subproblem_index = defined $parm{'subproblem_index'} ? $parm{'subproblem_index'} : 0;
	my $return_value;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_single_value');
# line 207 "lib/output_subs.pm" 
	my $arr;
	$return_value = undef;
	if ($self->can($attribute)) {
	  $arr = $self->$attribute(problems => [($problem_index + 1)], subproblems => [($subproblem_index + 1)]);
	  if (defined $arr->[0]) {
	    $return_value=$arr->[0]->[0];
	  } else {
	    1;
	  }
	} elsif ( ($attribute eq 'estimation_step_run') ) {
# removed support ($attribute eq 'user_defined_prior') 
#		or ($attribute eq 'omega_block_structure_type')
#		or ($attribute eq 'sigma_block_structure_type')
#		or ($attribute eq 'omega_block_sets')
#		or ($attribute eq 'sigma_block_sets')
	  $arr = $self->access_any(attribute => $attribute,
				   problems => [($problem_index + 1)],
				   subproblems => [(1)]);
	  if (defined $arr->[0]) {
	    $return_value=$arr->[0]->[0];
	  }
	} else {
	  croak("unknown attribute $attribute");
	}
# line 3058 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_single_value');
	# End of Non-Dia code #

	return $return_value;
}

sub ofv {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->ofv: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->ofv: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->ofv: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->ofv: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->ofv: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @ofv = @{$self->access_any(attribute=>'ofv',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> ofv');
# line 483 "lib/output_subs.pm" 
    # ofv returns the objective function value(s).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3103 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> ofv');
	# End of Non-Dia code #

	return \@ofv;
}

sub dic {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->dic: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->dic: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->dic: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->dic: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->dic: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @dic = @{$self->access_any(attribute=>'dic',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> dic');
# line 490 "lib/output_subs.pm" 
    # Level:  Sub problem
# line 3145 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> dic');
	# End of Non-Dia code #

	return \@dic;
}

sub have_omegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->have_omegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->have_omegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->have_omegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->have_omegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->have_omegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @have_omegas = @{$self->access_any(attribute=>'have_omegas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@have_omegas;
}

sub have_sigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->have_sigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->have_sigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->have_sigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->have_sigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->have_sigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @have_sigmas = @{$self->access_any(attribute=>'have_sigmas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@have_sigmas;
}

sub omega_block_structure {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY',
			'' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->omega_block_structure: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->omega_block_structure: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->omega_block_structure: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omega_block_structure: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omega_block_structure: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @omega_block_structure = @{$self->access_any(attribute=>'omega_block_structure',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> omega_block_structure');
# line 494 "lib/output_subs.pm" 
    # omega_block_structure returns the block structure for
    # the omega parameters in a lower triangular matrix form
    # as in the OMEGA HAS BLOCK FORM section in the NONMEM output file.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 3268 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> omega_block_structure');
	# End of Non-Dia code #

	return \@omega_block_structure;
}

sub omegacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->omegacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->omegacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->omegacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omegacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omegacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @omegacoordval = @{$self->access_any(attribute=>'omegacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> omegacoordval');
# line 504 "lib/output_subs.pm" 
    # omegacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. OMEGA(1,1), OMEGA(1,2) etc as keys
    # and parameter estimates as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3315 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> omegacoordval');
	# End of Non-Dia code #

	return \@omegacoordval;
}

sub seomegacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->seomegacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->seomegacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->seomegacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->seomegacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->seomegacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @seomegacoordval = @{$self->access_any(attribute=>'seomegacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> seomegacoordval');
# line 609 "lib/output_subs.pm" 
    # seomegacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. OMEGA(1,1), OMEGA(1,2) etc as keys
    # and parameter standard errors as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3362 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> seomegacoordval');
	# End of Non-Dia code #

	return \@seomegacoordval;
}

sub omeganames {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->omeganames: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->omeganames: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->omeganames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omeganames: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omeganames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @omeganames = @{$self->access_any(attribute=>'omeganames',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> omeganames');
# line 513 "lib/output_subs.pm" 
    # omeganames returns the default parameter names, OMEGA(1,1) etc for stored values
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3407 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> omeganames');
	# End of Non-Dia code #

	return \@omeganames;
}

sub cvseomegacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->cvseomegacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->cvseomegacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->cvseomegacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvseomegacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvseomegacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvseomegacoordval = @{$self->access_any(attribute=>'cvseomegacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@cvseomegacoordval;
}

sub cvsesigmacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->cvsesigmacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->cvsesigmacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->cvsesigmacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvsesigmacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvsesigmacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvsesigmacoordval = @{$self->access_any(attribute=>'cvsesigmacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@cvsesigmacoordval;
}

sub covariance_matrix {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->covariance_matrix: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->covariance_matrix: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->covariance_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->covariance_matrix: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->covariance_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @covariance_matrix = @{$self->access_any(attribute=>'covariance_matrix',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@covariance_matrix;
}

sub cvsethetacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->cvsethetacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->cvsethetacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->cvsethetacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvsethetacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->cvsethetacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvsethetacoordval = @{$self->access_any(attribute=>'cvsethetacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@cvsethetacoordval;
}

sub comegacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->comegacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->comegacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->comegacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->comegacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->comegacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @comegacoordval = @{$self->access_any(attribute=>'comegacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@comegacoordval;
}

sub csigmacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->csigmacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->csigmacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->csigmacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->csigmacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->csigmacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @csigmacoordval = @{$self->access_any(attribute=>'csigmacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@csigmacoordval;
}

sub omegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY',
			'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->omegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->omegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->omegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->omegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @omegas = @{$self->access_any(attribute=>'omegas',problems=>\@problems,subproblems=>\@subproblems,parameter_numbers=>\@parameter_numbers)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> omegas');
# line 520 "lib/output_subs.pm" 
    # omegas returns the omega parameter estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3676 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> omegas');
	# End of Non-Dia code #

	return \@omegas;
}

sub parameter_path {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->parameter_path: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->parameter_path: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->parameter_path: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->parameter_path: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->parameter_path: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_path = @{$self->access_any(attribute=>'parameter_path',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> parameter_path');
# line 527 "lib/output_subs.pm" 
    # parameter_path returns the (normalized) parameter estimates for each iteration in the monitoring of search section (Matrix returned).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3721 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> parameter_path');
	# End of Non-Dia code #

	return \@parameter_path;
}

sub pval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->pval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->pval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->pval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->pval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->pval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @pval = @{$self->access_any(attribute=>'pval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> pval');
# line 534 "lib/output_subs.pm" 
    # pval returns the P VAL (reflects the probability that the etas are not centered around zero).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3766 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> pval');
	# End of Non-Dia code #

	return \@pval;
}

sub raw_covmatrix {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->raw_covmatrix: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->raw_covmatrix: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->raw_covmatrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_covmatrix: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_covmatrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_covmatrix = @{$self->access_any(attribute=>'raw_covmatrix',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> raw_covmatrix');
# line 541 "lib/output_subs.pm" 
    # raw_covmatrix returns the (raw) covariance matrix including empty matrix elements marked as '.........'.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3811 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> raw_covmatrix');
	# End of Non-Dia code #

	return \@raw_covmatrix;
}

sub inverse_covariance_matrix {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->inverse_covariance_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @inverse_covariance_matrix = @{$self->access_any(attribute=>'inverse_covariance_matrix',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> inverse_covariance_matrix');
# line 548 "lib/output_subs.pm" 
    # inverse_covariance_matrix returns the inverse covariance matrix 
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3856 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> inverse_covariance_matrix');
	# End of Non-Dia code #

	return \@inverse_covariance_matrix;
}

sub raw_cormatrix {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->raw_cormatrix: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->raw_cormatrix: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->raw_cormatrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_cormatrix: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_cormatrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_cormatrix = @{$self->access_any(attribute=>'raw_cormatrix',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> raw_cormatrix');
# line 555 "lib/output_subs.pm" 
    # raw_cormatrix returns the (raw) correlation matrix including empty matrix elements marked as '.........'.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 3901 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> raw_cormatrix');
	# End of Non-Dia code #

	return \@raw_cormatrix;
}

sub correlation_matrix {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->correlation_matrix: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->correlation_matrix: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->correlation_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->correlation_matrix: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->correlation_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @correlation_matrix = @{$self->access_any(attribute=>'correlation_matrix',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@correlation_matrix;
}

sub output_matrix_headers {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->output_matrix_headers: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->output_matrix_headers: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->output_matrix_headers: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->output_matrix_headers: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->output_matrix_headers: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @output_matrix_headers = @{$self->access_any(attribute=>'output_matrix_headers',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@output_matrix_headers;
}

sub raw_omegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->raw_omegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->raw_omegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->raw_omegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_omegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_omegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_omegas = @{$self->access_any(attribute=>'raw_omegas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> raw_omegas');
# line 562 "lib/output_subs.pm" 
    # raw_omegas returns the (raw) omegas.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 4021 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> raw_omegas');
	# End of Non-Dia code #

	return \@raw_omegas;
}

sub raw_seomegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->raw_seomegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->raw_seomegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->raw_seomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_seomegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_seomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_seomegas = @{$self->access_any(attribute=>'raw_seomegas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> raw_seomegas');
# line 570 "lib/output_subs.pm" 
    # raw_seomegas returns the (raw) omega standard error estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 4067 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> raw_seomegas');
	# End of Non-Dia code #

	return \@raw_seomegas;
}

sub raw_sesigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->raw_sesigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->raw_sesigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->raw_sesigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_sesigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_sesigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_sesigmas = @{$self->access_any(attribute=>'raw_sesigmas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> raw_sesigmas');
# line 578 "lib/output_subs.pm" 
    # raw_sesigmas returns the (raw) sigma standard error estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 4113 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> raw_sesigmas');
	# End of Non-Dia code #

	return \@raw_sesigmas;
}

sub raw_sigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->raw_sigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->raw_sigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->raw_sigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_sigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_sigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_sigmas = @{$self->access_any(attribute=>'raw_sigmas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> raw_sigmas');
# line 586 "lib/output_subs.pm" 
    # raw_sigmas returns the (raw) sigmas.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 4159 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> raw_sigmas');
	# End of Non-Dia code #

	return \@raw_sigmas;
}

sub raw_tmatrix {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->raw_tmatrix: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->raw_tmatrix: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->raw_tmatrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_tmatrix: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->raw_tmatrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_tmatrix = @{$self->access_any(attribute=>'raw_tmatrix',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> raw_tmatrix');
# line 594 "lib/output_subs.pm" 
    # raw_tmatrix returns the (raw) T-matrix.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 4204 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> raw_tmatrix');
	# End of Non-Dia code #

	return \@raw_tmatrix;
}

sub seomegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY',
			'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->seomegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->seomegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->seomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->seomegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->seomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @seomegas = @{$self->access_any(attribute=>'seomegas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> seomegas');
# line 636 "lib/output_subs.pm" 
    # seomegas returns the omega standard error estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 4251 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> seomegas');
	# End of Non-Dia code #

	return \@seomegas;
}

sub sesigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY',
			'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sesigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sesigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sesigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sesigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sesigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @sesigmas = @{$self->access_any(attribute=>'sesigmas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sesigmas');
# line 643 "lib/output_subs.pm" 
#    # sesigmas returns the sigma standard error estimates.
#    # See L</comegas> for details of the method arguments.
#    #
#    # Level:  Sub problem
# line 4298 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sesigmas');
	# End of Non-Dia code #

	return \@sesigmas;
}

sub sethetas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY',
			'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sethetas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sethetas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sethetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sethetas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sethetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @sethetas = @{$self->access_any(attribute=>'sethetas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sethetas');
# line 650 "lib/output_subs.pm" 
#    # sethetas returns the theta standard error estimates.
#    # See L</comegas> for details of the method arguments.
#    #
#    # Level:  Sub problem
# line 4345 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sethetas');
	# End of Non-Dia code #

	return \@sethetas;
}

sub significant_digits {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->significant_digits: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->significant_digits: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->significant_digits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->significant_digits: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->significant_digits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @significant_digits = @{$self->access_any(attribute=>'significant_digits',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> significant_digits');
# line 657 "lib/output_subs.pm" 
    # significant_digits returns the number of significant digits for the model fit.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 4390 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> significant_digits');
	# End of Non-Dia code #

	return \@significant_digits;
}

sub sigma_block_structure {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sigma_block_structure: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sigma_block_structure: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sigma_block_structure: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigma_block_structure: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigma_block_structure: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sigma_block_structure = @{$self->access_any(attribute=>'sigma_block_structure',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sigma_block_structure');
# line 664 "lib/output_subs.pm" 
    # sigma_block_structure returns the block structure for
    # the sigma parameters in a lower triangular matrix form
    # as in the sigma HAS BLOCK FORM section in the NONMEM output file.
    # See L</csigmas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 4438 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sigma_block_structure');
	# End of Non-Dia code #

	return \@sigma_block_structure;
}

sub sigmacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sigmacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sigmacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sigmacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigmacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigmacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sigmacoordval = @{$self->access_any(attribute=>'sigmacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sigmacoordval');
# line 674 "lib/output_subs.pm" 
    # sigmacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. SIGMA(1,1), SIGMA(1,2) etc as keys
    # and parameter estimates as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 4485 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sigmacoordval');
	# End of Non-Dia code #

	return \@sigmacoordval;
}

sub sesigmacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sesigmacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sesigmacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sesigmacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sesigmacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sesigmacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sesigmacoordval = @{$self->access_any(attribute=>'sesigmacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sesigmacoordval');
# line 618 "lib/output_subs.pm" 
    # sesigmacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. SIGMA(1,1), SIGMA(1,2) etc as keys
    # and parameter standard errors as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 4532 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sesigmacoordval');
	# End of Non-Dia code #

	return \@sesigmacoordval;
}

sub sigmanames {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sigmanames: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sigmanames: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sigmanames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigmanames: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigmanames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sigmanames = @{$self->access_any(attribute=>'sigmanames',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sigmanames');
# line 683 "lib/output_subs.pm" 
    # sigmanames returns the default parameter names, i.e. SI1, SI1_2, SI2 etc.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 4577 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sigmanames');
	# End of Non-Dia code #

	return \@sigmanames;
}

sub sigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY',
			'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @sigmas = @{$self->access_any(attribute=>'sigmas',problems=>\@problems,subproblems=>\@subproblems,parameter_numbers=>\@parameter_numbers)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sigmas');
# line 690 "lib/output_subs.pm" 
    # sigmas returns the sigma parameter estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 4624 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sigmas');
	# End of Non-Dia code #

	return \@sigmas;
}

sub simulationstep {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->simulationstep: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->simulationstep: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->simulationstep: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->simulationstep: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->simulationstep: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @simulationstep = @{$self->access_any(attribute=>'simulationstep',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> simulationstep');
# line 697 "lib/output_subs.pm" 
    # simulationstep returns a boolean value 1 or 0, reflecting
    # whether a simulation was performed or not.  See L</comegas> for
    # Details of the method arguments.
    #
    # Level:  Sub Problem
# line 4670 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> simulationstep');
	# End of Non-Dia code #

	return \@simulationstep;
}

sub minimization_successful {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->minimization_successful: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->minimization_successful: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->minimization_successful: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->minimization_successful: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->minimization_successful: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @minimization_successful = @{$self->access_any(attribute=>'minimization_successful',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> minimization_successful');
# line 705 "lib/output_subs.pm" 
    # minimization_successful returns a boolean value 1 or 0,
    # reflecting whether the minimization was successful or not.  See
    # L</comegas> for details of the method arguments.
    #
    # Level:  Sub Problem
# line 4716 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> minimization_successful');
	# End of Non-Dia code #

	return \@minimization_successful;
}

sub upper_omega_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->upper_omega_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->upper_omega_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->upper_omega_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->upper_omega_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->upper_omega_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @upper_omega_bounds = @{$self->access_any(attribute=>'upper_omega_bounds',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@upper_omega_bounds;
}

sub lower_omega_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->lower_omega_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->lower_omega_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->lower_omega_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->lower_omega_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->lower_omega_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @lower_omega_bounds = @{$self->access_any(attribute=>'lower_omega_bounds',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@lower_omega_bounds;
}

sub upper_sigma_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->upper_sigma_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->upper_sigma_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->upper_sigma_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->upper_sigma_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->upper_sigma_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @upper_sigma_bounds = @{$self->access_any(attribute=>'upper_sigma_bounds',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@upper_sigma_bounds;
}

sub lower_sigma_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->lower_sigma_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->lower_sigma_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->lower_sigma_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->lower_sigma_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->lower_sigma_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @lower_sigma_bounds = @{$self->access_any(attribute=>'lower_sigma_bounds',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@lower_sigma_bounds;
}

sub upper_theta_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->upper_theta_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->upper_theta_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->upper_theta_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->upper_theta_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->upper_theta_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_index = $parm{'problem_index'};
	my @upper_theta_bounds;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> upper_theta_bounds');
# line 354 "lib/output_subs.pm" 
	# returns a vector of numbers

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->upper_theta_bounds()) {
		@upper_theta_bounds = @{$self->problems->[$problem_index]->upper_theta_bounds()};
	}
# line 4910 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> upper_theta_bounds');
	# End of Non-Dia code #

	return \@upper_theta_bounds;
}

sub lower_theta_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->lower_theta_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->lower_theta_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->lower_theta_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->lower_theta_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->lower_theta_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_index = $parm{'problem_index'};
	my @lower_theta_bounds;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> lower_theta_bounds');
# line 363 "lib/output_subs.pm" 
	# returns a vector of numbers

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->lower_theta_bounds()) {
		@lower_theta_bounds = @{$self->problems->[$problem_index]->lower_theta_bounds()};
	}
# line 4956 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> lower_theta_bounds');
	# End of Non-Dia code #

	return \@lower_theta_bounds;
}

sub final_zero_gradients {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->final_zero_gradients: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->final_zero_gradients: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->final_zero_gradients: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->final_zero_gradients: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->final_zero_gradients: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @final_zero_gradients = @{$self->access_any(attribute=>'final_zero_gradients',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@final_zero_gradients;
}

sub hessian_reset {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->hessian_reset: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->hessian_reset: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->hessian_reset: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->hessian_reset: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->hessian_reset: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @hessian_reset = @{$self->access_any(attribute=>'hessian_reset',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@hessian_reset;
}

sub zero_gradients {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->zero_gradients: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->zero_gradients: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->zero_gradients: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->zero_gradients: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->zero_gradients: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @zero_gradients = @{$self->access_any(attribute=>'zero_gradients',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@zero_gradients;
}

sub rounding_errors {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->rounding_errors: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->rounding_errors: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->rounding_errors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->rounding_errors: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->rounding_errors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @rounding_errors = @{$self->access_any(attribute=>'rounding_errors',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@rounding_errors;
}

sub minimization_message {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->minimization_message: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->minimization_message: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->minimization_message: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->minimization_message: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->minimization_message: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @minimization_message = @{$self->access_any(attribute=>'minimization_message',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> minimization_message');
# line 713 "lib/output_subs.pm" 
    # minimization_message returns the minimization message, i.e
    #   MINIMIZATION SUCCESSFUL... 
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 5150 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> minimization_message');
	# End of Non-Dia code #

	return \@minimization_message;
}

sub thetacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->thetacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->thetacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->thetacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->thetacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->thetacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @thetacoordval = @{$self->access_any(attribute=>'thetacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> thetacoordval');
# line 721 "lib/output_subs.pm" 
    # thetacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. THETA1, THETA2 etc as keys
    # and parameter estimates as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 5197 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> thetacoordval');
	# End of Non-Dia code #

	return \@thetacoordval;
}

sub sethetacoordval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sethetacoordval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sethetacoordval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sethetacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sethetacoordval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sethetacoordval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sethetacoordval = @{$self->access_any(attribute=>'sethetacoordval',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sethetacoordval');
# line 627 "lib/output_subs.pm" 
    # sethetacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. THETA1, THETA2 etc as keys
    # and parameter standard errors as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 5244 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sethetacoordval');
	# End of Non-Dia code #

	return \@sethetacoordval;
}

sub sum_estimation_time {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sum_estimation_time: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sum_estimation_time: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sum_estimation_time: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sum_estimation_time: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sum_estimation_time: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sum_estimation_time = @{$self->access_any(attribute=>'sum_estimation_time',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sum_estimation_time');
# line 601 "lib/output_subs.pm" 
    # Level:  Sub problem
# line 5286 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sum_estimation_time');
	# End of Non-Dia code #

	return \@sum_estimation_time;
}

sub burn_in_convergence {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->burn_in_convergence: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->burn_in_convergence: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->burn_in_convergence: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->burn_in_convergence: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->burn_in_convergence: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @burn_in_convergence = @{$self->access_any(attribute=>'burn_in_convergence',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@burn_in_convergence;
}

sub burn_in_iterations {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->burn_in_iterations: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->burn_in_iterations: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->burn_in_iterations: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->burn_in_iterations: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->burn_in_iterations: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @burn_in_iterations = @{$self->access_any(attribute=>'burn_in_iterations',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@burn_in_iterations;
}

sub sum_covariance_time {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->sum_covariance_time: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->sum_covariance_time: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->sum_covariance_time: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sum_covariance_time: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->sum_covariance_time: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sum_covariance_time = @{$self->access_any(attribute=>'sum_covariance_time',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sum_covariance_time');
# line 605 "lib/output_subs.pm" 
    # Level:  Sub problem
# line 5402 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sum_covariance_time');
	# End of Non-Dia code #

	return \@sum_covariance_time;
}

sub thetanames {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->thetanames: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->thetanames: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->thetanames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->thetanames: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->thetanames: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @thetanames = @{$self->access_any(attribute=>'thetanames',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> thetanames');
# line 730 "lib/output_subs.pm" 
    # thetanames returns the default theta parameter names, TH1, TH2 etc.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 5447 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> thetanames');
	# End of Non-Dia code #

	return \@thetanames;
}

sub thetas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY',
			'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->thetas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->thetas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->thetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->thetas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->thetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @thetas = @{$self->access_any(attribute=>'thetas',problems=>\@problems,subproblems=>\@subproblems,parameter_numbers=>\@parameter_numbers)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> thetas');
# line 737 "lib/output_subs.pm" 
    # thetas returns the theta parameter estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
# line 5494 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> thetas');
	# End of Non-Dia code #

	return \@thetas;
}

sub full_name {
	my $self = shift;
	my $full_name;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> full_name');
# line 124 "lib/output_subs.pm" 
	$full_name = $self->directory . $self->filename;
# line 5509 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> full_name');
	# End of Non-Dia code #

	return $full_name;
}

sub problem_structure {
	my $self = shift;
	my @structure;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> problem_structure');
# line 1323 "lib/output_subs.pm" 
	my $flush = 0;
	unless ( not_empty($self->problems) ) {
		# Try to read from disk
		$self -> _read_problems;
		$flush = 1;
	}
	if ( not_empty($self->problems) ) {
		for (my $problem = 0; $problem < @{$self->problems}; $problem++ ) {
	 		if ( defined $self->problems->[$problem]->subproblems ) {
				$structure[$problem] = scalar @{$self->problems->[$problem]->subproblems};
			} else {
				# This is a case when the subproblem(s) could not be read.
				$structure[$problem] = 0;
			}
		}
		$self -> flush if( $flush );
	}
# line 5540 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> problem_structure');
	# End of Non-Dia code #

	return \@structure;
}

sub parameter_significant_digits {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->parameter_significant_digits: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->parameter_significant_digits: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->parameter_significant_digits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->parameter_significant_digits: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->parameter_significant_digits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_significant_digits = @{$self->access_any(attribute=>'parameter_significant_digits',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@parameter_significant_digits;
}

sub parsing_error {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->parsing_error: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->parsing_error: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->parsing_error: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->parsing_error: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->parsing_error: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = $parm{'message'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> parsing_error');
# line 995 "lib/output_subs.pm" 
	$self->parsed_successfully( 0 );
	$self->parsing_error_message( $message );
# line 5618 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> parsing_error');
	# End of Non-Dia code #

}

sub _read_problems {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_problems');
# line 764 "lib/output_subs.pm" 
	# This is a private method, and should not be used outside
	# this file.

	my %months;
	$months{'Jan'} = 0;
	$months{'Feb'} = 1;
	$months{'Mar'} = 2;
	$months{'Apr'} = 3;
	$months{'May'} = 4;
	$months{'Jun'} = 5;
	$months{'Jul'} = 6;
	$months{'Aug'} = 7;
	$months{'Sep'} = 8;
	$months{'Oct'} = 9;
	$months{'Nov'} = 10;
	$months{'Dec'} = 11;

	my @lstfile = OSspecific::slurp_file($self-> full_name ) ;

	my $lstfile_pos = 0;

	$self->parsed_successfully(1);

	my $problem_start;
	my $problem_index = 0;
	my $success = 0;
	my $meth_counter = 0;
	my $tbln; #NONMEM table number from tag #TBLN
	my $n_previous_meth = 0;
	my $lst_version;
	my $endtime_string;
	my $starttime_string;

	#new in 3.5.10, read control stream from lst-file
	my $reading_control_stream = 0;
	my $done_reading_control_stream = 0;
	my $found_control_stream = 0;
	my $control_stream_problem_start_index;
	my $control_stream_problem_end_index;
	my @control_stream_problems;
	

	my $j = $#lstfile;
	while ( $_ = @lstfile[ $j -- ] ) {
	  if (/^\s*(Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)/){
	    $endtime_string = $_;
	    last;
	  }elsif (/^1NONLINEAR MIXED EFFECTS MODEL PROGRAM/){
	    #if we end up here the lst-file is incomplete, was no end time printed
	    #by nmfe
	    $self->lst_interrupted(1);
	    last;
	  }
	}


	while ( $_ = @lstfile[ $lstfile_pos++ ] ) {
	  if ((not defined ($starttime_string) and not defined ($problem_start)) and (/^\s*(Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)/)) {
	    $starttime_string = $_;
	    if (defined $endtime_string) {
	      $starttime_string =~ s/\s*$//; #remove trailing spaces
	      my ($wday, $mon, $mday, $tt, $zone, $year) = split(/\s+/, $starttime_string);
	      $mon = $months{$mon}; #convert to numeric
	      my ($hour, $min, $sec) = split(':',$tt);
	      my $starttime = timelocal($sec, $min, $hour, $mday, $mon, $year);

	      $endtime_string =~ s/\s*$//; #remove trailing spaces
	      ($wday, $mon, $mday, $tt, $zone, $year) = split(/\s+/, $endtime_string);
	      $mon = $months{$mon}; #convert to numeric
	      my ($hour, $min, $sec) = split(':', $tt);
	      my $endtime = timelocal($sec, $min, $hour, $mday, $mon, $year);
	      my $runtime = $endtime - $starttime; # in seconds
	      next if ($runtime == 0);
	      my $seconds = $runtime % 60;
	      my $minutes = (($runtime - $seconds) / 60) % 60;
	      my $hours = ($runtime - $seconds - 60 * $minutes) / 3600;
	      $self->runtime(sprintf "%i:%02i:%02i", $hours, $minutes, $seconds);
	    }
	  } elsif (not $done_reading_control_stream and (not $reading_control_stream) and (/^\s*\$PROB/)) {
	      #we ignore $SIZES here, not relevent for what we need in output handling
	      #found first record of control stream
	      $reading_control_stream = 1;
	      $found_control_stream = 1;
	      #store index of line
	      $control_stream_problem_start_index = $lstfile_pos;
	  } elsif (not $done_reading_control_stream and ($reading_control_stream) and (/^\s*\$PROB/)){
	      #we ignore $SIZES here, not relevent for what we need in output handling
	      #found first record of another $PROB
	      #add previous $PROB
	      $found_control_stream = 1;
	      $control_stream_problem_end_index = $lstfile_pos - 1;
	      my @control_lines = @lstfile[$control_stream_problem_start_index .. $control_stream_problem_end_index];

	      my $prob = model::problem -> new (
					directory										=> $self->directory,
					ignore_missing_files        => 1,
					ignore_missing_output_files => 1,
					prob_arr                    => \@control_lines);

	      push( @control_stream_problems, $prob );

	      #store index of line for new $PROB
	      $control_stream_problem_start_index = $lstfile_pos;
	  } elsif ((/^\s*NM\-TRAN MESSAGES/) or (/^\s*WARNINGS AND ERRORS \(IF ANY\)/) or (/^\s*License /) ) {

	      if ((not $done_reading_control_stream) and $found_control_stream) {
		  #add last control stream problem
		  $control_stream_problem_end_index = $lstfile_pos - 1;
		  my @control_lines = @lstfile[$control_stream_problem_start_index .. $control_stream_problem_end_index];

		  my $prob = model::problem -> new (
		      directory										=> $self->directory,
			    ignore_missing_files        => 1,
			    ignore_missing_output_files => 1,
			    prob_arr                    => \@control_lines);

		  push( @control_stream_problems, $prob );

		  $done_reading_control_stream = 1;
		  $reading_control_stream = 0;
	      }
	  } elsif (/^1NONLINEAR MIXED EFFECTS MODEL PROGRAM/) {

	      if ((not $done_reading_control_stream) and $found_control_stream) {
		  		#add last control stream problem
		  		$control_stream_problem_end_index = $lstfile_pos - 1;
		  		my @control_lines = @lstfile[$control_stream_problem_start_index .. $control_stream_problem_end_index];

		  		my $prob = model::problem -> new (
		      		directory                   => $self->directory,
			    		ignore_missing_files        => 1,
			    		ignore_missing_output_files => 1,
			    		prob_arr                    => \@control_lines);

		  		push( @control_stream_problems, $prob );
		  		$done_reading_control_stream = 1;
		  		$reading_control_stream = 0;
	      }

	    if (/VERSION 7/) {
	      $lst_version = 7;
	    } elsif (/VERSION VII /) {
	      $lst_version = 7;
	    } elsif (/VERSION 6/) {
	      $lst_version = 6;
	    } elsif (/VERSION VI /) {
	      $lst_version = 6;
	    } elsif (/VERSION V /) {
	      $lst_version = 5;
	    } else {
	      croak("could not read NONMEM version information from output file " . $self->filename);
	    }
	    $self->nonmem_version($lst_version);

	  } elsif (/^\s*\#METH:/) {
	      #NONMEM will print #METH also when simulation without estimation, do not count
	      # these occurences: #METH line followed by line with 1 and nothing more
	      unless ($lstfile[$lstfile_pos] =~ /^1$/) {
		  		$meth_counter++;
		  		if ($lstfile[ $lstfile_pos - 2 ] =~ /^\s*\#TBLN:\s*(.*)/) {
		      	#if previous line is #TBLN then this will help us find right table in extra output
		      	$tbln = $1;
		  		}
	      }
	  } elsif ( /^ PROBLEM NO\.:\s+\d+\s+$/ or $lstfile_pos > $#lstfile ) {
	    if ( defined $problem_start ) {
	      my $adj = 1;
	      my @problem_lstfile =	@lstfile[$problem_start .. ($lstfile_pos - $adj)];

	      #We send full raw_file, cov_file... arrays to problem object
	      #the right table number will be extracted there using $n_previous_meth
	      #or $tbln if present
	      #we skip tables that must come from restarted numbering (<= previous number),
	      #NM7 table numbering in additional output is inconsistent and we cannot handle it
	      #those numbers will be taken from lst-file instead, good enough since rare case
	      #if nm_major_version<=6 undefined arrays, okay

	      croak("Problems reading the lst-file (NONMEM output file).".
			 		" The line\n".
			 		"1NONLINEAR MIXED EFFECTS MODEL PROGRAM VERSION...\n".
			 		"was not found.") unless (defined $lst_version);
	      croak("Could not find a model file copy (control stream) at top of lst-file for problem number ".
			 ($problem_index + 1) . " in lst-file. The nmfe script normally copies the model file but PsN cannot find it.")
		  unless (defined $control_stream_problems[$problem_index]);
	      $self -> add_problem ( init_data => 
			             { lstfile		    => \@problem_lstfile,
				       ignore_missing_files => $self -> ignore_missing_files(),
				       nm_major_version     => $lst_version,
				       filename_root	    	=> $self -> filename_root(),
				       directory	    			=> $self -> directory(),
				       n_previous_meth      => $n_previous_meth,
				       table_number         => $tbln,
				       input_problem        => $control_stream_problems[$problem_index]});

	      $problem_index++;
	      @problem_lstfile = undef;
#	      $tbln = undef; leave this. Instead reread table number in problem_subs.pm
	      $success = 1;
	      $n_previous_meth = $meth_counter;

	      my @problems = @{$self->problems};

	      my $mes = $self->parsing_error_message();
	      $mes .= $problems[$#problems] -> parsing_error_message();
	      $self -> parsing_error_message( $mes );
	      $self -> parsed_successfully($self -> parsed_successfully() * $problems[$#problems] -> parsed_successfully());

	      $self -> msfo_has_terminated($self -> msfo_has_terminated() + $problems[$#problems] -> msfo_has_terminated());

	    }  #end if defined problem start
	    $problem_start = $lstfile_pos;
	  }
	}

	$self->control_stream_problems(\@control_stream_problems);
	unless( $success ) {
	  carp('Could not find a PROBLEM NO statement in "' .
			 $self -> full_name . '"' . "\n" ) unless $self->ignore_missing_files;

	  $self->parsing_error( message => 'Could not find a PROBLEM NO statement in "' . $self->full_name . '"' . "\n" );
	  $self->parsed_successfully(0);
	  return 0;
	}

	$self->parsed(1);
# line 5855 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_problems');
	# End of Non-Dia code #

}

sub initomegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->initomegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->initomegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->initomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->initomegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->initomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @initomegas = @{$self->access_any(attribute=>'initomegas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> initomegas');
# line 394 "lib/output_subs.pm" 
    # initomegas returns the initial omega values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 5900 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> initomegas');
	# End of Non-Dia code #

	return \@initomegas;
}

sub initsigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'subproblems' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output->initsigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output->initsigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output->initsigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->initsigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output->initsigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @initsigmas = @{$self->access_any(attribute=>'initsigmas',problems=>\@problems,subproblems=>\@subproblems)};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> initsigmas');
# line 402 "lib/output_subs.pm" 
    # initsigmas returns the initial sigma values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
# line 5946 libgen/output.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> initsigmas');
	# End of Non-Dia code #

	return \@initsigmas;
}

1;

