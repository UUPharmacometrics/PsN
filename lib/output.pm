package output;

# A Perl module for parsing NONMEM output files
use include_modules;
use OSspecific;
use Storable;
use Config;
use ext::Math::SigFigs;
use Data::Dumper;
use Time::Local;
use model::problem;
use array qw(:all);
use Moose;
use MooseX::Params::Validate;
use output::problem;

has 'problems' => ( is => 'rw', isa => 'ArrayRef[output::problem]' );
has 'directory' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'control_stream_problems' => ( is => 'rw', isa => 'ArrayRef' );
has 'filename_root' => ( is => 'rw', isa => 'Str' );
has 'filename' => ( is => 'rw', isa => 'Str' );
has 'nonmem_version' => ( is => 'rw', isa => 'Num' );
has 'output_id' => ( is => 'rw', isa => 'Int' );
has 'model_id' => ( is => 'rw', isa => 'Int' );
has 'ignore_missing_files' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'tablenames' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'target' => ( is => 'rw', isa => 'Str', default => 'mem' );
has 'abort_on_fail' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'parsed_successfully' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'msfo_has_terminated' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'runtime' => ( is => 'rw', isa => 'Str' );
has 'lst_interrupted' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'parsed' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'parsing_error_message' => ( is => 'rw', isa => 'Str' );

# {{{ description

# No method, just documentation
    # The PsN output class is built to ease the (often trivial,
    # but still time consuming) task of gathering and structuring the
    # information contained in NONMEM output files. The major parts of a
    # NONMEM output file are parsed and in the L</methods> section
    # you can find a listing of the routines that are available.

# }}} description

# {{{ synopsis

    #   use output;
    #   
    #   my $out_obj = output -> new ( filename => 'run1.lst' );
    #   
    #   my @thetas = @{$out_obj -> thetas};
    #   my @omegas = @{$out_obj -> omegas};
    #   my @ofvs   = @{$out_obj -> ofvs};

# }}} synopsis

# {{{ see_also

    # =begin html
    #
    # <a HREF="data.html">data</a>, <a HREF="model.html">model</a>
    # <a HREF="tool/modelfit.html">tool::modelfit</a>,
    # <a HREF="tool.html">tool</a>
    #
    # =end html
    #
    # =begin man
    #
    # data, model, tool::modelfit, tool
    #
    # =end man

# }}} see_also

		
		
sub BUILD
{
	my $self  = shift;

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

	carp("Initiating new\tNM::output object from file " . $self->filename );

	if ( defined $self->filename and $self->filename ne '' ) {
		my $name;
		my $directory;
		( $directory, $name ) = OSspecific::absolute_path( $self->directory, $self->filename );
		$self->directory($directory);
		$self->filename($name);

		if ( $name =~ /\.lst$/ ) {
			$name =~ s/\.lst$//;
			$self->filename_root($name);
		} elsif(  $name =~ /\.res$/ ) {
			$name =~ s/\.res$//;
			$self->filename_root($name);
		}

		if ( -e $self->full_name ) { 
			if ($self->target eq 'mem') {
				$self->_read_problems;
			}
		} else {
			croak("The NONMEM output file " . $self -> full_name . " does not exist" )
				unless $self->ignore_missing_files;
		}
	} else {
		croak("No filename specified or filename equals empty string!" );
		$self->filename('tempfile');
	}
}

sub add_problem
{
	my ($self, %parm) = validated_hash(\@_, 
									   init_data => {isa => 'Any', optional => 0}
		);
	$self->problems([]) unless defined $self->problems;
	push( @{$self->problems}, output::problem->new( %{$parm{'init_data'}} ) );

}

sub copy
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  filename => { isa => 'Str', optional => 1 }
		);
	my $filename = $parm{'filename'};
	my $new_output;

	$new_output = Storable::dclone( $self );

	return $new_output;
}

sub access_any
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  attribute => { isa => 'Str', optional => 0 },
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my $attribute = $parm{'attribute'};
	my @return_value;
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

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
#	  croak("Trying to access output object, that have no data on file(" . $self->full_name . ") or in memory" );
		print "\nTrying to access output object, that have no data on file(" . $self->full_name . ") or in memory\n" ;
		return [];
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
	return \@return_value;
}

sub high_correlations
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  limit => { isa => 'Num', default => 0.95, optional => 1 },
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my $limit = $parm{'limit'};
	my @high_correlations;
	my @found_correlations;
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

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
	return \@high_correlations ,\@found_correlations;
}

sub large_standard_errors
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  theta_cv_limit => { isa => 'Num', default => 0.95, optional => 1 },
							  omega_cv_limit => { isa => 'Num', default => 0.95, optional => 1 },
							  sigma_cv_limit => { isa => 'Num', default => 0.95, optional => 1 },
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my $theta_cv_limit = $parm{'theta_cv_limit'};
	my $omega_cv_limit = $parm{'omega_cv_limit'};
	my $sigma_cv_limit = $parm{'sigma_cv_limit'};
	my @large_standard_errors_names;
	my @large_standard_errors_values;
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : () ;
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

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

	return \@large_standard_errors_names ,\@large_standard_errors_values;
}

sub near_bounds
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  zero_limit => { isa => 'Num', default => 0.01, optional => 1 },
							  significant_digits => { isa => 'Int', default => 2, optional => 1 },
							  off_diagonal_sign_digits => { isa => 'Int', default => 2, optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my $zero_limit = $parm{'zero_limit'};
	my $significant_digits = $parm{'significant_digits'};
	my $off_diagonal_sign_digits = $parm{'off_diagonal_sign_digits'};
	my @near_bounds;
	my @found_bounds;
	my @found_estimates;
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

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
	return \@near_bounds ,\@found_bounds ,\@found_estimates;
}

sub comegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @comegas = @{$self->access_any(attribute => 'comegas', problems => \@problems, subproblems => \@subproblems)};

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
	return \@comegas;
}

sub condition_number
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @condition_number = @{$self->access_any(attribute=>'condition_number',problems=>\@problems,subproblems=>\@subproblems)};

    # condition_number returns the 2-norm condition number for the correlation matrix, i.e.
    # the largest eigen value divided by the smallest.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@condition_number;
}

sub covariance_step_run
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @covariance_step_run = @{$self->access_any(attribute=>'covariance_step_run',problems=>\@problems,subproblems=>\@subproblems)};

	# Returns 1 if the covariance step was run, 0 otherwise. See
	# L</comegas> for details.
	#
	# Level:  Problem
	return \@covariance_step_run;
}

sub covariance_step_successful
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @covariance_step_successful = @{$self->access_any(attribute=>'covariance_step_successful',problems=>\@problems,subproblems=>\@subproblems)};

    # Returns 1 if the covariance step was successful, 0
    # otherwise. See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem

	return \@covariance_step_successful;
}

sub estimate_near_boundary
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @estimate_near_boundary = @{$self->access_any(attribute=>'estimate_near_boundary',problems=>\@problems,subproblems=>\@subproblems)};


	return \@estimate_near_boundary;
}

sub covariance_step_warnings
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @covariance_step_warnings = @{$self->access_any(attribute=>'covariance_step_warnings',problems=>\@problems,subproblems=>\@subproblems)};


    # Returns 0 if there were no warnings or errors printed during the
    # covariance step, 1 otherwise. See L</comegas> for details on the
    # method arguments.
    #
    # Level:  Sub problem
	return \@covariance_step_warnings;
}

sub s_matrix_singular
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @s_matrix_singular = @{$self->access_any(attribute=>'s_matrix_singular',problems=>\@problems,subproblems=>\@subproblems)};


	return \@s_matrix_singular;
}

sub csigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @csigmas = @{$self->access_any(attribute=>'csigmas',problems=>\@problems,subproblems=>\@subproblems)};

    # csigmas returns the standard deviation for elements on the
    # diagonal and correlation coefficients for off-diagonal elements.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in sigmacoordval.
	return \@csigmas;
}

sub cvsethetas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvsethetas = @{$self->access_any(attribute=>'cvsethetas',problems=>\@problems,subproblems=>\@subproblems)};

    # cvsethetas returns the relative standard error for the thetas, i.e. SE/estimate.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in thetacoordval.
	return \@cvsethetas;
}

sub cvseomegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvseomegas = @{$self->access_any(attribute=>'cvseomegas',problems=>\@problems,subproblems=>\@subproblems)};

    # cvseomegas returns the relative standard error for the omegas, i.e. SE/estimate.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in omegacoordval.
	return \@cvseomegas;
}

sub cvsesigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvsesigmas = @{$self->access_any(attribute=>'cvsesigmas',problems=>\@problems,subproblems=>\@subproblems)};

    # cvsesigmas returns the relative standard error for the sigmas, i.e. SE/estimate.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in sigmacoordval.
	return \@cvsesigmas;
}

sub shrinkage_eta
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @shrinkage_eta = @{$self->access_any(attribute=>'shrinkage_eta',problems=>\@problems,subproblems=>\@subproblems)};


	return \@shrinkage_eta;
}

sub shrinkage_eps
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @shrinkage_eps = @{$self->access_any(attribute=>'shrinkage_eps',problems=>\@problems,subproblems=>\@subproblems)};


	return \@shrinkage_eps;
}

sub eigens
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @eigens = @{$self->access_any(attribute=>'eigens',problems=>\@problems,subproblems=>\@subproblems)};

    # eigens returns the eigen values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@eigens;
}

sub etabar
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @etabar = @{$self->access_any(attribute=>'etabar',problems=>\@problems,subproblems=>\@subproblems)};

    # etabar returns the ETABAR estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@etabar;
}

sub feval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @feval = @{$self->access_any(attribute=>'feval',problems=>\@problems,subproblems=>\@subproblems)};


    # feval returns the number of function evaluations.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@feval;
}

sub finalparam
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @finalparam = @{$self->access_any(attribute=>'finalparam',problems=>\@problems,subproblems=>\@subproblems)};


    # finalparam returns the final parameter vector as it appears in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@finalparam;
}

sub final_gradients
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @final_gradients = @{$self->access_any(attribute=>'final_gradients',problems=>\@problems,subproblems=>\@subproblems)};


    # final_gradients returns the final gradient vector as it appears in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@final_gradients;
}

sub fixedomegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problem_index => { isa => 'Int', optional => 1 }
		);
	my $problem_index = $parm{'problem_index'};
	my @fixedomegas;

	# fixedomegas returns the a vector of booleans; 1's if
	# the parameters were fixed during the model fit, 0's
	# if they were not.

	if ( defined $self->problems and defined $self->problems->[$problem_index]
		 and defined $self->problems->[$problem_index]->fixedomegas()) {
		@fixedomegas = @{$self->problems->[$problem_index]->fixedomegas()};
	}

	return \@fixedomegas;
}

sub estimated_sigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @estimatedsigmas = @{$self->access_any(attribute=>'estimatedsigmas',problems=>\@problems,subproblems=>\@subproblems)};


	return \@estimatedsigmas;
}

sub estimated_thetas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @estimatedthetas = @{$self->access_any(attribute=>'estimatedthetas',problems=>\@problems,subproblems=>\@subproblems)};


	return \@estimatedthetas;
}

sub estimated_omegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @estimatedomegas = @{$self->access_any(attribute=>'estimatedomegas',problems=>\@problems,subproblems=>\@subproblems)};


	return \@estimatedomegas;
}

sub est_thetanames
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @est_thetanames = @{$self->access_any(attribute=>'est_thetanames',problems=>\@problems,subproblems=>\@subproblems)};

    # return thetanames from output_matrix_headers
	return \@est_thetanames;
}

sub est_omeganames
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @est_omeganames = @{$self->access_any(attribute=>'est_omeganames',problems=>\@problems,subproblems=>\@subproblems)};

    # return omeganames from output_matrix headers
	return \@est_omeganames;
}

sub est_sigmanames
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @est_sigmanames = @{$self->access_any(attribute=>'est_sigmanames',problems=>\@problems,subproblems=>\@subproblems)};

    # return sigmanames from output_matrix headers
	return \@est_sigmanames;
}

sub fixedsigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problem_index => { isa => 'Int', optional => 1 }
		);
	my $problem_index = $parm{'problem_index'};
	my @fixedsigmas;

	# fixedsigmas returns a vector of booleans; 1's if
	# the parameters were fixed during the model fit, 0's
	# if they were not.

	if ( defined $self->problems and defined $self->problems->[$problem_index]
		 and defined $self->problems->[$problem_index]->fixedsigmas()) {
		@fixedsigmas = @{$self->problems->[$problem_index]->fixedsigmas()};
	}
	return \@fixedsigmas;
}

sub fixedthetas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problem_index => { isa => 'Int', optional => 1 }
		);
	my $problem_index = $parm{'problem_index'};
	my @fixedthetas;

	# fixedthetas returns a vector of booleans; 1's if
	# the parameters were fixed during the model fit, 0's
	# if they were not.

	if ( defined $self->problems and defined $self->problems->[$problem_index]
		 and defined $self->problems->[$problem_index]->fixedthetas()) {
		@fixedthetas = @{$self->problems->[$problem_index]->fixedthetas()};
	}
	return \@fixedthetas;
}

sub flush
{
	my $self = shift;

	# flush is not an accessor method. As its name implies it flushes the
	# output objects memory by setting the I<problems> attribute to undef.
	# This method can be useful when many output objects are handled and
	# the memory is limited.

	# Flushes the object to save memory. 

	$self->problems([]);
}

sub funcevalpath
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @funcevalpath = @{$self->access_any(attribute=>'funcevalpath',problems=>\@problems,subproblems=>\@subproblems)};

    # funcevalpath returns the number of function evaluations for each printed iteration in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@funcevalpath;
}

sub gradient_path
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @gradient_path = @{$self->access_any(attribute=>'gradient_path',problems=>\@problems,subproblems=>\@subproblems)};


    # gradient_path returns the gradients for each printed iteration in the monitoring of search section (returns a matrix for each sub problem).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@gradient_path;
}

sub have_output
{
	my $self = shift;
	my $return_value = 0;

	# have_output returns true if the output files exits or if there
	# is output data in memory.

	if ( -e $self->full_name || not_empty($self->problems) ) {
		return 1;
	} else {
		return 0;
	}

	return $return_value;
}

sub have_user_defined_prior
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @user_defined_prior = @{$self->access_any(attribute=>'user_defined_prior',problems=>\@problems,subproblems=>\@subproblems)};


	return \@user_defined_prior;
}

sub initgrad
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @initgrad = @{$self->access_any(attribute=>'initgrad',problems=>\@problems,subproblems=>\@subproblems)};

    # initgrad returns the initial gradient vector in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@initgrad;
}

sub initthetas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @initthetas = @{$self->access_any(attribute=>'initthetas',problems=>\@problems,subproblems=>\@subproblems)};

    # initthetas returns the initial theta values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
	return \@initthetas;
}

sub omega_indexes
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @omega_indexes = @{$self->access_any(attribute=>'omega_indexes',problems=>\@problems,subproblems=>\@subproblems)};


	return \@omega_indexes;
}

sub sigma_indexes
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sigma_indexes = @{$self->access_any(attribute=>'sigma_indexes',problems=>\@problems,subproblems=>\@subproblems)};


	return \@sigma_indexes;
}

sub iternum
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @iternum = @{$self->access_any(attribute=>'iternum',problems=>\@problems,subproblems=>\@subproblems)};


	return \@iternum;
}

sub labels
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  parameter_type => { isa => 'Str', optional => 1 }
		);
	my $parameter_type = $parm{'parameter_type'};
	my @labels;

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
	return \@labels;
}

sub nind
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @nind = @{$self->access_any(attribute=>'nind',problems=>\@problems,subproblems=>\@subproblems)};

    # nind returns the number of individuals.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Problem

	return \@nind;
}

sub nobs
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @nobs = @{$self->access_any(attribute=>'nobs',problems=>\@problems,subproblems=>\@subproblems)};


    # nobs returns the number of observations.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Problem
	return \@nobs;
}

sub npofv
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @npofv = @{$self->access_any(attribute=>'npofv',problems=>\@problems,subproblems=>\@subproblems)};


    # npofv returns the non-parametric objective function value.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@npofv;
}

sub nrecs
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @nrecs = @{$self->access_any(attribute=>'nrecs',problems=>\@problems,subproblems=>\@subproblems)};


    # nrecs returns the number of records.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Problem
	return \@nrecs;
}

sub npomegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @npomegas = @{$self->access_any(attribute=>'npomegas',problems=>\@problems,subproblems=>\@subproblems)};


    # npomegas returns the non-parametric omega estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@npomegas;
}

sub npetabars
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @npetabars = @{$self->access_any(attribute=>'npetabars',problems=>\@problems,subproblems=>\@subproblems)};


    # npthetas returns the non-parametric theta estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@npetabars;
}

sub nth
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @nth = @{$self->access_any(attribute=>'nth',problems=>\@problems,subproblems=>\@subproblems)};

    # nth returns the number of thetas.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem

    croak("This function cannot be used, is only a placeholder. Rewrite program.");
	return \@nth;
}

sub ofvpath
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @ofvpath = @{$self->access_any(attribute=>'ofvpath',problems=>\@problems,subproblems=>\@subproblems)};

    # ofvpath returns the objective [function] values in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@ofvpath;
}

sub get_single_value
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  attribute => { isa => 'Str', optional => 0 },
							  problem_index => { isa => 'Int', default => 0, optional => 1 },
							  subproblem_index => { isa => 'Int', default => 0, optional => 1 }
		);
	my $attribute = $parm{'attribute'};
	my $problem_index = $parm{'problem_index'};
	my $subproblem_index = $parm{'subproblem_index'};
	my $return_value;

	my $arr;
	$return_value = undef;
	if ($self->can($attribute)) {
		$arr = $self->$attribute(problems => [($problem_index + 1)], subproblems => [($subproblem_index + 1)]);
		if (defined $arr->[0]) {
			if (ref $arr->[0] eq "ARRAY"){
				$return_value=$arr->[0]->[0];
			}else{
				$return_value=$arr->[0];
			}
		} else {
			1;
		}
	} elsif ( ($attribute eq 'estimation_step_run') or
			  ($attribute eq 'estimation_step_initiated')
		) {
# removed support ($attribute eq 'user_defined_prior') 
#		or ($attribute eq 'omega_block_structure_type')
#		or ($attribute eq 'sigma_block_structure_type')
#		or ($attribute eq 'omega_block_sets')
#		or ($attribute eq 'sigma_block_sets')
		$arr = $self->access_any(attribute => $attribute,
								 problems => [($problem_index + 1)],
								 subproblems => [(1)]);
		if (defined $arr->[0]) {
			if (ref $arr->[0] eq "ARRAY"){
				$return_value=$arr->[0]->[0];
			}else{
				$return_value=$arr->[0];
			}
		}
	} else {
		croak("unknown attribute $attribute");
	}
	return $return_value;
}

sub ofv
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @ofv = @{$self->access_any(attribute=>'ofv',problems=>\@problems,subproblems=>\@subproblems)};


    # ofv returns the objective function value(s).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@ofv;
}

sub dic
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @dic = @{$self->access_any(attribute=>'dic',problems=>\@problems,subproblems=>\@subproblems)};


    # Level:  Sub problem
	return \@dic;
}

sub have_omegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @have_omegas = @{$self->access_any(attribute=>'have_omegas',problems=>\@problems,subproblems=>\@subproblems)};


	return \@have_omegas;
}

sub have_sigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @have_sigmas = @{$self->access_any(attribute=>'have_sigmas',problems=>\@problems,subproblems=>\@subproblems)};


	return \@have_sigmas;
}

sub omega_block_structure
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @omega_block_structure = @{$self->access_any(attribute=>'omega_block_structure',problems=>\@problems,subproblems=>\@subproblems)};

	# omega_block_structure returns the block structure for
    # the omega parameters in a lower triangular matrix form
    # as in the OMEGA HAS BLOCK FORM section in the NONMEM output file.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");

	return \@omega_block_structure;
}

sub omegacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @omegacoordval = @{$self->access_any(attribute=>'omegacoordval',problems=>\@problems,subproblems=>\@subproblems)};
    # omegacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. OMEGA(1,1), OMEGA(1,2) etc as keys
    # and parameter estimates as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem

	return \@omegacoordval;
}

sub seomegacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @seomegacoordval = @{$self->access_any(attribute=>'seomegacoordval',problems=>\@problems,subproblems=>\@subproblems)};

    # seomegacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. OMEGA(1,1), OMEGA(1,2) etc as keys
    # and parameter standard errors as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@seomegacoordval;
}

sub omeganames
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @omeganames = @{$self->access_any(attribute=>'omeganames',problems=>\@problems,subproblems=>\@subproblems)};


    # omeganames returns the default parameter names, OMEGA(1,1) etc for stored values
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@omeganames;
}

sub cvseomegacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvseomegacoordval = @{$self->access_any(attribute=>'cvseomegacoordval',problems=>\@problems,subproblems=>\@subproblems)};


	return \@cvseomegacoordval;
}

sub cvsesigmacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvsesigmacoordval = @{$self->access_any(attribute=>'cvsesigmacoordval',problems=>\@problems,subproblems=>\@subproblems)};


	return \@cvsesigmacoordval;
}

sub covariance_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @covariance_matrix = @{$self->access_any(attribute=>'covariance_matrix',problems=>\@problems,subproblems=>\@subproblems)};


	return \@covariance_matrix;
}

sub cvsethetacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @cvsethetacoordval = @{$self->access_any(attribute=>'cvsethetacoordval',problems=>\@problems,subproblems=>\@subproblems)};


	return \@cvsethetacoordval;
}

sub comegacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @comegacoordval = @{$self->access_any(attribute=>'comegacoordval',problems=>\@problems,subproblems=>\@subproblems)};


	return \@comegacoordval;
}

sub csigmacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @csigmacoordval = @{$self->access_any(attribute=>'csigmacoordval',problems=>\@problems,subproblems=>\@subproblems)};


	return \@csigmacoordval;
}

sub omegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @omegas = @{$self->access_any(attribute=>'omegas',problems=>\@problems,subproblems=>\@subproblems,parameter_numbers=>\@parameter_numbers)};


    # omegas returns the omega parameter estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@omegas;
}

sub parameter_path
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_path = @{$self->access_any(attribute=>'parameter_path',problems=>\@problems,subproblems=>\@subproblems)};


    # parameter_path returns the (normalized) parameter estimates for each iteration in the monitoring of search section (Matrix returned).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@parameter_path;
}

sub pval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @pval = @{$self->access_any(attribute=>'pval',problems=>\@problems,subproblems=>\@subproblems)};


    # pval returns the P VAL (reflects the probability that the etas are not centered around zero).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@pval;
}

sub raw_covmatrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_covmatrix = @{$self->access_any(attribute=>'raw_covmatrix',problems=>\@problems,subproblems=>\@subproblems)};


    # raw_covmatrix returns the (raw) covariance matrix including empty matrix elements marked as '.........'.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@raw_covmatrix;
}

sub inverse_covariance_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @inverse_covariance_matrix = @{$self->access_any(attribute=>'inverse_covariance_matrix',problems=>\@problems,subproblems=>\@subproblems)};


    # inverse_covariance_matrix returns the inverse covariance matrix 
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@inverse_covariance_matrix;
}

sub raw_cormatrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_cormatrix = @{$self->access_any(attribute=>'raw_cormatrix',problems=>\@problems,subproblems=>\@subproblems)};


    # raw_cormatrix returns the (raw) correlation matrix including empty matrix elements marked as '.........'.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@raw_cormatrix;
}

sub correlation_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @correlation_matrix = @{$self->access_any(attribute=>'correlation_matrix',problems=>\@problems,subproblems=>\@subproblems)};


	return \@correlation_matrix;
}

sub output_matrix_headers
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @output_matrix_headers = @{$self->access_any(attribute=>'output_matrix_headers',problems=>\@problems,subproblems=>\@subproblems)};


	return \@output_matrix_headers;
}

sub raw_omegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_omegas = @{$self->access_any(attribute=>'raw_omegas',problems=>\@problems,subproblems=>\@subproblems)};


    # raw_omegas returns the (raw) omegas.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
	return \@raw_omegas;
}

sub raw_seomegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_seomegas = @{$self->access_any(attribute=>'raw_seomegas',problems=>\@problems,subproblems=>\@subproblems)};


    # raw_seomegas returns the (raw) omega standard error estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
	return \@raw_seomegas;
}

sub raw_sesigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_sesigmas = @{$self->access_any(attribute=>'raw_sesigmas',problems=>\@problems,subproblems=>\@subproblems)};


    # raw_sesigmas returns the (raw) sigma standard error estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
	return \@raw_sesigmas;
}

sub raw_sigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_sigmas = @{$self->access_any(attribute=>'raw_sigmas',problems=>\@problems,subproblems=>\@subproblems)};


    # raw_sigmas returns the (raw) sigmas.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
	return \@raw_sigmas;
}

sub raw_tmatrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @raw_tmatrix = @{$self->access_any(attribute=>'raw_tmatrix',problems=>\@problems,subproblems=>\@subproblems)};


    # raw_tmatrix returns the (raw) T-matrix.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@raw_tmatrix;
}

sub seomegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @seomegas = @{$self->access_any(attribute=>'seomegas',problems=>\@problems,subproblems=>\@subproblems)};

    # seomegas returns the omega standard error estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@seomegas;
}

sub sesigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @sesigmas = @{$self->access_any(attribute=>'sesigmas',problems=>\@problems,subproblems=>\@subproblems)};
#    # sesigmas returns the sigma standard error estimates.
#    # See L</comegas> for details of the method arguments.
#    #
#    # Level:  Sub problem

	return \@sesigmas;
}

sub sethetas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @sethetas = @{$self->access_any(attribute=>'sethetas',problems=>\@problems,subproblems=>\@subproblems)};

#    # sethetas returns the theta standard error estimates.
#    # See L</comegas> for details of the method arguments.
#    #
#    # Level:  Sub problem
	return \@sethetas;
}

sub significant_digits
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @significant_digits = @{$self->access_any(attribute=>'significant_digits',problems=>\@problems,subproblems=>\@subproblems)};

    # significant_digits returns the number of significant digits for the model fit.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@significant_digits;
}

sub sigma_block_structure
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sigma_block_structure = @{$self->access_any(attribute=>'sigma_block_structure',problems=>\@problems,subproblems=>\@subproblems)};

    # sigma_block_structure returns the block structure for
    # the sigma parameters in a lower triangular matrix form
    # as in the sigma HAS BLOCK FORM section in the NONMEM output file.
    # See L</csigmas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
	return \@sigma_block_structure;
}

sub sigmacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sigmacoordval = @{$self->access_any(attribute=>'sigmacoordval',problems=>\@problems,subproblems=>\@subproblems)};

    # sigmacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. SIGMA(1,1), SIGMA(1,2) etc as keys
    # and parameter estimates as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@sigmacoordval;
}

sub sesigmacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sesigmacoordval = @{$self->access_any(attribute=>'sesigmacoordval',problems=>\@problems,subproblems=>\@subproblems)};

    # sesigmacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. SIGMA(1,1), SIGMA(1,2) etc as keys
    # and parameter standard errors as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@sesigmacoordval;
}

sub sigmanames
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sigmanames = @{$self->access_any(attribute=>'sigmanames',problems=>\@problems,subproblems=>\@subproblems)};


    # sigmanames returns the default parameter names, i.e. SI1, SI1_2, SI2 etc.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@sigmanames;
}

sub sigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @sigmas = @{$self->access_any(attribute=>'sigmas',problems=>\@problems,subproblems=>\@subproblems,parameter_numbers=>\@parameter_numbers)};

    # sigmas returns the sigma parameter estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@sigmas;
}

sub simulationstep
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @simulationstep = @{$self->access_any(attribute=>'simulationstep',problems=>\@problems,subproblems=>\@subproblems)};

    # simulationstep returns a boolean value 1 or 0, reflecting
    # whether a simulation was performed or not.  See L</comegas> for
    # Details of the method arguments.
    #
    # Level:  Sub Problem
	return \@simulationstep;
}

sub minimization_successful
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @minimization_successful = @{$self->access_any(attribute=>'minimization_successful',problems=>\@problems,subproblems=>\@subproblems)};

    # minimization_successful returns a boolean value 1 or 0,
    # reflecting whether the minimization was successful or not.  See
    # L</comegas> for details of the method arguments.
    #
    # Level:  Sub Problem
	return \@minimization_successful;
}

sub upper_omega_bounds
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @upper_omega_bounds = @{$self->access_any(attribute=>'upper_omega_bounds',problems=>\@problems,subproblems=>\@subproblems)};


	return \@upper_omega_bounds;
}

sub lower_omega_bounds
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @lower_omega_bounds = @{$self->access_any(attribute=>'lower_omega_bounds',problems=>\@problems,subproblems=>\@subproblems)};


	return \@lower_omega_bounds;
}

sub upper_sigma_bounds
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @upper_sigma_bounds = @{$self->access_any(attribute=>'upper_sigma_bounds',problems=>\@problems,subproblems=>\@subproblems)};


	return \@upper_sigma_bounds;
}

sub lower_sigma_bounds
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @lower_sigma_bounds = @{$self->access_any(attribute=>'lower_sigma_bounds',problems=>\@problems,subproblems=>\@subproblems)};


	return \@lower_sigma_bounds;
}

sub upper_theta_bounds
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problem_index => { isa => 'Int', optional => 1 }
		);
	my $problem_index = $parm{'problem_index'};
	my @upper_theta_bounds;

# returns a vector of numbers

	if ( defined $self->problems and defined $self->problems->[$problem_index]
		 and defined $self->problems->[$problem_index]->upper_theta_bounds()) {
		@upper_theta_bounds = @{$self->problems->[$problem_index]->upper_theta_bounds()};
	}

	return \@upper_theta_bounds;
}

sub lower_theta_bounds
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problem_index => { isa => 'Int', optional => 1 }
		);
	my $problem_index = $parm{'problem_index'};
	my @lower_theta_bounds;

	# returns a vector of numbers

	if ( defined $self->problems and defined $self->problems->[$problem_index]
		 and defined $self->problems->[$problem_index]->lower_theta_bounds()) {
		@lower_theta_bounds = @{$self->problems->[$problem_index]->lower_theta_bounds()};
	}
	return \@lower_theta_bounds;
}

sub final_zero_gradients
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @final_zero_gradients = @{$self->access_any(attribute=>'final_zero_gradients',problems=>\@problems,subproblems=>\@subproblems)};


	return \@final_zero_gradients;
}

sub hessian_reset
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @hessian_reset = @{$self->access_any(attribute=>'hessian_reset',problems=>\@problems,subproblems=>\@subproblems)};


	return \@hessian_reset;
}

sub zero_gradients
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @zero_gradients = @{$self->access_any(attribute=>'zero_gradients',problems=>\@problems,subproblems=>\@subproblems)};


	return \@zero_gradients;
}

sub rounding_errors
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @rounding_errors = @{$self->access_any(attribute=>'rounding_errors',problems=>\@problems,subproblems=>\@subproblems)};


	return \@rounding_errors;
}

sub minimization_message
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @minimization_message = @{$self->access_any(attribute=>'minimization_message',problems=>\@problems,subproblems=>\@subproblems)};

    # minimization_message returns the minimization message, i.e
    #   MINIMIZATION SUCCESSFUL... 
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@minimization_message;
}

sub thetacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @thetacoordval = @{$self->access_any(attribute=>'thetacoordval',problems=>\@problems,subproblems=>\@subproblems)};

    # thetacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. THETA1, THETA2 etc as keys
    # and parameter estimates as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@thetacoordval;
}

sub sethetacoordval
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sethetacoordval = @{$self->access_any(attribute=>'sethetacoordval',problems=>\@problems,subproblems=>\@subproblems)};

    # sethetacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. THETA1, THETA2 etc as keys
    # and parameter standard errors as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@sethetacoordval;
}

sub sum_estimation_time
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sum_estimation_time = @{$self->access_any(attribute=>'sum_estimation_time',problems=>\@problems,subproblems=>\@subproblems)};


    # Level:  Sub problem
	return \@sum_estimation_time;
}

sub burn_in_convergence
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @burn_in_convergence = @{$self->access_any(attribute=>'burn_in_convergence',problems=>\@problems,subproblems=>\@subproblems)};


	return \@burn_in_convergence;
}

sub burn_in_iterations
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @burn_in_iterations = @{$self->access_any(attribute=>'burn_in_iterations',problems=>\@problems,subproblems=>\@subproblems)};


	return \@burn_in_iterations;
}

sub sum_covariance_time
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @sum_covariance_time = @{$self->access_any(attribute=>'sum_covariance_time',problems=>\@problems,subproblems=>\@subproblems)};


    # Level:  Sub problem
	return \@sum_covariance_time;
}

sub thetanames
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @thetanames = @{$self->access_any(attribute=>'thetanames',problems=>\@problems,subproblems=>\@subproblems)};


    # thetanames returns the default theta parameter names, TH1, TH2 etc.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@thetanames;
}

sub thetas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
							  parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @thetas = @{$self->access_any(attribute=>'thetas',problems=>\@problems,subproblems=>\@subproblems,parameter_numbers=>\@parameter_numbers)};


    # thetas returns the theta parameter estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
	return \@thetas;
}

sub full_name
{
	my $self = shift;

	return $self->directory . $self->filename;
}

sub problem_structure
{
	my $self = shift;
	my @structure;

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

	return \@structure;
}

sub parameter_significant_digits
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_significant_digits = @{$self->access_any(attribute=>'parameter_significant_digits',problems=>\@problems,subproblems=>\@subproblems)};


	return \@parameter_significant_digits;
}

sub parsing_error
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  message => { isa => 'Str', optional => 1 }
		);
	my $message = $parm{'message'};

	$self->parsed_successfully( 0 );
	$self->parsing_error_message( $message );
}

sub _read_problems
{
	my $self = shift;

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
	while ( $_ = $lstfile[ $j -- ] ) {
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


	while ( $_ = $lstfile[ $lstfile_pos++ ] ) {
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
				($hour, $min, $sec) = split(':', $tt);
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
			$control_stream_problem_start_index = $lstfile_pos-1; #must have -1 here to get $PROB
		} elsif (not $done_reading_control_stream and ($reading_control_stream) and (/^\s*\$PROB/)){
			#we ignore $SIZES here, not relevent for what we need in output handling
			#found first record of another $PROB
			#add previous $PROB
			$found_control_stream = 1;
			$control_stream_problem_end_index = $lstfile_pos - 2; #must be -2 here otherwise get one too many lines
			my @control_lines = @lstfile[$control_stream_problem_start_index .. $control_stream_problem_end_index];
#		  print "found new \$PROB\n";
#		  print join(" \n",@control_lines)."\n";

			my $prob = model::problem -> new (
				directory										=> $self->directory,
				ignore_missing_files        => 1,
				ignore_missing_output_files => 1,
				prob_arr                    => \@control_lines);

			push( @control_stream_problems, $prob );

			#store index of line for new $PROB
			$control_stream_problem_start_index = $lstfile_pos-1; #must be -1 here to get new $PROB
		} elsif ((/^\s*NM\-TRAN MESSAGES/) or (/^\s*WARNINGS AND ERRORS \(IF ANY\)/) or (/^\s*License /) ) {

#		  print "Found nmtran messages or warnings or license\n";
			if ((not $done_reading_control_stream) and $found_control_stream) {
				#add last control stream problem
				$control_stream_problem_end_index = $lstfile_pos - 2; #must have -2 so do not get message line
				my @control_lines = @lstfile[$control_stream_problem_start_index .. $control_stream_problem_end_index];
#			  print "add control stream\n";
#			  print join("\n",@control_lines)."\n";

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

			if ((not $done_reading_control_stream) and $found_control_stream) {
		  		#add last control stream problem
		  		$control_stream_problem_end_index = $lstfile_pos - 2; #have not verified that 2 is ok here, infer from analogy to above cases
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


		} elsif (/^\s*\#METH:/) {
			#NONMEM will print #METH also when simulation without estimation, do not count
			# these occurences: #METH line followed by line with 1 and nothing more
			unless ($lstfile[$lstfile_pos] =~ /^1\s*$/) {
				$meth_counter++;
				if ($lstfile[ $lstfile_pos - 2 ] =~ /^\s*\#TBLN:\s*([0-9]+)/) {
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

				if (not defined $lst_version){
					print "\nProblems reading the lst-file (NONMEM output file).".
						" The line\n".
						"1NONLINEAR MIXED EFFECTS MODEL PROGRAM VERSION...\n".
						"was not found.\n";
					$self -> parsed_successfully(0);
					my $mes = $self->parsing_error_message();
					$mes .= ' lst-file corrupted, could not find line 1NONLINEAR MIXED EFFECTS MODEL PROGRAM VERSION... ';
					$self -> parsing_error_message( $mes );
					return 0;
				}

				if (not defined $control_stream_problems[$problem_index]){
					print "\nCould not find a model file copy (control stream) at top of lst-file for problem number ".
						($problem_index + 1) . " in lst-file\n".$self->full_name.
						"\nThe nmfe script normally copies the model file but PsN cannot find it.\n";
					$self -> parsed_successfully(0);
					my $mes = $self->parsing_error_message();
					$mes .= ' lst-file corrupted, could not find control stream copy for all PROBLEM NO ';
					$self -> parsing_error_message( $mes );
				}else{
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
					my @problems = @{$self->problems};
					
					my $mes = $self->parsing_error_message();
					$mes .= $problems[$#problems] -> parsing_error_message();
					$self -> parsing_error_message( $mes );
					$self -> parsed_successfully($self -> parsed_successfully() * $problems[$#problems] -> parsed_successfully());

					$self -> msfo_has_terminated($self -> msfo_has_terminated() + $problems[$#problems] -> msfo_has_terminated());
				}		  
				@problem_lstfile = undef;
#	      $tbln = undef; leave this. Instead reread table number in problem_subs.pm
				$success = 1;
				$n_previous_meth = $meth_counter;
				
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
}

sub initomegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @initomegas = @{$self->access_any(attribute=>'initomegas',problems=>\@problems,subproblems=>\@subproblems)};
    # initomegas returns the initial omega values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");

	return \@initomegas;
}

sub initsigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  problems => { isa => 'ArrayRef[Int]', optional => 1 },
							  subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
		);
	my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @initsigmas = @{$self->access_any(attribute=>'initsigmas',problems=>\@problems,subproblems=>\@subproblems)};

    # initsigmas returns the initial sigma values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
	return \@initsigmas;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
