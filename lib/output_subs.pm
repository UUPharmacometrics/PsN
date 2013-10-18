# {{{ include

start include statements
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
end include statements

# }}} include statements

# {{{ description

# No method, just documentation
start description
    # The PsN output class is built to ease the (often trivial,
    # but still time consuming) task of gathering and structuring the
    # information contained in NONMEM output files. The major parts of a
    # NONMEM output file are parsed and in the L</methods> section
    # you can find a listing of the routines that are available.
end description

# }}} description

# {{{ synopsis

start synopsis
    #   use output;
    #   
    #   my $out_obj = output -> new ( filename => 'run1.lst' );
    #   
    #   my @thetas = @{$out_obj -> thetas};
    #   my @omegas = @{$out_obj -> omegas};
    #   my @ofvs   = @{$out_obj -> ofvs};
end synopsis

# }}} synopsis

# {{{ see_also

start see_also
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
end see_also

# }}} see_also

# {{{ new

start new
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
end new

# }}} new


# {{{ filename

# }}} filename


# {{{ full_name
start full_name
	$full_name = $self->directory . $self->filename;
end full_name

# }}} full_name





# {{{ copy
start copy
	$new_output = Storable::dclone( $self );
end copy
# }}} copy

# {{{ Definitions and help text for all accessors

start comegas
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
end comegas

start condition_number
    # condition_number returns the 2-norm condition number for the correlation matrix, i.e.
    # the largest eigen value divided by the smallest.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end condition_number

start est_thetanames
    # return thetanames from output_matrix_headers
end est_thetanames

start est_omeganames
    # return omeganames from output_matrix headers
end est_omeganames

start est_sigmanames
    # return sigmanames from output_matrix headers
end est_sigmanames


start covariance_step_run
    # Returns 1 if the covariance step was run, 0 otherwise. See
    # L</comegas> for details.
    #
    # Level:  Problem
end covariance_step_run

start get_single_value
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
end get_single_value

start covariance_step_successful
    # Returns 1 if the covariance step was successful, 0
    # otherwise. See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
end covariance_step_successful

start covariance_step_warnings
    # Returns 0 if there were no warnings or errors printed during the
    # covariance step, 1 otherwise. See L</comegas> for details on the
    # method arguments.
    #
    # Level:  Sub problem
end covariance_step_warnings

start csigmas
    # csigmas returns the standard deviation for elements on the
    # diagonal and correlation coefficients for off-diagonal elements.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in sigmacoordval.
end csigmas

start cvseomegas
    # cvseomegas returns the relative standard error for the omegas, i.e. SE/estimate.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in omegacoordval.
end cvseomegas

start cvsesigmas
    # cvsesigmas returns the relative standard error for the sigmas, i.e. SE/estimate.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in sigmacoordval.
end cvsesigmas

start cvsethetas
    # cvsethetas returns the relative standard error for the thetas, i.e. SE/estimate.
    # See L</comegas> for details on the method arguments.
    #
    # Level:  Sub problem
    # On the subproblem level it is an array 
    # with one value for each name in sorted list of keys in thetacoordval.
end cvsethetas

start eigens
    # eigens returns the eigen values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end eigens

start etabar
    # etabar returns the ETABAR estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end etabar

start feval
    # feval returns the number of function evaluations.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end feval

start finalparam
    # finalparam returns the final parameter vector as it appears in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end finalparam

start final_gradients
    # final_gradients returns the final gradient vector as it appears in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end final_gradients

start fixedomegas
	# fixedomegas returns the a vector of booleans; 1's if
	# the parameters were fixed during the model fit, 0's
	# if they were not.

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->fixedomegas()) {
		@fixedomegas = @{$self->problems->[$problem_index]->fixedomegas()};
	}
end fixedomegas

start fixedsigmas
	# fixedsigmas returns a vector of booleans; 1's if
	# the parameters were fixed during the model fit, 0's
	# if they were not.

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->fixedsigmas()) {
      @fixedsigmas = @{$self->problems->[$problem_index]->fixedsigmas()};
	}
end fixedsigmas

start fixedthetas
	# fixedthetas returns a vector of booleans; 1's if
	# the parameters were fixed during the model fit, 0's
	# if they were not.

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->fixedthetas()) {
		@fixedthetas = @{$self->problems->[$problem_index]->fixedthetas()};
	}
end fixedthetas

start upper_theta_bounds
	# returns a vector of numbers

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->upper_theta_bounds()) {
		@upper_theta_bounds = @{$self->problems->[$problem_index]->upper_theta_bounds()};
	}
end upper_theta_bounds

start lower_theta_bounds
	# returns a vector of numbers

	if ( defined $self->problems and defined $self->problems->[$problem_index]
			and defined $self->problems->[$problem_index]->lower_theta_bounds()) {
		@lower_theta_bounds = @{$self->problems->[$problem_index]->lower_theta_bounds()};
	}
end lower_theta_bounds


start funcevalpath
    # funcevalpath returns the number of function evaluations for each printed iteration in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end funcevalpath

start gradient_path
    # gradient_path returns the gradients for each printed iteration in the monitoring of search section (returns a matrix for each sub problem).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end gradient_path

start initgrad
    # initgrad returns the initial gradient vector in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end initgrad

start initomegas
    # initomegas returns the initial omega values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end initomegas

start initsigmas
    # initsigmas returns the initial sigma values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end initsigmas

start initthetas
    # initthetas returns the initial theta values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end initthetas

start iternum
    # iternum returns a vector of the iteration numbers in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end iternum

start nind
    # nind returns the number of individuals.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Problem
end nind

start nobs
    # nobs returns the number of observations.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Problem
end nobs

start npofv
    # npofv returns the non-parametric objective function value.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end npofv

start nrecs
    # nrecs returns the number of records.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Problem
end nrecs

start npomegas
    # npomegas returns the non-parametric omega estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end npomegas

start npetabars
    # npthetas returns the non-parametric theta estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end npetabars

start nth
    # nth returns the number of thetas.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem

    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end nth

start ofvpath
    # ofvpath returns the objective [function] values in the monitoring of search section.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end ofvpath

start ofv
    # ofv returns the objective function value(s).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end ofv

start dic
    # Level:  Sub problem
end dic

start omega_block_structure
    # omega_block_structure returns the block structure for
    # the omega parameters in a lower triangular matrix form
    # as in the OMEGA HAS BLOCK FORM section in the NONMEM output file.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end omega_block_structure

start omegacoordval
    # omegacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. OMEGA(1,1), OMEGA(1,2) etc as keys
    # and parameter estimates as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end omegacoordval

start omeganames
    # omeganames returns the default parameter names, OMEGA(1,1) etc for stored values
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end omeganames

start omegas
    # omegas returns the omega parameter estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end omegas

start parameter_path
    # parameter_path returns the (normalized) parameter estimates for each iteration in the monitoring of search section (Matrix returned).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end parameter_path

start pval
    # pval returns the P VAL (reflects the probability that the etas are not centered around zero).
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end pval

start raw_covmatrix
    # raw_covmatrix returns the (raw) covariance matrix including empty matrix elements marked as '.........'.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end raw_covmatrix

start inverse_covariance_matrix
    # inverse_covariance_matrix returns the inverse covariance matrix 
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end inverse_covariance_matrix

start raw_cormatrix
    # raw_cormatrix returns the (raw) correlation matrix including empty matrix elements marked as '.........'.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end raw_cormatrix

start raw_omegas
    # raw_omegas returns the (raw) omegas.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end raw_omegas

start raw_seomegas
    # raw_seomegas returns the (raw) omega standard error estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end raw_seomegas

start raw_sesigmas
    # raw_sesigmas returns the (raw) sigma standard error estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end raw_sesigmas

start raw_sigmas
    # raw_sigmas returns the (raw) sigmas.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end raw_sigmas

start raw_tmatrix
    # raw_tmatrix returns the (raw) T-matrix.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end raw_tmatrix

start sum_estimation_time
    # Level:  Sub problem
end sum_estimation_time

start sum_covariance_time
    # Level:  Sub problem
end sum_covariance_time

start seomegacoordval
    # seomegacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. OMEGA(1,1), OMEGA(1,2) etc as keys
    # and parameter standard errors as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end seomegacoordval

start sesigmacoordval
    # sesigmacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. SIGMA(1,1), SIGMA(1,2) etc as keys
    # and parameter standard errors as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end sesigmacoordval

start sethetacoordval
    # sethetacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. THETA1, THETA2 etc as keys
    # and parameter standard errors as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end sethetacoordval

start seomegas
    # seomegas returns the omega standard error estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end seomegas

start sesigmas
#    # sesigmas returns the sigma standard error estimates.
#    # See L</comegas> for details of the method arguments.
#    #
#    # Level:  Sub problem
end sesigmas

start sethetas
#    # sethetas returns the theta standard error estimates.
#    # See L</comegas> for details of the method arguments.
#    #
#    # Level:  Sub problem
end sethetas

start significant_digits
    # significant_digits returns the number of significant digits for the model fit.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end significant_digits

start sigma_block_structure
    # sigma_block_structure returns the block structure for
    # the sigma parameters in a lower triangular matrix form
    # as in the sigma HAS BLOCK FORM section in the NONMEM output file.
    # See L</csigmas> for details of the method arguments.
    #
    # Level:  Sub problem
    croak("This function cannot be used, is only a placeholder. Rewrite program.");
end sigma_block_structure

start sigmacoordval
    # sigmacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. SIGMA(1,1), SIGMA(1,2) etc as keys
    # and parameter estimates as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end sigmacoordval

start sigmanames
    # sigmanames returns the default parameter names, i.e. SI1, SI1_2, SI2 etc.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end sigmanames

start sigmas
    # sigmas returns the sigma parameter estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end sigmas

start simulationstep
    # simulationstep returns a boolean value 1 or 0, reflecting
    # whether a simulation was performed or not.  See L</comegas> for
    # Details of the method arguments.
    #
    # Level:  Sub Problem
end simulationstep

start minimization_successful
    # minimization_successful returns a boolean value 1 or 0,
    # reflecting whether the minimization was successful or not.  See
    # L</comegas> for details of the method arguments.
    #
    # Level:  Sub Problem
end minimization_successful

start minimization_message
    # minimization_message returns the minimization message, i.e
    #   MINIMIZATION SUCCESSFUL... 
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end minimization_message

start thetacoordval
    # thetacoordval returns (at the sub problem level) a hash
    # with default parameter names , i.e. THETA1, THETA2 etc as keys
    # and parameter estimates as values.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end thetacoordval

start thetanames
    # thetanames returns the default theta parameter names, TH1, TH2 etc.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end thetanames

start thetas
    # thetas returns the theta parameter estimates.
    # See L</comegas> for details of the method arguments.
    #
    # Level:  Sub problem
end thetas

# }}} Definitions and help text for all accessors

# {{{ have_output

start have_output
	# have_output returns true if the output files exits or if there
	# is output data in memory.

	if ( -e $self->full_name || not_empty($self->problems) ) {
	  return 1;
	} else {
	  return 0;
	}
end have_output

# }}} have_output


# {{{ _read_problems

start _read_problems
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
end _read_problems

# }}} _read_problems

# {{{ parsing_error
start parsing_error
	$self->parsed_successfully( 0 );
	$self->parsing_error_message( $message );
end parsing_error
# }}} parsing_error

# {{{ access_any

start access_any
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
end access_any

# }}} access_any

# {{{ high_correlations
start high_correlations
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
end high_correlations
# }}} high_correlations

# {{{ large_standard_errors
start large_standard_errors
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
end large_standard_errors
# }}} large_standard_errors

# {{{ near_bounds

start near_bounds
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
end near_bounds

# }}} near_bounds

# {{{ problem_structure

start problem_structure
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
end problem_structure

# }}}


# {{{ labels
start labels
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
end labels
# }}} labels

# {{{ flush
start flush
	# flush is not an accessor method. As its name implies it flushes the
	# output objects memory by setting the I<problems> attribute to undef.
	# This method can be useful when many output objects are handled and
	# the memory is limited.

	# Flushes the object to save memory. There is no need to
	# synchronize the ouptut object before this since they are read-
	# only.

	$self->problems([]);
	#$self -> {'synced'} = 0;
end flush
# }}} flush
