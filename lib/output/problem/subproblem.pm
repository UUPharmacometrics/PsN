use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package output::problem::subproblem;
use Carp;
use Config;
use ext::Math::MatrixReal;
use Data::Dumper;
use ui;
use array;
use debug;


sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'subproblem_id' => 'SCALAR', 'skip_labels_matrix' => 'SCALAR',
			'next_to_last_step_successful' => 'SCALAR',
			'problem_id' => 'SCALAR', 'output_id' => 'SCALAR',
			'model_id' => 'SCALAR', 'comegas' => 'ARRAY',
			'condition_number' => 'SCALAR', 'covariance_step_successful' => 'SCALAR',
			'estimate_near_boundary' => 'SCALAR', 'covariance_step_warnings' => 'SCALAR',
			's_matrix_singular' => 'SCALAR', 'csigmas' => 'ARRAY',
			'cvseomegas' => 'ARRAY', 'cvsesigmas' => 'ARRAY',
			'cvsethetas' => 'ARRAY', 'shrinkage_eta' => 'ARRAY',
			'shrinkage_eps' => 'ARRAY', 'eigens' => 'ARRAY',
			'etabar' => 'ARRAY', 'feval' => 'SCALAR', 'final_gradients' => 'ARRAY',
			'finalparam' => 'ARRAY', 'funcevalpath' => 'ARRAY',
			'gradient_path' => 'ARRAY', 'initgrad' => 'ARRAY',
			'iternum' => 'ARRAY', 'nom' => 'SCALAR', 'npofv' => 'SCALAR',
			'npomegas' => 'ARRAY', 'npetabars' => 'ARRAY',
			'nrom' => 'SCALAR', 'nth' => 'SCALAR', 'ofvpath' => 'ARRAY',
			'ofv' => 'SCALAR', 'dic' => 'SCALAR', 'omegacoordval' => 'REF',
			'seomegacoordval' => 'REF', 'parameter_path' => 'ARRAY',
			'parsed' => 'SCALAR', 'parsed_successfully' => 'SCALAR',
			'parsing_error_message' => 'SCALAR', 'pval' => 'ARRAY',
			'raw_cormatrix' => 'ARRAY', 'correlation_matrix' => 'ARRAY',
			'output_matrix_headers' => 'ARRAY', 'raw_covmatrix' => 'ARRAY',
			'raw_invcovmatrix' => 'ARRAY', 'raw_omegas' => 'ARRAY',
			'raw_seomegas' => 'ARRAY', 'raw_sesigmas' => 'ARRAY',
			'raw_sigmas' => 'ARRAY', 'raw_tmatrix' => 'ARRAY',
			'significant_digits' => 'SCALAR', 'sigmacoordval' => 'REF',
			'sesigmacoordval' => 'REF', 'simulationstep' => 'SCALAR',
			'minimization_successful' => 'SCALAR', 'final_zero_gradients' => 'SCALAR',
			'hessian_reset' => 'SCALAR', 'zero_gradients' => 'SCALAR',
			'rounding_errors' => 'SCALAR', 'minimization_message' => 'ARRAY',
			'thetacoordval' => 'REF', 'sethetacoordval' => 'REF',
			'NM7_parsed_raw' => 'SCALAR', 'NM7_parsed_additional' => 'SCALAR',
			'nm_output_files' => '', 'ignore_missing_files' => 'SCALAR',
			'have_omegas' => 'SCALAR', 'have_sigmas' => 'SCALAR',
			'method_number' => 'SCALAR', 'input_problem' => 'REF',
			'classical_method' => 'SCALAR', 'table_number' => 'SCALAR',
			'nm_major_version' => 'm_SCALAR', 'method_string' => 'SCALAR',
			'lstfile' => 'ARRAY', 'lstfile_pos' => 'SCALAR',
			'covariance_step_run' => 'SCALAR', 'tablename' => 'SCALAR',
			'tableidcolumn' => 'SCALAR', 'omega_block_structure' => 'ARRAY',
			'sigma_block_structure' => 'ARRAY', 'omega_block_structure_type' => 'SCALAR',
			'sigma_block_structure_type' => 'SCALAR',
			'omega_block_sets' => 'HASH', 'sigma_block_sets' => 'HASH',
			'inverse_covariance_matrix' => 'ARRAY', 't_matrix' => 'ARRAY',
			'estimated_thetas' => 'ARRAY', 'estimated_omegas' => 'ARRAY',
			'estimated_sigmas' => 'ARRAY', 'parameter_significant_digits' => 'ARRAY',
			'estimation_step_run' => 'SCALAR', 'nonparametric_step_run' => 'SCALAR',
			'msfi_used' => 'SCALAR', 'finished_parsing' => 'SCALAR',
			'estimation_step_initiated' => 'SCALAR', 'sum_estimation_time' => 'SCALAR',
			'sum_covariance_time' => 'SCALAR', 'burn_in_convergence' => 'SCALAR',
			'covariance_matrix' => 'ARRAY', 'burn_in_iterations' => 'SCALAR',
			'NM7_parsed_raw' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'covariance_step_successful'} = defined $parm{'covariance_step_successful'} ? $parm{'covariance_step_successful'} : 0 unless defined $this -> {'covariance_step_successful'};
	$this -> {'estimate_near_boundary'} = defined $parm{'estimate_near_boundary'} ? $parm{'estimate_near_boundary'} : 0 unless defined $this -> {'estimate_near_boundary'};
	$this -> {'covariance_step_warnings'} = defined $parm{'covariance_step_warnings'} ? $parm{'covariance_step_warnings'} : 0 unless defined $this -> {'covariance_step_warnings'};
	$this -> {'s_matrix_singular'} = defined $parm{'s_matrix_singular'} ? $parm{'s_matrix_singular'} : 0 unless defined $this -> {'s_matrix_singular'};
	$this -> {'parsed'} = defined $parm{'parsed'} ? $parm{'parsed'} : 0 unless defined $this -> {'parsed'};
	$this -> {'parsed_successfully'} = defined $parm{'parsed_successfully'} ? $parm{'parsed_successfully'} : 1 unless defined $this -> {'parsed_successfully'};
	$this -> {'simulationstep'} = defined $parm{'simulationstep'} ? $parm{'simulationstep'} : 0 unless defined $this -> {'simulationstep'};
	$this -> {'minimization_successful'} = defined $parm{'minimization_successful'} ? $parm{'minimization_successful'} : 0 unless defined $this -> {'minimization_successful'};
	$this -> {'rounding_errors'} = defined $parm{'rounding_errors'} ? $parm{'rounding_errors'} : 0 unless defined $this -> {'rounding_errors'};
	$this -> {'minimization_message'} = defined $parm{'minimization_message'} ? $parm{'minimization_message'} : [] unless defined $this -> {'minimization_message'};
	$this -> {'NM7_parsed_raw'} = defined $parm{'NM7_parsed_raw'} ? $parm{'NM7_parsed_raw'} : 0 unless defined $this -> {'NM7_parsed_raw'};
	$this -> {'NM7_parsed_additional'} = defined $parm{'NM7_parsed_additional'} ? $parm{'NM7_parsed_additional'} : 0 unless defined $this -> {'NM7_parsed_additional'};
	$this -> {'ignore_missing_files'} = defined $parm{'ignore_missing_files'} ? $parm{'ignore_missing_files'} : 0 unless defined $this -> {'ignore_missing_files'};
	$this -> {'classical_method'} = defined $parm{'classical_method'} ? $parm{'classical_method'} : 1 unless defined $this -> {'classical_method'};
	$this -> {'lstfile_pos'} = defined $parm{'lstfile_pos'} ? $parm{'lstfile_pos'} : 0 unless defined $this -> {'lstfile_pos'};
	$this -> {'covariance_step_run'} = defined $parm{'covariance_step_run'} ? $parm{'covariance_step_run'} : 0 unless defined $this -> {'covariance_step_run'};
	$this -> {'tableidcolumn'} = defined $parm{'tableidcolumn'} ? $parm{'tableidcolumn'} : 0 unless defined $this -> {'tableidcolumn'};
	$this -> {'estimated_thetas'} = defined $parm{'estimated_thetas'} ? $parm{'estimated_thetas'} : [] unless defined $this -> {'estimated_thetas'};
	$this -> {'estimated_omegas'} = defined $parm{'estimated_omegas'} ? $parm{'estimated_omegas'} : [] unless defined $this -> {'estimated_omegas'};
	$this -> {'estimated_sigmas'} = defined $parm{'estimated_sigmas'} ? $parm{'estimated_sigmas'} : [] unless defined $this -> {'estimated_sigmas'};
	$this -> {'estimation_step_run'} = defined $parm{'estimation_step_run'} ? $parm{'estimation_step_run'} : 0 unless defined $this -> {'estimation_step_run'};
	$this -> {'nonparametric_step_run'} = defined $parm{'nonparametric_step_run'} ? $parm{'nonparametric_step_run'} : 0 unless defined $this -> {'nonparametric_step_run'};
	$this -> {'msfi_used'} = defined $parm{'msfi_used'} ? $parm{'msfi_used'} : 0 unless defined $this -> {'msfi_used'};
	$this -> {'finished_parsing'} = defined $parm{'finished_parsing'} ? $parm{'finished_parsing'} : 0 unless defined $this -> {'finished_parsing'};
	$this -> {'estimation_step_initiated'} = defined $parm{'estimation_step_initiated'} ? $parm{'estimation_step_initiated'} : 0 unless defined $this -> {'estimation_step_initiated'};
	$this -> {'NM7_parsed_raw'} = defined $parm{'NM7_parsed_raw'} ? $parm{'NM7_parsed_raw'} : 0 unless defined $this -> {'NM7_parsed_raw'};

	bless $this, $class;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 17 "lib/output/problem/subproblem_subs.pm" 
      {
	$this->have_sigmas(1) if (defined $this->input_problem()->sigmas() and scalar(@{$this->input_problem()->sigmas()}) > 0);
	
	$this->have_omegas(1) if (defined $this->input_problem()->omegas() and scalar(@{$this->input_problem()->omegas()}) > 0);

	$this -> parse_NM7_raw() if (defined $this->nm_output_files->{'raw'});
	$this -> parse_NM7_additional() if (defined $this->nm_output_files->{'cov'} and
					    defined $this->nm_output_files->{'cor'} and
					    defined $this->nm_output_files->{'coi'} and 
					    $this -> covariance_step_run() and
					    $this -> NM7_parsed_raw());

	while (1) {
	    #we will always break out of this loop, use as simple way of finishing parsing early. Cannot return because of dia code
	    $this -> _read_simulation;
	    if ($this->nm_major_version < 7) {
		if( $this -> estimation_step_run()) {
		    $this -> _read_iteration_path;
		    last unless ($this -> parsed_successfully() and not $this -> finished_parsing());
		    $this -> _read_term();
		}
	    } else {
		#NM7
		#When NM7 do not know yet if estimation_step_run actually set correctly, only whether we have a $EST. 
		#Could be a MAXEVAL=0, so must check the right #METH line for (Evaluation)
		#Regardless, we still need to scan to the right method
		#have separate method for NM7 find right #METH
		#But sometimes #METH not printed if simulation!
		if ($this->estimation_step_run() ){ #this is true if we have $EST
		    $this -> _scan_to_meth(); #inside routine set estimation_step_run to false if (Evaluation) on #METH line
		}
		last unless ($this -> parsed_successfully() and not $this -> finished_parsing());
		if( $this -> estimation_step_run()) { #this might be false now even if true above
		    $this -> _read_iteration_path;
		    last unless ($this -> parsed_successfully() and not $this -> finished_parsing());
				$this -> _read_term();
		}


			}
	    last unless ($this -> parsed_successfully() and not $this -> finished_parsing());
	    
	    if( $this -> estimation_step_initiated() ) {
		#this is often true even if do not have $EST. Sometimes there will be a #METH, sometimes not.
		$this -> _read_ofv()                  unless ($this -> NM7_parsed_raw());
		last unless ($this -> parsed_successfully() and not $this -> finished_parsing());

		$this -> _read_thomsi()               unless ( $this -> NM7_parsed_raw());
		if ($this -> parsed_successfully() and defined $this -> omegas() ) {
		    $this -> _compute_comegas();
		}
		if ($this -> parsed_successfully() and defined $this -> sigmas() ) {
		    $this -> _compute_csigmas();
		}

		last unless ($this -> parsed_successfully() and not $this -> finished_parsing());

		if ($this -> covariance_step_run() ) {
		    $this -> _read_sethomsi()             unless ($this -> NM7_parsed_raw() );
		    if ($this -> parsed_successfully()) {
			$this -> _compute_cvsetheta();
			$this -> _compute_cvseomega();
			$this -> _compute_cvsesigma();
		    }
		}
		
	    }
	    
	    last unless ($this -> parsed_successfully() and not $this -> finished_parsing());
	    
	    if ($this -> covariance_step_run()) {
		$this -> _read_covmatrix()          if ( not $this -> NM7_parsed_additional());
		last unless ($this -> parsed_successfully() and not $this -> finished_parsing());
		
		$this -> _read_eigen()              if (not $this -> NM7_parsed_raw());
	    }
	    last unless ($this -> parsed_successfully() and not $this -> finished_parsing());
	    
	    if( $this -> nonparametric_step_run() ) {
		$this -> _read_npomegas();
	    }
	    last; #always break loop here if not done already
	}
	delete $this -> {'lstfile'};
      }
# line 222 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub subproblem_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subproblem_id'} = $parm;
	} else {
		return $self -> {'subproblem_id'};
	}
}

sub skip_labels_matrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'skip_labels_matrix'} = $parm;
	} else {
		return $self -> {'skip_labels_matrix'};
	}
}

sub next_to_last_step_successful {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'next_to_last_step_successful'} = $parm;
	} else {
		return $self -> {'next_to_last_step_successful'};
	}
}

sub problem_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'problem_id'} = $parm;
	} else {
		return $self -> {'problem_id'};
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

sub comegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'comegas'} = $parm;
	} else {
		return $self -> {'comegas'};
	}
}

sub condition_number {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'condition_number'} = $parm;
	} else {
		return $self -> {'condition_number'};
	}
}

sub covariance_step_successful {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariance_step_successful'} = $parm;
	} else {
		return $self -> {'covariance_step_successful'};
	}
}

sub estimate_near_boundary {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimate_near_boundary'} = $parm;
	} else {
		return $self -> {'estimate_near_boundary'};
	}
}

sub covariance_step_warnings {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariance_step_warnings'} = $parm;
	} else {
		return $self -> {'covariance_step_warnings'};
	}
}

sub s_matrix_singular {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'s_matrix_singular'} = $parm;
	} else {
		return $self -> {'s_matrix_singular'};
	}
}

sub csigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'csigmas'} = $parm;
	} else {
		return $self -> {'csigmas'};
	}
}

sub cvseomegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cvseomegas'} = $parm;
	} else {
		return $self -> {'cvseomegas'};
	}
}

sub cvsesigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cvsesigmas'} = $parm;
	} else {
		return $self -> {'cvsesigmas'};
	}
}

sub cvsethetas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cvsethetas'} = $parm;
	} else {
		return $self -> {'cvsethetas'};
	}
}

sub shrinkage_eta {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'shrinkage_eta'} = $parm;
	} else {
		return $self -> {'shrinkage_eta'};
	}
}

sub shrinkage_eps {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'shrinkage_eps'} = $parm;
	} else {
		return $self -> {'shrinkage_eps'};
	}
}

sub eigens {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'eigens'} = $parm;
	} else {
		return $self -> {'eigens'};
	}
}

sub etabar {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'etabar'} = $parm;
	} else {
		return $self -> {'etabar'};
	}
}

sub feval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'feval'} = $parm;
	} else {
		return $self -> {'feval'};
	}
}

sub final_gradients {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'final_gradients'} = $parm;
	} else {
		return $self -> {'final_gradients'};
	}
}

sub finalparam {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'finalparam'} = $parm;
	} else {
		return $self -> {'finalparam'};
	}
}

sub funcevalpath {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'funcevalpath'} = $parm;
	} else {
		return $self -> {'funcevalpath'};
	}
}

sub gradient_path {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'gradient_path'} = $parm;
	} else {
		return $self -> {'gradient_path'};
	}
}

sub initgrad {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'initgrad'} = $parm;
	} else {
		return $self -> {'initgrad'};
	}
}

sub iternum {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'iternum'} = $parm;
	} else {
		return $self -> {'iternum'};
	}
}

sub nom {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nom'} = $parm;
	} else {
		return $self -> {'nom'};
	}
}

sub npofv {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'npofv'} = $parm;
	} else {
		return $self -> {'npofv'};
	}
}

sub npomegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'npomegas'} = $parm;
	} else {
		return $self -> {'npomegas'};
	}
}

sub npetabars {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'npetabars'} = $parm;
	} else {
		return $self -> {'npetabars'};
	}
}

sub nrom {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nrom'} = $parm;
	} else {
		return $self -> {'nrom'};
	}
}

sub nth {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nth'} = $parm;
	} else {
		return $self -> {'nth'};
	}
}

sub ofvpath {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ofvpath'} = $parm;
	} else {
		return $self -> {'ofvpath'};
	}
}

sub ofv {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ofv'} = $parm;
	} else {
		return $self -> {'ofv'};
	}
}

sub dic {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'dic'} = $parm;
	} else {
		return $self -> {'dic'};
	}
}

sub omegacoordval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'omegacoordval'} = $parm;
	} else {
		return $self -> {'omegacoordval'};
	}
}

sub seomegacoordval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'seomegacoordval'} = $parm;
	} else {
		return $self -> {'seomegacoordval'};
	}
}

sub parameter_path {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parameter_path'} = $parm;
	} else {
		return $self -> {'parameter_path'};
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

sub pval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'pval'} = $parm;
	} else {
		return $self -> {'pval'};
	}
}

sub raw_cormatrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_cormatrix'} = $parm;
	} else {
		return $self -> {'raw_cormatrix'};
	}
}

sub correlation_matrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'correlation_matrix'} = $parm;
	} else {
		return $self -> {'correlation_matrix'};
	}
}

sub output_matrix_headers {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'output_matrix_headers'} = $parm;
	} else {
		return $self -> {'output_matrix_headers'};
	}
}

sub raw_covmatrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_covmatrix'} = $parm;
	} else {
		return $self -> {'raw_covmatrix'};
	}
}

sub raw_invcovmatrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_invcovmatrix'} = $parm;
	} else {
		return $self -> {'raw_invcovmatrix'};
	}
}

sub raw_omegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_omegas'} = $parm;
	} else {
		return $self -> {'raw_omegas'};
	}
}

sub raw_seomegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_seomegas'} = $parm;
	} else {
		return $self -> {'raw_seomegas'};
	}
}

sub raw_sesigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_sesigmas'} = $parm;
	} else {
		return $self -> {'raw_sesigmas'};
	}
}

sub raw_sigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_sigmas'} = $parm;
	} else {
		return $self -> {'raw_sigmas'};
	}
}

sub raw_tmatrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'raw_tmatrix'} = $parm;
	} else {
		return $self -> {'raw_tmatrix'};
	}
}

sub significant_digits {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'significant_digits'} = $parm;
	} else {
		return $self -> {'significant_digits'};
	}
}

sub sigmacoordval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sigmacoordval'} = $parm;
	} else {
		return $self -> {'sigmacoordval'};
	}
}

sub sesigmacoordval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sesigmacoordval'} = $parm;
	} else {
		return $self -> {'sesigmacoordval'};
	}
}

sub simulationstep {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'simulationstep'} = $parm;
	} else {
		return $self -> {'simulationstep'};
	}
}

sub minimization_successful {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'minimization_successful'} = $parm;
	} else {
		return $self -> {'minimization_successful'};
	}
}

sub final_zero_gradients {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'final_zero_gradients'} = $parm;
	} else {
		return $self -> {'final_zero_gradients'};
	}
}

sub hessian_reset {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'hessian_reset'} = $parm;
	} else {
		return $self -> {'hessian_reset'};
	}
}

sub zero_gradients {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'zero_gradients'} = $parm;
	} else {
		return $self -> {'zero_gradients'};
	}
}

sub rounding_errors {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'rounding_errors'} = $parm;
	} else {
		return $self -> {'rounding_errors'};
	}
}

sub minimization_message {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'minimization_message'} = $parm;
	} else {
		return $self -> {'minimization_message'};
	}
}

sub thetacoordval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'thetacoordval'} = $parm;
	} else {
		return $self -> {'thetacoordval'};
	}
}

sub sethetacoordval {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sethetacoordval'} = $parm;
	} else {
		return $self -> {'sethetacoordval'};
	}
}

sub NM7_parsed_raw {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'NM7_parsed_raw'} = $parm;
	} else {
		return $self -> {'NM7_parsed_raw'};
	}
}

sub NM7_parsed_additional {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'NM7_parsed_additional'} = $parm;
	} else {
		return $self -> {'NM7_parsed_additional'};
	}
}

sub nm_output_files {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nm_output_files'} = $parm;
	} else {
		return $self -> {'nm_output_files'};
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

sub have_omegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'have_omegas'} = $parm;
	} else {
		return $self -> {'have_omegas'};
	}
}

sub have_sigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'have_sigmas'} = $parm;
	} else {
		return $self -> {'have_sigmas'};
	}
}

sub method_number {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'method_number'} = $parm;
	} else {
		return $self -> {'method_number'};
	}
}

sub input_problem {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'input_problem'} = $parm;
	} else {
		return $self -> {'input_problem'};
	}
}

sub classical_method {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'classical_method'} = $parm;
	} else {
		return $self -> {'classical_method'};
	}
}

sub table_number {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'table_number'} = $parm;
	} else {
		return $self -> {'table_number'};
	}
}

sub nm_major_version {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nm_major_version'} = $parm;
	} else {
		return $self -> {'nm_major_version'};
	}
}

sub method_string {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'method_string'} = $parm;
	} else {
		return $self -> {'method_string'};
	}
}

sub lstfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lstfile'} = $parm;
	} else {
		return $self -> {'lstfile'};
	}
}

sub lstfile_pos {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lstfile_pos'} = $parm;
	} else {
		return $self -> {'lstfile_pos'};
	}
}

sub covariance_step_run {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariance_step_run'} = $parm;
	} else {
		return $self -> {'covariance_step_run'};
	}
}

sub tablename {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tablename'} = $parm;
	} else {
		return $self -> {'tablename'};
	}
}

sub tableidcolumn {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tableidcolumn'} = $parm;
	} else {
		return $self -> {'tableidcolumn'};
	}
}

sub omega_block_structure {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'omega_block_structure'} = $parm;
	} else {
		return $self -> {'omega_block_structure'};
	}
}

sub sigma_block_structure {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sigma_block_structure'} = $parm;
	} else {
		return $self -> {'sigma_block_structure'};
	}
}

sub omega_block_structure_type {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'omega_block_structure_type'} = $parm;
	} else {
		return $self -> {'omega_block_structure_type'};
	}
}

sub sigma_block_structure_type {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sigma_block_structure_type'} = $parm;
	} else {
		return $self -> {'sigma_block_structure_type'};
	}
}

sub omega_block_sets {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'omega_block_sets'} = $parm;
	} else {
		return $self -> {'omega_block_sets'};
	}
}

sub sigma_block_sets {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sigma_block_sets'} = $parm;
	} else {
		return $self -> {'sigma_block_sets'};
	}
}

sub inverse_covariance_matrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'inverse_covariance_matrix'} = $parm;
	} else {
		return $self -> {'inverse_covariance_matrix'};
	}
}

sub t_matrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'t_matrix'} = $parm;
	} else {
		return $self -> {'t_matrix'};
	}
}

sub estimated_thetas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimated_thetas'} = $parm;
	} else {
		return $self -> {'estimated_thetas'};
	}
}

sub estimated_omegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimated_omegas'} = $parm;
	} else {
		return $self -> {'estimated_omegas'};
	}
}

sub estimated_sigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimated_sigmas'} = $parm;
	} else {
		return $self -> {'estimated_sigmas'};
	}
}

sub parameter_significant_digits {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'parameter_significant_digits'} = $parm;
	} else {
		return $self -> {'parameter_significant_digits'};
	}
}

sub estimation_step_run {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimation_step_run'} = $parm;
	} else {
		return $self -> {'estimation_step_run'};
	}
}

sub nonparametric_step_run {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonparametric_step_run'} = $parm;
	} else {
		return $self -> {'nonparametric_step_run'};
	}
}

sub msfi_used {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'msfi_used'} = $parm;
	} else {
		return $self -> {'msfi_used'};
	}
}

sub finished_parsing {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'finished_parsing'} = $parm;
	} else {
		return $self -> {'finished_parsing'};
	}
}

sub estimation_step_initiated {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimation_step_initiated'} = $parm;
	} else {
		return $self -> {'estimation_step_initiated'};
	}
}

sub sum_estimation_time {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sum_estimation_time'} = $parm;
	} else {
		return $self -> {'sum_estimation_time'};
	}
}

sub sum_covariance_time {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sum_covariance_time'} = $parm;
	} else {
		return $self -> {'sum_covariance_time'};
	}
}

sub burn_in_convergence {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'burn_in_convergence'} = $parm;
	} else {
		return $self -> {'burn_in_convergence'};
	}
}

sub covariance_matrix {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariance_matrix'} = $parm;
	} else {
		return $self -> {'covariance_matrix'};
	}
}

sub burn_in_iterations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'burn_in_iterations'} = $parm;
	} else {
		return $self -> {'burn_in_iterations'};
	}
}

sub tableobject {
	my $self = shift;
	my $table;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $table;
}

sub access_any {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'attribute' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->access_any: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->access_any: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->access_any: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->access_any: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->access_any: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $attribute = $parm{'attribute'};
	my $return_value;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return ;
}

sub parsing_error {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->parsing_error: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->parsing_error: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->parsing_error: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->parsing_error: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->parsing_error: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = $parm{'message'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> parsing_error');
# line 108 "lib/output/problem/subproblem_subs.pm" 
	$self->parsed_successfully( 0 );
	$self->parsing_error_message( $message );
# line 1765 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> parsing_error');
	# End of Non-Dia code #

}

sub _read_covmatrix {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_covmatrix');
# line 612 "lib/output/problem/subproblem_subs.pm" 
{
	my $matrix_nr = 0;
	my ( $t_success, $c_success, $corr_success, $i_success ) = (0, 0, 0, 0);
	my $start_pos = $self->lstfile_pos - 1;
	my $headers;
	my $dummyheaders;
	
	# {{{ sub clear dots

	sub clear_dots {
		my $m_ref = shift;
		my @matrix = @{$m_ref};
		# get rid of '........'
		my @clear;
		foreach ( @matrix ) {
			unless ( $_ eq '.........' ) {
				if ( $_ eq 'NAN' or $_ eq 'NaN') {
					push( @clear, 0 );
				} elsif ( $_ eq 'INF' ) {
					push( @clear, 99999999999999 );
				} elsif ( $_ eq '-INF' ) {
					push( @clear, -99999999999999 );
				} else {
					push( @clear, $_ );
				}
			}
		}
		return \@clear;
	}

	# }}}

# {{{ sub make square
	
	sub make_square {
	  my $m_ref = shift;
	  my @matrix = @{$m_ref};
	  # Make the matrix square:
	  my $elements = scalar @matrix; # = M*(M+1)/2
	  my $M = -0.5 + sqrt( 0.25 + 2 * $elements );
	  my @square;
	  for ( my $m = 1; $m <= $M; $m++ ) {
	    for ( my $n = 1; $n <= $m; $n++ ) {
	      push( @{$square[$m-1]}, $matrix[($m-1)*$m/2 + $n - 1] );
	      unless ( $m == $n ) {
					push( @{$square[$n-1]}, $matrix[($m-1)*$m/2 + $n - 1] );
	      }
	    }
	  }
	  return \@square;
	}
	
	# }}}

	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	  if (/T MATRIX/) {
	    while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	      if (/^ TH (\d)/ or /^\s+TH (\d) \| /) { # Read matrix and get out of inner while loop
					my $temp_matrix;
					( $start_pos, $temp_matrix, $t_success, $dummyheaders )  = $self ->
						_read_matrixoestimates( pos => $start_pos-1 ) and last;
					$self->tmatrix($temp_matrix);
	      }
	    }
	    last;		 # No covariance matrix will be found!
	  }
	  if (/    COVARIANCE MATRIX OF ESTIMATE/) {
	    while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	      if (/^ TH (\d)/ or /^\s+TH (\d) \| /) { # Read matrix and get out of inner while loop
					my $temp_matrix;
					( $start_pos, $temp_matrix, $c_success, $dummyheaders ) = $self -> _read_matrixoestimates( pos => $start_pos - 1 ) and last;
					$self->raw_covmatrix($temp_matrix);
	      }
	    }
	  }
	  if (/    CORRELATION MATRIX OF ESTIMATE/) {
	    while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	      if (/^ TH (\d)/ or /^\s+TH (\d) \| /) { # Read matrix and get out of inner while loop
					my $temp_matrix;
					( $start_pos, $temp_matrix, $corr_success, $headers ) = $self -> _read_matrixoestimates( pos => $start_pos - 1 ) and last;
					$self->raw_cormatrix($temp_matrix);
	      }
	    }
	  }
	  if (/    INVERSE COVARIANCE MATRIX OF ESTIMATE/) {
	    while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	      if (/^ TH (\d)/ or /^\s+TH (\d) \| /) { # Read matrix and get out of inner while loop
					my $temp_matrix;
					( $start_pos, $temp_matrix, $i_success, $dummyheaders ) = $self -> _read_matrixoestimates( pos => $start_pos - 1 ) and last;
					$self->raw_invcovmatrix($temp_matrix);
	      }
	    }
	    last;					# Last matrix?
	  }
	}
	$self->t_matrix([]) unless defined $self->t_matrix;
	$self->raw_tmatrix([]) unless defined $self->raw_tmatrix;
	foreach my $element ( @{$self->raw_tmatrix} ) {
	  push( @{$self->t_matrix}, eval($element) ) unless ( $element eq '.........' );
	}
	$self->covariance_matrix([]) unless defined $self->covariance_matrix;
	$self->raw_covmatrix([]) unless defined $self->raw_covmatrix;
	foreach my $element ( @{$self->raw_covmatrix} ) {
	  push( @{$self->covariance_matrix}, eval($element) ) unless ( $element eq '.........' );
	}
	$self->correlation_matrix([]) unless defined $self->correlation_matrix;
	$self->raw_cormatrix([]) unless defined $self->raw_cormatrix;
	foreach my $element ( @{$self->raw_cormatrix} ) {
	  push( @{$self->correlation_matrix}, eval($element) ) unless ( $element eq '.........' );
	}

	if ( defined $self->raw_invcovmatrix ) {
	  my $matrix_ref = make_square( clear_dots( $self->raw_invcovmatrix ));
	  if (scalar(@{$matrix_ref}) > 0) {
	    $self->inverse_covariance_matrix(Math::MatrixReal->new_from_cols($matrix_ref));
	  }
	}

	if (defined $headers) {
	  #need to get rid of row headers for lines with only .....
	  my $row = 1;
	  my $col = 1;
	  foreach my $element ( @{$self->raw_cormatrix } ) {
	    if (($col == 1) and ($element ne '.........' )) {
				$self->output_matrix_headers([]) unless defined $self->output_matrix_headers;
	      push( @{$self->output_matrix_headers}, $headers->[$row - 1]); 
	    }
	    $col++;
	    if ($col > $row) {
	      $col = 1;
	      $row++;
	    }
	  }
	}

	#If something has gone right!
	$self->lstfile_pos($start_pos) if ( $t_success + $c_success + $corr_success + $i_success );
}
# line 1915 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_covmatrix');
	# End of Non-Dia code #

}

sub _read_eigen {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_eigen');
# line 756 "lib/output/problem/subproblem_subs.pm" 
{
	my @eigens;
	my $start_pos = $self->lstfile_pos;
	my $eig_area = 0;
	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	  chomp;
	  if ( /EIGENVALUES OF COR MATRIX OF ESTIMATE/ ) {
	    $eig_area = 1;
	    $start_pos = $start_pos + 4 ; # Jump forward to the index numbers
	    carp("Found the eigenvalue area" );
	  INNER: while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) { # Get rid of indexes
	      last INNER if ( not /^\s+\d/ );
			 }
		}
	  if ( $eig_area ) {
	    $start_pos-- and last if (/^[a-df-zA-DF-Z]/); #Rewind one step
	    last if ( /^\s*\*/ or /^1/ );
	    push( @eigens, split );
	  }
	  $start_pos-- and last if ( /^ PROBLEM.*SUBPROBLEM/ or /^ PROBLEM NO\.:\s+\d/ );
	  $start_pos-- and last if (/^[a-df-zA-DF-Z]/); #Rewind one step
	}
	if ( scalar @eigens > 0 ) {
	  my @list = sort { $a <=> $b } @eigens;
	  $self->condition_number( abs($list[$#list] / $list[0]) ) if ( $list[0] != 0 );
	}
	$self->eigens(\@eigens);
}
# line 1955 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_eigen');
	# End of Non-Dia code #

}

sub _read_eval {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_eval');
# line 790 "lib/output/problem/subproblem_subs.pm" 
{
	my $start_pos = $self->lstfile_pos;

	while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
		if ( / PROBLEM NO\.:\s*\d+\n/ ) {
			$self -> parsing_error( message => "Error in reading the number of function evaluations!\nNext problem found" );
			return;
		}
      
		if ( $start_pos >= scalar @{$self->lstfile} ) {
			$self -> parsing_error( message => "Error in reading number of function evaluations!\nEOF found\n" );
			return;
		}

		if ( /^1/ or /MINIMUM VALUE OF OBJECTIVE FUNCTION/ ) {
			$self -> parsing_error( message => "Error in reading number of function evaluations!\nOFV found" );
			return;
		}
      
		if ( / NO. OF FUNCTION EVALUATIONS USED:\s*(\d*)/ ) {
			$self -> feval($1);
			last;
		}
	}
}
# line 1992 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_eval');
	# End of Non-Dia code #

}

sub _read_iteration_path {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'scan_only' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->_read_iteration_path: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_read_iteration_path: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_read_iteration_path: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_read_iteration_path: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_read_iteration_path: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $scan_only = defined $parm{'scan_only'} ? $parm{'scan_only'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_iteration_path');
# line 940 "lib/output/problem/subproblem_subs.pm" 
{

	my $start_pos = $self->lstfile_pos;
	my $success = 0;
	my (@func_eval, @parameter_vectors, @gradient_vectors) = ((), (), (), (), ());
	my @numsigdig_vectors;
	my $cumulative_evals = 0;
	my $zero_gradients = 0;
	my $hessian_reset = 0;
	my $found_monitoring = 0;
	my $burn_in_area = 0;
	my $burn_in_start;
	my $burn_in_end;

	my $method_exp = '^ #METH:\s*(.*)';
	my $term_exp = '^ #TERM:';
	my $tere_exp = '^ #TERE:';

	my $estprint = 1;
#	print "read iter path\n";
	if ($self->classical_method()) {
	  #check if iteration path output is skipped by option PRINT=0
		my @options;
		my $rec;
		if (defined $self->input_problem() and (defined $self->input_problem()->estimations())) {
			$rec = $self->input_problem()->estimations()->[(scalar(@{$self->input_problem()->estimations()})-1)];
		}
		@options = @{$rec-> options()} if (defined $rec and defined $rec->options());
	      
		foreach my $option ( @options ) {
			if ( defined $option and (($option -> name eq 'PRINT') or ( index( 'PRINT', $option -> name ) == 0 ))) {
				$estprint = $option->value();
				last;
			}
		}   
	}
	
	if ($self->nm_major_version >= 7) {
	  #If NM7 we should now be directly at right #METH line, check this
	    $_ = @{$self->lstfile}[ $start_pos ];
	    if( /$method_exp/ ) {
				my $string = $1;
				$string =~ s/\s*$//; #remove trailing spaces
					unless (($string =~ $self->{'method_string'}) or ($string eq $self->{'method_string'})) {
						croak("Error in read_iteration_path: METH in subprob has string\n"."$string ".
								"instead of expected\n" . $self->method_string);
					}
	    } else {
				croak("Bug in PsN read_iteration_path. Please report this, please include lst-file.");
			}
	    #next to last METH already checked, if necessary.
	    #scan_only is not relevant.
	    
	}

	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) { #Large reading loop
	  if ( /MONITORING OF SEARCH/ ) {
	    $found_monitoring = 1;
	  } elsif( /0HESSIAN OF POSTERIOR DENSITY IS NON-POSITIVE-DEFINITE DURING SEARCH/ ) {
	    # This is an error that stops the execution
			$self -> parsing_error( message => $_);
			$success = 1;
			$self -> finished_parsing(1);
			last;
	  } elsif ( /$term_exp/ ) {
			if ($burn_in_area){
				if (defined $burn_in_start and defined $burn_in_end){
					$self->burn_in_iterations(($burn_in_start-$burn_in_end));
				}
			}
			$success = 1 unless ($self->classical_method() and $estprint);
			last;
	  } elsif ( $found_monitoring and $start_pos >= scalar @{$self->lstfile} ) {
	    # This is probably (we cannot be really sure but it is
	    # likely) a "NONMEM black hole"-situation. We stop parsing
	    # but do not indicate that we had problems parsing the
	    # file.

	    $self -> parsing_error_message("Found \" MONITORING OF SEARCH:\" but no".
					   " records of the iterations before the end".
					   " of the output file. This is a sign of a".
					   " \"NONMEM black hole\"-situation. We cannot ".
					   "be 100% sure of this based on this type of".
					   " output so please take a good look at the files\n");
	    $self -> parsed_successfully(1);
	    $self -> finished_parsing(1);
	    return;
		} elsif ($found_monitoring and ( /Burn-in Mode/ )  ) {
			$burn_in_area = 1;
	  } elsif ($burn_in_area and ( /^\s*Convergence achieved/ )) {
	    $self->burn_in_convergence(1);
	    $burn_in_area = 0;
	    if (defined $burn_in_start and defined $burn_in_end){
	      $self->burn_in_iterations(($burn_in_start-$burn_in_end));
	    }
	  } elsif ($burn_in_area and (/^\s*iteration\s*-([0-9]+)/)  ) {
	    if (defined $burn_in_start) {
	      $burn_in_end = $1;
	    } else {
	      $burn_in_start = $1;
	    }
	  } elsif ($burn_in_area and (not (/^\s*iteration/)) ) {
	    $burn_in_area = 0;
	    if (defined $burn_in_start and defined $burn_in_end) {
	      $self->burn_in_iterations(($burn_in_start-$burn_in_end));
	    }
	  }

	    
	  if ( /0PROGRAM TERMINATED BY OBJ/ ) {
	    # This is an error message which terminates NONMEM. We
	    # return after reading the minimization message
	    $self -> minimization_successful(0);
	    $self -> finished_parsing(1);
	    $self -> _read_minimization_message();
	    return;
	  }	    

	  if (/^0ITERATION NO/) {
			unless (/0ITERATION NO\.:\s+(\d+)\s+OBJECTIVE VALUE:\s+(\S+)\s+NO\. OF FUNC\. EVALS\.:\s*(.+)/) {
				$self -> parsing_error( message => "Error in reading iteration path!\n$!" );
				return;
			}
	    $success = 1;
			$self->iternum([]) unless defined $self->iternum;
	    push(@{$self->iternum}, $1);

	    my $ofvpath = $2;
	    unless( $ofvpath eq '**' ) { # If funcion evals are more than 10000, NONMEM will print out two stars.
				$self->ofvpath([]) unless defined $self->ofvpath;
				push(@{$self->ofvpath}, $ofvpath );
	    } # If, in fact, we find stars, the number of evaluations are calculated below

	    my (@parameter_vector, @gradient_vector) = ((), ());
	    my @numsigdig_vector;
	    
	    while ( $_ = @{$self->lstfile}[ $start_pos ] ) {
	      if (/^ CUMULATIVE NO\. OF FUNC\. EVALS\.:\s*(\d+)/) {
					my $eval_path = $1; 
		
					if( $ofvpath eq '**' ) {
						my $ofvpath = $eval_path - $cumulative_evals;
						$cumulative_evals = $eval_path;
						carp("Calculated eval_path = $ofvpath" );
						$self->ofvpath([]) unless defined $self->ofvpath;
						push(@{$self->ofvpath}, $ofvpath );
		}

		$self->funcevalpath([]) unless defined $self->funcevalpath;
		push(@{$self->funcevalpath}, $eval_path);

		if (/RESET HESSIAN, TYPE (\w+)/) {
		  $hessian_reset++;
		}

		$start_pos++;
	      } elsif ( s/^ PARAMETER:\s*// ) {
		do {
		  push(@parameter_vector, split);
		  $_ = @{$self->lstfile}[ ++$start_pos ];
		  if( $start_pos >= scalar @{$self->lstfile} ) {
		    $self -> parsing_error( message => "Error in reading iteration path!\nEOF found\n" );
		    return;
		  }
		} while ( not /^ GRADIENT:\s*/ );
	      } elsif (s/^ GRADIENT:\s*//) {
		do {
		  push(@gradient_vector, split);
		  $_ = @{$self->lstfile}[ ++$start_pos ];
		  if( $start_pos >= scalar @{$self->lstfile} ) {
		    $self -> parsing_error( message => "Error in reading iteration path!\nEOF found\n" );
		    return;
		  }
		} while ( not /[a-zA-DF-X]/ ); #not #TERM either
	      } elsif (s/^ NUMSIGDIG:\s*//) {
	        do {
		  push(@numsigdig_vector, split);
		  $_ = @{$self->lstfile}[ ++$start_pos ];
		  if( $start_pos >= scalar @{$self->lstfile} ) {
		    $self -> parsing_error( message => "Error in reading iteration path!\nEOF found\n" );
		    return;
		  }
		} while ( not /[a-zA-DF-X]/ ); #not #TERM either
	      } elsif (s/^ NPARAMETR:\s*//) {
	        do {
		  $_ = @{$self->lstfile}[ ++$start_pos ];
		  if( $start_pos >= scalar @{$self->lstfile} ) {
		    $self -> parsing_error( message => "Error in reading iteration path!\nEOF found\n" );
		    return;
		  }
		} while ( not /[a-zA-DF-X]/ ); #not #TERM either
	      } else {
		last;
	      }
	    } #end of inner reading loop for ITERATION

	    foreach my $grad ( @gradient_vector ) {
	      $zero_gradients++ if ($grad == 0);
	    }
	    $self->initgrad(\@gradient_vector) unless ($self->initgrad);
	    $self->final_gradients(\@gradient_vector);
	    $self->finalparam(\@parameter_vector);
	    push(@parameter_vectors, \@parameter_vector);
	    push(@gradient_vectors, \@gradient_vector);
	    $self->parameter_significant_digits(\@numsigdig_vector) if scalar @numsigdig_vector;

	    if ( /0HESSIAN OF POSTERIOR DENSITY IS NON-POSITIVE-DEFINITE DURING SEARCH/ ) {
	      # This is an errror that stops the execution
	      $self -> parsing_error( message => $_);
	      $self -> finished_parsing(1);
	      last;
			}
	    last unless(/^0ITERATION NO/);
	  }			#End of if iteration no
	}			#End of large reading loop
    

	unless ( $self -> finished_parsing() ) {
	  my ($kill_found, $file_end, $kill_message, $search_pos) = (0, 0, "", $start_pos);
	  while ( $_ = @{$self->lstfile}[ $search_pos++ ] ) { #Have a look, a few lines down...
	    if( /kill/i ) {
	      $kill_found = 1;
	      $kill_message = $_;
	      last;
	    }
	    if( $search_pos + 1 == scalar @{$self->lstfile} ) {
	      $file_end = 1;
	      $search_pos = $start_pos + 4;
	    }
	    last if( $search_pos > $start_pos + 3 )
	  }
	  if (($kill_found == 1) or $file_end) { #Crash before last iteration
	    my $errstr = $kill_found ? "NONMEM killed" : "EOF found\n";
	    $self -> parsing_error( message => "Error in reading iteration path!\n$errstr" );
	    return;
	  }
	}
    
	unless ( $success ) {
	  unless ($self->nm_major_version < 7) {
	    my $mes= "Did not find expected information under METH: ".$self->method_string()." number ".$self->method_number()."\n" ;
	    print $mes."\n" unless $self->ignore_missing_files;
	    $self -> parsing_error( message =>$mes  );
	    return;
	  }
	  carp("rewinding to first position..." );
	} else {
	  $self->lstfile_pos($start_pos);
	  if ($self->classical_method() and $estprint) {
	    $self->parameter_path(\@parameter_vectors);
	    $self->gradient_path(\@gradient_vectors);
	    $self->zero_gradients($zero_gradients);
	    my $final_zero_gradients = 0;
	    foreach my $grad ( @{$self->final_gradients} ) {
	      $final_zero_gradients++ if $grad == 0;
	    }
	    $self->final_zero_gradients($final_zero_gradients);
	    $self->hessian_reset($hessian_reset);
	  }
	}
      }
# line 2291 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_iteration_path');
	# End of Non-Dia code #

}

sub _scan_to_meth {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'scan_only' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->_scan_to_meth: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_scan_to_meth: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_scan_to_meth: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_scan_to_meth: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_scan_to_meth: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $scan_only = defined $parm{'scan_only'} ? $parm{'scan_only'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _scan_to_meth');
# line 820 "lib/output/problem/subproblem_subs.pm" 
{
	my $start_pos = $self->lstfile_pos;

	my $method_exp = '^ #METH:\s*(.*)';
	my $term_exp = '^ #TERM:';
	my $tere_exp = '^ #TERE:';
	my $objt_exp = '^ #OBJT:';
	my $check_next_to_last_method = 0;
	my $found_next_to_last_method = 0;
	my $method_counter = 0;
	my $found_any_meth = 0;
	my $read_terminated_by_obj = 1;

	if ($self->method_string =~ /Objective Function Evaluation by Importance Sampling/ ) {
		$check_next_to_last_method = 1;
	}

	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) { #Large reading loop
	    if( /$method_exp/ ) {
		$found_any_meth = 1;
		$method_counter++;
		my $string = $1;
		$string =~ s/\s*$//; #remove trailing spaces
		if ($string eq 'Chain Method Processing') {
		    $read_terminated_by_obj = 0;
		} else {
		    $read_terminated_by_obj = 1;
		}
		if ($method_counter == scalar(@{$self->input_problem()->estimations()})) {
			#check that strings also match
			if ($string =~ /\(Evaluation\)/) {
				$self->estimation_step_run(0);
			}
			unless (($string =~ $self->method_string) or ($string eq $self->method_string) or ($self->method_string =~ $string  )) {
				croak("METH number $method_counter in subprob has string\n"."$string ".
						"instead of expected\n".$self->method_string);
		    }
		    $start_pos = $start_pos - 1; #undo ++ in loop head, leave directly at #METH line
		    last;
		} elsif ($method_counter == (scalar(@{$self->input_problem()->estimations()}) - 1)
			and $check_next_to_last_method) {
		    $found_next_to_last_method = 1;
		}
	    } elsif( /0HESSIAN OF POSTERIOR DENSITY IS NON-POSITIVE-DEFINITE DURING SEARCH/ ) {
		# This is an error that stops the execution
		$self->finished_parsing(1);
		last;
	    } elsif ( /$term_exp/ ) {
				if ($found_next_to_last_method) {
		  #inner loop to find termination status of next to last step
			while ( $_ = @{$self->lstfile}[ $start_pos ] ) {
			if ( /$tere_exp/ ) {
			    last; #break inner loop
			}
			if ( /(PORTION|OPTIMIZATION|BURN-IN|EXPECTATION ONLY PROCESS)( WAS | )NOT COMPLETED/ ) {
				if ( /USER INTERRUPT/ ) {
					$self->next_to_last_step_successful = 1;
				} else {
					$self->next_to_last_step_successful = 0;
					last; #do not read anything more, this is a failure
				}
			} elsif ( /(PORTION|OPTIMIZATION|BURN-IN|EXPECTATION ONLY PROCESS)( WAS | )NOT TESTED/ ) {
				$self->next_to_last_step_successful = 1;
			} elsif ( /(PORTION|OPTIMIZATION|BURN-IN|EXPECTATION ONLY PROCESS)( WAS | )COMPLETED/ ) {
				$self->next_to_last_step_successful = 1;
			}
			$start_pos++;
			if ( $start_pos > $#{$self->lstfile} ) { #we found end of file
			    #EOF This should not happen, raise error
			    my $errmess = "Reached end of file while scanning for termination message of next to last #METH\n";
			    carp($errmess."$!" );
			    $self -> parsing_error( message => $errmess."$!" );
			    return; #not enough to break inner loop, must return
			}

				}
		}
	    }elsif ( $read_terminated_by_obj and /0PROGRAM TERMINATED BY OBJ/ ) {
		# This is an error message which terminates NONMEM. We
		# return after reading the minimization message
		$self -> minimization_successful(0);
		$self -> finished_parsing(1);
		$self -> _read_minimization_message();
		last;
	    }elsif( /0HESSIAN OF POSTERIOR DENSITY IS NON-POSITIVE-DEFINITE DURING SEARCH/ ) {
		# This is an errror that stops the execution
		$self -> finished_parsing(1);
		last;
	    } elsif ((not $found_any_meth) and $self->simulationstep and /\(EVALUATION\)/ ) {
		#when simulation step sometimes no meth printed, do not look for it
		$self->estimation_step_run(0);
		last;
	    } elsif ((not $found_any_meth) and $self->simulationstep and /$objt_exp/ ) {
		#when simulation step sometimes no meth printed, do not look for it
		#this is fishy, but do not bail out yet
		$start_pos = $start_pos - 2; #leave at name of est meth line
		$self->estimation_step_run(0);
		last;
	    }

	    if( $start_pos > $#{$self->lstfile} ) { #we found end of file
		#EOF This should not happen, raise error
		my $errmess = "Reached end of file while scanning for #METH\n";
		carp($errmess."$!" );
		$self -> parsing_error( message => $errmess."$!" );
		last;
	    }

	}			#End of large reading loop
    

	$self->lstfile_pos($start_pos);
	
}
# line 2443 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _scan_to_meth');
	# End of Non-Dia code #

}

sub _read_npomegas {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_npomegas');
# line 1207 "lib/output/problem/subproblem_subs.pm" 
{
    my $start_pos = $self->lstfile_pos;
    my $success = 0;
    my $npofvarea = 0;
    my $nparea= 0;
    my $npetabararea = 0;
    my $npomegarea = 0;
    my ( @npetabar, @npomega, @T, $i );
    
    while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
      $nparea = 1 if /NONPARAMETRIC ESTIMATE/;
      last if ( /THERE ARE ERROR MESSAGES IN FILE PRDERR/ and $nparea );
      last if ( /^1/ and $nparea );
      last if ( /^1NONLINEAR/ and $nparea );
      last if ( /^[A-W]/ and $nparea );
      
      if (/MINIMUM VALUE OF OBJECTIVE FUNCTION/ and $nparea ) { #Only nonmem6 version
				$npofvarea = 1;
      } elsif ( /EXPECTED VALUE OF ETA/ and $nparea ) {
				$npetabararea = 1;
				$npofvarea = 0;
				$success = 1;
      } elsif ( /COVARIANCE MATRIX OF ETA/ and $nparea ) {
				$npomegarea = 1;
				$npetabararea = 0;
      } elsif ( /CORRELATION MATRIX OF ETA/ and $nparea ) {
				$npomegarea = 0;
				$npetabararea = 0;
      }
      if ($npofvarea) {
				if ( /^\s+(-?\d*\.\d*)/) { #Assignment of attribute at the spot
					$self->npofv($1);
					$npofvarea = 0;
				}
      } elsif($npetabararea) {
				if( /^\s*-?\d*\.\d*/) {
					@T = split(' ',$_);
					for $i (0..(@T-1)){
						if($T[$i] ne '.........') {
							$T[$i] = eval($T[$i]);
						} else {
							$T[$i] = undef;
						}
					}
					push(@npetabar, @T);
				}
			} elsif($npomegarea) {
				if ( /^(\+|\s{2,})/) {
					next if /ET/;
					@T = split(' ',$_);
					shift @T if $T[0] eq '+';
					for  $i (0..(@T-1)){
						if($T[$i] ne '.........') {
							$T[$i] = eval($T[$i]);
						} else {
							$T[$i] = undef;
						}
					}
					push(@npomega, @T);
				}
			}
		}
    $self->npetabars([@npetabar]);
    $self->npomegas([@npomega]);
    unless ( $success ) {
			carp("rewinding to first position..." );
		} else {
			$self->lstfile_pos($start_pos);
		}
}
# line 2525 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_npomegas');
	# End of Non-Dia code #

}

sub _read_ofv {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_ofv');
# line 1284 "lib/output/problem/subproblem_subs.pm" 
{
    my $start_pos = $self->lstfile_pos;

    while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
      if ( / PROBLEM NO\.:\s*\d+\n/ ) {
				$self -> parsing_error( message => "Error in reading the OFV!\nNext problem found" );
				return;
			} elsif (/^\s*0PROGRAM TERMINATED BY OBJ/) {
	  		# This is an error message which terminates NONMEM. 
				$self -> finished_parsing(1);
				$self -> _read_minimization_message();
				return;
			} elsif ( $start_pos >= scalar @{$self->lstfile} ) {
				$self -> parsing_error( message => "Error in reading the OFV!\nEOF found\n" );
				return;
			}

			if ( /^\s\#OBJV:/  || /^\s\*{50}\s+/ ) {
				(undef, my $ofvt, undef) = split(' ', $_, 3);
				if ( $ofvt =~ /\*\*\*\*\*\*/ ) {
					$self -> {'ofv'} = undef;	#FIXME: Can be done in moose with a clearer. Or perhaps it is possible to set to 0.
				} else {
					$_ = @{$self->lstfile}[ $start_pos++];
					if (0) {
	    #only true for BAYES
	    (undef, my $objs, undef) = split(' ',$_,3);
	    if ( $objs =~ /\*\*\*\*\*\*/ ) {
	      $self -> ofv($ofvt);
	    } else {
	      my $dic = $ofvt+$objs*$objs/2;
	      $self -> ofv($dic);
	      $self -> dic($dic);
	    }
	  }else{
	    $self -> ofv($ofvt);
	  }
	  $start_pos--;
	}
	last;
      }
    }
    $self->lstfile_pos($start_pos);
}
# line 2580 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_ofv');
	# End of Non-Dia code #

}

sub _read_sethomsi {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_sethomsi');
# line 1390 "lib/output/problem/subproblem_subs.pm" 
      {
	my $start_pos = $self->lstfile_pos;
	my $success  = 0;
	
	my $thetarea = 0;
	my $omegarea = 0;
	my $sigmarea = 0;
	my ( @setheta, @T, $i, $tmp );
	my ( @raw_seomega, @raw_sesigma );
	my (%sethetacoordval,%seomegacoordval,%sesigmacoordval);
	my $found_estimates = 0;
	
	# _read_thomsi should leave us right at where we should start reading
	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	  chomp;
	  if ( /THETA - VECTOR OF FIXED EFFECTS PARAMETERS/ ) {
	    $thetarea = 1;
	    $found_estimates=1;
	    next;
	  }
	  if ( /OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS/ ) {
	    $omegarea = 1;
	    $thetarea = 0;
	    $found_estimates=1;
	    next;
	  }
	  if ( /SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS/ ) {
	    $sigmarea = 1;
	    $omegarea = 0;
	    $thetarea = 0;
	    $found_estimates=1;
	    next;
	  }
	  if ( /CORR MATRIX FOR RANDOM EFFECTS/ ) {
	    $sigmarea = 0;
	    $omegarea = 0;
	    $thetarea = 0;
	    next;
	  }
	  if ( ($found_estimates) and 
	       /COVARIANCE MATRIX OF ESTIMATE/ ) {
	    # This is fine, we should end up here after reading the
	    # estimates
	    $success = 1;
	    last;
	  }

	  if ( /T MATRIX/ or
	       /R MATRIX/ ) {
	    # This is also fine, if those matrices were output, we
	    # should end up here before we could start reading the
	    # estimates
	    $success = 1;
	    last;
	  }

	  if ( /NONPARAMETRIC ESTIMATE/ ) {
	    # This is also fine. If the nonparametric step is run, we
	    # should end up here regardless of the termination status
	    # of the covariance step.
	    $success = 1;
	    last;
	  }

	  if ( /^1NONLINEAR/ ) {
	    $self -> parsing_error( message => "Error in reading the standard error of the parameter ".
				    "estimates!\nFound: $_" );
	    return;
	  }

	  if ( /THERE ARE ERROR MESSAGES IN FILE PRDERR/ ) {
	    # This is an NONMEM error message and is ok (to find), but
	    # it means that we can stop parsing the file
	    $self -> finished_parsing(1);
	    last;
	  }




	  if ( $thetarea and /^\s*-?\d*\.\d*/ ) {
	    @T = split(' ',$_);
	    for $i (0..(@T-1)) {
	      if($T[$i] ne '.........') {
					$tmp = eval($T[$i]);
	      } else {
					$tmp = 'NA';
	      }
	      $T[$i] = $tmp ;
	    }
	    push(@setheta,@T);
	  }elsif($omegarea and /^(\+|\s{2,})/) {
	    next if /ET/;
	    @T = split(' ',$_);
	    shift @T if $T[0] eq '+';
	    for  $i (0..(@T-1)) {
	      if($T[$i] ne '.........') {
					$tmp = eval($T[$i]);
	      } else {
					$tmp = 'NA';
	      }
	      $T[$i] = $tmp ;
	    }
	    push(@raw_seomega,@T);
	  }elsif($sigmarea and /^(\+|\s{2,})/) {
	    next if /EP/;
	    @T = split(' ',$_);
	    shift @T if $T[0] eq '+';
	    for $i (0..(@T-1)) {
	      if ($T[$i] ne '.........') {
					$tmp = eval($T[$i]);
	      } else {
					$tmp = 'NA';
	      }
	      $T[$i] = $tmp ;
	    }
	    push(@raw_sesigma,@T);
	  }
	  
	  if ( $start_pos >= scalar @{$self->lstfile} ) {
	    $self -> parsing_error( message => "Error in reading the standard error of the parameter ".
				    "estimates!\nEOF found\n" );
	    return;
	  }
	}
	
	my $index = 1;
	foreach my $th (@setheta){
	  $sethetacoordval{'THETA'.$index} = $th unless ($th eq 'NA');
	  $index++;
	}

	#any omega matrix form
	my $row = 1;
	my $col = 1;

	foreach my $val (@raw_seomega){
	  my $label = 'OMEGA('.$row.','.$col.')';
	  if ($val ne 'NA'){
	    $seomegacoordval{$label} = $val;
	  }
	  $col++;
	  if ($col > $row){
	    $row++;
	    $col=1;
	  }
	}

	#any sigma matrix form
	$row = 1;
	$col = 1;

	foreach my $val (@raw_sesigma){
	  my $label = 'SIGMA('.$row.','.$col.')';
	  if ($val ne 'NA') {
	    $sesigmacoordval{$label} = $val;
	  }
	  $col++;
	  if ($col > $row){
	    $row++;
	    $col=1;
	  }
	}

	$self->sethetacoordval(\%sethetacoordval);
	$self->seomegacoordval(\%seomegacoordval);
	$self->sesigmacoordval(\%sesigmacoordval);


	if ( scalar @setheta <= 0 ) {
	  $self->covariance_step_successful(0);
	} else {
	  $self->covariance_step_successful(1);
	}

	unless ( $success ) {
	  carp("No standard errors for thetas, sigmas or omegas." );
	} else {
	  $self->lstfile_pos($start_pos);
	}
      }
# line 2773 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_sethomsi');
	# End of Non-Dia code #

}

sub _read_significant_digits {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_significant_digits');
# line 1351 "lib/output/problem/subproblem_subs.pm" 
{
    my $start_pos = $self->lstfile_pos;
    
    while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
      if ( / PROBLEM NO\.:\s*\d+\n/ ) {
				$self -> parsing_error( message => "Error in reading the number of ".
						"significant digits!\nNext problem found" );
				return;
      }
      
      if ( $start_pos >= scalar @{$self->lstfile} ) {
				$self -> parsing_error( message => "Error in reading the number of ".
						"significant digits!\nEOF found\n" );
				return;
      }
      
      if ( /^1/ or /MINIMUM VALUE OF OBJECTIVE FUNCTION/ ) {
				$self -> parsing_error( message => "Error in reading the number of ".
						"significant digits!\nOFV found" );
				return;
      }

      if ( / NO. OF SIG. DIGITS UNREPORTABLE/ ) {
	# This is ok
				last;
      }

      if ( / NO. OF SIG. DIGITS IN FINAL EST.:\s*(-?\d*\.*\d*)/ ){
				$self -> significant_digits($1);
				last;
      }
    }
    $self->lstfile_pos($start_pos);
}
# line 2819 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_significant_digits');
	# End of Non-Dia code #

}

sub _read_simulation {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_simulation');
# line 1576 "lib/output/problem/subproblem_subs.pm" 
{
	# The simulation step is optional.
	my $start_pos = $self->lstfile_pos;
	while ( $_ = @{$self->lstfile}[ $start_pos ++ ] ) {
		if ( /MINIMUM VALUE OF OBJECTIVE FUNCTION/ ) {
			last;
		}
		if ( /^\s*MONITORING OF SEARCH:/) {
			last;
		}
		if ( /\s*SIMULATION STEP PERFORMED/ ) {
			$self->simulationstep(1);
			last;
		}
	}
      
	if ( $self->simulationstep ) {
		$self->lstfile_pos($start_pos);
	} 
}
# line 2851 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_simulation');
	# End of Non-Dia code #

}

sub _read_term {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_term');
# line 1602 "lib/output/problem/subproblem_subs.pm" 
{
	my $start_pos = $self->lstfile_pos;
	my $success_pos;
	my $success = 0;
	my $obj_exp = '^ #OBJT:';
	my $pred_exit_code = 0;
	my $found_fail = 0;
	my $no_check_minim = 0;
	$self -> minimization_successful(0);

	if (defined $self->next_to_last_step_successful) {
	  $self -> minimization_successful($self->next_to_last_step_successful);
	  $no_check_minim = 1;
	}
	

	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	  $self->s_matrix_singular(1) if ( /^0S MATRIX ALGORITHMICALLY SINGULAR/ );
	  if ( /^0R MATRIX ALGORITHMICALLY SINGULAR/ or 
	       /^0S MATRIX ALGORITHMICALLY SINGULAR/ ) {
	    $self -> covariance_step_warnings(1);
	    next;
	  }
	  if ( /^0ESTIMATE OF THETA IS NEAR THE BOUNDARY AND/ or
	       /0PARAMETER ESTIMATE IS NEAR ITS BOUNDARY/ ) {
	    $self -> estimate_near_boundary(1);
	    next;
	  }
	  if ( /ROUNDING ERRORS/ ) {
	    $self -> rounding_errors(1);
	    next;
	  }
	  if ( /0COVARIANCE STEP ABORTED/ ) {
	    $self -> covariance_step_run(0);
	    next;
	  }

	  if ( / THIS MUST BE ADDRESSED BEFORE THE COVARIANCE STEP CAN BE IMPLEMENTED/ ) {
	    $self -> covariance_step_run(0);
	  }

          # "0ERROR RMATX-  1" should (if it exists) occur after the minim. succ message
          if ( /0ERROR RMATX-  1/ ) {
            $self -> minimization_successful(0);
						next;
          }

	  if ( /^0MINIMIZATION SUCCESSFUL/ ) {
	    $self -> minimization_successful(1);
	    $success = 1;
	    $self->lstfile_pos($start_pos - 1);
	    next;
	  }

	  if ( /(PORTION|OPTIMIZATION|BURN-IN|EXPECTATION ONLY PROCESS)( WAS | )NOT COMPLETED/ ) {
	    if ( /USER INTERRUPT/ ) {
	      $self -> minimization_successful(1) unless ($found_fail or $no_check_minim );
	    } else {
	      $self -> minimization_successful(0) unless ($no_check_minim);
	      $found_fail = 1;
	    }
	    $self->lstfile_pos($start_pos - 1) unless ($success);
	    $success = 1;
	    next;
	  }
	  if ( /(PORTION|OPTIMIZATION|BURN-IN|EXPECTATION ONLY PROCESS)( WAS | )NOT TESTED/ ) {
	    $self -> minimization_successful(1) unless ($found_fail or $no_check_minim );
	    $self->lstfile_pos($start_pos - 1) unless ($success);
	    $success = 1;
	    next;
	  }
	  if ( /(PORTION|OPTIMIZATION|BURN-IN|EXPECTATION ONLY PROCESS)( WAS | )COMPLETED/ ) {
	    $self -> minimization_successful(1) unless ($found_fail or $no_check_minim );
	    $self->lstfile_pos($start_pos - 1) unless ($success);
	    $success = 1;
	    next;
	  }

	  if ( /^0MINIMIZATION TERMINATED/ ) {
#	      print "found terminated with old success $success\n";
	    $self -> minimization_successful(0);
	    $self -> covariance_step_run(0);
	    $self->lstfile_pos($start_pos - 1) unless ($success);
	    $success = 1;
	    next;
	  }

	  if ( /^0SEARCH WITH ESTIMATION STEP WILL NOT PROCEED/ ) {
	    $self -> minimization_successful(0);
	    $self -> covariance_step_run(0);
	    $success = 1;
	    $self -> finished_parsing(1);
	    return;
	  }

	  if ( /0PRED EXIT CODE = 1/ ) {
	    # This is an error message but the severity of it depends
	    # on the origin of it
	    $pred_exit_code = 1;
	    next;
	  }

	  if ( $pred_exit_code and 
	       /MESSAGE ISSUED FROM SIMULATION STEP/ ) {

	    # These are probably not needed if we match the sim. step string above
#		 /ERROR IN TRANS4 ROUTINE: (.*)  IS ZERO/ or
#	         /ERROR IN TRANS4 ROUTINE: (.*)  IS NEGATIVE/ ) ) {

	    # This is an error message which terminates NONMEM. We
	    # return after reading the minimization message
	    $self -> minimization_successful(0);
	    $self -> finished_parsing(1);
	    $self -> _read_minimization_message();
	    return;
	  }

	  if ( /0PROGRAM TERMINATED BY OBJ/ ) {
	    # This is an error message which terminates NONMEM. 
	      #unless it was from the COVARIANCE step. If $success already true
	      #assume it was covstep
	      #We
	    # return after reading the minimization message
#	      print "found term obj\n";
	      unless ($success){
		  $self -> minimization_successful(0);
		  $self -> finished_parsing(1);
		  $self -> _read_minimization_message();
		  return;
	      }
	  }	    

	  if ( /MINIMUM VALUE OF OBJECTIVE FUNCTION/ ) {
	    carp("Hmmm, reached the OFV area" );
	    last;
	  }
	  if ( /$obj_exp/ ) {
	    last;
	  }
	}
	if ($success) {
	  carp("Found a minimization statement" );
	  $self -> _read_minimization_message();
	  $self -> _read_eval()                 if ($self -> parsed_successfully() and $self->classical_method());
	  $self -> _read_significant_digits()   if ($self -> parsed_successfully() and $self->classical_method());
	} else {
	  carp("No minimization/termination statement found" ); #Back to starting line
	  $self -> parsing_error( message => "Error in reading minim/term statement!\n$!" );
	  return;
	}
      }
# line 3014 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_term');
	# End of Non-Dia code #

}

sub _read_minimization_message {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_minimization_message');
# line 1760 "lib/output/problem/subproblem_subs.pm" 
      {
	# This method is called from _read_term() and the listfile_pos
	# variable should leave us right at the start of the
	# minimization message. _read_eval() and
	# _read_significant_digits() are called after this method so
	# we need to rewind to our starting position
	my $success = 0;
	my (@mess, @etabar,@pval,@etashrink,@epsshrink);
    
	my $start_pos = $self->lstfile_pos;
	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	  if ( / PROBLEM NO\.:\s*\d+\n/ ) {
	    $self -> parsing_error( message => "Error in reading minimization ".
				    "message!\nNext problem found" );
	    return;
	  }
	     
	  if ( $start_pos >= scalar @{$self->lstfile} ) {
	      if ($self->finished_parsing() == 1){
		  $success = 1;
		  last;
	      }else{
		  $self -> parsing_error( message => "Error in reading minimization ".
					  "message!\nEOF found" );
		  return;
	      }
	  }

	  if ( /^1/ or /MINIMUM VALUE OF OBJECTIVE FUNCTION/ or /^\s*\#OBJT:/ ) {
	    # This is ok. We would expect to end up here probably
	    # catching the '1' above
	    carp("Found minimization area and reached ".$_ );
	    $success = 1;
	    last;
	  }

	  push @mess, $_  unless (/^\s*\#TERE:/);
	}

	push( @{$self->minimization_message}, @mess );		# minimization_message is default set to empty array.

	my @temp;
	my $etabar_found = 0;
	for( my $i = 0; $i <= $#mess; $i++ ) {
	  my $line = $mess[$i];

	  if ( $etabar_found or ($line =~ /\s+ETABAR:\s+/) ) {
	    $etabar_found = 1;
	    $line =~ s/ETABAR://;

	    last unless ( $line =~ s/^\s+//);
	    last unless ( $line =~ /\d/);
	    last if( $line =~ /^1/);
	    last if( $line =~ /[a-zA-DF-Z]/);

	    @temp = split(/\s+/,$line);
	    push @etabar, @temp;
	  }
	}
	# Initialize the attribute only if we have found any data
	$self->etabar(\@etabar) if ( scalar(@etabar) > 0 );
    
	my $pval_found;
	for( my $i = 0; $i <= $#mess; $i++ ) {
		my $line = $mess[$i];
		if ( $pval_found or ($line =~ /\s+P VAL\.:\s+/ ) ) {
			$pval_found = 1;
			$line =~ s/P VAL\.://;

			last unless ( $line =~ s/^\s+//);
			last unless ( $line =~ /\d/);
			last if( $line =~ /^1/);
			last if( $line =~ /[a-zA-DF-Z]/);
			@temp = split(/\s+/,$line);
			push @pval, @temp;	
		}
	}
	$self->pval(\@pval);

	my $etashrink_found = 0;
	for( my $i = 0; $i <= $#mess; $i++ ) {
		my $line = $mess[$i];

		if ( $etashrink_found or ($line =~ /\s+ETAshrink\(\%\):\s+/) ) {
			$etashrink_found = 1;
			$line =~ s/ETAshrink\(\%\)://;

	    last unless ( $line =~ s/^\s+//);
	    last unless ( $line =~ /\d/);
	    last if( $line =~ /^1/);
	    last if( $line =~ /[a-zA-DF-Z]/);

	    @temp = split(/\s+/,$line);
	    foreach my $tmp (@temp){
	      push (@etashrink, $tmp); #percent	
	    }
	  }
	}
	# Initialize the attribute only if we have found any data
	$self -> shrinkage_eta(\@etashrink) if (scalar(@etashrink) > 0);

	my $epsshrink_found = 0;
	for( my $i = 0; $i <= $#mess; $i++ ) {
	  my $line = $mess[$i];

	  if ( $epsshrink_found or ($line =~ /\s+EPSshrink\(\%\):\s+/) ) {
	    $epsshrink_found = 1;
	    $line =~ s/EPSshrink\(\%\)://;

	    last unless ( $line =~ s/^\s+//);
	    last unless ( $line =~ /\d/);
	    last if( $line =~ /^1/);
	    last if( $line =~ /[a-zA-DF-Z]/);

	    @temp = split(/\s+/,$line);
	    foreach my $tmp (@temp){
	      push (@epsshrink, $tmp); #percent
	    }
	  }
	}
	# Initialize the attribute only if we have found any data
	$self -> shrinkage_eps(\@epsshrink) if ( scalar(@epsshrink) > 0 );

	unless ( $success ) {
	  carp("No minimization message found" );
	}
      }
# line 3153 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_minimization_message');
	# End of Non-Dia code #

}

sub _read_thomsi {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_thomsi');
# line 1894 "lib/output/problem/subproblem_subs.pm" 
      {
	my $start_pos = $self->lstfile_pos;
	my $success = 0;
	my $thetarea = 0;
	my $omegarea = 0;
	my $sigmarea = 0;
	my ( @theta, @omega, @raw_omega, @sigma, @raw_sigma, @T, $i, $tmp );
	my ( @thetanames, %thetacoordval);
	my ( @omeganames, %omegacoordval);
	my ( @sigmanames, %sigmacoordval);
	my $found_estimates = 0;

	# _read_ofv should leave us right at where we should start reading
	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	  chomp;
	  if ( /THETA - VECTOR OF FIXED EFFECTS PARAMETERS/ ) {
	    $thetarea = 1;
	    $found_estimates = 1;
	    next;
	  }
	  if ( /OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS/ ) {
	    $omegarea = 1;
	    $thetarea = 0;
	    $sigmarea = 0;
	    $found_estimates = 1;
	    next;
	  }
	  if ( /SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS/ ) {
	    $sigmarea = 1;
	    $thetarea = 0;
	    $omegarea = 0;
	    $found_estimates = 1;
	    next;
	  }
	  if ( /- CORR MATRIX FOR RANDOM EFFECTS -/ ) {
	    $sigmarea = 0;
	    $thetarea = 0;
	    $omegarea = 0;
	    next;
	  }

	  if ( ($found_estimates) and 
	       # For some reason, NONMEM prints a '1' (i.e. /^1$/)
	       # after the thirteenth omega row. I other words, we
	       # cannot use /^1$/ as check for omega area ending.
	       ( 
                 #/^1\s*$/ or
		 /STANDARD ERROR OF ESTIMATE/ or
		 /NONPARAMETRIC ESTIMATE/ or
		 /T MATRIX/ or
		 /R MATRIX/ or
		 /TABLES OF DATA AND PREDICTIONS/ )) {
	    # This is fine, we should end up here after reading the estimates
	    $success = 1;
	    last;
	  }

	  if ( /^1NONLINEAR/ ) {
	    $self -> parsing_error( message => "Error in reading the parameter ".
				    "estimates!\nFound: $_" );
	    return;
	  }

	  if ( /THERE ARE ERROR MESSAGES IN FILE PRDERR/ ) {
	    # This is an NONMEM error message and is ok (to find), but
	    # it means that we can stop parsing the file
	    $self -> finished_parsing(1);
	    last;
	  }


	  if( $thetarea and /^\s*-?\d*\.\d*/ ) {
	    @T = split(' ',$_);
	    for $i (0..(@T-1)) {
	      if($T[$i] ne '.........') {
					$tmp = eval($T[$i]);
	      } else {
					$tmp = 'NA';
	      }
	      $T[$i] = $tmp ;
	    }
	    push(@theta, @T);
		} elsif($omegarea and /^(\+|\s{2,})/) {
			next if /ET/;
	    @T = split(' ',$_);
	    shift @T if $T[0] eq '+';
	    for  $i (0..(@T-1)) {
	      if($T[$i] ne '.........') {
					$tmp = eval($T[$i]);
	      } else {
					$tmp = 'NA';
	      }
	      $T[$i] = $tmp ;
	    }
	    push(@raw_omega,@T);
	  } elsif($sigmarea and /^(\+|\s{2,})/) {
	    next if /EP/;
	    next if /^\s*$/;
	    @T = split(' ',$_);
	    shift @T if $T[0] eq '+';
	    for  $i (0..(@T-1)) {
	      if($T[$i] ne '.........') {
					$tmp = eval($T[$i]);
	      } else {
					$tmp = 'NA';
	      }
	      $T[$i] = $tmp ;
	    }
	    push(@raw_sigma, @T);
	    
	  }
	  if ( $start_pos >= scalar @{$self->lstfile} ) {
	    # This is a valid match. Sometimes, the list file ends
	    # with the parameter estimates
	    $self -> finished_parsing(1);
	  }
	}

	my $index = 1;
	foreach my $th (@theta){
	  push (@thetanames, 'THETA' . $index);
	  $thetacoordval{'THETA'.$index} = $th unless ($th eq 'NA');
	  $index++;
	}

	#any omega matrix form, store all defined elements
	my $row = 1;
	my $col = 1;
	foreach my $val (@raw_omega){
	  if ($val ne 'NA'){
	    my $label = 'OMEGA('.$row.','.$col.')';
	    push (@omeganames, $label);
	    $omegacoordval{$label} = $val;
	  }
	  $col++;
	  if ($col > $row) {
	    $row++;
	    $col=1;
	  }
	}
	#any sigma matrix form, store all defined elements
	my $row = 1;
	my $col = 1;
	foreach my $val (@raw_sigma) {
	  if ($val ne 'NA'){
	    my $label = 'SIGMA('.$row.','.$col.')';
	    push (@sigmanames, $label);
	    $sigmacoordval{$label} = $val;
	  }
	  $col++;
	  if ($col > $row){
	    $row++;
	    $col = 1;
	  }
	}

	$self->thetacoordval(\%thetacoordval);
	$self->omegacoordval(\%omegacoordval);
	$self->sigmacoordval(\%sigmacoordval);

	unless ( $success ) {
	  carp("No thetas, omegas or sigmas found" );
	} else {
	  $self->lstfile_pos($start_pos - 1);
	}

}
# line 3332 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_thomsi');
	# End of Non-Dia code #

}

sub _read_tables {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _compute_comegas_or_csigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'omega_or_sigma' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->_compute_comegas_or_csigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_compute_comegas_or_csigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_compute_comegas_or_csigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_compute_comegas_or_csigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_compute_comegas_or_csigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $omega_or_sigma = $parm{'omega_or_sigma'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _compute_comegas_or_csigmas');
# line 346 "lib/output/problem/subproblem_subs.pm" 
{
	@_ = (); #otherwise params will be sent on to cmp_coords
	# This method transforms omegas or sigmas.
	#zeros are stored. 0 may mean not estimated
	my $error_printed = 0;
  if ( ($omega_or_sigma eq 'omega' and defined $self->omegacoordval) or ($omega_or_sigma eq 'sigma' and defined $self->sigmacoordval) ) {
		my %valueshash;
		if ($omega_or_sigma eq 'omega') {
			%valueshash = %{$self->omegacoordval};
		} else {
			%valueshash = %{$self->sigmacoordval};
		}
    my $ref = eval('$self -> ' . $omega_or_sigma . 'names()');
    my @names = @{$ref};
    foreach my $name (@names) {
      my $omega_or_sigma_value = undef;
      unless ($name =~ /\(([\d]+)\,([\d]+)\)/) {
				croak("Unknown form of $omega_or_sigma name: $name");
			}
      my $row = $1;
      my $col = $2;
      $omega_or_sigma_value = undef;
      unless ( $valueshash{$name} eq 'NA') {
				if ($row == $col) {
	  			#diagonal
	  			if ( $valueshash{$name} >= 0 ) {
	    			$omega_or_sigma_value = sqrt( $valueshash{$name} );
	  			} else {
	    			ui -> print( category => 'all', message  => "Warning: cannot take the square root of $omega_or_sigma with value " . $valueshash{$name} );
	  			}
				} else {
	  			# If we are off the diagonal, we need to find two
	  			# on-diagonal omegas, one on the same column and one on
	  			# the same row.
	  			my $name_a = uc($omega_or_sigma) . '(' . $row . ',' . $row . ')';
					my $name_b = uc($omega_or_sigma) . '(' . $col . ',' . $col . ')';
					if ((not (defined $valueshash{$name_a} and defined $valueshash{$name_b}) or $valueshash{$name_a} eq 'NA' or $valueshash{$name_b} eq 'NA') and ($error_printed < 3)) {
						ui -> print( category => 'all',
			   			message  => "Error, missing element $name_a and/or $name_b while $name is defined. Was the delimiter set to anything other than space in \$EST? ");
	      		$error_printed++;
					} else {
						$omega_or_sigma_value = $valueshash{$name};
						my $denominator = $valueshash{$name_a} * $valueshash{$name_b};
						if ( $denominator <= 0.00001 ) { # To avoid division by zero
							$omega_or_sigma_value = undef;
						} elsif ( $omega_or_sigma_value >= sqrt($denominator) ) { 
							# This rounding handles cases when the offdiagonals
							# are greater or equal to one.
							$omega_or_sigma_value = $omega_or_sigma_value / ( int( 10000 * sqrt($denominator) ) / 10000 );
						} else {
							$omega_or_sigma_value = $omega_or_sigma_value / sqrt($denominator);
						}
					}
				}
			}
			if ($omega_or_sigma eq 'omega') {
				$self->comegas([]) unless defined $self->comegas;
				push @{$self->comegas}, $omega_or_sigma_value;
			} else {
				$self->csigmas([]) unless defined $self->csigmas;
				push @{$self->csigmas}, $omega_or_sigma_value;
			}
    }
  }
}
# line 3443 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _compute_comegas_or_csigmas');
	# End of Non-Dia code #

}

sub _compute_comegas {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _compute_comegas');
# line 417 "lib/output/problem/subproblem_subs.pm" 
{
  $self -> _compute_comegas_or_csigmas( omega_or_sigma => 'omega' );
}
# line 3458 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _compute_comegas');
	# End of Non-Dia code #

}

sub _compute_csigmas {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _compute_csigmas');
# line 426 "lib/output/problem/subproblem_subs.pm" 
{
  $self -> _compute_comegas_or_csigmas( omega_or_sigma => 'sigma' );
}
# line 3473 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _compute_csigmas');
	# End of Non-Dia code #

}

sub _compute_cvsesigma {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _compute_cvsesigma');
# line 471 "lib/output/problem/subproblem_subs.pm" 
{
	if ( (defined $self->sigmacoordval) and (defined $self->sesigmacoordval) ) {
		my %valueshash = %{$self->sigmacoordval};
		my @names = @{$self->sigmanames()};
		my %sehash = %{$self->sesigmacoordval};
		my @cvse_values;

		foreach my $name (@names) {
			if( defined $sehash{$name} and $sehash{$name} ne 'NA' and $valueshash{$name} ne 'NA' and $valueshash{$name} != 0) {
				if( ($sehash{$name} == 'INF' or $sehash{$name} == '-INF') and ($valueshash{$name} == 'INF' or $valueshash{$name} == '-INF') ) {
					push @cvse_values, undef;
				} elsif ( $sehash{$name} == 'INF' ) {
					push @cvse_values, 'INF';
				} elsif ( $sehash{$name} == '-INF' ) {
					push @cvse_values, '-INF';
				} elsif ( $valueshash{$name} == 'INF' or $valueshash{$name} == '-INF' ) {
					push @cvse_values, 0;
				} else {
					push @cvse_values,$sehash{$name} / abs($valueshash{$name});
				}
			} else {
				push @cvse_values, undef;
			}
		}

		$self -> cvsesigmas(\@cvse_values);
	}
}
# line 3513 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _compute_cvsesigma');
	# End of Non-Dia code #

}

sub _compute_cvseomega {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _compute_cvseomega');
# line 436 "lib/output/problem/subproblem_subs.pm" 
{
	if ( (defined $self->omegacoordval) and (defined $self->seomegacoordval) ) {
		my %valueshash = %{$self->omegacoordval};
		my @names = @{$self->omeganames()};
		my %sehash = %{$self->seomegacoordval};
		my @cvse_values;

		foreach my $name (@names) {
			if ( defined $sehash{$name} and $sehash{$name} ne 'NA' and $valueshash{$name} ne 'NA' and $valueshash{$name} != 0 ) {
				if ( ($sehash{$name} == 'INF' or $sehash{$name} == '-INF') and ($valueshash{$name} == 'INF' or $valueshash{$name} == '-INF') ) {
					push @cvse_values, undef;
				} elsif ( $sehash{$name} == 'INF' ) {
					push @cvse_values, 'INF';
				} elsif ( $sehash{$name} == '-INF' ) {
					push @cvse_values, '-INF';
				} elsif ( $valueshash{$name} == 'INF' or $valueshash{$name} == '-INF' ) {
					push @cvse_values, 0;
				} else {
					push @cvse_values, $sehash{$name} / abs($valueshash{$name});
				}
			} else {
				push @cvse_values, undef;
			}
		}

		$self -> cvseomegas(\@cvse_values);
	}
}
# line 3553 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _compute_cvseomega');
	# End of Non-Dia code #

}

sub _compute_cvsetheta {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _compute_cvsetheta');
# line 505 "lib/output/problem/subproblem_subs.pm" 
{
	if ( (defined $self->thetacoordval) and (defined $self->sethetacoordval) ) {
		my @thetas   = @{$self->thetas()};
		my @sethetas = @{$self->sethetas()};
		my @cvsethetas;
    
		if ( scalar @sethetas > 0 ) {
			foreach my $i (0..$#thetas) {
				if( defined $sethetas[$i] and ($sethetas[$i] ne 'NA') and defined $thetas[$i] and ($thetas[$i] ne 'NA')) {
					if( $thetas[$i] != 0 ) {
						push(@cvsethetas, $sethetas[$i] / abs($thetas[$i]));
					} elsif( $sethetas[$i] > 0 ) {
						push(@cvsethetas,'INF');
					} else {
						push(@cvsethetas,'-INF');
					}
				} else {
					push @cvsethetas, undef;
				}
			}
		}
    
		$self -> cvsethetas(\@cvsethetas);
	}

}
# line 3591 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _compute_cvsetheta');
	# End of Non-Dia code #

}

sub _rowcolind {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->_rowcolind: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_rowcolind: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_rowcolind: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_rowcolind: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_rowcolind: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $index = $parm{'index'};
	my $row;
	my $col;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _rowcolind');
# line 1333 "lib/output/problem/subproblem_subs.pm" 
{
	my $i = 1;
	my $found = 0;
	while ( not $found ) {
		my $test = $index - ($i - 1) * ($i) / 2;
	  if ( $test <= $i ) {
	    $row = $i;
	    $col = $test;
	    $found = 1;
	  }
	  $i++;
	}
}
# line 3644 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _rowcolind');
	# End of Non-Dia code #

	return $row ,$col;
}

sub thetas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> thetas');
# line 116 "lib/output/problem/subproblem_subs.pm" 
{
	if (defined $self->thetacoordval) {
    my %valueshash = %{$self->thetacoordval};
    my @names = @{$self->thetanames()};
    foreach my $name (@names) {
      push(@values, $valueshash{$name});
    }
  }
}
# line 3667 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> thetas');
	# End of Non-Dia code #

	return \@values;
}

sub est_thetanames {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> est_thetanames');
# line 129 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->output_matrix_headers) {
    my @names = @{$self->output_matrix_headers};
    foreach my $name (@names) {
      if ($name =~ /THETA/) {
				push(@values, $name);
      }
    }
  }
}
# line 3691 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> est_thetanames');
	# End of Non-Dia code #

	return \@values;
}

sub thetanames {
	my $self = shift;
	my @names;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> thetanames');
# line 168 "lib/output/problem/subproblem_subs.pm" 
{
  sub cmp_coords {
    if ($a =~ /THETA/) {
      return substr($a,5) <=> substr($b,5);
    } else {
      ($a.$b) =~ /\((\d+)\,(\d+)\)[A-Z]*\((\d+)\,(\d+)\)/;
      return $2+(($1-1)*$1)/2 <=> $4+(($3-1)*$3)/2; 
    }
  }
  if (defined $self->thetacoordval) {
    my %valueshash = %{$self->thetacoordval};
    @names = sort cmp_coords (keys %valueshash);
  }
}
# line 3719 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> thetanames');
	# End of Non-Dia code #

	return \@names;
}

sub sethetas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sethetas');
# line 186 "lib/output/problem/subproblem_subs.pm" 
{
  if ( (defined $self->thetacoordval) and (defined $self->sethetacoordval) ) {
    my %sehash = %{$self->sethetacoordval};
    my @names = @{$self->thetanames()};
    foreach my $name (@names) {
      my $val= defined $sehash{$name} ? $sehash{$name}: undef;
      push(@values,$val);
    }
  }
}
# line 3743 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sethetas');
	# End of Non-Dia code #

	return \@values;
}

sub omegas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> omegas');
# line 260 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->omegacoordval) {
    my %valueshash = %{$self->omegacoordval};
    my @names = @{$self->omeganames()};
    foreach my $name (@names) {
      push(@values, $valueshash{$name});
    }
  }
}
# line 3766 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> omegas');
	# End of Non-Dia code #

	return \@values;
}

sub est_omeganames {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> est_omeganames');
# line 142 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->output_matrix_headers) {
    my @names = @{$self->output_matrix_headers};
    foreach my $name (@names) {
      if ($name =~ /OMEGA/) {
				push(@values, $name);
      }
    }
  }
}
# line 3790 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> est_omeganames');
	# End of Non-Dia code #

	return \@values;
}

sub omeganames {
	my $self = shift;
	my @names;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> omeganames');
# line 272 "lib/output/problem/subproblem_subs.pm" 
{
  sub cmp_coords {
    if ($a =~ /THETA/){
      return substr($a,5) <=> substr($b,5);
    } else {
      ($a.$b) =~ /\((\d+)\,(\d+)\)[A-Z]*\((\d+)\,(\d+)\)/;
      return $2+(($1-1)*$1)/2 <=> $4+(($3-1)*$3)/2; 
    }
  }
  if (defined $self->omegacoordval) {
    my %valueshash = %{$self->omegacoordval};
    @names = sort cmp_coords (keys %valueshash);
  }
}
# line 3818 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> omeganames');
	# End of Non-Dia code #

	return \@names;
}

sub cvseomegacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> cvseomegacoordval');
# line 211 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->omegacoordval and defined $self->cvseomegas) {
    my @names = @{$self->omeganames()};
    my @cvse = @{$self->cvseomegas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $cvse[$i];
    }
  }
}
# line 3841 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> cvseomegacoordval');
	# End of Non-Dia code #

	return \%hash;
}

sub cvsesigmacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> cvsesigmacoordval');
# line 223 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->sigmacoordval and defined $self->cvsesigmas) {
    my @names = @{$self->sigmanames()};
    my @cvse = @{$self->cvsesigmas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $cvse[$i];
    }
  }
}
# line 3864 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> cvsesigmacoordval');
	# End of Non-Dia code #

	return \%hash;
}

sub cvsethetacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> cvsethetacoordval');
# line 199 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->thetacoordval and defined $self->cvsethetas) {
    my @names = @{$self->thetanames()};
    my @cvse = @{$self->cvsethetas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $cvse[$i];
    }
  }
}
# line 3887 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> cvsethetacoordval');
	# End of Non-Dia code #

	return \%hash;
}

sub comegacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> comegacoordval');
# line 247 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->omegacoordval and defined $self->comegas) {
    my @names = @{$self->omeganames()};
    my @val = @{$self->comegas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $val[$i];
    }
  }
}
# line 3910 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> comegacoordval');
	# End of Non-Dia code #

	return \%hash;
}

sub csigmacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> csigmacoordval');
# line 235 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->sigmacoordval and defined $self->csigmas) {
    my @names = @{$self->sigmanames()};
    my @val = @{$self->csigmas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $val[$i];
    }
  }
}
# line 3933 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> csigmacoordval');
	# End of Non-Dia code #

	return \%hash;
}

sub seomegas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> seomegas');
# line 289 "lib/output/problem/subproblem_subs.pm" 
{
  if ( (defined $self->omegacoordval) and (defined $self->seomegacoordval) ) {
    my %sehash = %{$self->seomegacoordval};
    my @names = @{$self->omeganames()};
    foreach my $name (@names){
      my $val= defined $sehash{$name} ? $sehash{$name}: undef;
      push(@values, $val);
    }
  }
}
# line 3957 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> seomegas');
	# End of Non-Dia code #

	return \@values;
}

sub sigmas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sigmas');
# line 302 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->sigmacoordval) {
    my %valueshash = %{$self->sigmacoordval};
    my @names = @{$self->sigmanames()};
    foreach my $name (@names) {
      push(@values, $valueshash{$name});
    }
  }
}
# line 3980 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sigmas');
	# End of Non-Dia code #

	return \@values;
}

sub est_sigmanames {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> est_sigmanames');
# line 155 "lib/output/problem/subproblem_subs.pm" 
{
  if (defined $self->output_matrix_headers) {
    my @names = @{$self->output_matrix_headers};
    foreach my $name (@names) {
      if ($name =~ /SIGMA/) {
				push(@values,$name);
      }
    }
  }
}
# line 4004 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> est_sigmanames');
	# End of Non-Dia code #

	return \@values;
}

sub sigmanames {
	my $self = shift;
	my @names;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sigmanames');
# line 314 "lib/output/problem/subproblem_subs.pm" 
{
  sub cmp_coords {
    if ($a =~ /THETA/){
      return substr($a,5) <=> substr($b,5);
    } else {
      ($a.$b) =~ /\((\d+)\,(\d+)\)[A-Z]*\((\d+)\,(\d+)\)/;
      return $2+(($1-1)*$1)/2 <=> $4+(($3-1)*$3)/2; 
    }
  }
  if (defined $self->sigmacoordval) {
    my %valueshash = %{$self->sigmacoordval};
    @names = sort cmp_coords (keys %valueshash);
  }
}
# line 4032 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sigmanames');
	# End of Non-Dia code #

	return \@names;
}

sub sesigmas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sesigmas');
# line 331 "lib/output/problem/subproblem_subs.pm" 
{
  if ( (defined $self->sigmacoordval) and (defined $self->sesigmacoordval) ) {
    my %sehash = %{$self->sesigmacoordval};
    my @names = @{$self->sigmanames()};
    foreach my $name (@names) {
      my $val = defined $sehash{$name} ? $sehash{$name}: undef;
      push(@values, $val);
    }
  }
}
# line 4056 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sesigmas');
	# End of Non-Dia code #

	return \@values;
}

sub get_NM7_table_numbers {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'line_array' => 'm_REF', 'method_string' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_numbers: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_numbers: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_numbers: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_numbers: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_numbers: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $line_array = $parm{'line_array'};
	my $method_string = defined $parm{'method_string'} ? $parm{'method_string'} : 'T';
	my @table_numbers;
	my $not_found;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_NM7_table_numbers');
# line 2067 "lib/output/problem/subproblem_subs.pm" 
{
	#return all table numbers in line array, optionally filter on 
	#method string match
	#in: $line_array (mandatory), $method_string optional, default 'T' which always matches TABLE
	#out: @table_numbers,$not_found (in that order)
	
	@table_numbers = ();
	$not_found = 1;

	if (scalar(@{$line_array}) < 1 ) {
	  carp("empty line_array input to get_NM7_table_numbers");
	} else {
	  foreach my $line (@{$line_array}) {
	    if ($line =~ /^\s*TABLE NO.\s+(\d+):/) {
	      my $number = $1;
	      if ($line =~ /$method_string/) {
					push (@table_numbers, $number);
					$not_found = 0;
	      }
	    }
	  }
	}
}
# line 4121 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_NM7_table_numbers');
	# End of Non-Dia code #

	return \@table_numbers ,$not_found;
}

sub get_column_index_order {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'header_label' => 'm_REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->get_column_index_order: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_column_index_order: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_column_index_order: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_column_index_order: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_column_index_order: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $header_label = $parm{'header_label'};
	my @index_order;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_column_index_order');
# line 2536 "lib/output/problem/subproblem_subs.pm" 
{
	#must be done after raw
	unless ($self->NM7_parsed_raw()) {
	  croak('get_column_index_order must be called *after* parse_NM7_raw');
	}

	#input @header_label
	#output \@index_order
	my $skip_labels_matrix = $self->skip_labels_matrix;
	@index_order = ();
	my @sigma_order = ();
	for (my $i = 1; $i < scalar(@{$header_label}); $i++) {
	  if (index($skip_labels_matrix, $header_label->[$i]) >= 0) {
	    next;
	  } elsif ($header_label->[$i] =~ /THETA/ ) {
	    push (@index_order, $i);
	  } elsif  ($header_label->[$i] =~ /SIGMA/ ) {
	    push (@sigma_order, $i) if $self->have_sigmas();
	  } elsif  ($header_label->[$i] =~ /OMEGA/ ) {
	    push (@index_order, $i) if $self->have_omegas();
	  }
	}
	push (@index_order,@sigma_order);
	return \@index_order;
}
# line 4186 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_column_index_order');
	# End of Non-Dia code #

	return \@index_order;
}

sub permute_and_clean_rows {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->permute_and_clean_rows: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->permute_and_clean_rows: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->permute_and_clean_rows: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->permute_and_clean_rows: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->permute_and_clean_rows: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> permute_and_clean_rows');
# line 2487 "lib/output/problem/subproblem_subs.pm" 
{
	#input  type, no output
	#must be done after raw
	unless ($self->NM7_parsed_raw()) {
	  croak('permute_and_clean_rows must be called *after* parse_NM7_raw');
	}

	my $skip_labels_matrix = $self->skip_labels_matrix;
	my $found_table = 0;
	return if ($type eq 'phi'); #can't do this with phi
	if (defined $self->nm_output_files->{$type}) {
	  if (scalar (@{$self->nm_output_files->{$type}}) > 1) {
	    my @temp_array = ();
	    my @sigma_array = ();
	    foreach my $line (@{$self->nm_output_files->{$type}}) {
	      if ($line =~ /^\s*TABLE NO.\s+(\d+):/ ) {
					croak("two tables found where 1 expected for $type" ) 
		    if $found_table;
					$found_table = 1;
					push (@temp_array,$line);
	      } elsif ($line =~ /^\s*NAME\s+/ ) {
					push (@temp_array,$line);
	      } else {
					my $templine = $line;
					$templine =~ s/^\s*//; #get rid of leading spaces
						my ($label,$rest) = split (/\s+/,$templine,2);

					unless (index($skip_labels_matrix,$label) >= 0) {
						if ($label =~ /SIGMA/ ) {
							push (@sigma_array,$line) if $self->have_sigmas();
						} elsif ($label =~ /OMEGA/ ) {
							push (@temp_array,$line) if $self->have_omegas();
						} else {
							push (@temp_array,$line);
						}
					}
				}
	    }
			push(@temp_array, @sigma_array);
			@{$self->nm_output_files->{$type}} = ();
			push (@{$self->nm_output_files->{$type}}, @temp_array);
		}
	}
}
# line 4269 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> permute_and_clean_rows');
	# End of Non-Dia code #

}

sub get_NM7_table_method {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'line_array' => 'm_REF', 'table_number' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_method: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_method: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_method: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_method: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_table_method: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $line_array = $parm{'line_array'};
	my $table_number = $parm{'table_number'};
	my $method_string;
	my $not_found;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_NM7_table_method');
# line 2096 "lib/output/problem/subproblem_subs.pm" 
{
	#return the method string of table with number $table_number
	#in: $line_array (mandatory), $table_number (mandatory)
	#out:$method_string,$not_found (in that order)

	$not_found = 1;
	$method_string = '';
	if (scalar(@{$line_array}) < 1 ) {
	  carp("empty line_array input to get_NM7_table_method");
	} else {
	  foreach my $line (@{$line_array}){
	    if ($line =~ /^\s*TABLE NO.\s+(\d+):\s*([^:]+)/ ) {
	      if ($1 == $table_number) {
					$not_found = 0;
					$method_string = $2;
					chomp $method_string;
					$method_string =~ s/\s*$//; #remove trailing spaces
	      }
	    }
	  }
	}
}
# line 4332 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_NM7_table_method');
	# End of Non-Dia code #

	return $method_string ,$not_found;
}

sub get_NM7_tables {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'line_array' => 'm_REF', 'table_numbers' => 'm_REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $line_array = $parm{'line_array'};
	my $table_numbers = $parm{'table_numbers'};
	my @tableline_array;
	my @not_found_array;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_NM7_tables');
# line 2123 "lib/output/problem/subproblem_subs.pm" 
{
	#return array of table lines for tables with numbers in table_numbers
	#in: $line_array (mandatory), $table_numbers (mandatory)
	#out: @tableline_array,@not_found_array (in that order)

	@not_found_array = ();
	@tableline_array = ();
	if (scalar(@{$table_numbers}) < 1 ) {
	  carp("empty table number input to get_NM7_tables");
	  unshift(@not_found_array, 1);
	} elsif (scalar(@{$line_array}) < 1 ) {
	  carp("empty line_array input to get_NM7_tables " );
	  @not_found_array = 1 x scalar(@{$table_numbers});
	} else {
	  @tableline_array = ();
	  #make sure numbers are sorted.
	  #assume table numbers are sorted in NM7 lines
	  my @sorted_numbers = sort {$a <=> $b} @{$table_numbers};
	  my $table_number = shift (@sorted_numbers);
	  
	  my $store_line = 0;
	  foreach my $line (@{$line_array}) {
	    if ($line =~ /^\s*TABLE NO.\s+(\d+):\s*([^:]+)/ ) {
	      last unless (defined $table_number); #store no more lines, search no more
	      while (($1 > $table_number) && (defined $table_number) ) {
					#we won't find this table_number provided tables are sorted
					unshift(@not_found_array, 1);
					$table_number = shift (@sorted_numbers);
					$store_line = 0; #we have found a new table we do not want
	      }
	      last unless (defined $table_number); #no more tables to look for
	      if ($1 == $table_number) {
					$store_line = 1;
					unshift(@not_found_array, 0);
					$table_number = shift (@sorted_numbers);
	      } elsif ($1 < $table_number) {
					$store_line = 0;
	      }
	    } 
	    push (@tableline_array,$line) if ($store_line);
	  }
	  while (defined $table_number){
	    #we won't find these table_numbers, reached end of linearray
	    unshift(@not_found_array, 1);
	    $table_number = shift (@sorted_numbers);
	  }
	  unless (scalar(@not_found_array) == scalar(@{$table_numbers})) {
	    croak("error in get_NM7_tables" );
	  }
	}
}
# line 4425 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_NM7_tables');
	# End of Non-Dia code #

	return \@tableline_array ,\@not_found_array;
}

sub get_NM7_tables_all_types {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'table_numbers' => 'm_REF', 'raw_array' => 'm_REF',
			'cov_array' => 'REF', 'cor_array' => 'REF',
			'coi_array' => 'REF', 'phi_array' => 'REF',
			'covariance_step_run' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables_all_types: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables_all_types: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables_all_types: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables_all_types: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->get_NM7_tables_all_types: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $table_numbers = $parm{'table_numbers'};
	my $raw_array = $parm{'raw_array'};
	my $cov_array = $parm{'cov_array'};
	my $cor_array = $parm{'cor_array'};
	my $coi_array = $parm{'coi_array'};
	my $phi_array = $parm{'phi_array'};
	my $covariance_step_run = $parm{'covariance_step_run'};
	my @raw_table;
	my @cov_table;
	my @cor_table;
	my @coi_table;
	my @phi_table;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_NM7_tables_all_types');
# line 2180 "lib/output/problem/subproblem_subs.pm" 
{
	#return arrays of table lines for tables with numbers in table_numbers
	#in: $raw_array,$cov_array,$cor_array,$coi_array,$phi_array, $covariance_step_run
	#$table_numbers (mandatory)
	#out: @raw_table,@cov_table,@cor_table,@coi_table,@phi_table

	  my $not_found;
	  my $not_found_single;
	  my $number_count = scalar (@{$table_numbers});
	  my ($method_string,$check_string);

	  my ($raw_ref,$cov_ref,$cor_ref,$coi_ref,$phi_ref);

	  ($raw_ref,$not_found) = 
	      $self->get_NM7_tables('line_array' => $raw_array,
				    'table_numbers' => $table_numbers);
	  for (my $i = 0; $i < $number_count; $i++) { 
	    croak("did not find table " . $table_numbers->[$i] . " in raw_file array" )
				if ($not_found->[$i]); 
	  }
	  if ($number_count == 1) {
	    ($method_string,$not_found_single) = 
		$self->get_NM7_table_method('line_array' => $raw_ref, 'table_number' => $table_numbers->[0]);
	    croak("Could not find table".$table_numbers ->[0]." in raw_file array" )
				if ($not_found_single); 
	  }
	  @raw_table = @{$raw_ref};

	  my $expect_cov = $covariance_step_run;
	  $expect_cov = 0 if ($method_string =~ /Stochastic Approximation/ );

	  if (defined $cov_array and $expect_cov) {
	    ($cov_ref,$not_found) = 
		$self->get_NM7_tables('line_array' => $cov_array,
				      'table_numbers' => $table_numbers);
	    for (my $i= 0; $i < $number_count; $i++) { 
	      carp("did not find table " . $table_numbers->[$i] . " in cov_file array" )
					if ($not_found->[$i]);
	    }
	    if (($number_count == 1) && ($not_found->[0] == 0)) {
	      ($check_string,$not_found_single) = 
		  $self->get_NM7_table_method('line_array' => $cov_ref,
					      'table_number' => $table_numbers ->[0]);
	      croak("Could not find table".$table_numbers ->[0]." in cov_file array" )
					if ($not_found_single); 
	      
				unless (($method_string =~ $check_string) || ($method_string eq $check_string)) {
					croak("strings $method_string from raw and ".
							"$check_string from cov do not match" );
				}
			}
			@cov_table = @{$cov_ref};
		}

		if (defined $cor_array and $expect_cov){
			($cor_ref,$not_found) = 
				$self->get_NM7_tables('line_array' => $cor_array, 'table_numbers' => $table_numbers);
	    for (my $i = 0; $i < $number_count; $i++) { 
	      carp("did not find table ".$table_numbers->[$i]." in cor_file array" )
		  if ($not_found->[$i]); 
	    }
	    if (($number_count == 1) && (not $not_found->[0])) {
	      ($check_string,$not_found_single) = 
		  $self->get_NM7_table_method('line_array' => $cor_ref,
					      'table_number' => $table_numbers ->[0]);
	      croak("Could not find table".$table_numbers ->[0]." in cor_file array" )
					if ($not_found_single); 
	      
	      unless (($method_string =~ $check_string) || ($method_string eq $check_string)) {
					croak("strings $method_string from raw and ".
							"$check_string from cor do not match" );
				}
			}
			@cor_table = @{$cor_ref};
	  }


	  if (defined $coi_array and $expect_cov) {
	    ($coi_ref,$not_found) = 
		$self->get_NM7_tables('line_array' => $coi_array,
				      'table_numbers' => $table_numbers);
	    for (my $i = 0; $i < $number_count; $i++){ 
	      carp("did not find table ".$table_numbers->[$i]." in coi_file array" )
					if ($not_found->[$i]); 
	    }
	    if (($number_count == 1) && (not $not_found->[0])) {
	      ($check_string,$not_found_single) = 
					$self->get_NM7_table_method('line_array' => $coi_ref, 'table_number' => $table_numbers ->[0]);
				croak("Could not find table".$table_numbers ->[0]." in coi_file array" )
					if ($not_found_single); 
	      
				unless (($method_string =~ $check_string) || ($method_string eq $check_string)) {
					croak("strings $method_string from raw and ".
							"$check_string from coi do not match" );
				}
			}
			@coi_table = @{$coi_ref};
	  }

	  if (defined $phi_array) {
			($phi_ref,$not_found) = 
				$self->get_NM7_tables('line_array' => $phi_array, 'table_numbers' => $table_numbers);
			for (my $i = 0; $i < $number_count; $i++) { 
				carp("did not find table ".$table_numbers->[$i]." in phi_file array" )
					if ($not_found->[$i]); 
			}
	    if (($number_count == 1) && (not $not_found->[0])) {
				($check_string,$not_found_single) = 
					$self->get_NM7_table_method('line_array' => $phi_ref, 'table_number' => $table_numbers ->[0]);
				croak("Could not find table".$table_numbers ->[0]." in phi_file array" )
					if ($not_found_single); 

	      unless (($method_string =~ $check_string) || ($method_string eq $check_string)) {
					croak("strings $method_string from raw and ".
							"$check_string from phi do not match" );
				}
			}
			@phi_table = @{$phi_ref};
		}
}
# line 4598 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_NM7_tables_all_types');
	# End of Non-Dia code #

	return \@raw_table ,\@cov_table ,\@cor_table ,\@coi_table ,\@phi_table;
}

sub parse_NM7_raw {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_raw: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_raw: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_raw: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_raw: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_raw: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> parse_NM7_raw');
# line 2305 "lib/output/problem/subproblem_subs.pm" 
{
  #Assume that we have only one table now, in $self->nm_output_files->{'raw'}
  #Assume whitespace as field separator
  #add error checking of separator

  my $skip_labels_matrix = '';
  my $found_table = 0; #for error checking
  my @header_labels = ();
  my %final_values;
  my %standard_errors;
  my %correlation_matrix_data;
  my $n_eigenvalues = 0;
  my (@theta,@standard_errors_theta);
  my @omega;
  my @sigma;
  my @eigenvalues;
  my $no_value = 10000000000;
  my $read_standard_errors = 0;
  my $given_header_warning = 0;
  my (%thetacoordval, %omegacoordval, %sigmacoordval);
  my (%sethetacoordval, %seomegacoordval, %sesigmacoordval);
  my $header_ok = 0;
  my $val;
  my $found_ofv_line = 0;

  foreach my $line (@{$self->nm_output_files->{'raw'}}) {
    if ($line =~ /^\s*TABLE NO.\s+(\d+):/ ) {
      croak("two tables found where 1 expected" ) if $found_table;
      $found_table = 1;
    } elsif ($line =~ /^\s*ITERATION/ ) {
      $line =~ s/^\s*//; #get rid of leading spaces
      @header_labels = split /\s+/,$line;
      $header_ok = 1 if ($header_labels[0] eq 'ITERATION');
    }

    next unless ($line =~ /^\s*(-100000000\d)/ ); #only want neg iteration numbers = special values
    
    if ($1 == -1000000000 ) {
      $found_ofv_line = 1;
      #final values
      #check that we have labels in array. Otherwise missing label row or wrong delimiter
      unless ((scalar(@header_labels > 2)) or $given_header_warning or $header_ok){
				my $mes = "\n\n\***Warning***\n".
					"Too few elements in parameter label array in raw output file. ".
					"Is label row missing, or is the ".
					"delimiter something other than spaces (default)? ".
					"Parsing is likely to fail".
					"\n*************\n";
				print $mes;
				$given_header_warning = 1;
      }
			$line =~ s/^\s*//; #get rid of leading spaces
			my @values = split /\s+/, $line;
      for (my $i = 1; $i < scalar(@values); $i++) {
				if ($values[$i] eq 'NaN' or eval($values[$i]) == $no_value){
					$val = 'NA';
				} else {
					$val = eval($values[$i]); # can get zeros here
				}
				$skip_labels_matrix .= $header_labels[$i]." " if ($val eq 'NA');
				if ($header_labels[$i] =~ /THETA/){
					$thetacoordval{$header_labels[$i]} = $val unless ($val eq 'NA'); 
				} elsif ($header_labels[$i] =~ /OMEGA/) {
					$omegacoordval{$header_labels[$i]} = $val unless ($val eq 'NA');
				} elsif ($header_labels[$i] =~ /SIGMA/) {
					$sigmacoordval{$header_labels[$i]} = $val unless ($val eq 'NA' );
				} elsif ($header_labels[$i] =~ /OBJ/) {
					$self->ofv($val);
				} else { 
					my $mes = "Unknown header label ".$header_labels[$i]." in raw output.";
					croak($mes);
				}
      }
    } elsif ($1 == -1000000001) {
      #standard errors
      $line =~ s/^\s*//; #get rid of leading spaces
      my @values = split /\s+/,$line;
      for (my $i = 1; $i < scalar(@values); $i++) {
				if ($values[$i] eq 'NaN' or eval($values[$i]) == $no_value){
					$val='NA';
				} else {
					$val = eval($values[$i]);
				}
				$skip_labels_matrix .= $header_labels[$i]." "  if ($val eq 'NA');
				if ($header_labels[$i] =~ /THETA/) {
					$sethetacoordval{$header_labels[$i]} = $val unless ($val eq 'NA');
				} elsif ($header_labels[$i] =~ /OMEGA/) {
					$seomegacoordval{$header_labels[$i]} = $val unless ($val eq 'NA');
				} elsif ($header_labels[$i] =~ /SIGMA/) {
					$sesigmacoordval{$header_labels[$i]} = $val unless ($val eq 'NA');
				} elsif ($header_labels[$i] =~ /OBJ/) {
					#set dic if have std
					if (0) {
						if (($val ne 'NA') and ($val != 0)){
							#assume Bayes
							my $dic = $self->ofv() + $val * $val / 2;
							$self->ofv($dic);
							$self->dic($dic);
						}
					}
				} else { 
					my $mes = "Unknown header label ".$header_labels[$i]." in raw output.";
					croak($mes);
				}
			}
			$read_standard_errors = 1;
		} elsif ($1 == -1000000002) {
      #eigenvalues
      $line =~ s/^\s*//; #get rid of leading spaces
      my @values = split /\s+/,$line;
      for (my $i=1; $i < scalar(@values); $i++) {
				last if ($values[$i] == 0); #array is padded with zeros
					$n_eigenvalues++;
				if ($values[$i] eq 'NaN' or eval($values[$i]) == $no_value) {
					$val = undef;
				} else {
					$val = eval($values[$i]);
				}
				push (@eigenvalues, $val);
			}
			$self->eigens([]) unless defined $self->eigens;
			@{$self->eigens} = @eigenvalues;
    } elsif ($1 == -1000000003) {
      #matrix properties
      $line =~ s/^\s*//; #get rid of leading spaces
      my @values = split /\s+/,$line;
      $correlation_matrix_data{'condition_number'} = eval($values[1]);
      $correlation_matrix_data{'lowest_eigenvalue'} = eval($values[2]);
      $correlation_matrix_data{'highest_eigenvalue'} = eval($values[3]);
      $self->condition_number($correlation_matrix_data{'condition_number'});
    }
  }

  return unless ($found_ofv_line);

  #store rest of values in $self
  $self->thetacoordval(\%thetacoordval); 
  $self->omegacoordval(\%omegacoordval);
  $self->sigmacoordval(\%sigmacoordval);
  $self->sethetacoordval(\%sethetacoordval);
  $self->seomegacoordval(\%seomegacoordval);
  $self->sesigmacoordval(\%sesigmacoordval);
  
  if (%sethetacoordval) { #at least one value
    $self->covariance_step_successful(1);
  }
  
  $self->skip_labels_matrix($skip_labels_matrix);
  delete $self->nm_output_files->{'raw'};
  $self->NM7_parsed_raw(1);
  
  #verify that not have_omegas/sigmas is correct
  unless ($self->have_sigmas()) {
    my $count = 0;
    foreach my $lab(@header_labels) {
      $count++ if ($lab =~ /SIGMA/);
    }
    if ($count > 1) {
      $self->have_sigmas(1); #never more than one dummy column
    } elsif (defined $sesigmacoordval{'SIGMA(1,1)'}) {
      $self->have_sigmas(1) if (defined $sesigmacoordval{'SIGMA(1,1)'} and 
				$sesigmacoordval{'SIGMA(1,1)'} != 0);
    }
  }
  unless ($self->have_omegas()) {
    my $count = 0;
    foreach my $lab(@header_labels){
      $count++ if ($lab =~ /OMEGA/);
    }
    if ($count > 1) {
      $self->have_omegas(1); #never more than one dummy column
    } elsif (defined $seomegacoordval{'OMEGA(1,1)'}) {
      $self->have_omegas(1) if (defined $seomegacoordval{'OMEGA(1,1)'} and
				$seomegacoordval{'OMEGA(1,1)'} != 0);
    }
  }
}
# line 4813 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> parse_NM7_raw');
	# End of Non-Dia code #

}

sub parse_NM7_additional {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_additional: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_additional: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_additional: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_additional: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->parse_NM7_additional: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> parse_NM7_additional');
# line 2567 "lib/output/problem/subproblem_subs.pm" 
{
	#must be done after raw
	unless ($self->NM7_parsed_raw()){
	  croak('parse_NM7_additional must be called *after* parse_NM7_raw');
	}

	#Assume that we have only one table now in each $self->nm_output_files->{$type}
	#Assume whitespace as field separator
	#add error checking of separator
	#success is all or nothing, except phi which is optional, break if failure

	my $success = 1;
	my $skip_labels_matrix = $self->skip_labels_matrix;
	my $given_header_warning = 0;

	my $expect_cov = $self->covariance_step_run();
	$expect_cov = 0 if ($self->method_string() =~ /Stochastic Approximation/ );
	return unless ($expect_cov);

	foreach my $type ('cov', 'coi', 'cor') {
	  my @header_labels = ();
	  my @matrix_array;
	  my @inverse;
	  my $found_table = 0;
	  unless (defined $self->nm_output_files->{$type}) {
	    if ($type eq 'phi') {
	      next;
	    } else {
	      $success = 0;
	      last;
	    }
	  }
	  unless (scalar (@{$self->nm_output_files->{$type}}) > 1) {
	    if ($type eq 'phi') {
	      next;
	    } else {
	      $success = 0;
	      last;
	    }
	  }
	  $self->permute_and_clean_rows(type => $type);
	  my $row_index = 0;
	  my @index_order;
	  my $header_ok = 0;
	  foreach my $line (@{$self->nm_output_files->{$type}}) {
	    if ($line =~ /^\s*TABLE NO.\s+(\d+):/ ) {
	      croak("two tables found where 1 expected for $type" ) if $found_table;
	      $found_table = 1;
	    } elsif ($line =~ /^\s*NAME/ ) {
	      $line =~ s/^\s*//; #get rid of leading spaces
	      @header_labels = split /\s+/, $line;
	      $header_ok = 1 if ($header_labels[0] eq 'NAME');
	      @index_order = @{$self->get_column_index_order(header_label=>\@header_labels)};
	    } else {
	      unless ((scalar(@header_labels > 2)) or $given_header_warning or $header_ok) {
					my $mes = "\n\n\***Warning***\n".
						"Too few elements in parameter label array in additional output file. ".
						"Is label row missing, or is the ".
						"delimiter something other than spaces (default)? ".
						"Parsing is likely to fail".
						"\n*************\n";
					print $mes;
					$given_header_warning = 1;
				}
				$row_index++;
				$line =~ s/^\s*//; #get rid of leading spaces
				my @line_values = split /\s+/,$line;
	      my $max_column;
	      my @new_line;
	      if (($type eq 'coi') || ($type eq 'phi')) {
					$max_column = scalar(@index_order) ; #store full matrix
				} else {
					$max_column = $row_index; #store lower triangular matrix
				}
				for (my $j = 0; $j < $max_column; $j++) {
					my $i = $index_order[$j]; #must permute omega-sigma
					if ($line_values[$i] eq 'NaN') {
						push(@new_line, undef);
					} else {
						push(@new_line, eval($line_values[$i]));
					}
				}
	      if (($type eq 'coi') || ($type eq 'phi')) {
					push(@matrix_array, \@new_line); #square matrix
	      } else {
					push(@matrix_array, @new_line); #linear array
	      }
	    }
	  }


	  if ($type eq 'cov') {
			$self->raw_covmatrix([]) unless defined $self->raw_covmatrix;
	    push( @{$self->raw_covmatrix}, @matrix_array);
			$self->covariance_matrix([]) unless defined $self->covariance_matrix;
	    foreach my $element ( @{$self->raw_covmatrix} ) {
	      push( @{$self->covariance_matrix}, eval($element) ) 
					unless ( $element eq '.........' );
			}
		} elsif ($type eq 'cor') {
			$self->correlation_matrix([]) unless defined $self->correlation_matrix;
	    push( @{$self->correlation_matrix}, @matrix_array);
	    my @column_headers = ();
	    foreach my $ind (@index_order) {
	      push (@column_headers, $header_labels[$ind]);
	    }
			$self->output_matrix_headers([]) unless defined $self->output_matrix_headers;
	    push( @{$self->output_matrix_headers}, @column_headers );
		} elsif ($type eq 'coi') {
			if (scalar(@matrix_array) > 0) {
	      $self->inverse_covariance_matrix(Math::MatrixReal -> new_from_cols(\@matrix_array));
			}
		} elsif ($type eq 'phi') {
			1; #nothing yet
		}
	}

	unless ($success) {
	  #erase, read everything from lst
	  carp("Failed to read all matrices cov, cor and coi. Will try reading matrices from lst instead. Note: This is not an error unless all three" .
									" additional output files cov, cor and coi are expected given the model input.") unless $self -> {'ignore_missing_files'};
	  $self->raw_covmatrix([]);
	  $self->correlation_matrix([]);
		$self->output_matrix_headers([]);
	  $self->{'inverse_covariance_matrix'} = undef;		# FIXME: Need to be set to undef. Fix for Moose
	}

	delete $self->nm_output_files->{'cov'};
	delete $self->nm_output_files->{'coi'};
	delete $self->nm_output_files->{'cor'};
	delete $self->nm_output_files->{'phi'};

	#what about t-matrix?
	#what about  phi

	$self->NM7_parsed_additional($success);
	
}
# line 4988 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> parse_NM7_additional');
	# End of Non-Dia code #

}

sub _isdiagonal {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'index' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->_isdiagonal: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_isdiagonal: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_isdiagonal: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_isdiagonal: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_isdiagonal: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $index = $parm{'index'};
	my $isdiagonal = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _isdiagonal');
# line 536 "lib/output/problem/subproblem_subs.pm" 
{
	my $previ = 1;
	my $j;
	return(1) if $index == 1;
	foreach my $j (2 .. 100) {
		return(1) if $index == $previ + $j;
		$previ = $previ + $j;
		last if $index < $previ;
	}
}
# line 5037 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _isdiagonal');
	# End of Non-Dia code #

	return $isdiagonal;
}

sub _return_function {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'hash' => '', 'scalar_return' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->_return_function: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_return_function: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_return_function: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_return_function: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_return_function: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $scalar_return = defined $parm{'scalar_return'} ? $parm{'scalar_return'} : 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_matrixoestimates {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'pos' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem::subproblem->_read_matrixoestimates: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_read_matrixoestimates: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem::subproblem->_read_matrixoestimates: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_read_matrixoestimates: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem::subproblem->_read_matrixoestimates: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $pos = defined $parm{'pos'} ? $parm{'pos'} : 0;
	my @subprob_matrix;
	my $success = 0;
	my @row_headers;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_matrixoestimates');
# line 551 "lib/output/problem/subproblem_subs.pm" 
{
	# Reads one matrix structure and returns the file handle at
	# the beginning of the next structure

	#this does not handle TH1 | TH2 type format
	my $reading_header = 0;
	while ( $_ = @{$self->lstfile}[ $pos++ ] ) {
		last if (/^\s*\*/);
		# Rewind one step if we find something that marks the end of
		# our structure
		$pos-- and last if ( /^ PROBLEM.*SUBPROBLEM/ or /^ PROBLEM NO\.:\s+\d/ );
		$pos-- and last if (/^[a-df-zA-DF-Z]/);

		if ( /^ TH/ or /^ OM/ or /^ SG/ ) {	  # Row header row (single space)
			my $label;
			chomp;				# Get rid of line-feed
			s/\s*//g; #get rid of whitespaces
	    #transform into correct label format
			if (/TH([0-9]+)/) {
				$label = 'THETA'.$1;
			} elsif (/(OM|SG)([0-9]+)/) {
				if ($1 eq 'OM') {
					$label = 'OMEGA(';
				} elsif ($1 eq 'SG') {
					$label = 'SIGMA(';
				} else {
					croak("unknown $1");
				}
	      my $len = length($2); #half of this is number of characters for each index
	      my $x = substr ($2, 0, ($len/2));
	      my $y = substr ($2, ($len/2));
	      #NONMEM may pad with zeros
	      $x =~ s/^0*//;
	      $y =~ s/^0*//;
	      $label .= $y.','.$x.')'; #NONMEM indexes upper triangular matrix
	    } else {
	      croak("Unknown format of labels in matrix ".$_);
	    }
	    push( @row_headers, $label ) ;
	    next;
	  } elsif ( /^\s+TH/ or /^\s+OM/ or /^\s+SG/ ) {	  # Column header (multiple spaces)
	    next;
	  }

	  next if ( /^1/ );			  # Those annoying 1's

	  chomp;				# Get rid of line-feed
	  my @row = split;
		shift( @row ) if ( $row[0] eq '+' );	   # Get rid of +-sign

		next if ( $#row < 0 );			   # Blank row
	  
		push( @subprob_matrix, @row );
	}
	$success = 1 if ( scalar @subprob_matrix > 0 );
}
# line 5169 libgen/output/problem/subproblem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_matrixoestimates');
	# End of Non-Dia code #

	return $pos ,\@subprob_matrix ,$success ,\@row_headers;
}

1;

