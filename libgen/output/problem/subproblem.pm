use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package output::problem::subproblem;

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
	# End of Non-Dia code #

}

sub _read_covmatrix {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_eigen {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_eval {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

sub _read_npomegas {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_ofv {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_sethomsi {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_significant_digits {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_simulation {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_term {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_minimization_message {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_thomsi {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

sub _compute_comegas {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _compute_csigmas {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _compute_cvsesigma {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _compute_cvseomega {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _compute_cvsetheta {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return $row ,$col;
}

sub thetas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@values;
}

sub est_thetanames {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@values;
}

sub thetanames {
	my $self = shift;
	my @names;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@names;
}

sub sethetas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@values;
}

sub omegas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@values;
}

sub est_omeganames {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@values;
}

sub omeganames {
	my $self = shift;
	my @names;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@names;
}

sub cvseomegacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \%hash;
}

sub cvsesigmacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \%hash;
}

sub cvsethetacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \%hash;
}

sub comegacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \%hash;
}

sub csigmacoordval {
	my $self = shift;
my %hash;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \%hash;
}

sub seomegas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@values;
}

sub sigmas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@values;
}

sub est_sigmanames {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@values;
}

sub sigmanames {
	my $self = shift;
	my @names;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@names;
}

sub sesigmas {
	my $self = shift;
	my @values;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return $pos ,\@subprob_matrix ,$success ,\@row_headers;
}

1;

