use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package output::problem;

#---------------------------------------------------------------------
#         Used Packages
#---------------------------------------------------------------------
use output::problem::subproblem;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'subproblems' => 'ARRAY', 'problem_id' => 'SCALAR',
			'output_id' => 'SCALAR', 'nm_output_files' => '',
			'filename_root' => 'SCALAR', 'directory' => 'SCALAR',
			'model_id' => 'SCALAR', 'covariance_step_run' => 'SCALAR',
			'estimatedsigmas' => 'ARRAY', 'estimatedthetas' => 'ARRAY',
			'estimatedomegas' => 'ARRAY', 'finished_parsing' => 'SCALAR',
			'fixedomegas' => 'ARRAY', 'fixedsigmas' => 'ARRAY',
			'fixedthetas' => 'ARRAY', 'initomegas' => 'ARRAY',
			'initsigmas' => 'ARRAY', 'initthetas' => 'ARRAY',
			'lower_theta_bounds' => 'ARRAY', 'lower_omega_bounds' => 'ARRAY',
			'lower_sigma_bounds' => 'ARRAY', 'n_previous_meth' => 'SCALAR',
			'table_number' => 'SCALAR', 'input_problem' => 'REF',
			'nm_major_version' => 'm_SCALAR', 'ignore_missing_files' => 'SCALAR',
			'lstfile' => 'ARRAY', 'lstfile_pos' => 'SCALAR',
			'nind' => 'SCALAR', 'nobs' => 'SCALAR', 'nrecs' => 'SCALAR',
			'omega_block_sets' => 'HASH', 'omega_block_structure' => 'ARRAY',
			'omega_block_structure_type' => 'SCALAR',
			'omega_indexes' => 'ARRAY', 'parsed' => 'SCALAR',
			'parsed_successfully' => 'SCALAR', 'msfo_has_terminated' => 'SCALAR',
			'parsing_error_message' => 'SCALAR', 'sigma_block_structure' => 'ARRAY',
			'sigma_block_structure_type' => 'SCALAR',
			'sigma_block_sets' => 'HASH', 'sigma_indexes' => 'ARRAY',
			'tableidcolumns' => 'ARRAY', 'tablenames' => '',
			'upper_omega_bounds' => 'ARRAY', 'upper_sigma_bounds' => 'ARRAY',
			'upper_theta_bounds' => 'ARRAY', 'user_defined_prior' => 'SCALAR',
			'pre_run_errors' => 'SCALAR', 'estimation_step_run' => 'SCALAR',
			'nonparametric_step_run' => 'SCALAR', 'msfi_used' => 'SCALAR',
			'tables_step_run' => 'SCALAR', 'simulation_step_run' => 'SCALAR',
			'estimation_step_initiated' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'covariance_step_run'} = defined $parm{'covariance_step_run'} ? $parm{'covariance_step_run'} : 0 unless defined $this -> {'covariance_step_run'};
	$this -> {'estimatedsigmas'} = defined $parm{'estimatedsigmas'} ? $parm{'estimatedsigmas'} : [] unless defined $this -> {'estimatedsigmas'};
	$this -> {'estimatedthetas'} = defined $parm{'estimatedthetas'} ? $parm{'estimatedthetas'} : [] unless defined $this -> {'estimatedthetas'};
	$this -> {'estimatedomegas'} = defined $parm{'estimatedomegas'} ? $parm{'estimatedomegas'} : [] unless defined $this -> {'estimatedomegas'};
	$this -> {'finished_parsing'} = defined $parm{'finished_parsing'} ? $parm{'finished_parsing'} : 0 unless defined $this -> {'finished_parsing'};
	$this -> {'fixedomegas'} = defined $parm{'fixedomegas'} ? $parm{'fixedomegas'} : [] unless defined $this -> {'fixedomegas'};
	$this -> {'fixedsigmas'} = defined $parm{'fixedsigmas'} ? $parm{'fixedsigmas'} : [] unless defined $this -> {'fixedsigmas'};
	$this -> {'fixedthetas'} = defined $parm{'fixedthetas'} ? $parm{'fixedthetas'} : [] unless defined $this -> {'fixedthetas'};
	$this -> {'initomegas'} = defined $parm{'initomegas'} ? $parm{'initomegas'} : [] unless defined $this -> {'initomegas'};
	$this -> {'initsigmas'} = defined $parm{'initsigmas'} ? $parm{'initsigmas'} : [] unless defined $this -> {'initsigmas'};
	$this -> {'initthetas'} = defined $parm{'initthetas'} ? $parm{'initthetas'} : [] unless defined $this -> {'initthetas'};
	$this -> {'n_previous_meth'} = defined $parm{'n_previous_meth'} ? $parm{'n_previous_meth'} : 0 unless defined $this -> {'n_previous_meth'};
	$this -> {'ignore_missing_files'} = defined $parm{'ignore_missing_files'} ? $parm{'ignore_missing_files'} : 0 unless defined $this -> {'ignore_missing_files'};
	$this -> {'lstfile_pos'} = defined $parm{'lstfile_pos'} ? $parm{'lstfile_pos'} : 0 unless defined $this -> {'lstfile_pos'};
	$this -> {'parsed_successfully'} = defined $parm{'parsed_successfully'} ? $parm{'parsed_successfully'} : 1 unless defined $this -> {'parsed_successfully'};
	$this -> {'msfo_has_terminated'} = defined $parm{'msfo_has_terminated'} ? $parm{'msfo_has_terminated'} : 0 unless defined $this -> {'msfo_has_terminated'};
	$this -> {'user_defined_prior'} = defined $parm{'user_defined_prior'} ? $parm{'user_defined_prior'} : 0 unless defined $this -> {'user_defined_prior'};
	$this -> {'estimation_step_run'} = defined $parm{'estimation_step_run'} ? $parm{'estimation_step_run'} : 0 unless defined $this -> {'estimation_step_run'};
	$this -> {'nonparametric_step_run'} = defined $parm{'nonparametric_step_run'} ? $parm{'nonparametric_step_run'} : 0 unless defined $this -> {'nonparametric_step_run'};
	$this -> {'msfi_used'} = defined $parm{'msfi_used'} ? $parm{'msfi_used'} : 0 unless defined $this -> {'msfi_used'};
	$this -> {'tables_step_run'} = defined $parm{'tables_step_run'} ? $parm{'tables_step_run'} : 0 unless defined $this -> {'tables_step_run'};
	$this -> {'simulation_step_run'} = defined $parm{'simulation_step_run'} ? $parm{'simulation_step_run'} : 0 unless defined $this -> {'simulation_step_run'};
	$this -> {'estimation_step_initiated'} = defined $parm{'estimation_step_initiated'} ? $parm{'estimation_step_initiated'} : 0 unless defined $this -> {'estimation_step_initiated'};

	bless $this, $class;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub subproblems {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subproblems'} = $parm;
	} else {
		return $self -> {'subproblems'};
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

sub estimatedsigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimatedsigmas'} = $parm;
	} else {
		return $self -> {'estimatedsigmas'};
	}
}

sub estimatedthetas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimatedthetas'} = $parm;
	} else {
		return $self -> {'estimatedthetas'};
	}
}

sub estimatedomegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimatedomegas'} = $parm;
	} else {
		return $self -> {'estimatedomegas'};
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

sub fixedomegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'fixedomegas'} = $parm;
	} else {
		return $self -> {'fixedomegas'};
	}
}

sub fixedsigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'fixedsigmas'} = $parm;
	} else {
		return $self -> {'fixedsigmas'};
	}
}

sub fixedthetas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'fixedthetas'} = $parm;
	} else {
		return $self -> {'fixedthetas'};
	}
}

sub initomegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'initomegas'} = $parm;
	} else {
		return $self -> {'initomegas'};
	}
}

sub initsigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'initsigmas'} = $parm;
	} else {
		return $self -> {'initsigmas'};
	}
}

sub initthetas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'initthetas'} = $parm;
	} else {
		return $self -> {'initthetas'};
	}
}

sub lower_theta_bounds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lower_theta_bounds'} = $parm;
	} else {
		return $self -> {'lower_theta_bounds'};
	}
}

sub lower_omega_bounds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lower_omega_bounds'} = $parm;
	} else {
		return $self -> {'lower_omega_bounds'};
	}
}

sub lower_sigma_bounds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lower_sigma_bounds'} = $parm;
	} else {
		return $self -> {'lower_sigma_bounds'};
	}
}

sub n_previous_meth {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'n_previous_meth'} = $parm;
	} else {
		return $self -> {'n_previous_meth'};
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

sub nind {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nind'} = $parm;
	} else {
		return $self -> {'nind'};
	}
}

sub nobs {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nobs'} = $parm;
	} else {
		return $self -> {'nobs'};
	}
}

sub nrecs {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nrecs'} = $parm;
	} else {
		return $self -> {'nrecs'};
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

sub omega_indexes {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'omega_indexes'} = $parm;
	} else {
		return $self -> {'omega_indexes'};
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

sub sigma_indexes {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sigma_indexes'} = $parm;
	} else {
		return $self -> {'sigma_indexes'};
	}
}

sub tableidcolumns {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tableidcolumns'} = $parm;
	} else {
		return $self -> {'tableidcolumns'};
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

sub upper_omega_bounds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'upper_omega_bounds'} = $parm;
	} else {
		return $self -> {'upper_omega_bounds'};
	}
}

sub upper_sigma_bounds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'upper_sigma_bounds'} = $parm;
	} else {
		return $self -> {'upper_sigma_bounds'};
	}
}

sub upper_theta_bounds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'upper_theta_bounds'} = $parm;
	} else {
		return $self -> {'upper_theta_bounds'};
	}
}

sub user_defined_prior {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'user_defined_prior'} = $parm;
	} else {
		return $self -> {'user_defined_prior'};
	}
}

sub pre_run_errors {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'pre_run_errors'} = $parm;
	} else {
		return $self -> {'pre_run_errors'};
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

sub tables_step_run {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tables_step_run'} = $parm;
	} else {
		return $self -> {'tables_step_run'};
	}
}

sub simulation_step_run {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'simulation_step_run'} = $parm;
	} else {
		return $self -> {'simulation_step_run'};
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

sub add_subproblem {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_subproblem given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'subproblems'}},
		output::problem::subproblem -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub access_any {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'attribute' => 'm_SCALAR', 'subproblems' => 'ARRAY',
			'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem->access_any: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem->access_any: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem->access_any: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->access_any: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->access_any: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $attribute = $parm{'attribute'};
	my @return_value;
	my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@return_value;
}

sub full_name_NM7_file {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'file_type' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem->full_name_NM7_file: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem->full_name_NM7_file: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem->full_name_NM7_file: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->full_name_NM7_file: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->full_name_NM7_file: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $file_type = $parm{'file_type'};
	my $full_name;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $full_name;
}

sub store_NM7_output {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'max_table_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem->store_NM7_output: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem->store_NM7_output: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem->store_NM7_output: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->store_NM7_output: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->store_NM7_output: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $max_table_number = $parm{'max_table_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub parsing_error {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem->parsing_error: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem->parsing_error: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem->parsing_error: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->parsing_error: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->parsing_error: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = $parm{'message'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_nrecs {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_nobs {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_nind {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_eststep {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_tablesstep {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_prior {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_steps_allowed {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_nonpstep {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_covstep {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'prob_arr' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem->_read_covstep: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem->_read_covstep: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem->_read_covstep: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->_read_covstep: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->_read_covstep: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @prob_arr = defined $parm{'prob_arr'} ? @{$parm{'prob_arr'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_arbitrary {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'regexp' => 'm_SCALAR', 'member' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem->_read_arbitrary: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem->_read_arbitrary: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem->_read_arbitrary: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->_read_arbitrary: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->_read_arbitrary: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $regexp = $parm{'regexp'};
	my $member = $parm{'member'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_msfo_status {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_subproblems {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'subprob_arr' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem->_read_subproblems: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem->_read_subproblems: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem->_read_subproblems: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->_read_subproblems: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->_read_subproblems: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @subprob_arr = defined $parm{'subprob_arr'} ? @{$parm{'subprob_arr'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _set_labels {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _scan_to_subproblems {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _read_block_structures {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'prob_arr' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in output::problem->_read_block_structures: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in output::problem->_read_block_structures: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in output::problem->_read_block_structures: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->_read_block_structures: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in output::problem->_read_block_structures: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @prob_arr = defined $parm{'prob_arr'} ? @{$parm{'prob_arr'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

1;

