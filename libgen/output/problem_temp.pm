use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package output::problem;
use Carp;
use Data::Dumper;
use Config;

	my $nrec_exp = '^\s*NO. OF DATA RECS IN DATA SET:\s*(\d*)|^\s*TOT. NO. OF DATA RECS:\s*(\d*)';
	my $nobs_exp = ' TOT. NO. OF OBS RECS:\s*(\d*)';
	my $nind_exp = ' TOT. NO. OF INDIVIDUALS:\s*(\d*)';
	my $subprob_exp = '^ PROBLEM NO\.:\s*(\d+)\s*SUBPROBLEM NO\.:\s*(\d+)';
	my $method_exp = '^ #METH:\s*(.*)';
	my $est_time_exp = '^ Elapsed estimation time in seconds:\s*(.+)';
	my $cov_time_exp = '^ Elapsed covariance time in seconds:\s*(.+)';
	my $simulation_exp = '^ SIMULATION STEP PERFORMED';
  my %table_numbers;
  my %table_strings;
use debug;


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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 25 "lib/output/problem_subs.pm" 
	# Read Global data
	$this -> _read_nrecs();
	$this -> _read_nobs() if ( $this -> parsed_successfully() and not $this -> finished_parsing() );
	$this -> _read_nind() if ( $this -> parsed_successfully() and not $this -> finished_parsing() );
	$this -> _read_msfo_status() if ( $this -> parsed_successfully() and not $this -> finished_parsing() );

	$this -> _scan_to_subproblems() if ( $this -> parsed_successfully() and not $this -> finished_parsing() );
	
	if ($this->nm_major_version() >= 7 and ($this -> estimation_step_initiated() or $this->covariance_step_run())) {
		#we have output to read
	  $this->store_NM7_output(max_table_number => $this->table_number()); 
	}

	$this -> _read_subproblems() if ( $this -> parsed_successfully() and not $this -> finished_parsing() );

	my $mes = $this -> parsing_error_message();
	if( defined $this -> subproblems() ) {
	  foreach my $subp ( @{$this -> subproblems()} ) {
	    $mes .= $subp -> parsing_error_message();
	    $this -> parsed_successfully($this -> parsed_successfully() * $subp -> parsed_successfully());
	  }
	}

	$this -> parsing_error_message( $mes );

	$this->lstfile([]);
# line 150 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> access_any');
# line 1071 "lib/output/problem_subs.pm" 
	unless( scalar(@subproblems) > 0 ) {
		carp("subproblems undefined, using all." );
	  if( defined $self->subproblems ) {
	    @subproblems = (1 .. scalar @{$self->subproblems});
	  } else {
	    carp("No subproblems defined in this problem." );
	    @subproblems = ();
	  }
	}

	my @own_subproblems = defined $self->subproblems ? @{$self->subproblems} : ();

	foreach my $i ( @subproblems ) {
	  if ( defined $own_subproblems[$i - 1] ) {
	    carp("subproblems: $i" );
	    carp("Attribute: " . $own_subproblems[$i - 1] -> $attribute );
	    my $meth_ret = $own_subproblems[$i - 1] -> $attribute;

	    # Test if the returned value is an array (with hashes we
	    # can't allow selection based on parameter numbers, since
	    # a hash is not ordered)
	    if ( ref ( $meth_ret ) eq 'ARRAY' ) {
	      my @subprob_attr = @{$meth_ret};
	      if ( scalar @parameter_numbers > 0 ) {
					my @tmp_arr = ();
					foreach my $num ( @parameter_numbers ) {
		  			if ( $num > 0 and $num <= scalar @subprob_attr ) {
		    		push( @tmp_arr, $subprob_attr[$num-1] );
		  			} else {
		    			croak("( $attribute ): no such parameter number $num!" . "(" . scalar @subprob_attr . " exists)" );
		  			}
					}
					@subprob_attr = @tmp_arr;
	      }
	      push( @return_value, \@subprob_attr );
	    } else {
	      push( @return_value, $meth_ret );
	    }
	  } else {
	    croak("No such subproblem " . ($i - 1) );
	  }
	}
	# Check the return_value to see if we have empty arrays
	if ( $#return_value == 0  and ref ($return_value[0]) eq 'ARRAY' and scalar @{$return_value[0]} < 1 ) {
	   @return_value = ();
	}
# line 1041 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> access_any');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> full_name_NM7_file');
# line 64 "lib/output/problem_subs.pm" 
	unless ($file_type =~ /^(raw|cov|coi|cor|phi)$/) {
	  croak("illegal input $file_type to full_name_NM7_file");
	}
	if ($file_type eq 'raw') {
	  $full_name = $self -> directory() . $self -> filename_root() . '.ext';	  
	} else {
	  $full_name = $self -> directory() . $self -> filename_root() . '.' . $file_type;	  
	}
# line 1089 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> full_name_NM7_file');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> store_NM7_output');
# line 79 "lib/output/problem_subs.pm" 
	# put these in five arrays of arrays
	# also store five arrays of table strings

	#make sure no old data left
	%table_numbers = {};
	%table_strings = {};

	$self->nm_output_files({ 'raw' => [], 'cov' => [], 'coi' => [], 'cor' => [], 'phi' => [] });

	foreach my $type ('raw','cov','coi','cor','phi') {
		my $filename = $self -> full_name_NM7_file('file_type' => $type);
		if (-e $filename) {
			my @tmp = OSspecific::slurp_file($filename);
			unless (scalar (@tmp) > 0) {
				my $mes = "Empty file ".$filename;
				carp($mes);
			}
			my $prev_num = 0;
			my $prev_string;
			my $tab_index = -1;
			my $line_count = 0;
			foreach my $line (@tmp) {
				if ($line =~ /^\s*TABLE NO.\s+(\d+):\s*([^:]+)/) {
					my $number = $1;
				  my $string = $2;
				  chomp $string;
				  $string =~ s/\s*$//; #remove trailing spaces
			  
				  if ((defined $max_table_number) and ($number > $max_table_number)) {
				      last; #skip the rest, know we do not need them for this $PROB
				  }
				  if ($prev_num == 0) {
				      #first table, no lines to store
				      1; #ok
				  } elsif ($line_count == 0) {
				      #not first table but still no lines to store
				      #NONMEM sometimes prints table header without body, skip those headers,
				      #replace previously stored number and string
				      1; #ok
				  } elsif ($number <= $prev_num) {
				      #not first table, lines to store, but inconsistent numbering
				      last; #skip the rest, we can't handle it anyway
				      #we skip tables that must come from restarted numbering (<= previous number),
				      #NM7 onumbering in additional output is inconsistent and we cannot handle it
				  }
				  $prev_string = $string;
				  $prev_num = $number;
				  $line_count = 0;
		  	    
				} elsif ($line =~  /[0-9A-Za-z]/) {
					if ($line_count == 0 and ($prev_num > 0)) {
				  	#this is the first data/header line of new table 
				  	#store string and number that we must have just read in previous loop iteration
				  	if (defined $prev_string) {
					  	push(@{$table_strings{$type}}, $prev_string);
						}
				    $tab_index++;
				    push(@{$table_numbers{$type}}, $prev_num);
				  } elsif($line_count == 0 and ($prev_num == 0)) {
				  	#NONMEM gives some tables number 0. skip them
				    next;
				  }
				  push (@{$self->nm_output_files->{$type}->[$tab_index]}, $line);
				  $line_count++;
				}
			}

		} else {
		  my $mes = "Could not find NM7 output file $filename ";
		  carp($mes) unless $self->ignore_missing_files;
		}
	}
# line 1200 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> store_NM7_output');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> parsing_error');
# line 57 "lib/output/problem_subs.pm" 
	$self -> parsed_successfully( 0 );
	$self -> parsing_error_message( $message );
# line 1240 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> parsing_error');
	# End of Non-Dia code #

}

sub _read_nrecs {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_nrecs');
# line 314 "lib/output/problem_subs.pm" 
	# The data recs statement should always be present
	# Raise parsing error if not found
	my $errmess = "Error in reading the number of data records!\n";
	my $start_pos = $self->lstfile_pos;
	my $success  = 0;

	while ( $_ = @{$self->lstfile}[$start_pos++] ) {
		if ( /$nobs_exp/ or ($start_pos + 1) == scalar @{$self->lstfile} ) {
			carp($errmess."$!" );
			$self -> parsing_error( message => $errmess."$!" );
			return;
		}
		if ( /$nrec_exp/ ) {
			$self -> nrecs($1);
			$success = 1;
			last;
		}
	}

	if ( $success ) {
		$self->lstfile_pos($start_pos);
	} else {
		carp($errmess."$!" );
		$self -> parsing_error( message => $errmess."$!" );
	}
# line 1277 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_nrecs');
	# End of Non-Dia code #

}

sub _read_nobs {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_nobs');
# line 344 "lib/output/problem_subs.pm" 
	# The no of obs recs statement should always be present
	# Raise parsing error if not found
	my $errmess = "Error in reading the number of observation records!\n";
	my $start_pos = $self->lstfile_pos;
	my $success = 0;

	while ( $_ = @{$self->lstfile}[$start_pos++] ) {
		if ( /$nind_exp/ or ($start_pos + 1) == scalar @{$self->lstfile} ) {
			carp($errmess."$!" );
			$self -> parsing_error( message => $errmess."$!" );
			return;
		}
		if ( /$nobs_exp/ ) {
			$self -> nobs($1);
			$success = 1;
			last;
		}
	}

	if ( $success ) {
		$self->lstfile_pos($start_pos);
	} else {
		carp($errmess."$!" );
		$self -> parsing_error( message => $errmess."$!" );
	}
# line 1314 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_nobs');
	# End of Non-Dia code #

}

sub _read_nind {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_nind');
# line 374 "lib/output/problem_subs.pm" 
	# The no of individuals statement should always be present
	# Raise parsing error if not found
	my $errmess = "Error in reading the number of individuals!\n";
	my $start_pos = $self->lstfile_pos;
	my $success = 0;

	while ( $_ = @{$self->lstfile}[$start_pos++] ) {
  	if ( /^0LENGTH OF THETA/ or
       /^0MODEL SPECIFICATION FILE INPUT/ or
       ($start_pos + 1) == scalar @{$self->lstfile} ) {
    	carp($errmess."$!" );
    	$self -> parsing_error( message => $errmess."$!" );
    	return;
 		}
  	if ( /$nind_exp/ ) {
    	$self -> nind($1);
    	$success = 1;
    	last;
  	}
	}
	if ( $success ) {
  	$self->lstfile_pos($start_pos);
	} else {
  	carp($errmess."$!" );
		$self -> parsing_error( message => $errmess."$!" );
	}
# line 1352 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_nind');
	# End of Non-Dia code #

}

sub _read_eststep {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_eststep');
# line 774 "lib/output/problem_subs.pm" 
	# A combination of simulation and estimation step indications should always be found, raise error otherwise
	# we may have a sequence here in NM7

	my $start_pos = $self->lstfile_pos;
	my $success = 0;

	while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {

		if ( /^0COVARIANCE STEP OMITTED/ ) {
			unless( $success ) {
				if ($self->msfi_used()) {
					last;
				} else {
					# unless we have msfi this should not happen, raise error
					my $errmess = "Found $_ while searching for the simulation/estimation step indicators\n";
					carp($errmess . "$!" );
					$self -> parsing_error( message => $errmess . "$!" );
					return; 
				}
			}
		}
	if ( /^0TABLES STEP OMITTED/ ) {
		unless( $success ) {
			if ($self->msfi_used()) {
				last;
			} else {
				$self -> estimation_step_initiated(0);
				$self -> estimation_step_run(0);
				$self -> simulation_step_run(0);
				last;
			}
		}
	}

  if ( /^0NONPARAMETRIC STEP OMITTED/ or
       /^1/ or
       /^0MINIMIZATION/ or
       /^ PROBLEM NO\.:\s+\d/ ) {
		unless( $success ) {
      # This should not happen, raise error
      my $errmess = "Found $_ while searching for the simulation/estimation step indicators\n";
      carp($errmess."$!" );
      $self -> parsing_error( message => $errmess."$!" );
    }
    return;
  }

  if ( ($start_pos + 1) == scalar @{$self->lstfile} ) {
    #EOF This should not happen, raise error
    my $errmess = "Reached end of file while searching for the simulation/estimation step indicators\n";
    carp($errmess."$!" );
    $self -> parsing_error( message => $errmess."$!" );
    return;
  }

  if(/^ PRIOR SUBROUTINE USER-SUPPLIED/) {
    $success = 1;
  }

  if(/^0ESTIMATION STEP OMITTED:\s*\b(.*)\b/) {
    #we might end up here several times, but that is okay, since we want to store last one.
    $self -> estimation_step_initiated(1);
    $self -> estimation_step_run(0) if $1 eq 'YES';
    $self -> estimation_step_run(1) if $1 eq 'NO';
    $success = 1;
  }
  if(/^0SIMULATION STEP OMITTED:\s*\b(.*)\b/) {
    $self -> simulation_step_run(0) if $1 eq 'YES';
    $self -> simulation_step_run(1) if $1 eq 'NO';
    $success = 1;
  }
}

unless ( $success ) {
  carp("rewinding to first position..." );
} else {
  $self->lstfile_pos($start_pos);
}
# line 1442 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_eststep');
	# End of Non-Dia code #

}

sub _read_tablesstep {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_tablesstep');
# line 643 "lib/output/problem_subs.pm" 
	# The tables step is optional
  my $start_pos = $self->lstfile_pos;
	my $success = 0;

	while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
  	if ( /^1\s*$/ ) {
    	# This is ok, the tables step was not used.
    	$start_pos -= 2;
    	$success = 1;
    	last;
  	}
  	if( /^ PROBLEM NO\.:\s+\d/ or /^0MINIMIZATION/ ) {
    	# This should not happen, raise error
    	my $errmess = "Found $_ while searching for the (optional) tables step indicator\n";
			carp($errmess."$!" );
			$self -> parsing_error( message => $errmess."$!" );
			return;
		}

		if ( ($start_pos + 1) == scalar @{$self->lstfile} ) {
			#EOF This should not happen, raise error
    	my $errmess = "Reached end of file while  searching for the (optional) tables step indicator\n";
			carp($errmess."$!" );
			$self -> parsing_error( message => $errmess."$!" );
			return;
		}

		if (/^0TABLES STEP OMITTED:\s*\b(.*)\b/) {
			$self->tables_step_run(0) if $1 eq 'YES';
			$self->tables_step_run(1) if $1 eq 'NO';
			$success = 1;
			last;
		}
	}

	unless ( $success ) {
  	carp("rewinding to first position..." );
	} else {
  	$self->lstfile_pos($start_pos);
	}
# line 1494 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_tablesstep');
	# End of Non-Dia code #

}

sub _read_prior {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_prior');
# line 689 "lib/output/problem_subs.pm" 
  my $start_pos = $self->lstfile_pos;
  my $success = 0;
  while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
    if ( /^1\s*$/ or /0SEARCH WITH ESTIMATION STEP WILL NOT PROCEED/ ) {
      # This is ok, no user defined prior was used.
      $start_pos -= 2;
      $success = 1;
      last;
    }
    if( /^ PROBLEM NO\.:\s+\d/ or /^0MINIMIZATION/ ) {
      # This should not happen, raise error
      my $errmess = "Found $_ while searching for the (optional) user defined prior indicator\n";
      carp($errmess."$!" );
      $self -> parsing_error( message => $errmess."$!" );
      return;
    }
    
    if ( ($start_pos + 1) == scalar @{$self->lstfile} ) {
      #EOF This should not happen, raise error
      my $errmess = "Reached end of file while  searching for the (optional) user defined prior indicator\n";
      carp($errmess."$!" );
      $self -> parsing_error( message => $errmess."$!" );
      return;
    }
    
    if(/^ PRIOR SUBROUTINE USER-SUPPLIED/){
      $self->user_defined_prior(1);
      $success = 1;
      last;
    }
  }

  unless ( $success ) {
    carp("rewinding to first position..." );
  } else {
    $self->lstfile_pos = $start_pos;
  }
# line 1543 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_prior');
	# End of Non-Dia code #

}

sub _read_steps_allowed {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_steps_allowed');
# line 589 "lib/output/problem_subs.pm" 
	# These statements are optional. Return to start_pos if not found
	my $start_pos = $self->lstfile_pos;
	my $est_allowed  = 1;
	my $cov_allowed  = 1;
	my $nonp_allowed  = 1;
	my $tables_allowed  = 1; # I am not sure that this is actually something which can be marked as not valid

	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
		if ( /^ PROBLEM.*SUBPROBLEM/ or /^ PROBLEM NO\.:\s+\d/ or /^0ITERATION NO./ or /^0MINIMIZATION/) {
    	# This is ok, we should end up here
    	last;
  	}
  
  	if ( /0ESTIMATION STEP NOT ALLOWED/ ) {
    	$est_allowed = 0;
  	}

  	if ( /0COVARIANCE STEP NOT ALLOWED/ ) {
    	$cov_allowed = 0;
  	}

  	if ( /0NONPARAMETRIC STEP NOT ALLOWED/ ) {
    	$nonp_allowed = 0;
  	}

  	if ( /0TABLES STEP NOT ALLOWED/ ) { # As indicated above, this is unsure but this coding should not harm
    	$tables_allowed = 0;
  	}

  	if( /0INPUT MODEL SPECIFICATION FILE GENERATED FROM A NON-TERMINATING ESTIMATION STEP/ ) {
    	if( @{$self->lstfile}[ $start_pos ] =~ / BUT CONTINUING ESTIMATION STEP NOT IMPLEMENTED/ ) {
      	# If this happens, NONMEM aborts so we are finished reading
      	$self -> finished_parsing(1);
    	}
  	}

  	if( /0MODEL SPECIFICATION FILE IS EMPTY/ ) {
    	# If this happens, NONMEM aborts so we are finished reading
   		$self -> finished_parsing(1);
  	}
	}

	unless( ( $self -> estimation_step_initiated()    * $est_allowed ) or
		( $self -> covariance_step_run()    * $cov_allowed ) or
		( $self -> nonparametric_step_run() * $nonp_allowed ) or
		( $self -> tables_step_run() * $tables_allowed ) ) {
  	# If this happens, NONMEM aborts so we are finished reading
  	$self -> finished_parsing(1);
	}
# line 1604 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_steps_allowed');
	# End of Non-Dia code #

}

sub _read_nonpstep {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_nonpstep');
# line 732 "lib/output/problem_subs.pm" 
	# The nonparametric step is optional
	my $start_pos = $self->lstfile_pos;
	my $success = 0;

	while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
		if ( /^0COVARIANCE STEP OMITTED/ or 
       /0TABLES STEP OMITTED/ or
       /1DOUBLE PRECISION PREDPP/ or
       /0SEARCH WITH ESTIMATION STEP WILL NOT PROCEED/ or
       /^1/ or
       /^0MINIMIZATION/ or
       /^ PROBLEM NO\.:\s+\d/ ) {
    	# This is ok, the nonp step was not used.
    	last;
  	}

		if ( ($start_pos + 1) == scalar @{$self->lstfile} ) {
			#EOF This should not happen, raise error
			my $errmess = "Reached end of file while  searching for the (optional) nonparametric step indicator\n";
			carp($errmess."$!" );
			$self -> parsing_error( message => $errmess."$!" );
			return;
		}

		if(/^0NONPARAMETRIC STEP OMITTED:\s*\b(.*)\b/) {
    	$self->nonparametric_step_run(0) if $1 eq 'YES';
    	$self->nonparametric_step_run(1) if $1 eq 'NO';
    	$success = 1;
    	last;
		}
	}

	unless ( $success ) {
  	carp("rewinding to first position..." );
	} else {
  	$self->lstfile_pos($start_pos);
	}
# line 1653 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_nonpstep');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_covstep');
# line 857 "lib/output/problem_subs.pm" 
	my $start_pos = $self->lstfile_pos;
	my $success  = 0;

	while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {

	  if (/0COVARIANCE STEP OMITTED:\s*\b(.*)\b/) {
	    $self->covariance_step_run(0) if $1 eq 'YES';
	    $self->covariance_step_run(1) if $1 eq 'NO';
	    $success = 1;
	    last;
	  }
	  if ( /^ PROBLEM.*SUBPROBLEM/ or /^ PROBLEM NO\.:\s+\d/ or /^0ITERATION NO./ or /^0MINIMIZATION/) {
			# This is ok, we should end up here
	    last;
	  }
	}

	unless ( $success ) {
		carp("rewinding to first position..." );
	} else {
		$self->lstfile_pos($start_pos);
	}
# line 1713 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_covstep');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_arbitrary');
# line 408 "lib/output/problem_subs.pm" 
	my $start_pos = $self->lstfile_pos;
	my $success = 0;

	while ( $_ = @{$self->lstfile}[$start_pos++] ) {
		last if ( /^ PROBLEM.*SUBPROBLEM/ or /^ PROBLEM NO\.:\s+\d/ );
		if ( /$regexp/ ) {
	    $self -> { $member } = $1;
	    $success = 1;
	    last;
		}
  }
	if ( $success ) {
		$self->lstfile_pos($start_pos);
	} else {
		carp("rewinding to first position..." );
	}
# line 1768 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_arbitrary');
	# End of Non-Dia code #

}

sub _read_msfo_status {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_msfo_status');
# line 430 "lib/output/problem_subs.pm" 
{
  my $start_pos = $self->lstfile_pos;
  while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
    
    if( /^0SEARCH WITH ESTIMATION STEP WILL NOT PROCEED BEYOND PREVIOUS TERMINATION POINT/ ){
      $self -> msfo_has_terminated(1); # Means that $ESTIMATION
                                            # must be removed to enable continuation.
      $self -> finished_parsing(1);
    }
    
    if( /^0MODEL SPECIFICATION FILE IS EMPTY/ ){
      $self -> finished_parsing(1);
    }
  }
}
# line 1795 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_msfo_status');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_subproblems');
# line 159 "lib/output/problem_subs.pm" 
	my $subproblem_start;

	my $last_method_number = $self->n_previous_meth(); #initiation, start at number of #METH from prev problem
	my $subprob_method_number = 0; #to be used when scanning lst-file
	my $last_method_string; #of last #METH found
	my $classical_method = 1;
	my $subproblem_number;
	my $sum_estimation_time = 0;
	my $sum_covariance_time = 0;
	my $no_est = 0;
	my $no_meth = 0;
	while ( $_ = @{$self->lstfile}[ $self -> {'lstfile_pos'}++ ] ) {
		if( /$method_exp/ ) {
			# NONMEM will sometimes print #METH also when running without estimation, do not count
			# these occurences, which have ^1 line directly following #METH
			# and either empty method string or a string matching Evaluation or EVALUATION 
			# (MAXEVAL=0 will also have a ^1 line)
			my $string = $1;
			$string =~ s/\s*$//; #remove trailing spaces
			$no_est = 1 if ($self->lstfile->[$self->lstfile_pos] =~ /^1$/ and (length($string) < 1));
			if ($self->lstfile->[ $self->lstfile_pos - 2] =~ /^\s*\#TBLN:\s*(.*)/) {
				#if previous line is #TBLN then this will help us find right table in extra output
				#and also right #METH
				$self->table_number($1);
			}
			# should we also check for EVALUATION???
			unless ($no_est) {
				$last_method_number++;
				# when done this will be number of last #METH for this problem
				$last_method_string = $string;
				$subprob_method_number++;
			}

		} elsif ( /$est_time_exp/ ) {
			my $val = $1;
			$val =~ s/\s*$//; #remove trailing spaces
			$sum_estimation_time += $val;

		} elsif ( /$cov_time_exp/ ) {
			my $val = $1;
			$val =~ s/\s*$//; #remove trailing spaces
			$sum_covariance_time += $val;

		} elsif( /$subprob_exp/ or $self->lstfile_pos > $#{$self->lstfile} ) {
			if ( defined $subproblem_start or $self->lstfile_pos > $#{$self->lstfile}) {
				# we should submit subprob
				my @subproblem_lstfile;
				my $subproblem_index;
				if ( defined $subproblem_start) {
					@subproblem_lstfile = @{$self->lstfile}[$subproblem_start .. $self->lstfile_pos - 2];
					if ( $self->lstfile_pos > $#{$self->lstfile} ) { #we found end of file
						$subproblem_index = $subproblem_number; 
					} else { #we found new problem
						$subproblem_number = $2;
						$subproblem_index = $subproblem_number - 1; # Assuming problems come in order
					}
				} else {
					# whole file is one subprob
					@subproblem_lstfile = @{$self->lstfile}[0 .. $self->lstfile_pos - 1];
				}
				#for NM7

				if ($self->nm_major_version >= 7) {
					if ($last_method_number == $self->n_previous_meth()) {
						$no_meth = 1;
						carp("No METH: found in subproblem " . ($subproblem_index + 1) . " in lst-file" ) 
		    			unless ($self -> {'ignore_missing_files'} or $no_est or (not $self -> estimation_step_initiated())
			    			or ((not $self -> estimation_step_run()) and $self->simulation_step_run()  )); 
	    		}
	    		if ($last_method_string =~ /(Stochastic|Importance|Iterative|MCMC)/) {
						$classical_method = 0;
					}
				}

				$last_method_number = $self->table_number() if defined ($self->table_number());
				my %subprob;
				if ((defined $table_numbers{'raw'}) and (scalar(@{$table_numbers{'raw'}}) > 0) and ($no_meth < 1)) {
					my $index = 0;
					my $tab_index = -1;
					foreach my $num (@{$table_numbers{'raw'}}) {
						if ($last_method_number == $num) {
							$tab_index = $index;
							last;
						}
						$index++;
					}

				if ($tab_index < 0) {
					carp("table $last_method_number not found in raw output" ); 
				} else {
					# retrieve table and check that strings match
					$subprob{'raw'} = $self->nm_output_files->{'raw'}->[$tab_index];
					unless (($last_method_string =~ $table_strings{'raw'}->[$tab_index] ) or 
						($last_method_string eq $table_strings{'raw'}->[$tab_index]) or
						($table_strings{'raw'}->[$tab_index] =~ $last_method_string  ) ) {
						croak("method strings\n".$table_strings{'raw'}->[$tab_index] . " and\n"."$last_method_string do not match" );
					}
					for my $type ('cov','cor','coi','phi') {
						if (defined $table_numbers{$type}) {
							for (my $i = 0; $i < scalar(@{$table_numbers{$type}}); $i++ ) {
			    			if ($last_method_number == $table_numbers{$type}->[$i]) {
									$subprob{$type} = $self->nm_output_files->{$type}->[$i];
									last;
			    			}
							}
		    		}
					}
	    	}
			} else {
	    	1;
			}
	#end for NM7

#	print "adding subproblem where table number is ".$self -> table_number().", last method num $last_method_number\n";

      $self -> add_subproblem
	  ( 'init_data' => {lstfile                    => \@subproblem_lstfile,
			    ignore_missing_files       => $self -> {'ignore_missing_files'},
			    input_problem              => $self->input_problem(),
					nm_output_files						 => { 'raw' => $subprob{'raw'}, 'cov' => $subprob{'cov'}, 'cor' => $subprob{'cor'}, 'coi' => $subprob{'coi'}, 'phi' => $subprob{'phi'} },
			    method_string              => $last_method_string,
			    classical_method           => $classical_method,
			    nm_major_version           => $self -> nm_major_version(),
			    table_number               => $self -> table_number(),
			    method_number              => $subprob_method_number,
			    estimation_step_initiated  => $self -> estimation_step_initiated(),
			    estimation_step_run        => $self -> estimation_step_run(),
			    nonparametric_step_run     => $self -> nonparametric_step_run(),
			    covariance_step_run        => $self -> covariance_step_run(),
			    msfi_used                  => $self -> msfi_used(),
			    sum_estimation_time        => $sum_estimation_time,
			    sum_covariance_time        => $sum_covariance_time});

	@subproblem_lstfile = undef;
      %subprob = undef;

      $subprob_method_number = 0;
      $sum_estimation_time = 0;
      $sum_covariance_time = 0;
      $no_est = 0;
      $no_meth = 0;

    }
    unless ( $self->lstfile_pos > $#{$self->lstfile} ) {
      $subproblem_start = $self->lstfile_pos;
    }
  } # if subprob-exp or new prob or end of file
} #while scanning lines


# line 1983 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_subproblems');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _scan_to_subproblems');
# line 885 "lib/output/problem_subs.pm" 
	$self -> estimation_step_initiated(0);
	$self -> estimation_step_run(0);
	my $meth_printed = 1;

	if (defined $self->input_problem()->estimations()) {
		$self -> estimation_step_initiated(1);
		if ($self->nm_major_version < 7) {
		  #check if maxeval=0 in any $EST (can be split on multiple lines)
		  my @options;
		  foreach my $rec (@{$self->input_problem()->estimations()}) {
				push(@options, @{$rec->options()});
			}
		  my $found = 0;
		  foreach my $option ( @options ) {
				if ( defined $option and (($option -> name eq 'MAXEVALS') or ( index( 'MAXEVALS', $option -> name ) == 0 ))) {
					$found = 1 if ($option->value() eq '0');
			  	last;
				}
		  }
		  $self -> estimation_step_run(1) unless ($found);
		} else {
		  $self -> estimation_step_run(1); 
		  #incorrect if MAXEVAL=0 and NM7. if NM7 check for (Evaluation) string on #METH line of last subprob
		  #if (Evaluation) found then estimation_step was not run
		}

	} elsif (defined $self->input_problem()->simulations()) {
		my @options;
	  foreach my $rec (@{$self->input_problem()->simulations()}) {
		  push(@options,@{$rec->options()});
	  }
		my $onlysim = 0;
	  foreach my $option ( @options ) {
		  if ( defined $option and (($option -> name eq 'ONLYSIMULATION') or ( index( 'ONLYSIMULATION', $option -> name ) == 0 ))) {
		  	$onlysim = 1;
		    last;
		  }
	  }   
		unless ($onlysim) {
		  $self -> estimation_step_initiated(1);
		  $meth_printed = 0; #what if cov also? cannot be since would have $est and not end up here.
		}
	} elsif (defined $self->input_problem()->covariances()) {
		$self -> estimation_step_initiated(1) ;
	} else {
		my $tnpri=0;
	  if ((defined $self->input_problem()->priors()) and scalar(@{$self->input_problem()->priors()}) > 0 ) {
		  foreach my $rec (@{$self->input_problem()->priors()}) {
		  	foreach my $option ( @{$rec -> options} ) {
			  	if ((defined $option) and (($option->name eq 'TNPRI') || (index('TNPRI',$option ->name ) == 0))) {
			      $tnpri=1;
			  	}
		    }
		  }
	  }
    #if $PRIOR TNPRI and neither $SIM nor $EST then estimation step not initiated
    # (NONMEM does not allow task records, see nmhelp7 task, in this case)
    #otherwise
    #if end up here assume second $PROB and only $TABLE, or $MSFI and $TAB or $COV
    #then NM will still print maxeval etc
    #which we handle as estimation_step_initiated
    #NM will complain if neither simulation nor estimation nor correct $PRIOR TNPRI in first $PROB. 
    #Let nonmem handle it

    $self->estimation_step_initiated(1) unless $tnpri;
  }
	  
  $self->simulation_step_run(0);
  if (defined $self->input_problem()->simulations()) {
		$self->simulation_step_run(1);
  }

	$self->nonparametric_step_run(0);
	if (defined $self->input_problem()->nonparametrics()) {
		$self -> nonparametric_step_run(1);
	}
	  
  $self -> covariance_step_run(0);
	if (defined $self->input_problem()->covariances()) {
	  $self -> covariance_step_run(1);
	}
	  
	if ((not $self->simulation_step_run()) and ($self->nm_major_version < 7) and (not $self -> estimation_step_initiated())) {
		#nothing to read in this $PROB (only table info or $PRIOR TNPRI
	  $self -> finished_parsing(1);
	  return;
	}

	my $start_pos = $self->lstfile_pos;

	my $endstring;
	if ($self->simulation_step_run()) {
		# $endstring = $subprob_exp; #not always ok!!! if one prob one subprob and 7.1.2 then no subprob
	  #check for both if simulation_step
	  $endstring = $simulation_exp;
	} else {
		if ($self->nm_major_version >= 7) {
			#meth is not always printed with nm7, but happens only if simulation is run,
		  #so no problem here
		  $endstring =  '^\s*\#METH:';
	  } else {
		  #NM6
		  if ($self -> estimation_step_run()) {
		    $endstring = 'MONITORING OF SEARCH';
		  } elsif ($self -> estimation_step_initiated()) {
		    $endstring = 'MINIMUM VALUE OF OBJECTIVE FUNCTION';
		  } else {
		      #BUG, should already have returned
		      croak("BUG: should not end up here in scan_to_subproblems");
		  }
	  }
	}

	#		  /^0ITERATION NO./ or /^0MINIMIZATION/ or
	#		  /^\s*SIMULATION STEP PERFORMED/ or 
	#		  /MINIMUM VALUE OF OBJECTIVE FUNCTION/ or
	#		  /MONITORING OF SEARCH/ or
	#		  /^\s*\#METH:/ or
	#		  /^\s*\#TBLN:/ or
	

	my $found_endtime = 0;

	while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {

	  if ( /^ INITIAL ESTIMATE OF OMEGA HAS A NONZERO BLOCK WHICH IS NUMERICALLY NOT POSITIVE DEFINITE/ ) {
	  	$self -> parsing_error( message => $_ );
	    $self -> pre_run_errors($_);
	    $self -> finished_parsing(1);
	    return;
	  } elsif ( /^\s*0INPUT MODEL SPECIFICATION FILE GENERATED FROM A NON-TERMINATING ESTIMATION STEP/ ) {
	  	if( @{$self->lstfile}[ $start_pos ] =~ / BUT CONTINUING ESTIMATION STEP NOT IMPLEMENTED/ ) {
		  	# If this happens, NONMEM aborts so we are finished reading
		  	$self -> parsing_error( message => "INPUT MODEL SPECIFICATION FILE GENERATED FROM A NON-TERMINATING ESTIMATION STEP" );
		  	$self -> finished_parsing(1);
		  	return;
	    }
	  } elsif ( /^\s*0MODEL SPECIFICATION FILE IS EMPTY/ ) {
	      # If this happens, NONMEM aborts so we are finished reading
	      $self -> parsing_error( message => $_ );
	      $self -> finished_parsing(1);
	      return;
	  } elsif (( /^\s*0OBJ. FUNCT. IS NOT DEFINED/ ) and ($self -> estimation_step_initiated())) {
	      #this message does not necessarily mean failure!! only if we expect an estimation
	      $self -> parsing_error( message => $_ );
	      $self -> finished_parsing(1);
	      return;
	  } elsif ( /^\s*0RUN TERMINATED/ ) {
	      $self -> parsing_error( message => $_ );
	      $self -> finished_parsing(1);
	      return;
	  } elsif ($self->simulation_step_run() and /$subprob_exp/) {
	      last;
	  } elsif ( /$endstring/ ){
	      last;
	  } elsif (/^\s*(Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)/) {
	      #end time stamp, means nmfe finished fine
	      $found_endtime = 1;
	  } elsif (/^\s*Finished\s*(Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s*/){
	      #end time stamp, means nmfe finished fine
	      $found_endtime = 1;
	  }

	  if ( $start_pos > $#{$self->lstfile} ) { #we found end of file
	  	if ($found_endtime) {
		  	#run did not crash since nmfe printed end time, but nothing to parse
		  	$self -> parsing_error( message => "Found end of lst-file before any run information." );
		  	$self -> finished_parsing(1);
		  	return;
	  	} else {
		  	#EOF This should not happen, raise error
		  	my $errmess = "Reached end of file while scanning for subproblems\n";
		  	carp($errmess . "$!" );
		  	$self -> parsing_error( message => $errmess . "$!" );
		  	return;
			}
		}
	}
	$self->lstfile_pos($start_pos - 1);
# line 2182 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _scan_to_subproblems');
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
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_block_structures');
# line 451 "lib/output/problem_subs.pm" 
	#we do not call this routine with new parser
	#With new parser do not read anything. Block structures are only
	#used to check if parameters are near bounds. Rewrite that
	#routine in output_subs.pm
	  
	my $errmess = "Error in reading the block structures!";
	my $start_pos = $self->lstfile_pos;
	my $success = 1;
	
	my $obarea = 0;
	my $sbarea = 0;
	
	my $oblock_set = -1;
	my $sblock_set = -1;
	while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
	  if ( /^0MODEL SPECIFICATION FILE INPUT/ ) {
	    # We can't find anything but that's ok
	    $success = 1;
	    $start_pos--;
	    last;
	  }
	  if ( /^0INITIAL ESTIMATE/ ){
	      # We want to find this if we are currently reading omega
	      # or sigma block structures
	      $success = 1 if ( $sbarea or $obarea ); 
	      $start_pos --;
	      last;
	  }
	  if ( /^0DEFAULT OMEGA BOUNDARY TEST OMITTED:/ ) {
	      # We want to find this if we are currently reading omega
	      # block structure
	      $success = 1 if ( $obarea );
	      $obarea=0;
	      next;
	  }
	  if ( /^0DEFAULT SIGMA BOUNDARY TEST OMITTED:/ ) {
	      # We want to find this if we are currently reading sigma
	      # block structure
	      $success = 1 if ( $sbarea );
	      $sbarea=0;
	      #$start_pos --;
	      next;
	  }

	  if ( ($start_pos + 1) == scalar @{$self->lstfile} ) {
	    #EOF This should not happen, raise error
	    my $errmess = "Reached end of file while parsing block structures\n";
	    carp($errmess."$!" );
	    $self -> parsing_error( message => $errmess."$!" );
	    return;
	  }
	  
	  if(/0OMEGA HAS BLOCK FORM:/) {
	    $self->omega_block_structure_type('BLOCK');
	    $obarea = 1;
	    $success = 1;
	    next;
	  }
	  if(/0SIGMA HAS BLOCK FORM:/) {
	    $self->sigma_block_structure_type('BLOCK');
	    $sbarea = 1;
	    $obarea = 0;
	    $success = 1;
	    next;
	  }
	  if ( /^0OMEGA HAS SIMPLE DIAGONAL FORM/ ) {
	    $self->omega_block_structure_type('DIAGONAL');
	    $success = 1;
	    next;
	  }
	  if ( /^0SIGMA HAS SIMPLE DIAGONAL FORM/ ) {
	    $self->sigma_block_structure_type('DIAGONAL');
	    $success = 1;
	    last;
	  }
	  if ( $obarea ) {
	    my @row = split;
	    # All rows with the last but one element set to 0 indicate the start of a new block
	    #size is not really 'size', but the number of repetions of this block with BLOCK SAME
	    if ( $#row == 0 or $row[$#row-1] == 0 ) {
	      #this is the first row of a new block
	      # $#row == 0 indicates the first row of the full matrix.
	      # $row[$#row-1] == 0 indicates the first row of a new block
	      if ( $oblock_set == $row[$#row] ) {
					# If the same number as previous set
					# $row[$#row] is the block id number
					$self->omega_block_sets->{$oblock_set}{'size'}++;
	      } else {
					$oblock_set = $row[$#row];
					$self->omega_block_sets->{$oblock_set}{'size'} = 1;
	      }
	      # Always set dimension to 1 when starting a new block
	      $self->omega_block_sets->{$oblock_set}{'dimension'} = 1;
	    } else {
	      #this is a continuation of a previous block
	      $self->omega_block_sets->{$oblock_set}{'dimension'}++;
	    }
	    push( @{$self->omega_block_structure}, \@row );
	  }
	  if ( $sbarea ) {
	    my @row = split;
	    # All rows with the last but one element set to 0 indicate the start of a new block
	    if ( $#row == 0 or $row[$#row-1] == 0 ) {
	      # If the same number as previous set
	      if ( $sblock_set == $row[$#row] ) {
					$self->sigma_block_sets->{$sblock_set}{'size'}++;
	      } else {
					$sblock_set = $row[$#row];
					$self->sigma_block_sets->{$sblock_set}{'size'} = 1;
	      }
	      # Always set dimension to 1 when starting a new block
	      $self->sigma_block_sets->{$sblock_set}{'dimension'} = 1;
	    } else {
	      $self->sigma_block_sets->{$sblock_set}{'dimension'}++;
	    }
	    push( @{$self->sigma_block_structure}, \@row );
	  }
	}

	unless( defined $self->omega_block_structure_type ) {
	  $self->omega_block_structure_type('DIAGONAL');
	}
	unless( defined $self->sigma_block_structure_type ) {
	  $self->sigma_block_structure_type('DIAGONAL');
	}

	unless ( $success ) {
	  carp($errmess." 2 $!" );
	  $self -> parsing_error( message => $errmess." 2 $!" );
	} else {
	  $self->lstfile_pos($start_pos);
	}
# line 2352 libgen/output/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_block_structures');
	# End of Non-Dia code #

}

1;

