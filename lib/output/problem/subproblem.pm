package output::problem::subproblem;

use include_modules;
use Config;
use ext::Math::MatrixReal;
use ui;
use array;
use Moose;
use MooseX::Params::Validate;
use Scalar::Util qw(looks_like_number);

has 'subproblem_id' => ( is => 'rw', isa => 'Int' );
has 'skip_labels_matrix' => ( is => 'rw', isa => 'Str' );
has 'next_to_last_step_successful' => ( is => 'rw', isa => 'Bool' );
has 'problem_id' => ( is => 'rw', isa => 'Int' );
has 'output_id' => ( is => 'rw', isa => 'Int' );
has 'model_id' => ( is => 'rw', isa => 'Int' );
has 'comegas' => ( is => 'rw', isa => 'ArrayRef' );
has 'condition_number' => ( is => 'rw', isa => 'Num' );
has 'covariance_step_successful' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'estimate_near_boundary' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'covariance_step_warnings' => ( is => 'rw', isa => 'Bool', default => 0 );
has 's_matrix_singular' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'csigmas' => ( is => 'rw', isa => 'ArrayRef' );
has 'cvseomegas' => ( is => 'rw', isa => 'ArrayRef' );
has 'cvsesigmas' => ( is => 'rw', isa => 'ArrayRef' );
has 'cvsethetas' => ( is => 'rw', isa => 'ArrayRef' );
has 'shrinkage_eta' => ( is => 'rw', isa => 'ArrayRef' );
has 'shrinkage_eps' => ( is => 'rw', isa => 'ArrayRef' );
has 'eigens' => ( is => 'rw', isa => 'ArrayRef' );
has 'etabar' => ( is => 'rw', isa => 'ArrayRef' );
has 'feval' => ( is => 'rw', isa => 'Num' );
has 'final_gradients' => ( is => 'rw', isa => 'ArrayRef' );
has 'finalparam' => ( is => 'rw', isa => 'ArrayRef' );
has 'funcevalpath' => ( is => 'rw', isa => 'ArrayRef' );
has 'gradient_path' => ( is => 'rw', isa => 'ArrayRef' );
has 'initgrad' => ( is => 'rw', isa => 'ArrayRef' );
has 'iternum' => ( is => 'rw', isa => 'ArrayRef' );
has 'nom' => ( is => 'rw', isa => 'Num' );
has 'npofv' => ( is => 'rw', isa => 'Num' );
has 'npomegas' => ( is => 'rw', isa => 'ArrayRef' );
has 'npetabars' => ( is => 'rw', isa => 'ArrayRef' );
has 'nrom' => ( is => 'rw', isa => 'Num' );
has 'nth' => ( is => 'rw', isa => 'Num' );
has 'ofvpath' => ( is => 'rw', isa => 'ArrayRef' );
has 'ofv' => ( is => 'rw', isa => 'Num', clearer => 'clear_ofv' );
has 'dic' => ( is => 'rw', isa => 'Num' );
has 'omegacoordval' => ( is => 'rw', isa => 'HashRef' );
has 'seomegacoordval' => ( is => 'rw', isa => 'HashRef' );
has 'parameter_path' => ( is => 'rw', isa => 'ArrayRef' );
has 'parsed' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'parsed_successfully' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'parsing_error_message' => ( is => 'rw', isa => 'Str' );
has 'pval' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_cormatrix' => ( is => 'rw', isa => 'ArrayRef' );
has 'correlation_matrix' => ( is => 'rw', isa => 'ArrayRef' );
has 'output_matrix_headers' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_covmatrix' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_invcovmatrix' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_omegas' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_seomegas' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_sesigmas' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_sigmas' => ( is => 'rw', isa => 'ArrayRef' );
has 'raw_tmatrix' => ( is => 'rw', isa => 'ArrayRef' );
has 'tmatrix' => ( is => 'rw', isa => 'ArrayRef' );
has 'significant_digits' => ( is => 'rw', isa => 'Num' );
has 'sigmacoordval' => ( is => 'rw', isa => 'HashRef' );
has 'sesigmacoordval' => ( is => 'rw', isa => 'HashRef' );
has 'simulationstep' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'minimization_successful' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'final_zero_gradients' => ( is => 'rw', isa => 'Num' );
has 'hessian_reset' => ( is => 'rw', isa => 'Num' );
has 'zero_gradients' => ( is => 'rw', isa => 'Num' );
has 'rounding_errors' => ( is => 'rw', isa => 'Num', default => 0 );
has 'minimization_message' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'thetacoordval' => ( is => 'rw', isa => 'HashRef' );
has 'sethetacoordval' => ( is => 'rw', isa => 'HashRef' );
has 'NM7_parsed_raw' => ( is => 'rw', isa => 'Int', default => 0 );
has 'NM7_parsed_additional' => ( is => 'rw', isa => 'HashRef');
has 'nm_output_files' => ( is => 'rw', isa => 'HashRef' );
has 'ignore_missing_files' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_omegas' => ( is => 'rw', isa => 'Bool' );
has 'have_sigmas' => ( is => 'rw', isa => 'Bool' );
has 'method_number' => ( is => 'rw', isa => 'Int' );
has 'input_problem' => ( is => 'rw', isa => 'model::problem' );
has 'classical_method' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'table_number' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'nm_major_version' => ( is => 'rw', isa => 'Int' );
has 'method_string' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'lstfile' => ( is => 'rw', isa => 'ArrayRef' );
has 'lstfile_pos' => ( is => 'rw', isa => 'Int', default => 0 );
has 'covariance_step_run' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'tablename' => ( is => 'rw', isa => 'Str' );
has 'tableidcolumn' => ( is => 'rw', isa => 'Int', default => 0 );
has 'omega_block_structure' => ( is => 'rw', isa => 'ArrayRef' );
has 'sigma_block_structure' => ( is => 'rw', isa => 'ArrayRef' );
has 'omega_block_structure_type' => ( is => 'rw', isa => 'Str' );
has 'sigma_block_structure_type' => ( is => 'rw', isa => 'Str' );
has 'omega_block_sets' => ( is => 'rw', isa => 'HashRef' );
has 'sigma_block_sets' => ( is => 'rw', isa => 'HashRef' );
has 'inverse_covariance_matrix' => ( is => 'rw', isa => 'Math::MatrixReal', clearer => 'clear_inverse_covariance_matrix' );
has 't_matrix' => ( is => 'rw', isa => 'ArrayRef' );
has 'estimated_thetas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'estimated_omegas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'estimated_sigmas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'parameter_significant_digits' => ( is => 'rw', isa => 'ArrayRef' );
has 'estimation_step_run' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nonparametric_step_run' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'msfi_used' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'finished_parsing' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'estimation_step_initiated' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'sum_estimation_time' => ( is => 'rw', isa => 'Num' );
has 'sum_covariance_time' => ( is => 'rw', isa => 'Num' );
has 'burn_in_convergence' => ( is => 'rw', isa => 'Bool' );
has 'covariance_matrix' => ( is => 'rw', isa => 'ArrayRef' );
has 'burn_in_iterations' => ( is => 'rw', isa => 'Int' );

	
sub BUILD
{
	my $self  = shift;

	$self->have_sigmas(1) if (defined $self->input_problem()->sigmas() and scalar(@{$self->input_problem()->sigmas()}) > 0);

	$self->have_omegas(1) if (defined $self->input_problem()->omegas() and scalar(@{$self->input_problem()->omegas()}) > 0);
	$self->final_gradients([]);


	while (1) {
		#we will always break out of this loop, use as simple way of finishing parsing early. Cannot return because of dia code
		$self -> _read_simulation;
		if ($self->nm_major_version < 7) {
			if( $self -> estimation_step_run()) {
				$self -> _read_iteration_path;
				last unless ($self -> parsed_successfully() and not $self -> finished_parsing());
				$self -> _read_term();
			}
		} else {
			#NM7
			#When NM7 do not know yet if estimation_step_run actually set correctly, only whether we have a $EST. 
			#Could be a MAXEVAL=0, so must check the right #METH line for (Evaluation)
			#Regardless, we still need to scan to the right method
			#have separate method for NM7 find right #METH
			#But sometimes #METH not printed if simulation!
			if ($self->estimation_step_run() ){ #this is true if we have $EST
				$self -> _scan_to_meth(); #inside routine set estimation_step_run to false if (Evaluation) on #METH line
			}
			last unless ($self -> parsed_successfully() and not $self -> finished_parsing());
			if( $self -> estimation_step_run()) { #this might be false now even if true above
				$self -> _read_iteration_path;
				last unless ($self -> parsed_successfully() and not $self -> finished_parsing());
				$self -> _read_term();
			}


		}
		last unless ($self -> parsed_successfully() and not $self -> finished_parsing());

		if( $self -> estimation_step_initiated() ) {
			#this is often true even if do not have $EST. Sometimes there will be a #METH, sometimes not.
			$self -> parse_NM7_raw() if (defined $self->nm_output_files->{'raw'});

			$self -> _read_ofv()                  unless ($self -> NM7_parsed_raw());
			last unless ($self -> parsed_successfully() and not $self -> finished_parsing());

			$self -> _read_thomsi()               unless ( $self -> NM7_parsed_raw());
			if ($self -> parsed_successfully() and defined $self -> omegas() ) {
				$self -> _compute_comegas();
			}
			if ($self -> parsed_successfully() and defined $self -> sigmas() ) {
				$self -> _compute_csigmas();
			}

			last unless ($self -> parsed_successfully() and not $self -> finished_parsing());

			if ($self -> covariance_step_run() ) {
				$self -> _read_sethomsi()             unless ($self -> NM7_parsed_raw() );
				if ($self -> parsed_successfully()) {
					$self -> _compute_cvsetheta();
					$self -> _compute_cvseomega();
					$self -> _compute_cvsesigma();
				}
			}
		}

		last unless ($self -> parsed_successfully() and not $self -> finished_parsing());

		if ($self -> covariance_step_run()) {
			$self -> parse_NM7_additional() if ((defined $self->nm_output_files->{'cov'} or
												defined $self->nm_output_files->{'cor'} or
												defined $self->nm_output_files->{'coi'}) and 
												$self -> NM7_parsed_raw());
			$self -> _read_covmatrix()          unless (defined $self -> NM7_parsed_additional() and
														$self -> NM7_parsed_additional()->{'cov'} and 
														$self -> NM7_parsed_additional()->{'cor'} and 
														$self -> NM7_parsed_additional()->{'coi'} );
			last unless ($self -> parsed_successfully() and not $self -> finished_parsing());

			$self -> _read_eigen()              if (not $self -> NM7_parsed_raw());
		}
		last unless ($self -> parsed_successfully() and not $self -> finished_parsing());

		if( $self -> nonparametric_step_run() ) {
			$self -> _read_npomegas();
		}
		last; #always break loop here if not done already
	}
	delete $self -> {'lstfile'};
}


sub tableobject
{
	my $self = shift;
	my $table;

	return $table;
}

sub access_any
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 attribute => { isa => 'Str', optional => 1 }
	);
	my $attribute = $parm{'attribute'};
	my $return_value;

	return;
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

sub _read_covmatrix
{
	my $self = shift;

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
						_read_matrixoestimates( pos => $start_pos-1 );# and last;
					$self->raw_tmatrix($temp_matrix);
					last;
				}
			}
			last;		 # No covariance matrix will be found!
		}
		if (/    COVARIANCE MATRIX OF ESTIMATE/) {
			while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
				if (/^ TH (\d)/ or /^\s+TH (\d) \| /) { # Read matrix and get out of inner while loop
					my $temp_matrix;
					( $start_pos, $temp_matrix, $c_success, $dummyheaders ) = $self -> _read_matrixoestimates( pos => $start_pos - 1 );# and last;
					unless (defined $self->raw_covmatrix and scalar(@{$self->raw_covmatrix})>0){
						#only store if not already read from NM7 additional output
						$self->raw_covmatrix($temp_matrix);
					}
					last;
				}
			}
		}
		if (/    CORRELATION MATRIX OF ESTIMATE/) {
			while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
				if (/^ TH (\d)/ or /^\s+TH (\d) \| /) { # Read matrix and get out of inner while loop
					my $temp_matrix;
					( $start_pos, $temp_matrix, $corr_success, $headers ) = $self -> _read_matrixoestimates( pos => $start_pos - 1 );# and last;
					$self->raw_cormatrix($temp_matrix);
					last;
				}
			}
		}
		if (/    INVERSE COVARIANCE MATRIX OF ESTIMATE/) {
			while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
				if (/^ TH (\d)/ or /^\s+TH (\d) \| /) { # Read matrix and get out of inner while loop
					my $temp_matrix;
					( $start_pos, $temp_matrix, $i_success, $dummyheaders ) = $self -> _read_matrixoestimates( pos => $start_pos - 1 );# and last;
					$self->raw_invcovmatrix($temp_matrix);
					last;
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

	unless (defined $self->covariance_matrix and scalar(@{$self->covariance_matrix})>0){
		#only store if not already read from NM7 additional output
		$self->covariance_matrix([]);
		$self->raw_covmatrix([]);
		foreach my $element ( @{$self->raw_covmatrix} ) {
			push( @{$self->covariance_matrix}, eval($element) ) unless ( $element eq '.........' );
		}
	}

	unless (defined $self->correlation_matrix and scalar(@{$self->correlation_matrix})>0){
		#only store if not already read from NM7 additional output
		$self->correlation_matrix([]);
		$self->raw_cormatrix([]);
		foreach my $element ( @{$self->raw_cormatrix} ) {
			push( @{$self->correlation_matrix}, eval($element) ) unless ( $element eq '.........' );
		}
	}

	unless (defined $self->inverse_covariance_matrix){
		#only store if not already read from NM7 additional output
		if ( defined $self->raw_invcovmatrix ) {
			my $matrix_ref = make_square( clear_dots( $self->raw_invcovmatrix ));
			if (scalar(@{$matrix_ref}) > 0) {
				$self->inverse_covariance_matrix(Math::MatrixReal->new_from_cols($matrix_ref));
			}
		}
	}
	unless (defined $self->output_matrix_headers and scalar(@{$self->output_matrix_headers})>0){
		#only store if not already read from NM7 additional output
		if (defined $headers) {
			#need to get rid of row headers for lines with only .....
			my $row = 1;
			my $col = 1;
			foreach my $element ( @{$self->raw_cormatrix } ) {
				if (($col == 1) and ($element ne '.........' )) {
					$self->output_matrix_headers([]);
					push( @{$self->output_matrix_headers}, $headers->[$row - 1]); 
				}
				$col++;
				if ($col > $row) {
					$col = 1;
					$row++;
				}
			}
		}
	}
	#If something has gone right!
	$self->lstfile_pos($start_pos) if ( $t_success + $c_success + $corr_success + $i_success );
}

sub _read_eigen
{
	my $self = shift;

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
			last if ( /^\s*\*/ or /^1/ or /^\s*#/);
			push( @eigens, split );
		}
		$start_pos-- and last if ( /^ PROBLEM.*SUBPROBLEM/ or /^ PROBLEM NO\.:\s+\d/ );
		$start_pos-- and last if (/^[a-df-zA-DF-Z]/); #Rewind one step
		$start_pos-- and last if (/^\s*#/); #For example #CPUT tag
	}
	if ( scalar @eigens > 0 ) {
		my @list = sort { $a <=> $b } @eigens;
		$self->condition_number( abs($list[$#list] / $list[0]) ) if ( $list[0] != 0 );
	}
	$self->eigens(\@eigens);
}

sub _read_eval
{
	my $self = shift;

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

sub _read_iteration_path
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 scan_only => { isa => 'Bool', default => 0, optional => 0 }
	);
	my $scan_only = $parm{'scan_only'};

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
					unless (($string =~ $self->method_string) or ($string eq $self->method_string)) {
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

sub _scan_to_meth
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 scan_only => { isa => 'Bool', default => 0, optional => 0 }
	);
	my $scan_only = $parm{'scan_only'};

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
						unless (defined $self -> next_to_last_step_successful){
							print "Failed to read minimization status from next to last \$EST when last \$EST is IMP EONLY=1. minimization_successful may not be correct.\n";
						}
						last; #break inner loop
					}
					if ( /(PORTION|OPTIMIZATION|BURN-IN|EXPECTATION ONLY PROCESS)( WAS | )NOT COMPLETED/ ) {
						if ( /USER INTERRUPT/ ) {
							$self->next_to_last_step_successful(1);
						} else {
							$self->next_to_last_step_successful(0);
							last; #do not read anything more, this is a failure
						}
					} elsif ( /(PORTION|OPTIMIZATION|BURN-IN|EXPECTATION ONLY PROCESS)( WAS | )NOT TESTED/ ) {
						$self->next_to_last_step_successful(1);
					} elsif ( /(PORTION|OPTIMIZATION|BURN-IN|EXPECTATION ONLY PROCESS)( WAS | )COMPLETED/ ) {
						$self->next_to_last_step_successful(1);
					}

					if ( /^0MINIMIZATION SUCCESSFUL/ ) {
						$self -> next_to_last_step_successful(1);
					}
					# "0ERROR RMATX-  1" should (if it exists) occur after the minim. succ message
					if ( /0ERROR RMATX-  1/ ) {
						$self -> next_to_last_step_successful(0);
						last;
					}

					if ( /^0MINIMIZATION TERMINATED/ ) {
						$self -> next_to_last_step_successful(0);
						last;
					}

					if ( /^0SEARCH WITH ESTIMATION STEP WILL NOT PROCEED/ ) {
						$self -> next_to_last_step_successful(0);
						last;
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

sub _read_npomegas
{
	my $self = shift;

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

sub _read_ofv
{
	my $self = shift;

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
				$self->clear_ofv;
			} else {
				$_ = @{$self->lstfile}[ $start_pos++];
				$self -> ofv($ofvt);
				$start_pos--;
			}
			last;
		}
	}
	$self->lstfile_pos($start_pos);
}

sub _read_sethomsi
{
	my $self = shift;

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

sub _read_significant_digits
{
	my $self = shift;

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

sub _read_simulation
{
	my $self = shift;

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

sub _read_term
{
	my $self = shift;

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

sub _read_minimization_message
{
	my $self = shift;

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

	  if (/^\s*\#TERE:/){
		  $success = 1;
		  last;
	  }else{
		  push @mess, $_  ;
	  }
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

sub _read_thomsi
{
	my $self = shift;

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
	$row = 1;
	$col = 1;
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

sub _read_tables
{
	my $self = shift;


}

sub _compute_comegas_or_csigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  omega_or_sigma => { isa => 'Str', optional => 0 }
		);
	my $omega_or_sigma = $parm{'omega_or_sigma'};

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

sub _compute_comegas
{
	my $self = shift;

  $self -> _compute_comegas_or_csigmas( omega_or_sigma => 'omega' );
}

sub _compute_csigmas
{
	my $self = shift;

  $self -> _compute_comegas_or_csigmas( omega_or_sigma => 'sigma' );
}

sub _compute_cvsesigma
{
	my $self = shift;

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

sub _compute_cvseomega
{
	my $self = shift;

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

sub _compute_cvsetheta
{
	my $self = shift;

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

sub _rowcolind
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 index => { isa => 'Int', optional => 1 }
	);
	my $index = $parm{'index'};
	my $row;
	my $col;

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

	return $row ,$col;
}

sub thetas
{
	my $self = shift;
	my @values;

	if (defined $self->thetacoordval) {
    my %valueshash = %{$self->thetacoordval};
    my @names = @{$self->thetanames()};
    foreach my $name (@names) {
      push(@values, $valueshash{$name});
    }
  }

	return \@values;
}

sub est_thetanames
{
	my $self = shift;
	my @values;

  if (defined $self->output_matrix_headers) {
    my @names = @{$self->output_matrix_headers};
    foreach my $name (@names) {
      if ($name =~ /THETA/) {
				push(@values, $name);
      }
    }
  }

	return \@values;
}

sub thetanames
{
	my $self = shift;
	my @names;

  if (defined $self->thetacoordval) {
    my %valueshash = %{$self->thetacoordval};
    @names = sort cmp_coords (keys %valueshash);
  }

	return \@names;
}

sub sethetas
{
	my $self = shift;
	my @values;

  if ( (defined $self->thetacoordval) and (defined $self->sethetacoordval) ) {
    my %sehash = %{$self->sethetacoordval};
    my @names = @{$self->thetanames()};
    foreach my $name (@names) {
      my $val= defined $sehash{$name} ? $sehash{$name}: undef;
      push(@values,$val);
    }
  }

	return \@values;
}

sub omegas
{
	my $self = shift;
	my @values;

  if (defined $self->omegacoordval) {
    my %valueshash = %{$self->omegacoordval};
    my @names = @{$self->omeganames()};
    foreach my $name (@names) {
      push(@values, $valueshash{$name});
    }
  }

	return \@values;
}

sub est_omeganames
{
	my $self = shift;
	my @values;

  if (defined $self->output_matrix_headers) {
    my @names = @{$self->output_matrix_headers};
    foreach my $name (@names) {
      if ($name =~ /OMEGA/) {
				push(@values, $name);
      }
    }
  }

	return \@values;
}

# Comparison method for omeganames, sigmanames and thetanames
sub cmp_coords
{
	if ($a =~ /THETA/) {
		return substr($a,5) <=> substr($b,5);
	} else {
		($a.$b) =~ /\((\d+)\,(\d+)\)[A-Z]*\((\d+)\,(\d+)\)/;
		return $2+(($1-1)*$1)/2 <=> $4+(($3-1)*$3)/2; 
	}
}

sub omeganames
{
	my $self = shift;
	my @names;

  if (defined $self->omegacoordval) {
    my %valueshash = %{$self->omegacoordval};
    @names = sort cmp_coords (keys %valueshash);
  }

	return \@names;
}

sub cvseomegacoordval
{
	my $self = shift;
	my %hash;

  if (defined $self->omegacoordval and defined $self->cvseomegas) {
    my @names = @{$self->omeganames()};
    my @cvse = @{$self->cvseomegas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $cvse[$i];
    }
  }

	return \%hash;
}

sub cvsesigmacoordval
{
	my $self = shift;
	my %hash;

  if (defined $self->sigmacoordval and defined $self->cvsesigmas) {
    my @names = @{$self->sigmanames()};
    my @cvse = @{$self->cvsesigmas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $cvse[$i];
    }
  }

	return \%hash;
}

sub cvsethetacoordval
{
	my $self = shift;
	my %hash;

  if (defined $self->thetacoordval and defined $self->cvsethetas) {
    my @names = @{$self->thetanames()};
    my @cvse = @{$self->cvsethetas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $cvse[$i];
    }
  }

	return \%hash;
}

sub comegacoordval
{
	my $self = shift;
	my %hash;

  if (defined $self->omegacoordval and defined $self->comegas) {
    my @names = @{$self->omeganames()};
    my @val = @{$self->comegas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $val[$i];
    }
  }

	return \%hash;
}

sub csigmacoordval
{
	my $self = shift;
	my %hash;

  if (defined $self->sigmacoordval and defined $self->csigmas) {
    my @names = @{$self->sigmanames()};
    my @val = @{$self->csigmas()};
    for (my $i = 0; $i <= $#names; $i++) {
      $hash{$names[$i]} = $val[$i];
    }
  }

	return \%hash;
}

sub seomegas
{
	my $self = shift;
	my @values;

  if ( (defined $self->omegacoordval) and (defined $self->seomegacoordval) ) {
    my %sehash = %{$self->seomegacoordval};
    my @names = @{$self->omeganames()};
    foreach my $name (@names){
      my $val= defined $sehash{$name} ? $sehash{$name}: undef;
      push(@values, $val);
    }
  }

	return \@values;
}

sub sigmas
{
	my $self = shift;
	my @values;

  if (defined $self->sigmacoordval) {
    my %valueshash = %{$self->sigmacoordval};
    my @names = @{$self->sigmanames()};
    foreach my $name (@names) {
      push(@values, $valueshash{$name});
    }
  }

	return \@values;
}

sub est_sigmanames
{
	my $self = shift;
	my @values;

  if (defined $self->output_matrix_headers) {
    my @names = @{$self->output_matrix_headers};
    foreach my $name (@names) {
      if ($name =~ /SIGMA/) {
				push(@values,$name);
      }
    }
  }

	return \@values;
}

sub sigmanames
{
	my $self = shift;
	my @names;

  if (defined $self->sigmacoordval) {
    my %valueshash = %{$self->sigmacoordval};
    @names = sort cmp_coords (keys %valueshash);
  }

	return \@names;
}

sub sesigmas
{
	my $self = shift;
	my @values;

  if ( (defined $self->sigmacoordval) and (defined $self->sesigmacoordval) ) {
    my %sehash = %{$self->sesigmacoordval};
    my @names = @{$self->sigmanames()};
    foreach my $name (@names) {
      my $val = defined $sehash{$name} ? $sehash{$name}: undef;
      push(@values, $val);
    }
  }

	return \@values;
}

sub get_NM7_table_numbers
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 line_array => { isa => 'Ref', optional => 0 },
		 method_string => { isa => 'Str', default => 'T', optional => 1 }
	);
	my $line_array = $parm{'line_array'};
	my $method_string = $parm{'method_string'};
	my @table_numbers;
	my $not_found;

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

	return \@table_numbers ,$not_found;
}

sub get_column_index_order
{
	#static method
	# create array of column indices in the order that columns should appear in the final formatted matrix
	my %parm = validated_hash(\@_,
		 header_label => { isa => 'Ref', optional => 0 },
		 have_sigmas => { isa => 'Bool', optional => 0 },
		 have_omegas => { isa => 'Bool', optional => 0 },
		 skip_labels_matrix => { isa => 'Str', optional => 0 }
	);
	my $header_label = $parm{'header_label'};
	my $have_sigmas = $parm{'have_sigmas'};
	my $have_omegas = $parm{'have_omegas'};
	my $skip_labels_matrix = $parm{'skip_labels_matrix'};
	my @index_order;

	#input @header_label
	#output \@index_order

	@index_order = ();
	my @sigma_order = ();
	for (my $i = 1; $i < scalar(@{$header_label}); $i++) {
	  if (index($skip_labels_matrix, $header_label->[$i]) >= 0) {
	    next;
	  } elsif ($header_label->[$i] =~ /THETA/ ) {
	    push (@index_order, $i);
	  } elsif  ($header_label->[$i] =~ /SIGMA/ ) {
	    push (@sigma_order, $i) if $have_sigmas;
	  } elsif  ($header_label->[$i] =~ /OMEGA/ ) {
	    push (@index_order, $i) if $have_omegas;
	  }
	}
	push (@index_order,@sigma_order);
	return \@index_order;

}

sub permute_and_clean_rows
{
	#static method
	#returns modifed version of input tableref, rearranges rows so that sigma comes last,
	# also skips rows for omega/sigma that were not estimated as indicated by skip_labels_matrix 
	my %parm = validated_hash(\@_,
		 tableref => { isa => 'Maybe[ArrayRef]', optional => 0 },
		 skip_labels_matrix => { isa => 'Str', optional => 0 },
		 have_sigmas => { isa => 'Bool', optional => 0 },
		 have_omegas => { isa => 'Bool', optional => 0 },
	);
	my $tableref = $parm{'tableref'};
	my $skip_labels_matrix = $parm{'skip_labels_matrix'};
	my $have_sigmas = $parm{'have_sigmas'};
	my $have_omegas = $parm{'have_omegas'};

	my $found_table=0;
	my @temp_array = ();
	my @sigma_array = ();
	if (defined $tableref and scalar (@{$tableref}) > 1) {
	    foreach my $line (@{$tableref}) {
			if ($line =~ /^\s*TABLE NO.\s+(\d+):/ ) {
				croak("two tables found where 1 expected in permute_and_clean_rows" ) if $found_table;
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
						push (@sigma_array,$line) if $have_sigmas;
					} elsif ($label =~ /OMEGA/ ) {
						push (@temp_array,$line) if $have_omegas;
					} else { #assume THETA
						push (@temp_array,$line);
					}
				}
			}
	    }
		push(@temp_array, @sigma_array); #add the sigma rows at the end
	}
	return \@temp_array;

}

sub get_NM7_table_method
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 line_array => { isa => 'Ref', optional => 0 },
		 table_number => { isa => 'Int', optional => 0 }
	);
	my $line_array = $parm{'line_array'};
	my $table_number = $parm{'table_number'};
	my $method_string;
	my $not_found;

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

	return $method_string ,$not_found;
}

sub get_NM7_tables
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 line_array => { isa => 'Ref', optional => 0 },
		 table_numbers => { isa => 'Ref', optional => 0 }
	);
	my $line_array = $parm{'line_array'};
	my $table_numbers = $parm{'table_numbers'};
	my @tableline_array;
	my @not_found_array;

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

	return \@tableline_array ,\@not_found_array;
}

sub get_NM7_tables_all_types
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 table_numbers => { isa => 'Ref', optional => 0 },
		 raw_array => { isa => 'Ref', optional => 0 },
		 cov_array => { isa => 'Ref', optional => 1 },
		 cor_array => { isa => 'Ref', optional => 1 },
		 coi_array => { isa => 'Ref', optional => 1 },
		 phi_array => { isa => 'Ref', optional => 1 },
		 covariance_step_run => { isa => 'Bool', optional => 0 }
	);
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

	return \@raw_table ,\@cov_table ,\@cor_table ,\@coi_table ,\@phi_table;
}

sub _get_value{
	#static method, translate text in NM7 ext,cov, coi etc to either number or undef
	my %parm = validated_hash(\@_,
							  val => { isa => 'Any', optional => 0 }
		);
	my $val = $parm{'val'};
	my $no_value = 10000000000;
	my $answer=undef;
	if(looks_like_number($val)){
		$answer = eval($val);
		#check if +-infinity
		if ( ($answer == $no_value) or ($answer == -(10**10**10)) or ($answer == (10**10**10))){
			$answer = undef;
		}
	}
	return $answer;
}

sub parse_NM7_raw
{
	my $self = shift;

	#Assume that we have only one table now, in $self->nm_output_files->{'raw'}
	#Assume whitespace as field separator
	#add error checking of separator

	my $skip_labels_matrix = ' ';
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
	my $read_standard_errors = 0;
	my $given_header_warning = 0;
	my (%thetacoordval, %omegacoordval, %sigmacoordval);
	my (%sethetacoordval, %seomegacoordval, %sesigmacoordval);
	my $header_ok = 0;
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
				my $val = _get_value(val => $values[$i]);
				if (defined $val){
					if ($header_labels[$i] =~ /THETA/){
						$thetacoordval{$header_labels[$i]} = $val; 
					} elsif ($header_labels[$i] =~ /OMEGA/) {
						$omegacoordval{$header_labels[$i]} = $val;
					} elsif ($header_labels[$i] =~ /SIGMA/) {
						$sigmacoordval{$header_labels[$i]} = $val;
					} elsif ($header_labels[$i] =~ /OBJ/) {
						$self->ofv($val);
					} else { 
						my $mes = "Unknown header label ".$header_labels[$i]." in raw output.";
						croak($mes);
					}
				}else{
					$skip_labels_matrix .= $header_labels[$i]." ";
				}
			}
		} elsif ($1 == -1000000001) {
			#standard errors
			$line =~ s/^\s*//; #get rid of leading spaces
			my @values = split /\s+/,$line;
			for (my $i = 1; $i < scalar(@values); $i++) {
				my $val = _get_value(val => $values[$i]);
				if (defined $val){
					if ($header_labels[$i] =~ /THETA/) {
						$sethetacoordval{$header_labels[$i]} = $val;
					} elsif ($header_labels[$i] =~ /OMEGA/) {
						$seomegacoordval{$header_labels[$i]} = $val;
					} elsif ($header_labels[$i] =~ /SIGMA/) {
						$sesigmacoordval{$header_labels[$i]} = $val;
					} elsif ($header_labels[$i] =~ /OBJ/) {
					} else { 
						my $mes = "Unknown header label ".$header_labels[$i]." in raw output.";
						croak($mes);
					}
				}else{
					$skip_labels_matrix .= $header_labels[$i]." ";
				}
			}
			$read_standard_errors = 1;
		} elsif ($1 == -1000000002) {
			#eigenvalues
			$line =~ s/^\s*//; #get rid of leading spaces
			my @values = split /\s+/,$line;
			for (my $i = 1; $i < scalar(@values); $i++) {
				last if ($values[$i] == 0); #array is padded with zeros
				$n_eigenvalues++;
				my $val = _get_value(val => $values[$i]);
				push (@eigenvalues, $val); #push also undefs
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

sub parse_additional_table
{
	#static method, parse line array of single table from either cov, cor, coi
	my %parm = validated_hash(\@_,
							  covariance_step_run => { isa => 'Bool', optional => 0 },
							  have_omegas => { isa => 'Bool', optional => 0 },
							  have_sigmas => { isa => 'Bool', optional => 0 },
							  method_string => { isa => 'Str', optional => 0 },
							  skip_labels_matrix => { isa => 'Str', optional => 0 },
							  type => { isa => 'Str', optional => 0 },
							  tableref => { isa => 'Maybe[ArrayRef]', optional => 0 }
		);
	my $covariance_step_run = $parm{'covariance_step_run'};
	my $have_omegas = $parm{'have_omegas'};
	my $have_sigmas = $parm{'have_sigmas'};
	my $method_string = $parm{'method_string'};
	my $skip_labels_matrix = $parm{'skip_labels_matrix'};
	my $type = $parm{'type'};
	my $tableref = $parm{'tableref'};
	my $success = 0;
	#must be done after raw
	unless ($type eq 'cov' or $type eq 'coi' or $type eq 'cor'){
		croak("unknown type $type in parse_NM7_additional");
	}

	#Assume that we have only one table now in $tableref
	#Assume whitespace as field separator
	#add error checking of separator

	my $given_header_warning = 0;

	my $expect_cov = $covariance_step_run;
	$expect_cov = 0 if ($method_string =~ /Stochastic Approximation/ );
	my @header_labels = ();
	my @matrix_array =();
	my @inverse;
	my $found_table = 0;
	my $cleaned_table_ref=[];
	if ($expect_cov and defined $tableref and scalar(@{$tableref})>1) {
		$cleaned_table_ref = permute_and_clean_rows(tableref => $tableref,
													skip_labels_matrix => $skip_labels_matrix,
													have_sigmas => $have_sigmas,
													have_omegas => $have_omegas);
	}

	$tableref = undef;
	my $row_index = 0;
	my @index_order=();
	my $header_ok = 0;
	foreach my $line (@{$cleaned_table_ref}) {
	    if ($line =~ /^\s*TABLE NO.\s+(\d+):/ ) {
			croak("two tables found where 1 expected for $type" ) if $found_table;
			$found_table = 1;
	    } elsif ($line =~ /^\s*NAME/ ) {
			$line =~ s/^\s*//; #get rid of leading spaces
			@header_labels = split /\s+/, $line;
			$header_ok = 1 if ($header_labels[0] eq 'NAME');
			@index_order = @{get_column_index_order(header_label=>\@header_labels,
													have_sigmas => $have_sigmas,
													have_omegas => $have_omegas,
													skip_labels_matrix => $skip_labels_matrix)};
		  
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
			if ($type eq 'coi') {
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
			if ($type eq 'coi')  {
				push(@matrix_array, \@new_line); #square matrix
			} else {
				push(@matrix_array, @new_line); #linear array
			}
			$success = 1;
	    }
	}

	return ($success,\@matrix_array,\@index_order,\@header_labels);
}

sub parse_NM7_additional
{
	my $self = shift;

	my $success;
	my $matrix_array_ref;
	my $index_order_ref;
	my $header_labels_ref;
	my %hash;

	unless ($self->NM7_parsed_raw){
		croak('parse_NM7_additional must be called *after* parse_NM7_raw');
		#because need skip_labels_matrix and have_sigmas and have_omegas and cov_step_run
	}
	
	foreach my $type ('cov','coi','cor'){
		$hash{$type}=0;
		next unless (defined $self->nm_output_files->{$type});

		($success,$matrix_array_ref,$index_order_ref,$header_labels_ref) = 
			parse_additional_table (covariance_step_run => $self->covariance_step_run,
									have_omegas => $self->have_omegas,
									have_sigmas => $self->have_sigmas,
									method_string => $self->method_string,
									skip_labels_matrix => $self->skip_labels_matrix,
									type => $type,
									tableref => $self->nm_output_files->{$type}
			);

		next unless ($success and defined $matrix_array_ref and scalar(@{$matrix_array_ref}) > 0);

		unless (defined $self->output_matrix_headers and scalar(@{$self->output_matrix_headers})>0){
			my @column_headers = ();
			foreach my $ind (@{$index_order_ref}) {
				push (@column_headers, $header_labels_ref->[$ind]);
			}
			$self->output_matrix_headers(\@column_headers);
		}
	
		if ($type eq 'cov') {
			$self->raw_covmatrix([]);
			push( @{$self->raw_covmatrix}, @{$matrix_array_ref});
			$self->covariance_matrix([]);
			foreach my $element ( @{$self->raw_covmatrix} ) {
				push( @{$self->covariance_matrix}, eval($element) ) 
					unless ( $element eq '.........' );
			}
		} elsif ($type eq 'cor') {
			$self->correlation_matrix([]);
			push( @{$self->correlation_matrix}, @{$matrix_array_ref});
		} elsif ($type eq 'coi') {
			$self->inverse_covariance_matrix(Math::MatrixReal -> new_from_cols($matrix_array_ref));
		} 
		$hash{$type}=1; #success
	}

	delete $self->nm_output_files->{'cov'};
	delete $self->nm_output_files->{'coi'};
	delete $self->nm_output_files->{'cor'};
	delete $self->nm_output_files->{'phi'};

	#what about t-matrix?
	#what about  phi

	$self->NM7_parsed_additional(\%hash);
}

sub _isdiagonal
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 index => { isa => 'Int', optional => 1 }
	);
	my $index = $parm{'index'};
	my $isdiagonal = 0;

	my $previ = 1;
	my $j;
	return(1) if $index == 1;
	foreach my $j (2 .. 100) {
		return(1) if $index == $previ + $j;
		$previ = $previ + $j;
		last if $index < $previ;
	}

	return $isdiagonal;
}

sub _return_function
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 hash => { isa => 'HashRef', optional => 1 },
		 scalar_return => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $scalar_return = $parm{'scalar_return'};
}

sub _read_matrixoestimates
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 pos => { isa => 'Int', default => 0, optional => 1 }
	);
	my $pos = $parm{'pos'};
	my @subprob_matrix;
	my $success = 0;
	my @row_headers;

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
	  next if ( /\s*#/ );			  # NONMEM tag

	  chomp;				# Get rid of line-feed
	  my @row = split;
		shift( @row ) if ( $row[0] eq '+' );	   # Get rid of +-sign

		next if ( $#row < 0 );			   # Blank row
	  
		push( @subprob_matrix, @row );
	}
	$success = 1 if ( scalar @subprob_matrix > 0 );

	return $pos ,\@subprob_matrix ,$success ,\@row_headers;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
