package output::problem;
use include_modules;
use Config;
use Mouse;
use MouseX::Params::Validate;
use output::problem::subproblem;
use utils::file;
use Time::Local;
use nmtablefile;

my $nrec_exp1 = '^\s*NO. OF DATA RECS IN DATA SET:\s*(\d+)';
my $nrec_exp2 = '^\s*TOT. NO. OF DATA RECS:\s*(\d+)';
my $nobs_exp = ' TOT. NO. OF OBS RECS:\s*(\d+)';
my $nind_exp = ' TOT. NO. OF INDIVIDUALS:\s*(\d+)';
my $subprob_exp = '^ PROBLEM NO\.:\s*(\d+)\s*SUBPROBLEM NO\.:\s*(\d+|\*\*\*\*)';
my $star_subprob_number=9999;
my $method_exp = '^ #METH:\s*(.*)';
my $est_time_exp = '^ Elapsed estimation\s* time in seconds:\s*(.+)';
my $cov_time_exp = '^ Elapsed covariance\s* time in seconds:\s*(.+)';
my $simulation_exp = '^ SIMULATION STEP PERFORMED';


has 'ext_file' => ( is => 'rw', isa => 'Maybe[nmtablefile]' );
has 'table_numbers_hash' => ( is => 'rw', isa => 'HashRef' );
has 'table_strings_hash' => ( is => 'rw', isa => 'HashRef' );
has 'subproblems' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'nm_output_files' => ( is => 'rw', isa => 'HashRef' );
has 'filename_root' => ( is => 'rw', isa => 'Str' );
has 'directory' => ( is => 'rw', isa => 'Str' );
has 'covariance_step_run' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'covariance_step_omitted' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'estimatedsigmas' => ( is => 'rw', isa => 'ArrayRef[Bool]', default => sub { [] } );
has 'estimatedthetas' => ( is => 'rw', isa => 'ArrayRef[Bool]', default => sub { [] } );
has 'estimatedomegas' => ( is => 'rw', isa => 'ArrayRef[Bool]', default => sub { [] } );
has 'finished_parsing' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'fixedomegas' => ( is => 'rw', isa => 'ArrayRef[Bool]', default => sub { [] } );
has 'fixedsigmas' => ( is => 'rw', isa => 'ArrayRef[Bool]', default => sub { [] } );
has 'fixedthetas' => ( is => 'rw', isa => 'ArrayRef[Bool]', default => sub { [] } );
has 'lower_theta_bounds' => ( is => 'rw', isa => 'ArrayRef[Num]' );
has 'lower_omega_bounds' => ( is => 'rw', isa => 'ArrayRef[Num]' );
has 'lower_sigma_bounds' => ( is => 'rw', isa => 'ArrayRef[Num]' );
has 'n_previous_meth' => ( is => 'rw', isa => 'Int', default => 0 );
has 'table_number' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'input_problem' => ( is => 'rw', isa => 'model::problem' );
has 'nm_major_version' => ( is => 'rw', isa => 'Int' );
has 'nm_version_710' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'evaluation_missing_from_ext_file' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'ext_file_has_evaluation' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'ignore_missing_files' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'lstfile' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'lstfile_pos' => ( is => 'rw', isa => 'Int', default => 0 );
has 'nind' => ( is => 'rw', isa => 'Num' );
has 'nobs' => ( is => 'rw', isa => 'Num' );
has 'nrecs' => ( is => 'rw', isa => 'Num' );
has 'omega_block_sets' => ( is => 'rw', isa => 'HashRef' );
has 'omega_block_structure' => ( is => 'rw', isa => 'ArrayRef' );
has 'omega_block_structure_type' => ( is => 'rw', isa => 'Str' );
has 'omega_indexes' => ( is => 'rw', isa => 'ArrayRef[ArrayRef[Int]]' );
has 'parsed' => ( is => 'rw', isa => 'Bool' );
has 'parsed_successfully' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'msfo_has_terminated' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'parsing_error_message' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'sigma_block_structure' => ( is => 'rw', isa => 'ArrayRef' );
has 'sigma_block_structure_type' => ( is => 'rw', isa => 'Str' );
has 'sigma_block_sets' => ( is => 'rw', isa => 'HashRef' );
has 'sigma_indexes' => ( is => 'rw', isa => 'ArrayRef[ArrayRef[Str]]' );
has 'tableidcolumns' => ( is => 'rw', isa => 'ArrayRef[Int]' );
has 'tablenames' => ( is => 'rw' );
has 'upper_omega_bounds' => ( is => 'rw', isa => 'ArrayRef[Num]' );
has 'upper_sigma_bounds' => ( is => 'rw', isa => 'ArrayRef[Num]' );
has 'upper_theta_bounds' => ( is => 'rw', isa => 'ArrayRef[Num]' );
has 'user_defined_prior' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'pre_run_errors' => ( is => 'rw', isa => 'Str' );
has 'estimation_step_run' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nonparametric_step_run' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'msfi_used' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'simulation_step_run' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'estimation_step_initiated' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'last_method_number' => ( is => 'rw', isa => 'Int' );
has 'tables_step_error' => ( is => 'rw', isa => 'Maybe[Str]', default => undef );
has 'iterations_interrupted' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'problem_index' => ( is => 'rw', isa => 'Int' );


sub BUILD
{
    my $self = shift;

    $self->_read_covstep();
    # Read Global data
    $self->_read_nrecs;
    $self->_read_nobs if ( $self -> parsed_successfully and not $self -> finished_parsing );
    $self->_read_nind if ( $self -> parsed_successfully and not $self -> finished_parsing );
    $self->_read_msfo_status if ( $self -> parsed_successfully and not $self -> finished_parsing );

    $self -> _scan_to_subproblems() if ( $self -> parsed_successfully() and not $self -> finished_parsing() );

    if ($self->nm_major_version() >= 7 and ($self -> estimation_step_initiated() or $self->covariance_step_run())) {
        #we have output to read
        $self->store_NM7_output(max_table_number => $self->table_number());
    }

    $self -> _read_subproblems() if ( $self -> parsed_successfully() and not $self -> finished_parsing() );

    my $mes = '';
    $mes = $self -> parsing_error_message() if (defined $self -> parsing_error_message());
    if( defined $self -> subproblems() ) {
      foreach my $subp ( @{$self -> subproblems()} ) {
        $mes .= $subp -> parsing_error_message() if (defined $subp -> parsing_error_message());
        $self->parsed_successfully($self -> parsed_successfully() * $subp -> parsed_successfully());
      }
    }

    $self->parsing_error_message($mes);

    $self->_check_tables_error();

    $self->lstfile([]);
}

sub add_subproblem
{
    my ($self, %parm) = validated_hash(\@_,
        init_data => { isa => 'Any', optional => 0 }
    );
    $self->subproblems([]) unless defined $self->subproblems;
    push( @{$self->subproblems}, output::problem::subproblem->new( %{$parm{'init_data'}} ) );
}

sub get_subproblem_count
{
    my $self = shift;
    my $count=0;
    if( defined $self->subproblems ) {
        $count = scalar(@{$self->subproblems});
    }
    return $count;
}

sub access_any
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         attribute => { isa => 'Str', optional => 0 },
         subproblems => { isa => 'ArrayRef[Int]', optional => 1 },
         parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my $attribute = $parm{'attribute'};
    my @return_value;
    my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
    my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

    unless( scalar(@subproblems) > 0 ) {
      if( defined $self->subproblems ) {
        @subproblems = (1 .. scalar @{$self->subproblems});
      } else {
        @subproblems = ();
      }
    }

    my @own_subproblems = defined $self->subproblems ? @{$self->subproblems} : ();

    foreach my $i ( @subproblems ) {
      if ( defined $own_subproblems[$i - 1] ) {
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

    return \@return_value;
}

sub full_name_NM7_file
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         file_type => { isa => 'Str', optional => 0 }
    );
    my $file_type = $parm{'file_type'};
    my $full_name;

    unless ($file_type =~ /^(raw|cov|coi|cor|phi)$/) {
      croak("illegal input $file_type to full_name_NM7_file");
    }
    if ($file_type eq 'raw') {
      $full_name = $self -> directory() . $self -> filename_root() . '.ext';
    } else {
      $full_name = $self -> directory() . $self -> filename_root() . '.' . $file_type;
    }

    return $full_name;
}

sub store_NM7_output
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         max_table_number => { isa => 'Maybe[Int]', optional => 1 }
    );
    my $max_table_number = $parm{'max_table_number'};

    # put these in five arrays of arrays
    # also store five arrays of table strings

    #make sure no old data left
    $self->table_numbers_hash({});
    $self->table_strings_hash({});

    $self->nm_output_files({'cov' => [], 'coi' => [], 'cor' => [] });

    foreach my $type ('cov','coi','cor') {
        my $filename = $self -> full_name_NM7_file('file_type' => $type);
        if (-e $filename) {
            my @tmp = utils::file::slurp_file($filename);
            unless (scalar (@tmp) > 0) {
                my $mes = "Empty file ".$filename;
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
                    if (($type eq 'raw') and (($string =~ /\(Evaluation\)/) or ($string =~ /\(EVALUATION\)/))){
                        $self->ext_file_has_evaluation(1);
                    }
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
                            push(@{$self->table_strings_hash->{$type}}, $prev_string);
                        }
                        $tab_index++;
                        push(@{$self->table_numbers_hash->{$type}}, $prev_num);
                    } elsif($line_count == 0 and ($prev_num == 0)) {
                        #NONMEM gives some tables number 0. skip them
                        next;
                    }
                    push (@{$self->nm_output_files->{$type}->[$tab_index]}, $line);
                    $line_count++;
                }
            }
        }
    }
}

sub parsing_error
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         message => { isa => 'Str', optional => 1 }
    );
    my $message = $parm{'message'};

    $self -> parsed_successfully( 0 );
    $self -> parsing_error_message( $message );
}

sub _read_nrecs
{
    my $self = shift;

    # The data recs statement should always be present
    # Raise parsing error if not found
    my $errmess = "Error in reading the number of data records!\n";
    my $start_pos = $self->lstfile_pos;
    my $success  = 0;

    while ( $_ = @{$self->lstfile}[$start_pos++] ) {
        if ( /$nobs_exp/ or ($start_pos + 1) == scalar @{$self->lstfile} ) {
            $self -> parsing_error( message => $errmess."$!" );
            return;
        }
        if ( /$nrec_exp1/ ) {
            $self -> nrecs($1);
            $success = 1;
            last;
        }elsif ( /$nrec_exp2/ ) {
            $self -> nrecs($1);
            $success = 1;
            last;
        }

    }

    if ( $success ) {
        $self->lstfile_pos($start_pos);
    } else {
        $self -> parsing_error( message => $errmess."$!" );
    }
}

sub _read_nobs
{
    my $self = shift;

    # The no of obs recs statement should always be present
    # Raise parsing error if not found
    my $errmess = "Error in reading the number of observation records!\n";
    my $start_pos = $self->lstfile_pos;
    my $success = 0;

    while ( $_ = @{$self->lstfile}[$start_pos++] ) {
        if ( /$nind_exp/ or ($start_pos + 1) == scalar @{$self->lstfile} ) {
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
        $self -> parsing_error( message => $errmess."$!" );
    }
}

sub _read_nind
{
    my $self = shift;

    # The no of individuals statement should always be present
    # Raise parsing error if not found
    my $errmess = "Error in reading the number of individuals!\n";
    my $start_pos = $self->lstfile_pos;
    my $success = 0;

    while ( $_ = @{$self->lstfile}[$start_pos++] ) {
      if ( /^0LENGTH OF THETA/ or
       /^0MODEL SPECIFICATION FILE INPUT/ or
       ($start_pos + 1) == scalar @{$self->lstfile} ) {
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
        $self -> parsing_error( message => $errmess."$!" );
    }
}

sub _read_eststep
{
    my $self = shift;

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
                $self -> parsing_error( message => $errmess."$!" );
            }
            return;
        }

        if ( ($start_pos + 1) == scalar @{$self->lstfile} ) {
            #EOF This should not happen, raise error
            my $errmess = "Reached end of file while searching for the simulation/estimation step indicators\n";
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

    if ($success) {
        $self->lstfile_pos($start_pos);
    }

}

sub _read_prior
{
    my $self = shift;

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
      $self -> parsing_error( message => $errmess."$!" );
      return;
    }

    if ( ($start_pos + 1) == scalar @{$self->lstfile} ) {
      #EOF This should not happen, raise error
      my $errmess = "Reached end of file while  searching for the (optional) user defined prior indicator\n";
      $self -> parsing_error( message => $errmess."$!" );
      return;
    }

    if(/^ PRIOR SUBROUTINE USER-SUPPLIED/){
      $self->user_defined_prior(1);
      $success = 1;
      last;
    }
  }

  if ($success) {
      $self->lstfile_pos = $start_pos;
  }
}

sub _read_steps_allowed
{
    my $self = shift;

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
        ( $self -> nonparametric_step_run() * $nonp_allowed ) ) {
          # If this happens, NONMEM aborts so we are finished reading
          $self -> finished_parsing(1);
    }

}

sub _read_nonpstep
{
    my $self = shift;

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

    if ($success) {
        $self->lstfile_pos($start_pos);
    }
}

sub _read_covstep
{
    my $self = shift;

    my $start_pos = $self->lstfile_pos;

    while ( $_ = @{$self->lstfile}[ $start_pos++ ] ) {

      if (/0COVARIANCE STEP OMITTED:\s*\b(.*)\b/) {
        $self->covariance_step_omitted(1) if $1 eq 'YES';
        $self->covariance_step_omitted(0) if $1 eq 'NO';
        last;
      }
      if ( /^ PROBLEM.*SUBPROBLEM/ or /^ PROBLEM NO\.:\s+\d/ or /^0ITERATION NO./ or /^0MINIMIZATION/) {
            # This is ok, we should end up here
        last;
      }
    }
}

sub _read_arbitrary
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         regexp => { isa => 'Str', optional => 0 },
         member => { isa => 'Str', optional => 0 }
    );
    my $regexp = $parm{'regexp'};
    my $member = $parm{'member'};

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
    if ($success) {
        $self->lstfile_pos($start_pos);
    }
}

sub _read_msfo_status
{
    my $self = shift;

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

sub _read_subproblems
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        subprob_arr => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my @subprob_arr = defined $parm{'subprob_arr'} ? @{$parm{'subprob_arr'}} : ();

    my $subproblem_start;

    my $last_method_number = $self->n_previous_meth(); #initiation, start at number of #METH from prev problem
    my $subprob_method_number = 0; #to be used when scanning lst-file
    my $last_method_string; #of last #METH found
    my $classical_method = 1;
    my $subproblem_number;
    my $sum_estimation_time = 0;
    my $sum_covariance_time = 0;
    my $no_meth = 0;
    my $need_evaluation_in_ext_file = 0;
    my $objt_exp = '^ #OBJT:';
    my $found_new_meth = 0;
    my $last_found_table_number;
    my $last_found_method_string;

    while ( $_ = @{$self->lstfile}[ $self -> {'lstfile_pos'}++ ] ) {
        if( /$method_exp/ ) {
            # NONMEM will sometimes print #METH also when running without estimation, do not count
            # these occurences, which have ^1 line directly following #METH
            # and either empty method string or a string matching (Evaluation) (?or (EVALUATION)?)
            # (MAXEVAL=0 will also have a ^1 line)
            my $string = $1;
            $string =~ s/\s*$//; #remove trailing spaces
            $found_new_meth = 1;
            $last_found_method_string = $string;

            if ($self->lstfile->[ $self->lstfile_pos - 2] =~ /^\s*\#TBLN:\s*([0-9]+)/) {
                #if previous line is #TBLN then this will help us find right table in extra output
                #and also right #METH
                $last_found_table_number = $1;
            }

        } elsif ( /$objt_exp/ ) {
            #this is not the reliable way of counting "methods" if something goes wrong
            $last_method_number++;
            $subprob_method_number++;
            if ($found_new_meth){
                $self->table_number($last_found_table_number) if (defined $last_found_table_number);
                $last_method_string = $last_found_method_string;
                if (($last_found_method_string =~ /\(Evaluation\)/) or ($last_found_method_string =~ /\(EVALUATION\)/)){
                    $need_evaluation_in_ext_file = 1; #have seen results sometimes printed to ext, sometimes not
                    #for NM 7.1 which does not have table numbers as TBLN tag
                }
            }
            #reset
            $found_new_meth = 0;
            $last_found_table_number =undef;
            $last_found_method_string = undef;
        } elsif ( /$est_time_exp/ ) {
            my $val = $1;
            $val =~ s/\s*$//; #remove trailing spaces
            $sum_estimation_time += $val;

        } elsif ( /$cov_time_exp/ ) {
            my $val = $1;
            $val =~ s/\s*$//; #remove trailing spaces
            $sum_covariance_time += $val;

        }
        #new if clause here to handle case if above line was also last in lst-file, make sure add subprob then
        if( /$subprob_exp/ or $self->lstfile_pos > $#{$self->lstfile} ) {
            if ($found_new_meth){
                if ($self->nm_version_710){
                    #this is either simply excess #METH or a failed run. Hard to tell.
                }else{
                    #we have found a #METH without the OBJT, and this is not NM7.1.0 which prints excess #METH
                    #this is probably a failed run of some sort, NONMEM crashed before printing OBJT
                    #but there could be a table to read in ext-file, so try to get correct table number
                    $last_method_number++;
                    $subprob_method_number++;
                    $self->table_number($last_found_table_number) if (defined $last_found_table_number);
                    $last_method_string = $last_found_method_string;
                }
                #reset
                $found_new_meth = 0;
                $last_found_table_number =undef;
                $last_found_method_string = undef;
            }

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
                        if ($subproblem_number eq '****'){
                            #In NM 7.3.0 amything above 9999 is output as ****
                            $star_subprob_number++;
                            $subproblem_number = $star_subprob_number;
                        }
                        $subproblem_index = $subproblem_number - 1; # Assuming problems come in order
                    }
                } else {
                    # whole file is one subprob
                    $subproblem_index = 0;
                    @subproblem_lstfile = @{$self->lstfile}[0 .. $self->lstfile_pos - 1];
                }
                #for NM7

                if ($self->nm_major_version >= 7) {
                    if ($last_method_number == $self->n_previous_meth()) {
                        $no_meth = 1;
                    }
                    if ((defined $last_method_string) and $last_method_string =~ /(Stochastic|Importance|Iterative|MCMC|NUTS)/) {
                        $classical_method = 0;
                    }
                }

                if (defined $self->table_number()){
                    $last_method_number = $self->table_number();
                }elsif ($need_evaluation_in_ext_file and
                        (not (defined $self->ext_file and $self->ext_file->has_evaluation))){
                    $self->evaluation_missing_from_ext_file(1);
                }

                my %subprob;
                my $ext_table;
                if ((not $self->evaluation_missing_from_ext_file) and
                    (defined $self->ext_file and scalar(@{$self->ext_file->tables})>0) and
                    ($no_meth < 1)) { #FIXME

                    $ext_table = $self->ext_file->get_table(number => $last_method_number);
                    if (defined $ext_table){
                        # retrieve table and check that strings match
                        if ((defined $last_method_string) and length($last_method_string)>0){
                            my $mstring = $ext_table->method;
                            unless (($last_method_string =~ $mstring ) or
                                    ($last_method_string eq $mstring) or
                                    ($mstring =~ $last_method_string  ) ) {
                                my $mess = "method strings\n".$mstring . " and\n"."$last_method_string do not match";
                                $self -> parsing_error( message => $mess );
                                $self -> finished_parsing(1);
                                return;
                            }
                        }
                        for my $type ('cov','cor','coi') {
                            if (defined $self->table_numbers_hash and defined $self->table_numbers_hash->{$type}) {
                                for (my $i = 0; $i < scalar(@{$self->table_numbers_hash->{$type}}); $i++ ) {
                                    if ($last_method_number == $self->table_numbers_hash->{$type}->[$i]) {
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
                $self->input_problem->set_estimated_parameters_hash() if (defined $self->input_problem);

                $self->add_subproblem('init_data' => {
                        lstfile                                   => \@subproblem_lstfile,
                        ignore_missing_files       => $self -> {'ignore_missing_files'},
                        input_problem              => $self->input_problem(),
#                        nm_output_files               => { 'raw' => $subprob{'raw'}, 'cov' => $subprob{'cov'}, 'cor' => $subprob{'cor'}, 'coi' => $subprob{'coi'}, 'phi' => $subprob{'phi'} },
                        nm_output_files               => {'cov' => $subprob{'cov'}, 'cor' => $subprob{'cor'}, 'coi' => $subprob{'coi'}},
                        ext_table                  => $ext_table,
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


                $self->last_method_number($last_method_number); #to be used by output.pm to set n_previous_meth
                @subproblem_lstfile = undef;
                undef %subprob;

                $subprob_method_number = 0;
                $sum_estimation_time = 0;
                $sum_covariance_time = 0;
                $no_meth = 0;
                if (not $self->iterations_interrupted and $self->subproblems->[-1]->iterations_interrupted){
                    $self->iterations_interrupted(1);
                }
            }
            unless ( $self->lstfile_pos > $#{$self->lstfile} ) {
                $subproblem_start = $self->lstfile_pos;
            }
        } # if subprob-exp or new prob or end of file
    } #while scanning lines
}

sub _scan_to_subproblems
{
    my $self = shift;

    $self->estimation_step_initiated(0);
    $self->estimation_step_run(0);
    my $meth_printed = 1;

    if (defined $self->input_problem->estimations) {
        $self -> estimation_step_initiated(1);
        if ($self->nm_major_version < 7) {
            #check if maxeval=0 in any $EST (can be split on multiple lines)
            my @options;
            foreach my $rec (@{$self->input_problem->estimations}) {
                push(@options, @{$rec->options});
            }
            my $found = 0;
            foreach my $option ( @options ) {
                if ( defined $option and (($option -> name eq 'MAXEVALS') or ( index( 'MAXEVALS', $option -> name ) == 0 ))) {
                    $found = 1 if ($option->value eq '0');
                    last;
                }
            }
            $self->estimation_step_run(1) unless ($found);
        } else {
            $self->estimation_step_run(1);
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

    #          /^0ITERATION NO./ or /^0MINIMIZATION/ or
    #          /^\s*SIMULATION STEP PERFORMED/ or
    #          /MINIMUM VALUE OF OBJECTIVE FUNCTION/ or
    #          /MONITORING OF SEARCH/ or
    #          /^\s*\#METH:/ or
    #          /^\s*\#TBLN:/ or


    my $found_endtime = 0;
    my $is_timestamp = 0;
    my $endtime;

    while( $_ = @{$self->lstfile}[ $start_pos++ ] ) {
        if (/^0MUST SET COVARIANCE MATRIX TO R MATRIX WHEN USING A PRIOR WITH FO, FOCE, OR LAPLACE/) {
            $self->parsing_error(message => "NONMEM terminated with message:\n" . $_);
            $self->pre_run_errors($_);
            $self->finished_parsing(1);
            return;
        }
        if ( /^ INITIAL ESTIMATE OF OMEGA HAS A NONZERO BLOCK WHICH IS NUMERICALLY NOT POSITIVE DEFINITE/ ) {
            $self -> parsing_error( message => "NONMEM terminated with message:\n".$_ );
            $self -> pre_run_errors($_);
            $self -> finished_parsing(1);
            return;
        } elsif ( /^\s*0INPUT MODEL SPECIFICATION FILE GENERATED FROM A NON-TERMINATING ESTIMATION STEP/ ) {
            my $mes = $_;
            if( @{$self->lstfile}[ $start_pos ] =~ / BUT CONTINUING ESTIMATION STEP NOT IMPLEMENTED/ ) {
                # If this happens, NONMEM aborts so we are finished reading
                $self -> parsing_error( message => "NONMEM terminated with message:\n".$mes );
                $self -> pre_run_errors($mes);
                $self -> finished_parsing(1);
                return;
            }
        } elsif ( /^\s*0MODEL SPECIFICATION FILE IS EMPTY/ ) {
            # If this happens, NONMEM aborts so we are finished reading
            $self -> parsing_error( message => "NONMEM terminated with message:\n".$_ );
            $self -> pre_run_errors($_);
            $self -> finished_parsing(1);
            return;
        } elsif (( /^\s*0OBJ. FUNCT. IS NOT DEFINED/ ) and ($self -> estimation_step_initiated())) {
            #this message does not necessarily mean failure!! only if we expect an estimation
            $self -> parsing_error( message => "NONMEM terminated with message:\n".$_ );
            $self -> pre_run_errors($_);
            $self -> finished_parsing(1);
            return;
        } elsif ($self -> nonparametric_step_run and (/^\s*SKIPPING ESTIMATION, USING ETAS THAT ARE FROM MSF FILE/ )) {
            $self-> estimation_step_initiated(0);
            $self -> estimation_step_run(0);
            last;
        } elsif (/^\s*0ESTIMATION STEP IMPLEMENTED/ ) {
            if( @{$self->lstfile}[ $start_pos ] =~ / BUT THE NUMBER OF PARAMETERS TO BE ESTIMATED IS 0/ ) {
                # If this happens, NONMEM aborts so we are finished reading
                my $mes = "NONMEM terminated with message:\nESTIMATION STEP IMPLEMENTED\n".
                    "BUT THE NUMBER OF PARAMETERS TO BE ESTIMATED IS 0\n";
                $self -> parsing_error( message =>  $mes);
                $self -> pre_run_errors($mes);
                $self -> finished_parsing(1);
                return;
            }
        } elsif ( /^\s*0RUN TERMINATED/ ) {
            $self -> parsing_error( message => "NONMEM terminated with message:\n".$_ );
            $self -> finished_parsing(1);
            return;
        } elsif ($self->simulation_step_run() and /$subprob_exp/) {
            last;
        } elsif ( /$endstring/ ){
            last;
        } elsif (/^\s*Finished\s*(Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s*/){
            #end time stamp, means nmfe finished fine
            $found_endtime = 1;
        } elsif (/^\s*Stop Time:\s*$/){
            #end time stamp, means nmfe finished fine
            $found_endtime = 1;
        } elsif (not $found_endtime){
            ($is_timestamp,$endtime)=is_timestamp($self->lstfile,($start_pos-1));
            #end time stamp, means nmfe finished fine
            $found_endtime = 1 if ($is_timestamp);
        }

        if ( $start_pos > $#{$self->lstfile} ) { #we found end of file
            if ($found_endtime) {
                #run did not crash since nmfe printed end time, but nothing to parse
                unless ($self->problem_index > 0){
                    $self -> parsing_error( message => "nmfe finished before printing any run output.\n" );
                }
                $self -> finished_parsing(1);
                return;
            } else {
                #EOF This should not happen, raise error
                my $errmess = "Reached end of file while scanning for subproblems\n";
                $self -> parsing_error( message => $errmess );
                return;
            }
        }
    }
    $self->lstfile_pos($start_pos - 1);

}

sub is_timestamp
{
    my $arrayref = shift;
    my $index = shift;
    my $line1 = $arrayref->[$index];
    my $is_time = 0;
    my $seconds = undef;
    my ($wday, $mon, $mday, $tt, $zone, $year, $hour, $min, $sec, $ampm);
    if ($line1 =~ /^\s*(Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)/) {
        $is_time = 1;
        my %months = ('Jan'=> 0,'Feb' => 1, 'Mar' => 2, 'Apr' => 3, 'May' => 4, 'Jun' => 5,
                      'Jul' => 6, 'Aug' => 7, 'Sep' => 8, 'Oct' => 9, 'Nov' => 10, 'Dec' => 11);

        $line1 =~ s/\s*$//; #remove trailing spaces
        ($wday, $mon, $mday, $tt, $ampm, $zone, $year) = split(/\s+/, $line1);
        $mon = $months{$mon}; #convert to numeric
        ($hour, $min, $sec) = split(':',$tt);
        if ($ampm eq "AM" or $ampm eq "PM") {
            if ($hour == 12 and $ampm eq "AM") {
                $hour = 0;
            } elsif ($hour < 12 and $ampm eq "PM") {
                $hour = $hour + 12;
            }
        } else {
            $year = $zone;  
            $zone = $ampm;
        }
    } elsif ($line1 =~ /^(\d\d)\/(\d\d)\/(\d\d\d\d)\s*$/) {
        # Alternative date format: dd/mm/yyyy\nhh:mm
        $year = $3;
        $mon = $2 - 1;
        $mday = $1;
        if ((scalar(@{$arrayref}) > ($index+1)) and
            ($arrayref->[$index+1] =~ /^(\d\d):(\d\d)\s*$/)) {
            $hour = $1;
            $min = $2;
            $is_time = 1;
        }
    }elsif ($line1 =~ /^(\d\d\d\d)-(\d\d)-(\d\d)\s*$/) {
        # Alternative date format: yyyy-mm-dd\nhh:mm
        $year = $1;
        $mon = $2 - 1;
        $mday = $3;
        if ((scalar(@{$arrayref}) > ($index+1)) and
            ($arrayref->[$index+1] =~ /^(\d\d):(\d\d)\s*$/)) {
            $hour = $1;
            $min = $2;
            $is_time = 1;
        }
    }
    if ($is_time){
        $seconds = timelocal($sec, $min, $hour, $mday, $mon, $year);
    }
    return($is_time,$seconds);
}

sub _read_block_structures
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         prob_arr => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my @prob_arr = defined $parm{'prob_arr'} ? @{$parm{'prob_arr'}} : ();

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
      $self -> parsing_error( message => $errmess." 2 $!" );
    } else {
      $self->lstfile_pos($start_pos);
    }
}

sub _check_tables_error
{
    my $self = shift;

    return if not defined $self->lstfile;

    my $start_message;
    my $end_message;

    for (my $i = 0; $i < scalar(@{$self->lstfile}); $i++) {
        if ($self->lstfile->[$i] =~ /^0PROGRAM TERMINATED BY FNLETA/) {
           $start_message = $i;
        }
        if ($self->lstfile->[$i] =~ /^ MESSAGE ISSUED FROM TABLE STEP/) {
            $end_message = $i;
            last;
        }
    }

    my @error;
    if (defined $start_message and defined $end_message) {
        @error = @{$self->lstfile}[$start_message .. $end_message];
        $self->tables_step_error(join("", @error));
    }
}

1;
