package output;

# A Perl module for parsing NONMEM output files
use include_modules;
use OSspecific;
use Storable;
use Config;
use Math::SigFigs;
use model;
use array qw(:all);
use Mouse;
use MouseX::Params::Validate;
use output::problem;
use utils::file;
use nmtablefile;

has 'problems' => ( is => 'rw', isa => 'ArrayRef[output::problem]' );
has 'lst_model' => ( is => 'rw', isa => 'model' );
has 'directory' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'control_stream_problems' => ( is => 'rw', isa => 'ArrayRef' );
has 'filename_root' => ( is => 'rw', isa => 'Str' );
has 'filename' => ( is => 'rw', isa => 'Str' );
has 'nonmem_version' => ( is => 'rw', isa => 'Num' );
has 'ignore_missing_files' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'tablenames' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'parse_output' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'parsed_successfully' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'msfo_has_terminated' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'runtime' => ( is => 'rw', isa => 'Str' );
has 'iterations_interrupted' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'parsed' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'append_nm_OUTPUT' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'could_append_OUTPUT' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'parsing_error_message' => ( is => 'rw', isa => 'Str' );
has 'iterations_interrupted' => ( is => 'rw', isa => 'Bool', default => 0 );

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
        } else {
            $self->filename_root($name);
        }

        if ( -e $self->full_name ) {
            if ($self->parse_output) {
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

sub load
{
    #this method returns an empty string if all goes well. Empty string evaluates to
    #false while non-empty strings are true, so return value from load can always be
    #checked with $error = $output->load; if ($error){"something went wrong $error"}else{'all ok'}
    my $self = shift;
    my $error = '';

    if ($self->have_output){
        unless ( not_empty($self->problems) ) {
            $self -> _read_problems;
        }
        unless( $self -> parsed_successfully ){
            $error = "Trying to load output but unable to read everything from outputfile, parser error message:\n".
                $self -> parsing_error_message();
        }
    }else{
        $error = "Trying to load output but output object is empty, file\n".$self->full_name."\n";
    }
    return $error;
}

sub nonmem_run_failed
{
    my $self = shift;
    my $failed = 0;
    my $reason = '';

    while (1){
        if (not $self->parsed_successfully){
            $failed = 1;
            $reason = 'lst-file not parsed successfully'; #FIXME nmtran error etc
            last;
        }
#        if ($self->iterations_interrupted){
#            $failed = 1;
#            last;
#        }
        my $prob_number = $self->get_estimation_evaluation_problem_number;
        if ($self->get_estimation_evaluation_problem_number >0){
            #check that ofv defined and numeric. otherwise we have failure
            my $ofv = $self-> get_single_value(attribute => 'ofv',
                                               problem_index=> ($prob_number-1));
            unless (defined $ofv){
                $failed = 1;
                $reason = 'The estimation ofv is undefined';
            }
        }else{
            #check if have maxeval0 and undef ofv
            if ((not defined $self->problems->[(-$prob_number-1)]->subproblems) or
                (scalar(@{$self->problems->[(-$prob_number-1)]->subproblems})==0)){
                $failed = 1;
                $reason = 'No subproblems';
            }elsif ($self->problems->[(-$prob_number-1)]->subproblems->[0]->estimation_step_initiated()){
                my $ofv = $self-> get_single_value(attribute => 'ofv',
                                                   problem_index=> (-$prob_number-1));
                unless (defined $ofv){
                    $failed = 1;
                    $reason = 'The evaluation ofv is undefined';
                }
            }elsif(defined $self->problems->[(-$prob_number-1)]->subproblems->[0]->simulation_error_message){
                if (length($self->problems->[(-$prob_number-1)]->subproblems->[0]->simulation_error_message)>0){
                    $failed =1;
                    $reason = 'Simulation error message: '.
                        $self->problems->[(-$prob_number-1)]->subproblems->[0]->simulation_error_message;
                }
            }
        }
        last;
    }

    return ($failed,$reason);
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

sub get_estimation_evaluation_problem_number
{
    #get the problem number to check status of in modelfit::restart_needed
    #always the first $PROB, if any, that has final $EST that is not MAXEVAL=0
    #standard case is 1, if that prob is estimation
    # with two $PROB and $PRIOR TNPRI it is 2, if it is estimation
    # if do not find $PROB with est run then return negative of est probnum
    my $self = shift;
    my $evaluation_probnum= -1;
    my $estimation_step_run = 0;

    if ( $self -> have_output ) {
        $self -> load;

        if( defined $self->problems ) {
            for (my $i=0; $i<scalar(@{$self->problems}); $i++){
                if ((defined $self->problems->[$i]) and (defined $self->problems->[$i]->subproblems) and
                    (defined $self->problems->[$i]->subproblems->[0])
                     and $self->problems->[$i]->subproblems->[0]->estimation_step_run() ){
                    $evaluation_probnum = ($i+1); #number not index
                    $estimation_step_run = 1;
                    last;
                }
            }
            my $found_evaluation=0;
            unless ($estimation_step_run){
                #no hit in above loop
                for (my $i=0; $i<scalar(@{$self->problems}); $i++){
                    if ((defined $self->problems->[$i]) and (defined $self->problems->[$i]->subproblems)
                        and (defined $self->problems->[$i]->subproblems->[0])
                        and $self->problems->[$i]->subproblems->[0]->estimation_step_initiated() ){
                        $evaluation_probnum = -($i+1); #number not index, negative since not run
                        $found_evaluation = 1;
                        last;
                    }
                }
                unless ($found_evaluation){
                    #make sure we return a prob that has subproblems, ie not $PRIOR TNPRI
                    for (my $i=0; $i<scalar(@{$self->problems}); $i++){
                        if ((defined $self->problems->[$i]) and (defined $self->problems->[$i]->subproblems)
                            and (defined $self->problems->[$i]->subproblems->[0])){
                            $evaluation_probnum = -($i+1); #number not index, negative since not run
                            last;
                        }
                    }
                }
            }
        }
    }

    return $evaluation_probnum;
}

sub get_problem_count
{
    my $self = shift;

    my $answer = undef;
    my $error = $self->load;
    unless ($error){
        if( defined $self->problems ) {
            $answer = scalar(@{$self->problems});
        } else {
            $answer= 0;
        }
    }
    return $answer;
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


    my $error = $self->load;
    if ( $error  ) {
        print "\n $error \n" ;
        return [];
    }

    my @own_problems;
    if( defined $self->problems ) {
        unless( scalar(@problems) > 0 ){
            @problems = (1 .. scalar @{$self->problems});
        }
        @own_problems = @{$self->problems};
    } else {
        return \@return_value; #Return the empty array
    }

    foreach my $i ( @problems ) {
        if ( defined $own_problems[$i - 1] ) {
            if (( defined( $own_problems[$i - 1] -> can( $attribute ) ) ) and (not $attribute eq 'estimation_step_run')) {
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
        );
    my $limit = $parm{'limit'};
    my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
    my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();

    my @high_names_correlations;
    my @high_values_correlations;

    my $problem_count = $self->get_problem_count(); #will also read output if not already done
    return [] unless (defined $problem_count);

    if (scalar(@problems) == 0){
        @problems = (1 .. $problem_count) if ($problem_count > 0);
    }

    my $found_any=0;

    foreach my $probnum (@problems){
        my @prob_high_names_array=();
        my @prob_high_values_array=();
        if (defined $self->problems->[$probnum-1]){
            if (scalar(@subproblems)==0 ){
                my $subproblem_count = $self->problems->[$probnum-1]->get_subproblem_count();
                @subproblems = (1 .. $subproblem_count) if ($subproblem_count > 0);
            }
            foreach my $subprobnum (@subproblems){
                my @sub_high_names_array=();
                my @sub_high_values_array=();
                if (defined $self->problems->[$probnum-1]->subproblems->[$subprobnum-1]){
                    my $correlation_matrix = $self->problems->[$probnum-1]->subproblems->[$subprobnum-1]->correlation_matrix;
                    if (defined $correlation_matrix and scalar(@{$correlation_matrix})>0){
                        $found_any=1;
                        my @names = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                                             attribute => 'labels',
                                                                             problem_index => ($probnum-1))};
                        my $idx = 0;
                        for ( my $row = 1; $row <= scalar(@names); $row++ ) {
                            for ( my $col = 1; $col <= $row; $col++ ) {
                                if ( ( $row != $col ) and abs($correlation_matrix->[$idx]) > $limit) {
                                    push( @sub_high_names_array, $names[$row-1]." - ".$names[$col-1] );
                                    push( @sub_high_values_array, $correlation_matrix -> [$idx] );
                                }
                                $idx++;
                            }
                        }
                    }
                }
                push(@prob_high_names_array,\@sub_high_names_array);
                push(@prob_high_values_array,\@sub_high_values_array);
            }
        }
        push(@high_names_correlations,\@prob_high_names_array);
        push(@high_values_correlations,\@prob_high_values_array);
    }

    if ($found_any){
        return \@high_names_correlations ,\@high_values_correlations;
    }else{
        return [];
    }
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
        );
    my $theta_cv_limit = $parm{'theta_cv_limit'};
    my $omega_cv_limit = $parm{'omega_cv_limit'};
    my $sigma_cv_limit = $parm{'sigma_cv_limit'};
    my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
    my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();

    my @large_standard_errors_names;
    my @large_standard_errors_values;
    my @params = ( 'theta', 'omega', 'sigma' );

    my $problem_count = $self->get_problem_count(); #will also read output if not already done
    return [],[] unless (defined $problem_count);

    if (scalar(@problems) == 0){
        @problems = (1 .. $problem_count) if ($problem_count > 0);
    }
    my $found_any=0;

    foreach my $probnum (@problems){
        my @prob_high_names_array=();
        my @prob_high_values_array=();
        if (defined $self->problems->[$probnum-1]){
            if (scalar(@subproblems)==0 ){
                my $subproblem_count = $self->problems->[$probnum-1]->get_subproblem_count();
                @subproblems = (1 .. $subproblem_count) if ($subproblem_count > 0);
            }
            foreach my $subprobnum (@subproblems){
                my @sub_high_names_array=();
                my @sub_high_values_array=();
                if (defined $self->problems->[$probnum-1]->subproblems->[$subprobnum-1]){
                    foreach my $param (@params) {
                        my $ref = $self->get_filtered_values(category => 'cvse',
                                                             parameter => $param,
                                                             problem_index => ($probnum-1),
                                                             subproblem_index => ($subprobnum-1));
                        if (defined $ref and scalar(@{$ref}>0)){
                            $found_any=1;
                            my @labels = @{$self->output_get_estimated_attributes(parameter => $param,
                                                                                  attribute => 'labels',
                                                                                  problem_index => ($probnum-1))};

                            for (my $k=0; $k<scalar(@labels); $k++){
                                if ( defined ($ref->[$k]) and abs($ref->[$k]) > eval('$'.$param.'_cv_limit')) {
                                    push (@sub_high_names_array,$labels[$k]);
                                    push (@sub_high_values_array,$ref->[$k]);
                                }
                            }
                        }
                    }
                }
                push(@prob_high_names_array,\@sub_high_names_array);
                push(@prob_high_values_array,\@sub_high_values_array);
            }
        }
        push(@large_standard_errors_names,\@prob_high_names_array);
        push(@large_standard_errors_values,\@prob_high_values_array);
    }

    if ($found_any){
        return \@large_standard_errors_names ,\@large_standard_errors_values;
    }else{
        return [],[];
    }
}

sub perfect_individual_count
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              problem_index => { isa => 'Int', default => 0 },
                              subproblem_index => { isa => 'Int', default => 0 },
        );
    my $problem_index = $parm{'problem_index'};
    my $subproblem_index = $parm{'subproblem_index'};

    my %individual_count = ();

    my $problem_count = $self->get_problem_count(); #will also read output if not already done
    return {} unless (defined $problem_count and ($problem_count > $problem_index)
                      and (defined $self->problems->[$problem_index]) and
                      (defined $self->problems->[$problem_index]->subproblems ) and
                      (defined $self->problems->[$problem_index]->subproblems->[$subproblem_index]));

    my $seref = $self->get_filtered_values(category => 'se',
                                           parameter => 'omega',
                                           problem_index => $problem_index,
                                           subproblem_index => $subproblem_index);
    my $estref = $self->get_filtered_values(category => 'estimate',
                                            parameter => 'omega',
                                            problem_index => $problem_index,
                                            subproblem_index => $subproblem_index);

    my @off_diagonal = @{$self->output_get_estimated_attributes(parameter => 'omega',
                                                                attribute => 'off_diagonal',
                                                                problem_index => $problem_index)};

    my @strings = @{$self->output_get_estimated_attributes(parameter => 'omega',
                                                           attribute => 'coordinate_strings',
                                                           problem_index => $problem_index)};

    if (defined $seref and scalar(@{$seref}>0)){
        for (my $k=0; $k < scalar(@{$seref}); $k++){
            if ($off_diagonal[$k] == 0 ){
                if ((defined $seref->[$k]) and ($seref->[$k] > 0)){
                    #we have se
                    if ($strings[$k] =~ /^OMEGA\((\d+),\1\)/){
                        my $etanum = $1;
                        $individual_count{$etanum}=(2*((($estref->[$k])/($seref->[$k]))**2)+1);
                    }else{
                        croak("error regexp ".$strings[$k]);
                    }
                }
            }
        }
    }
    return \%individual_count;
}

sub near_bounds
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              zero_limit => { isa => 'Num', default => 0.01, optional => 1 },
                              significant_digits => { isa => 'Int', default => 2, optional => 1 },
                              off_diagonal_sign_digits => { isa => 'Int', default => 2, optional => 1 },
        );
    my $zero_limit = $parm{'zero_limit'};
    my $significant_digits = $parm{'significant_digits'};
    my $off_diagonal_sign_digits = $parm{'off_diagonal_sign_digits'};
    my @found_names;
    my @found_bounds;
    my @found_values;

    sub test_sigdig {
        my ( $number, $goal, $sigdig, $zerolim ) = @_;
        $number = &FormatSigFigs($number, $sigdig );
        my $test=0;
        no warnings qw(numeric); #sometimes number is '' FIXME
        if ( $goal == 0 ) {
            $test = abs($number) < $zerolim ? 1 : 0;
        } else {
            $goal = &FormatSigFigs($goal, $sigdig );
            $test = $number eq $goal ? 1 : 0;
        }
        return $test;
    }


    my $problem_count = $self->get_problem_count(); #will also read output if not already done
    return [],[],[] unless (defined $problem_count and $problem_count >0);

    my @problems = (1 .. $problem_count);
    my @subproblems=();
    my $found_any=0;
    foreach my $probnum (@problems){
        my @prob_bounds_array=();
        my @prob_names_array=();
        my @prob_values_array=();
        if (defined $self->problems->[$probnum-1]){
            my $subproblem_count = $self->problems->[$probnum-1]->get_subproblem_count();
            @subproblems = (1 .. $subproblem_count) if ($subproblem_count > 0);

            my @lower = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                                 attribute => 'lower_bounds',
                                                                 problem_index => ($probnum-1))};
            my @upper = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                                 attribute => 'upper_bounds',
                                                                 problem_index => ($probnum-1))};
            my @labels = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                                  attribute => 'labels',
                                                                  problem_index => ($probnum-1))};
            my @off_diagonal = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                                        attribute => 'off_diagonal',
                                                                        problem_index => ($probnum-1))};
            #The boundary test for off-diagonal omega elements
            #are performed by first converting the covariances to the corre-
            #sponding correlations and then check if they are close to +/-1

            foreach my $subprobnum (@subproblems){
                my @sub_bounds_array=();
                my @sub_names_array=();
                my @sub_values_array=();
                if (defined $self->problems->[$probnum-1]->subproblems->[$subprobnum-1]
                    and (scalar(@lower)>0) ){
                    #for thetas and diagonals we want the estimate,
                    # for of-diagonals we want c
                    #make sure arrays are equal length
                    my $estref = $self->get_filtered_values(category => 'estimate',
                                                            parameter => 'all',
                                                            problem_index => ($probnum-1),
                                                            subproblem_index => ($subprobnum-1));
                    my $cref = $self->get_filtered_values(category => 'c',
                                                          parameter => 'all',
                                                          problem_index => ($probnum-1),
                                                          subproblem_index => ($subprobnum-1));

                    if (defined $estref and scalar(@{$estref}>0)){
                        $found_any=1;
                        for (my $k=0; $k<scalar(@labels); $k++){
                            my $value = $estref->[$k];
                            my $sigdig=$significant_digits;
                            my $low = $lower[$k];
                            my $high=$upper[$k];
                            if ($off_diagonal[$k]){
                                $value=$cref->[$k];
                                $sigdig = $off_diagonal_sign_digits;
                                $low=-1;
                                $high=1;
                            }

                            if ( defined ($value)){
                                foreach my $limit ($low,$high){
                                    if ( test_sigdig( $value, $limit, $sigdig, $zero_limit ) ) {
#                                        print $labels[$k]." $value $limit\n ";
                                        push( @sub_bounds_array, $limit );
                                        push( @sub_names_array, $labels[$k] );
                                        push( @sub_values_array, $value );
                                    }
                                }
                            }
                        }
                    }
                }
                push(@prob_bounds_array,\@sub_bounds_array);
                push(@prob_names_array,\@sub_names_array);
                push(@prob_values_array,\@sub_values_array);
            }
        }
        push(@found_bounds,\@prob_bounds_array);
        push(@found_names,\@prob_names_array);
        push(@found_values,\@prob_values_array);
    }
    if ($found_any){
        return \@found_bounds ,\@found_names ,\@found_values;
    }else{
        return [],[],[];
    }
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

sub estnames
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              problems => { isa => 'Maybe[ArrayRef]', optional => 1 },
                              subproblems => { isa => 'Maybe[ArrayRef]', optional => 1 },
                              parameter => { isa => 'Str', optional => 0 },
        );
    my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
    my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
    my $parameter = $parm{'parameter'};

    #FIXME default all problems all subproblems

    my @estnames =();

    my $error = $self->load;

    if ( $error ) {
        print "\n $error \n";
        return [];
    }
    unless (defined $self->problems and scalar(@{$self->problems})>0 ) {
        return [];
    }

    my $max_prob = scalar(@{$self->problems});

    foreach my $probnum (@problems){
        if ($probnum > $max_prob){
            croak("probnum $probnum too high in output->estnames, problem count is $max_prob");
        }

        my @coordinate_strings = @{$self->output_get_estimated_attributes(parameter => $parameter,
                                                                           attribute => 'coordinate_strings',
                                                                           problem_index => ($probnum-1))};
        my @arr =();
        foreach my $subprob (@subproblems){
            push(@arr,\@coordinate_strings);
        }
        push(@estnames,\@arr);
    }
    return \@estnames;
}

sub est_thetanames
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              problems => { isa => 'ArrayRef', optional => 1 },
                              subproblems => { isa => 'ArrayRef', optional => 1 },
        );
    my $problems = $parm{'problems'};
    my $subproblems = $parm{'subproblems'};

    return $self->estnames(problems => $problems,
                           subproblems => $subproblems,
                           parameter => 'theta');
}

sub est_omeganames
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              problems => { isa => 'ArrayRef', optional => 1 },
                              subproblems => { isa => 'ArrayRef', optional => 1 },
        );
    my $problems = $parm{'problems'};
    my $subproblems = $parm{'subproblems'};

    return $self->estnames(problems => $problems,
                           subproblems => $subproblems,
                           parameter => 'omega');
}

sub est_sigmanames
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              problems => { isa => 'ArrayRef', optional => 1 },
                              subproblems => { isa => 'ArrayRef', optional => 1 },
        );
    my $problems = $parm{'problems'};
    my $subproblems = $parm{'subproblems'};

    return $self->estnames(problems => $problems,
                           subproblems => $subproblems,
                           parameter => 'sigma');
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

sub npcorr
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              problems => { isa => 'ArrayRef[Int]', optional => 1 },
                              subproblems => { isa => 'ArrayRef[Int]', optional => 1 }
        );
    my @problems = defined $parm{'problems'} ? @{$parm{'problems'}} : ();
    my @subproblems = defined $parm{'subproblems'} ? @{$parm{'subproblems'}} : ();
    my @npomegas = @{$self->access_any(attribute=>'npcorr',problems=>\@problems,subproblems=>\@subproblems)};


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

sub get_eta_eps_correlations
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              problem_index => { isa => 'Int', optional => 1, default => 0 },
                              subproblem_index => { isa => 'Int', optional => 1, default => 0 },
                              num_to_sd_rescale => { isa => 'HashRef', optional => 0},
                              type  => { isa => 'Str', optional => 1, default => 'eta' },
        );
    my $problem_index=$parm{'problem_index'};
    my $subproblem_index=$parm{'subproblem_index'};
    my $num_to_sd_rescale=$parm{'num_to_sd_rescale'};
    my $type=$parm{'type'};

    my $error='';

    my $parameter;
    if ($type eq 'eta'){
        $parameter = 'omega';
    }elsif($type eq 'eps'){
        $parameter = 'sigma';
    }else{
        $error = "illegal type $type";
        return (undef,undef,undef,$error);
    }

    if (not $self->have_output){
        $error = "Trying get_eta_eps_correlations but output object is empty, output file\n".$self->full_name."\n";
    }elsif (not $self -> parsed_successfully ){
        $error = "Trying get_eta_eps_correlations but unable to read everything from outputfile, parser error message:\n".
            $self -> parsing_error_message();
    }elsif (not $self-> get_single_value(attribute => 'covariance_step_run')){
        $error = "Trying get_eta_eps_correlations but the covariance step was not run";
    }
    return (undef,undef,undef,$error) if (length($error)>0);


    #first compute correlations only.
    #find all estimated diagonal omega or sigma

    my @coordinate_strings = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                                      attribute => 'coordinate_strings',
                                                                      problem_index=> $problem_index)};
    my @coords = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                          attribute => 'coords',
                                                          problem_index=> $problem_index)};
    my @off_diagonal = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                                attribute => 'off_diagonal',
                                                                problem_index=> $problem_index)};
    my @labels = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                          attribute => 'labels',
                                                          problem_index=> $problem_index)};
    my @param = @{$self->output_get_estimated_attributes(parameter => 'all',
                                                         attribute => 'param',
                                                         problem_index=> $problem_index)};

    my @estimates = @{$self->get_filtered_values (problem_index => $problem_index,
                                                  subproblem_index => $subproblem_index,
                                                  parameter => 'all',
                                                  category => 'estimate')};
    my @diagonal_i = ();
    my @diagonal_coords = ();
    my @out_labels = ();
    my %diagonal_estimates;
    my %diagonal_position;
    for (my $i=0; $i< scalar(@coordinate_strings); $i++){
        if (($param[$i] eq $parameter) and ($off_diagonal[$i] == 0)){
            if ($coords[$i] =~ /^(\d+),/){
                my $num = $1;
                push(@diagonal_coords,$num);
                $diagonal_estimates{$num} = $estimates[$i];
                $diagonal_position{$num} = scalar(@diagonal_i);
                if ($coordinate_strings[$i] eq $labels[$i]){
                    push(@out_labels,uc($type).$num);
                }else{
                    push(@out_labels,$labels[$i]);
                }
            }else{
                croak("regexp ".$coords[$i]);
            }
            push(@diagonal_i,$i);
        }else{
            push(@diagonal_coords,0); #placeholder
        }
    }

    my $size =scalar(@diagonal_i);

    my ($correlations,$coefficients,$covariances) = correlations_coefficients_covariances(
        size => $size,
        coordinate_strings => \@coordinate_strings,
        param_vector => \@param,
        parameter => $parameter,
        rescale_sd => $num_to_sd_rescale,
        coords => \@coords,
        off_diagonal => \@off_diagonal,
        diagonal_estimates => \%diagonal_estimates,
        diagonal_position => \%diagonal_position,
        estimates => \@estimates);

    return ($correlations,\@out_labels,undef,$error); #FIXME, remove

    my $lower_covar;
    my $covar;
    if (not $self-> get_single_value(attribute => 'covariance_step_successful')){
        $error = "Trying get_eta_eps_correlations but the covariance step was not successful";
    }elsif ($self-> get_single_value(attribute => 'covariance_step_warnings')){
        $error = "Doing get_eta_eps_correlations but there were covariance step warnings in the lst-file";
    }else{
        $lower_covar  = $self -> get_single_value(attribute => 'covariance_matrix');
        if (not defined $lower_covar){
            $error = "Trying get_eta_eps_correlations but the covariance matrix is undefined. Parsing error? Output file is\n".
                $self->full_name."\n";
        }else{
            $covar = output::problem::subproblem::make_square( $lower_covar);
        }
    }

    return ($correlations,\@out_labels,undef,$error) if (length($error)>0);
    #TODO uncert enligt frem_userguide
    #tests in get_estimated_attributes.t
}

sub correlations_coefficients_covariances
{
    my %parm = validated_hash(\@_,
                              size => { isa => 'Int', optional => 0 },
                              coordinate_strings => { isa => 'ArrayRef', optional => 0 },
                              param_vector => { isa => 'ArrayRef', optional => 0 },
                              coords => { isa => 'ArrayRef', optional => 0 },
                              off_diagonal => { isa => 'ArrayRef', optional => 0 },
                              estimates => { isa => 'ArrayRef', optional => 0 },
                              rescale_sd => { isa => 'HashRef', optional => 0 },
                              diagonal_estimates => { isa => 'HashRef', optional => 0 },
                              diagonal_position => { isa => 'HashRef', optional => 0 },
                              parameter => { isa => 'Str', optional => 0 },
        );
    my $size=$parm{'size'};
    my $coordinate_strings=$parm{'coordinate_strings'};
    my $param_vector=$parm{'param_vector'};
    my $coords=$parm{'coords'};
    my $off_diagonal=$parm{'off_diagonal'};
    my $estimates=$parm{'estimates'};
    my $rescale_sd=$parm{'rescale_sd'};
    my $diagonal_estimates=$parm{'diagonal_estimates'};
    my $diagonal_position=$parm{'diagonal_position'};
    my $parameter=$parm{'parameter'};


    my @correlations = ();
    my @coefficients = ();
    my @covariances = ();
    for (my $j=0; $j < $size; $j++){
        push(@correlations,[]);
        for (my $k=0; $k < $size; $k++){
            push (@{$correlations[$j]},undef);
            push (@{$coefficients[$j]},undef);
            push (@{$covariances[$j]},undef);
        }
        $correlations[$j]->[$j]=1;
        $coefficients[$j]->[$j]=1;
    }
    foreach my $key (keys %{$diagonal_estimates}){
        my $pos = $diagonal_position->{$key};
        if (defined $rescale_sd->{$key}){
            $covariances[$pos]->[$pos] = $diagonal_estimates->{$key}*($rescale_sd->{$key})**2;
        }else{
            $covariances[$pos]->[$pos] = $diagonal_estimates->{$key};
        }
    }

    for (my $i=0; $i< scalar(@{$coordinate_strings}); $i++){
        if (($param_vector->[$i] eq $parameter) and ($off_diagonal->[$i] == 1)){
            if ($coords->[$i] =~ /^(\d+),(\d+)$/){
                my $left = $1;
                my $right = $2;
                unless ((defined $diagonal_estimates->{$left}) and (defined $diagonal_estimates->{$right}) and
                        ($diagonal_estimates->{$left} > 0) and ($diagonal_estimates->{$right} > 0)){
                    croak("error diagonal estimates left $left ".$diagonal_estimates->{$left}.
                          " right $right ".$diagonal_estimates->{$right});
                }
                my $a = $diagonal_position->{$left};
                my $b = $diagonal_position->{$right};

                my $corr = $estimates->[$i]/sqrt($diagonal_estimates->{$left}*$diagonal_estimates->{$right});
                $correlations[$a]->[$b] = $corr;
                $correlations[$b]->[$a] = $corr;

                if ((defined $rescale_sd->{$left}) and (defined $rescale_sd->{$right})){
                    #both cov. no coeff
                    $covariances[$a]->[$b] = $estimates->[$i]*$rescale_sd->{$left}*$rescale_sd->{$right};
                    $covariances[$b]->[$a] = $estimates->[$i]*$rescale_sd->{$left}*$rescale_sd->{$right};
                } elsif ((not defined $rescale_sd->{$left}) and (not defined $rescale_sd->{$right})){
                    #both parameter. no coeff
                    $covariances[$a]->[$b] = $estimates->[$i];
                    $covariances[$b]->[$a] = $estimates->[$i];
                }else{
                    #one cov, one par. do coeff
                    my $rescale;
                    my $coeff;
                    if (defined $rescale_sd->{$left}){
                        $rescale = $rescale_sd->{$left};
                        $coeff = $estimates->[$i]/($rescale*$diagonal_estimates->{$left});
                    }else{
                        $rescale = $rescale_sd->{$right};
                        $coeff = $estimates->[$i]/($rescale*$diagonal_estimates->{$right});
                    }
                    $covariances[$a]->[$b] = $rescale*$estimates->[$i];
                    $covariances[$b]->[$a] = $rescale*$estimates->[$i];
                    $coefficients[$a]->[$b] = $coeff;
                    $coefficients[$b]->[$a] = $coeff;
                }
            }
        }
    }
    return (\@correlations,\@coefficients,\@covariances);
}

sub output_get_estimated_attributes
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              parameter  => { isa => 'Str', optional => 0},
                              attribute => { isa => 'Str', optional => 0},
                              problem_index => { isa => 'Int', optional => 0},
        );
    my $parameter=$parm{'parameter'};
    my $attribute=$parm{'attribute'};
    my $problem_index = $parm{'problem_index'};

    unless (defined $self->problems->[$problem_index]->input_problem->msfis and scalar(@{$self->problems->[$problem_index]->input_problem->msfis})>0){
        return $self->problems->[$problem_index]->input_problem->get_estimated_attributes(parameter => $parameter,
                                                                                          attribute => $attribute);
    }else{
        my $probnum = $self->problems->[$problem_index]->input_problem->msfis->[0]->get_msfo_from_problem_number;
        if (($probnum > 0) and (defined $self->problems->[$probnum-1]) and (defined $self->problems->[$probnum-1]->input_problem)) {
            unless (defined $self->problems->[$probnum-1]->input_problem->msfis and scalar(@{$self->problems->[$probnum-1]->input_problem->msfis})>0){
                return $self->problems->[$probnum-1]->input_problem->get_estimated_attributes(parameter => $parameter,
                                                                                              attribute => $attribute);
            }
            $problem_index = $probnum-1;
        }
        #fallback to guess
        my @array = ();
        if (defined $self->problems->[$problem_index]->subproblems and
            defined $self->problems->[$problem_index]->subproblems->[0] and
            defined $self->problems->[$problem_index]->subproblems->[0]->guess_estimated_attributes->{$attribute} ){
            for (my $i=0; $i<scalar(@{$self->problems->[$problem_index]->subproblems->[0]->guess_estimated_attributes->{$attribute}}); $i++){
                if (($parameter eq 'all') or
                    ($parameter eq $self->problems->[$problem_index]->subproblems->[0]->guess_estimated_attributes->{'param'}->[$i])){
                    push(@array,$self->problems->[$problem_index]->subproblems->[0]->guess_estimated_attributes->{$attribute}->[$i]);
                }
            }
        }
        return \@array;
    }
}

sub get_filtered_values
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              problem_index => { isa => 'Int', optional => 1, default => 0 },
                              subproblem_index => { isa => 'Int', optional => 1, default => 0 },
                              parameter  => { isa => 'Str', optional => 1, default => 'all' },
                              category  => { isa => 'Str', optional => 1, default => 'estimate' },
                              allow_sdcorrform  => { isa => 'Bool', optional => 1, default => 0}
#                              sdcorrform_ref  => { isa => 'Maybe[ArrayRef]', optional => 1} #TODO need to give this option by option
        );
    my $problem_index=$parm{'problem_index'};
    my $subproblem_index=$parm{'subproblem_index'};
    my $parameter=$parm{'parameter'};
    my $category=$parm{'category'};
    my $allow_sdcorrform=$parm{'allow_sdcorrform'};


    my @problems = ($problem_index+1);
    my @subproblems = ($subproblem_index +1);

    unless ($parameter eq 'all' or $parameter eq 'theta' or $parameter eq 'omega' or $parameter eq 'sigma'){
        croak("Illegal value $parameter of option parameter in output::get_filtered_values");
    }

    unless ($category eq 'estimate' or $category eq 'se' or $category eq 'cvse' or $category eq 'c'){
        croak("Illegal value $category of option category in output::get_filtered_values");
    }

    unless (defined $self->problems and scalar(@{$self->problems})>$problem_index){
        croak("problem with index $problem_index does not exist in output object in get_filtered_values");
    }

    my @coordinate_strings = @{$self->output_get_estimated_attributes(parameter => $parameter,
                                                                      attribute => 'coordinate_strings',
                                                                      problem_index => $problem_index)};

    my %sdcorr_hash;
    if ($allow_sdcorrform){
        my @sdcorr_arr = @{$self->output_get_estimated_attributes(parameter => $parameter,
                                                                  attribute => 'sdcorrform',
                                                                  problem_index => $problem_index)};
        for (my $j=0; $j< scalar(@coordinate_strings); $j++){
            $sdcorr_hash{$coordinate_strings[$j]} = $sdcorr_arr[$j];
        }
    }

    my $attribute = $category;
    $attribute = '' if ($category eq 'estimate');

    my %values_hash;
    my @values = ();

    foreach my $param ('theta','omega','sigma'){
        next unless ($parameter eq 'all' or ($parameter eq $param));
        next if ($param eq 'theta' and $category eq 'c');
        my $attr= $attribute.$param.'coordval';
        my $ref = $self->access_any(attribute=>$attr,problems=>\@problems,subproblems=>\@subproblems);

        if ($allow_sdcorrform and ($param eq 'omega' or $param eq 'sigma')){
            $attr = 'sdcorrform_'.$attr;
            my $sdcorrref = $self->access_any(attribute=>$attr,problems=>\@problems,subproblems=>\@subproblems);
            if (defined $ref and defined $ref->[0] and defined $ref->[0]->[0] and
                defined $sdcorrref and defined $sdcorrref->[0] and defined $sdcorrref->[0]->[0]){
                foreach my $key (keys %{$ref->[0]->[0]}){
                    if ($sdcorr_hash{$key} == 1){
                        $values_hash{$key} = $sdcorrref->[0]->[0]->{$key};
                    }else{
                        $values_hash{$key} = $ref->[0]->[0]->{$key};
                    }
                }
            }

        }else{
            if (defined $ref and defined $ref->[0] and defined $ref->[0]->[0]){
                foreach my $key (keys %{$ref->[0]->[0]}){
                    $values_hash{$key} = $ref->[0]->[0]->{$key};
                }
            }
        }
    }

    foreach my $coord (@coordinate_strings){
        push(@values,$values_hash{$coord});
    }

    return \@values;
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
    } elsif ( ($attribute eq 't_matrix')
              or
              ($attribute eq 'nonparametric_step_run')
        ) {
        $arr = $self->access_any(attribute => $attribute,
                                 problems => [($problem_index + 1)],
                                 subproblems => [($subproblem_index +1)]);
        if (defined $arr->[0]) {
            if (ref $arr->[0] eq "ARRAY"){
                $return_value=$arr->[0]->[0];
            }else{
                $return_value=$arr->[0];
            }
        }
    } elsif ( ($attribute eq 'estimation_step_run') or
              ($attribute eq 'estimation_step_initiated')
        ) {
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

sub sdcorrform_omegas
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
    my @omegas = @{$self->access_any(attribute=>'sdcorrform_omegas',problems=>\@problems,subproblems=>\@subproblems,parameter_numbers=>\@parameter_numbers)};


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

sub sdcorrform_seomegas
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
    my @seomegas = @{$self->access_any(attribute=>'sdcorrform_seomegas',problems=>\@problems,subproblems=>\@subproblems)};

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

sub sdcorrform_sesigmas
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
    my @sesigmas = @{$self->access_any(attribute=>'sdcorrform_sesigmas',problems=>\@problems,subproblems=>\@subproblems)};
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

sub sdcorrform_sigmas
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
    my @sigmas = @{$self->access_any(attribute=>'sdcorrform_sigmas',problems=>\@problems,subproblems=>\@subproblems,parameter_numbers=>\@parameter_numbers)};

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
        $self -> load;
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

    my @lstfile = utils::file::slurp_file($self->full_name);

    # Get rid of SIR results. When needed send to separate parser
    my @newlst;
    my $skip = 0;
    for my $line (@lstfile) {
        if ($line =~ /^SIR SAMPLING ITERATION/) {
            $skip = 1;
        } elsif ($line =~ /^ #CPUT:/) {
            $skip = 0;
        }
        if (not $skip) {
            push @newlst, $line;
        }
    }
    @lstfile = @newlst;

    if ($self->append_nm_OUTPUT and -e $self->directory.'OUTPUT'){
        my @extra = utils::file::slurp_file($self->directory.'OUTPUT');
        push(@lstfile,@extra);
    }

    my $lstfile_pos = -1;
    $self->parsed_successfully(1);

    my $problem_start;
    my $problem_index = 0;
    my $success = 0;
    #my $tbln; #NONMEM table number from tag #TBLN
    my $n_previous_meth = 0;
    my $evaluation_missing_from_ext_file = 0;
    my $lst_version;
    my $endtime;
    my $starttime;
    my $is_timestamp=0;
    my $nm_version_710=0;

    #new in 3.5.10, read control stream from lst-file
    my $reading_control_stream = 0;
    my $done_reading_control_stream = 0;
    my $found_control_stream = 0;
    my $found_nmtran_message = 0;
    my $control_stream_start_index;
    my $control_stream_end_index;

    my @prerun_messages=();
    my $reading_prerun_messages=1;

    #first read date stamp
    while ($lstfile_pos < $#lstfile){
        $lstfile_pos++;
        $_ = $lstfile[$lstfile_pos];
        ($is_timestamp,$starttime) = output::problem::is_timestamp(\@lstfile,$lstfile_pos);
        if ($is_timestamp){
            @prerun_messages =();
            last;
        }
        if (/^\s*(;|\$)/){
            #found control stream
            $control_stream_start_index=$lstfile_pos;
            #rewind one step
            $lstfile_pos--;
            @prerun_messages =();
            last;
        }
        if ((/^\s*NM\-TRAN MESSAGES/) or (/^\s*WARNINGS AND ERRORS \(IF ANY\)/) or (/^\s* AN ERROR WAS FOUND IN THE CONTROL STATEMENTS/)) {
            $found_nmtran_message = 1;
            @prerun_messages =();
        }
        chomp;
        push(@prerun_messages, $_) if /\w/;
    }

    unless (defined $starttime or defined $control_stream_start_index){
        #something went wrong. Store error messages and finish
        $self->parsing_error( message => "It seems there was an error before NONMEM started, messages are:\n".join("\n",@prerun_messages)."\n");
        $self->parsed_successfully(0);
        return 0;
    }

    #then read  control stream. Handle case when none exists (e.g. lst-file for missing control stream, psn stderr)
    #model->new
    while ($lstfile_pos < $#lstfile){
        $lstfile_pos++;
        $_ = $lstfile[$lstfile_pos];
        if ((defined $control_stream_start_index) and (not $found_control_stream) and /^\s*[^;\$]/ ){
            #false alarm, not a comment and not part of control stream
            $control_stream_start_index=undef;
        }
        if ( (not defined $control_stream_start_index ) and /^\s*(;|\$)/){
            #found control stream
            $control_stream_start_index=$lstfile_pos;
        }
        if (/^\s*\$PROB/){
            $found_control_stream=1;
            @prerun_messages =();
        }
        if ((/^\s*NM\-TRAN MESSAGES/) or (/^\s*WARNINGS AND ERRORS \(IF ANY\)/) or (/^\s* AN ERROR WAS FOUND IN THE CONTROL STATEMENTS/)
            or /^1NONLINEAR MIXED EFFECTS MODEL PROGRAM/ or (/^\s*License /) or (/^\s*doing nmtran/) ){

            $found_nmtran_message=1;
            @prerun_messages = ();
            if ($found_control_stream){
                $control_stream_end_index= ($lstfile_pos-1);
                #rewind one step
                $lstfile_pos--;
                last;
            }
            #else there is an error and we keep reading messages until end of file
        }
        chomp;
        push(@prerun_messages, $_) if /\w/;
    }

    unless ($found_control_stream){
        if ($found_nmtran_message){
            $self->parsing_error( message => "NMtran messages without a control stream in the output file. Messages are:\n".join("\n",@prerun_messages)."\n");
        }else{
            $self->parsing_error( message => "It seems there was an error before NONMEM started, messages are:\n".join("\n",@prerun_messages)."\n");
        }
        $self->parsed_successfully(0);
        return 0;
    }
    unless (defined $control_stream_end_index){
        $self->parsing_error( message => "Could not find anything after the control stream copy in ".$self -> full_name."\n");
        $self->parsed_successfully(0);
        return 0;
    }
    my @model_lines = @lstfile[$control_stream_start_index .. $control_stream_end_index];

    $self->lst_model(model->new(model_lines => \@model_lines,
                                filename => 'dummy',
                                directory => 'dummy',
                                ignore_missing_data => 1,
                                ignore_missing_files =>1,
                                ignore_missing_output =>1));


    #then read NMtran messages and license and nmversion
    my $found_license=0;
    my $found_nonmem = 0;
    while ($lstfile_pos < $#lstfile){
        $lstfile_pos++;
        $_ = $lstfile[$lstfile_pos];
        if ((/^\s*NM\-TRAN MESSAGES/) or (/^\s*WARNINGS AND ERRORS \(IF ANY\)/) or (/^\s* AN ERROR WAS FOUND IN THE CONTROL STATEMENTS/)
            or (/^\s*doing nmtran/) ){
            $found_nmtran_message=1;
            @prerun_messages = ();
        }elsif (/^\s*License /) {
            $found_license=1;
            @prerun_messages = (); #probably nmtran was ok, reset
        }elsif (/^\s*ERROR reading license file /) {
            @prerun_messages = (); #probably nmtran was ok, reset
        }elsif (/^1NONLINEAR MIXED EFFECTS MODEL PROGRAM/) {
            $found_nonmem=1;
            if (/VERSION 7/) {
                $lst_version = 7;
                if (/VERSION 7\.1\.0/) {
                    $nm_version_710=1;
                }
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

            @prerun_messages = ();
            last;
        }
        chomp;
        if (/\w/){
            push(@prerun_messages, $_) unless (/^\s*#CPUT:/ or /^\s*Stop Time/ or
                                               /^\s*(Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)/);
        }
    }

    unless (defined $lst_version){
        if (not $found_license and not $found_nonmem and -e 'OUTPUT'){
            $self->could_append_OUTPUT(1);
        }
        $self->parsing_error( message => "It seems there was an error before NONMEM started, messages are:\n".join("\n",@prerun_messages)."\n");
        $self->parsed_successfully(0);
        return 0;
    }

    #then read endtime
    my $j = $#lstfile;
    $is_timestamp = 0;
    while ( $_ = $lstfile[ $j -- ] ) {
        ($is_timestamp,$endtime) = output::problem::is_timestamp(\@lstfile,($j + 1));
        if ($is_timestamp){
            if (defined $starttime) {
                $self->set_runtime(($endtime - $starttime));
            }
            last;
        } elsif (/^1NONLINEAR MIXED EFFECTS MODEL PROGRAM/) {
            #if we end up here the lst-file is incomplete, was no end time printed
            #by nmfe
            last;
        }
    }

#    print "runtime is ".$self->runtime."\n";
    my $ext_file = nmtablefile->new(filename => $self -> directory().$self -> filename_root().'.ext',
                                    is_ext_file => 1) if (-e $self -> directory().$self -> filename_root().'.ext');

    #then read NONMEM output
    while ( $_ = $lstfile[ $lstfile_pos++ ] ) {
#        if (/^\s*\#TBLN:\s*([0-9]+)/) {
#            $tbln = $1; #used for limiting reading of tables from additional output
#        } els
        if ( /^ PROBLEM NO\.:\s+\d+\s+$/ or $lstfile_pos > $#lstfile ) {
            if ( defined $problem_start ) {
                my $adj = 1;
                my @problem_lstfile =    @lstfile[$problem_start .. ($lstfile_pos - $adj)];

                #We send full raw_file, cov_file... arrays to problem object
                #the right table number will be extracted there using $n_previous_meth
                #we skip tables that must come from restarted numbering (<= previous number),
                #NM7 table numbering in additional output is inconsistent and we cannot handle it
                #those numbers will be taken from lst-file instead, good enough since rare case
                #if nm_major_version<=6 undefined arrays, okay

                if (not (defined $self->lst_model and defined $self->lst_model->problems
                         and (scalar(@{$self->lst_model->problems}) >$problem_index)
                         and defined $self->lst_model->problems->[$problem_index])){
                    print "\nCould not find a model file copy (control stream) at top of lst-file for problem number ".
                        ($problem_index + 1) . " in lst-file\n".$self->full_name.
                        "\nThe nmfe script normally copies the model file but PsN cannot find it.\n";
                    $self -> parsed_successfully(0);
                    my $mes = $self->parsing_error_message();
                    $mes .= ' lst-file corrupted, could not find control stream copy for all PROBLEM NO ';
                    $self -> parsing_error_message( $mes );
                }else{
#                    print "adding problem\n";
                    $self -> add_problem ( init_data =>
                                           { lstfile            => \@problem_lstfile,
                                             ignore_missing_files => $self -> ignore_missing_files(),
                                             nm_major_version     => $lst_version,
                                             nm_version_710       => $nm_version_710,
                                             evaluation_missing_from_ext_file => $evaluation_missing_from_ext_file,
                                             filename_root          => $self -> filename_root(),
                                             directory              => $self -> directory(),
                                             n_previous_meth      => $n_previous_meth,
                                             ext_file             => $ext_file,
                                             #                                             table_number         => $tbln,
                                             problem_index        => $problem_index,
                                             input_problem        => $self->lst_model->problems->[$problem_index]});

                    my $mes = $self->parsing_error_message();
                    $mes .= $self->problems->[$problem_index] -> parsing_error_message();
                    $self -> parsing_error_message( $mes );
                    $self -> parsed_successfully($self -> parsed_successfully() *
                                                 ($self->problems->[$problem_index] -> parsed_successfully()));

                    if (not $self->iterations_interrupted and $self->problems->[$problem_index]->iterations_interrupted){
                        $self->iterations_interrupted(1);
                    }

                    $self -> msfo_has_terminated($self->msfo_has_terminated()+$self->problems->[$problem_index]->msfo_has_terminated());
                    $n_previous_meth = $self->problems->[$problem_index]->last_method_number if
                        (defined $self->problems->[$problem_index]->last_method_number);
                    $evaluation_missing_from_ext_file = $self->problems->[$problem_index]->evaluation_missing_from_ext_file;
                    $problem_index++;
                }
                @problem_lstfile = undef;
                $success = 1;

            }  #end if defined problem start
            $problem_start = $lstfile_pos;
        }

    }

    unless( $success ) {
        $self->parsing_error( message => 'Output file seems interrupted, could not find a PROBLEM NO statement in "' .
                              $self->full_name . '"' . "\n" );
        $self->parsed_successfully(0);
        return 0;
    }

    $self->parsed(1);
}

sub get_nonmem_parameters
{
    my %parm = validated_hash(\@_,
        output => { isa => 'output', optional => 0 }
    );
    my $output = $parm{'output'};

    my $error = $output -> load;

    if ($error){
        croak("Failed get_nonmem_parameters:\n $error \n");
    }

    my %hash;
    $hash{'values'} = $output->get_filtered_values(parameter => 'all',
                                                   category => 'estimate');

    unless ( not_empty($output->problems) ) {
        croak("No problems defined in output object in get_nonmem_parameters");
    }

    foreach my $key (keys %{$output->problems->[0]->input_problem->estimated_parameters_hash}){
        $hash{$key} = $output->problems->[0]->input_problem->estimated_parameters_hash->{$key};
    }
    return \%hash;

}

sub set_runtime
{
    # Creates a time string +hh:mm:ss from a time in seconds and sets the runtime
    my $self = shift;
    my $runtime = shift;

    if ($runtime == 0) {
        $self->runtime('00:00:00');
    } else {
        my $seconds = $runtime % 60;
        my $minutes = (($runtime - $seconds) / 60) % 60;
        my $hours = ($runtime - $seconds - 60 * $minutes) / 3600;
        $self->runtime(sprintf "%i:%02i:%02i", $hours, $minutes, $seconds);
    }
}

1;
