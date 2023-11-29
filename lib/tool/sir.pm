package tool::sir;

use include_modules;
use strict;
use File::Copy 'cp';
use data;
use OSspecific;
use tool::modelfit;
use random qw(random_get_seed random_set_seed random_uniform random_multivariate_normal);
use Mouse;
use MouseX::Params::Validate;
use Math::MatrixReal;
use Math::Trig;    # For pi
use File::Spec;
use output;
use array qw(:all);
use math qw(usable_number round);
use linear_algebra;
use utils::file;
use boxcox;
use Data::Dumper;
extends 'tool';

has 'sir_raw_results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['sirlog.csv'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'sir_results.csv' );

has 'done' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'recover' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'add_iterations' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'recenter' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'adjust_blocks' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'check_cholesky_reparameterization' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'center_rawresults_vector' => ( is => 'rw', isa => 'ArrayRef',default => sub { [] } );
has 'boxcox' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'negative_dofv' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'recompute' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'with_replacement' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'fast_posdef_checks' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'cap_resampling' => ( is => 'rw', isa => 'Int', default => 1 );
has 'cap_correlation' => ( is => 'rw', isa => 'Num', default => 0.8);
has 'samples' => ( is => 'rw', required => 1, isa => 'ArrayRef' ); #default in bin script
has 'resamples' => ( is => 'rw', required => 1, isa => 'ArrayRef' ); #default in bin script
has 'attempted_samples' => ( is => 'rw', isa => 'ArrayRef',default => sub { [] } );
has 'successful_samples' => ( is => 'rw', isa => 'ArrayRef',default => sub { [] } );
has 'actual_resamples' => ( is => 'rw', isa => 'ArrayRef',default => sub { [] } );
has 'parameter_hash' => ( is => 'rw', isa => 'HashRef' );
has 'iteration' => ( is => 'rw', isa => 'Int', default => 0 );
has 'rawres_samples' => ( is => 'rw', isa => 'Int', default => 0 );
has 'max_iteration' => ( is => 'rw', isa => 'Int');
has 'covmat_input' => ( is => 'rw', isa => 'Str' );
has 'auto_rawres' => ( is => 'rw', isa => 'Maybe[Num]' );
has 'rawres_input' => ( is => 'rw', isa => 'Str' );
has 'offset_rawres' => ( is => 'rw', isa => 'Int', default => 1 );
has 'in_filter' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'inflation' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'theta_inflation' => ( is => 'rw', isa => 'Str', default => '1' );
has 'omega_inflation' => ( is => 'rw', isa => 'Str', default => '1' );
has 'sigma_inflation' => ( is => 'rw', isa => 'Str', default => '1' );
has 'rse_theta' => ( is => 'rw', isa => 'Str');
has 'rse_omega' => ( is => 'rw', isa => 'Str');
has 'rse_sigma' => ( is => 'rw', isa => 'Str');
has 'mceta' => ( is => 'rw', isa => 'Int', default => 0 );
has 'problems_per_file' => ( is => 'rw', isa => 'Maybe[Int]', default => 100 );
has 'print_iter_N' => ( is => 'rw', isa => 'Int', default => 0 );
has 'full_rawres_header' => ( is => 'rw', isa => 'ArrayRef' );

has 'minimum_ofv' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] });
has 'reference_ofv' => ( is => 'rw', isa => 'Num');
has 'subjects' => ( is => 'rw', isa => 'Int');
has 'pdf_vector' => ( is => 'rw', isa => 'ArrayRef' );
has 'intermediate_raw_results_files' => ( is => 'rw', isa => 'ArrayRef', default => sub{[]} );

our $relativepdf = 1;
our $recovery_filename = 'restart_information_do_not_edit.pl'; #this must be copied to bin also
our $inflate_only_diagonal = 1;

sub BUILD
{
    my $self  = shift;


    $self->problems_per_file(100) unless defined ($self->problems_per_file);
    croak("problems_per_file must be larger than 0") unless ($self->problems_per_file > 0);

    croak("No \$PROBLEM in input model") unless
        (defined $self ->models()->[0]->problems and scalar(@{$self ->models()->[0]->problems})>0);

    croak("No \$INPUT found") unless
        (defined $self ->models()->[0]->problems->[0]->inputs and
         scalar(@{$self ->models()->[0]->problems->[0]->inputs})>0);
    croak("No \$DATA found") unless
        (defined $self ->models()->[0]->problems->[0]->datas and
         scalar(@{$self ->models()->[0]->problems->[0]->datas})>0);

    #TODO support multiple $PROB
    croak("sir does not yet support more than 1 \$PROB in model ") if (scalar(@{$self ->models()->[0]->problems})>1);

    unless (scalar(@{$self->samples}) == scalar(@{$self->resamples})){
        croak("samples and resamples arrays must have equal length");
    }

    if ($self->add_iterations){
        unless ((-d $self->directory ) and ( -e $self->directory.$recovery_filename )){
            croak("Cannot use option -add_iterations unless directory contains restart information (file $recovery_filename)");
        }
        if (defined $self->covmat_input or defined $self->rawres_input){
            croak("Not allowed to set covmat_input or rawres_input with -add_iterations");
        }
    }elsif ((-d $self->directory ) and ( -e $self->directory.$recovery_filename )){
        $self->recover(1); #TODO message
        if (defined $self->covmat_input or defined $self->rawres_input){
            croak("Not allowed to set covmat_input or rawres_input when auto-recovering an old sir run");
        }
    }elsif ((-d $self->directory ) and ( -e $self->directory.$self->models->[0]->filename )){
        croak("\nThis looks like a restart (run directory already exists with some content) but there ".
            "is no restart information (file $recovery_filename). Cannot run in this directory.");
    }

    if ($self->add_iterations or $self->recover){
        unless ($self->load_restart_information()){
            croak("Failed to read restart information from $recovery_filename");
        }
    }else{
        #we only inflate if first iteration of new run
        $self->inflation(setup_inflation(theta_inflation => $self->theta_inflation,
                                         omega_inflation => $self->omega_inflation,
                                         sigma_inflation => $self->sigma_inflation,
                                         model => $self ->models()->[0]));
    }

    if($self->check_cholesky_reparameterization){
        #look for tags . If found print message otherwise unset
        my @code;
        my $prob = $self ->models()->[0]->problems->[0];
        if (defined $prob->pks and scalar(@{$prob->pks})>0 ) {
            @code = @{$prob->pks->[0]->code};
        } elsif (defined $prob->preds and scalar(@{$prob->preds})>0) {
            @code = @{$prob->preds->[0]->code};
        }
        my $found=0;
        my $tag = 'Cholesky reparameterize start';
        foreach my $line (@code){
            if ($line =~ /$tag/){
                $found=1;
                last;
            }
        }
        if ($found){
            ui->print(category=>'sir',
                      message => "Found Cholesky reparameterization tag in model code, ".
                      "will check reparameterized blocks posdef during sampling");#FIXME option unset
        }else{
            $self->check_cholesky_reparameterization(0);
        }
    }

    for my $accessor ('logfile','raw_results_file','raw_nonp_file'){
        my @new_files=();
        my @old_files = @{$self->$accessor};
        for (my $i=0; $i < scalar(@old_files); $i++){
            my $name;
            my $ldir;
            ( $ldir, $name ) =
                OSspecific::absolute_path( $self ->directory(), $old_files[$i] );
            push(@new_files,$ldir.$name) ;
        }
        $self->$accessor(\@new_files);
    }


    unless (scalar(@{$self->samples})>0){
        croak("samples arrays must have at least length 1");
    }

    $self->max_iteration(scalar(@{$self->samples}));
    for (my $j=0; $j<scalar(@{$self->samples}); $j++){
        croak("Number of samples must be larger than 0") unless ($self->samples()->[$j]>0);
        croak("Number of resamples must be larger than 1") unless ($self->resamples()->[$j]>1);
        croak("Number of resamples cannot be larger than samples unless with_replacement is set or ".
              "cap_resampling is large enough") unless
            (($self->resamples()->[$j] <= ($self->samples->[$j]*$self->cap_resampling)) or
             ($self->with_replacement));
    }

    if (defined $self->covmat_input and defined $self->rawres_input){
        croak("Not allowed to set both covmat_input and rawres_input");
    }
    if (defined $self->covmat_input and defined $self->auto_rawres){
        croak("Not allowed to set both covmat_input and auto_rawres");
    }
    if ($self->with_replacement and ($self->cap_resampling > 1)){
        croak("Cannot set both with_replacement and cap_resampling");
    }
    if ($self->cap_resampling < 1){
        croak("Cannot set cap_resampling less than 1");
    }
}

sub setup_covmatrix_from_variancevec
{
    my %parm = validated_hash(\@_,
                              variance => {isa => 'ArrayRef', optional => 0},
        );
    my $variance = $parm{'variance'};

    my $covmatrix = linear_algebra::get_identity_matrix(scalar(@{$variance}));
    for (my $i=0; $i<scalar(@{$variance}); $i++){
        $covmatrix->[$i][$i] = $variance->[$i];
    }
    return $covmatrix;
}

sub get_offdiagonal_variance
{
    my %parm = validated_hash(\@_,
                              covariance => {isa => 'Num', optional => 0},
                              rse_i => {isa => 'Num', optional => 0},
                              rse_j => {isa => 'Num', optional => 0},
                              var_i => {isa => 'Num', optional => 0},
                              var_j => {isa => 'Num', optional => 0},
                              type =>  {isa => 'Int', default => 1},
        );
    my $covariance = $parm{'covariance'};
    my $rse_i = $parm{'rse_i'};
    my $rse_j = $parm{'rse_j'};
    my $var_i = $parm{'var_i'};
    my $var_j = $parm{'var_j'};
    my $type = $parm{'type'};

    croak("illegal rse_i $rse_i") unless ($rse_i > 0);
    croak("illegal rse_j $rse_j") unless ($rse_j > 0);
    croak("illegal var_i $var_i") unless ($var_i > 0);
    croak("illegal var_j $var_j") unless ($var_j > 0);

    my $N = (100/$rse_i)**2+(100/$rse_j)**2+1;
    my $variance = ($covariance**2+$var_j*$var_i)/$N;

    if ($type == 2){
        #old
#        my $correlation = $covariance/sqrt($var_i*$var_j);
#        my $sd_i = ($var_i*$rse_i/100);
#        my $sd_j = ($var_j*$rse_j/100);
        $variance =abs($covariance)*sqrt($var_i*$var_j)*$rse_i*$rse_j/(100**2);
    }
    return $variance;

}

sub setup_variancevec_from_rse
{
    my %parm = validated_hash(\@_,
                              rse_theta => {isa => 'Str', optional => 0},
                              rse_omega => {isa => 'Str', optional => 0},
                              rse_sigma => {isa => 'Str', optional => 0},
                              parameter_hash => {isa => 'HashRef', optional => 0},
                              type =>  {isa => 'Int', default => 1},
        );
    my %rse;
    $rse{'theta'} = $parm{'rse_theta'};
    $rse{'omega'} = $parm{'rse_omega'};
    $rse{'sigma'} = $parm{'rse_sigma'};
    my $parameter_hash = $parm{'parameter_hash'};
    my $type = $parm{'type'};
    no warnings qw(uninitialized);

    #param => $self->parameter_hash->{'param'},
    #coords => $self->parameter_hash->{'coords'},
    #'values'
    #off_diagonal

    my $param='';
    my $needed_count=0;
    my @given;
    my %givencount;
    my %needed;
    my %diag_uncert_sd;
    my %diag_est_var;
    my %rse_hash;

    for (my $i=0; $i<scalar(@{$parameter_hash->{'param'}}); $i++){
        my $coord = $parameter_hash->{'coords'}->[$i];
        my $estimate = $parameter_hash->{'values'}->[$i];
        my $thisrse = undef;
        unless ($parameter_hash->{'param'}->[$i] eq $param){
            #new param
            $param = $parameter_hash->{'param'}->[$i];
            $needed{$param}=0;
            $rse_hash{$param}={};
            @given = split(/,/,$rse{$param});
            foreach my $val (@given){
                unless (usable_number($val) and ($val>0)){
                    croak("value $val in rse $param is not a positive number");
                }
            }
            $givencount{$param}=scalar(@given);
            $diag_uncert_sd{$param}={};
            $diag_est_var{$param}={};
        }
        unless ($parameter_hash->{'off_diagonal'}->[$i]==1){
            $diag_est_var{$param}->{$coord} = $estimate;
            $needed{$param}++;
            if ($givencount{$param} == 1){
                $thisrse=$given[0];
            }elsif($givencount{$param} >= $needed{$param}){
                $thisrse=$given[($needed{$param}-1)];
            }
            $rse_hash{$param}->{$coord}= $thisrse;
        }
        if (defined $thisrse){
            #this is diag, compute sd
            #as (rse_theta(i)*(final estimate theta (i))/100)
            $diag_uncert_sd{$param}->{$coord} = ($thisrse*($estimate)/100);
        }
    }
    foreach my $param ('theta','omega','sigma'){
        my $diagonal = '';
        $diagonal = 'diagonal' unless ($param eq 'theta');
        unless ($givencount{$param} == 1){
            if ($needed{$param} != $givencount{$param}){
                croak("illegal input rse $param ".$rse{$param}.": there are ".$needed{$param}." estimated $param ".
                      "$diagonal elements in model, but ".$givencount{$param}." rse values were given");
            }
        }
    }
    #unless we croaked already, we have all sd for theta and diagonal omega/sigma. Fill in whole vector now
    my @variancevec=();
    for (my $i=0; $i<scalar(@{$parameter_hash->{'param'}}); $i++){
        $param = $parameter_hash->{'param'}->[$i];
        my $coord = $parameter_hash->{'coords'}->[$i];
        if ($parameter_hash->{'off_diagonal'}->[$i]==1){
            my $estimate = $parameter_hash->{'values'}->[$i];
            if ($coord =~ /(\d+),(\d+)/){
                my $left = $1;
                my $right = $2;
                if (defined $diag_est_var{$param}->{$left.','.$left} and (defined $diag_est_var{$param}->{$right.','.$right})
                    and defined $rse_hash{$param}->{$left.','.$left} and (defined $rse_hash{$param}->{$right.','.$right})){
                        push(@variancevec,get_offdiagonal_variance(covariance => $estimate,
                                                                   rse_i => $rse_hash{$param}->{$left.','.$left},
                                                                   rse_j => $rse_hash{$param}->{$right.','.$right},
                                                                   var_i => $diag_est_var{$param}->{$left.','.$left},
                                                                   var_j => $diag_est_var{$param}->{$right.','.$right},
                                                                   type => $type));

                }else{
                    croak("bug finding diagvalues for $param $left and $right");
                }

            }else{
                croak("bug string $param matching for $coord");
            }
        }else{
            push(@variancevec,($diag_uncert_sd{$param}->{$coord})**2);
        }
    }
    return \@variancevec;

}

sub setup_inflation
{
    my %parm = validated_hash(\@_,
                              theta_inflation => {isa => 'Str', optional => 0},
                              omega_inflation => {isa => 'Str', optional => 0},
                              sigma_inflation => {isa => 'Str', optional => 0},
                              model => {isa => 'model', optional => 0},
        );
    my %inflation;
    $inflation{'theta'} = $parm{'theta_inflation'};
    $inflation{'omega'} = $parm{'omega_inflation'};
    $inflation{'sigma'} = $parm{'sigma_inflation'};
    my $model = $parm{'model'};

    my @inflationvec=();
    my $any_not_one=0;

    foreach my $param ('theta','omega','sigma'){
        my $coords = $model->problems->[0]->get_estimated_attributes(parameter => $param,
                                                                     attribute => 'coords');
        my $off_diagonals = $model->problems->[0]->get_estimated_attributes(parameter => $param,
                                                                            attribute => 'off_diagonal');
        my $needed_length = 0;
        my $have_off_diagonals=0;
        my $full_length = scalar(@{$coords});
        for (my $i=0; $i<scalar(@{$off_diagonals}); $i++){
            if ($off_diagonals->[$i] == 1){
                $have_off_diagonals=1;
            }else{
                $needed_length++;
            }
        }

        my @given = split(/,/,$inflation{$param});
        if ($needed_length == 0){
            #check that not set on commandline
            unless(scalar(@given)==1 and $given[0]==1){
                croak("illegal input $param inflation ".$inflation{$param}.": there are no estimated $param in model");
            }
        }else{
            #check that either length 1 or same length
            my $diagonal = '';
            $diagonal = 'diagonal' unless ($param eq 'theta');
            unless ( (scalar(@given) == $needed_length)
                     or (scalar(@given) == 1)){
                croak("illegal input $param inflation ".$inflation{$param}.": there are $needed_length estimated $param ".
                    "$diagonal elements in model, but ".scalar(@given)." values were given for inflation");
            }
        }
        foreach my $val (@given){
            unless (usable_number($val) and ($val>0)){
                croak("value $val in $param inflation is not a positive number");
            }
            $any_not_one = 1 unless ($val == 1);
        }

        if (scalar(@given) == $needed_length){
            if ($have_off_diagonals){
                #add derived values for off-diagonals
                my %diagvalues;
                my $index = 0;
                for (my $i=0; $i<scalar(@{$off_diagonals}); $i++){
                    unless ($off_diagonals->[$i] == 1){
                        $diagvalues{$coords->[$i]} = sqrt($given[$index]);
                        $index++;
                    }
                }

                my @augmented = ();
                $index = 0;
                for (my $i=0; $i<scalar(@{$off_diagonals}); $i++){
                    if ($off_diagonals->[$i] == 1){
                        my $str = $coords->[$i];
                        if ($str =~ /(\d+),(\d+)/){
                            my $left = $1;
                            my $right = $2;
                            if (defined $diagvalues{$left.','.$left} and (defined $diagvalues{$right.','.$right})){
                                push(@augmented,(($diagvalues{$left.','.$left})*($diagvalues{$right.','.$right})));
                            }else{
                                croak("bug finding diagvalues for $param $left and $right");
                            }

                        }else{
                            croak("bug string $param matching for $str");
                        }
                    }else{
                        push(@augmented,$given[$index]);
                        $index++;
                    }
                }
                push(@inflationvec,@augmented);
            }else{
                push(@inflationvec,@given);
            }
        }elsif ($full_length > 0){
            push(@inflationvec,($given[0]) x $full_length);
        } #else nothing

    }
    unless ($any_not_one){
        @inflationvec=();
    }
    return \@inflationvec;
}

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    my $model = $self ->models() -> [$model_number-1];

    my $first_iteration = $self->iteration +1; #can be > 1 if recover or add


    unless ($self->recover or $self->add_iterations ){
        $first_iteration = 1; #to be safe
        #print model copy to enable restart
        my $copymodel =$model -> copy( filename    => $model->filename,
                                       write_copy => 1,
                                       copy_datafile   => 0,
                                       copy_output => 0,
                                       copy_etas => 1,
                                       directory => $self->directory);
    }
    if ($self->recover and $self->done){
        ui->print(category => 'sir',
            message => "The restart information file in ".$self->directory."\n".
            "indicates that the previous run is finished. Nothing to do.\n");
        $self->rplots(-1); #rplot generation would crash when no rawres header
    }

    # ------------------------  Run original run if not already done  -------------------------------

    my $check_output_and_model_match=0;
    my $output;
    unless ($self->recover or $self->add_iterations){
        if ( $model -> is_run ) {
            #check that input model and provided lst-file with control stream copy match
            $check_output_and_model_match=1 unless (defined $self->rawres_input);
        }else{
            my %subargs = ();
            if ( defined $self -> subtool_arguments() ) {
                %subargs = %{$self -> subtool_arguments()};
            }

            if( $self -> nonparametric_etas() or
                $self -> nonparametric_marginals() ) {
                $model -> add_nonparametric_code;
            }
            my @models=();
            my $message = "Running input model";

            my $orig_fit =
                tool::modelfit->new( %{common_options::restore_options(@common_options::tool_options)},
                    base_directory     => $self->base_directory,
                                     directory         => $self ->directory().
                                     '/orig_modelfit_dir'.$model_number,
                                     models         => [$model],
                                     threads               => $self->threads,
                                     nm_output => 'ext,cov,coi,cor,phi',
                                     logfile             => undef,
                                     raw_results           => undef,
                                     prepared_models       => undef,
                                     copy_data             => $self->copy_data,
                                     top_tool              => 0,
                                     %subargs,
                                copy_up => 1, );

            ui->print(category => 'sir', message => $message);

            $orig_fit->run;
            $self->metadata->{'copied_files'} = $orig_fit->metadata->{'copied_files'};
        }

        $output = $model -> outputs -> [0];
        unless (defined $output){
            croak("No output object from input model");
        }

        my $original_ofv = $output->get_single_value(attribute => 'ofv');
        if (defined $original_ofv){
            $self->reference_ofv($original_ofv);
        }else{
            croak("No ofv from input model result files");
        }
        if (defined $output->nind() and scalar(@{$output->nind()})>0){
            $self->subjects($output->nind()->[0]);
        }
        $self->parameter_hash(output::get_nonmem_parameters(output => $output));

        if ($check_output_and_model_match ){
            #check that the same set of estimated parameter with the same labels
            #in both input model and existing output file

            my $mod_coords = $model->problems->[0]->get_estimated_attributes(parameter=> 'all',
                                                                             attribute => 'coordinate_strings');
            my $mismatch=0;
            if (scalar(@{$mod_coords})==scalar(@{$self->parameter_hash->{'coordinate_strings'}})){
                for (my $i=0; $i< scalar(@{$mod_coords}); $i++){
                    unless ($mod_coords->[$i] eq $self->parameter_hash->{'coordinate_strings'}->[$i]){
                        $mismatch = 1;
                        last;
                    }
                }
            }else{
                $mismatch=1;
            }

            if ($mismatch){
                my $message = "\nsir input error:\n".
                    "The input model and the control stream copy and the top of the lst-file do not match\n".
                    "Estimated parameters in the input model are\n".join(' ',@{$mod_coords})."\n".
                    "Estimated parameters in the lst-file model are\n".
                    join(' ',@{$self->parameter_hash->{'coordinate_strings'}})."\n";
                croak($message);
            }

        }
    }#end unless recover or add_iterations


    my $covmatrix;

    my $rawres_resampled_params_arr;
    if (defined $self->rawres_input or (defined $self->auto_rawres)){
        if (defined $self->rawres_input){
            my $resamp_href;
            my $extracolumns=[];
            my $require_numeric_ofv=0;
            if ($self->recover or $self->add_iterations){
                $extracolumns=['resamples'];
                $require_numeric_ofv=1;
                #when loading restart filter is set to empty
                #we read all successful samples even if not resampled. The filtering on resamples column
                #is done in empirical_statistics.
            }

            ($rawres_resampled_params_arr,$resamp_href) =
                model::get_rawres_params(filename => $self->rawres_input,
                                         filter => $self->in_filter,
                                         offset => $self->offset_rawres,
                                         extra_columns => $extracolumns,
                                         require_numeric_ofv => $require_numeric_ofv,
                                         model => $model);
            $self->rawres_samples(scalar(@{$rawres_resampled_params_arr}));

            if (scalar(@{$rawres_resampled_params_arr}) >= scalar(@{$self->parameter_hash->{'values'}})){
                unless (params_arr_full_rank(sampled_params_arr => $rawres_resampled_params_arr,
                                             parameter_hash => $self->parameter_hash)){
                    my $message = "The set of parameter vectors read from ".$self->rawres_input;
                    $message .= " using filter ".join(',',@{$self->in_filter}) if (scalar(@{$self->in_filter})>0);
                    $message .= " does not have full rank (vectors are too similar).";
                    $message .= " Cannot proceed with sir\n";
                    croak($message);
                }
            }
        }else{
            $rawres_resampled_params_arr = [];
        }

        unless ($self->recover or $self->add_iterations or
            (scalar(@{$rawres_resampled_params_arr}) >= scalar(@{$self->parameter_hash->{'values'}}))){
            my $filename = 'tweak_inits_rawres.csv';
            my $covfile = 'tweak_inits.cov';
            my $message = "Have ".scalar(@{$rawres_resampled_params_arr}).
                " parameter vectors but need at least ".(scalar(@{$self->parameter_hash->{'values'}})).
                "\n".
                "Using tweak inits to create fake raw results file tweak_inits_rawres.csv and covmat file $covfile";
            ui -> print( category => 'sir', message =>  $message);
            my $tweaksamples = tweak_inits_sampling( sampled_params_arr => $rawres_resampled_params_arr,
                                                     parameter_hash => $self->parameter_hash,
                                                     model => $model,
                                                     output => $output,
                                                     degree => $self->auto_rawres,
                                                     filename => $filename,
                                                     directory => $self->directory,
                );

            $rawres_resampled_params_arr = create_sampled_params_arr(samples_array => $tweaksamples,
                                                                     labels_hash => $self->parameter_hash,
                                                                     user_labels => 1); #otherwise get empty values, zero vectors

            my $resulthash = empirical_statistics( sampled_params_arr => $rawres_resampled_params_arr,
                                                   labels_hash => $self->parameter_hash,
                                                   do_percentiles => 0,
                                                   do_sdcorr => 0);

            my( $ldir, $name ) = OSspecific::absolute_path( $self ->directory(),$covfile);
            print_empirical_covmatrix(filename=> $ldir.$name,
                                      parameter_hash => $self->parameter_hash,
                                      covar => $resulthash->{'covar'});

        }
    }


    if (defined $self->covmat_input){
        my $strings = $output->problems->[0]->input_problem->get_estimated_attributes(attribute=>'coordinate_strings');

        if ($self->covmat_input eq 'identity'){
            $covmatrix = linear_algebra::get_identity_matrix(scalar(@{$strings}));
        }else{
            #read user matrix
            #use keep_labels_hash from input model problem
            my %keep_labels_hash;
            foreach my $coord (@{$strings}){
                $keep_labels_hash{$coord}=1;
            }

            my @lines = utils::file::slurp_file($self->covmat_input);
            my ($success,$lower_covar,$index_order_ref,$header_labels_ref) =
                output::problem::subproblem::parse_additional_table (covariance_step_run => 1,
                                                                     have_omegas => 1,
                                                                     have_sigmas => 1,
                                                                     method_string => ' ',
                                                                     type => 'cov',
                                                                     keep_labels_hash => \%keep_labels_hash,
                                                                     tableref => \@lines);
            @lines=undef;
            unless ($success){
                croak("failed to parse covmat_input file ".$self->covmat_input);
            }

            #error check that column headers match parameters in nonmem output
            unless ( scalar(@{$index_order_ref}) == scalar(@{$self->parameter_hash->{'param'}})){
                croak("Number of parameters ".scalar(@{$index_order_ref})." in covmat_input does not match number ".
                      scalar(@{$self->parameter_hash->{'param'}})." of estimated parameters in input model." );
            }
            my @covmat_column_headers = ();

            foreach my $ind (@{$index_order_ref}) {
                push (@covmat_column_headers, $header_labels_ref->[$ind]);
            }

            for (my $j=0; $j < scalar(@covmat_column_headers); $j++){
                my $covheader = $covmat_column_headers[$j];
                my $outheader;
                my $par = uc($self->parameter_hash->{'param'}->[$j]);
                if ($par eq 'THETA'){
                    $outheader = $par.$self->parameter_hash->{'coords'}->[$j];
                }else{
                    $outheader = $par.'('.$self->parameter_hash->{'coords'}->[$j].')';
                }
                unless ($covheader eq $outheader){
                    croak("headers $covheader from covmat_input and $outheader from input model does not match\n");
                }
            }

            $covmatrix = output::problem::subproblem::make_square($lower_covar);
        }
    }elsif (defined $self->rawres_input or (defined $self->auto_rawres)){
        #do not need any matrices at all, not sampling in 0th iteration
        1;
    }elsif (defined $self->rse_theta){
        my $variances = setup_variancevec_from_rse(rse_theta => $self->rse_theta,
                                                  rse_omega => $self->rse_omega,
                                                  rse_sigma => $self->rse_sigma,
                                                  parameter_hash => $self->parameter_hash);
        $covmatrix = setup_covmatrix_from_variancevec(variance => $variances);
    }else{
        $covmatrix = get_nonmem_covmatrix(output => $output);
    }

    if (defined $covmatrix) {
        my $is_posdef = linear_algebra::is_matrix_posdef(matrix => $covmatrix);
        if (not $is_posdef) {
            ($covmatrix, my $count) = linear_algebra::get_symmetric_posdef(
                matrix => $covmatrix,
                minEigen => 1E-10,
            );

            ui->print(category => 'sir', message => "\nWARNING the input covariance matrix is numerically not positive definite\n".
                  "(as checked with Cholesky decomposition without pivoting). Will proceed after forcing positive definiteness.\n");
        }
    }

    my $user_labels=0; #always use generic labels
    my ($resampled_params_arr,$resamp_href);
    my $previous_iteration_resulthash;
    my $have_resampled_params = 0;

    if (defined $rawres_resampled_params_arr ){
        #rawres either from file or tweak_inits or mix
        $resampled_params_arr = $rawres_resampled_params_arr;
    }

    my $fix_theta_labels = [];
    my $fix_theta_values = [];
    if ($self->check_cholesky_reparameterization){
        my $pr;
        if (defined $output and defined $output->problems->[0]->input_problem){
            $pr = $output->problems->[0]->input_problem;
        }else{
            $pr = $model->problems->[0];
        }
        if (defined $pr->thetas){
            foreach my $record (@{$pr->thetas}){
                foreach my $opt (@{$record->options}){
                    next unless ($opt->fix);
                    if (defined $opt->label and length($opt->label)>0){
                        push(@{$fix_theta_values},$opt->init);
                        push(@{$fix_theta_labels},$opt->label);
                    }# else cannot be SD or COR
                }
            }
        }
    }

    for (my $iteration=$first_iteration;$iteration<=$self->max_iteration(); $iteration++){
        if (defined $resampled_params_arr){
            #we have resampled_params_arr, either from first iteration or rawresinput, on original scale
            #Optionally do Box-Cox and get (possibly transformed) covariance matrix
            my $message = "Find optimal Box-Cox transformation of resampled vectors";
            my( $ldir, $name ) = OSspecific::absolute_path( $self ->directory(),'boxcox_covmatrix_iteration'.
                                                            ($iteration-1).'.cov');
            if (not $self->boxcox){
                $message = "Computing empirical covariance matrix from resampled vectors without Box-Cox transformation";
                ( $ldir, $name ) = OSspecific::absolute_path( $self ->directory(),'untransformed_covmatrix_iteration'.
                                                              ($iteration-1).'.cov');
            }
            ui -> print( category => 'sir', message =>  $message);
            $previous_iteration_resulthash = empirical_statistics( sampled_params_arr => $resampled_params_arr,
                                                                   labels_hash => $self->parameter_hash,
                                                                   get_lambda_delta => $self->boxcox,
                                                                   estimated_vector => $self->parameter_hash->{'values'},
                                                                   do_percentiles => 0,
                                                                   do_sdcorr => 0);
            #this does not use mu-vector
            #will overwrite previous iteration if recover
            print_empirical_covmatrix(filename=> $ldir.$name,
                                      covar => $previous_iteration_resulthash->{'covar'},
                                      parameter_hash => $self->parameter_hash); #unless recover/restart?? overwrite?

            $have_resampled_params =1;
        }
        $self->iteration($iteration);

        my $mu_values= $self->parameter_hash->{'values'};
        my $lambda = [];
        my $delta = [];

        my $inflation = $self->inflation();

        #covmatrix already read if *not* have resampled params
        if ($have_resampled_params){
            $inflation = [] unless ($iteration ==1); #keep inflation if rawres input and first iteration
            croak("iteration ".($iteration-1)."iteration_resulthash not defined")
                unless (defined $previous_iteration_resulthash and
                        defined $previous_iteration_resulthash->{'covar'});

            if ($previous_iteration_resulthash->{'rank_deficient'}){
                croak("The number of parameter vectors obtained in iteration ".($iteration-1)." is smaller than ".
                      "the number of estimated parameters. This gives a rank deficient covariance matrix, so ".
                      "sampling cannot proceed in iteration $iteration.\n");
            }
            $covmatrix = $previous_iteration_resulthash->{'covar'};
            if (not linear_algebra::is_matrix_posdef(matrix => $covmatrix)) {
                if ($self->boxcox){
                    croak("\nERROR: Empirical covariance matrix obtained after Box-Cox transformation is numerically ".
                          "not positive definite\n(as checked with Cholesky decomposition without pivoting). Cannot proceed with sir.\n");
                }else{
                    croak("\nERROR: Empirical covariance matrix obtained after resampling is numerically ".
                          "not positive definite\n(as checked with Cholesky decomposition without pivoting). Cannot proceed with sir.\n");
                }
            }

            if ($self->boxcox){
                $lambda = $previous_iteration_resulthash->{'lambda'};
                $delta = $previous_iteration_resulthash->{'delta'};
                $mu_values = boxcox::shift_and_box_cox(vector=>$self->parameter_hash->{'values'},
                                                       inverse=>0,
                                                       lambda=>$lambda,
                                                       delta=>$delta);
                if (1){
                    no warnings qw(uninitialized);
                    open ( RES, ">" . $self->directory().'delta_lambda_iteration'.($iteration-1).'.csv' );
                    print RES "parameter,delta,lambda,original.estimate,transformed.estimate\n";
                    for (my $k=0; $k< scalar(@{$delta}); $k++){
                        print RES '"'.$self->parameter_hash->{'labels'}->[$k].'",'.$delta->[$k].','.$lambda->[$k].','.
                            $self->parameter_hash->{'values'}->[$k].','.$mu_values->[$k]."\n";
                    }
                    close(RES);
                }
            }
        }#end if have resampled params. otherwise should have covmatrix from covmat input or $COV

        if (scalar(@{$inflation})>0 and $inflate_only_diagonal){
            #this modifies covmatrix in place, then we discard inflation that is built in
            inflate_covmatrix_diagonal(matrix => $covmatrix,
                                       inflation => $inflation);
            $inflation=[];
        }

        my ($modified,$maxcorr,$max_indices,$cap_indices) = linear_algebra::cap_correlation($covmatrix,$self->cap_correlation);
        if ($modified < 0){
            my @thelines=();
            foreach my $line (@{$covmatrix}){
                push(@thelines,join("\t",@{$line}));
            }
            croak("Input error cap_correlation, cap is ".$self->cap_correlation.
                  " and covmatrix is \n".join("\n",@thelines)."\n");
        }
        my $warn_correlations=0.8;
        if ($modified > 0){
            my $message = "Capped $modified off-diagonal(s) of proposal so that abs(correlation) <= ".
                $self->cap_correlation." between\n";
            for (my $i=0; $i<scalar(@{$cap_indices}); $i++){
                $message .= $self->parameter_hash->{'labels'}->[$cap_indices->[$i]->[0]].
                    " and ".$self->parameter_hash->{'labels'}->[$cap_indices->[$i]->[1]].
                    " (original correlation ".sprintf("%4.2f",$cap_indices->[$i]->[2]).")\n";
            }
            $message .= "If possible, reparameterize your model to avoid correlations. If it is not ".
                "possible, it is recommended to increase the number of samples (e.g. double) ".
                "without changing the number of resamples to avoid the exhaustion of good samples.";
            ui -> print( category => 'sir',     message => $message);
        }elsif (abs($maxcorr) > $warn_correlations){
            my $message =
                "\nThe correlation between ".$self->parameter_hash->{'labels'}->[$max_indices->[0]].
                " and ".$self->parameter_hash->{'labels'}->[$max_indices->[1]]." in the proposal exceeds $warn_correlations. This\n".
                "might lead to an underestimation of parameter uncertainty if the true\n".
                "correlation is less than $warn_correlations, or if it is nonlinear. It is advised to\n".
                "reparameterize the model if possible.\n";
            ui -> print( category => 'sir',     message => $message);
        }

        my $message = "Sampling from the truncated multivariate normal distribution";
        ui -> print( category => 'sir',     message => $message);

        my $mat = new Math::MatrixReal(1,1);
        my $muvector = $mat->new_from_rows( [$mu_values] );
        my $current_samples = update_attempted_samples(samples=>$self->samples,
                                                       successful_samples=>$self->successful_samples,
                                                       attempted_samples=>$self->attempted_samples,
                                                       iteration=> $iteration);
        if (($current_samples > $self->samples->[($iteration-1)]) and
            ($self->problems_per_file() > 1)){
            ui->print(category=> "sir", message => "Setting problems_per_file to 1 to minimize ofv losses");
            $self->problems_per_file(1);
        }
        my $sampled_params_arr;

        # Forcing the covariance matrix to be positive definite
        if (not linear_algebra::is_matrix_posdef(matrix => $covmatrix)) {
            ui->print(category => "sir", message => "Forcing covariance matrix used by sampler to be positive definite");
            ($covmatrix, my $count) = linear_algebra::get_symmetric_posdef(
                matrix => $covmatrix,
                minEigen => 1E-10,
            );
        }

        my ($vectorsamples,$boxcox_samples) = sample_multivariate_normal(
            check_cholesky_reparameterization => $self->check_cholesky_reparameterization,
            fix_theta_labels => $fix_theta_labels,
            fix_theta_values => $fix_theta_values,
            samples=>$current_samples,
            labels => $self->parameter_hash->{'labels'},
            covmatrix => $covmatrix,
            inflation => $inflation,
            adjust_blocks => $self->adjust_blocks,
            lower_bound => $self->parameter_hash->{'lower_bounds'},
            upper_bound => $self->parameter_hash->{'upper_bounds'},
            param => $self->parameter_hash->{'param'},
            coords => $self->parameter_hash->{'coords'},
            block_number => $self->parameter_hash->{'block_number'},
            choleskyform => $self->parameter_hash->{'choleskyform'},
            mu => $muvector,
            lambda => $lambda,
            delta => $delta,
            fast_posdef_checks => $self->fast_posdef_checks,
            print_iter_N => $self->print_iter_N,
        );

        $sampled_params_arr = create_sampled_params_arr(samples_array => $vectorsamples,
                                                        labels_hash => $self->parameter_hash,
                                                        user_labels => $user_labels);
        my $pdfvec;
        if ($self->boxcox and $have_resampled_params){
            $pdfvec = linear_algebra::mvnpdf_cholesky($covmatrix,$mu_values,$boxcox_samples,$inflation,$relativepdf);
        }else{
            $pdfvec= linear_algebra::mvnpdf_cholesky($covmatrix,$mu_values,$vectorsamples,$inflation,$relativepdf)
        }
        $self->pdf_vector($pdfvec);

        ui -> print( category => 'sir',
                     message => "Creating parameter vector evaluation models iteration $iteration...");

        my $modelsarr = model::create_maxeval_zero_models_array(
            problems_per_file => $self->problems_per_file,
            sampled_params_arr => $sampled_params_arr,
            mceta => $self->mceta(),
            ignore_missing_parameters => 1,
            basedirectory => $self->directory,
            subdirectory => $self->directory().'m'.$model_number.'/',
            model => $model,
            purpose => 'sir_iteration'.$iteration,
            match_labels => $user_labels
            );

        my %subargs = ();
        if ( defined $self -> subtool_arguments() ) {
            %subargs = %{$self -> subtool_arguments()};
        }

        $message = "Running iteration ".$self->iteration()." evaluation models";

        #Handle $ETAS
        my $phi_file = $model->get_or_set_etas_file();
        if (defined $phi_file) {
            for my $model (@$modelsarr) {
                my @extra_files;
                if (defined $model->extra_files) {
                    @extra_files = @{$model->extra_files};
                }

                @extra_files = ( @extra_files, $phi_file );
                $model->extra_files(\@extra_files);
            }
        }
        my $iteration_evaluation =
            tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
                                  models         => $modelsarr,
                                  base_directory   => $self -> directory,
                                  raw_results_file => [$self->directory.'raw_results_sir_iteration'.$iteration.'.csv'],
                                  directory             => undef,
                                  directory_name_prefix => 'iteration'.$iteration,
                                  _raw_results_callback => $self ->
                                  _modelfit_raw_results_callback( model_number => $model_number ),
                                  nmtran_skip_model => 2,
                                  raw_results           => undef,
                                  prepared_models       => undef,
                                  top_tool              => 0,
                                  copy_data             => $self->copy_data,
                                  %subargs );



        ui -> print( category => 'sir', message => $message );

        if ($iteration < $self->max_iteration()){
            #not final round
            push(@{$self->intermediate_raw_results_files},'raw_results_sir_iteration'.$iteration.'.csv');
            $iteration_evaluation -> run;
            #here we read all successful samples even if not resampled. The filtering on resamples column
            #is done in empirical_statistics
            ($resampled_params_arr,$resamp_href) =
                model::get_rawres_params(filename => $iteration_evaluation->raw_results_file()->[0],
                                         require_numeric_ofv => 1,
                                         extra_columns => ['resamples'],
                                         offset => 1, #first is center
                                         model => $model);

            if (($self->negative_dofv->[($iteration-1)] > 0) and ($self->recenter)){
                my ($errors,$new_ofv,$new_center) = get_min_ofv_values(sampled_params_arr => $resampled_params_arr,
                                                                       parameter_hash => $self->parameter_hash);
                if (scalar(@{$errors})>0){
                    croak("Recentering mu failed, error messages are \n".join("\n",@{$errors})."\n");
                }
                $self->parameter_hash->{'values'} = $new_center;

                $self->reference_ofv($new_ofv);
                ui->print(category => 'sir',
                          message => "Recentered mu to parameter vector\n".
                          join(' ',@{$self->parameter_hash->{'values'}})."\n".
                          "with lowest ofv $new_ofv");
            }

            my @restartseed = random_get_seed;
            save_restart_information( parameter_hash => $self->parameter_hash,
                                      center_rawresults_vector => $self->center_rawresults_vector,
                                      nm_version  => $self->nm_version,
                                      subjects => $self->subjects,
                                      model_filename => $model->filename,
                                      cap_resampling => $self->cap_resampling,
                                      cap_correlation => $self->cap_correlation,
                                      done  => 0,
                                      adjust_blocks  => $self->adjust_blocks,
                                      check_cholesky_reparameterization => $self->check_cholesky_reparameterization,
                                      recenter  => $self->recenter,
                                      copy_data => $self->copy_data,
                                      boxcox => $self->boxcox,
                                      with_replacement => $self->with_replacement,
                                      iteration  => $self->iteration,
                                      mceta  => $self->mceta,
                                      problems_per_file  => $self->problems_per_file,
                                      reference_ofv  => $self->reference_ofv,
                                      minimum_ofv => $self->minimum_ofv,
                                      negative_dofv => $self->negative_dofv,
                                      samples => $self->samples,
                                      resamples => $self->resamples,
                                      attempted_samples => $self->attempted_samples,
                                      successful_samples => $self->successful_samples,
                                      actual_resamples => $self->actual_resamples,
                                      intermediate_raw_results_files => $self->intermediate_raw_results_files,
                                      seed_array => \@restartseed
                );

        }else{
            #final round
            my ($dir,$file)=
                OSspecific::absolute_path( $self ->directory(),
                                           $self -> raw_results_file()->[$model_number-1] );
            push(@{$self->intermediate_raw_results_files},$file);

            $self -> prepared_models -> [$model_number-1]{'own'} = $modelsarr;
            $self->tools([]) unless (defined $self->tools());
            push( @{$self -> tools()},$iteration_evaluation);
        }
    }#end iteration loop


}

sub load_restart_information
{
    my $self = shift;

    #have already checked file exists

    #declare variables
    #these should be same set as "variables to store" in create_template_models
    my %parameter_hash;

    my $nm_version;
    my $subjects;
    my $done;
    my $iteration;
    my $recenter;
    my $adjust_blocks;
    my $check_cholesky_reparameterization,
    my $copy_data;
    my $boxcox;
    my $with_replacement;
    my $cap_resampling;
    my $cap_correlation;
    my $mceta;
    my $problems_per_file;
    my $reference_ofv;

    my $model_filename;

    my @center_rawresults_vector;
    my @negative_dofv;
    my @samples;
    my @resamples;
    my @attempted_samples;
    my @successful_samples;
    my @actual_resamples;
    my @intermediate_raw_results_files;
    my @minimum_ofv;
    my @seed_array;

    my $ok = 0;

    if (-e $self->directory.$recovery_filename){
        open(FH, $self->directory.$recovery_filename) or croak("could not read recovery file");
        my $string = join(' ',<FH>);
        close(FH);

        eval $string;

        if ($self->recover){
            #store everything
            ui->print(category => 'sir',message => "Reading recovery information from iteration $iteration. "."\n".
                      "All sir-specific options, plus option -nm_version, will automatically\n".
                      "be set as in original run, even if set differently on new commandline.\n");
            $self->done($done);
            $self->recenter($recenter);
            $self->adjust_blocks($adjust_blocks);
            $self->check_cholesky_reparameterization($check_cholesky_reparameterization);
            $self->copy_data($copy_data);
            $self->boxcox($boxcox);
            $self->with_replacement($with_replacement);
            $self->cap_resampling($cap_resampling);
            $self->cap_correlation($cap_correlation) if (defined $cap_correlation);
            $self->mceta($mceta);
            $self->problems_per_file($problems_per_file);
            $self->samples(\@samples);
            $self->resamples(\@resamples);

             $self->nm_version($nm_version);
            random_set_seed(@seed_array);

        }else{
            #add_iterations
            ui->print(category => 'sir',message => "Reading results from iteration $iteration. "."\n".
                      "Options from original run will not be read.\n");

            my @new_samples = @samples[0 .. ($iteration-1)];
            push(@new_samples,@{$self->samples}); #append from commandline
            $self->samples(\@new_samples);

            my @new_resamples = @resamples[0 .. ($iteration-1)];
            push(@new_resamples,@{$self->resamples}); #append from commandline
            $self->resamples(\@new_resamples);

        }

        $self->subjects($subjects);
        $self->iteration($iteration);
        $self->reference_ofv($reference_ofv);
        $self->negative_dofv(\@negative_dofv);
        $self->attempted_samples(\@attempted_samples);
        $self->successful_samples(\@successful_samples);
        $self->actual_resamples(\@actual_resamples);
        $self->minimum_ofv(\@minimum_ofv);
        $self->center_rawresults_vector(\@center_rawresults_vector);

        $self->rawres_input($self->directory.$intermediate_raw_results_files[-1]);
        $self->offset_rawres(1); #first is original
        $self->in_filter([]); #no in-filter, we will use resamples column

        my $model = model -> new ( filename                    => $self->directory.$model_filename,
                                   ignore_missing_output_files => 1 );
        unless (defined $parameter_hash{'choleskyform'}){
            $parameter_hash{'choleskyform'} = $model->problems->[0]->get_estimated_attributes(attribute => 'choleskyform');
        }
        $self->parameter_hash(\%parameter_hash);
        unless ($self->copy_data){
            $model->relative_data_path(0);
        }
        $self->models([$model]);

        if ($done and $self->add_iterations){
            #we need to copy final raw_results file from last iteration to a numbered raw results file,
            #and change last item in intermediate_raw_results_files
            #in order to get this as intermediate rawres in add_iterations run
            my $newname = 'raw_results_sir_iteration'.$iteration.'.csv';
            cp($self->directory.$intermediate_raw_results_files[-1],$self->directory.$newname);
            $intermediate_raw_results_files[-1] = $newname;
        }
        $self->intermediate_raw_results_files(\@intermediate_raw_results_files);
        #redo autoset final rawresults file name
        my ($directory, $filename) = OSspecific::absolute_path( $model->directory, $model->filename );
        $model->filename( $filename );
        $model->directory( $directory );
        $filename =~ s/\.[^.]+$//; #remove last dot and extension
        $self->raw_results_file(['raw_results_'.$filename.'.csv']);
        $self->raw_nonp_file(['raw_nonparametric_'.$filename.'.csv']);

        $ok=1;
    }
    return $ok;
}

sub save_restart_information
{
    # $rawres_filename = $self->intermediate_raw_results_files->[-1];
    #static to facilitate testing
    my %parm = validated_hash(\@_,
                              parameter_hash => { isa => 'HashRef', optional => 0 },
                              nm_version  => { isa => 'Str', optional => 0 },
                              model_filename => { isa => 'Str', optional => 0 },
                              done  => { isa => 'Bool', optional => 0 },
                              recenter  => { isa => 'Bool', optional => 0 },
                              adjust_blocks  => { isa => 'Bool', optional => 0 },
                              check_cholesky_reparameterization  => { isa => 'Bool', optional => 0 },
                              copy_data => { isa => 'Bool', optional => 0 },
                              boxcox => { isa => 'Bool', optional => 0 },
                              with_replacement => { isa => 'Bool', optional => 0 },
                              iteration  => { isa => 'Int', optional => 0 },
                              subjects  => { isa => 'Int', optional => 0 },
                              cap_resampling  => { isa => 'Int', optional => 0 },
                              cap_correlation  => { isa => 'Maybe[Num]', optional => 1 },
                              mceta  => { isa => 'Int', optional => 0 },
                              problems_per_file  => { isa => 'Int', optional => 0 },
                              reference_ofv  => { isa => 'Num', optional => 0 },
                              center_rawresults_vector => { isa => 'ArrayRef', optional => 0 },
                              minimum_ofv => { isa => 'ArrayRef', optional => 0 },
                              negative_dofv => { isa => 'ArrayRef', optional => 0 },
                              samples => { isa => 'ArrayRef', optional => 0 },
                              resamples => { isa => 'ArrayRef', optional => 0 },
                              attempted_samples => { isa => 'ArrayRef', optional => 0 },
                              successful_samples => { isa => 'ArrayRef', optional => 0 },
                              actual_resamples => { isa => 'ArrayRef', optional => 0 },
                              intermediate_raw_results_files => { isa => 'ArrayRef', optional => 0 },
                              seed_array => { isa => 'ArrayRef', optional => 0 },
        );


    my $parameter_hash = $parm{'parameter_hash'};
    my $nm_version = $parm{'nm_version'};
    my $model_filename = $parm{'model_filename'};
    my $done = $parm{'done'};
    my $subjects = $parm{'subjects'};
    my $iteration = $parm{'iteration'};
    my $recenter = $parm{'recenter'};
    my $adjust_blocks = $parm{'adjust_blocks'};
    my $check_cholesky_reparameterization = $parm{'check_cholesky_reparameterization'};
    my $cap_resampling = $parm{'cap_resampling'};
    my $cap_correlation = $parm{'cap_correlation'};
    my $copy_data = $parm{'copy_data'};
    my $boxcox = $parm{'boxcox'};
    my $negative_dofv = $parm{'negative_dofv'};
    my $with_replacement = $parm{'with_replacement'};
    my $center_rawresults_vector = $parm{'center_rawresults_vector'};
    my $samples = $parm{'samples'};
    my $resamples = $parm{'resamples'};
    my $attempted_samples = $parm{'attempted_samples'};
    my $successful_samples = $parm{'successful_samples'};
    my $actual_resamples = $parm{'actual_resamples'};
    my $intermediate_raw_results_files = $parm{'intermediate_raw_results_files'};
    my $mceta = $parm{'mceta'};
    my $problems_per_file = $parm{'problems_per_file'};
    my $minimum_ofv = $parm{'minimum_ofv'};
    my $reference_ofv = $parm{'reference_ofv'};
    my $seed_array = $parm{'seed_array'};

    #local name

    my @dumper_names = qw(*parameter_hash *center_rawresults_vector *negative_dofv *intermediate_raw_results_files *samples *resamples *attempted_samples *successful_samples *actual_resamples *seed_array with_replacement mceta problems_per_file *minimum_ofv reference_ofv done iteration recenter copy_data boxcox nm_version model_filename cap_resampling cap_correlation adjust_blocks check_cholesky_reparameterization subjects);


    open(FH, '>'.$recovery_filename) or return -1; #die "Could not open file $recovery_filename for writing.\n";
    print FH Data::Dumper->Dump(
        [$parameter_hash,$center_rawresults_vector,$negative_dofv,$intermediate_raw_results_files,
         $samples,$resamples,$attempted_samples,$successful_samples,$actual_resamples,$seed_array,
         $with_replacement,$mceta,
         $problems_per_file,$minimum_ofv,$reference_ofv,$done,$iteration,$recenter,$copy_data,$boxcox,$nm_version,
        $model_filename, $cap_resampling, $cap_correlation, $adjust_blocks, $check_cholesky_reparameterization, $subjects],
        \@dumper_names
        );
    close FH;
    return 0;

}

sub get_min_ofv_values
{
    #static, no shift
    my %parm = validated_hash(\@_,
                              sampled_params_arr => { isa => 'ArrayRef', optional => 0 },
                              parameter_hash => { isa => 'HashRef', optional => 0 }
        );
    my $sampled_params_arr = $parm{'sampled_params_arr'};
    my $parameter_hash = $parm{'parameter_hash'};

    #sampled_params_arr has been created with require_numeric_ofv, so should not have NaN here
    my $index_lowest_ofv=0;
    my $lowest_ofv = $sampled_params_arr->[$index_lowest_ofv]->{'ofv'};
    for (my $i=1; $i<scalar(@{$sampled_params_arr}); $i++){
        if ($sampled_params_arr->[$i]->{'ofv'} < $lowest_ofv){
            $index_lowest_ofv=$i;
            $lowest_ofv = $sampled_params_arr->[$index_lowest_ofv]->{'ofv'};
        }
    }
#    print "\nLowest ofv $lowest_ofv at index $index_lowest_ofv\n";
    #modify $parameter_hash->{'values'}, match on $parameter_hash->{'labels'}

    my ($new_center,$errors) = get_vector_from_sampled_params_arr(sampled_params_arr => $sampled_params_arr,
                                                                  parameter_hash => $parameter_hash,
                                                                  index => $index_lowest_ofv);

    return ($errors,$lowest_ofv,$new_center);
}

sub params_arr_full_rank
{
    #static, no shift
    my %parm = validated_hash(\@_,
                              sampled_params_arr => { isa => 'ArrayRef', optional => 0 },
                              parameter_hash => { isa => 'HashRef', optional => 0 },
        );
    my $sampled_params_arr = $parm{'sampled_params_arr'};
    my $parameter_hash = $parm{'parameter_hash'};

    my @Amatrix=();

    for (my $i=0; $i<scalar(@{$sampled_params_arr}); $i++){
        my ($vector,$errors) = get_vector_from_sampled_params_arr(sampled_params_arr => $sampled_params_arr,
                                                                  parameter_hash => $parameter_hash,
                                                                  index => $i);
        push(@Amatrix,$vector);
    }

    return linear_algebra::full_rank(\@Amatrix);

}

sub get_vector_from_sampled_params_arr
{
    #static, no shift
    my %parm = validated_hash(\@_,
                              sampled_params_arr => { isa => 'ArrayRef', optional => 0 },
                              parameter_hash => { isa => 'HashRef', optional => 0 },
                              index => {isa => 'Int', optional => 0}
        );
    my $sampled_params_arr = $parm{'sampled_params_arr'};
    my $parameter_hash = $parm{'parameter_hash'};
    my $index = $parm{'index'};

    my @vector = ();
    my @errors = ();

    for (my $j=0; $j< scalar(@{$parameter_hash->{'labels'}}); $j++) {
        my $parameter = $parameter_hash->{'param'}->[$j];
        my $label = $parameter_hash->{'labels'}->[$j];
        if (defined $sampled_params_arr->[$index]->{$parameter}->{$label}){
            push(@vector,$sampled_params_arr->[$index]->{$parameter}->{$label});
        }else{
            push(@errors,"index $index parameter $parameter label $label undefined in sampled_params_arr");
        }
    }
    return (\@vector,\@errors);

}

sub mvnpdf
{
    #Note: this sub is not used in current sir procedure
    #inflation should be vector
    my %parm = validated_hash(\@_,
                              inverse_covmatrix => { isa => 'Math::MatrixReal', optional => 0 },
                              mu => { isa => 'Math::MatrixReal', optional => 0 },
                              xvec_array => { isa => 'ArrayRef[ArrayRef]', optional => 0 },
                              inflation => { isa => 'Num', optional => 0 },
                              relative => { isa => 'Bool', optional => 1, default => 1 }
    );
    my $inverse_covmatrix = $parm{'inverse_covmatrix'};
    my $mu = $parm{'mu'};
    my $xvec_array = $parm{'xvec_array'};
    my $inflation = $parm{'inflation'};
    my $relative = $parm{'relative'};

    my ($rows,$columns) = $inverse_covmatrix->dim();
    unless (($rows == $columns) and ($rows > 0)){
        croak("Input error mvnpdf: Inverse covmatrix must be square and dimension > 0, but we have rows=$rows and columns=$columns");
    }
    my $k=$rows;
    ($rows,$columns) = $mu->dim();
    unless ($rows == 1 and $columns == $k){
        croak("Input error mvnpdf: mu vector should have dimension (1,$k) but has dimension ($rows,$columns)");
    }
    unless (scalar(@{$xvec_array})>0){
        croak("Input error mvnpdf: xvec_array is empty");
    }
    unless ($inflation>0){
        croak("Input error mvnpdf: inflation is not > 0");
    }

    my $det_factor = get_determinant_factor(inverse_covmatrix => $inverse_covmatrix,
                                            k => $k,
                                            inflation => $inflation        );

    my @pdf_array=();
    my $delta_right = $mu->shadow(); #zeros matrix same size as $mu
    my $delta_left = $mu->shadow(); #zeros matrix same size as $mu

    foreach my $xvec (@{$xvec_array}){
        unless (scalar(@{$xvec}) == $k){
            croak("Input error mvnpdf: xvec should have dimension $k but has dimension ".scalar(@{$xvec}));
        }
        for (my $i=0; $i< $k; $i++){
            $delta_right->assign(1,($i+1),($xvec->[$i] - $mu->element(1,($i+1))));
            $delta_left->assign(1,($i+1),($xvec->[$i] - $mu->element(1,($i+1)))*(1/$inflation));
        }
        #now $delta is $xvec - $mu
        my $product_left = $delta_left->multiply($inverse_covmatrix);
        my $product=$product_left->multiply(~$delta_right); # ~ is transpose
        if ($relative){
            push(@pdf_array,exp(-0.5 * $product->element(1,1))); #we want relative the mu here, so divide by determinant factori which vanishes
        }else{
            push(@pdf_array,$det_factor*exp(-0.5 * $product->element(1,1))); #matlab absolute mvnpdf possibly change back
        }
    }
    return \@pdf_array;
}

sub compute_weights
{

    #weight is same as importance_ratio
    my %parm = validated_hash(\@_,
        pdf_array => { isa => 'ArrayRef', optional => 0 },
        dofv_array => { isa => 'ArrayRef', optional => 0 }
    );
    my $pdf_array = $parm{'pdf_array'};
    my $dofv_array = $parm{'dofv_array'};

    my $len = scalar(@{$pdf_array});
    my $bignum=1e20;
    unless ($len > 0){
        croak("pdf array is empty into compute_weights");
    }
    unless ($len == scalar(@{$dofv_array})){
        croak("compute_weights: pdf_array is length $len but dofv_array is length ".scalar(@{$dofv_array}));
    }

    my %hash;
    $hash{'weights'}=[];
    $hash{'cdf'}=[];
    my $cumsum=0;
    for (my $i=0; $i< $len; $i++){
        #we check in mvnpdf that pdf is not zero??
        my $wgt;
        if (not defined $dofv_array->[$i]){
            $wgt = 0;
        }elsif (not defined $pdf_array->[$i]){
            $wgt = 0;
        }elsif ($pdf_array->[$i] > 0){
            $wgt=exp(-0.5*($dofv_array->[$i]))/($pdf_array->[$i]);
        }else{
            $wgt=$bignum;
        }
        push(@{$hash{'weights'}},$wgt);
        $cumsum = $cumsum+$wgt;
        push(@{$hash{'cdf'}},$cumsum);
    }
    $hash{'sum_weights'}=$cumsum;
    return \%hash;
}

sub recompute_weights
{
    my %parm = validated_hash(\@_,
                              weight_hash => { isa => 'HashRef', optional => 0 },
                              reset_index => { isa => 'Int', optional => 0 }
        );
    my $weight_hash = $parm{'weight_hash'};
    my $reset_index = $parm{'reset_index'};

    my $len = scalar(@{$weight_hash->{'weights'}});
    if ($reset_index < 0 or $reset_index >= $len){
        croak("illlegal input to recompute_weights, reset_index is $reset_index but length of weights is $len");
    }
    $weight_hash->{'weights'}->[$reset_index] = 0;
    my $cumsum=0;
    $cumsum = $weight_hash->{'cdf'}->[$reset_index-1] if ($reset_index > 0);
    #loop over rest and recompute cdf
    for (my $i=$reset_index; $i< $len; $i++){
        $cumsum = $cumsum+$weight_hash->{'weights'}->[$i]; #first round this wgt is 0
        $weight_hash->{'cdf'}->[$i] = $cumsum;
    }
    $weight_hash->{'sum_weights'}=$cumsum;
}

sub weighted_sample
{
    my %parm = validated_hash(\@_,
                              cdf => { isa => 'ArrayRef', optional => 0 }
        );
    my $cdf = $parm{'cdf'};
    my $len = scalar(@{$cdf});
    croak("empty cdf into weighted_sample") unless ($len>0);
    my $max = $cdf->[$len-1];
    my $val = random_uniform(1,0,$max); # n low high

    for (my $i=0; $i< $len; $i++){
        return $i if ($val <= $cdf->[$i]);
    }

}

sub empirical_statistics
{
    my %parm = validated_hash(\@_,
                              sampled_params_arr => { isa => 'ArrayRef', optional => 0 },
                              labels_hash => { isa => 'HashRef', optional => 0 },
                              do_percentiles => {isa => 'Bool', optional => 1, default => 1},
                              do_sdcorr => {isa => 'Bool', optional => 1, default => 1},
                              get_lambda_delta => {isa => 'Bool', optional => 1, default => 0},
                              absmaxlambda => {isa => 'Num', optional => 1, default => 3},
                              resolutionlambda => {isa => 'Num', optional => 1, default => 0.2},
                              estimated_vector => { isa => 'ArrayRef', optional => 1 },
        );
    my $sampled_params_arr = $parm{'sampled_params_arr'};
    my $labels_hash = $parm{'labels_hash'};
    my $do_percentiles = $parm{'do_percentiles'};
    my $do_sdcorr = $parm{'do_sdcorr'};
    my $get_lambda_delta = $parm{'get_lambda_delta'};
    my $absmaxlambda = $parm{'absmaxlambda'};
    my $resolutionlambda = $parm{'resolutionlambda'};
    my $estimated_vector = $parm{'estimated_vector'};

    my $len = scalar(@{$sampled_params_arr});
    croak("empty set of samples to empirical_statistics") unless ($len >0);
    my $dim = scalar(@{$labels_hash->{'labels'}});
    croak("empty set of labels to empirical_statistics") unless ($dim >0);

    my %resulthash;

    if ($len < $dim){
        $resulthash{'rank_deficient'}=1;
    }
    my $count_resamples=1;
    $count_resamples = 0 unless (defined $sampled_params_arr->[0]->{'resamples'}); #assume all included

    my @all_labels=();
    my @all_params=();
    my $n_resamples=0;

    for (my $i=0; $i<$dim; $i++){
        #order is theta omega sigma
        push(@all_labels,$labels_hash->{'labels'}->[$i]);
        push(@all_params,$labels_hash->{'param'}->[$i]);
    }


    #create samples matrix
    my @Amatrix=();
    my @parameter_vectors=();
    my @sums=(0) x $dim;
    for (my $j=0; $j< $dim; $j++){
        push(@parameter_vectors,[]);
    }

    for (my $i=0; $i< $len; $i++){
        my @vector=();
        for (my $j=0; $j<$dim; $j++){
            push(@vector,$sampled_params_arr->[$i]->{$all_params[$j]}->{$all_labels[$j]});
        }

        my $max_k=1;
        if ($count_resamples){
            $max_k=$sampled_params_arr->[$i]->{'resamples'};
        }
        for (my $k=0; $k< $max_k; $k++){
            $n_resamples++;
            push(@Amatrix,\@vector);
            for (my $j=0; $j< $dim; $j++){
                push(@{$parameter_vectors[$j]},$vector[$j]);
                $sums[$j] = $sums[$j] + $vector[$j] unless ($get_lambda_delta);
            }
        }
    }
    croak("Number of resamples is 0 in empirical_statistics") if ($n_resamples < 1);

    $resulthash{'covar'}=[];
    $resulthash{'sdcorr'}=[];
    if ($get_lambda_delta){
        $resulthash{'lambda'}=[];
        $resulthash{'delta'}=[];
        my @Bmatrix=();
        for (my $j=0; $j< $dim; $j++){
            $sums[$j] =0;
            #label is $all_labels[$j]
            my ($lam,$del,$numerr) = boxcox::get_lambda_delta($parameter_vectors[$j],$absmaxlambda,$estimated_vector->[$j]);
            if ($numerr){
                ui->print(category=>'sir',
                          message => "Numerical error when searching for optimal lambda for Box Cox ".
                          "transformation of ".$all_labels[$j].
                          ". This might be caused by the parameter samples being too similar. ".
                          "Skipping transformation of this parameter in this iteration."."\n");
                push(@{$resulthash{'lambda'}},undef);
                push(@{$resulthash{'delta'}},0);
            }elsif (abs($lam-1)<$resolutionlambda){
                push(@{$resulthash{'lambda'}},undef);
                push(@{$resulthash{'delta'}},0);
            }else{
                push(@{$resulthash{'lambda'}},$lam);
                push(@{$resulthash{'delta'}},$del);
            }
        }
        #redo sums and Bmatrix
        foreach my $vec (@Amatrix){
            my $trans = boxcox::shift_and_box_cox(vector=>$vec,
                                                  lambda => $resulthash{'lambda'},
                                                  delta=>$resulthash{'delta'},
                                                  inverse=>0);
            push(@Bmatrix,$trans);
            for (my $j=0; $j< $dim; $j++){
                $sums[$j] = $sums[$j] + $trans->[$j];
            }
        }

        my $err1 = linear_algebra::row_cov(\@Bmatrix,$resulthash{'covar'});
        if ($err1 == 1){
            ui->print (category=> 'all',
                       message => "numerical error in linear_algebra rowcov for boxcox, cannot compute covmatrix");
        }elsif ($err1 == 2){
            ui->print (category=> 'all',
                       message => "input error in linear_algebra rowcov for boxcox, cannot compute covmatrix");
        }

    }else{
        my $err1 = linear_algebra::row_cov(\@Amatrix,$resulthash{'covar'});
        if ($err1 == 1){
            ui->print (category=> 'all',
                       message => "numerical error in linear_algebra rowcov, cannot compute covmatrix");
        }elsif ($err1 == 2){
            ui->print (category=> 'all',
                       message => "input error in linear_algebra rowcov, cannot compute covmatrix");
        }
        if ($do_sdcorr){
            $err1 = linear_algebra::covar2sdcorr($resulthash{'covar'},$resulthash{'sdcorr'});
            if ($err1 == 1){
                ui->print (category=> 'all',
                           message => "numerical error in linear_algebra covar2sdcorr, cannot compute correlation matrix");
            }elsif ($err1 == 2){
                ui->print (category=> 'all',
                           message => "input error to linear_algebra covar2sdcorr, cannot compute correlation matrix");
            }
        }
    }

    $resulthash{'center_estimate'}=[];
    $resulthash{'mean'}=[];
    $resulthash{'median'}=[];
     for (my $j=0; $j< $dim; $j++){
        push(@{$resulthash{'center_estimate'}},$labels_hash->{'values'}->[$j]);
        push(@{$resulthash{'mean'}},($sums[$j]/$n_resamples));
        push(@{$resulthash{'median'}},median($parameter_vectors[$j]));
    }


    if ($do_percentiles){
        my @pred_int = sort {$a <=> $b} 0,40,80,90,95;
        my @temp =();
        foreach my $pi (@pred_int){
            if ($pi == 0){
                push (@temp,50);
            }else {
                push (@temp,(100-$pi)/2);
                push (@temp,(100-(100-$pi)/2));
            }
        }
        my @perc_limit = sort {$a <=> $b} @temp;
        my @probs;
        foreach my $lim (@perc_limit){
            push(@probs,$lim/100);
        }

        my $no_perc_limits = scalar(@perc_limit);

        $resulthash{'percentiles_labels'}=\@perc_limit;
        $resulthash{'percentiles_values'}=[];
        for (my $j=0; $j< scalar(@probs); $j++){
            push(@{$resulthash{'percentiles_values'}},[(0) x $dim]);
        }

        for (my $j=0; $j< $dim; $j++){
            my @sorted = (sort {$a <=> $b} @{$parameter_vectors[$j]}); #sort ascending
            my $quantref = quantile(probs => \@probs, numbers=> \@sorted);
            for (my $k=0; $k< scalar(@probs); $k++){
                $resulthash{'percentiles_values'}->[$k]->[$j] = $quantref->[$k];
            }
        }
        #se and rse
        $resulthash{'standard_error'}=[];
        $resulthash{'relative_standard_error'}=[];
        $resulthash{'rse_sd_scale'}=[];
        for (my $j=0; $j< $dim; $j++){
            push(@{$resulthash{'standard_error'}},stdev($parameter_vectors[$j])); #interpretation is SE but formula is stdev
            push(@{$resulthash{'relative_standard_error'}},rse($parameter_vectors[$j],$labels_hash->{'values'}->[$j]));
            if ($labels_hash->{'param'}->[$j] eq 'theta'){
                push(@{$resulthash{'rse_sd_scale'}},'');
            }else{
                push(@{$resulthash{'rse_sd_scale'}},rse($parameter_vectors[$j],$labels_hash->{'values'}->[$j])/2);
            }
        }

    }
    return \%resulthash;
}

sub get_determinant_factor
{
    #Note not used
    #inflation should be vector
    my %parm = validated_hash(\@_,
                              inverse_covmatrix => { isa => 'Math::MatrixReal', optional => 0 },
                              k => { isa => 'Int', optional => 0 },
                              inflation => { isa => 'Num', optional => 1, default => 1 },
    );
    my $inverse_covmatrix = $parm{'inverse_covmatrix'};
    my $k = $parm{'k'};
    my $inflation = $parm{'inflation'};

    unless ($inflation > 0){
        croak("Inflation less than 0 in get_determinant_factor")
    }

    my $invdeterminant = $inverse_covmatrix->det();

    unless (defined $invdeterminant and $invdeterminant > 0){
        print "\nInverse covmatrix:\n";
        print $inverse_covmatrix;
        croak("\nFailed to compute determinant of inverse covariance matrix");
    }
    #p90 Mathematics Handbook
    # det(AB)=det(A)det(B) det I = 1,  det(xI)=x^n*det(I)=x^n
    # derivation: det((A*xI)^(-1))=det((xI)^(-1)A^(-1))=det((xI)^(-1))*det(A^(-1))=(1/x)^n*det(A^(-1))
    # k is n
    $invdeterminant = $invdeterminant * ((1/$inflation)**($k));

    return sqrt($invdeterminant)/((2* pi)**($k/2));

}

sub check_auto_cholesky_blocks_posdef
{
    #static, no shift
    my %parm = validated_hash(\@_,
                              xvec => { isa => 'ArrayRef', optional => 0 },
                              hash_array => {isa => 'ArrayRef', optional => 0},
                              fix_xvec => { isa => 'ArrayRef', optional => 0 },
        );

    my $xvec = $parm{'xvec'};
    my $hash_array = $parm{'hash_array'};
    my $fix_xvec = $parm{'fix_xvec'};

    my $minEigen=0.000000001;
    my $accept = 1;

    sub thepos{
        my $r=shift; #0 based row
        my $c=shift; #0 based col
        return round(($r*($r+1)/2)+($c+1)-1);
    }

    foreach my $hashref (@{$hash_array}){
        my $size = $hashref->{'size'};
        my $bounded = $hashref->{'bounded'};
        my $mat = [];
        my @sdvalues = ();
        my $pos;
        my $cor;
        my $value;
        for (my $row=0; $row< $size; $row++){
            push(@{$mat},[(0) x $size]);
            $pos = $hashref->{'indices'}->[thepos($row,$row)];
            #            print "pos is $pos\n";
            if ($pos >= 0){
                $value = $xvec->[$pos];
            }else{
#                print "pos is $pos, index ".abs($pos+1)."\n";
                $value = $fix_xvec->[abs($pos+1)];
            }
            unless ($bounded){
                $value = exp($bounded);
            }
            push(@sdvalues,$value);
        }
#        print "sd ".join(' ',@sdvalues)."\n";
        for (my $row=0; $row < $size; $row++){
            for (my $col=0; $col < $row; $col++){
                $pos = $hashref->{'indices'}->[thepos($row,$col)];
                if ($pos >= 0){
                    $cor = $xvec->[$pos];
                }else{
                    $cor = $fix_xvec->[abs($pos+1)];
                }
                unless ($bounded){
                    $cor = math::unbounded2correlation($cor);
                }
                $mat->[$row]->[$col] = $cor*$sdvalues[$row]*$sdvalues[$col]; #covar
                $mat->[$col]->[$row] = $mat->[$row]->[$col];
            }
            $mat->[$row]->[$row] = ($sdvalues[$row])**2; #sd
        }

        (my $eigenvalues, my $Q) = linear_algebra::eigenvalue_decomposition($mat);
#        for (my $row=0; $row< $size; $row++){
#            print join(' ',@{$mat->[$row]})."\n";
#        }
#        print "\n";
        if (linear_algebra::min($eigenvalues) < $minEigen){
            $accept = 0;
            last;
        }
    }
    return $accept;
}

sub setup_auto_cholesky_block_posdef_check
{
    my %parm = validated_hash(\@_,
                              labels => { isa => 'ArrayRef', optional => 0 },
                              fix_theta_labels => { isa => 'ArrayRef', optional => 0 },
                              param => { isa => 'ArrayRef', optional => 0 },
        );
    my $labels = $parm{'labels'};
    my $fix_theta_labels = $parm{'fix_theta_labels'};
    my $param = $parm{'param'};

    my $dim = scalar(@{$labels});

    #one row at a time, sd first , then corr if any
    #each time find sd, push a row

    #FIXME option to turn off check
    #FIXME auto-detect in pk/pred code

    my %sdcorr_matrices;
    my %sdcorr_fix;
    my %dimension;
    my $thisid = '';
    my $row;
    my $col;
    my $transform;
    for (my $i=0; $i< $dim; $i++){
        last unless ($param->[$i] eq 'theta');
        $transform = undef;
        if($labels->[$i] =~ /^(|log )SD_([A-Z]+)(\d?\d)$/){
            $transform = $1;
            $thisid = $2;
            $row=$3;
            $row =~ s/^0//;
            $sdcorr_matrices{$thisid}->{'sd'}->{$row} = $i;
            if ((not defined $dimension{$thisid}) or
                ($dimension{$thisid} < $row)){
                $dimension{$thisid} = $row;
            }
        }elsif($labels->[$i] =~ /^(|logit \()COR_([A-Z]+)(\d?\d)(\d?\d)\b/){
            $transform = $1;
            $thisid = $2;
            $row=$3;
            $col=$4;
            $row =~ s/^0//;
            $col =~ s/^0//;
            $sdcorr_matrices{$thisid}->{'corr'}->{$row}->{$col} = $i;
#            print "block $thisid, CORR $row $col\n";
            if ((not defined $dimension{$thisid}) or
                ($dimension{$thisid} < $row)){
                $dimension{$thisid} = $row;
            }
        }
        if (defined $transform){
            if (defined $sdcorr_matrices{$thisid}->{'bounded'}){
                unless ($sdcorr_matrices{$thisid}->{'bounded'} == (length($transform)==0)){
                    #ambigous
                    $sdcorr_matrices{$thisid}->{'bounded'} = -1;
                }
            }else{
                $sdcorr_matrices{$thisid}->{'bounded'} = (length($transform)==0)? 1 : 0;
            }
        }
    }
    for (my $i=0; $i< scalar(@{$fix_theta_labels}); $i++){
        $transform = undef;
        if($fix_theta_labels->[$i] =~ /^(|log )SD_([A-Z]+)(\d?\d)$/){
            $transform = $1;
            $thisid = $2;
            $row=$3;
            $row =~ s/^0//;
            $sdcorr_fix{$thisid}->{'sd'}->{$row} = $i;
            if ((not defined $dimension{$thisid}) or
                ($dimension{$thisid} < $row)){
                $dimension{$thisid} = $row;
            }
        }elsif($fix_theta_labels->[$i] =~ /^(|logit \()COR_([A-Z]+)(\d?\d)(\d?\d)\b/){
            $transform = $1;
            $thisid = $2;
            $row=$3;
            $col=$4;
            $row =~ s/^0//;
            $col =~ s/^0//;
            $sdcorr_fix{$thisid}->{'corr'}->{$row}->{$col} = $i;
            if ((not defined $dimension{$thisid}) or
                ($dimension{$thisid} < $row)){
                $dimension{$thisid} = $row;
            }
        }
        if (defined $transform){
            if (defined $sdcorr_matrices{$thisid}->{'bounded'}){
                unless ($sdcorr_matrices{$thisid}->{'bounded'} == (length($transform)==0)){
                    #ambigous
                    $sdcorr_matrices{$thisid}->{'bounded'} = -1;
                }
            }else{
                $sdcorr_matrices{$thisid}->{'bounded'} = (length($transform)==0) ? 1 : 0;
            }
        }
    }

    my @finals;
    my $ind;
    foreach my $id (sort {lc($a) cmp lc($b)} keys %dimension){
        if ((not defined $sdcorr_matrices{$id}->{'bounded'}) or
            ($sdcorr_matrices{$id}->{'bounded'} == -1)){
            next; #if ambigous
        }
        if ((not defined $sdcorr_fix{$id}->{'corr'}) and
            (not defined $sdcorr_matrices{$id}->{'corr'})){
            #if no off-diagonal
            next;
        }
        if ((not defined $sdcorr_matrices{$id}->{'sd'}) and
            (not defined $sdcorr_matrices{$id}->{'corr'})){
            next; #if none estimated
        }
        push(@finals,{'size'=> $dimension{$id}, 'indices'=> [], 'bounded' => $sdcorr_matrices{$id}->{'bounded'}});
        for (my $row=1; $row <= $dimension{$id}; $row++){
            for (my $col=1; $col<$row; $col++){
                if (defined $sdcorr_matrices{$id}->{'corr'}->{$row}->{$col}){
                    $ind = $sdcorr_matrices{$id}->{'corr'}->{$row}->{$col};
                }elsif(defined $sdcorr_fix{$id}->{'corr'}->{$row}->{$col}){
                    $ind = (-1+(-1*($sdcorr_fix{$id}->{'corr'}->{$row}->{$col})));
                }else{
                    croak("Failed setup of cholesky reparameterization check for block $id, no CORR $row $col");
                }
                push(@{$finals[-1]->{'indices'}},$ind);
            }
            if (defined $sdcorr_matrices{$id}->{'sd'}->{$row}){
                $ind = $sdcorr_matrices{$id}->{'sd'}->{$row};
            }elsif(defined $sdcorr_fix{$id}->{'sd'}->{$row}){
                $ind = (-1+(-1*($sdcorr_fix{$id}->{'sd'}->{$row})));
            }else{
                croak("Failed setup of cholesky reparameterization check for block $id, no SD $row");
            }
            push(@{$finals[-1]->{'indices'}},$ind);
        }
    }
    return \@finals;
}

sub setup_block_posdef_check
{
    my %parm = validated_hash(\@_,
                              block_number => { isa => 'ArrayRef', optional => 0 },
                              choleskyform =>{isa => 'ArrayRef', optional => 0},
                              coords => { isa => 'ArrayRef', optional => 0 },
                              param => { isa => 'ArrayRef', optional => 0 },
        );
    my $block_number = $parm{'block_number'};
    my $choleskyform = $parm{'choleskyform'};
    my $coords = $parm{'coords'};
    my $param = $parm{'param'};

    my $dim = scalar(@{$coords});

    unless ($dim>0){
        croak("Input error setup_block_posdef_check: coords has dimension $dim");
    }
    unless (scalar(@{$block_number})==$dim){
        croak("Input error setup_block_posdef_check: sim $dim block_number has dimension ".scalar(@{$block_number}));
    }
    unless (scalar(@{$param})==$dim){
        croak("Input error setup_block_posdef_check: sim $dim param has dimension ".scalar(@{$param}));
    }

    my @blockarrays=();

    my @thisarray=();
    my $any_offdiag=0;
    my $blockstart=0;
    my $prev_localrow = 0;
    my $prev_localcol = 0;
    my $prev_globalrow = 0;
    my $prev_globalcol = 0;

    for (my $i=0; $i< $dim; $i++){
        #does this i belong to a block we should care about?
        if (($block_number->[$i] == 0) or ($choleskyform->[$i] == 1)){
            #not interesting block
            next;
        }else{
            #this is part of block
            my ($row,$col) = split(',',$coords->[$i]);
            #should be defined, otherwise complain
            unless ((defined $row) and (defined $col)){
                croak("bug in setup_block_posdef split, coords ".$coords->[$i]);
            }
            if (scalar(@thisarray)==0){
                #this is first i of a block, should be diag, otherwise complain
                unless ($row == $col){
                    croak("bug in setup_block_posdef first element, coords ".$coords->[$i]);
                }
                $prev_localcol = 1;
                $prev_localrow = 1;
                $blockstart = $row;
            }else{
                #if not first, then check if offdiag
                if ($row != $col){
                    $any_offdiag = 1;
                }
                unless ($prev_localrow > 0 and ($prev_globalrow > 0)){
                    croak("bug setup_block_posdef, prev_localrow $prev_localrow coords ".$coords->[$i]);
                }
                unless ($prev_localcol > 0 and ($prev_globalcol > 0)){
                    croak("bug setup_block_posdef, prev_localcol $prev_localcol coords ".$coords->[$i]);
                }
                #is this the next expected entry, or do we pad with zeros for band matrix?
                my $this_localrow=$row-$blockstart+1;
                my $this_localcol=$col-$blockstart+1;

                my $expected_localcol;
                my $expected_localrow;
                if ($prev_localcol == $prev_localrow){
                    $expected_localcol = 1;
                    $expected_localrow = $prev_localrow+1;
                }else{
                    $expected_localcol = $prev_localcol+1;
                    $expected_localrow = $prev_localrow;
                }
                unless ($this_localrow == $expected_localrow){
                    croak("bug setup_block_posdef this_localrow $this_localrow expected $expected_localrow coords ".$coords->[$i]);
                }

                #loop from first expected up to one before the one we actually have
                #and add any we do not have, can be empty loop
                for (my $j=$expected_localcol; $j< $this_localcol; $j++){
                    push(@thisarray,-1);
                }
                $prev_localcol = $this_localcol;
                $prev_localrow = $this_localrow;
            }
            #finally add the one we have
            $prev_globalcol = $col;
            $prev_globalrow = $row;
            push(@thisarray,$i);

            #if this is last i of a block
            if ( ($i==($dim-1)) or
                 ($block_number->[$i] != $block_number->[$i+1]) or
                 ($param->[$i] ne $param->[$i+1])
                ){
                unless ($prev_localrow == $prev_localcol){
                    croak("last item in block but not diagonal $prev_localrow , $prev_localcol coords ".$coords->[$i]);
                }

                #only add block if have any offdiag
                if ($any_offdiag){
                    push(@blockarrays,{'size' => $prev_localrow, 'indices'=> [@thisarray]});
                }
                #reset stuff
                $any_offdiag = 0;
                $prev_localrow = 0;
                $prev_localcol = 0;
                $prev_globalrow = 0;
                $prev_globalcol = 0;
                @thisarray = ();
            }

        }
    }

    return \@blockarrays;

}

sub sample_multivariate_normal
{
    my %parm = validated_hash(\@_,
                              samples => { isa => 'Int', optional => 0 },
                              adjust_blocks => { isa => 'Bool', optional => 0},
                              minEigen => { isa => 'Num', optional => 1, default => 1E-10 },
                              print_summary => { isa => 'Bool', default => 1},
                              check_cholesky_reparameterization => { isa => 'Bool', optional => 0},
                              fix_theta_labels => { isa => 'ArrayRef', optional => 0 },
                              fix_theta_values => { isa => 'ArrayRef', optional => 0 },
                              covmatrix => { isa => 'ArrayRef[ArrayRef]', optional => 0 }, #required for multnorm
                              labels => { isa => 'ArrayRef', optional => 0 },
                              lower_bound => { isa => 'ArrayRef', optional => 0 },
                              upper_bound => { isa => 'ArrayRef', optional => 0 },
                              param => { isa => 'ArrayRef', optional => 0 },
                              block_number => { isa => 'ArrayRef', optional => 0 },
                              choleskyform => { isa => 'ArrayRef', optional => 0 },
                              coords => { isa => 'ArrayRef', optional => 0 },
                              mu => { isa => 'Math::MatrixReal', optional => 0 }, #required for multnorm
                              inflation => { isa => 'ArrayRef', optional => 0 }, #required for multnorm
                              lambda => { isa => 'ArrayRef', optional => 1 }, #not allowed for uniform?
                              delta => { isa => 'ArrayRef', optional => 1 }, #not allowed for uniform?
                              fast_posdef_checks => { isa => 'Bool', default => 0, optional => 1 },
                              print_iter_N => { isa => 'Int', default => 0, optional => 1 },
                              directory => { isa => 'Str', optional => 1 },     # Directory for log file
        );
    my $samples = $parm{'samples'};
    my $adjust_blocks = $parm{'adjust_blocks'};
    my $minEigen = $parm{'minEigen'};
    my $print_summary = $parm{'print_summary'};
    my $check_cholesky_reparameterization = $parm{'check_cholesky_reparameterization'};
    my $covmatrix = $parm{'covmatrix'};
    my $labels = $parm{'labels'};
    my $fix_theta_labels = $parm{'fix_theta_labels'};
    my $fix_theta_values = $parm{'fix_theta_values'};
    my $lower_bound = $parm{'lower_bound'};
    my $upper_bound = $parm{'upper_bound'};
    my $param = $parm{'param'};
    my $block_number = $parm{'block_number'};
    my $choleskyform = $parm{'choleskyform'};
    my $coords = $parm{'coords'};
    my $mu = $parm{'mu'};
    my $inflation = $parm{'inflation'};
    my $lambda = $parm{'lambda'};
    my $delta = $parm{'delta'};
    my $fast_posdef_checks = $parm{'fast_posdef_checks'};
    my $print = $parm{'print_iter_N'};
    my $directory = $parm{'directory'};

    my $dim = scalar(@{$coords});

    unless ($dim>0){
        croak("Input error sample_multivariate_normal: coords has dimension $dim");
    }

    my $transform=0;
    if (defined $lambda){
        if (scalar(@{$lambda})>0){
            unless(scalar(@{$lambda})==$dim){
                croak("Input error sample_multivariate_normal: coords vector has dimension $dim but lambda has dimension ".scalar(@{$lambda}));
            }
            unless(scalar(@{$delta})==$dim){
                croak("Input error sample_multivariate_normal: coords vector has dimension $dim but delta has dimension ".scalar(@{$delta}));
            }
            $transform=1;
        }
    }
    unless (scalar(@{$lower_bound})==$dim){
        croak("Input error sample_multivariate_normal: coords vector has dimension $dim but lower_bound has dimension ".scalar(@{$lower_bound}));
    }
    unless (scalar(@{$upper_bound})==$dim){
        croak("Input error sample_multivariate_normal: coords vector has dimension $dim but upper_bound has dimension ".scalar(@{$upper_bound}));
    }
    unless (scalar(@{$block_number})==$dim){
        croak("Input error sample_multivariate_normal: coords vector has dimension $dim but block_number has dimension ".scalar(@{$block_number}));
    }
    unless (scalar(@{$param})==$dim){
        croak("Input error sample_multivariate_normal: coords vector has dimension $dim but param has dimension ".scalar(@{$param}));
    }
    unless (scalar($samples)>0){
        croak("Input error sample_multivariate_normal: samples must be larger than 0");
    }

    my @muvec=();
    my $use_covmatrix;
    unless (defined $covmatrix ){
        croak("Input error sample_multivariate_normal: covmatrix is required for multivariate normal");
    }
    unless (defined $mu ){
        croak("Input error sample_multivariate_normal: mu is required for multivariate normal");
    }
    unless (defined $inflation){
        croak("inflation parameter is required for multivariate normal");
    }

    my ($rows,$columns) = $mu->dim();
    unless ($rows == 1 and ($columns == $dim)){
        croak("Input error sample_multivariate_normal: mu vector has dimension ($rows,$columns)");
    }
    for (my $i=1; $i<= $dim; $i++){
        push(@muvec,$mu->element(1,$i));
    }

    unless (scalar(@{$covmatrix})==$dim){
        croak("Input error sample_multivariate_normal: coords vector has dimension $dim but covmatrix has dimension ".scalar(@{$covmatrix}));
    }
    unless (scalar(@{$covmatrix->[0]})==$dim){
        croak("Input error sample_multivariate_normal: covmatrix is not square ");
    }
    if (scalar(@{$inflation}) != 0){
        $use_covmatrix = inflate_covmatrix(matrix => $covmatrix,
                                           inflation => $inflation);
    }else{
        $use_covmatrix = $covmatrix;
    }

    my $block_check_array = setup_block_posdef_check(block_number => $block_number,
                                                     choleskyform => $choleskyform,
                                                     coords => $coords,
                                                     param => $param);

    my $cholesky_block_array;
    if ($check_cholesky_reparameterization){
        $cholesky_block_array = setup_auto_cholesky_block_posdef_check(
            param=> $param,
            labels => $labels,
            fix_theta_labels => $fix_theta_labels);
    }

    my %rejections;
    $rejections{'lower_bound'}=[(0) x $dim];
    $rejections{'upper_bound'}=[(0) x $dim];
    $rejections{'block'}=0;
    $rejections{'reparameterized_block'}=0;
    $rejections{'boxcox'}=[(0) x $dim];
    $rejections{'zero'}=[(0) x $dim];

    if ($fast_posdef_checks) {
        print "Using fast cholesky decomposition for positive semi-definiteness checks\n";
    } else {
        print "Using full eigenvalue decomposition for positive semi-definiteness checks\n";
    }

    my @samples_array=();
    my @boxcox_samples_array=();
    my $counter=0;
    my $max_iter=1000;
    my $half_iter = 500;
    if ($print>0) {
        print "Starting $max_iter iterations to get $samples samples";
        if ($adjust_blocks) {
             print " (forcing all posdef)\n";
         } else {
             print " (forcing all posdef at Iter $half_iter)\n";
         }
    }
    my $discarded=0;
    my $adjusted = 0;
    my $this_adjusted=0;
    my $discarded_adjusted = 0;
    for (my $j=0; $j<$max_iter; $j++){
        #we will probably discard some samples, generate twice needed amount to start with
        my @candidate_samples;
        @candidate_samples = random_multivariate_normal((2*$samples), @muvec, @{$use_covmatrix});

        for (my $cand=0; $cand < scalar(@candidate_samples); $cand++){
            my $xvec;
            my $accept = 1;
            if ($transform){
                #back-transform from boxcox
                $xvec = boxcox::shift_and_box_cox(vector=>$candidate_samples[$cand],
                                                  lambda=>$lambda,
                                                  delta=>$delta,
                                                  inverse => 1);
                for (my $i=0; $i< $dim; $i++){
                    unless (usable_number($xvec->[$i])){
                        $rejections{'boxcox'}->[$i]=$rejections{'boxcox'}->[$i]+1;
                        $accept=0;
                        last;
                    }
                }
            }else{
                $xvec= $candidate_samples[$cand];
            }
            unless ($accept){
                $discarded++;
                next ;
            }

            for (my $i=0; $i< $dim; $i++){
                if ($xvec->[$i] <= $lower_bound->[$i]){
                    $rejections{'lower_bound'}->[$i]=$rejections{'lower_bound'}->[$i]+1;
                    $accept=0;
                    last;
                }elsif($xvec->[$i] >= $upper_bound->[$i]){
                    $rejections{'upper_bound'}->[$i]=$rejections{'upper_bound'}->[$i]+1;
                    $accept=0;
                    last;
                }elsif($xvec->[$i] == 0){
                    #unlikely, but must handle
                    $rejections{'zero'}->[$i]=$rejections{'zero'}->[$i]+1;
                    $accept=0;
                    last;
                }
            }
            unless ($accept){
                $discarded++;
                next ;
            }
            ($accept,$this_adjusted) = check_blocks_posdef(xvec => $xvec,
                                                           hash_array => $block_check_array,
                                                           adjust_blocks => $adjust_blocks,
                                                           cholesky_decomposition => $fast_posdef_checks);
            unless ($accept){
                $rejections{'block'}++;
                $discarded++;
                $discarded_adjusted++ if ($this_adjusted);
                next ;
            }
            if ($check_cholesky_reparameterization){
                $accept = check_auto_cholesky_blocks_posdef(hash_array =>$cholesky_block_array,
                                                            xvec => $xvec,
                                                            fix_xvec => $fix_theta_values);
            }
            unless ($accept){
                $rejections{'reparameterized_block'}++;
                next ;
            }

            $adjusted++ if ($this_adjusted);
            push(@samples_array,$xvec);
            push(@boxcox_samples_array,$candidate_samples[$cand]) if ($transform);
            $counter++;
            last if ($counter == $samples);
        }
        last if ($counter == $samples);
        my $cand_drawn = (2*($j+1)*$samples);
        if ($print > 0 and $j == 0 || (($j+1) % $print == 0)) {
            my $iter = sprintf("Iter%5d", $j+1);

            my $bc_rate = 0;
            my $bound_rate = 0;
            my $np_rate = 0;
            map { $bc_rate += $_ } @{$rejections{'boxcox'}};
            $bc_rate = $bc_rate/$cand_drawn;
            map { $bound_rate += $_ } ( @{$rejections{'lower_bound'}}, @{$rejections{'upper_bound'}}, @{$rejections{'zero'}} );
            $bound_rate = $bound_rate/$cand_drawn;
            $np_rate = ( $rejections{'block'} + $rejections{'reparameterized_block'} )/$cand_drawn;

            my $acc_rate = sprintf("%.1f", ($counter/$cand_drawn)*100);
            my $rej_rate = sprintf("%.1f/%.1f/%.1f", $bc_rate*100, $bound_rate*100, $np_rate*100);
            print "  $iter: $counter/$samples sampled w/ ${acc_rate}% acc. (${rej_rate}% boxcox/bounds/non-posdef rej.)\n";
        } elsif (($j > 0) and (($j+1) % 100 == 0) and ($j<($max_iter-1))){
            ui->print(category=> 'sir',
                      message => "Only have $counter accepted parameter vectors within the boundaries ".
                      "after generating ".$cand_drawn." candidates, drawing more candidates\n");
        }
        if (($j==$half_iter) and (not $adjust_blocks)){
            ui->print(category=> 'sir',
                      message => "Turning on automatic adjustment of OMEGA/SIGMA blocks to increase acceptance rate\n");
            $adjust_blocks = 1;
        }
    }

    my $fname = 'sample_rejection_summary.txt';
    if (defined $directory) {
        $fname = File::Spec->catdir($directory, $fname);
    }
    print_rejections(rejections => \%rejections,
                     labels => $labels,
                     file => $fname,
                     dim => $dim,
                     check_cholesky_reparameterization => $check_cholesky_reparameterization) if ($print_summary);

    unless ($counter == $samples){
        ui->print(category => 'sir',message=>"Failed to generate $samples accepted parameter vectors within the boundaries ".
                  "even after generating ".
                  (2*$max_iter*$samples)." candidates, only have $counter accepted.\n".
                  "See summary of causes of rejections in file $fname.\n");
        croak("Cannot proceed with sir\n");
    }
    my $extra = '';
    if ($adjusted > 0){
        $extra = "and adjusted $adjusted samples to make blocks positive definite\n";
    }
    ui->print(category => 'sir',message=> "Redrew $discarded samples that did not fulfill conditions\n".
              "$extra");
    return (\@samples_array,\@boxcox_samples_array);

}

sub print_rejections
{
    my %parm = validated_hash(\@_,
                              rejections => {isa => 'HashRef', optional => 0},
                              labels => {isa => 'ArrayRef', optional => 0},
                              file => {isa => 'Str', optional => 0},
                              dim => {isa => 'Int', optional => 0},
                              check_cholesky_reparameterization => {isa => 'Bool', optional => 0},
        );

    my $rejections = $parm{'rejections'};
    my $labels = $parm{'labels'};
    my $file = $parm{'file'};
    my $dim = $parm{'dim'};
    my $check_cholesky_reparameterization = $parm{'check_cholesky_reparameterization'};

    open ( RES, ">" .$file); #overwrite
    print RES "Sample rejection summary:\n";
    print RES "BoxCox LowerBound UpperBound Zero  Label\n";
    for (my $i=0; $i< $dim; $i++){
        my $line=sprintf("%6i %10i %9i %5i  ",
                         $rejections->{'boxcox'}->[$i],
                         $rejections->{'lower_bound'}->[$i],
                         $rejections->{'upper_bound'}->[$i],
                         $rejections->{'zero'}->[$i]).
                         $labels->[$i]."\n";
        print RES $line;
    }
    print RES "OMEGA/SIGMA blocks nonposdef rejections: ".$rejections->{'block'}."\n";
    if ($check_cholesky_reparameterization){
        print RES "Cholesky reparameterized blocks nonposdef rejections: ".
            $rejections->{'reparameterized_block'}."\n";
    }
    close RES;
}

sub check_blocks_posdef
{
    #static, no shift

    # Check multiple matrices (blocks) for positive definiteness and adjust them (if desired).
    # Blocks are triangular and consecutively contained within xvec, indexed by hash_array.
    # E.g. for identity 2x2 and 3x3 interspaced by a single (ignored) zero:
    # xvec = [1,0,1,0,1,0,1,0,0,1]
    # hash_array = [
    #                { 'size' => 2, indices => [1,2,3] },
    #                { 'size' => 3, indices => [5,6,7,8,9,10] },
    #              ]
    #
    # cholesky_decomposition will use cholesky decomposition instead of eigenvalue decomposition
    # to speed up rejection process (but adjustment shall remain unaffected)

    my %parm = validated_hash(\@_,
                              xvec => { isa => 'ArrayRef', optional => 0 },
                              hash_array => {isa => 'ArrayRef', optional => 0},
                              adjust_blocks => {isa => 'Bool', optional => 0},
                              minEigen => {isa => 'Num', optional => 1, default => 1E-09},
                              cholesky_decomposition => { isa => 'Bool', default => 0, optional => 1 },
        );

    my $xvec = $parm{'xvec'};
    my $hash_array = $parm{'hash_array'};
    my $adjust_blocks = $parm{'adjust_blocks'};
    my $minEigen = $parm{'minEigen'};
    my $cholesky_decomposition = $parm{'cholesky_decomposition'};

    my $accept = 1;
    my $adjusted = 0;

    foreach my $hashref (@{$hash_array}){
        my $size = $hashref->{'size'};
        my $mat = [];
        my @xvec_i=();
        for (my $row=0; $row< $size; $row++){
            push(@{$mat},[(0) x $size]);
        }
        my $row=0;
        my $col=0;
        my $band_matrix = 0;
        foreach my $index (@{$hashref->{'indices'}}){
            if ($index >= 0){
                $mat->[$row]->[$col] = $xvec->[$index];
                $mat->[$col]->[$row] = $xvec->[$index];
            }else{
                #already 0
                $band_matrix = 1;
            }
            $col++;
            if($col>$row){
                $row++;
                $col=0;
            }
        }
        my $is_matrix_posdef = 1;
        if ($cholesky_decomposition) {
            # fast pre-check via cholesky decomposition
            # (if fine we don't need to decompose into eigenvalues, saving valuable time)
            $is_matrix_posdef = linear_algebra::is_matrix_posdef(matrix => $mat);
            if (not $is_matrix_posdef and !$adjust_blocks) {
                # if not adjusting we can already reject the blocks (otherwise it might get adjusted below)
                $accept = 0;
                last;
            }
        }
        if (!$cholesky_decomposition or (not $is_matrix_posdef && $adjust_blocks)) {
            (my $eigenvalues, my $Q) = linear_algebra::eigenvalue_decomposition($mat);

            if (linear_algebra::min($eigenvalues) < $minEigen){
                if ($adjust_blocks){
                    my ($newmat,$change) = linear_algebra::spdarise(matrix => $mat,
                                                                    eigenvalues => $eigenvalues,
                                                                    Q => $Q,
                                                                    minEigen => $minEigen);
                    $row=0;
                    $col=0;
                    foreach my $index (@{$hashref->{'indices'}}){
                        if ($index >= 0){
                            $xvec->[$index] = $newmat->[$row]->[$col];
                        }else{
                            #need to verify that 0 is ok
                            $newmat->[$row]->[$col] = 0;
                            $newmat->[$col]->[$row] = 0;
                        }
                        $col++;
                        if($col>$row){
                            $row++;
                            $col=0;
                        }
                    }
                    $adjusted = 1;
                    #if band matrix we need to check that setting 0 was ok
                    if ($band_matrix){
                        if ($cholesky_decomposition) {
                            # save time via fast posdef check here also
                            $is_matrix_posdef = linear_algebra::is_matrix_posdef(matrix => $newmat);
                            if (not $is_matrix_posdef) {
                                $accept = 0;
                            }
                        } else {
                            ($eigenvalues, $Q) = linear_algebra::eigenvalue_decomposition($newmat);
                            if (linear_algebra::min($eigenvalues) < $minEigen){
                                $accept = 0;
                            }
                        }
                    }
                }else{
                    $accept = 0;
                    last;
                }
            }
        }
        last unless ($accept);
    }
    return ($accept,$adjusted);
}

sub tweak_inits_sampling
{
    #static, no shift
    my %parm = validated_hash(\@_,
                              sampled_params_arr => { isa => 'ArrayRef', optional => 0 }, #can be empty
                              parameter_hash => { isa => 'HashRef', optional => 0 },
                              model => {isa => 'model', optional => 0},
                              output => {isa => 'output', optional => 1},
                              degree => {isa => 'Maybe[Num]', optional => 1},
                              filename => {isa => 'Str', optional => 1},
                              directory => {isa => 'Str', optional => 1},
        );
    my $sampled_params_arr = $parm{'sampled_params_arr'};
    my $parameter_hash = $parm{'parameter_hash'};
    my $model = $parm{'model'};
    my $output = $parm{'output'};
    my $degree = $parm{'degree'};
    my $filename = $parm{'filename'};
    my $directory = $parm{'directory'};

    my $default_degree=0.1;

    my @Amatrix=();
    my $nparam = scalar(@{$parameter_hash->{'values'}});

    my $referencemodel = $model->copy( filename => 'referencefortweakinits.mod',
                                  write_copy => 0,
                                  copy_output => 0);
    my $tweakmodel = $model->copy( filename => 'dummyfortweakinits.mod',
                                   write_copy => 0,
                                   copy_output => 0);
    my $minimum_samples = $nparam;
    if (scalar(@{$sampled_params_arr}) == 0){
        $minimum_samples = $nparam+2;
        #we have nothing but estimates of intput model
        push(@Amatrix,$parameter_hash->{'values'});
        unless (defined $output and (defined $degree)){
            croak("tweak_inits_sampling: output and degree required when do not have sampled array");
        }
        $referencemodel->update_inits( from_output => $output,
                                  ensure_posdef => 0,
                                  ignore_missing_parameters => 0,
                                  update_fix => 0);
    }else{
        my @problem_lines=();
        my $linesarray = $referencemodel->problems->[0]->_format_problem(relative_data_path => 1,
                                                                    write_directory => $referencemodel->directory);
        #we cannot use this array directly, must make sure items do not contain line breaks
        foreach my $line (@{$linesarray}){
            my @arr = split(/\n/,$line);
            push(@problem_lines,@arr);
        }
        unless (defined $degree){
            $degree = $default_degree;
        }

        for (my $i=0; $i<scalar(@{$sampled_params_arr}); $i++){
            my ($vector,$errors) = get_vector_from_sampled_params_arr(sampled_params_arr => $sampled_params_arr,
                                                                      parameter_hash => $parameter_hash,
                                                                      index => $i);
            push(@Amatrix,$vector);
            unless ($i==0){
                #add problem and then update inits
                push(@{$referencemodel->problems()},
                     model::problem ->
                     new ( directory                   => $referencemodel->directory,
                           ignore_missing_files        => 1,
                           ignore_missing_output_files => 1,
                           prob_arr                    => \@problem_lines));
            }

            $referencemodel -> update_inits(from_hash => $sampled_params_arr->[$i],
                                            problem_number => ($i+1),
                                            ignore_missing_parameters => 0,
                                            match_labels => 1);
        }
    }

    my $indexmax = (scalar(@{$referencemodel->problems})-1);

    my $done = 0;
    my $full_rank=0;
    $done = $full_rank;
    my $problem_index =0;
    while (not $done){
        my $any = $tweakmodel->problems->[0]->set_random_inits(degree => $degree,
                                                               basic_model => $referencemodel,
                                                               problem_index => $problem_index);
        $problem_index++;
        $problem_index = 0 if ($problem_index > $indexmax);

        $tweakmodel->problems->[0]->reset_estimated_parameters_hash();
        push(@Amatrix,$tweakmodel->problems->[0]->get_estimated_attributes(parameter => 'all',attribute => 'inits'));
        next if (scalar(@Amatrix)<($minimum_samples));
        $done=1 if (scalar(@Amatrix)> (100*$nparam)); #to avoid inf loop
        $full_rank = linear_algebra::full_rank(\@Amatrix);
        $done = 1 if ($full_rank);
    }

    #create  rawres
    if (defined $filename and (defined $directory)){
        open ( RES, ">" . $directory.'/'.$filename );
        print RES 'model,"'.join('","',@{$parameter_hash->{'coordinate_strings'}}).'"'."\n";
        for (my $i=0;$i < scalar(@Amatrix); $i++){
            print RES ($i+1).','.join(',',@{$Amatrix[$i]})."\n";
        }
        close RES;
    }

    return \@Amatrix;
}

sub create_sampled_params_arr
{
    #to be used in update_inits (from_hash=> $sampled_params_arr->[$k],ignore_missing_parameters=>1);
    my %parm = validated_hash(\@_,
                              samples_array => { isa => 'ArrayRef[ArrayRef]', optional => 0 },
                              labels_hash => { isa => 'HashRef', optional => 0 },
                              user_labels => { isa => 'Bool', optional => 0 },
        );
    my $samples_array = $parm{'samples_array'};
    my $labels_hash = $parm{'labels_hash'};
    my $user_labels = $parm{'user_labels'};

    my @allparams=();

    my $labelkey='coordinate_strings';
    if ($user_labels){
        $labelkey='labels';
    }

    foreach my $sample (@{$samples_array}){
        my %allpar;
        $allpar{'theta'} = {};
        $allpar{'omega'} = {};
        $allpar{'sigma'} = {};
        for (my $i=0; $i<scalar(@{$sample}); $i++){
            my $label = $labels_hash->{$labelkey}->[$i];
            my $param = $labels_hash->{'param'}->[$i];
            my $value = $sample->[$i];
            $allpar{$param}->{$label} = $value;
        }
        push (@allparams,\%allpar);
    }

    return \@allparams;
}

sub inflate_covmatrix
{
    my %parm = validated_hash(\@_,
                              matrix => { isa => 'ArrayRef[ArrayRef]', optional => 0 },
                              inflation => { isa => 'ArrayRef', optional => 0 }
        );
    my $matrix = $parm{'matrix'};
    my $inflation = $parm{'inflation'};

    my $dim = scalar(@{$matrix});
    unless ($dim > 0){
        croak("Input error inflate_covmatrix: dimension is 0 ");
    }
    unless (scalar(@{$matrix->[0]})==$dim){
        croak("Input error inflate_covmatrix: covmatrix is not square ");
    }
    unless (scalar(@{$inflation}) == $dim){
        croak("Input error inflate_covmatrix: inflation must have length equal to matrix dim $dim");
    }
    my @rootinfl = ();
    foreach my $val (@{$inflation}){
        croak("Input error inflate_covmatrix: inflation must be positive") unless ($val > 0);
        push(@rootinfl,sqrt($val));
    }

    my @copy=();
    for (my $i=0;$i< $dim; $i++){
        push(@copy,[0 x $dim]);
        for (my $j=0;$j< $i; $j++){
            $copy[$i]->[$j] = $copy[$j]->[$i];
        }
        for (my $j=$i;$j< $dim; $j++){
            $copy[$i]->[$j] = ($matrix->[$i]->[$j])*$rootinfl[$i]*$rootinfl[$j];
        }
    }
    return \@copy;
}

sub inflate_covmatrix_diagonal
{
    my %parm = validated_hash(\@_,
                              matrix => { isa => 'ArrayRef[ArrayRef]', optional => 0 },
                              inflation => { isa => 'ArrayRef', optional => 0 }
        );
    my $matrix = $parm{'matrix'};
    my $inflation = $parm{'inflation'};

    my $dim = scalar(@{$matrix});
    unless ($dim > 0){
        croak("Input error inflate_covmatrix_diagonal: dimension is 0 ");
    }
    unless (scalar(@{$matrix->[0]})==$dim){
        croak("Input error inflate_covmatrix_diagonal: covmatrix is not square ");
    }
    unless (scalar(@{$inflation}) == $dim){
        croak("Input error inflate_covmatrix_diagonal: inflation must have length equal to matrix dim $dim");
    }

    for (my $i=0;$i< $dim; $i++){
        my $val = $inflation->[$i];
        croak("Input error inflate_covmatrix_diagonal: inflation must be positive") unless ($val > 0);
        $matrix->[$i]->[$i] = $val*($matrix->[$i]->[$i]);
    }
}

sub get_nonmem_covmatrix
{
    my %parm = validated_hash(\@_,
        output => { isa => 'output', optional => 0 }
    );
    my $output = $parm{'output'};

    unless ($output->have_output){
        croak("Trying get_nonmem_covmatrix but output object is empty, output file\n".$output->full_name."\n");
    }
    unless( $output -> parsed_successfully ){
        croak("Trying get_nonmem_covmatrix but unable to read everything from outputfile, parser error message:\n".
              $output -> parsing_error_message());
    }
    unless ($output-> get_single_value(attribute => 'covariance_step_run')){
        croak("Trying get_nonmem_covmatrix but the covariance step was not run");
    }
    unless ($output-> get_single_value(attribute => 'covariance_step_successful')){
        croak("Trying get_nonmem_covmatrix but the covariance step was not successful");
    }
    if ($output-> get_single_value(attribute => 'covariance_step_warnings')){
        ui -> print( category => 'sir',
                     message  => "Warning: Doing get_nonmem_covmatrix but there were covariance step warnings in the lst-file. This is ".
                     " likely to give errors in the computation of weights");
    }

    my $lower_covar  = $output -> get_single_value(attribute => 'covariance_matrix');

    unless (defined $lower_covar){
        croak("Trying get_nonmem_covmatrix but the covariance matrix is undefined. Parsing error? Output file is\n".$output->full_name."\n");
    }

    my $covar = output::problem::subproblem::make_square( $lower_covar);
    return $covar;
}

sub get_nonmem_inverse_covmatrix
{

    my %parm = validated_hash(\@_,
        output => { isa => 'output', optional => 0 }
    );
    my $output = $parm{'output'};

    unless ($output->have_output){
        croak("Trying get_nonmem_inverse_covmatrix but output object is empty, output file\n".$output->full_name."\n");
    }
    unless( $output -> parsed_successfully ){
        croak("Trying get_nonmem_inverse_covmatrix but unable to read everything from outputfile, parser error message:\n".
              $output -> parsing_error_message());
    }
    unless ($output-> get_single_value(attribute => 'covariance_step_run')){
        croak("Trying get_nonmem_inverse_covmatrix but the covariance step was not run");
    }
    unless ($output-> get_single_value(attribute => 'covariance_step_successful')){
        croak("Trying get_nonmem_inverse_covmatrix but the covariance step was not successful");
    }
    if ($output-> get_single_value(attribute => 'covariance_step_warnings')){
        ui -> print( category => 'sir',
                     message  => "Warning: Doing get_nonmem_inverse_covmatrix but there were covariance step warnings in the lst-file. This is ".
                     " likely to give errors in the SIR sampling");
    }

    my $lower_icm  = $output -> get_single_value(attribute => 'inverse_covariance_matrix');

    unless (defined $lower_icm){
        croak("Trying get_nonmem_inverse_covmatrix but the matrix is undefined. Parsing error? Output file is\n".
              $output->full_name."\n");
    }
    my $icm = Math::MatrixReal -> new_from_cols(output::problem::subproblem::make_square( $lower_icm));
    return $icm;
}

sub modelfit_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    if (defined $self->tools and defined $self->tools()->[0]){
        $self->full_rawres_header($self->tools()->[0]->raw_results_header);
    }

    my @restartseed = random_get_seed;
    save_restart_information( parameter_hash => $self->parameter_hash,
                              nm_version  => $self->nm_version,
                              model_filename => $self->models->[0]->filename,
                              done  => 1,
                              subjects => $self->subjects,
                              recenter  => $self->recenter,
                              adjust_blocks  => $self->adjust_blocks,
                              check_cholesky_reparameterization => $self->check_cholesky_reparameterization,
                              copy_data => $self->copy_data,
                              boxcox => $self->boxcox,
                              center_rawresults_vector => $self->center_rawresults_vector,
                              with_replacement => $self->with_replacement,
                              cap_resampling => $self->cap_resampling,
                              cap_correlation => $self->cap_correlation,
                              iteration  => $self->iteration,
                              mceta  => $self->mceta,
                              problems_per_file  => $self->problems_per_file,
                              reference_ofv  => $self->reference_ofv,
                              minimum_ofv => $self->minimum_ofv,
                              negative_dofv => $self->negative_dofv,
                              samples => $self->samples,
                              resamples => $self->resamples,
                              attempted_samples => $self->attempted_samples,
                              successful_samples => $self->successful_samples,
                              actual_resamples => $self->actual_resamples,
                              intermediate_raw_results_files => $self->intermediate_raw_results_files,
                              seed_array => \@restartseed
        );

}

sub _modelfit_raw_results_callback
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              model_number => { isa => 'Int', optional => 0 }
        );
    my $model_number = $parm{'model_number'};
    my $subroutine;

    my ($dir,$file,$nonp_file);

    my $reference_ofv = $self->reference_ofv();
    my $pdf_vector = $self->pdf_vector();
    my $attempted_samples = $self->attempted_samples()->[($self->iteration-1)];

    my $iteration = $self->iteration;
    my $final_iteration = 0;
    $final_iteration = 1 if ($self->iteration == $self->max_iteration);
    ($dir,$file)=
        OSspecific::absolute_path( $self ->directory(),
                                   $self -> raw_results_file()->[$model_number-1] );
    ($dir,$nonp_file) =
        OSspecific::absolute_path( $self ->directory(),
                                   $self -> raw_nonp_file()->[$model_number-1] );

    my $with_replacement = $self->with_replacement();
    my $problems_per_file = $self->problems_per_file();

    my $orig_mod = $self ->models()->[$model_number-1];


    $subroutine = sub {
        my $modelfit = shift;
        my $mh_ref   = shift;
        my %max_hash = %{$mh_ref};
        if ($final_iteration){
            $modelfit -> raw_results_file([$dir.$file] );
            $modelfit -> raw_nonp_file( [$dir.$nonp_file] );
        }else{
            #leave as is
        }
#        $self->raw_line_structure($modelfit -> raw_line_structure());

        #make sure that we have valid raw_line_structure and not from crashed run here
        my $structure;
        for (my $i=1;$i <= scalar(keys %{$modelfit->raw_line_structure()}); $i++){
            if (defined $modelfit->raw_line_structure()->{$i} and defined $modelfit->raw_line_structure()->{$i}->{'ofv'}){
                $structure = $i;
                last;
            }else{
                #print "rawline $i not ok!\n";
            }
        }
        croak("could not find defined ofv in raw_line_structure") unless (defined $structure);

        my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$structure}->{'model'});
        my $modelindex = $start;
        ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$structure}->{'problem'});
        my $probindex = $start;
        ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$structure}->{'subproblem'});
        my $subindex = $start;
        ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$structure}->{'ofv'});
        my $ofvindex=$start;
        croak("could not find ofv in raw results header") unless (defined $ofvindex);



        my @delta_ofv=();
        my @filtered_pdf=();
        my @sample_id=();
        my $index = 0;
        my $successful_count=0;
        my $negative_count = 0;
        my $this_minimum_ofv = 1000000;
        my @minimum_ofv_row=();
        #new filtered_pdf for rows actually in raw_results_file

        foreach my $row ( @{$modelfit -> raw_results()} ) {
            #compute index in original pdf vector based on model and problem column
            #index is (model-1)*problems_per_file + (problem-1)
            #push this pdf to filtered_pdf
            my $delta_ofv;
            if (defined $row->[$ofvindex]){
                $delta_ofv = $row->[$ofvindex] - $reference_ofv;
                $successful_count++;
                if ($delta_ofv < 0){
                    $negative_count++;
                }
                if ($row->[$ofvindex] < $this_minimum_ofv){
                    $this_minimum_ofv = $row->[$ofvindex];
                    @minimum_ofv_row = @{$row};
                }
            }else{
                $delta_ofv = undef;
            }
            my $pdf;
            if (defined $row->[$modelindex] and defined $row->[$probindex] and
                ($row->[$modelindex]>0) and  ($row->[$probindex]>0)){
                my $tmp =($row->[$modelindex]-1)*$problems_per_file + ($row->[$probindex]-1);
                if ($tmp > scalar(@{$pdf_vector})){
                    croak("computed index $tmp based on model ".$row->[$modelindex]." problem ".$row->[$probindex].
                          " and problems_per_file $problems_per_file, ".
                          " but pdf_vector has length ".scalar(@{$pdf_vector}));
                }
                $pdf = $pdf_vector->[$tmp];
            }else{
                $pdf = undef;
            }
            push(@filtered_pdf,$pdf);
            push(@delta_ofv,$delta_ofv);
            $index++;
            push(@sample_id,$index); #starts at 1
        }

        push(@{$self->minimum_ofv},$this_minimum_ofv);
        push(@{$self->negative_dofv},$negative_count);
        my $current_resamples = tool::sir::update_actual_resamples(samples=>$self->samples,
                                                                   resamples=>$self->resamples,
                                                                   successful_samples=>$self->successful_samples,
                                                                   actual_resamples=>$self->actual_resamples,
                                                                   successful_count=>$successful_count,
                                                                   iteration=> $iteration);

        if ($successful_count == $attempted_samples){
            #check that original pdf and filtered_pdf are the same
            my $mismatch=0;
            for (my $j=0; $j < $attempted_samples; $j++){
                unless ($filtered_pdf[$j] == $pdf_vector->[$j]){
                    $mismatch++;
                    ui->print(category => 'sir',
                              message => "mismatch in pdf $j, filtered is ".$filtered_pdf[$j].
                              " but original is ".$pdf_vector->[$j]);
                }
            }
#            print "mismatch count $mismatch ok $ok\n";
        }else{
            unless ($with_replacement){
                #almost always true
                my $original_resamples = $self->resamples->[$iteration-1];
                unless ($current_resamples == $original_resamples){
                    my $message;
                    if ($current_resamples < $original_resamples){
                        $message = "\nOnly $successful_count samples gave an ofv-value.\n".
                              "Some \$PROBLEMs must have terminated with error, check lst-files in\n".
                              $self->directory."m1\n";
                    }else{
                        $message = "\n $successful_count samples gave an ofv-value.\n".
                            "This is more than expected given previous losses\n";
                    }
                    $message .= "Resetting resamples from $original_resamples to $current_resamples in this iteration so ".
                        "that the samples/resamples ratio set on commandline is maintained ";
                    ui->print(category => 'sir', message => $message);
                }

            }
        }

        if ($self->cap_resampling > 1){
            my $hashref = tool::sir::augment_rawresults( raw_results => $modelfit->raw_results,
                                                         filtered_pdf => \@filtered_pdf,
                                                         dofv_array => \@delta_ofv,
                                                         id_array => \@sample_id,
                                                         cap_resampling => $self->cap_resampling);
            $modelfit -> raw_results($hashref->{'raw'});
            @filtered_pdf = @{$hashref->{'pdf'}};
            @delta_ofv = @{$hashref->{'dofv'}};
            @sample_id = @{$hashref->{'id'}};
        }


        my $wghash = tool::sir::compute_weights(pdf_array => \@filtered_pdf,
                                                dofv_array => \@delta_ofv);

        my @original_weights = @{$wghash->{'weights'}};
        my $total_weights = $wghash->{'sum_weights'};
        my @times_sampled = (0) x scalar(@delta_ofv); #this is the filtered_samples length
        my @sample_order = (0) x scalar(@delta_ofv);

        ui -> print( category => 'sir', message => "Resampling vectors based on weights" );

        my $ok_resamples = do_resampling(times_sampled => \@times_sampled,
                                         sample_order => \@sample_order,
                                         current_resamples => $current_resamples,
                                         wghash => $wghash,
                                         with_replacement => $with_replacement);

        unless ($ok_resamples == $current_resamples){
            #reset current_resamples to what was possible, reset total weights
            ui -> print( category => 'sir', message => "\n Error: After resampling $ok_resamples vectors the probability".
                         " of resampling is 0 for all remaining vectors. Stopping resampling." );
            $ok_resamples = 1 if ($ok_resamples == 0); #avoid div by zero below
            $self->actual_resamples->[-1] = $ok_resamples;
            $total_weights = 1 if ($total_weights == 0); #to avoid division by 0 when printing results
        }


        my @extra_headers = ('deltaofv','likelihood_ratio','relPDF','importance_ratio','probability_resample','resamples','sample_order');
        $index=0;
        foreach my $row ( @{$modelfit -> raw_results()} ) {
            my @oldrow =@{$row};
            $row = [$sample_id[$index],
                    @oldrow[0 .. $ofvindex],
                    $delta_ofv[$index],
                    ((defined $delta_ofv[$index]) ? exp(-0.5*$delta_ofv[$index]): undef), #likelihood_ratio
                    $filtered_pdf[$index],
                    $original_weights[$index], #importance_ratio
                    $original_weights[$index]/$total_weights, #probability_resample
                    $times_sampled[$index],
                    $sample_order[$index],
                    @oldrow[$ofvindex+1 .. $#oldrow]];
            $index++;
        }

        # The prepare_raw_results in the modelfit will fix the
        # raw_results for each maxev0 model, we must add
        # the result for the center model.

        if ($iteration == 1){
            my %dummy;
            my ($raw_results_row, $nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
                                                                                  model => $orig_mod,
                                                                                  raw_line_structure => \%dummy );
            $orig_mod -> outputs -> [0] -> flush;
            $raw_results_row->[0]->[0] = 'input'; #model column #FIXME add to restart info
            $self->center_rawresults_vector($raw_results_row->[0]);
        }
        my @oldrow =@{$self->center_rawresults_vector};

        #sample.id                      'deltaofv','likelihood_ratio','relPDF','importance_ratio','probability_resample','resamples','sample_order'
        my $row = [0,@oldrow[0 .. $ofvindex],0,undef,undef,undef,undef,undef,undef,@oldrow[$ofvindex+1 .. $#oldrow]];

        unshift( @{$modelfit -> raw_results()}, @{[$row]} );

        #recenter for next iteration
        if (($negative_count > 0) and $self->recenter){
            $minimum_ofv_row[0]='center';
            $self->center_rawresults_vector(\@minimum_ofv_row);
        }

        #fix the header

        my @old_header = @{$modelfit -> raw_results_header()};
        my $headerindex;
        for (my $k=0; $k<scalar(@old_header);$k++){
            $headerindex = $k if ($old_header[$k] eq 'ofv');
        }
        $modelfit -> raw_results_header(
            ['sample.id',@old_header[0 .. $headerindex],@extra_headers,@old_header[$headerindex+1 .. $#old_header]]);

        my $extra_header_count = scalar(@extra_headers);
        foreach my $mod (sort({$a <=> $b} keys %{$modelfit->raw_line_structure()})){
            foreach my $category (keys %{$modelfit->raw_line_structure() -> {$mod}}){
                next if ($category eq 'line_numbers');
                my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$mod}->{$category});
                if ($start > $ofvindex){
                    $modelfit->raw_line_structure() -> {$mod}->{$category} = ($start+$extra_header_count+1).','.$len;#+1 extra for sample.id
                }else{
                    $modelfit->raw_line_structure() -> {$mod}->{$category} = ($start+1).','.$len; #+1 for sample id
                }
            }
            my $hc = 0;
            $modelfit->raw_line_structure() -> {$mod}->{'sample.id'} = ($hc).',1'; #sample has index 0
            $hc++;
            foreach my $head (@extra_headers){
                $hc++; #first will be 2
                $modelfit->raw_line_structure() -> {$mod}->{$head} = ($ofvindex+$hc).',1'; #first has 2 extra relative old ofvindex
                #    $modelfit->raw_line_structure() -> {$mod}->{'deltaofv'} = ($ofvindex+1).',1';
            }
        }

        if ($negative_count > 0) {
            my $message ="\nFound ".$negative_count.
                " samples with negative delta-ofv, this means that ".
                "the model was not at the global minimum. Details in raw results file\n".
                $modelfit -> raw_results_file->[0];
            ui->print(category => 'sir', message => $message);
        }
        if ($final_iteration){
            $self->raw_line_structure($modelfit -> raw_line_structure());
            $self->raw_line_structure() -> write( $dir.'raw_results_structure' );

            $self -> raw_results_header($modelfit -> raw_results_header());
            $self -> raw_results($modelfit -> raw_results());
        }else{
            my ($dir,$file) =     OSspecific::absolute_path( $modelfit ->directory(),
                                                           $modelfit -> raw_results_file()->[$model_number-1] );
            $modelfit->raw_line_structure() -> write( $dir.'raw_results_structure' );

        }
    };
    return $subroutine;
}

sub augment_rawresults
{
    #static
    my %parm = validated_hash(\@_,
                              raw_results => { isa => 'ArrayRef', optional => 0 },
                              filtered_pdf => { isa => 'ArrayRef', optional => 0 },
                              dofv_array => { isa => 'ArrayRef', optional => 0 },
                              id_array => { isa => 'ArrayRef', optional => 0 },
                              cap_resampling => { isa => 'Int', optional => 0 },
        );
    my $raw_results = $parm{'raw_results'};
    my $filtered_pdf = $parm{'filtered_pdf'};
    my $dofv_array = $parm{'dofv_array'};
    my $id_array = $parm{'id_array'};
    my $cap_resampling = $parm{'cap_resampling'};

    my @augmented_raw=();
    my @augmented_pdf=();
    my @augmented_dofv=();
    my @augmented_id=();

    my $count = scalar(@{$raw_results});
    for (my $i=0; $i< $count ; $i++){
        my $max = 1;
        if (defined $filtered_pdf->[$i] and defined $dofv_array->[$i]){
            $max = $cap_resampling;
        }
        for (my $j=0; $j<$max; $j++){
            push(@augmented_raw,$raw_results->[$i]);
            push(@augmented_pdf,$filtered_pdf->[$i]);
            push(@augmented_dofv,$dofv_array->[$i]);
            push(@augmented_id,$id_array->[$i]);
        }
    }
    return {'raw' => \@augmented_raw,'dofv' => \@augmented_dofv, 'pdf' => \@augmented_pdf, 'id' => \@augmented_id};

}

sub do_resampling
{
    #static
    my %parm = validated_hash(\@_,
                              times_sampled => { isa => 'ArrayRef', optional => 0 },
                              sample_order => { isa => 'ArrayRef', optional => 0 },
                              current_resamples => { isa => 'Int', optional => 0 },
                              wghash => { isa => 'HashRef', optional => 0 },
                              with_replacement => { isa => 'Bool', optional => 0 },
        );
    my $times_sampled = $parm{'times_sampled'};
    my $sample_order = $parm{'sample_order'};
    my $current_resamples = $parm{'current_resamples'};
    my $wghash = $parm{'wghash'};
    my $with_replacement = $parm{'with_replacement'};

    my $ok_resamples=0;

    for (my $i=0; $i<$current_resamples; $i++){
        unless ($wghash->{'sum_weights'}>0){
            last; #stop sampling if all probs are 0
        }
        my $sample_index = weighted_sample(cdf => $wghash->{'cdf'});
        $times_sampled->[$sample_index]++;
        $ok_resamples++;
        $sample_order->[$sample_index] = $ok_resamples; #may overwrite if with_replacement
        unless ($with_replacement ){
            recompute_weights(weight_hash => $wghash,
                              reset_index => $sample_index);
        }
    }
    return $ok_resamples;
}

sub update_actual_resamples
{
    #static
    my %parm = validated_hash(\@_,
                              samples => { isa => 'ArrayRef', optional => 0 },
                              resamples => { isa => 'ArrayRef', optional => 0 },
                              successful_samples => { isa => 'ArrayRef', optional => 0 },
                              actual_resamples => { isa => 'ArrayRef', optional => 0 },
                              iteration => { isa => 'Int', optional => 0 },
                              successful_count => { isa => 'Int', optional => 0 },
        );
    my $samples = $parm{'samples'};
    my $resamples = $parm{'resamples'};
    my $successful_samples = $parm{'successful_samples'};
    my $actual_resamples = $parm{'actual_resamples'};
    my $iteration = $parm{'iteration'};
    my $successful_count = $parm{'successful_count'};

    push(@{$successful_samples},$successful_count);

    my $new_resamples = $resamples->[($iteration-1)];

    my $turnout = $successful_count/($samples->[($iteration-1)]);
    #turnout can be both larger and smaller than 1
    if (abs($turnout-1) >= 0.05){ # at least 5 % loss or gain relative original request
        $new_resamples = round($resamples->[($iteration-1)]*$turnout);
    }
    push(@{$actual_resamples},$new_resamples);

    return $new_resamples;
}

sub update_attempted_samples
{
    #static
    my %parm = validated_hash(\@_,
                              samples => { isa => 'ArrayRef', optional => 0 },
                              successful_samples => { isa => 'ArrayRef', optional => 0 },
                              attempted_samples => { isa => 'ArrayRef', optional => 0 },
                              iteration => { isa => 'Int', optional => 0 },
        );
    my $samples = $parm{'samples'};
    my $successful_samples = $parm{'successful_samples'};
    my $attempted_samples = $parm{'attempted_samples'};
    my $iteration = $parm{'iteration'};

    my $new_samples = $samples->[($iteration-1)];
    if ($iteration > 1){
        my $previous_turnout = ($successful_samples->[($iteration-2)])/($attempted_samples->[($iteration-2)]);
        #previous turnout cannot be larger than 1, then bug
        if ($previous_turnout > 1){
            croak("this is a bug, successful samples ".$successful_samples->[($iteration-2)].
                " iteration ".($iteration-1)." larger than attempted samples ".$attempted_samples->[($iteration-2)]);
        }
        if ($previous_turnout <= 0.95){ # at least 5 % loss
            $new_samples = round($samples->[($iteration-1)]/$previous_turnout);
        }
    }

    push(@{$attempted_samples},$new_samples);
    return $new_samples;
}

sub prepare_results
{
    my $self = shift;
    ui -> print( category => 'sir',
                 message => "\nAnalyzing results");

    my $rawresfile;
    if ($self->recompute()){
        #change results_file so that old is not overwritten
        my $fname = $self->results_file();
        $fname =~ s/\.csv$/_recompute/ ;
        my $addnum=1;
        while (-e $self -> directory."/$fname$addnum".'.csv'){
            $addnum++;
        }
        $self->results_file("$fname$addnum".'.csv');
    }

#    print "prepare results model name ".$self -> models -> [0]->full_name."\n";
#    print "prepare results rawres name ".$self->raw_results_file()->[0]."\n";

    my $iterationfile = $self -> directory."/summary_iterations.csv";
    open ( RES, ">" . $iterationfile );
    print RES "iteration,commandline.samples,attempted.samples,successful.samples,".
        "commandline.resamples,actual.resamples,requested.ratio,actual.ratio,negative.dOFV,minimum.sample.ofv\n";
    if (defined $self->rawres_input and (not $self->recover) and (not $self->add_iterations)){
        print RES "0, , , , ,".$self->rawres_samples().", , , , \n";
    }

    for (my $i=0; $i<$self->max_iteration; $i++){
        print RES ($i+1).','.$self->samples->[$i].','.$self->attempted_samples->[$i].','.$self->successful_samples->[$i].','.
            $self->resamples->[$i].','.$self->actual_resamples->[$i].','.($self->samples->[$i]/$self->resamples->[$i]).
            ','.($self->successful_samples->[$i]/$self->actual_resamples->[$i]).
            ','.$self->negative_dofv->[$i].','.$self->minimum_ofv->[$i]."\n";
    }
    close(RES);


    my $model = $self -> models -> [0];
    my ($sampled_params_arr,$href) = model::get_rawres_params(filename => $self->raw_results_file()->[0],
                                                              require_numeric_ofv => 1,
                                                              extra_columns => ['resamples'],
                                                              offset => 1,
                                                              model => $model);
    my $len= 0;
    $len = scalar(@{$sampled_params_arr}) if (defined $sampled_params_arr);
    ## Prepare general run info for output file
    my %return_section;
    $return_section{'name'} = 'SIR run info';
    my $modelname=$model ->filename();
    $return_section{'labels'} = [[],['Date','model','PsN version','NONMEM version']];

    my @datearr=localtime;
    my $the_date=($datearr[5]+1900).'-'.($datearr[4]+1).'-'.($datearr[3]);
    $return_section{'values'} = [[$the_date,$modelname,'v'.$PsN::version,$self->nm_version]];
    #results is initialized in tool.dia
    push( @{$self -> results->[0]{'own'}},\%return_section );

#    print "output is ".$output->full_name."\n";

    my $resulthash = empirical_statistics( sampled_params_arr => $sampled_params_arr,
                                           labels_hash => $self->parameter_hash);

    #TODO print restart info here
    #hash

    my %se_section;
    $se_section{'name'}='Summary statistics over resamples';
    $se_section{'labels'}=[['center_estimate','mean','median','se','rse','rse_sd'],$self->parameter_hash->{'labels'}];
    $se_section{'values'}=[$resulthash->{'center_estimate'},$resulthash->{'mean'},
                           $resulthash->{'median'},$resulthash->{'standard_error'},
                           $resulthash->{'relative_standard_error'},$resulthash->{'rse_sd_scale'}];
    push( @{$self -> results->[0]{'own'}},\%se_section );

    my %perc_section;
    my @perc_labels=();
    foreach my $lab (@{$resulthash->{'percentiles_labels'}}){
        push(@perc_labels,$lab.'%');
    }

    $perc_section{'name'}='Quantiles (R type=2)';
    $perc_section{'labels'}=[\@perc_labels,$self->parameter_hash->{'labels'}];
    $perc_section{'values'}=$resulthash->{'percentiles_values'};
    push( @{$self -> results->[0]{'own'}},\%perc_section );

    my %space_section;
    $space_section{'name'}= ' ';
    $space_section{'labels'}= [' '];
    $space_section{'values'}= [[]];
    push( @{$self -> results->[0]{'own'}},\%space_section );

    my %sdcorr_section;
    $sdcorr_section{'name'}='Empirical sd-correlation matrix';
    $sdcorr_section{'labels'}=[$self->parameter_hash->{'labels'},$self->parameter_hash->{'labels'}];
    $sdcorr_section{'values'}=$resulthash->{'sdcorr'};
    push( @{$self -> results->[0]{'own'}},\%sdcorr_section );

    my $basename = $model->create_output_filename();
    $basename =~ s/\.lst$/_sir.cov/;
    my( $ldir, $name ) = OSspecific::absolute_path( $self ->directory(),$basename);
    print_empirical_covmatrix(filename=> $ldir.$name,
                              parameter_hash => $self->parameter_hash,
                              covar => $resulthash->{'covar'});

}

sub print_empirical_covmatrix
{
    my %parm = validated_hash(\@_,
                              filename => { isa => 'Str', optional => 0 },
                              parameter_hash => { isa => 'HashRef', optional => 0 },
                              covar => { isa => 'ArrayRef', optional => 0 },
        );
    my $filename = $parm{'filename'};
    my $parameter_hash = $parm{'parameter_hash'};
    my $covar = $parm{'covar'};

    my @order=();
    my @coords=();
    foreach my $param ('theta','sigma','omega'){
        for (my $i=0; $i<scalar(@{$parameter_hash->{'coordinate_strings'}}); $i++){
            if ($parameter_hash->{'param'}->[$i] eq $param){
                push(@coords, $parameter_hash->{'coordinate_strings'}->[$i]) ;
                push(@order, $i);
            }
        }
    }
    my $copy = linear_algebra::copy_and_reorder_square_matrix($covar,\@order);
    my $formatted = tool::format_covmatrix(matrix => $copy, header => \@coords, comma => 0, print_labels => 1);
    open ( RES, ">" . $filename );
    foreach my $line (@{$formatted}){
        print RES $line;
    }
    close(RES);

}

sub create_R_plots_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              rplot => { isa => 'rplots', optional => 0 }
        );
    my $rplot = $parm{'rplot'};

    my @cols=();
    my %count;
    $count{'param'}=0;
    $count{'theta'}=0;
    $count{'omega'}=0;
    $count{'sigma'}=0;


    my $maxresamplestring = 'MAX.RESAMPLE <- 1    # maximum resamples for single sample. 1 if without replacement';
    if ($self->with_replacement){
        $maxresamplestring = 'MAX.RESAMPLE <- '.$self->resamples->[($self->max_iteration-1)];
    }

    my $labelref = $self->models->[0]->problems->[0]->get_estimated_attributes(parameter => 'all',
                                                                          attribute => 'labels');
    #we should not filter out off-diagonals like in sse. Verified.

    if (defined $labelref){
        $count{'param'} = scalar(@{$labelref});
        #cannot use raw_results_header, has only placeholders for sigma etc
        my $headerref = $self->full_rawres_header; #ref of array of strings, no quotes
#        print join(' ',@{$headerref})."\n";
        @cols = @{get_array_positions(target => $headerref,keys => $labelref)};
        foreach my $par ('theta','omega','sigma'){
            my $ref = $self->models->[0]->problems->[0]->get_estimated_attributes(parameter => $par,
                                                                                  attribute => 'labels');
            $count{$par} = scalar(@{$ref}) if (defined $ref);
        }
    }


    my $colstring = 'COL.ESTIMATED.PARAMS <-  c('.join(',',@cols).')   #column numbers in raw_results for parameters';
    my $paramstring = 'N.ESTIMATED.PARAMS <- '.$count{'param'};
    my $cistring = 'CI <- 95';
    my $subjectstring = 'model.subjects <- NA';
    $subjectstring = 'model.subjects <- '.$self->subjects if (defined $self->subjects) ;

    $rplot->add_preamble(code => [
                             '#sir-specific preamble',
                             'SAMPLES          <-'.$self->samples->[($self->max_iteration-1)],
                             'RESAMPLES        <-'.$self->resamples->[($self->max_iteration-1)],
                             'ALL.SAMPLES      <-c('.join(',',@{$self->samples}).')',
                             'ALL.RESAMPLES    <-c('.join(',',@{$self->resamples}).')',
                             "ALL.RAWRESFILES   <-c('".join("','",@{$self->intermediate_raw_results_files})."')",
                             $maxresamplestring,
                             $colstring,
                             $paramstring,
                             'N.ESTIMATED.THETAS <- '.$count{'theta'},
                             'N.ESTIMATED.OMEGAS <- '.$count{'omega'},
                             'N.ESTIMATED.SIGMAS <- '.$count{'sigma'},
                             $subjectstring,
                             $cistring,
                         ]);

}

1;
