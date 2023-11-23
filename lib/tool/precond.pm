package tool::precond;

use include_modules;
use Math::Trig;
use File::Spec;
use Mouse;
use MouseX::Params::Validate;
use File::Copy qw(copy);

use array qw(:all);
use tool::modelfit;
use linear_algebra;
use output;
use Storable qw(dclone);
use utils::file;

extends 'tool';

has 'precond_matrix' => ( is => 'rw', isa => 'Maybe[ArrayRef[ArrayRef]]' );
has 'precond_model' => ( is => 'rw', isa => 'model' );
has 'base_model' => ( is => 'rw', isa => 'model' );
has 'update_model' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'negaEigenIndex' =>  ( is => 'rw', isa => 'ArrayRef' );
has 'notalways' =>  ( is => 'rw', isa => 'Bool', default => 0 );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'perturb' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has '_no_precond' => ( is => 'rw', isa => 'Bool', default => 0 );       # Did we run a preconditioned model?
has '_repara_model' => ( is => 'rw', isa => 'model' );


sub BUILD
{
    my $self = shift;

}

sub modelfit_setup
{
    my $self = shift;

    if (not defined $self->precond_matrix) {
        my $base_model = $self->create_base_model();
        $self->base_model($base_model);

        my %tool_options = %{common_options::restore_options(@common_options::tool_options)};
        my $nm_output = $tool_options{'nm_output'};
        my $rmt_added = 0;
        if (not defined $nm_output or $nm_output eq "") {
            $nm_output = "rmt,cov";
            $rmt_added = 1;
        } elsif (not $nm_output =~ /rmt,cov/) {
            $nm_output .= ',rmt,cov';
            $rmt_added = 1;
        }

        my $base_modelfit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $base_model ],
            base_dir => $self->directory,
            directory => "base_modelfit",
            top_tool => 0,
            nm_output => $nm_output,
            clean => 2,
            copy_data => $self->copy_data,
        );

        $base_modelfit->run;

        copy("base_modelfit/raw_results.csv", "base_raw_results.csv");

        my @files = glob "m1/*";
        foreach my $file (@files) {
            next if ($file =~ /\.(mod|ctl)$/);
            next if ($file =~ /\.rmt$/ and $rmt_added);
            copy($file, "..");
        }

        if (not $base_model->is_run) {
            croak("\n\nmodel " . $self->precond_model->filename . " could not be run\n");
        }
        if (not $base_model->outputs->[0]->covariance_step_run->[0]) {
            croak("\nCovariance step of the original model did not run (most likely the parameter estimation had failed).\n\nSee .lst file at ",$self->directory."m1/",utils::file::replace_extension($self->precond_model->filename, 'lst')," for the reason.\n\nPrecond cannot help if the parameter estimation of the original model fails in a way that the NONMEM terminates before proceeding to the covariance step.");
        }
        if ($base_model->outputs->[0]->problems->[0]->subproblems->[0]->s_matrix_unobtainable) {
            print "\n\nS matrix was unobtainable, precond is intended to stablise covariance step by reducing the R-matrix related computational issues, hence most unlikely to remedy this issues with S matrix.\n";
        }

        my $cov_successful = $base_model->outputs->[0]->covariance_step_successful->[0][0];

        if ($cov_successful and $self->notalways) {
            $self->tools([]);
            $self->_no_precond(1);
            print "\nCovariance step of the original model successful, as -notalways option has been specified, will not proceed to preconditioning.\n";
            return;
        }

        if (not $cov_successful) {
            print "\nCovariance step of the original model failed will now run preconditioning\n";
        } else {
            print "\nOriginal model run finished.  Will now run preconditioning\n";
        }

        my $rmt_filename = $base_model->directory . utils::file::replace_extension($base_model->filename, 'rmt');

        $self->read_precond_matrix($rmt_filename);
        preprocess_precond_matrix(precond_matrix => $self->precond_matrix, nthetas => $base_model->nthetas);
        $self->eigenvalue_decomposition();
    }

    my $repara_filename = $self->precond_model->filename;
    $repara_filename =~ s/(\.ctl|\.mod)$//;
    $repara_filename .= '_repara.mod';
    $repara_filename = File::Spec->catfile(($self->directory, "m1"), $repara_filename);

    my $model = create_reparametrized_model(
        filename => $repara_filename,
        model => $self->precond_model,
        precond_matrix => $self->precond_matrix,
        negaEigenIndex => $self->negaEigenIndex,
        directory => $self->directory,
    );

    my $modelfit = tool::modelfit->new(
        %{common_options::restore_options(@common_options::tool_options)},
        models => [ $model ],
        base_dir => $self->directory,
        directory => "repara_modelfit",
        top_tool => 0,
        nm_output => 'ext,cov,rmt',
        copy_data => $self->copy_data,
    );

    $self->_repara_model($model);

    $self->tools([]) unless defined $self->tools;
    push(@{$self->tools}, $modelfit);
}

sub modelfit_analyze
{
    my $self = shift;

    if ($self->_no_precond) {
        return;
    }

    my $filename = $self->_repara_model->filename;
    $filename =~ s/\.mod$/.cov/;
    my $cov_filename = 'm1/' . $filename;

    my $model_filename = $self->precond_model->filename;
    $model_filename =~ s/(\.ctl|\.mod)$//;
    my $result_cov = $model_filename . '.cov';

    my $cov_matrix = convert_reparametrized_cov(
        cov_filename => $cov_filename,
        model => $self->precond_model,
        repara_model => $self->_repara_model,
        precond_matrix => $self->precond_matrix,
        output_filename => $result_cov,
        directory => $self->directory
    );

    if ($self->_repara_model->is_run) {
        my $output = $self->_repara_model->outputs->[0];

        my $copy = $self->precond_model->copy(
            filename => 'updated_model.mod',
            copy_datafile => 0,
            write_copy => 0,
            copy_output => 0
        );

        my $theta = $output->get_single_value(attribute => 'thetas');
        my $new_theta = [];

        # Multiply precond_matrix with theta to obtain original theta
        for (my $row = 0; $row < scalar(@$theta); $row++) {
            my $sum = 0;
            for (my $col = 0; $col < scalar(@$theta); $col++) {
                $sum += $self->precond_matrix->[$row]->[$col] * $theta->[$col];
            }
            $new_theta->[$row] = $sum;
        }

        my $theta_ind = 0;
        for my $theta (@{$copy->problems->[0]->thetas}) {
            for (my $i = 0; $i < scalar(@{$theta->options}); $i++) {
                my $option = $theta->options->[$i];
                $option->init($new_theta->[$theta_ind]);

                if (defined $option->lobnd and $new_theta->[$theta_ind] < $option->lobnd) {
                    $option->clear_lobnd;
                    print "Warning: updated THETA(", $theta_ind + 1, ") estimate is below original lower bound. The bound will be removed in updated model.\n";
                }
                if (defined $option->upbnd and $new_theta->[$theta_ind] > $option->upbnd) {
                    $option->clear_upbnd;
                    print "Warning: updated THETA(", $theta_ind + 1, ") estimate is above original upper bound. The bound will be removed in updated model.\n";
                }
                $theta_ind++;
            }
        }

        $copy->_write;
        if (defined $self->update_model) {
            copy $copy->full_name, '../' . $self->update_model;
        }


#######################################
#######################################
#######################################
        my $isEstimable=1;

        my $foldername=(split(/\//, $self->directory))[-1];


        if (defined $self->base_model){
            if($self->base_model->is_run){
                my $theta_original = $self->base_model->outputs->[0]->get_single_value(attribute => 'thetas');

                my $ofv_original=$self->base_model->outputs->[0]->ofv()->[0][0];
                my $ofv_precond=$self->_repara_model->outputs->[0]->ofv()->[0][0];
                my $para_diff=0;

                # compare the parameters found by the original model and preconditioned model and give a worning if it is more than 10% differences

                if (abs($ofv_original-$ofv_precond)<0.1){
                    for (my $i=0; $i<@$new_theta;$i++){
                        if (abs($new_theta->[$i] + $theta_original->[$i])>0){
                            my $para_diff=abs($new_theta->[$i] - $theta_original->[$i])/abs($new_theta->[$i] + $theta_original->[$i])*200;
                            my $abscheck=abs($new_theta->[$i] - $theta_original->[$i])/abs(abs($new_theta->[$i]) + abs($theta_original->[$i]))*200;

                            if($para_diff>10&&int($abscheck)!=200){
                            print "\nWarning: THETA(", $i + 1, ") is most likely to be non-estimable from the data.  Precond has found a set of parameters that are ", int($para_diff*100)/100 ,"% different from eachother ($theta_original->[$i] and $new_theta->[$i]) but gives almost identical ofv (dOFV=",int(abs($ofv_original-$ofv_precond)*100)/100,"). \n";
                            $isEstimable=0;
                            }
                        }
                    }

                }else{
                    if(($ofv_original-$ofv_precond<0)){
                        print "\nWarning: parameter estimation seems to be unstable (the ofvs of the original model and preconditioned model differ by ", int(($ofv_original-$ofv_precond)*100)/100, " (in theory they should be the same), try 'update_inits ", $self->base_model->filename,"' and re-run preconditioning.  If the model is very instable try with MAXEVAL=0 after update_inits (not recommended as it will make precond ignore the issues on parameter estimation).";
                    }else{
                        print "\nWarning: precond has found a set of parameters with lower ofv than the original model, hence the parameters found by the original model is not at the maximum likelihood.\n\nTry 'update_inits ",$self->base_model->filename," -from_model ", $foldername,"/updated_model.mod' and re-run precond.\n";
                    }
                }
            }
        }


        # Print raw results
        my $modelfit = $self->tools->[0];
        my $filename = $self->precond_model->filename;
        $filename =~ s/.mod$/.csv/;
        $filename = "raw_results_" . $filename;
        $modelfit->directory(".");
        $modelfit->raw_results_file->[0] = $filename;

        my $theta_pos = $modelfit->raw_line_structure->{1}->{theta};
        $theta_pos =~ s/(.*),.*/$1/;
        for (my $i = 0; $i < @$new_theta; $i++) {
            $modelfit->raw_results->[0]->[$theta_pos + $i] = $new_theta->[$i];
        }
        my $se_pos = $modelfit->raw_line_structure->{1}->{setheta};
        my $ofv_pos= $modelfit->raw_line_structure->{1}->{ofv};

        $se_pos =~ s/(.*),.*/$1/;

        my $covSuccess=0;

        for (my $i=0; $i<@$new_theta;$i++){
            if ($cov_matrix->[$i]->[$i] !=0){
                $covSuccess=1;
            }
        }

        if ($covSuccess==0){
            for (my $i = 0; $i < @$new_theta; $i++) {
                $modelfit->raw_results->[0]->[$se_pos + $i] = "NA";
            }
        }else{
            for (my $i = 0; $i < @$new_theta; $i++) {
                $modelfit->raw_results->[0]->[$se_pos + $i] = sqrt($cov_matrix->[$i]->[$i]);
            }



            if (defined $self->base_model){

                  my $filename = $self->base_model->filename;
                    $filename =~ s/\.mod$/.cov/;
                    my $cov_filename = 'm1/' . $filename;
                    my $model = $self->base_model;

                    my @cov_lines;

                print "$cov_filename";
                    if (-e $cov_filename) {
                        open(my $fh, '<', $cov_filename);
                        while (my $tline = <$fh>) {
                            chomp $tline;
                            push @cov_lines, $tline;
                        }

                        my @originalCov;

                        my $rowCount = 0;
                        my $found_table;
                        my @header_labels;
                        my $header_ok;
                        my $given_header_warning;

                        for (my $k = 0; $k < scalar(@cov_lines); $k++) {
                            my $line = $cov_lines[$k];
                            if ($line =~ /^\s*TABLE NO.\s+(\d+):/) {
                                croak("two tables found where 1 expected") if $found_table;
                                $found_table = 1;
                            } elsif ($line =~ /^\s*NAME/ ) {
                                $line =~ s/^\s*//; #get rid of leading spaces
                                @header_labels = split /\s+/, $line;
                                $header_ok = 1 if ($header_labels[0] eq 'NAME');
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
                                $line =~ s/^\s*//; #get rid of leading spaces
                                my @line_values = split /\s+/,$line;
                                my $max_column;

                                my @new_line;
                                $max_column = scalar(@header_labels) ; #store full matrix
                                for (my $j = 0; $j < $max_column; $j++) {
                                    my $i = $j + 1; #must permute omega-sigma
                                    if ((not defined $line_values[$i]) or $line_values[$i] eq 'NaN') {
                                        push(@new_line, undef);
                                        $originalCov[$rowCount][$j]=undef;
                                    } else {
                                        push(@new_line, eval($line_values[$i]));
                                        $originalCov[$rowCount][$j]=eval($line_values[$i]);
                                    }

                                }
                                $rowCount++;
                            }
                        }


                    if ($isEstimable) {
                        my $standardErrorConsistent=1;
                        my $maxchange=0;

                        for (my $i = 0; $i < @$new_theta; $i++) {

                            if ($originalCov[$i]->[$i]!=0){
                                my $change=((abs(sqrt($cov_matrix->[$i]->[$i])-sqrt($originalCov[$i]->[$i]))/abs(sqrt($cov_matrix->[$i]->[$i])+sqrt($originalCov[$i]->[$i])))*200);
                                if( sqrt($cov_matrix->[$i]->[$i])/sqrt($originalCov[$i]->[$i])>2){
                                    print "\nWarning: standard error of THETA(",$i+1,") has grown more than twice after preconditioning, non-estimability of THETA(",$i+1,") is suspected. Repeat preconditioning using -pre=",$foldername," option to see if the standard error grows more.";
                                    $standardErrorConsistent=0;
                                }elsif($change>10){
                                    $standardErrorConsistent=0;
                                }
                                if ($change>$maxchange){
                                    $maxchange=$change;
                                }
                            }

                        }
                        if ($standardErrorConsistent==1){
                            print "\nStandard errors are consistent (maximum difference is ", int($maxchange*100)/100," %) between the original model and preconditioned model, hence the calculated variance-covariance matrix can be trusted."
                        }else{
                            print "\nWarning: Standard errors are not consistent (maximum difference is ", int($maxchange*100)/100," %) between the original model and preconditioned model, hence use calculated variance-covariance matrix with caution."
                        }

                    }
                }

            #    print "\n\n  preconditioning successful \n";
        }

        }
        if (!$isEstimable){
                    #$modelfit->raw_results->[0]->[$ofv_pos]="NaN";
                    print "\n\n  If a parameter is non-estimable, the variance-covariance matrix in theory does not exist, hence it is correct for NONMEM and precond to ''fail'' covariance step.\n";
        }

        $modelfit->print_raw_results;

    } else {
        print "Unable to update model: model was not run";
    }
}

sub _reparametrize
{
    my $code = shift;
    my $nthetas = shift;
    my $model = shift;

    for (my $i = 0; $i < scalar(@$code); $i++) {
        my $find = "THE_";
        my $replace = "TTHE_";
        $find = quotemeta $find;
        $code->[$i] =~ s/$find/$replace/g;

    }

    my $fixed = $model->fixed(parameter_type => 'theta');

    for (my $i = 0; $i < scalar(@$code); $i++) {
            for (my $j = 1; $j < $nthetas + 1; $j++) {
                if (!$fixed->[0]->[$j-1]){
                    my $find = "THETA($j)";
                    my $replace = "THE_$j";
                    $find = quotemeta $find;
                    $code->[$i] =~ s/$find/$replace/g;
                }
            }
        }

}

sub create_base_model
{
    # Create the base model that adds options for R-matrix
    my $self = shift;

    my $base_filename = $self->precond_model->filename;
    #$base_filename =~ s/(\.ctl|\.mod)$//;
    #$base_filename .= '_base.mod';
    $base_filename = File::Spec->catfile(($self->directory, "m1"), $base_filename);

    my $base_model = $self->precond_model->copy(
        output_same_directory => 1,
        filename => $base_filename,
        copy_datafile => 0,
        write_copy => 0,
        copy_output => 0,
    );

    _set_model_options(model => $base_model);

    $base_model->_write;

    return $base_model;
}

sub _set_model_options
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    $model->problems->[0]->covariance(enabled => 1);
    if ($model->is_option_set(record => 'covariance', name => 'MATRIX', fuzzy_match => 1)) {
        print "\nprecond is designed for the covariance matrix defined as R^(-1)SR^(-1) (i.e., the sandwich matrix) hence does not work with option MATRIX set in \$COVARIANCE, we have removed option MATRIX.\n";
        #    croak("Error: Option MATRIX set in \$COVARIANCE will not work with precond. Please remove and run again\n");
    }
    $model->remove_option(record_name => 'covariance', option_name => 'MATRIX');


    if ($model->is_option_set(record => 'covariance', name => 'MATRIX', fuzzy_match => 1)) {
        croak("Error: Option MATRIX set in \$COVARIANCE will not work with precond. Please remove and run again\n");
    }

    if (not $model->is_option_set(record => 'covariance', name => 'UNCONDITIONAL', fuzzy_match => 1)) {
        $model->add_option(record_name => 'covariance', option_name => 'UNCONDITIONAL');
    }

    my $values = $model->get_option_value(record_name => 'covariance', option_name => 'PRINT', option_index => 'all');
    my $found = 0;
    foreach my $value (@$values) {
        if ($value eq 'R') {
            $found = 1;
            last;
        }
    }
    if (not $found) {
        $model->add_option(record_name => 'covariance', option_name => 'PRINT', option_value => 'R');
    }

    if (not $model->is_option_set(record => 'estimation', name => 'FORMAT', fuzzy_match => 1)) {
        $model->add_option(record_name => 'estimation', option_name => 'FORMAT', option_value => 's1PE23.16');
    }
}

sub create_reparametrized_model
{
    my %parm = validated_hash(\@_,
        filename => { isa => 'Str', optional => 0 },
        model => { isa => 'model' },
        precond_matrix => { isa => 'ArrayRef[ArrayRef]' },
         negaEigenIndex=> {isa => 'rw', isa => 'ArrayRef'},
        directory =>  { isa => 'Str'},
    );
    my $filename = $parm{'filename'};
    my $model = $parm{'model'};
    my @precond_matrix = @{$parm{'precond_matrix'}};
    my @negaEigenIndex = @{$parm{'negaEigenIndex'}};
    my $directory = $parm{'directory'};

    $model = $model->copy(
        output_same_directory => 1,
        filename => $filename,
        copy_datafile => 0,
        write_copy => 0,
        copy_output => 0
    );

    _set_model_options(model => $model);

    my @code;
    my $code_record;
    if ($model->has_code(record => 'pk')) {
        @code = @{$model->get_code(record => 'pk')};
        $code_record = 'pk';
    } elsif ($model->has_code(record => 'pred')) {
        @code = @{$model->get_code(record => 'pred')};
        $code_record = 'pred';
    } else {
        croak("Neither PK nor PRED defined in " . $model->filename . "\n");
    }

    _reparametrize(\@code, $model->nthetas, $model);

    my $tempString;
    my $tempTempSt;

    my $fixed = $model->fixed(parameter_type => 'theta');

    for (my $i = 0; $i < $model->nthetas; $i++) {
        if (!$fixed->[0]->[$i]){

        $tempString = 'THE_' . ($i + 1) . '=';
        my @temp=@{$precond_matrix[$i]};
        for (my $j = 0; $j < scalar(@temp); $j++) {
            if ($j == 0 or $precond_matrix[$i][$j] < -0.000000000000001) {
                if (length($tempString) + length($precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')') > 70) {
                    $tempString = $tempString . "\n";
                    $tempTempSt = $tempTempSt . $tempString;
                    $tempString = 'THE_'.($i + 1) . '=THE_' . ($i + 1) . $precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
                } else {
                    $tempString = $tempString . $precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
                }
            } elsif ($precond_matrix[$i][$j] > 0.000000000000001) {
                if (length($tempString) + length($precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')') > 70) {
                    $tempString = $tempString . "\n";
                    $tempTempSt = $tempTempSt . $tempString;
                    $tempString = 'THE_' . ($i + 1) . '=THE_' . ($i + 1) . '+' . $precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
                } else {
                    $tempString = $tempString . '+' . $precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
                }
            }
        }
        $tempString = $tempString . "\n";
        $tempTempSt = $tempTempSt . $tempString;
        }
    }

    # Make sure reparametrization is only done when THETAs have changed
    $tempTempSt = "IF (NEWIND == 0) THEN\n" . $tempTempSt . "END IF\n";

    unshift @code, $tempTempSt;

    $model->set_code(record => $code_record, code => \@code);

    # Reparametrize other blocks of abbreviated code
    for my $record (('error', 'des', 'aes', 'aesinitial', 'mix', 'infn')) {
        if ($model->has_code(record => $record)) {
            my $code = $model->get_code(record => $record);
            _reparametrize($code, $model->nthetas, $model);
            $model->set_code(record => $record, code => $code);
        }
    }

    # Remove limits for thetas
    foreach my $theta (@{$model->problems->[0]->thetas}) {
           foreach my $option (@{$theta->options}) {
            $option->clear_upbnd;
            $option->clear_lobnd;
        }
    }

    # Set new initial values for thetas
    my @parameter_initial = @{$model->initial_values(parameter_type => 'theta')->[0]};
    my @dummie_parameter_initial = @{$model->initial_values(parameter_type => 'theta')->[0]};

    my @parameter_initial_copy;

    for(my $i=0; $i<scalar(@precond_matrix);$i++){
        $parameter_initial_copy[$i]=$parameter_initial[$i];
    }

    # Clone to be not overwrite precond_matrix
    my $ref = dclone(\@precond_matrix);
    @precond_matrix = (@$ref);


    my $offDiag = 0;
    my $diagSum = 0;
    my $offDigTemp = 0;
    my @diagElement;

    for (my $k = 0; $k < scalar(@parameter_initial); $k++) {
        $diagElement[$k] = 0;

        for (my $i = 0; $i < scalar(@parameter_initial); $i++) {
            $parameter_initial[$i] = 0;
            for (my $j = 0; $j < scalar(@parameter_initial); $j++) {
                if ($k == $i) {
                    $diagSum = $diagSum + ($precond_matrix[$j][$k] * $precond_matrix[$j][$i]);
                    $diagElement[$i] = $diagElement[$i] + $precond_matrix[$j][$k] * $precond_matrix[$j][$i];
                }else{
                    $offDigTemp = $offDigTemp + ($precond_matrix[$j][$k] * $precond_matrix[$j][$i]);
                }
            }
            $offDiag = $offDiag + abs($offDigTemp);
            $offDigTemp = 0;
        }
    }

    if ($offDiag < 0.000001) {   # if precond_matrix is orthogonal up to scaling
      for (my $i = 0; $i < scalar(@parameter_initial); $i++) {
        $parameter_initial[$i] = 0;
        for (my $j = 0; $j < scalar(@parameter_initial); $j++) {
            $parameter_initial[$i] = $parameter_initial[$i] + $dummie_parameter_initial[$j] * $precond_matrix[$j][$i];
        }
        $parameter_initial_copy[$i] = $parameter_initial[$i] / $diagElement[$i];
      }
    } else { # if precond_matrix is not orthogonal up to scaling

        linear_algebra::LU_factorization(\@precond_matrix);

        for (my $i = 1; $i < scalar(@precond_matrix); $i++) {
            for (my $j = 0; $j < $i; $j++) {
                $parameter_initial_copy[$i] = $parameter_initial_copy[$i] - $parameter_initial_copy[$j] * $precond_matrix[$i][$j];
            }
        }
        for (my $i = scalar(@precond_matrix)-1; $i >= 0; $i--) {
            for (my $j = scalar(@precond_matrix)-1; $j > $i; $j--) {
                $parameter_initial_copy[$i] = $parameter_initial_copy[$i] - $parameter_initial_copy[$j] * $precond_matrix[$i][$j];
             }
            $parameter_initial_copy[$i] = $parameter_initial_copy[$i]/$precond_matrix[$i][$i];
        }
    }

    if (scalar(@negaEigenIndex) > 0) {
        print "\n\nPerturbed indices: ";
        for (my $i = 0; $i < scalar(@negaEigenIndex); $i++) {
            print $negaEigenIndex[$i];
            print " ";
            $parameter_initial_copy[$negaEigenIndex[$i]] = $parameter_initial_copy[$negaEigenIndex[$i]] + 1;
        }
    }

    $model->initial_values(parameter_type => 'theta', new_values => [[@parameter_initial_copy]]);

    if ($PsN::nm_major_version >= 8 or ($PsN::nm_major_version == 7 and $PsN::nm_minor_version >= 2)) {
        # Increase NMTRAN limits on the number of intermediate variables and total number of constants
        my $size_value = 100000;
        my %changed_sizes = (DIMCNS => 0, DIMNEW => 0, DIMTMP => 0);
        if (defined $model->problems->[0]->sizess) {
            for my $sizes (@{$model->problems->[0]->sizess}) {
                for my $option (@{$sizes->options}) {
                    for my $relevant (keys %changed_sizes) {
                        if ($option->name eq $relevant) {
                            $option->name($relevant);
                            $option->value($size_value);
                            $changed_sizes{$relevant} = 1;
                        }
                    }
                }
            }
        }
        my @record_strings = ();
        for my $name (keys %changed_sizes) {
            if (not $changed_sizes{$name}) {
                push @record_strings, "$name=$size_value";
            }
        }

        if (@record_strings) {
            $model->problems->[0]->add_records(type => 'sizes', record_strings => \@record_strings );
        }
    } else {
        print "Warning: You have an older version of NONMEM than 7.2 which means that it is not possible to set\n";
        print "\$SIZES for DIMCNS, DIMNEW and DIMTMP which are recommended to be set to at least 100000.\n";
        print "You have to make sure that these sizes are appropriately set in your version of NONMEM,\n";
        print "otherwise this run might crash.\n";
    }


    $model->_write;

    my @precMatSLine;
    my @line_values;
    for (my $i = 0; $i < $model->nthetas; $i++) {
        for (my $j = 0; $j < $model->nthetas; $j++) {
            $line_values[$j]=$precond_matrix[$i][$j];
            if ($line_values[$j] < 0) {
                $line_values[$j] = sprintf(" %.17E", $line_values[$j]);
            } else {
                $line_values[$j] = sprintf("  %.17E", $line_values[$j]);
            }
        }
        $precMatSLine[$i] = join " ", @line_values;
    }

    open (my $MYFILE, '>', File::Spec->catfile($directory, "precMatrix"));
    for (my $i = 0; $i < scalar(@precMatSLine); $i++) {
        print $MYFILE $precMatSLine[$i] . "\n";
    }

    close $MYFILE;

    return $model;
}

sub convert_reparametrized_cov
{
    my %parm = validated_hash(\@_,
        cov_filename => { isa => 'Str' },
        model => { isa => 'model' },
        repara_model => { isa => 'model' },
        precond_matrix => { isa => 'ArrayRef[ArrayRef]' },
        output_filename => { isa => 'Str' },
        directory => { isa => 'Str' },

    );
    my $cov_filename = $parm{'cov_filename'};
    my $model = $parm{'model'};
    my $repara_model = $parm{'repara_model'};
    my @precond_matrix = @{$parm{'precond_matrix'}};
    my $output_filename = $parm{'output_filename'};
    my $directory = $parm{'directory'};

    my @cov_lines;

    if (-e $cov_filename) {

        open(my $fh, '<', $cov_filename) or croak("Cannot find the .cov file '$cov_filename' [$!]\n");
        while (my $tline = <$fh>) {
            chomp $tline;
            push @cov_lines, $tline;
        }

        my @reparaCov;

        my $rowCount = 0;
        my $found_table;
        my @header_labels;
        my $header_ok;
        my $given_header_warning;

        for (my $k = 0; $k < scalar(@cov_lines); $k++) {
            my $line = $cov_lines[$k];
            if ($line =~ /^\s*TABLE NO.\s+(\d+):/) {
                croak("two tables found where 1 expected") if $found_table;
                $found_table = 1;
            } elsif ($line =~ /^\s*NAME/ ) {
                $line =~ s/^\s*//; #get rid of leading spaces
                @header_labels = split /\s+/, $line;
                $header_ok = 1 if ($header_labels[0] eq 'NAME');
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
                $line =~ s/^\s*//; #get rid of leading spaces
                my @line_values = split /\s+/,$line;
                my $max_column;

                my @new_line;
                $max_column = scalar(@header_labels) ; #store full matrix
                for (my $j = 0; $j < $max_column; $j++) {
                    my $i = $j + 1; #must permute omega-sigma
                    if ((not defined $line_values[$i]) or
                        (defined $line_values[$i] and ($line_values[$i] eq 'NaN'))) {
                        push(@new_line, undef);
                        $reparaCov[$rowCount][$j]=undef;
                    } else {
                        push(@new_line, eval($line_values[$i]));
                        $reparaCov[$rowCount][$j]=eval($line_values[$i]);
                    }

                }
                $rowCount++;
            }
        }

        my @temp_varcovMatrix;
        for (my $i = 0; $i < $model->nthetas; $i++) {
            for (my $j = 0; $j < $model->nthetas; $j++) {
                $temp_varcovMatrix[$i][$j] = 0;
                for (my $k = 0; $k < $model->nthetas; $k++) {
                    $temp_varcovMatrix[$i][$j] = $temp_varcovMatrix[$i][$j] + $precond_matrix[$i][$k] * $reparaCov[$k][$j];
                }
            }
        }

        my @varcovMatrix;

        for (my $i = 0; $i < $model->nthetas; $i++) {
            for (my $j = 0; $j < $model->nthetas; $j++) {
                $varcovMatrix[$i][$j] = 0;
                for (my $k = 0; $k < $model->nthetas; $k++) {
                    $varcovMatrix[$i][$j] = $varcovMatrix[$i][$j] + $temp_varcovMatrix[$i][$k] * $precond_matrix[$j][$k];
                }
            }
        }

        #make symmetric to avoid almost symmetry
        for (my $row = 0; $row < @varcovMatrix; $row++) {
            for (my $col = $row + 1; $col < @varcovMatrix; $col++) {
                $varcovMatrix[$row][$col] = $varcovMatrix[$col][$row];
            }
        }

        my $line;
        for (my $i = 0; $i < $model->nthetas; $i++) {
            $line = $cov_lines[$i + 2];
            $line =~ s/^\s*//; #get rid of leading spaces
            my @line_values = split /\s+/, $line;
            for (my $j = 0; $j < $model->nthetas; $j++) {
                $line_values[$j + 1] = $varcovMatrix[$i][$j];
            }
            $line_values[0] = $line_values[0] . (" " x (12 - length($line_values[0]) ) );
            for my $i (1..@line_values - 1) {
                if ($line_values[$i] < 0) {
                    $line_values[$i] = sprintf(" %.17E", $line_values[$i]);
                } else {
                    $line_values[$i] = sprintf("  %.17E", $line_values[$i]);
                }
            }
            $cov_lines[$i + 2] = join " ", @line_values;
            $cov_lines[$i + 2] = " ".$cov_lines[$i + 2];
        }

        open (my $MYFILE, '>', $output_filename);
        for (my $i = 0; $i < scalar(@cov_lines); $i++) {
            print $MYFILE $cov_lines[$i] . "\n";
        }

        close $MYFILE;

        return \@varcovMatrix;

    } else {
        $cov_filename = utils::file::replace_extension($cov_filename, 'rmt');
        #$cov_filename = "repara_modelfit/NM_run1/psn.rmt";
         my $foldername=(split(/\//, $directory))[-1];
         my $filename=$model->filename;

        open(my $fh, '<', $cov_filename) or croak("\nCovariance step of the preconditioned model did not run (most likely the parameter estimation had failed).\n\nSee .lst file at $foldername/",utils::file::replace_extension($cov_filename, 'lst')," for the reason.\n\n  One common issue is due to the boundaries of the parameters, see PRECOND user guide section 4.2 for workaround.  Otherwise, update the initial estimates with the final estimate update_inits ",$filename,". If the model is very instable try with MAXEVAL=0 after update_inits (not recommended as it will make precond ignore the issues on parameter estimation).\n\n");

        while (my $tline = <$fh>) {
            chomp $tline;
            push @cov_lines, $tline;
        }

        my @reparaCov;

        my $rowCount = 0;
        my $found_table;
        my @header_labels;
        my $header_ok;
        my $given_header_warning;

        for (my $k = 0; $k < scalar(@cov_lines); $k++) {
            my $line = $cov_lines[$k];
            if ($line =~ /^\s*TABLE NO.\s+(\d+):/) {
                croak("two tables found where 1 expected") if $found_table;
                $found_table = 1;
            } elsif ($line =~ /^\s*NAME/ ) {
                $line =~ s/^\s*//; #get rid of leading spaces
                @header_labels = split /\s+/, $line;
                $header_ok = 1 if ($header_labels[0] eq 'NAME');
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
                $line =~ s/^\s*//; #get rid of leading spaces
                my @line_values = split /\s+/,$line;
                my $max_column;

                my @new_line;
                $max_column = scalar(@header_labels) ; #store full matrix
                for (my $j = 0; $j < $max_column; $j++) {
                    my $i = $j + 1; #must permute omega-sigma
                    if ($line_values[$i] eq 'NaN') {
                        push(@new_line, undef);
                        $reparaCov[$rowCount][$j]=undef;
                    } else {
                        push(@new_line, eval($line_values[$i]));
                        $reparaCov[$rowCount][$j]=eval($line_values[$i]);
                    }

                }
                $rowCount++;
            }
        }

        my $fixed = $model->fixed(parameter_type => 'theta');

        for (my $i=0; $i<scalar(@reparaCov);$i++){
            if ($fixed->[0]->[$i]||$i>$model->nthetas-1){
                $reparaCov[$i][$i]=1;
            }
        }

        # Do decomposition to be able to give suggestions to the user
        my @eigenValMatrix = map { [@$_] }@reparaCov;

        (my $eigen, my $Q) = linear_algebra::eigenvalue_decomposition(\@eigenValMatrix);

        my $abs_eigens = array::absolute($eigen);
        my $maxEigen = array::max($abs_eigens);
        my $minEigen = array::min($abs_eigens);

        my $negaCounter = 0;
        foreach my $index (0 .. scalar(@$eigen) - 1) {
            if ($eigen->[$index] < 0) {
                $negaCounter++;
            }
        }

        print "\n ===== Eigenvalues of the R-matrix of the preconditioned model ===== \n";
        foreach my $e (@$eigen) {
            print "$e\n";
        }
        print("================= \n");

        print "Condition number of the R-matrix : 10^" . int(log($maxEigen/$minEigen)/log(10)) . "\n";
        print("Number of negative eigenvalues : $negaCounter\n");

        $foldername=(split(/\//, $directory))[-1];

#        if ($repara_model->outputs->[0]->problems->[0]->subproblems->[0]->s_matrix_unobtainable) {
#            print "\nS matrix was unobtainable, precond is intended to stablise covariance step by reducing the R-matrix related computational issues, hence cannot remedy this issues with S matrix.\n";

#        }else
        if (int(log($maxEigen/$minEigen) / log(10)) < 3 and $negaCounter > 0) {
            print "\nCovariance step of the preconditioned model failed \n\nThe estimated parameter is mostlikely at the saddle of the likelihood surface. \n try -pre=$foldername -perturb option\n\n";
        } elsif($negaCounter > 0) {
            print "\nCovariance step of the preconditioned model failed \n\ntry -pre=$foldername option\n\n";
        } else {
            print "\nCovariance step of the preconditioned model failed \n\nTHETA part of the R-matrix is positive-definite hence the issues are from the OMEGA and SIGMA part of the Covariance Matrix.\n  Reparameterise OMEGA and SIGMA using THETAs and re-run preconditioning See section 4.1 of PRECOND user guide. \n\n.";
        }

        my @G;
        for (my $index1 = 0; $index1 < scalar(@eigenValMatrix); $index1++) {
            for (my $index2 = 0; $index2 < $index1; $index2++) {
                $G[$index1][$index2] = 0;
                $G[$index2][$index1] = 0;
            }
            $G[$index1][$index1] = 0;
        }

        return \@G;
    }
}

sub _read_matrix
{
    # Read either a nonmem .cov file or an ordinary csv file without header
    my $fh = shift;

    my @precMatrix;

    my $line = <$fh>;
    if ($line =~ /^TABLE NO./) {
        <$fh>;
        my $numtheta = 0;
        while (my $line = <$fh>) {
            chomp $line;
            my @fields = split(/\s+/, $line);
            shift @fields;
            my $a = shift @fields;
            if ($a =~ /^THETA/) {
                $numtheta++;
            }
            push @precMatrix, \@fields;
        }
        linear_algebra::reduce_matrix(\@precMatrix, $numtheta);
    } else {
        seek $fh, 0, 0;
        while (my $line = <$fh>) {
            chomp $line;
            my @fields = split(/,/, $line);
            push @precMatrix, \@fields;
        }
    }

    return @precMatrix;
}

sub read_precond_matrix
{
    my $self = shift;
    my $filename = shift;

if (defined $self->precond_matrix) {
    open(my $fh, '<', $filename)
            or die "Cannot find the R-matrix file.\nSpecify the R-matrix or the modelfit directory using -pre option\n";

}

    open(my $fh, '<', $filename)
                or die "\nCovariance step of the original model did not run (most likely the parameter estimation had failed).\n\nSee .lst file at ",$self->directory."m1/",utils::file::replace_extension($self->base_model->filename, 'lst')," for the reason.\n\n  Precond cannot help if the parameter estimation of the original model fails in a way that the NONMEM terminates before proceeding to the covariance step.\n\n";


    my @precond_matrix = _read_matrix($fh);

    $self->precond_matrix(\@precond_matrix);
}

sub preprocess_precond_matrix
{
    # Preprocess the preconditioning matrix
    # A matrix smaller than nthetas will get padded
    # A matrix larger than nthetas will get reduced
    # Rows that are all zeros will get ones on the diagonal

    my %parm = validated_hash(\@_,
        precond_matrix => { isa => 'ArrayRef[ArrayRef]' },
        nthetas => { isa => 'Int' },
    );
    my $precond_matrix = $parm{'precond_matrix'};
    my $nthetas = $parm{'nthetas'};

    # Pad the preconditioning matrix if it is too small
    if (scalar(@$precond_matrix) < $nthetas) {
        linear_algebra::pad_matrix($precond_matrix, $nthetas);
    } elsif (scalar(@$precond_matrix) > $nthetas) {
        linear_algebra::reduce_matrix($precond_matrix, $nthetas);
    }

    linear_algebra::put_ones_on_diagonal_of_zero_lines($precond_matrix);
}

sub eigenvalue_decomposition
{
    my $self = shift;

    my @eigenValMatrix = map { [@$_] } @{$self->precond_matrix};
    (my $eigen, my $Q) = linear_algebra::eigenvalue_decomposition(\@eigenValMatrix);

    my $abs_eigens = array::absolute($eigen);
    my $maxEigen = array::max($abs_eigens);
    my $minEigen = array::min($abs_eigens);

    my @negaEigIndex;
    my $negaCounter = 0;
    foreach my $index (0 .. scalar(@$eigen) - 1) {
        if ($eigen->[$index] < 0) {
            if ($self->perturb) {
                $negaEigIndex[$negaCounter] = $index;
            }
            $negaCounter++;
        }
    }

    $self->negaEigenIndex(\@negaEigIndex);

    print "\nCondition number of the R-matrix : 10^" . int(log($maxEigen / $minEigen) / log(10)) . "\n";
    print "Number of negative eigenvalues : $negaCounter\n";

    if ($self->verbose) {
        print "=== Eigenvalues ===\n";
        array::print($eigen);
        print "===================\n";
    }

    for (my $index1 = 0; $index1 < scalar(@$Q); $index1++) {
        for (my $index2 = 0; $index2 < scalar(@$Q); $index2++) {
            $self->precond_matrix->[$index1]->[$index2] = $Q->[$index1]->[$index2] / sqrt(abs($eigen->[$index2]));
        }
    }
}

1;
