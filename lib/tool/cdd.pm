package tool::cdd;

use include_modules;
use Cwd;
use File::Copy 'cp';
use tool::llp;
use tool::modelfit;
use random;
use Math::MatrixReal;
use Mouse;
use MouseX::Params::Validate;
use linear_algebra;
use math qw(usable_number);
use array qw(not_empty);

extends 'tool';

# Either the number or the number of the column. Will be converted to number by BUILD
has 'case_column' => ( is => 'rw', required => 1, isa => 'Value', default => 'ID' );
has 'bins' => ( is => 'rw', isa => 'Int' );
has 'actual_bins' => ( is => 'rw', isa => 'Int' );
has 'cook_scores' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'delta_ofv' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'parameter_cook_scores' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'jackknife_cook_scores' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'jackknife_parameter_cook_scores' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'labels_parameter_cook_scores' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'cdd_id' => ( is => 'rw', isa => 'Str' );
has 'covariance_ratios' => ( is => 'rw', isa => 'ArrayRef' );
has 'outside_n_sd' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'bca_mode' => ( is => 'rw', isa => 'Bool', default => 0);
has 'skipped_ids' => ( is => 'rw', isa => 'ArrayRef' );
has 'skipped_individuals_filename' => ( is => 'rw', isa => 'Str' );
has 'skipped_keys' => ( is => 'rw', isa => 'ArrayRef' );
has 'skipped_values' => ( is => 'rw', isa => 'ArrayRef' );
has 'selection_method' => ( is => 'rw', isa => 'Str' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['cdd.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'cdd_results.csv' );
has 'prediction_models' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'cross_validate' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'outside_n_sd_check' => ( is => 'rw', isa => 'Num', default => 2 );
has 'update_inits' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'etas' => ( is => 'rw', isa => 'Bool', default => 0 );      # If to add $ETAs from original model on all cdd models
has 'ignore' => ( is => 'rw', isa => 'Bool', default => 0 );    # Use IGNORE instead of generating new datasets

sub BUILD
{
    my $self = shift;

    my $input_column = $self->models->[0]->find_input_column(name => $self->case_column);
    if (not defined $input_column) {
        croak("Error finding column " . $self->case_column . " in \$INPUT of model\n");
    }
    $self->case_column($input_column);

    for my $accessor ('logfile','raw_results_file','raw_nonp_file'){
        my @new_files = ();
        my @old_files = ();
        @old_files = @{$self->$accessor} if (defined $self->$accessor);
        for (my $i = 0; $i < scalar(@old_files); $i++) {
            my $name;
            my $ldir;
            ($ldir, $name) = OSspecific::absolute_path($self->directory, $old_files[$i]);
            push(@new_files,$ldir.$name) ;
        }
        $self->$accessor(\@new_files);
    }
}

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    $self->general_setup( model_number => $model_number,
        class        => 'tool::modelfit',
        subm_threads => $self->threads );
}

sub llp_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    my @subm_threads;
    if (ref($self->threads) eq 'ARRAY') {
        @subm_threads = @{$self->threads};
    } else {
        @subm_threads = ($self->threads);
    }
    $self->general_setup( model_number => $model_number,
        class        => 'tool::llp',
        subm_threads => \@subm_threads );
}

sub modelfit_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    # Only valid for one problem and one sub problem.

    if ($self->cross_validate) {

        # ---  Evaluate the models on the remainder data sets  ----

        for ( my $i = 0;
            $i < scalar @{$self -> prediction_models->[$model_number-1]{'own'}};
            $i++ ) {
            $self -> prediction_models->[$model_number-1]{'own'}[$i] ->
                update_inits( from_model => $self ->
                              prepared_models->[$model_number-1]{'own'}[$i]);
            $self -> prediction_models->[$model_number-1]{'own'}[$i] ->
                remove_records( type => 'covariance' );
            $self -> prediction_models->[$model_number-1]{'own'}[$i] -> _write(overwrite => 1);
        }
        my ($dir,$file) =
            OSspecific::absolute_path( $self -> directory,
                                       $self -> raw_results_file->[$model_number-1] );
        my $xv_threads = ref( $self -> threads ) eq 'ARRAY' ? $self -> threads -> [1]:$self -> threads;
        my $mod_eval = tool::modelfit->new( %{common_options::restore_options(@common_options::tool_options)},
                                            copy_data  => 0,  #do not copy models to NM_run, use rel path to m1
                                            models           => $self -> prediction_models->[$model_number-1]{'own'},
                                            directory => 'crossval',
                                            base_directory   => $self -> directory,
                                            nmtran_skip_model => 2,
                                            threads          => $xv_threads,
                                            _raw_results_callback => $self -> _modelfit_raw_results_callback( model_number => $model_number,
                                                                                                              cross_validation_set => 1 ),
                                            parent_tool_id   => $self -> tool_id,
                                            logfile        => undef,
                                            raw_results      => undef,
                                            prepared_models  => undef,
                                            top_tool         => 0);
        print "Running xv runs\n";
        $mod_eval -> run;

    }

    # ------------  Cook-scores and Covariance-Ratios  ----------

    my $do_pca = 1;
    ui -> print( category => 'cdd',
                 message  => "Calculating diagnostics" );

    my $problem_index = 0;
    my $b = $self->bins;
    $b=$self->actual_bins unless (defined $b);
    my ($cook_scores,$cov_ratios,$parameter_cook_scores,$relative_changes,$bias, $relative_bias,
        $jackknife_cook_scores,$jackknife_parameter_cook_scores,$jackknife_full_cov,$sample_ofvs) =
            cook_scores_and_cov_ratios(original => $self->models->[$model_number-1]->outputs -> [0],
                                       cdd_models => $self -> prepared_models->[$model_number-1]{'own'},
                                       problem_index => $problem_index,
                                       bins => $b);

    my ($delta_ofvs,$message) = get_delta_ofv(output => $self->models->[$model_number-1]->outputs -> [0],
                                              problem_index => $problem_index,
                                              sample_ofv => $sample_ofvs,
                                              skipped_keys => $self->skipped_keys) if (scalar(@{$sample_ofvs})>0);
    $self ->delta_ofv($delta_ofvs) if (defined $delta_ofvs);
    $self -> cook_scores($cook_scores);
    $self -> parameter_cook_scores($parameter_cook_scores);
    $self -> jackknife_cook_scores($jackknife_cook_scores);
    $self -> jackknife_parameter_cook_scores($jackknife_parameter_cook_scores);
    $self -> covariance_ratios($cov_ratios);
    $do_pca = 0 unless (scalar(@{$cook_scores})>0 and usable_number($cook_scores->[0]));

    my @relative_changes_labels = ('ofv');
    my $labelsref = $self->models->[$model_number-1]->problems->[$problem_index]->get_estimated_attributes(attribute => 'labels');
    push(@relative_changes_labels,@{$labelsref}); #estimates
    my @labels_parameter_cook_scores = ();
    foreach my $lab (@{$labelsref}){
        push(@relative_changes_labels,'se'.$lab);
        push(@labels_parameter_cook_scores,'"cook.par.'.$lab.'"');
    }
    foreach my $lab (@{$labelsref}){
        push(@labels_parameter_cook_scores,'"jack.cook.par.'.$lab.'"');
    }
    $self->labels_parameter_cook_scores(\@labels_parameter_cook_scores);
    ui -> print( category => 'cdd',
                 message  => " ... done." );

    if(    defined $jackknife_full_cov and scalar(@{$jackknife_full_cov})>0){
        #FIXME use format covmatrix, reorder lines
        my $formatted = tool::format_covmatrix(matrix => $jackknife_full_cov,
                                               header => $labelsref,
                                               comma => 1,
                                               print_labels => 1);

        open( COV, ">".$self -> directory.'/jackknife.cov' );
        foreach my $line (@{$formatted}){
            print COV $line;
        }
        close(COV);
    }

    # -  Perform a PCA on the cook-score:covariance-ratio data --

    for (my $i=0; $i< scalar(@{$cov_ratios}); $i++){
        #replace undef cov ratio with 0, determinant of covmatrix 0 when covstep failed
        $cov_ratios->[$i]=0 unless (defined $cov_ratios->[$i]);
    }
    my @outside_n_sd;

    if( $self -> models -> [$model_number-1] ->
        outputs -> [0] -> get_single_value(attribute=> 'covariance_step_successful')
            and $do_pca){
        my ( $eig_ref, $eig_vec_ref, $proj_ref, $std_ref ) =
            $self -> pca( data_matrix => [$cook_scores,$cov_ratios] );
        my @projections = @{$proj_ref};
        my @standard_deviation = @{$std_ref};

        # ----  Mark the runs with CS-CR outside N standard deviations of the PCA ----

        for( my $i = 0; $i <= $#projections; $i++ ) {
            my $vector_length = 0;
            for( my $j = 0; $j <= 1; $j++ ) {
                $vector_length += $projections[$i][$j]**2;
            }
            $vector_length = sqrt( $vector_length );
            my $n_sd = 0;
            for( my $j = 0; $j <= 1; $j++ ) {
                $n_sd += (($projections[$i][$j]/$vector_length)*$standard_deviation[$j])**2;
            }
            $n_sd = ( $self -> outside_n_sd_check * sqrt( $n_sd ) );
            $outside_n_sd[$i] = $vector_length > $n_sd ? 1 : 0;
        }
    }

    $self -> outside_n_sd(\@outside_n_sd);

    my %run_info_return_section;
    my @run_info_labels=('Date','PsN version','NONMEM version');
    my @datearr=localtime;
    my $the_date=($datearr[5]+1900).'-'.($datearr[4]+1).'-'.($datearr[3]);
    my @run_info_values =($the_date,'v'.$PsN::version,'v'.$self->nm_version);
    $run_info_return_section{'labels'} =[[],\@run_info_labels];
    $run_info_return_section{'values'} = [\@run_info_values];

    my %covariance_return_section;
    $covariance_return_section{'name'} = 'Diagnostics';
    $covariance_return_section{'labels'} = [[],['cook.scores','jackknife.cook.scores','covariance.ratios','outside.n.sd']];

    my @res_array;
    for( my $i = 0; $i < scalar(@{$cov_ratios}); $i ++ ){
        push( @res_array , [$cook_scores->[$i],$jackknife_cook_scores->[$i],$cov_ratios->[$i],$outside_n_sd[$i]] );
    }

    $covariance_return_section{'values'} = \@res_array;

    push( @{$self -> results->[$model_number-1]{'own'}},\%covariance_return_section );

    my %return_section;
    $return_section{'name'} = 'relative.changes.percent';
    $return_section{'labels'} = [[],[]];
    push( @{$return_section{'labels'} -> [1]},@relative_changes_labels); #including ofv and se

    my %bias_return_section;
    $bias_return_section{'name'} = 'Jackknife.bias.estimate';
    $bias_return_section{'labels'} = [['bias','relative.bias.percent'],[]];
    push(@{$bias_return_section{'labels'} -> [1]},@{$labelsref}); #only estimates

    $bias_return_section{'values'} = [$bias,$relative_bias];

    $return_section{'values'} = $relative_changes ;
    push( @{$self -> results->[$model_number-1]{'own'}},\%return_section );
    push( @{$self -> results->[$model_number-1]{'own'}},\%bias_return_section );

    $self -> update_raw_results(model_number => $model_number);

    # experimental: to save memory
    $self -> prepared_models->[$model_number-1]{'own'} = undef;
}

sub get_ofv_estimates_se
{
    my %parm = validated_hash(\@_,
        cdd_models => { isa => 'ArrayRef', optional => 0 },
        problem_index => { isa => 'Int', optional => 1, default => 0 },
    );
    my $cdd_models = $parm{'cdd_models'};
    my $problem_index = $parm{'problem_index'};

    my @ofv=();
    my @estimates=();
    my @se=();
    my @root_cov_det=();
    my $successful_estimates=0;
    foreach my $model (@{$cdd_models}){
        my $outobj = $model->outputs->[0] if (defined $model->outputs);
        my $sample_ofv=undef;
        my $root_det=undef;
        my @sample_estimates=();
        my @sample_se=();
        if (defined $outobj and $outobj -> parsed_successfully){
            unless ( not_empty($outobj->problems) ) {
                $outobj -> _read_problems;
            }
            $sample_ofv = $outobj -> get_single_value(attribute => 'ofv', problem_index => $problem_index);
            if (defined $sample_ofv and usable_number($sample_ofv)){
                my $sample_est = $outobj->get_filtered_values(category => 'estimate', problem_index => $problem_index);
                if (usable_number($sample_est->[0])){
                    $successful_estimates++;
                    @sample_estimates = @{$sample_est};
                    if ($outobj->get_single_value(attribute =>'covariance_step_successful',problem_index => $problem_index)){
                        my $ref = $outobj->get_filtered_values(category => 'se', problem_index => $problem_index);
                        @sample_se=@{$ref};
                        my $covmat=$outobj->get_single_value(attribute=>'covariance_matrix',problem_index => $problem_index);
                        $root_det = linear_algebra::sqrt_determinant_vector_covmatrix($covmat);
                    }
                }
            }else{
                $sample_ofv=undef;
            }
        }
        push(@ofv,$sample_ofv);
        push(@estimates,\@sample_estimates);
        push(@se,\@sample_se);
        push(@root_cov_det,$root_det);
    }
    return (\@ofv,\@estimates,\@se,\@root_cov_det,$successful_estimates);
}

sub get_delta_ofv
{
    my %parm = validated_hash(\@_,
        output => { isa => 'output', optional => 0 },
        problem_index  => { isa => 'Int', default => 0 },
        table_index  => { isa => 'Int', default => -1 },
        skipped_keys => { isa => 'ArrayRef', optional => 0 },
        sample_ofv => { isa => 'ArrayRef', optional => 0 },
    );
    my $output = $parm{'output'};
    my $problem_index = $parm{'problem_index'};
    my $table_index = $parm{'table_index'};
    my $skipped_keys = $parm{'skipped_keys'};
    my $sample_ofv = $parm{'sample_ofv'};

    my $error = 0;
    my $message = '';

    my $filename = $output->problems->[$problem_index]->full_name_NM7_file(file_type => 'phi');
    my $orig_ofv = $output->get_single_value(attribute => 'ofv',problem_index => $problem_index);

    unless (length($filename)> 0){
        $error = 2;
        $message .= 'Empty phi file name';
    }
    unless (-e $filename){
        $error = 2;
        $message .= ' File '.$filename.' does not exist';
    }

    return([],$message) unless ($error == 0);

    my $nmtablefile = nmtablefile->new(filename => $filename);
    my @sorted_iofv = ();
    my $iofv = $nmtablefile->tables->[$table_index]->get_column(name=> 'OBJ');

    for (my $i=0; $i < scalar(@{$skipped_keys}); $i++){
        my $sum=0;
        #usually each case is just one key, but can be many
        foreach my $key (@{$skipped_keys->[$i]}){
            $sum += $iofv->[$key];
        }
        push(@sorted_iofv,$sum);
    }
    my @delta_ofv=();
    for (my $i=0; $i < scalar(@sorted_iofv); $i++){
        if (defined $sample_ofv->[$i]){
            push(@delta_ofv,($orig_ofv-$sorted_iofv[$i]-$sample_ofv->[$i]));
        }else{
            push(@delta_ofv,undef);
        }
    }
    return (\@delta_ofv,'');
}

sub cook_scores_and_cov_ratios
{
    #static
    #compute relative changes in percent of ofv, estimates and se:s
    #fixme do a rawres callback instead, add determinant of covmatrix and cook score there.
    my %parm = validated_hash(\@_,
                              original => { isa => 'output', optional => 0 },
                              cdd_models => { isa => 'ArrayRef', optional => 0 },
                              problem_index => { isa => 'Int', optional => 1, default => 0 },
                              bins => { isa => 'Int', optional => 0 },
    );
    my $original = $parm{'original'};
    my $cdd_models = $parm{'cdd_models'};
    my $problem_index = $parm{'problem_index'};
    my $bins = $parm{'bins'};

    my $minimum_success=1; #require all successful

    my $original_standard_errors=undef;
    my $original_estimates=undef;
    my $orig_ofv=undef;
    my $original_inverse_cholesky=undef;
    my $err;
    my $original_sqrt_inv_determinant =undef;
    my $have_original_cov=0;
    my $have_jackknife_cov=0;
    if ($original->have_output and $original -> parsed_successfully){
        unless ( not_empty($original->problems) ) {
            $original -> _read_problems;
        }

        $original_estimates = $original->get_filtered_values(category => 'estimate', problem_index => $problem_index);
        $orig_ofv = $original->get_single_value(attribute => 'ofv',problem_index => $problem_index);
        if ($original-> get_single_value(attribute => 'covariance_step_run', problem_index => $problem_index) and
            $original-> get_single_value(attribute => 'covariance_step_successful', problem_index => $problem_index)){

            $original_standard_errors = $original->get_filtered_values(category => 'se', problem_index => $problem_index);
            if ( defined $original_standard_errors and usable_number($original_standard_errors->[0])){
                my $invcovmat = $original->get_single_value(attribute => 'inverse_covariance_matrix',
                                                            problem_index => $problem_index);
                ($err,$original_inverse_cholesky) = linear_algebra::cholesky_of_vector_matrix($invcovmat);
                if ($err == 0){
                    $original_sqrt_inv_determinant= linear_algebra::diagonal_product($original_inverse_cholesky);
                    $have_original_cov=1;
                } else {
                    print "Warning: the inverse of the covariance matrix of the original model is not positive definite. Cook scores cannot be calculated\n";
                }
            }else{
                $original_standard_errors = undef;
            }
        }
    }

    unless (defined $orig_ofv and defined $original_estimates and usable_number($original_estimates->[0])){
        ui->print(category => 'cdd',
                  message => "Cannot compute Cook scores and cov-ratios, no estimates or ofv from input model");
        return ([],[],[],[],[],[],[],[],[],[]);
    }

    my ($sample_ofvs,$sample_estimates,$sample_ses,$sample_root_det,$successful_estimates) =
        get_ofv_estimates_se(cdd_models => $cdd_models, problem_index => $problem_index);

    my $jackknife_means=undef;
    my $jackknife_standard_errors=undef;
    my $jackknife_inverse_cholesky=undef;
    my $jackknife_sqrt_inv_determinant =undef;
    my $jackknife_full_cov=undef;
    if ($successful_estimates >= scalar(@{$sample_ofvs})*$minimum_success){
        $jackknife_means=[];
        $jackknife_standard_errors=[];
        $jackknife_inverse_cholesky=[];
        $jackknife_full_cov=[];
        ($err,$jackknife_sqrt_inv_determinant)= linear_algebra::jackknife_inv_cholesky_mean_det($sample_estimates,
                                                                                                $jackknife_inverse_cholesky,
                                                                                                $jackknife_means,
                                                                                                $jackknife_standard_errors,
                                                                                                $jackknife_full_cov);
        $have_jackknife_cov=1 if ($err==0);
    }else{
        if (not defined $original_sqrt_inv_determinant) {
            ui->print(category => 'cdd',
                      message => "No covariance step of input model and too many cdd samples failed. ".
                      "Cannot compute Cook scores and cov-ratios");
            return ([],[],[],[],[],[],[],[],[],[]);
        }
    }
    my $npar = scalar(@{$original_estimates});

    my @original_cook=();
    my @original_cov_ratios = ();
    my @original_parameter_cook = ();
    my @jackknife_cook=();
    my @jackknife_parameter_cook = ();
    my @all_relative_changes = ();

    my @sum_estimates = (0) x $npar;

    for (my $index=0; $index < scalar(@{$sample_ofvs}); $index++){
        my $orig_cook=undef;
        my $jack_cook=undef;
        my $ratio=undef;
        my @original_param = (undef) x $npar;
        my @jackknife_param = (undef) x $npar;
        my @relative_changes = ('') x (2*$npar +1); #ofv, par, se_par
        my $original_individual_cook;
        my $jackknife_individual_cook;

        if (defined $sample_ofvs->[$index]){
            $relative_changes[0] = 100*($sample_ofvs->[$index]-$orig_ofv)/$orig_ofv;
            if (scalar(@{$sample_estimates->[$index]})>0){
                #individual cook all params
                if ($have_original_cov){
                    ($err,$original_individual_cook) = linear_algebra::cook_score_parameters($original_standard_errors,
                                                                                             $sample_estimates->[$index],
                                                                                             $original_estimates);
                    @original_param = @{$original_individual_cook} if ($err==0);
                    ($err,$orig_cook) = linear_algebra::cook_score_all($original_inverse_cholesky,
                                                                       $sample_estimates->[$index],
                                                                       $original_estimates);
                }
                if ($have_jackknife_cov){
                    ($err,$jackknife_individual_cook) = linear_algebra::cook_score_parameters($jackknife_standard_errors,
                                                                                              $sample_estimates->[$index],
                                                                                              $original_estimates);
                    @jackknife_param = @{$jackknife_individual_cook} if ($err==0);
                    ($err,$jack_cook) = linear_algebra::cook_score_all($jackknife_inverse_cholesky,
                                                                       $sample_estimates->[$index],
                                                                       $original_estimates);
                }
                for (my $j=0; $j<$npar; $j++){
                    $relative_changes[$j+1] =
                        100*($sample_estimates->[$index]->[$j]-$original_estimates->[$j])/($original_estimates->[$j]);
                }
                if (scalar(@{$sample_ses->[$index]})>0){
                    #if covstep successful for sample then
                    if ($have_original_cov){
                        for (my $j=0; $j<$npar; $j++){
                            $relative_changes[$j+1+$npar] =
                                100*($sample_ses->[$index]->[$j] - $original_standard_errors->[$j])/($original_standard_errors->[$j]);
                        }
                        $ratio = $sample_root_det->[$index] * $original_sqrt_inv_determinant if (defined $sample_root_det->[$index]);
                    }
                }
            }
        }
        push(@original_cook,$orig_cook);
        push(@jackknife_cook,$jack_cook);
        push(@original_cov_ratios,$ratio);
        push(@original_parameter_cook,\@original_param);
        push(@jackknife_parameter_cook,\@jackknife_param);
        push(@all_relative_changes,\@relative_changes);
    }

    my @all_bias=();
    my @rel_bias=();
    if ($successful_estimates >0 and scalar(@{$jackknife_means})>0){
        for (my $j=0; $j<$npar; $j++){
            my $bias=($bins-1)*($jackknife_means->[$j]-$original_estimates->[$j]);
            push(@all_bias,$bias);
            push(@rel_bias,100*$bias/$original_estimates->[$j]);
        }
    }
    return (\@original_cook,\@original_cov_ratios,\@original_parameter_cook,\@all_relative_changes,\@all_bias,
            \@rel_bias,\@jackknife_cook,\@jackknife_parameter_cook,$jackknife_full_cov,$sample_ofvs);
}

sub llp_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    print "POSTFORK\n";
    my %proc_results;

    push( @{$self -> results -> {'own'}}, \%proc_results );
}

sub modelfit_results
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        accessor => { isa => 'Str', optional => 1 },
        format => { isa => 'Str', optional => 1 }
    );
    my $accessor = $parm{'accessor'};
    my $format = $parm{'format'};
    my @results;

    my @orig_models  = @{$self -> models};
    my @orig_raw_results = ();
    foreach my $orig_model ( @orig_models ) {
        my $orig_output = $orig_model -> outputs -> [0];
        push( @orig_raw_results, $orig_output -> $accessor );
    }
    my @outputs = @{$self -> results};

    my @raw_results = ();

    foreach my $mod ( @outputs ) {
        my @raw_inner = ();
        foreach my $output ( @{$mod -> {'subset_outputs'}} ) {
            push( @raw_inner, $output -> $accessor );
        }
        push( @raw_results, \@raw_inner );
    }
    if ( $format eq 'relative' or $format eq 'relative_percent' ) {
        @results = ();
        for ( my $i = 0; $i <= $#orig_raw_results; $i++ ) {
            print "Model\t$i\n";
            my @rel_subset = ();
            for ( my $i2 = 0; $i2 < scalar @{$raw_results[$i]}; $i2++ ) {
                print "Subset Model\t$i2\n";
                my @rel_prob = ();
                for ( my $j = 0; $j < scalar @{$orig_raw_results[$i]}; $j++ ) {
                    print "Problem\t$j\n";
                    if( ref( $orig_raw_results[$i][$j] ) eq 'ARRAY' ) {
                        my @rel_subprob = ();
                        for ( my $k = 0; $k < scalar @{$orig_raw_results[$i][$j]}; $k++ ) {
                            print "Subprob\t$k\n";
                            if( ref( $orig_raw_results[$i][$j][$k] ) eq 'ARRAY' ) {
                                my @rel_instance = ();
                                for ( my $l = 0; $l < scalar @{$orig_raw_results[$i][$j][$k]}; $l++ ) {
                                    print "Instance\t$l\n";
                                    my $orig = $orig_raw_results[$i][$j][$k][$l];
                                    my $res  = $raw_results[$i][$i2][$j][$k][$l];
                                    if( defined $orig and ! $orig == 0 ) {
                                        print "ORIGINAL $orig\n";
                                        print "SUBSET   $res\n";
                                        print "RELATIVE ",$res/$orig,"\n";
                                        if ( $format eq 'relative_percent' ) {
                                            push( @rel_instance, ($res/$orig-1)*100 );
                                        } else {
                                            push( @rel_instance, $res/$orig );
                                        }
                                    } else {
                                        push( @rel_instance, 'NA' );
                                    }
                                    push( @rel_subprob,\@rel_instance );
                                }
                            } elsif( ref( $orig_raw_results[$i][$j][$k] ) eq 'SCALAR' ) {
                                print "One instance per problem\n";
                                my $orig = $orig_raw_results[$i][$j][$k];
                                my $res  = $raw_results[$i][$i2][$j][$k];
                                if( defined $orig and ! $orig == 0 ) {
                                    print "ORIGINAL $orig\n";
                                    print "SUBSET   $res\n";
                                    print "RELATIVE ",$res/$orig,"\n";
                                    if ( $format eq 'relative_percent' ) {
                                        push( @rel_subprob, ($res/$orig-1)*100 );
                                    } else {
                                        push( @rel_subprob, $res/$orig );
                                    }
                                } else {
                                    push( @rel_subprob, 'NA' );
                                }
                            } else {
                                print "WARNING: tool::cdd -> modelfit_results: neither\n\t".
                                "array or scalar reference found at layer 4 in result data\n\t".
                                "structure (found ",ref( $orig_raw_results[$i][$j][$k] ),")\n";
                            }
                        }
                        push( @rel_prob, \@rel_subprob );
                    } elsif( ref( $orig_raw_results[$i][$j] ) eq 'SCALAR' ) {
                        print "One instance per problem\n";
                        my $orig = $orig_raw_results[$i][$j];
                        my $res  = $raw_results[$i][$i2][$j];
                        if( defined $orig and ! $orig == 0 ) {
                            print "ORIGINAL $orig\n";
                            print "SUBSET   $res\n";
                            print "RELATIVE ",$res/$orig,"\n";
                            if ( $format eq 'relative_percent' ) {
                                push( @rel_prob, ($res/$orig-1)*100 );
                            } else {
                                push( @rel_prob, $res/$orig );
                            }
                        } else {
                            push( @rel_prob, 'NA' );
                        }
                    } else {
                        print "WARNING: tool::cdd -> modelfit_results: neither\n\t".
                        "array or scalar reference found at layer 3 in result data\n\t".
                        "structure (found ",ref( $orig_raw_results[$i][$j] ),")\n";
                    }
                }
                push( @rel_subset, \@rel_prob );
            }
            push( @results, \@rel_subset );
        }
    } else {
        @results = @raw_results;
    }

    return \@results;
}

sub general_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        class => { isa => 'Str', optional => 1 },
        subm_threads => { isa => 'Any' , optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my $class = $parm{'class'};

    # Sub tool threads can be given as scalar or reference to an array?
    my $subm_threads = $parm{'subm_threads'};
    my $own_threads = ref( $self -> threads ) eq 'ARRAY' ?
    $self -> threads -> [0]:$self -> threads;
    # case_column names are matched in the model, not the data!

    my $model = $self->models->[$model_number-1];

    # Check which models that hasn't been run and run them
    # This will be performed each step but will only result in running
    # models at the first step, if at all.

    # If more than one process is used, there is a VERY high risk of interaction
    # between the processes when creating directories for model fits. Therefore
    # the directory attribute is given explicitly below.

    if (not $model->is_run or not -e $model->outputs->[0]->problems->[0]->full_name_NM7_file(file_type => 'phi')) {

        # Run original run

        my %subargs = ();
        if ( defined $self -> subtool_arguments ) {
            %subargs = %{$self -> subtool_arguments};
        }
        if( $self -> nonparametric_etas or
            $self -> nonparametric_marginals ) {
            $model -> add_nonparametric_code;
        }

        my $orig_fit = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
            base_directory => $self->base_directory,
            models                => [$model],
            threads               => 1,
            directory             => $self -> directory.'/orig_modelfit_dir'.$model_number,
            subtools              => [],
            parent_threads        => $own_threads,
            parent_tool_id        => $self -> tool_id,
            logfile             => undef,
            raw_results           => undef,
            prepared_models       => undef,
            top_tool              => 0,
            %subargs,
            copy_up => 1);

        ui -> print( category => 'cdd',
            message => 'Executing base model.' );

        $orig_fit->add_to_nmoutput(extensions => ['phi','ext','cov','coi']);
        $orig_fit -> run;
        $self->metadata->{'copied_files'} = $orig_fit->metadata->{'copied_files'};
    }

    unless ( $model -> outputs -> [0] -> have_output ) {
        if ($self->update_inits){
            ui -> print( category => 'cdd',
                         message => 'There is no output from the base model, will use base model initial values for cdd models.' );
        }
    }

    # ------------------------  Print a log-header  -----------------------------

    open( LOG, ">>".$self -> logfile->[$model_number-1] );
    my $ui_text = sprintf("%-5s",'RUN').','.sprintf("%20s",'FILENAME  ').',';
    print LOG sprintf("%-5s",'RUN'),',',sprintf("%20s",'FILENAME  '),',';
    foreach my $param ( 'ofv', 'theta', 'omega', 'sigma' ) {
        my $orig_ests;
        my $name = $param;
        if ($param eq 'ofv'){
            $orig_ests   = $model -> outputs -> [0] -> ofv();
            $name = 'DIC'
            if (defined $model -> outputs -> [0]->get_single_value(attribute => 'dic'));
        }else{
            $orig_ests   = $model -> get_values_to_labels(category => $param);
        }
        # Loop the problems
        if( defined $orig_ests ){
            for ( my $j = 0; $j < scalar @{$orig_ests}; $j++ ) {
                if ( $param eq 'ofv' ) {
                    my $label = uc($name)."_".($j+1);
                    $ui_text = $ui_text.sprintf("%12s",$label).',';
                    print LOG sprintf("%12s",$label),',';
                } else {
                    # Loop the parameter numbers (skip sub problem level)
                    if( defined $orig_ests -> [$j] and
                        defined $orig_ests -> [$j][0] ){
                        for ( my $num = 1; $num <= scalar @{$orig_ests -> [$j][0]}; $num++ ) {
                            my $label = uc($param).$num."_".($j+1);
                            $ui_text = $ui_text.sprintf("%12s",$label).',';
                            print LOG sprintf("%12s",$label),',';
                        }
                    }
                }
            }
        }
    }

    print LOG "\n";

    # ------------------------  Log original run  -------------------------------

    open( LOG, ">>".$self -> logfile->[$model_number-1] );
    $ui_text = sprintf("%5s",'0').','.sprintf("%20s",$model -> filename).',';
    print LOG sprintf("%5s",'0'),',',sprintf("%20s",$model -> filename),',';
    foreach my $param ( 'ofv', 'theta', 'omega', 'sigma' ) {
        my $orig_ests;
        if ($param eq 'ofv'){
            $orig_ests   = $model -> outputs -> [0] -> ofv();
        }else{
            $orig_ests   = $model -> get_values_to_labels(category => $param);
        }
        # Loop the problems
        if( defined $orig_ests ) {
            for ( my $j = 0; $j < scalar @{$orig_ests}; $j++ ) {
                if ( $param eq 'ofv' ) {
                    $ui_text = $ui_text.sprintf("%12f",$orig_ests -> [$j][0]).',';
                    print LOG sprintf("%12f",$orig_ests -> [$j][0]),',';
                } else {
                    # Loop the parameter numbers (skip sub problem level)
                    if( defined $orig_ests -> [$j] and
                        defined $orig_ests -> [$j][0] ){
                        for ( my $num = 0; $num < scalar @{$orig_ests -> [$j][0]}; $num++ ) {
                            $ui_text = $ui_text.sprintf("%12f",$orig_ests -> [$j][0][$num]).',';
                            print LOG sprintf("%12f",$orig_ests -> [$j][0][$num]),',';
                        }
                    }
                }
            }
        }
    }

    print LOG "\n";

    # ---------------------  Initiate some variables ----------------------------

    # Case-deletion Diagnostics will only work for models with one problem.
    my $datafiles = $model->datafiles(absolute_path => 1);
    my ( $junk, $idcol ) = $model -> _get_option_val_pos( name            => 'ID',
                                                          record_name     => 'input',
                                                          problem_numbers => [1] );
    unless (defined $idcol->[0][0]){
        croak( "Error finding column ID in \$INPUT of model\n");
    }
    my $ignoresign = defined $model -> ignoresigns ? $model -> ignoresigns -> [0] : undef;

    my @new_models = ();

    my ( @skipped_ids, @skipped_keys, @skipped_values );

    my $done = ( -e $self -> directory."/m$model_number/done" ) ? 1 : 0;

    my ( @seed, $new_datas, $skip_ids, $skip_keys, $skip_values, $remainders, $pr_bins );

    if ( not $done ) {
        # --------------  Create new case-deleted data sets  ----------------------

        my $output_directory = $self -> directory.'/m'.$model_number;
        ($new_datas, $skip_ids, $skip_keys, $skip_values, $remainders) =
            data::cdd_create_datasets(
                input_filename => $datafiles->[0],
                bins => $self->bins,
                case_column => $self->case_column,
                selection_method => $self->selection_method,
                output_directory => $output_directory,
                ignoresign => $ignoresign,
                idcolumn => $idcol->[0][0],  #number not index
                missing_data_token => $self->missing_data_token,
                model => $model,
                ignore => $self->ignore,
            );

        my $ndatas = scalar @{$new_datas};
        $self->actual_bins($ndatas);

        my $templatemodel = $model -> copy( filename => $output_directory.'/template.mod',
                                           copy_datafile   => 0,
                                           output_same_directory => 1,
                                           write_copy => 0,
                                           copy_output => 0);
        if ($model->outputs->[0]->have_output and $self->update_inits){
            $templatemodel -> update_inits( from_output => $model->outputs->[0] );
        }

        for ( my $j = 1; $j <= $ndatas; $j++ ) {
            my @datasets = ( $new_datas -> [$j-1], $remainders -> [$j-1] );
            my @names = ('cdd_'.$j,'rem_'.$j);
            foreach my $i ( 0, 1 ) {
                my $set = $datasets[$i];
                my $newmodel = $templatemodel->copy(
                    filename => $output_directory . '/' . $names[$i] . '.mod',
                    copy_datafile => 0,
                    output_same_directory => 1,
                    write_copy => 0,
                    copy_output => 0
                );
                if ($self->ignore) {        # Check if we need to reverse INPUT and DATA
                    my $need_swap = 0;
                    my $found_input;
                    for my $record (@{$newmodel->problems->[0]->own_print_order}) {
                        if ($record eq 'input') {
                            $found_input = 1;
                        }
                        if ($record eq 'data' and not $found_input) {
                            $need_swap = 1;
                        }
                    }
                    if ($need_swap) {
                        for my $record (@{$newmodel->problems->[0]->own_print_order}) {
                            if ($record eq 'input') {
                               $record = 'data';
                           } elsif ($record eq 'data') {
                                $record = 'input';
                           }
                        }
                    }
                }
                if ($self->etas) {
                    my $phi_file = $model->get_phi_file();
                    if (defined $phi_file) {
                       $newmodel->init_etas(phi_name => $phi_file);
                    }
                }
                if (not $self->ignore) {
                    $newmodel -> datafiles( new_names => [$set] );
                } else {
                    my $verb;
                    my $op;

                    my $ign_acc = $model->problems->[0]->datas->[0]->have_ignore_accept();  # Do we already have ignore or accept in model?

                    if ($i == 0) {
                        if ($ign_acc != 2) {
                            $verb = 'IGNORE';
                            $op = '.EQN.';
                        } else {
                            $verb = 'ACCEPT';
                            $op = '.NEN.';
                        }
                    } else {
                        if ($ign_acc != 1) {
                            $verb = 'ACCEPT';
                            $op = '.EQN.';
                        } else {
                            $verb = 'IGNORE';
                            $op = '.NEN.';
                        }
                    }
                    my @expressions;
                    for my $id (@{$skip_ids->[$j - 1]}) {
                        push @expressions, "ID$op" . int($id);    #Assume integer IDs
                    }
                    my $expression_list = join ",", @expressions;
                    my $statement = "($expression_list)";
                    $newmodel->add_option(record_name => 'data', option_name => $verb, option_value => $statement);
                }
                if ($i == 1) {
                    # set MAXEVAL=0. Again, CDD will only work for one $PROBLEM
                    my $warn = 0;
                    $warn = 1 if ($j == 1);
                    my $ok = $newmodel->set_maxeval_zero(
                        need_ofv => 1,
                        print_warning => $warn,
                        niter_eonly => $self->niter_eonly,
                        last_est_complete => $self->last_est_complete(),
                    );
                }

                if ($self->nonparametric_etas or $self->nonparametric_marginals) {
                    $newmodel->add_nonparametric_code;
                }

                $newmodel -> _write;
                push( @{$new_models[$i]}, $newmodel );
            }
        }

        # Create a checkpoint. Log the samples and individuals.
        open( DONE, ">".$self -> directory."/m$model_number/done" ) ;
        print DONE "Sampling from ",$model->datafiles()->[0], " performed\n";
        print DONE "$pr_bins bins\n" if (defined $pr_bins);
        print DONE "Skipped individuals:\n";
        for( my $k = 0; $k < scalar @{$skip_ids}; $k++ ) {
            print DONE join(',',@{$skip_ids -> [$k]}),"\n";
        }
        print DONE "Skipped keys:\n";
        for( my $k = 0; $k < scalar @{$skip_keys}; $k++ ) {
            print DONE join(',',@{$skip_keys -> [$k]}),"\n";
        }
        print DONE "Skipped values:\n";
        for( my $k = 0; $k < scalar @{$skip_values}; $k++ ) {
            print DONE join(',',@{$skip_values -> [$k]}),"\n";
        }
        @seed = random_get_seed;
        print DONE "seed: @seed\n";
        close( DONE );
        $self->skipped_individuals_filename("skipped_individuals".$model_number.".csv");
        open( SKIP, ">".$self -> directory.$self->skipped_individuals_filename ) ;
        for( my $k = 0; $k < scalar @{$skip_ids}; $k++ ) {
            print SKIP join(',',@{$skip_ids -> [$k]}),"\n";
        }
        close( SKIP );
        open( SKIP, ">".$self -> directory."skipped_keys".$model_number.".csv" ) ;
        for( my $k = 0; $k < scalar @{$skip_keys}; $k++ ) {
            print SKIP join(',',@{$skip_keys -> [$k]}),"\n";
        }
        close( SKIP );
        open( SKIP, ">".$self -> directory."skipped_values".$model_number.".csv" ) ;
        for( my $k = 0; $k < scalar @{$skip_values}; $k++ ) {
            print SKIP join(',',@{$skip_values -> [$k]}),"\n";
        }
        close( SKIP );

        # }}} create new
        $self->skipped_keys($skip_keys);

    } else {

        # ---------  Recreate the datasets and models from a checkpoint  ----------

        #need stored_bins and skipped_keys

        ui -> print( category => 'cdd',
            message  => "Recreating models from a previous run" );
        open( DONE, $self -> directory."/m$model_number/done" );
        my @rows = <DONE>;
        close( DONE );
        my ( $junk, $junk1, $stored_filename, $junk2 ) = split(' ',$rows[0],4); #sampling from data performed
        my ( $stored_bins, $junk3 );
        my $start;
        if ($rows[1] =~ / bins/){
            ( $stored_bins, $junk3 ) = split(' ',$rows[1],2);
            $start=3;
            while (not ($rows[$start] =~ /^Skipped keys/)){
                $start++;
            }
        }else{
            my $count=0;
            while (not ($rows[(2+$count)] =~ /^Skipped keys/)){
                $count++;
            }
            $start=2+$count;
            $stored_bins=$count;
        }
        my (@stored_keys);
        unless ($rows[$start] =~ /^Skipped keys/){
            croak("bug in reading restart information: start is $start, line is ".$rows[$start]);
        }
        $start++;
        while (not ($rows[$start] =~ /^Skipped values/)){
            chomp($rows[$start]);
            my @bin_keys = split(',', $rows[$start] );
            push( @stored_keys, \@bin_keys );
            $start++;
        }
        $self->skipped_keys(\@stored_keys);
        while (not ($rows[$start] =~ /^seed/)){
            $start++;
        }
        @seed = split(' ',$rows[$start]);
        shift( @seed ); # get rid of 'seed'-word
        random_set_seed( @seed );

        $self->actual_bins($stored_bins);
        for ( my $j = 1; $j <= $stored_bins; $j++ ) {
            my @names = ( 'cdd_'.$j, 'rem_'.$j );
            foreach my $i ( 0, 1 ) {
                my ($model_dir, $filename) = OSspecific::absolute_path( $self -> directory.'/m'.
                    $model_number,
                    $names[$i].'.mod' );
                my ($out_dir, $outfilename) = OSspecific::absolute_path( $self -> directory.'/m'.
                    $model_number,
                    $names[$i].'.lst' );
                my $new_mod = model ->     new( directory   => $model_dir,
                                             filename    => $filename,
                                             outputfile  => $outfilename,
                                             extra_files => $model -> extra_files,
                                             ignore_missing_files => 1);
                push( @{$new_models[$i]}, $new_mod );
            }

            my $nl = $j == $stored_bins ? "" : "\r";
            ui -> print( category => 'cdd',
                message  => ui -> status_bar( sofar => $j+1,
                    goal  => $stored_bins+1 ).$nl,
                wrap     => 0,
                newline  => 0 );
        }
        ui -> print( category => 'cdd',
            message  => " ... done." );
        ui -> print( category => 'cdd',
            message  => "Using $stored_bins previously sampled case-deletion sets ".
            "from $stored_filename" )
        unless $self -> parent_threads > 1;
    }

    # Use only the first half (the case-deleted) of the data sets.
    $self -> prepared_models->[$model_number-1]{'own'} = $new_models[0];

    # The remainders are left until the analyze step, only run if xv

    $self -> prediction_models->[$model_number-1]{'own'} = $new_models[1];

    # ---------------------  Create the sub tools  ------------------------------

    #this is just a modelfit
    my $subdir = $class;
    $subdir =~ s/tool:://;
    my @subtools;
    @subtools = @{$self -> subtools} if (defined $self->subtools);
    shift( @subtools );
    my %subargs = ();
    if ( defined $self -> subtool_arguments ) {
        %subargs = %{$self -> subtool_arguments};
    }
    $subargs{'resume'}=$done; #do not rerun models that have lst-file in m1
    $self->tools([]) unless (defined $self->tools);
    push( @{$self -> tools},
        $class ->new( %{common_options::restore_options(@common_options::tool_options)},
            models                => $new_models[0],
             copy_data            => 0, #use relative data path to m1
            threads               => $subm_threads,
            nmtran_skip_model => 2,
            _raw_results_callback => $self ->
            _modelfit_raw_results_callback( model_number => $model_number ),
            subtools              => \@subtools,
            parent_tool_id        => $self -> tool_id,
            logfile             => undef,
            raw_results           => undef,
            prepared_models       => undef,
            top_tool              => 0,
            directory => "modelfit_dir1",
            %subargs ) );

    $self->tools->[-1]->add_to_nmoutput(extensions => ['ext','cov','coi']);
}

sub llp_pre_fork_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    $self -> modelfit_pre_fork_setup;
}

sub _modelfit_raw_results_callback
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        cross_validation_set => { isa => 'Bool', default => 0, optional => 1 },
        jackknife_mode => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my $cross_validation_set = $parm{'cross_validation_set'};
    my $jackknife_mode = $parm{'jackknife_mode'};
    my $subroutine;

    # Use the cdd's raw_results file.
    # The cdd and the bootstrap's callback methods are identical
    # in the beginning, then the cdd callback adds cook.scores and
    # cov.ratios.

    my ($dir,$file) =
    OSspecific::absolute_path( $self -> directory,
        $self -> raw_results_file->[$model_number-1] );
    my $orig_mod = $self -> models->[$model_number-1];
    my $xv = $self -> cross_validate;
    $subroutine = sub {
        my $modelfit = shift;
        my $mh_ref   = shift;
        my %max_hash = %{$mh_ref};
        $modelfit -> raw_results_file( [$dir.$file] );

        if( $cross_validation_set ) {
            $modelfit -> raw_results_append( 1 ) if( not $self -> bca_mode ); # overwrite when doing a jackknife
            my ( @new_header, %param_names );
            foreach my $row ( @{$modelfit -> raw_results} ) {
                unshift( @{$row}, 'cross_validation' );
            }
            $modelfit->clear_raw_results_header;
        } else {

            my %dummy;

            my ($raw_results_row,$nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
                model => $orig_mod,
                raw_line_structure => \%dummy );

            $orig_mod -> outputs -> [0] -> flush;

            unshift( @{$modelfit -> raw_results}, @{$raw_results_row} );

            &{$self -> _raw_results_callback}( $self, $modelfit )
            if ( defined $self -> _raw_results_callback ); #true from bootstrap jackknife

            $self->raw_line_structure($modelfit -> raw_line_structure);
            if( $xv and not $self -> bca_mode ) {
                foreach my $row ( @{$modelfit -> raw_results} ) {
                    unshift( @{$row}, 'cdd' );
                }
                unshift( @{$modelfit -> raw_results_header}, 'method' );
                foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure})){
                    foreach my $category (keys %{$self->raw_line_structure -> {$mod}}){
                        next if ($category eq 'line_numbers');
                        my ($start,$len) = split(',',$self->raw_line_structure -> {$mod}->{$category});
                        $self->raw_line_structure -> {$mod}->{$category} = ($start+1).','.$len; #+1 for method
                    }
                    $self->raw_line_structure -> {$mod}->{'method'} = '0,1';
                }
            }
            $self->raw_line_structure -> {'0'} = $self->raw_line_structure -> {'1'};
            $self->raw_line_structure -> write( $dir.'raw_results_structure' );
        }
    };
    return $subroutine;
}

sub update_raw_results
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    my ($dir,$file) =
    OSspecific::absolute_path( $self -> directory,$self -> raw_results_file->[$model_number-1] );
    return unless (-e $dir.$file );

    open( RRES, $dir.$file );
    my @rres = <RRES>;
    close( RRES );
    return if (scalar(@rres)<1);
    open( RRES, '>',$dir.$file );

    chomp( $rres[0] );
    print RRES $rres[0] . ',"cook.scores","jackknife.cook.scores","cov.ratios","outside.n.sd","cdd.delta.ofv",'.
        join(',',@{$self->labels_parameter_cook_scores})."\n";
    my @origparameter = (0) x scalar(@{$self->labels_parameter_cook_scores});
    chomp( $rres[1] );
    my @tmp = split(',',$rres[1]);
    my $cols = scalar(@tmp);
    print RRES $rres[1] . ",0,0,1,0,0,".join(',',@origparameter)."\n"; #original model

    foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure})){
        $self->raw_line_structure -> {$mod}->{'cook.scores'} = $cols.',1';
        $self->raw_line_structure -> {$mod}->{'jackknife.cook.scores'} = ($cols+1).',1';
        $self->raw_line_structure -> {$mod}->{'cov.ratios'} = ($cols+2).',1';
        $self->raw_line_structure -> {$mod}->{'outside.n.sd'} = ($cols+3).',1';
        $self->raw_line_structure -> {$mod}->{'cdd.delta.ofv'} = ($cols+4).',1';
    }

    $self->raw_line_structure -> write( $dir.'raw_results_structure' );

    my @new_rres;
    my $length = scalar(@{$self -> cook_scores});
    sub format_score{
        my $val=shift;
        if (defined $val){
            return sprintf( ",%.5f",$val);
        }else{
            return ',';
        }
    }
    sub format_count{
        my $val=shift;
        if (defined $val){
            return sprintf( ",%.1f",$val);
        }else{
            return ',';
        }
    }
    for( my $i = 2 ; $i <= $#rres; $i ++ ) {
        my $row_str = $rres[$i];
        chomp( $row_str );
        if (($i-2) < $length){
            $row_str .= format_score($self -> cook_scores -> [$i-2]).format_score($self -> jackknife_cook_scores -> [$i-2]).
                format_score($self -> covariance_ratios -> [$i-2]).format_count($self -> outside_n_sd -> [$i-2]).
                format_score($self -> delta_ofv -> [$i-2]);
            foreach my $val (@{$self->parameter_cook_scores-> [$i-2]} ){
                $row_str .= format_score($val);
            }
            foreach my $val (@{$self->jackknife_parameter_cook_scores-> [$i-2]} ){
                $row_str .= format_score($val);
            }
        }else{
            $row_str .= ',' x (5 + scalar(@{$self->labels_parameter_cook_scores}));
        }
        $row_str .= "\n";
        print RRES $row_str;
    }
    close( RRES );
}

sub pca
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        data_matrix => { isa => 'ArrayRef', optional => 0 }
    );
    my @data_matrix = defined $parm{'data_matrix'} ? @{$parm{'data_matrix'}} : ();
    my @eigenvalues;
    my @eigenvectors;
    my @projections;
    my @std;

    my $D = Math::MatrixReal ->  new_from_rows( \@data_matrix );
    my @n_dim = @{$data_matrix[0]};
    my @d_dim = @data_matrix;
    my $n = scalar @n_dim;
    my $d = scalar @d_dim;
    map( $_=(1/$n), @n_dim );
    my $frac_vec_n = Math::MatrixReal ->  new_from_cols( [\@n_dim] );
    map( $_=1, @n_dim );
    map( $_=1, @d_dim );
    my $one_vec_n = Math::MatrixReal -> new_from_cols( [\@n_dim] );
    my $one_vec_d = Math::MatrixReal -> new_from_cols( [\@d_dim] );
    my $one_vec_d_n = $one_vec_d * ~$one_vec_n;
    my $M = $D*$frac_vec_n;
    my $M_matrix = $M * ~$one_vec_n;

    # Calculate the mean-subtracted data
    my $S = $D-$M_matrix;

    # compue the empirical covariance matrix
    my $C = $S * ~$S;

    # compute the eigenvalues and vectors
    my ($l, $V) = $C -> sym_diagonalize();

    # Project the original data on the eigenvectors
    my $P = ~$V * $S;

    # l, V and projections are all MatrixReal objects.
    # We need to return the normal perl equivalents.
    @eigenvalues = @{$l->[0]};
    @eigenvectors = @{$V->[0]};
    @std = @{$self -> std( data_matrix => $P -> [0] )};
    # Make $P a n * d matrix
    $P = ~$P;
    @projections = @{$P->[0]};

    return \@eigenvalues ,\@eigenvectors ,\@projections ,\@std;
}

sub std
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        data_matrix => { isa => 'ArrayRef', optional => 1 }
    );
    my @data_matrix = defined $parm{'data_matrix'} ? @{$parm{'data_matrix'}} : ();
    my @std;

    my ( @sum, @pow_2_sum );
    if ( defined $data_matrix[0] ) {
        my $n = scalar @{$data_matrix[0]};
        for( my $i = 0; $i <= $#data_matrix; $i++ ) {
            $sum[$i]=0;
            for( my $j = 0; $j < $n; $j++ ) {
                $sum[$i] = $sum[$i]+$data_matrix[$i][$j];
                $pow_2_sum[$i] += $data_matrix[$i][$j]*$data_matrix[$i][$j];
            }
            $std[$i] = sqrt( ( $n*$pow_2_sum[$i] - $sum[$i]*$sum[$i] ) / ($n*$n) );
        }
    }

    return \@std;
}

sub prepare_results
{
    my $self = shift;

    if (not defined $self->raw_results) {
        $self->read_raw_results;
    }
    PsN::call_pharmpy("results cdd " . $self->directory);      # Generate results.json and results.csv
}

sub create_R_plots_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        rplot => { isa => 'rplots', optional => 0 }
    );
    my $rplot = $parm{'rplot'};

    my $case_column_name = $self->models->[0]->problems->[0]->inputs->[0]->options->[($self->case_column)-1]->name;
    $rplot->add_preamble(code => [
            "case.column.name   <-'".$case_column_name."'",
            "skipped.id.file <-'".$self->skipped_individuals_filename."'"
        ]);
}

1;
