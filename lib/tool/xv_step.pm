package tool::xv_step;

use include_modules;
use tool::modelfit;
use Mouse;
use MouseX::Params::Validate;
use data;

extends 'tool';

# When we started discussions on implementing crossvalidation we
# stumbled on the question on what a crossvalidation really is. We
# agreed on that it can be two things, a simpler verstion that is
# part of the other, the more complex version. We descided two
# implement both as separate classes. This class, the
# xv_step(short for cross validation step)m is the simpler form of
# crossvalidation is where you create two datasets, one for
# training (in NONMEM its called estimation) and one for
# validation(prediction in NONMEM), and perform both training and
# validation. Then just return the resulting output.

# The return value is a reference to the data objects containing
# the prediction and the estimation datasets.

has 'nr_validation_groups' => ( is => 'rw', isa => 'Int', default => 5 );
has 'stratify_on' => ( is => 'rw', isa => 'Str' );
has 'cutoff' => ( is => 'rw', isa => 'Num' ); #for lasso
has 'n_model_thetas' => ( is => 'rw', isa => 'Int', default => 0 ); #for lasso
has 'estimation_data' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'prediction_data' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'init' => ( is => 'rw', isa => 'Ref' );
has 'post_analyze' => ( is => 'rw', isa => 'Ref' );
has 'cont' => ( is => 'rw', isa => 'Bool' );
has 'msf' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'is_lasso' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'is_nonparametric' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'own_parameters' => ( is => 'rw', isa => 'HashRef' );
has 'estimation_models' => ( is => 'rw', isa => 'ArrayRef[model]', default => sub { [] } );
has 'prediction_models' => ( is => 'rw', isa => 'ArrayRef[model]', default => sub { [] } );
has 'prediction_is_run' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'warnings' => ( is => 'rw', isa => 'Int', default => 0 );
has 'do_estimation' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'do_prediction' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'ignoresigns' => ( is => 'rw', isa => 'ArrayRef');
has 'model' => ( is => 'rw', isa => 'model');
has 'sum_ofv' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'sum_npofv' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'results_header' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'results_rows' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );


sub BUILD
{
    my $self  = shift;

    my $model;
    $model = $self -> models -> [0];
    $self->ignoresigns($model -> ignoresigns);
    $self->model($model);

    unless ( $self -> do_prediction or $self -> do_estimation ){
        croak("must do either prediction or estimation");
    }

    if ($self->is_nonparametric){
        $self->is_lasso(0);
        $self->msf(1);
    }
}

sub estimation_setup
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              directory => { isa => 'Str', optional => 0 },
                              tag => { isa => 'Str', optional => 1, default => '' },
                              msf => { isa => 'Bool', optional => 0 },
                              estimation_data => { isa => 'ArrayRef', optional => 0 },
    );
    my $model = $parm{'model'};
    my $directory = $parm{'directory'};
    my $tag = $parm{'tag'};
    my $msf = $parm{'msf'};
    my $estimation_data = $parm{'estimation_data'};

    my @estimation_models =();

    for( my $i = 0; $i < scalar(@{$estimation_data}); $i++  ){
        my $model_copy_est = $model -> copy(filename =>
                                            $directory.'m1/est_model'.$tag.$i.'.mod',
                                            output_same_directory => 1,
                                            write_copy => 0,
                                            copy_datafile => 0,
                                            copy_output => 0,
            );

        $model_copy_est -> datafiles( new_names => [$estimation_data -> [$i]] );
        if ($msf){
            $model_copy_est ->rename_msfo(add_if_absent => 1,
                                          name => 'est_model'.$tag.$i.'.msf'); #FIXME we do not handle prior tnpri here
        }
        $model_copy_est -> _write();
        push( @estimation_models, $model_copy_est );
    }
    return \@estimation_models;
}

sub prediction_setup
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              directory => { isa => 'Str', optional => 0 },
                              estimation_model_offset => { isa => 'Int', optional => 1, default => 0 },
                              last_est_complete => { isa => 'Bool', optional => 0 },
                              msf => { isa => 'Bool', optional => 0 },
                              is_nonparametric => { isa => 'Bool', optional => 0 },
                              prediction_data => { isa => 'ArrayRef', optional => 0 },
                              estimation_models => { isa => 'ArrayRef', optional => 1 },
    );
    my $model = $parm{'model'};
    my $directory = $parm{'directory'};
    my $estimation_model_offset = $parm{'estimation_model_offset'};
    my $last_est_complete = $parm{'last_est_complete'};
    my $msf = $parm{'msf'};
    my $is_nonparametric = $parm{'is_nonparametric'};
    my $prediction_data = $parm{'prediction_data'};
    my $estimation_models = $parm{'estimation_models'};

    my @prediction_models =();
    my @extra_prediction_models_msfi =();
    my @extra_prediction_models_update =();

    my $scenario_1 = 0; #parametric update_inits, separate model file
    my $scenario_2 = 0; #parametric MSFI, separate model file
    my $scenario_3 = 1; #parametric MSFI, additional $PROB
    my $scenario_4 = 0; #parametric update_inits, additional $PROB
    for( my $i = 0; $i < scalar(@{$prediction_data}); $i++){
        my $filename = 'pred_model'.$i. '.mod';
        my $est_mod;
        if (defined $estimation_models and defined $estimation_models->[$estimation_model_offset + $i]){
            #only true when not lasso
            $est_mod = $estimation_models->[$estimation_model_offset + $i];
            $filename = $est_mod->filename;
            $filename =~ s/est_model/pred_model/;
        }
        my $model_copy_pred = $model -> copy(
            filename => $directory.'m1/'.$filename,
            output_same_directory => 1,
            copy_datafile => 0,
            write_copy => 0,
            copy_output => 0,
            );

        #to handle NM7 methods
        $model_copy_pred -> set_maxeval_zero(print_warning => 0,
                                             need_ofv => 1,
                                             last_est_complete => $last_est_complete);
        $model_copy_pred->remove_option(record_name => 'estimation',
                                        option_name => 'NOABORT');

        $model_copy_pred -> datafiles( new_names => [$prediction_data -> [$i]] );
        if (defined $est_mod){
            #only true when not lasso
            if( defined $est_mod -> outputs -> [0] and
                defined $est_mod -> outputs -> [0] ->get_single_value(attribute=> 'ofv') ){

                if ($msf){
                    my $oldmsfoname = $est_mod->msfo_names(problem_numbers => [1], absolute_path => 1);
                    unless (defined $oldmsfoname->[0]){
                        croak("cannot do set_first_problem_msfi, no msfo in est model");
                    }
                    my $extra_options = {};
                    my $temp_model;
                    if ($is_nonparametric){
                        $extra_options = {'NEW' => undef};

                        if ($scenario_1 or $scenario_4){
                            $temp_model = $model_copy_pred -> copy(
                                filename => $directory.'dummy.mod',
                                output_same_directory => 1,
                                copy_datafile => 0,
                                write_copy => 0,
                                copy_output => 0,
                                );
                            $temp_model->remove_records(type=> 'nonparametric');
                            $temp_model->remove_records(type=> 'covariance');
                            $temp_model->remove_option(record_name => 'estimation',
                                                       option_name => 'MSFO',
                                                       fuzzy_match => 1);
                            $temp_model -> update_inits( from_output => $est_mod->outputs->[0],
                                                         update_omegas => 1,
                                                         update_sigmas => 1,
                                                         update_thetas => 1);
                            #we never write temp_model to disk, only use as template for update_inits scenario
                        }

                        if ($scenario_1){
                            #create second model file for separate parametric evaluation using update_inits
                            my $extra_filename = $filename;
                            $extra_filename =~ s/pred_model/pred_param_upd_model/;

                            my $model_copy_extra_update = $temp_model -> copy(
                                filename => $directory.'m1/'.$extra_filename,
                                output_same_directory => 1,
                                copy_datafile => 0,
                                write_copy => 0,
                                copy_output => 0,
                                );
                            $model_copy_extra_update -> _write();
                            push( @extra_prediction_models_update, $model_copy_extra_update );
                        }
                    }#end if is_nonparametric

                    $model_copy_pred -> set_first_problem_msfi(msfiname => $oldmsfoname->[0],
                                                               extra_options => $extra_options,
                                                               set_new_msfo => 1);
                    if ($is_nonparametric){
                        if ($scenario_2){
                            #Scenario 2:  create second model file for separate parametric evaluation
                            my $extra_filename = $filename;
                            $extra_filename =~ s/pred_model/pred_param_model/;

                            my $model_copy_extra = $model_copy_pred -> copy(
                                filename => $directory.'m1/'.$extra_filename,
                                output_same_directory => 1,
                                copy_datafile => 0,
                                write_copy => 0,
                                copy_output => 0,
                                );
                            $model_copy_extra->remove_records(type=> 'nonparametric');
                            $model_copy_extra->remove_records(type=> 'table');
                            $model_copy_extra->remove_records(type=> 'covariance');
                            $model_copy_extra->remove_option(record_name => 'estimation',
                                                             option_name => 'MSFO',
                                                             fuzzy_match => 1);
                            $model_copy_extra -> _write();
                            push( @extra_prediction_models_msfi, $model_copy_extra );
                        }
                        if ($scenario_3){
                            #add additional $PROB using MSFI for parametric evaluation
                            push(@{$model_copy_pred->problems()},get_second_problem_msfi(model =>$model_copy_pred));
                            push(@{$model_copy_pred->active_problems()},1);
                        }
                        if ($scenario_4){
                            #Scenario 4: add additional $PROB using update_inits for parametric evaluation
                            push(@{$model_copy_pred->problems()},get_second_problem_update(model =>$temp_model));
                            push(@{$model_copy_pred->active_problems()},1);
                        }

                        #turn off parametric evaluation completely in first $PROB
                        $model_copy_pred -> set_option(problem_numbers => [1],
                                                       record_name => 'estimation',
                                                       record_number => 0, #0 means all
                                                       option_name =>'FNLETA',
                                                       option_value =>'2',
                                                       fuzzy_match => 1);
                    }
                }else{
                    #if not msf then not nonparametric
                    $model_copy_pred -> update_inits( from_output => $est_mod->outputs->[0],
                                                      update_omegas => 1,
                                                      update_sigmas => 1,
                                                      update_thetas => 1);
                }
            }else{
                #no ofv from est model. run pred anyway to get correct number of pred models
            }
        }
        $model_copy_pred -> _write();
        push( @prediction_models, $model_copy_pred );
    }

    push(@prediction_models,@extra_prediction_models_msfi) if (scalar(@extra_prediction_models_msfi)>0);
    push(@prediction_models,@extra_prediction_models_update) if (scalar(@extra_prediction_models_update)>0);
    return \@prediction_models;
}


sub get_second_problem_msfi
{
    #only for nonparametric, scenario 3 with two $PROB in prediction models
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
    );
    my $model = $parm{'model'};

    my @problem_lines = ();
    my $dummymodel = $model ->  copy( filename    => $model->directory.'dummy.mod',
                                      output_same_directory => 1,
                                      copy_output => 0,
                                      write_copy =>0);

    #set $DATA REWIND
    $dummymodel->add_option(problem_numbers => [1],
                            record_name => 'data',
                            option_name => 'REWIND');

    foreach my $record ('simulation','pk','pred','error','covariance','scatter','subroutine',
                        'abbreviated','sizes','prior','model','tol','infn','aesinitial',
                        'aes','des','mix','nonparametric'){
        $dummymodel -> remove_records (problem_numbers => [1],
                                       keep_last => 0,
                                       type => $record);
    }

    $dummymodel->remove_option(record_name => 'estimation',
                               option_name => 'MSFO',
                               fuzzy_match => 1);
    $dummymodel->remove_option(record_name => 'msfi',
                               option_name => 'NEW',
                               fuzzy_match => 1);
    $dummymodel->add_option(record_name => 'msfi',
                            option_name => 'POPETAS');


    my $linesarray = $dummymodel->problems->[0]->_format_problem(relative_data_path => $model->relative_data_path,
                                                                 write_directory => $model->directory);
    #we cannot use this array directly, must make sure items do not contain line breaks
    foreach my $line (@{$linesarray}){
        my @arr = split(/\n/,$line);
        push(@problem_lines,@arr);
    }
    my $sh_mod = model::shrinkage_module -> new ( nomegas => $model -> nomegas -> [0],
                                                  directory => $model -> directory(),
                                                  problem_number => 2 );
    my $problem = model::problem ->    new ( directory                   => $model->directory,
                                          ignore_missing_files        => 1,
                                          ignore_missing_output_files => 1,
                                          sde                         => $model->sde,
                                          omega_before_pk             => $model->omega_before_pk,
                                          psn_record_order            => $model->psn_record_order,
                                          cwres                       => $model->cwres,
                                          tbs                         => 0,
                                          dtbs                         => 0,
                                          prob_arr                    => \@problem_lines,
                                          shrinkage_module            => $sh_mod );


    return $problem;

}

sub get_second_problem_update
{
    #only for nonparametric, scenario with two $PROB in prediction models
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
    );
    my $model = $parm{'model'};

    my @problem_lines = ();
    my $dummymodel = $model ->  copy( filename    => $model->directory.'dummy.mod',
                                      output_same_directory => 1,
                                      copy_output => 0,
                                      write_copy =>0);

    #set $DATA REWIND
    $dummymodel->add_option(problem_numbers => [1],
                            record_name => 'data',
                            option_name => 'REWIND');

    foreach my $record ('simulation','pk','pred','error','covariance','scatter','subroutine',
                        'abbreviated','sizes','prior','model','tol','infn','aesinitial',
                        'aes','des','mix','nonparametric'){
        $dummymodel -> remove_records (problem_numbers => [1],
                                       keep_last => 0,
                                       type => $record);
    }


    my $linesarray = $dummymodel->problems->[0]->_format_problem(relative_data_path => $model->relative_data_path,
                                                                 write_directory => $model->directory);
    #we cannot use this array directly, must make sure items do not contain line breaks
    foreach my $line (@{$linesarray}){
        my @arr = split(/\n/,$line);
        push(@problem_lines,@arr);
    }
    my $sh_mod = model::shrinkage_module -> new ( nomegas => $model -> nomegas -> [0],
                                                  directory => $model -> directory(),
                                                  problem_number => 2 );
    my $problem = model::problem ->    new ( directory                   => $model->directory,
                                          ignore_missing_files        => 1,
                                          ignore_missing_output_files => 1,
                                          sde                         => $model->sde,
                                          omega_before_pk             => $model->omega_before_pk,
                                          psn_record_order            => $model->psn_record_order,
                                          cwres                       => $model->cwres,
                                          tbs                         => 0,
                                          dtbs                         => 0,
                                          prob_arr                    => \@problem_lines,
                                          shrinkage_module            => $sh_mod );


    return $problem;

}

sub prediction_update
{
    #only used by lasso
    my %parm = validated_hash(\@_,
                              prediction_models => { isa => 'ArrayRef', optional => 0 },
                              estimation_models => { isa => 'ArrayRef', optional => 0 },
                              n_model_thetas => { isa => 'Int', optional => 0 },
                              cutoff => { isa => 'Maybe[Num]', optional => 1 },
    );
    my $prediction_models = $parm{'prediction_models'};
    my $estimation_models = $parm{'estimation_models'};
    my $n_model_thetas = $parm{'n_model_thetas'};
    my $cutoff = $parm{'cutoff'};

    for( my $i = 0; $i < scalar(@{$prediction_models}); $i++){
        my $est_mod = $estimation_models->[$i];
        if( defined $est_mod -> outputs -> [0] and
            defined $est_mod -> outputs -> [0] ->get_single_value(attribute=> 'ofv') ){
            $prediction_models->[$i] -> update_inits( from_output => $est_mod->outputs->[0],
                                                      update_omegas => 1,
                                                      update_sigmas => 1,
                                                      update_thetas => 1);
            my $nth = $prediction_models->[$i]->nthetas();
            my $init_val = $prediction_models->[$i]-> initial_values( parameter_type    => 'theta',
                                                                      parameter_numbers => [[1..$nth]])->[0];
            for(my $j = $n_model_thetas; $j<scalar(@{$init_val}); $j++){ #leave original model thetas intact
                my $value = $init_val -> [$j];
                if ((defined $cutoff) and (abs($value) <= $cutoff)){
                    $prediction_models->[$i]->initial_values(parameter_type => 'theta',
                                                             parameter_numbers => [[$j+1]],
                                                             new_values => [[0]] );
                    $prediction_models->[$i]->fixed(parameter_type => 'theta',
                                                    parameter_numbers => [[$j+1]],
                                                    new_values => [[1]] );
                }
            }
            $prediction_models->[$i] -> _write(overwrite => 1);
        }
    }
}

sub modelfit_setup
{
    my $self = shift;

    $self -> create_data_sets;

    # Create copies of the model. This is reasonable to do every
    # time, since the model is the thing that changes in between
    # xv steps in a lasso.

    if( $self -> do_estimation ){
        $self -> estimation_models(estimation_setup( model => $self->model,
                                                     directory => $self->directory,
                                                     msf => $self->msf,
                                                     estimation_data => $self->estimation_data));
        if ($self->is_lasso){
            if ($self->do_prediction){
                $self -> prediction_models(prediction_setup(model => $self->model,
                                                            directory => $self->directory,
                                                            last_est_complete => $self->last_est_complete,
                                                            msf => 0,
                                                            is_nonparametric => 0,
                                                            prediction_data => $self->prediction_data));
            }
        }else{
            #not lasso. loop extra models
            for (my $j=1; $j < scalar(@{$self->models}); $j++){
                push(@{$self -> estimation_models},@{estimation_setup( model => $self->models->[$j],
                                                                       tag => '_m'.($j+1).'_',
                                                                       directory => $self->directory,
                                                                       msf => $self->msf,
                                                                       estimation_data => $self->estimation_data)});

            }
        }

    }else{
        #only prediction
        $self -> prediction_models(prediction_setup(model => $self->model,
                                                    directory => $self->directory,
                                                    last_est_complete => $self->last_est_complete,
                                                    msf => $self->msf,
                                                    is_nonparametric => $self->is_nonparametric,
                                                    prediction_data => $self->prediction_data));
        unless ($self->is_lasso){
            for (my $j=1; $j < scalar(@{$self->models}); $j++){
                push(@{$self -> prediction_models},@{prediction_setup( model => $self->models->[$j],
                                                                       tag => '_m'.($j+1).'_',
                                                                       directory => $self->directory,
                                                                       last_est_complete => $self->last_est_complete,
                                                                       msf => $self->msf,
                                                                       is_nonparametric => $self->is_nonparametric,
                                                                       prediction_data => $self->prediction_data)});
            }
        }
    }

    my %modf_args;
    if (defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'modelfit'}){
        %modf_args = %{$self -> subtool_arguments -> {'modelfit'}};
    }

    my $task;
    if( $self -> do_estimation ){
        $self -> tools( [ tool::modelfit -> new ( 'models' => $self -> estimation_models,
                                                  %modf_args,
                                                  nmtran_skip_model => 2,
                                                  copy_data => 0,
                                                  directory_name_prefix => 'estimation',
                                                  model_subdir => 0,
                          ) ] );
        $task = 'estimation';
    } else{
        #only prediction
        $self -> tools( [ tool::modelfit -> new ( 'models' => $self -> prediction_models,
                                                  %modf_args,
                                                  nmtran_skip_model => 2,
                                                  copy_data => 0,
                                                  directory_name_prefix => 'prediction',
                                                  model_subdir => 0,
                          ) ] );
        $task = 'prediction';
    }

    if (defined $self->init) {
        &{$self -> init}($self);
    }
}

sub modelfit_analyze
{
    my $self = shift;

    if( defined $self -> post_analyze ){
        my $temp = &{$self -> post_analyze}($self);
        $self -> cont($temp); #is this really a boolean???
    } else {
        $self -> cont(0);
        $self->summarize_results;
    }
    #here summarize prediction ofv, parametric and nonparametric
}

sub print_xv_results
{
    my $self = shift;
    open XV_REPORT, '>', $self->directory."xv_result.csv";
    print XV_REPORT join(',',@{$self->results_header})."\n";
    for (my $i=0; $i< scalar(@{$self->results_rows}); $i++){
        print XV_REPORT join(',',@{$self->results_rows->[$i]})."\n";
    }
    close XV_REPORT;
    return 0;
}

sub summarize_results
{
    my $self = shift;
    my @sum_ofv = (0) x scalar(@{$self->models});
    my @estimation_ofv = [];
    my @prediction_ofv = [];
    my @sum_npofv = (0) x scalar(@{$self->models});
    my @estimation_npofv = [];
    my @prediction_npofv = [];

    for (my $j=0; $j<scalar(@{$self->models}); $j++){
        push(@estimation_ofv,[]);
        push(@estimation_npofv,[]);
        push(@prediction_ofv,[]);
        push(@prediction_npofv,[]);

        for( my $i = 0; $i < $self->nr_validation_groups; $i++  ){
            my $index = $j*($self->nr_validation_groups)+$i;
            my $est_model = $self->estimation_models->[$index];
            my $pred_model = $self->prediction_models->[$index];
            my $ofv=undef;
            my $npofv=undef;
            if (defined $est_model and defined $est_model -> outputs and
                defined $est_model -> outputs -> [0] and $est_model->outputs->[0]->have_output) {
                $ofv =  $est_model -> outputs -> [0] -> get_single_value(attribute => 'ofv',problem_index => 0);
                if ($self->is_nonparametric){
                    $npofv =  $est_model -> outputs -> [0] -> get_single_value(attribute => 'npofv',problem_index => 0);
                }
            }
            push(@{$estimation_ofv[$j]},$ofv);
            push(@{$estimation_npofv[$j]},$npofv);

            $ofv=undef;
            $npofv=undef;
            if (defined $pred_model and defined $pred_model -> outputs and
                defined $pred_model -> outputs -> [0] and $pred_model->outputs->[0]->have_output) {
                if ($self->is_nonparametric){
                    $ofv =  $pred_model -> outputs -> [0] -> get_single_value(attribute => 'ofv',problem_index => 1);
                    $npofv =  $pred_model -> outputs -> [0] -> get_single_value(attribute => 'npofv',problem_index => 0);
                }else{
                    $ofv =  $pred_model -> outputs -> [0] -> get_single_value(attribute => 'ofv',problem_index => 0);
                }
            }
            push(@{$prediction_ofv[$j]},$ofv);
            push(@{$prediction_npofv[$j]},$npofv);
            if (defined $sum_ofv[$j] and defined $ofv){
                $sum_ofv[$j] += $ofv;
            }else{
                $sum_ofv[$j] = undef;
            }
            if (defined $sum_npofv[$j] and defined $npofv){
                $sum_npofv[$j] += $npofv;
            }else{
                $sum_npofv[$j] = undef;
            }
        }
    }
    $self->sum_ofv(\@sum_ofv);
    $self->sum_npofv(\@sum_npofv);


    my @header = ('model','sum_ofv');
    push(@header,'sum_npofv') if $self->is_nonparametric;
    for( my $i = 0; $i < $self->nr_validation_groups; $i++  ){
        push(@header,'est_ofv'.$i);
    }
    if ($self->is_nonparametric){
        for( my $i = 0; $i < $self->nr_validation_groups; $i++  ){
            push(@header,'est_npofv'.$i);
        }
    }
    for( my $i = 0; $i < $self->nr_validation_groups; $i++  ){
        push(@header,'pred_ofv'.$i);
    }
    if ($self->is_nonparametric){
        for( my $i = 0; $i < $self->nr_validation_groups; $i++  ){
            push(@header,'pred_npofv'.$i);
        }
    }

    $self->results_header(\@header);
    my @rows = ();
    for (my $j=0; $j<scalar(@{$self->models}); $j++){
        push(@rows,[]);
        my $fname = $self->models->[$j]->filename;
        $fname =~ s/\.mod$//;
        $fname =~ s/\.ctl$//;
        push(@{$rows[$j]},$fname,$sum_ofv[$j]);
        push(@{$rows[$j]},$sum_npofv[$j]) if $self->is_nonparametric;
        for( my $i = 0; $i < $self->nr_validation_groups; $i++  ){
            push(@{$rows[$j]},$estimation_ofv[$j]->[$i]);
        }
        if ($self->is_nonparametric){
            for( my $i = 0; $i < $self->nr_validation_groups; $i++  ){
                push(@{$rows[$j]},$estimation_npofv[$j]->[$i]);
            }
        }
        for( my $i = 0; $i < $self->nr_validation_groups; $i++  ){
            push(@{$rows[$j]},$prediction_ofv[$j]->[$i]);
        }
        if ($self->is_nonparametric){
            for( my $i = 0; $i < $self->nr_validation_groups; $i++  ){
                push(@{$rows[$j]},$prediction_npofv[$j]->[$i]);
            }
        }
    }
    $self->results_rows(\@rows);

}

sub create_data_sets
{
    my $self = shift;

    my $model = $self -> model;
    my $ignoresign = (defined $self->ignoresigns and defined $self->ignoresigns->[0])? $self->ignoresigns->[0]:'@';
    my ( $junk, $idcol ) = $model -> _get_option_val_pos( name            => 'ID',
                                                          record_name     => 'input',
                                                          problem_numbers => [1]);
    unless (defined $idcol->[0][0]){
        croak( "Error finding column ID in \$INPUT of model\n");
    }
    my $data_obj = data->new(filename => $model->datafiles(absolute_path=>1)->[0],
                             idcolumn => $idcol->[0][0],
                             ignoresign => $ignoresign,
                             missing_data_token => $self->missing_data_token);

    if (!$data_obj->have_unique_ids()) {
        print("Warning: The dataset does not have unique IDs. There is a risk that individuals will be merged together.\n");
    }

    my $subsets;
    my $array;


    # First we check if estimation and prediction datasets were
    # given to us. If so, we don't do it again. This is good if
    # one xv_step object is initialised with datasets from an
    # earlies xv_step instance. It is also good if this instance
    # is run again (but with a new modelfile).
    my $have_data;
    unless( scalar(@{$self -> estimation_data})>0 and scalar(@{$self -> prediction_data})>0 ){
        $have_data = 0;
        # Create subsets of the dataobject.
        ($subsets,$array) = $data_obj->subsets(bins => $self->nr_validation_groups,
                                               stratify_on => $self->stratify_on);
    } else {
        $have_data = 1;
        if( scalar( @{$self -> estimation_data} ) != $self -> nr_validation_groups ){
            $self -> warn( message => 'The number of given datasets '.scalar(@{$self->estimation_data}).
                ' differs from the given number of validation groups '.$self -> nr_validation_groups );
        }

        if( scalar( @{$self -> estimation_data} ) != scalar( @{$self -> prediction_data} ) ){
            $self -> die( message => 'The number of estimation data sets '.scalar(@{$self->estimation_data}).
                ' does not match the number of prediction data sets '.scalar(@{$self->prediction_data}));
        }
        return;
    }

    # The prediction dataset is one of the elements in the
    # subsets array.

    unless ($have_data){
        for( my $i = 0; $i <= $#{$subsets}; $i++ ) {
            #each subset is a data object with ignoresign and idcolumn.
            #
            $subsets -> [$i] -> filename( 'pred_data' . $i . '.dta' );
            $subsets -> [$i] -> directory( $self -> directory );
            $subsets -> [$i] -> _write();
            push( @{$self -> prediction_data}, $subsets -> [$i]->full_name );

            my $est_data;
            for (my $j = 0; $j <= $#{$subsets}; $j++){
                if ($j == 0) {
                    $est_data = data->new(
                        filename => 'est_data' . $i . '.dta',
                        directory => $self->directory,
                        ignoresign => $subsets -> [$i]->ignoresign,
                        ignore_missing_files => 1,
                        header => $data_obj->header,
                        idcolumn => $subsets -> [$i]->idcolumn);
                }

                # The estimation data set is a merge of the datasets
                # complementing the prediction data in the subsets
                # array.

                unless( $i == $j ){
                    $est_data -> merge( mergeobj => $subsets -> [$j] );
                }
            }
            # TODO Remove this write when the data object is sane.
            $est_data -> _write();
            push( @{$self -> estimation_data}, $est_data->full_name );
        }
    }
}

sub modelfit_post_subtool_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Maybe[Int]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    if( $self -> prediction_is_run or not ($self -> do_estimation and $self -> do_prediction)){
        return;
    } else {
        $self -> prediction_is_run(1);
    }

    if ($self->is_lasso){
        prediction_update(prediction_models => $self->prediction_models,
                          estimation_models => $self->estimation_models,
                          n_model_thetas => $self->n_model_thetas,
                          cutoff => $self->cutoff);
    }else{
        $self -> prediction_models(prediction_setup(model => $self->model,
                                                    directory => $self->directory,
                                                    last_est_complete => $self->last_est_complete,
                                                    msf => $self->msf,
                                                    is_nonparametric => $self->is_nonparametric,
                                                    prediction_data => $self->prediction_data,
                                                    estimation_models => $self->estimation_models));
        for (my $j=1; $j < scalar(@{$self->models}); $j++){
            push(@{$self -> prediction_models},@{prediction_setup(model => $self->models->[$j],
                                                                  estimation_model_offset => $j*(scalar(@{$self->prediction_data})),
                                                                  directory => $self->directory,
                                                                  last_est_complete => $self->last_est_complete,
                                                                  msf => $self->msf,
                                                                  is_nonparametric => $self->is_nonparametric,
                                                                  prediction_data => $self->prediction_data,
                                                                  estimation_models => $self->estimation_models)});
        }
    }
    my %modelfit_arg;
    if(defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'modelfit'}){ # Override user threads. WHY???
        %modelfit_arg  = %{$self->subtool_arguments->{'modelfit'}};
    }
    $modelfit_arg{'cut_thetas_rounding_errors'} = 0;
    $modelfit_arg{'cut_thetas_maxevals'} = 0;
    $modelfit_arg{'handle_hessian_npd'} = 0;

    if( scalar(@{$self->prediction_models}) > 0 ){
        $self -> tools([]) unless (defined $self->tools);
        push( @{$self -> tools}, tool::modelfit -> new ( models => $self->prediction_models,
                                                         %modelfit_arg,
                                                         nmtran_skip_model => 2,
                                                         copy_data => 0,
                                                         directory_name_prefix => 'prediction',
                                                         model_subdir => 0,
              ) );
    }
}

1;
