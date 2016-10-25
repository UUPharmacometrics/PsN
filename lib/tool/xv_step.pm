package tool::xv_step;

use include_modules;
use tool::modelfit;
use Moose;
use MooseX::Params::Validate;
use data;
use log;

extends 'tool';

#start description
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
#end description

#start synopsis
    # The return value is a reference to the data objects containing
    # the prediction and the estimation datasets.
#end synopsis

#start see_also
    # =begin html
    #
    # <a HREF="../data.html">data</a>, <a
    # HREF="../model.html">model</a> <a
    # HREF="../output.html">output</a>, <a
    # HREF="../tool.html">tool</a>
    #
    # =end html
    #
    # =begin man
    #
    # data, model, output, tool
    #
    # =end man
#end see_also

		
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

	if (defined $model->problems->[0]->nonparametrics() and scalar(@{$model->problems->[0]->nonparametrics})> 0) {
		$self->is_nonparametric(1);
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
							  msf => { isa => 'Bool', optional => 0 },
							  estimation_data => { isa => 'ArrayRef', optional => 0 },
	);
	my $model = $parm{'model'};
	my $directory = $parm{'directory'};
	my $msf = $parm{'msf'};
	my $estimation_data = $parm{'estimation_data'};

	my @estimation_models =();

	for( my $i = 0; $i < scalar(@{$estimation_data}); $i++  ){
		my $model_copy_est = $model -> copy(filename => 
											$directory.'m1/est_model'.$i.'.mod',
											output_same_directory => 1,
											write_copy => 0,
											copy_datafile => 0, 
											copy_output => 0,
			);
		
		$model_copy_est -> datafiles( new_names => [$estimation_data -> [$i]] );
		if ($msf){
			$model_copy_est ->rename_msfo(add_if_absent => 1,
										  name => 'est_model'.$i.'.msf'); #FIXME we do not handle prior tnpri here
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
							  last_est_complete => { isa => 'Bool', optional => 0 },
							  msf => { isa => 'Bool', optional => 0 },
							  is_nonparametric => { isa => 'Bool', optional => 0 },
							  prediction_data => { isa => 'ArrayRef', optional => 0 },
							  estimation_models => { isa => 'ArrayRef', optional => 1 },
	);
	my $model = $parm{'model'};
	my $directory = $parm{'directory'};
	my $last_est_complete = $parm{'last_est_complete'};
	my $msf = $parm{'msf'};
	my $is_nonparametric = $parm{'is_nonparametric'};
	my $prediction_data = $parm{'prediction_data'};
	my $estimation_models = $parm{'estimation_models'};

	my @prediction_models =();
	my @extra_prediction_models =();
	for( my $i = 0; $i < scalar(@{$prediction_data}); $i++){
		my $model_copy_pred = $model -> copy(
			filename => $directory.'m1/pred_model' . $i . '.mod',
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
		if (defined $estimation_models and defined $estimation_models->[$i]){
			#only true when not lasso
			my $est_mod = $estimation_models->[$i];
			if( defined $est_mod -> outputs -> [0] and 
				defined $est_mod -> outputs -> [0] ->get_single_value(attribute=> 'ofv') ){
				if ($msf){
					my $oldmsfoname = $est_mod->msfo_names(problem_numbers => [1], absolute_path => 1);
					unless (defined $oldmsfoname->[0]){
						croak("cannot do set_first_problem_msfi, no msfo in est model");
					}
					my $extra_options = {};
					if ($is_nonparametric){
						$extra_options = {'NEW' => undef};
					}
					$model_copy_pred -> set_first_problem_msfi(msfiname => $oldmsfoname->[0],
															   extra_options => $extra_options,
															   set_new_msfo => 1);
					if ($is_nonparametric){
						#FIXME check with Mats about $ESTIM POSTHOC
						if (1){
							#Scenario 2:  create second model file for separate parametric evaluation
							my $model_copy_extra = $model_copy_pred -> copy(
								filename => $directory.'m1/pred_param_model' . $i . '.mod',
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
							push( @extra_prediction_models, $model_copy_extra );
						}
						if (1){
							#Scenario 1: add second $PROB for parametric evaluation
							push(@{$model_copy_pred->problems()},get_second_problem(model =>$model_copy_pred));
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

	push(@prediction_models,@extra_prediction_models) if (scalar(@extra_prediction_models)>0);
	return \@prediction_models;
}


sub get_second_problem
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

	foreach my $record ('table','simulation','pk','pred','error','covariance','scatter','subroutine',
						'abbreviated','sizes','prior','model','tol','infn','aesinitial',
						'aes','des','mix','nonparametric'){
		$dummymodel -> remove_records (problem_numbers => [1],
									   keep_last => 0,
									   type => $record);
	}

	$dummymodel->remove_option(record_name => 'estimation',
							   option_name => 'MSFO',
							   fuzzy_match => 1);

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
	my $problem = model::problem ->	new ( directory                   => $model->directory,
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
			trace(tool => 'xv_step_subs',message => "cut thetas in xv_step_subs ".
				  "modelfit_post_subtool_analyze", level => 1);
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

	trace(tool => "xv_step", message => "modelfit_setup\n", level => 1);

	$self -> create_data_sets;

	# Create copies of the model. This is reasonable to do every
	# time, since the model is the thing that changes in between
	# xv steps in a lasso.

	if( $self -> do_estimation ){
		$self -> estimation_models(estimation_setup( model => $self->model,
													 directory => $self->directory,	
													 msf => $self->msf,
													 estimation_data => $self->estimation_data));

		if ($self->do_prediction and $self->is_lasso){
			$self -> prediction_models(prediction_setup(model => $self->model,
														directory => $self->directory,
														last_est_complete => $self->last_est_complete,
														msf => 0,
														is_nonparametric => 0,
														prediction_data => $self->prediction_data));
		}
			
	}else{
		#only prediction
		$self -> prediction_models(prediction_setup(model => $self->model,
													directory => $self->directory,
													last_est_complete => $self->last_est_complete,
													msf => $self->msf,
													is_nonparametric => $self->is_nonparametric,
													prediction_data => $self->prediction_data));
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
												  directory_name_prefix => 'estimation'
						  ) ] );
		$task = 'estimation';
	} else{
		#only prediction
		$self -> tools( [ tool::modelfit -> new ( 'models' => $self -> prediction_models,
												  %modf_args,
												  nmtran_skip_model => 2,
												  copy_data => 0,
												  directory_name_prefix => 'prediction'
						  ) ] );
		$task = 'prediction';
	}
	
	trace(tool => 'xv_step_subs', message => "a new modelfit object for $task", level => 1);

	if (defined $self->init) {
		&{$self -> init}($self);
	}
}

sub modelfit_analyze
{
	my $self = shift;

	trace(tool => 'xv_step', message => "modelfit_analyze\n", level => 1);
	if( defined $self -> post_analyze ){
		my $temp = &{$self -> post_analyze}($self);
		$self -> cont($temp); #is this really a boolean???
	} else {
		$self -> cont(0);
	}
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
											   stratify_on => $self->stratify_on());
		
		trace(tool => 'xv_step_subs', message => "create data", level => 1);
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
	trace(tool => 'xv_step_subs', message => "written data in ".$self->directory, level => 1);
}

sub modelfit_post_subtool_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Maybe[Int]', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	trace(tool => "xv_step", message => "modelfit_post_subtool_analyze\n", level => 1);
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
	}
	my %modelfit_arg;
	if(defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'modelfit'}){ # Override user threads. WHY???
		%modelfit_arg  = %{$self->subtool_arguments->{'modelfit'}};
	}
	$modelfit_arg{'cut_thetas_rounding_errors'} = 0;
	$modelfit_arg{'cut_thetas_maxevals'} = 0;
	$modelfit_arg{'handle_hessian_npd'} = 0;
	trace(tool => 'xv_step_subs',message => "set no cut_thetas_rounding errors in xv_step_subs ".
		"modelfit_post_subtool_analyze, push modelfit object with pred models only", level => 1);

	if( scalar(@{$self->prediction_models}) > 0 ){
		$self -> tools([]) unless (defined $self->tools);
		push( @{$self -> tools}, tool::modelfit -> new ( models => $self->prediction_models,
														 %modelfit_arg,
														 nmtran_skip_model => 2,
														 copy_data => 0,
														 directory_name_prefix => 'prediction'
			  ) );
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
