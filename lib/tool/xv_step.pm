package tool::xv_step;

use include_modules;
use tool::modelfit;
use Moose;
use MooseX::Params::Validate;
use data;
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
has 'cutoff' => ( is => 'rw', isa => 'Num' );
has 'n_model_thetas' => ( is => 'rw', isa => 'Int', default => 0 );
has 'estimation_data' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'prediction_data' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'init' => ( is => 'rw', isa => 'Ref' );
has 'post_analyze' => ( is => 'rw', isa => 'Ref' );
has 'cont' => ( is => 'rw', isa => 'Bool' );
has 'own_parameters' => ( is => 'rw', isa => 'HashRef' );
has 'estimation_models' => ( is => 'rw', isa => 'ArrayRef[model]', default => sub { [] } );
has 'prediction_models' => ( is => 'rw', isa => 'ArrayRef[model]', default => sub { [] } );
has 'prediction_is_run' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'warnings' => ( is => 'rw', isa => 'Int', default => 0 );
has 'estimate_only' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'predict_only' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'last_est_complete' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'ignoresigns' => ( is => 'rw', isa => 'ArrayRef');


sub BUILD
{
	my $self  = shift;

	my $model;
	$model = $self -> models -> [0];
	$self->ignoresigns($model -> ignoresigns);

	if( $self -> predict_only and $self -> estimate_only ){
		$self -> predict_only(0);
		$self -> estimate_only(0);
	}
}

sub modelfit_setup
{
	my $self = shift;

	print "\n xv_step: modelfit_setup\n" if ($self->stop_motion());
	my $model = $self -> models -> [0];
	$self -> create_data_sets;

	# Create copies of the model. This is reasonable to do every
	# time, since the model is the thing that changes in between
	# xv steps.

	for( my $i = 0; $i <= $#{$self -> estimation_data}; $i++  )
	{
		unless( $self -> estimate_only ){
			my $model_copy_pred = $model -> copy(
				filename => $self -> directory().'m1/pred_model' . $i . '.mod',
				output_same_directory => 1,
				copy_datafile => 0, 
				write_copy => 0,
				copy_output => 0,
				);

			#to handle NM7 methods
			$model_copy_pred -> set_maxeval_zero(print_warning => 0,
				need_ofv => 1,
				last_est_complete => $self->last_est_complete());
			$model_copy_pred->remove_option(record_name => 'estimation',
				option_name => 'NOABORT');

			$model_copy_pred -> datafiles( new_names => [$self -> prediction_data -> [$i]] );
			# Make sure changes is reflected on disk.
			$model_copy_pred -> _write(); #setting overwrite here does not help lasso
			push( @{$self -> prediction_models}, $model_copy_pred );
		}

		unless( $self -> predict_only ){
			my $model_copy_est = $model -> copy(filename => 
												$self -> directory().'m1/est_model'.$i.'.mod',
												output_same_directory => 1,
												write_copy => 0,
												copy_datafile => 0, 
												copy_output => 0,
												);

			$model_copy_est -> datafiles( new_names => [$self -> estimation_data -> [$i]] );
			#do not write model here, will modify more later
			push( @{$self -> estimation_models}, $model_copy_est );
		}
	}

	my %modf_args;
	if (defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'modelfit'}){
		%modf_args = %{$self -> subtool_arguments -> {'modelfit'}};
	} 

	unless( $self -> predict_only ){
		$self -> tools( [ tool::modelfit -> new ( 'models' => $self -> estimation_models,
												  %modf_args,
												  nmtran_skip_model => 2,
												  copy_data => 0
				) ] );
	} elsif( not $self -> estimate_only ) {
		$self -> tools( [ tool::modelfit -> new ( 'models' => $self -> prediction_models,
												  %modf_args,
												  nmtran_skip_model => 2,
												  copy_data => 0
						  ) ] );
	}
	
	$self->stop_motion_call(tool=>'xv_step_subs',message => "a new modelfit object for estimation")
		if ($self->stop_motion());
	
	if( defined $self -> init ){
		&{$self -> init}($self);
	}
}

sub modelfit_analyze
{
	my $self = shift;

	print "\n xv_step: modelfit_analyze\n" if ($self->stop_motion());
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

	my $model = $self -> models -> [0];
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
		
		$self->stop_motion_call(tool=>'xv_step_subs',message => "create data")
		if ($self->stop_motion());
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
	$self->stop_motion_call(tool=>'xv_step_subs',message => "written data in ".$self->directory)
	if ($self->stop_motion());
}

sub modelfit_post_subtool_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Maybe[Int]', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	print "\n xv_step: modelfit_post_subtool_analyze\n"  if ($self->stop_motion());
	if( $self -> prediction_is_run or $self -> estimate_only or $self -> predict_only){
		return;
	} else {
		$self -> prediction_is_run(1);
	}

	my @est_models = @{$self->estimation_models};
	my @pred_models = @{$self->prediction_models};
	my @models_to_run;

	for( my $i=0; $i < @pred_models; $i++ ){
		my $pred_mod = $pred_models[$i];
		my $est_mod = $est_models[$i];

		if( defined $est_mod -> outputs -> [0] and 
			defined $est_mod -> outputs -> [0] ->get_single_value(attribute=> 'ofv') ){
			#before we required minimization successful here

			$pred_mod -> update_inits( from_output => $est_models[$i]->outputs->[0],
									   update_omegas => 1,
									   update_sigmas => 1,
									   update_thetas => 1);
			my $init_val = $pred_mod ->
				initial_values( parameter_type    => 'theta',
								parameter_numbers => [[1..$pred_mod->nthetas()]])->[0];
			$self->stop_motion_call(tool=>'xv_step_subs',message => "cut thetas in xv_step_subs ".
									"modelfit_post_subtool_analyze")
				if ($self->stop_motion());
			for(my $j = $self->n_model_thetas(); $j<scalar(@{$init_val}); $j++){ #leave original model thetas intact
				my $value = $init_val -> [$j];
				if (abs($value) <= $self->cutoff())
				{
					$pred_mod->initial_values(parameter_type => 'theta',
											  parameter_numbers => [[$j+1]],
											  new_values => [[0]] );
					$pred_mod->fixed(parameter_type => 'theta',
									 parameter_numbers => [[$j+1]],
									 new_values => [[1]] );
				}
			}

			# Make sure changes are reflected on disk.
			$pred_mod->_write(overwrite => 1);
			push( @models_to_run, $pred_mod );
		}else{
			print "est model index $i did not have defined ofv.";
		}
	}

	my %modelfit_arg;
	if(defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'modelfit'}){ # Override user threads. WHY???
		%modelfit_arg  = %{$self->subtool_arguments->{'modelfit'}};
	}
	$modelfit_arg{'cut_thetas_rounding_errors'} = 0;
	$modelfit_arg{'cut_thetas_maxevals'} = 0;
	$modelfit_arg{'handle_hessian_npd'} = 0;
	$self->stop_motion_call(tool=>'xv_step_subs',message => "set no cut_thetas_rounding errors in xv_step_subs ".
		"modelfit_post_subtool_analyze, push modelfit object with pred models only")
	if ($self->stop_motion());

	if( @models_to_run > 0 ){
		$self -> tools([]) unless (defined $self->tools);
		push( @{$self -> tools}, tool::modelfit -> new ( 'models' => \@models_to_run,
														 %modelfit_arg,
														 nmtran_skip_model => 2,
														 copy_data => 0
			) );
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
