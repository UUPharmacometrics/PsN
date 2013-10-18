# {{{ description, examples, synopsis, see also

# No tool, just documentation
start description

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

end description

start examples
end examples

start synopsis

    # The return value is a reference to the data objects containing
    # the prediction and the estimation datasets.

end synopsis

start see_also
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
end see_also

# }}}

# {{{ include
start include statements
    {
	use Carp;
	use tool::modelfit;
    }
end include statements
# }}}

# {{{ new
start new
    {
	my $model;
	$model = $this -> models -> [0];
	unless( defined $model -> datas ){
	    $this -> die ( message => "No data object in modelobject\n" );
	}

	if( $this -> predict_only and $this -> estimate_only ){
	    $this -> predict_only(0);
	    $this -> estimate_only(0);
	}
	
    }
end new
# }}} new

# {{{ create_data_sets
start create_data_sets
    {
    	my $model = $self -> models -> [0];
	
	my @datas = @{$model->datas};
        my $data_obj = $datas[0];
	
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
						   target => 'mem',
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
	
	for( my $i = 0; $i <= $#{$subsets}; $i++ )
	{
	    $subsets -> [$i] -> filename( 'pred_data' . $i . '.dta' );
	    $subsets -> [$i] -> directory( $self -> directory );
	    $subsets -> [$i] -> _write();
	    push( @{$self -> prediction_data}, $subsets -> [$i] );

	    my $est_data;
	    for( my $j = 0; $j <= $#{$subsets} ; $j++ )
	    {
		if( $j == 0 ){
		    $est_data = data -> new( filename => 'est_data' . $i . '.dta', 
					     directory => $self -> directory, 
					     ignore_missing_files => 1, 
					     header => $data_obj -> header );
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
	    push( @{$self -> estimation_data}, $est_data );
	}
	$self->stop_motion_call(tool=>'xv_step_subs',message => "written data in ".$self->directory)
	    if ($self->stop_motion());

    }
end create_data_sets
# }}}

# {{{ modelfit_setup
start modelfit_setup
    {
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
	      copy_data => 0, copy_output => 0,
	      target => 'mem' );
	  
#	  $model_copy_pred -> set_option( record_name => 'estimation',
#					  fuzzy_match => 1,
#					  option_name => 'MAX',
#					  option_value => 0 );
	  
	  #to handle NM7 methods
	  $model_copy_pred -> set_maxeval_zero(print_warning => 0,
					       need_ofv => 1,
					       last_est_complete => $self->last_est_complete());
	  $model_copy_pred->remove_option(record_name => 'estimation',
					  option_name => 'NOABORT');

	  # Make sure changes is reflected on disk.
	  $model_copy_pred -> _write();
	  
	  $model_copy_pred -> datas( [$self -> prediction_data -> [$i]] );
	  push( @{$self -> prediction_models}, $model_copy_pred );
	}
	
	unless( $self -> predict_only ){
	  my $model_copy_est = $model -> copy(filename => 
					      $self -> directory().'m1/est_model'.$i.'.mod',
					      output_same_directory => 1,
					      copy_data => 0, copy_output => 0,
					      target => 'mem' );
	  

	  $model_copy_est -> datas( [$self -> estimation_data -> [$i]] );
	  push( @{$self -> estimation_models}, $model_copy_est );
	}
      }

      my %modf_args;
      if (defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'modelfit'}){
	  %modf_args = %{$self -> subtool_arguments -> {'modelfit'}};
      } 

      
      unless( $self -> predict_only ){
	$self -> tools( [ tool::modelfit -> new ( 'models' => $self -> estimation_models,
						  %modf_args ) ] );
      } elsif( not $self -> estimate_only ) {
	$self -> tools( [ tool::modelfit -> new ( 'models' => $self -> prediction_models,
						  %modf_args ) ] );
      }

      $self->stop_motion_call(tool=>'xv_step_subs',message => "a new modelfit object for estimation")
	  if ($self->stop_motion());
      
      if( defined $self -> init ){
	&{$self -> init}($self);
      }
    }	
end modelfit_setup

# }}}

# {{{ modelfit_post_subtool_analyze
start modelfit_post_subtool_analyze
    { 
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
	  $pred_mod->_write();
	  push( @models_to_run, $pred_mod );
	}else{
	  print "est model index $i did not have defined ofv.";
	}
      }
      
      my %modelfit_arg;
      if(defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'modelfit'}){ # Override user threads. WHY???
	%modelfit_arg  = %{$self->subtool_arguments->{'modelfit'}};
      }
#      $modelfit_arg{'threads'} = 1; #why not in parallel??
#      $modelfit_arg{'handle_maxevals'} = 0; #don't need this since maxeval=0 or similar
      $modelfit_arg{'cut_thetas_rounding_errors'} = 0;
      $modelfit_arg{'cut_thetas_maxevals'} = 0;
      $modelfit_arg{'handle_hessian_npd'} = 0;
      $self->stop_motion_call(tool=>'xv_step_subs',message => "set no cut_thetas_rounding errors in xv_step_subs ".
			      "modelfit_post_subtool_analyze, push modelfit object with pred models only")
	  if ($self->stop_motion());
      
      if( @models_to_run > 0 ){
	  $self -> tools([]) unless (defined $self->tools);
	  push( @{$self -> tools}, tool::modelfit -> new ( 'models' => \@models_to_run,
							   %modelfit_arg ) );
      }
    }
end modelfit_post_subtool_analyze
# }}}

# {{{ modelfit_analyze
start modelfit_analyze
    {
      print "\n xv_step: modelfit_analyze\n" if ($self->stop_motion());
	if( defined $self -> post_analyze ){
	    my $temp = &{$self -> post_analyze}($self);
	    $self -> cont($temp); #is this really a boolean???
	} else {
	    $self -> cont(0);
	}
    }
end modelfit_analyze
# }}}

start die
end die


start warn
end warn
