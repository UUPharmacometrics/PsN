use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::xv_step;
    {
	use Carp;
	use tool::modelfit;
    }
use debug;


#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(tool);

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'nr_validation_groups' => 'SCALAR', 'stratify_on' => 'SCALAR',
			'cutoff' => 'SCALAR', 'n_model_thetas' => 'SCALAR',
			'estimation_data' => 'ARRAY', 'prediction_data' => 'ARRAY',
			'init' => 'CODE', 'post_analyze' => 'CODE',
			'cont' => 'SCALAR', 'own_parameters' => 'HASH',
			'estimation_models' => 'ARRAY', 'prediction_models' => 'ARRAY',
			'prediction_is_run' => 'SCALAR', 'warnings' => 'SCALAR',
			'estimate_only' => 'SCALAR', 'predict_only' => 'SCALAR',
			'last_est_complete' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
	}
	foreach my $givenp ( keys %parm ) {
		$superParms{$givenp} = $parm{$givenp} and next unless( defined $valid_parm{$givenp});

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::xv_step->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv_step->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'nr_validation_groups'} = defined $parm{'nr_validation_groups'} ? $parm{'nr_validation_groups'} : 5 unless defined $this -> {'nr_validation_groups'};
	$this -> {'n_model_thetas'} = defined $parm{'n_model_thetas'} ? $parm{'n_model_thetas'} : 0 unless defined $this -> {'n_model_thetas'};
	$this -> {'estimation_data'} = defined $parm{'estimation_data'} ? $parm{'estimation_data'} : [] unless defined $this -> {'estimation_data'};
	$this -> {'prediction_data'} = defined $parm{'prediction_data'} ? $parm{'prediction_data'} : [] unless defined $this -> {'prediction_data'};
	$this -> {'estimation_models'} = defined $parm{'estimation_models'} ? $parm{'estimation_models'} : [] unless defined $this -> {'estimation_models'};
	$this -> {'prediction_models'} = defined $parm{'prediction_models'} ? $parm{'prediction_models'} : [] unless defined $this -> {'prediction_models'};
	$this -> {'prediction_is_run'} = defined $parm{'prediction_is_run'} ? $parm{'prediction_is_run'} : 0 unless defined $this -> {'prediction_is_run'};
	$this -> {'warnings'} = defined $parm{'warnings'} ? $parm{'warnings'} : 0 unless defined $this -> {'warnings'};
	$this -> {'estimate_only'} = defined $parm{'estimate_only'} ? $parm{'estimate_only'} : 0 unless defined $this -> {'estimate_only'};
	$this -> {'predict_only'} = defined $parm{'predict_only'} ? $parm{'predict_only'} : 0 unless defined $this -> {'predict_only'};
	$this -> {'last_est_complete'} = defined $parm{'last_est_complete'} ? $parm{'last_est_complete'} : 0 unless defined $this -> {'last_est_complete'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 59 "lib/tool/xv_step_subs.pm" 
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
# line 95 libgen/tool/xv_step.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub nr_validation_groups {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nr_validation_groups'} = $parm;
	} else {
		return $self -> {'nr_validation_groups'};
	}
}

sub stratify_on {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'stratify_on'} = $parm;
	} else {
		return $self -> {'stratify_on'};
	}
}

sub cutoff {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cutoff'} = $parm;
	} else {
		return $self -> {'cutoff'};
	}
}

sub n_model_thetas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'n_model_thetas'} = $parm;
	} else {
		return $self -> {'n_model_thetas'};
	}
}

sub estimation_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimation_data'} = $parm;
	} else {
		return $self -> {'estimation_data'};
	}
}

sub prediction_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prediction_data'} = $parm;
	} else {
		return $self -> {'prediction_data'};
	}
}

sub init {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'init'} = $parm;
	} else {
		return $self -> {'init'};
	}
}

sub post_analyze {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'post_analyze'} = $parm;
	} else {
		return $self -> {'post_analyze'};
	}
}

sub cont {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cont'} = $parm;
	} else {
		return $self -> {'cont'};
	}
}

sub own_parameters {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'own_parameters'} = $parm;
	} else {
		return $self -> {'own_parameters'};
	}
}

sub estimation_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimation_models'} = $parm;
	} else {
		return $self -> {'estimation_models'};
	}
}

sub prediction_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prediction_models'} = $parm;
	} else {
		return $self -> {'prediction_models'};
	}
}

sub prediction_is_run {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prediction_is_run'} = $parm;
	} else {
		return $self -> {'prediction_is_run'};
	}
}

sub warnings {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'warnings'} = $parm;
	} else {
		return $self -> {'warnings'};
	}
}

sub estimate_only {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimate_only'} = $parm;
	} else {
		return $self -> {'estimate_only'};
	}
}

sub predict_only {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'predict_only'} = $parm;
	} else {
		return $self -> {'predict_only'};
	}
}

sub last_est_complete {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'last_est_complete'} = $parm;
	} else {
		return $self -> {'last_est_complete'};
	}
}

sub die {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::xv_step->die: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::xv_step->die: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv_step->die: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->die: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->die: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = defined $parm{'message'} ? $parm{'message'} : "Error message Undefined \n";

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> die');
# line 324 "lib/tool/xv_step_subs.pm" 
# line 372 libgen/tool/xv_step.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> die');
	# End of Non-Dia code #

}

sub warn {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::xv_step->warn: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::xv_step->warn: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv_step->warn: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->warn: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->warn: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = defined $parm{'message'} ? $parm{'message'} : "Warning message Undefined\n";

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> warn');
# line 328 "lib/tool/xv_step_subs.pm" 
# line 410 libgen/tool/xv_step.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> warn');
	# End of Non-Dia code #

}

sub modelfit_setup {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> modelfit_setup');
# line 158 "lib/tool/xv_step_subs.pm" 
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
# line 494 libgen/tool/xv_step.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> modelfit_setup');
	# End of Non-Dia code #

}

sub modelfit_analyze {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> modelfit_analyze');
# line 311 "lib/tool/xv_step_subs.pm" 
    {
      print "\n xv_step: modelfit_analyze\n" if ($self->stop_motion());
	if( defined $self -> post_analyze ){
	    my $temp = &{$self -> post_analyze}($self);
	    $self -> cont($temp); #is this really a boolean???
	} else {
	    $self -> cont(0);
	}
    }
# line 515 libgen/tool/xv_step.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> modelfit_analyze');
	# End of Non-Dia code #

}

sub create_data_sets {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> create_data_sets');
# line 77 "lib/tool/xv_step_subs.pm" 
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
# line 603 libgen/tool/xv_step.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> create_data_sets');
	# End of Non-Dia code #

}

sub modelfit_post_subtool_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv_step->modelfit_post_subtool_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> modelfit_post_subtool_analyze');
# line 236 "lib/tool/xv_step_subs.pm" 
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
# line 711 libgen/tool/xv_step.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> modelfit_post_subtool_analyze');
	# End of Non-Dia code #

}

1;

