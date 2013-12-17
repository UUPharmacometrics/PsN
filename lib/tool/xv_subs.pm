# {{{ description, examples, synopsis, see also

# No method, just documentation
start description


end description

start examples
end examples

start synopsis


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
#	use Carp;
	use PsN;
	use tool::xv_step;
}
end include statements
# }}}

# {{{ new

start new
    {
	my $model;
	$model = $this -> models -> [0];
	unless( defined $model -> datas ){
	    croak("No data object in modelobject\n");
	}
    }
end new

# }}} new

# {{{ xv_step_pre_fork_setup

start xv_step_pre_fork_setup
    {
      print "\n xv: xv_step_pre_fork_setup\n" if ($self->stop_motion());
	my $subtools = undef;
	if( scalar @{$self -> subtools} > 1 ){
	    my @subtools = @{$self -> subtools};
	    shift( @subtools );
	    $subtools = \@subtools;
	}

	my %step_args;
	if (defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'xv_step'}){
	    %step_args = %{$self -> subtool_arguments -> {'xv_step'}};
	} 
	
	my $xv_step = tool::xv_step -> new( models => [$self -> models -> [0]], 
					    subtools => $subtools,
					    %step_args,
					    subtool_arguments => $self->subtool_arguments);

      $xv_step -> create_data_sets;
      $self -> xv_steps([]) unless (defined $self -> xv_steps);
      push( @{$self -> xv_steps}, $xv_step );
    }
end xv_step_pre_fork_setup

# }}}

# {{{ xv_step_setup

start xv_step_setup
    {
      print "\n xv: xv_step_setup\n" if ($self->stop_motion());
	unless( $model_number == 1 ){
	    my $subtools = undef;
	    if( scalar @{$self -> subtools} > 1 ){
		my @subtools = @{$self -> subtools};
		shift( @subtools );
		$subtools = \@subtools;
	    }

	    my %step_args;
	    if (defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'xv_step'}){
		%step_args = %{$self -> subtool_arguments -> {'xv_step'}};
	    } 

	    my $first_xv_step = $self -> xv_steps -> [0];
	    my $xv_step = tool::xv_step -> new( models => [$self -> models -> [$model_number - 1]],
						prediction_data => $first_xv_step -> prediction_data,
						estimation_data => $first_xv_step -> estimation_data, 
						stratify_on => $first_xv_step -> stratify_on, 
						subtools => $subtools,
						%step_args,
						subtool_arguments => $self -> subtool_arguments);

	    $self->stop_motion_call(tool=>'xv',message => "xv_step_setup model number $model_number")
		if ($self->stop_motion());
	    $self -> xv_steps([]) unless (defined $self -> xv_steps);
	    push( @{$self -> xv_steps}, $xv_step );
	} 
	
	$self -> tools([$self -> xv_steps -> [$model_number-1]]);
    }	
end xv_step_setup

# }}}

# {{{ xv_step_post_subtool_analyze

start xv_step_post_subtool_analyze
    {
      print "\n xv: xv_step_post_subtool_analyze\n" if ($self->stop_motion());
      my $subtools = undef;
      if( scalar @{$self -> subtools} > 1 ){
	my @subtools = @{$self -> subtools};
	shift( @subtools );
	$subtools = \@subtools;
      }
      my $newwarn = $self->warnings() + $self -> xv_steps -> [$model_number - 1] ->warnings; 
      $self->warnings($newwarn); 
      my $first_xv_step = $self -> xv_steps -> [0];
      if( $self -> xv_steps -> [$model_number - 1] -> cont ){
	$self->stop_motion_call(tool=>'xv',message => "create new xv_step, last was ok (cont ==1)")
	    if ($self->stop_motion());

	my %step_args;
	if (defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'xv_step'}){
	    %step_args = %{$self -> subtool_arguments -> {'xv_step'}};
	} 


	$self -> xv_steps -> [$model_number -1] = 
	    tool::xv_step -> new( models => [$self -> models -> [$model_number - 1]],
				  prediction_data => $first_xv_step -> prediction_data,
				  estimation_data => $first_xv_step -> estimation_data, 
				  stratify_on => $first_xv_step -> stratify_on, 
				  subtools => $subtools,
				  %step_args,
				  subtool_arguments => $self->subtool_arguments );
	
	$self->tools([]) unless (defined $self->tools);
	push( @{$self -> tools}, $self -> xv_steps -> [$model_number-1] );
      }
    }
end xv_step_post_subtool_analyze

# }}}

