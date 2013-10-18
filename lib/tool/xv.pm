use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::xv;
{
	use Carp;
	use tool::xv_step;
}
use debug;


#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(tool);

#---------------------------------------------------------------------
#         Used Packages
#---------------------------------------------------------------------
use tool::xv_step;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'xv_steps' => 'ARRAY', 'subtools' => 'ARRAY',
			'warnings' => 'SCALAR' );

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
			'debug' -> die( message => "ERROR in tool::xv->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'subtools'} = defined $parm{'subtools'} ? $parm{'subtools'} : ['xv_step'] unless defined $this -> {'subtools'};
	$this -> {'warnings'} = defined $parm{'warnings'} ? $parm{'warnings'} : 0 unless defined $this -> {'warnings'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 48 "lib/tool/xv_subs.pm" 
    {
	my $model;
	$model = $this -> models -> [0];
	unless( defined $model -> datas ){
	    croak("No data object in modelobject\n");
	}
    }
# line 78 libgen/tool/xv.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub xv_steps {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'xv_steps'} = $parm;
	} else {
		return $self -> {'xv_steps'};
	}
}

sub subtools {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subtools'} = $parm;
	} else {
		return $self -> {'subtools'};
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

sub add_xv_step {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_xv_step given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'xv_steps'}},
		tool::xv_step -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub xv_step_pre_fork_setup {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> xv_step_pre_fork_setup');
# line 62 "lib/tool/xv_subs.pm" 
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
# line 174 libgen/tool/xv.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> xv_step_pre_fork_setup');
	# End of Non-Dia code #

}

sub xv_step_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::xv->xv_step_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::xv->xv_step_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv->xv_step_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv->xv_step_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv->xv_step_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> xv_step_setup');
# line 92 "lib/tool/xv_subs.pm" 
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
# line 244 libgen/tool/xv.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> xv_step_setup');
	# End of Non-Dia code #

}

sub xv_step_post_subtool_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::xv->xv_step_post_subtool_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::xv->xv_step_post_subtool_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::xv->xv_step_post_subtool_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv->xv_step_post_subtool_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::xv->xv_step_post_subtool_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> xv_step_post_subtool_analyze');
# line 131 "lib/tool/xv_subs.pm" 
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
# line 316 libgen/tool/xv.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> xv_step_post_subtool_analyze');
	# End of Non-Dia code #

}

1;

