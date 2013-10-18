use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::randtest;
use Carp;
use strict;
use File::Copy 'cp';
use data;
use OSspecific;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
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
	my %valid_parm = ( 'randtest_raw_results' => 'REF', 'copy_data' => 'SCALAR',
			'samples' => 'm_SCALAR', 'base_model' => 'REF',
			'stratify_on' => 'SCALAR', 'strat_index' => 'SCALAR',
			'rand_index' => 'SCALAR', 'randomization_column' => 'm_SCALAR',
			'logfile' => 'REF', 'results_file' => 'SCALAR',
			'match_transitions' => 'SCALAR', 'reference_column' => 'SCALAR' );

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
			'debug' -> die( message => "ERROR in tool::randtest->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'randtest_raw_results'} = defined $parm{'randtest_raw_results'} ? $parm{'randtest_raw_results'} : [] unless defined $this -> {'randtest_raw_results'};
	$this -> {'copy_data'} = defined $parm{'copy_data'} ? $parm{'copy_data'} : 0 unless defined $this -> {'copy_data'};
	$this -> {'logfile'} = defined $parm{'logfile'} ? $parm{'logfile'} : ['randtestlog.csv'] unless defined $this -> {'logfile'};
	$this -> {'results_file'} = defined $parm{'results_file'} ? $parm{'results_file'} : 'randtest_results.csv' unless defined $this -> {'results_file'};
	$this -> {'match_transitions'} = defined $parm{'match_transitions'} ? $parm{'match_transitions'} : 0 unless defined $this -> {'match_transitions'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 19 "lib/tool/randtest_subs.pm" 
{

    for my $accessor ('logfile','raw_results_file','raw_nonp_file'){
	my @new_files=();
	my @old_files = @{$this->$accessor};
	for (my $i=0; $i < scalar(@old_files); $i++){
	    my $name;
	    my $ldir;
	    ( $ldir, $name ) =
		OSspecific::absolute_path( $this ->directory(), $old_files[$i] );
	    push(@new_files,$ldir.$name) ;
	}
	$this->$accessor(\@new_files);
    }	

    croak("No \$PROBLEM in input model") unless 
	(defined $this ->models()->[0]->problems and scalar(@{$this ->models()->[0]->problems})>0);

    croak("No \$INPUT found") unless 
	(defined $this ->models()->[0]->problems->[0]->inputs and 
	 scalar(@{$this ->models()->[0]->problems->[0]->inputs})>0);
    croak("No \$DATA found") unless 
	(defined $this ->models()->[0]->problems->[0]->datas and 
	 scalar(@{$this ->models()->[0]->problems->[0]->datas})>0);

    #make sure IGNORE=C is not used
    my @ignores = $this->models->[0]->get_option_value(record_name=>'data', 
						       option_name=>'IGNORE',
						       problem_index=>0, 
						       record_index=>0,
						       option_index=>'all');

    foreach my $ig (@ignores){
	croak("PsN randtest cannot handle IGNORE=C. Use IGNORE=@ instead\n")
	    if ($ig eq 'C');
    }

    #Find column index of rand column
    #Find column index of strat column
    my $counter = 0;
    foreach my $opt (@{$this->models->[0]->problems->[0]->inputs->[0]->options()}){
#	print $opt->name()." ".$this->randomization_column()."\n";
	$this->rand_index($counter) if ($opt->name() eq $this->randomization_column());
	$this->strat_index($counter) if ((defined $this->stratify_on()) and ($opt->name() eq $this->stratify_on()));
	$counter++;
    }
    croak("Could not find randomization column ".$this->randomization_column()." in \$INPUT")
	unless (defined $this->rand_index);
    croak("Could not find stratification column ".$this->stratify_on()." in \$INPUT")
	unless ((not defined $this->stratify_on) or (defined $this->strat_index));

    croak("Number of samples must be larger than 0") unless ($this->samples()>0);
    
}
# line 131 libgen/tool/randtest.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub randtest_raw_results {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'randtest_raw_results'} = $parm;
	} else {
		return $self -> {'randtest_raw_results'};
	}
}

sub copy_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'copy_data'} = $parm;
	} else {
		return $self -> {'copy_data'};
	}
}

sub samples {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'samples'} = $parm;
	} else {
		return $self -> {'samples'};
	}
}

sub base_model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'base_model'} = $parm;
	} else {
		return $self -> {'base_model'};
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

sub strat_index {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'strat_index'} = $parm;
	} else {
		return $self -> {'strat_index'};
	}
}

sub rand_index {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'rand_index'} = $parm;
	} else {
		return $self -> {'rand_index'};
	}
}

sub randomization_column {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'randomization_column'} = $parm;
	} else {
		return $self -> {'randomization_column'};
	}
}

sub logfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'logfile'} = $parm;
	} else {
		return $self -> {'logfile'};
	}
}

sub results_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'results_file'} = $parm;
	} else {
		return $self -> {'results_file'};
	}
}

sub match_transitions {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'match_transitions'} = $parm;
	} else {
		return $self -> {'match_transitions'};
	}
}

sub reference_column {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'reference_column'} = $parm;
	} else {
		return $self -> {'reference_column'};
	}
}

sub _sampleTools {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'samples' => 'SCALAR', 'subjects' => 'SCALAR',
			'target' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->_sampleTools: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->_sampleTools: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->_sampleTools: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->_sampleTools: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->_sampleTools: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $samples = defined $parm{'samples'} ? $parm{'samples'} : 200;
	my $subjects = $parm{'subjects'};
	my @newModels;
	my $target = defined $parm{'target'} ? $parm{'target'} : 'disk';

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@newModels;
}

sub modelfit_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->modelfit_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> modelfit_setup');
# line 81 "lib/tool/randtest_subs.pm" 
{	

    my $model = $self ->models() -> [$model_number-1];

    # Check which models that hasn't been run and run them 
    
    # ------------------------  Run original run  -------------------------------

    unless ( $model -> is_run and ((not defined $self->base_model) or $self->base_model->is_run) ) {
	my %subargs = ();
	if ( defined $self -> subtool_arguments() ) {
	    %subargs = %{$self -> subtool_arguments()};
	}

	if( $self -> nonparametric_etas() or
	    $self -> nonparametric_marginals() ) {
	    $model -> add_nonparametric_code unless ($model->is_run);
	    $self->base_model -> add_nonparametric_code if (defined $self->base_model and not $self->base_model->is_run);
	}
	my @models=();
	my $message = "Executing ";
	unless ($model->is_run){
	    push(@models,$model) ;
	    $message .= "input model";
	}
	if (defined $self->base_model and not $self->base_model->is_run){
	    push(@models,$self->base_model) ;
	    if ($model->is_run){
		$message .= "base model";
	    }else{
		$message .= "and base model";
	    }
	}

	my $orig_fit = tool::modelfit ->
	    new( %{common_options::restore_options(@common_options::tool_options)},
		 base_directory	 => $self ->directory(),
		 directory		 => $self ->directory().
		 '/orig_modelfit_dir'.$model_number,
		 models		 => \@models,
		 threads               => $self->threads,
		 logfile	         => undef,
		 raw_results           => undef,
		 prepared_models       => undef,
		 top_tool              => 0,
		 %subargs );
	
	ui -> print( category => 'randtest',
		     message => $message );
	
	$orig_fit -> run;
	
    }

    my $output = $model -> outputs -> [0];
    my $base_output;
    $base_output = $self->base_model -> outputs ->[0] if (defined $self->base_model);
    my $new_mod;
    my @problems   = @{$model -> problems};
    my @new_models;

    if (scalar(@{$model -> datas})>1){
	print "\nWarning: PsN randtest only randomizes first data file, seems like model has more than one data file\n";
    }
    
    my $orig_data = $model -> datas->[0];

    my $done = ( -e $self ->directory()."/m$model_number/done" ) ? 1 : 0;
    my $new_datas;
    if ( not $done ) {
	ui -> print( category => 'randtest',
		     message  => "Randomizing column ".$self->randomization_column." in ".$orig_data -> filename );

	$new_datas = $orig_data -> randomize_data( directory   => $self ->directory().'/m'.$model_number,
						   name_stub   => 'rand',
						   samples     => $self->samples(),
						   stratify_index => $self->strat_index(), 
						   rand_index => $self->rand_index(), 
						   equal_obs => (not $self->match_transitions()));
#						   target	     => 'disk');

	$self->stop_motion_call(tool=>'randtest',message => "Created randomized datasets in ".
				$self ->directory().'m'.$model_number)
	    if ($self->stop_motion());

	for ( my $j = 0; $j < $self->samples(); $j++ ) {
	    my @data_arr = ($new_datas->[$j]) x scalar(@{$model->problems});

	    $new_mod = $model ->  copy( filename    => $self -> directory().'m'.$model_number.'/rand_'.($j+1).'.mod',
					output_same_directory => 1,
					copy_data   => 0,
					copy_output => 0);
	    
	    $new_mod->datas(\@data_arr); #sets record and data object. Number of $PROBS and length data_arr must match
	    
	    if( $self -> shrinkage() ) {
		$new_mod -> shrinkage_stats( enabled => 1 );
		$new_mod -> shrinkage_modules( $model -> shrinkage_modules );
	    }

	    if( $self -> nonparametric_etas() or
		$self -> nonparametric_marginals() ) {
		$new_mod -> add_nonparametric_code;
	    }

	    $new_mod -> update_inits( from_output => $output );
	    $new_mod -> _write;

	    push( @new_models, $new_mod );
	}
	$self->stop_motion_call(tool=>'randtest',message => "Created one modelfile per dataset in ".
				$self ->directory().'m'.$model_number)
	    if ($self->stop_motion());
	
	# Create a checkpoint. Log the samples and individuals.
	open( DONE, ">".$self ->directory()."/m$model_number/done" ) ;
	print DONE "Randomization of ",$orig_data -> filename, " performed\n";
	print DONE $self->samples()." samples\n";
	close( DONE );
    } else {
	ui -> print( category => 'randtest',
		     message  => "Recreating randtest from previous run." );

	# Recreate the datasets and models from a checkpoint
	my ($stored_filename, $stored_samples);
	my ($stored_filename_found, $stored_samples_found);
	open( DONE, $self ->directory()."/m$model_number/done" );
	while( <DONE> ){
	    if( /^Randomization of (.+) performed$/ ){
		$stored_filename = $1;
		$stored_filename_found = 1;
		next;
	    }
	    if( /^(\d+) samples$/ ){
		ui -> print( category => 'randtest',
			     message  => "Samples saved: $1" );
		$stored_samples = $1;
		$stored_samples_found = 1;
		next;
	    }
	}
	close( DONE );
	unless( $stored_filename_found and $stored_samples_found ) {
	    croak("The randtest/m1/done file could not be parsed.");
	}
	if ( $stored_samples < $self->samples() ) {
	    croak("The number of samples saved in previous run ($stored_samples) ".
			  "is smaller than the number of samples specified for this run (".
			  $self->samples().")" );
	}

	# Reinitiate the model objects
	for ( my $j = 1; $j <= $self->samples(); $j++ ) {
	    my ($model_dir, $filename) = OSspecific::absolute_path( $self ->directory().'/m'.$model_number,
								    'rand_'.($j+1).'.mod' );
	    
	    $new_mod = model ->
		new( directory   => $model_dir,
		     filename    => $filename,
		     extra_files => $model -> extra_files,
		     target      => 'disk',
		     ignore_missing_files => 1,
		);
	      push( @new_models, $new_mod );
	}
	ui -> print( category => 'randtest',
		     message  => "Using $stored_samples previously randomized ".
		     "data sets sets from $stored_filename" )
    }

    $self -> prepared_models -> [$model_number-1]{'own'} = \@new_models;

    my @subtools = ();
    @subtools = @{$self -> subtools()} if (defined $self->subtools());
    shift( @subtools );
    my %subargs = ();
    if ( defined $self -> subtool_arguments() ) {
	%subargs = %{$self -> subtool_arguments()};
    }
    if (not $self->copy_data()){
	$subargs{'data_path'}='../../m'.$model_number.'/';
    }
    $self->tools([]) unless (defined $self->tools());
    
    push( @{$self -> tools()},
	  tool::modelfit ->
	  new( %{common_options::restore_options(@common_options::tool_options)},
	       models		 => \@new_models,
	       threads               => $self->threads,
	       directory             => $self ->directory().'/modelfit_dir'.$model_number,
	       _raw_results_callback => $self ->
	       _modelfit_raw_results_callback( model_number => $model_number ),
	       subtools              => \@subtools,
	       logfile		 => [$self -> logfile()->[$model_number-1]],
	       raw_results           => undef,
	       prepared_models       => undef,
	       top_tool              => 0,
	       %subargs ) );

    $self->stop_motion_call(tool=>'randtest',message => "Created a modelfit object to run all the models in ".
			    $self ->directory().'m'.$model_number)
	if ($self->stop_motion());


}
# line 582 libgen/tool/randtest.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> modelfit_setup');
	# End of Non-Dia code #

}

sub cleanup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->cleanup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->cleanup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->cleanup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->cleanup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->cleanup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> cleanup');
# line 391 "lib/tool/randtest_subs.pm" 
{
  #remove datafiles in modelfit_dirX/NM_runX
  #leave in m1

  my $prob=1;
  while (1){
    my $dir = $self ->directory()."modelfit_dir$prob/";
    last unless (-e $dir);
    my $sample=1;
    while (1){
      my $file = $dir."NM_run".$sample."/rand_".$sample.".dta"; 
      last unless (-e $file);
      unlink $file;
      $sample++;
    }
    $prob++;
  }

}
# line 638 libgen/tool/randtest.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> cleanup');
	# End of Non-Dia code #

}

sub randomize {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'target' => 'SCALAR', 'model' => 'model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->randomize: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->randomize: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->randomize: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->randomize: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->randomize: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $target = defined $parm{'target'} ? $parm{'target'} : 'disk';
	my @randomized_models;
	my $model = $parm{'model'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@randomized_models;
}

sub calculate_delta_ofv {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->calculate_delta_ofv: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->calculate_delta_ofv: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->calculate_delta_ofv: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->calculate_delta_ofv: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->calculate_delta_ofv: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub general_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'class' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->general_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->general_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->general_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->general_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->general_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $class = $parm{'class'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub modelfit_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->modelfit_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> modelfit_analyze');
# line 416 "lib/tool/randtest_subs.pm" 
      {
#	$self-> cleanup();
	  1;

      }
# line 787 libgen/tool/randtest.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> modelfit_analyze');
	# End of Non-Dia code #

}

sub modelfit_post_fork_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->modelfit_post_fork_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_post_fork_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_post_fork_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_post_fork_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->modelfit_post_fork_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _modelfit_raw_results_callback {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->_modelfit_raw_results_callback: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->_modelfit_raw_results_callback: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->_modelfit_raw_results_callback: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _modelfit_raw_results_callback');
# line 294 "lib/tool/randtest_subs.pm" 
      {
	# Use the  raw_results file.
	my ($dir,$file) = 
	  OSspecific::absolute_path( $self ->directory(),
				     $self -> raw_results_file()->[$model_number-1] );
	my ($dir,$nonp_file) = 
	  OSspecific::absolute_path( $self ->directory(),
				     $self -> raw_nonp_file()->[$model_number-1] );
	my $orig_mod = $self ->models()->[$model_number-1];
	my $base_mod_ofv;
	my $base_mod;
	if (defined $self->base_model and $self->base_model->is_run){
	    $base_mod= $self->base_model;
	    $base_mod_ofv=$self->base_model->outputs->[0]->ofv(); #array over problems and subprobs
	}

	$subroutine = sub {
	  my $modelfit = shift;
	  my $mh_ref   = shift;
	  my %max_hash = %{$mh_ref};
	  $modelfit -> raw_results_file([$dir.$file] );
	  $modelfit -> raw_nonp_file( [$dir.$nonp_file] );

	  # The prepare_raw_results in the modelfit will fix the
	  # raw_results for each rand sample model, we must add
	  # the result for the original model.

	  my %dummy;

	  my ($raw_results_row, $nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
										model => $orig_mod,
										raw_line_structure => \%dummy );

	  my ($base_raw_results_row, $base_nonp_rows);
	  if (defined $base_mod){
	      ($base_raw_results_row, $base_nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
											   model => $base_mod,
											   raw_line_structure => \%dummy );
	  }
	  $orig_mod -> outputs -> [0] -> flush;
	  $raw_results_row->[0]->[0] = 'input';
	  
	  unshift( @{$modelfit -> raw_results()}, @{$raw_results_row} );
	  if (defined $base_raw_results_row){
	      $base_raw_results_row->[0]->[0] = 'base';
	      unshift( @{$modelfit -> raw_results()}, @{$base_raw_results_row} ) ;
	  }
	  $self->raw_line_structure($modelfit -> raw_line_structure());

	  if ( defined $base_mod_ofv ) {
	      my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'problem'});
	      my $probindex = $start;
	      my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'subproblem'});
	      my $subindex = $start;
	      my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'ofv'});
	      my $ofvindex=$start;
	      croak("could not find ofv in raw results header") unless (defined $ofvindex);

	      foreach my $row ( @{$modelfit -> raw_results()} ) {
		  my $delta_ofv = $row->[$ofvindex] - $base_mod_ofv->[($row->[$probindex]-1)]->[($row->[$subindex]-1)];
		  my @oldrow =@{$row};
		  $row = [@oldrow[0 .. $ofvindex],$delta_ofv,@oldrow[$ofvindex+1 .. $#oldrow]]; 
	      }

	      my @old_header = @{$modelfit -> raw_results_header()};
	      my $headerindex;
	      for (my $k=0; $k<scalar(@old_header);$k++){
		  $headerindex = $k if ($old_header[$k] eq 'ofv');
	      }
	      $modelfit -> raw_results_header(
		  [@old_header[0 .. $headerindex],'deltaofv',@old_header[$headerindex+1 .. $#old_header]]);

	      foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure()})){
		  foreach my $category (keys %{$self->raw_line_structure() -> {$mod}}){
		      next if ($category eq 'line_numbers');
		      my ($start,$len) = split(',',$self->raw_line_structure() -> {$mod}->{$category});
		      $self->raw_line_structure() -> {$mod}->{$category} = ($start+1).','.$len
			  if ($start > $ofvindex); #+1 for deltaofv
		  }
		  $self->raw_line_structure() -> {$mod}->{'deltaofv'} = ($ofvindex+1).',1';
	      }
	  }
	  $self->raw_line_structure() -> {'input'} = $self->raw_line_structure() -> {'1'}; #input model
	  $self->raw_line_structure() -> {'base'} = $self->raw_line_structure() -> {'1'}; 
	  $self->raw_line_structure() -> write( $dir.'raw_results_structure' );

	  $self -> raw_results_header($modelfit -> raw_results_header());
	  $self -> raw_results($modelfit -> raw_results());

	};
	return $subroutine;
      }
# line 952 libgen/tool/randtest.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _modelfit_raw_results_callback');
	# End of Non-Dia code #

	return \&subroutine;
}

sub _sse_raw_results_callback {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::randtest->_sse_raw_results_callback: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::randtest->_sse_raw_results_callback: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::randtest->_sse_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->_sse_raw_results_callback: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::randtest->_sse_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \&subroutine;
}

sub sse_read_raw_results {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub prepare_results {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> prepare_results');
# line 428 "lib/tool/randtest_subs.pm" 
      {
	  1;

      }
# line 1013 libgen/tool/randtest.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> prepare_results');
	# End of Non-Dia code #

}

1;

