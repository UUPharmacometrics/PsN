use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::nonpb;
use Carp;
use Data::Dumper;
use Math::Random;
use strict;
use tool::modelfit;
use tool::bootstrap;
use tool::pind;
use File::Copy qw/mv/;
use File::Path;
use model;
use ui;
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
	my %valid_parm = ( 'lst_file' => 'm_SCALAR', 'n_individuals' => 'SCALAR',
			'samples' => 'SCALAR', 'nonpb_version' => 'SCALAR',
			'etas' => 'SCALAR', 'start_bs_models' => 'REF',
			'bs_directory' => 'SCALAR', 'pind_directories' => 'ARRAY',
			'logfile' => 'REF', 'results_file' => 'SCALAR' );

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
			'debug' -> die( message => "ERROR in tool::nonpb->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'nonpb_version'} = defined $parm{'nonpb_version'} ? $parm{'nonpb_version'} : 1 unless defined $this -> {'nonpb_version'};
	$this -> {'start_bs_models'} = defined $parm{'start_bs_models'} ? $parm{'start_bs_models'} : [] unless defined $this -> {'start_bs_models'};
	$this -> {'logfile'} = defined $parm{'logfile'} ? $parm{'logfile'} : ['nonpb.log'] unless defined $this -> {'logfile'};
	$this -> {'results_file'} = defined $parm{'results_file'} ? $parm{'results_file'} : 'nonpb_results.csv' unless defined $this -> {'results_file'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 20 "lib/tool/nonpb_subs.pm" 
{
  unless ($this->nonpb_version == 1 || $this->nonpb_version == 2) {
    croak('nonpb version must be either 1 or 2 (2 is default).');
  }

  if ( scalar (@{$this->models->[0]->problems}) != 1 ){
    croak('The input model must contain exactly one problem.');
  }


  #check at least one omega -> eta
  my $record_ref = $this->models->[0]->record(record_name => 'omega' );
  unless ( scalar(@{$record_ref}) > 0 ){ 
    croak('The input model must contain at least one $OMEGA record.');
  }
  my $model_n_etas = $this->models->[0]->nomegas->[0];
  if (defined $this->etas) {
    if ($this->etas > $model_n_etas) {
      croak("Too many etas requested, model ".
		     "contains only $model_n_etas.");
    }
    if ($this->etas < 1) {
      croak("Option -etas must be larger than 0. ");
    }
  }

  unless ( -e $this->lst_file ) {
    croak("Cannot find lst-file " . $this->lst_file);
  }

  unless (defined $this->samples) {
    croak("The option -samples is required.");
  }
  unless ( $this->samples > 0 ) {
    croak("Number of samples must be larger than 0.");
  }


}  
# line 117 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub lst_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'lst_file'} = $parm;
	} else {
		return $self -> {'lst_file'};
	}
}

sub n_individuals {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'n_individuals'} = $parm;
	} else {
		return $self -> {'n_individuals'};
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

sub nonpb_version {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonpb_version'} = $parm;
	} else {
		return $self -> {'nonpb_version'};
	}
}

sub etas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'etas'} = $parm;
	} else {
		return $self -> {'etas'};
	}
}

sub start_bs_models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'start_bs_models'} = $parm;
	} else {
		return $self -> {'start_bs_models'};
	}
}

sub bs_directory {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'bs_directory'} = $parm;
	} else {
		return $self -> {'bs_directory'};
	}
}

sub pind_directories {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'pind_directories'} = $parm;
	} else {
		return $self -> {'pind_directories'};
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

sub modelfit_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->modelfit_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> modelfit_setup');
# line 67 "lib/tool/nonpb_subs.pm" 
{
  my $model_n_etas = $self->models->[0]->nomegas->[0];
  unless (defined $self->etas) {
    $self->etas($model_n_etas);
  }

  if ($self->nonpb_version == 1){
    #section 1 and 2 nonp_bootstrap_v1 are done in bin-script
    return;
    
  } else {

    #Section 3,4,5 from URS nonp_bootstrap_v2
    #Create 1+samples new models from original plus 'samples' bootstrap models
    #The modified bootstrap models shall each have P_values matrix created by pind tool,
    #and therefore let pind do model modification and estimation, after updating inits here
    #since we want to update inits using output object instead of lst-file. When
    #using pind tool, set options tablename and modelname to get filenames requested in
    #URS nonp_bootstrap_v2
    #The modified original model shall only be estimated after 
    #modification, no P_values matrix created. 
    #Therefore do modification here, and push to modelfit

    #Updating part of sec 3.1, plus sec 3.8 nonp_bootstrap_v2 for bs models. 
    #Then call pind for 3.2-3.7,sec4,sec5 nonp_bootstrap_v2
    my @directories; #need to keep track of where pind results end up

    #save directory name so can delete models later
    $self->bs_directory($self->start_bs_models->[0]->directory);
    
    for (my $id=1; $id<= $self->samples; $id++){
      my $bs_model = shift(@{$self->start_bs_models});
      my $stem='bs_model_'.$id;
      $bs_model -> filename($stem .'.mod') ;
      #my $copy = $bs_model -> copy( filename => );

      #3.1 update inits. (Section 3.1 fixing parameters is done in pind tool, not here)
      $bs_model -> update_inits( from_output => $bs_model -> outputs -> [0] );
      #$bs_model = undef;
      
      #3.8 Use original dataset by changing $DATA
      #is this it????
      $bs_model -> datafiles( new_names =>$self->models->[0]->datafiles(absolute_path => 1) );

      #Section 3.2-3.7, 4 and 5 in URS nonp_bootstrap_v2 are all supposed to be done by pind 

      $bs_model -> extra_output(['fort.80']); #needed by pind
      my $old_category = ui->category();
      ui->category('pind');

      my $pind = tool::pind -> 
	  new ( %{common_options::restore_options(@common_options::tool_options)},
		top_tool     => 0,
		directory      => "pind_dir$id",

		models	     => [$bs_model],
		modelname      => $stem,
		tablename      => $stem.'.patab',
		ind_param      => 'eta',
		lst_file       => '0',
		clean          => 0
		);
      
      push (@directories,$pind -> directory());
      $pind -> run;
      $self->n_individuals($pind->n_individuals) if ($id == 1);

      #keep only relevant files, delete everything else
      #copy P_values to directory intermediate files under new name P_values_$id.csv, 
      #and change filename for read statement later in code
      my $oldname = $pind ->directory().'P_values.csv'; 
      my $newname='intermediate_files/P_values_'.$id.'.csv';
      mv ($oldname,$newname);

      #copy bs_model_$id.patab to directory intermediate files under new name P_values_$id.csv, 
      #and change filename for read statement later in code
      $oldname = $pind->directory()."m1/bs_model_$id.patab";
      $newname='intermediate_files/bs_model_'.$id.'.patab';
      mv ($oldname,$newname);

      #delete whoel directory $pind->directory()
      rmtree($pind->directory());

      ui->category($old_category);
    }
  
    $self->pind_directories(\@directories);

    #section 3.1-3.8 nonp_bootstrap_v2 for original model
    #Update copy of original model using separate function and push to modelfit
    my $new_original = $self-> setup_original_jd_model();

    #section 4 nonp_bootstrap_v2 for original model only 
    #(actual run command resides elsewhere)
    #section 5 nonp_bootstrap_v2shall not be done for original.
    #fix the options, talk to Pontus
		$self->tools([]) unless defined $self->tools;
    push( @{$self->tools}, tool::modelfit ->
				new( %{common_options::restore_options(@common_options::tool_options)},
	       directory => undef,
	       top_tool => 0,
	       models		   => [$new_original],
	       ) );
  }
  
}
# line 402 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> modelfit_setup');
	# End of Non-Dia code #

}

sub _modelfit_raw_results_callback {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->_modelfit_raw_results_callback: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->_modelfit_raw_results_callback: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->_modelfit_raw_results_callback: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->_modelfit_raw_results_callback: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _modelfit_raw_results_callback');
# line 302 "lib/tool/nonpb_subs.pm" 
#{
#}
# line 443 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _modelfit_raw_results_callback');
	# End of Non-Dia code #

	return \&subroutine;
}

sub modelfit_analyze {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> modelfit_analyze');
# line 182 "lib/tool/nonpb_subs.pm" 
{

  my $index_matrix = $self->get_bootstrap_index_matrix(); #version dependent inside

  #3D array, array of $self->etas transposed n_individuals rows by samples columns 2D matrix
  #this is array of x matrices row1_results_ETAX
  my @eta_cbjd_xsn_matrix; #first dim is x (etas) second is s (samples) third is n (individuals)

  for (my $id=1; $id <= $self->samples; $id++){
    my $filename;
    if ($self->nonpb_version == 2) {
      #section 6.1 and 6.2 nonp_bootstrap_v2
      $filename = 'intermediate_files/P_values_'.$id.'.csv'; 
    } else {
      #sec 2b nonp_bootstrap_v1
      $filename = $self->pind_directories->[0].'P_values.csv'; 
    }

    my ($P_values_bootstrapped, $P_values_rowsums) = 
	$self->get_P_values_bootstrapped_and_rowsums(id=>$id, 
						     filename => $filename,
						     index_vector => $index_matrix->[$id-1]);

    if ($self->nonpb_version == 2) {
      #section 6.3 nonp_bootstrap_v2
      $filename = "intermediate_files/bs_model_$id.patab";
    } else {
      #section 3 URS nonp_bootstrap_v1
      $filename = $self->pind_directories->[0].'m1/original.patab'; 
    }
    my $bootstrapped_np_probabilities_T = 
	$self->create_bootstrapped_np_probabilities_T(id => $id,
						      P_values_rowsums => $P_values_rowsums,
						      filename => $filename,
						      index_vector => $index_matrix->[$id-1]);
    
    #section 4 of nonp_bootstrap_v1 is same as sec 7-9 of nonp_bootstrap_v2

    #section 7 nonp_bootstrap_v2
    my ($sorted_eta_T,$cbjd_T,$id_T) = 
	$self->create_cbjd(id => $id,
			   bootstrapped_np_probabilities_T => $bootstrapped_np_probabilities_T);


    #to use for sec 8 nonp_bootstrap_v2
    for (my $x=0; $x < $self->etas; $x++){
      push(@{$eta_cbjd_xsn_matrix[$x]},$sorted_eta_T->[$x]);
      push(@{$eta_cbjd_xsn_matrix[$x]},$cbjd_T->[$x]);
    }
  }

  #sec 8 nonp_bootstrap_v2 write 'etas' files row1_results_ETAX.csv
  for (my $x=0; $x < $self->etas; $x++){
    my $fname='intermediate_files/row1_results_ETA'.($x+1).'.csv';
    open (FILE,'>',$fname) || croak("Could not open $fname for writing: $!");
    for (my $n=0; $n < $self->n_individuals; $n++){
      my @row;
      for (my $s=0; $s<$self->samples; $s++){
	push(@row,$eta_cbjd_xsn_matrix[$x]->[2*$s]->[$n]); #ETA
	push(@row,$eta_cbjd_xsn_matrix[$x]->[2*$s+1]->[$n]); #CBJD
      }
      print( FILE join( ',', @row ), "\n" );
    }
    close (FILE);
  }

  #sec 8 nonp_bootstrap_v2, adjusted files
  #and sec 9.2 nonp_bootstrap_v2, rearrangement of same file
  #and sec 9.3 and 9.4 nonp_bootstrap_v2, statistics of same file and output of stats

  #get etas, sorted ascending, from original.patab, etas rows, n_individuals columns
  my $original_etas_xn_matrix = $self->get_sorted_original_eta_matrix();

  my $header = "ETA,mean,median,95th CI upper,95th CI lower,90th CI upper,90th CI lower,";
  $header .= "50th CI upper,50th CI lower,25th CI upper,25th CI lower\n";

  for (my $x=0; $x < $self->etas; $x++){
    my $fname1='intermediate_files/bootstrap_CBJD_ETA'.($x+1).'_adjusted.csv';
    open (FILE1,'>',$fname1) || croak("Could not open $fname1 for writing: $!");
    my $fname2='result_files/row2_results_ETA'.($x+1).'.csv';
    open (FILE2,'>',$fname2) || croak("Could not open $fname2 for writing: $!");
    my $fname3='result_files/CI_results_ETA'.($x+1).'.csv';
    open (FILE3,'>',$fname3) || croak("Could not open $fname3 for writing: $!");
    print( FILE3 $header );
    for (my $n=0; $n< $self->n_individuals; $n++){
      my @row1; #for sec 8
      my @row2; #for sec 9.2
      my $new_eta = $original_etas_xn_matrix->[$x]->[$n];
      for (my $s=0; $s< $self->samples; $s++){
	my $index= $self->get_index_first_lower(new_eta=> $new_eta,
						old_etavec => $eta_cbjd_xsn_matrix[$x]->[2*$s]);
	push(@row1,($new_eta,$eta_cbjd_xsn_matrix[$x]->[2*$s+1]->[$index])); #for sec 8
	push(@row2,$eta_cbjd_xsn_matrix[$x]->[2*$s+1]->[$index]); #for sec 9.2

      }
      #for sec 9.3, 9.4
      my $stats = $self->get_statistics_array(array =>\@row2);
      my @row3 = ($new_eta);
      push (@row3,@{$stats});
      print( FILE3 join( ',', @row3 ), "\n" );

      print( FILE1 join( ',', @row1 ), "\n" );

      unshift(@row2,$new_eta); #for sec 9.2 nonp_bootstrap_v2
      print( FILE2 join( ',', @row2 ), "\n" );
    }
    close (FILE1);
    close (FILE2);
    close (FILE3);
  }


}
# line 595 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> modelfit_analyze');
	# End of Non-Dia code #

}

sub modelfit_analyze_v1 {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v1: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v1: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v1: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v1: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v1: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub modelfit_analyze_v2 {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v2: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v2: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v2: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v2: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->modelfit_analyze_v2: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub sum_vector_entries {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'vector' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->sum_vector_entries: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->sum_vector_entries: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->sum_vector_entries: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->sum_vector_entries: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->sum_vector_entries: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $vector = $parm{'vector'};
	my $sum_entries;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> sum_vector_entries');
# line 644 "lib/tool/nonpb_subs.pm" 
{
  #input $vector is reference to single-dimension array 
  #output is scalar $sum_entries
  #sort in ascending order and add from smallest to avoid numerical issues

  unless (scalar(@{$vector}) > 0){
    croak("Error sum_vector_entries: Empty vector.");
  }

  my @sorted_values = sort {$a <=> $b} @{$vector};
  $sum_entries=0;
  foreach my $entr (@sorted_values){
    $sum_entries+=$entr;
  }
}
# line 715 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> sum_vector_entries');
	# End of Non-Dia code #

	return $sum_entries;
}

sub ceil {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->ceil: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->ceil: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->ceil: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->ceil: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->ceil: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $number = $parm{'number'};
	my $integer_out;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> ceil');
# line 771 "lib/tool/nonpb_subs.pm" 
{
  my $floor=int($number);
  my $rem=$number-$floor;
  if ($rem > 0){
    $integer_out = $floor+1;
  } else {
    #equal or  neg
    $integer_out = $floor;
  } 

}
# line 766 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> ceil');
	# End of Non-Dia code #

	return $integer_out;
}

sub get_statistics_array {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'array' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->get_statistics_array: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->get_statistics_array: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->get_statistics_array: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->get_statistics_array: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->get_statistics_array: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $array = $parm{'array'};
	my @statistics;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_statistics_array');
# line 788 "lib/tool/nonpb_subs.pm" 
{
  #input array (unsorted)
  #output array statistics with mean,median,95th upper,lower,90th upper,lower,
  #50th upper,lower,25th upper,lower

  my $nval = scalar(@{$array}) ;
  if ($nval == 0){
    croak("Error: Cannot compute statistics of empty array.");
  }

  #mean
  my $sum=0;
  foreach my $val (@{$array}){
    $sum += $val;
  }
  push (@statistics,($sum/$nval));

  my @sorted_array = sort {$a <=> $b} @{$array};

  #median
  if( $nval  % 2 ){
    push (@statistics,($sorted_array[($nval-1)/2]));
  } else {
    push (@statistics,(($sorted_array[$nval/2]+$sorted_array[($nval-2)/2])/ 2));
  }

  foreach my $ci (95,90,50,25){
    my $lower_index = $self->ceil(number => ($nval*(100-$ci)/200))-1;
    my $upper_index = $nval - ($lower_index+1);
    push (@statistics,($sorted_array[$upper_index]));
    push (@statistics,($sorted_array[$lower_index]));
  }

}
# line 840 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_statistics_array');
	# End of Non-Dia code #

	return \@statistics;
}

sub setup_original_jd_model {
	my $self = shift;
	my $new_model;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> setup_original_jd_model');
# line 666 "lib/tool/nonpb_subs.pm" 
{
  #this is section 3.1 to 3.8 of URS nonp_bootstrap_v2 for
  #original model only
  #no input, return is new model, modifed original
  
  $new_model = $self->models->[0]->copy(filename=>'original.mod');
  
  #3.1 update initial values in model and FIX them. Important to fix *after* updating.
  my $outputObject= output -> new(filename => '../'.$self->lst_file);
  unless ($outputObject->parsed_successfully()){
    croak("lst file " . $self->lst_file . " could not be parsed.");
  }
  $new_model -> update_inits ( from_output => $outputObject);

  $new_model -> fixed( parameter_type => 'theta',
		       new_values => [[(1) x $new_model -> nthetas ]] );
  $new_model -> fixed( parameter_type => 'omega',
		       new_values => [[(1) x $new_model -> nomegas -> [0] ]] );
  $new_model -> fixed( parameter_type => 'sigma',
		       new_values => [[(1) x $new_model -> nsigmas -> [0] ]] );
  

  #3.2 set MAXEVALS=0
  my $record_ref = $new_model -> record(record_name => 'estimation' );
  if ( scalar(@{$record_ref}) > 0 ){ 
    $new_model -> set_option(record_name => 'estimation',
			     option_name => 'MAXEVALS',
			     fuzzy_match => 1,
			  option_value => '0');
  } else {
    $new_model -> add_records(type => 'estimation',
			      record_strings => ['MAXEVAL=0'] );
  }
  
  #3.3 add $NONPARAMETIC UNCONDITIONAL
  my $record_ref = $new_model -> record(record_name => 'nonparametric' );
  if ( scalar(@{$record_ref}) > 0 ){ 
    $new_model -> set_option(record_name => 'nonparametric',
			     option_name => 'UNCONDITIONAL',
			     fuzzy_match => 1);
  } else {
    $new_model -> add_records(type => 'nonparametric',
			      record_strings => ['UNCONDITIONAL'] );
  }
  
  #3.4 Add JD code
  
  my $code_block;
  my $use_pk = 0;
  
  if( defined $new_model -> pk ){
    $code_block = $new_model -> pk;
    $use_pk = 1;
  } elsif( defined $new_model -> pred ){
    $code_block = $new_model -> pred;
  } else {
    croak("Error: Neither \$PK nor \$PRED found in modelfile.");
  }
  
  push( @{$code_block},'   JD = DEN_');
  
  for( 1..$new_model -> nomegas -> [0] ){
    push(@{$code_block},"   DN$_=CDEN_($_)" );
  }

  if( $use_pk ){
    $new_model -> pk( new_pk => $code_block );
  } else {
    $new_model -> pred( new_pred => $code_block );
  }

  #3.7
  $new_model -> remove_records( type => 'table' );
  
  #3.5
  my $table_string = 'ETA1';
  my $dn_string = 'DN1';
  my $filestring = 'FILE=original.patab';
  
  for( 2..$self->etas ){
    $table_string = $table_string . " ETA$_";
    $dn_string = $dn_string . " DN$_";
  }
  $table_string = $table_string . " " . $dn_string;
  $new_model -> add_records( type => 'table',
			     record_strings=>["ID JD $table_string NOPRINT ONEHEADER FIRSTONLY $filestring"]);
  
  #3.6
  $new_model -> remove_records( type => 'covariance' );
    
  #3.8 Use original dataset by changing $DATA
  #is this it????
  $new_model -> datafiles( new_names =>$self->models->[0]->datafiles(absolute_path => 1) );
  
  #finish
  $new_model -> extra_output( ["$filestring"] ); #why needed??
  $new_model -> _write; #why??
  
}
# line 953 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> setup_original_jd_model');
	# End of Non-Dia code #

	return $new_model;
}

sub get_bootstrap_index_matrix {
	my $self = shift;
	my @index_matrix;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_bootstrap_index_matrix');
# line 377 "lib/tool/nonpb_subs.pm" 
{
  #no input
  #output is index_matrix with scalar indices, start 0, to the individuals included in the sample
  #number of rows should be $self->samples, length of rows should be n_individuals

  #One file for all, indices start with 0

  if ($self->nonpb_version == 2) {

    my $file= $self->bs_directory . "../included_keys1.csv";
    unless (-e $file){
      croak("Error get_bootstrap_index_matrix: File \n$file \ndoes not exist.");
    }
    
    open( KEYS, "<", $file  ) || croak("Could not open file \n$file");
    while (my $row = <KEYS>){
      chomp $row;
      my @values = split(/,/,$row);
      my $count = scalar(@values); 
      unless ($count == $self->n_individuals){
	croak("Error get_bootstrap_index_matrix: ".
		     "number of indices in row $count not equal to number of".
		     " individuals ".$self->n_individuals);
      }
      push (@index_matrix,\@values);
    }
    close(KEYS);
    
    unless (scalar(@index_matrix) == $self->samples){
      croak("Error get_bootstrap_index_matrix: \n".
		  "Number of rows in matrix not equal to number of samples.");
    }
    
  } else {
    #create bootstrap indices for section 3
    #changed filename from samplekey.csv in instructions to included_keys.csv

    my $maxindex = $self->n_individuals-1;
    my $fname='intermediate_files/included_keys.csv';
    open (FILE,'>',$fname) || croak("Could not open $fname for writing: $!");
    
    for (my $sample=0; $sample< $self->samples; $sample++){
      for (my $i=0; $i<$self->n_individuals; $i++){
	my $individual = random_uniform_integer(1,0,$maxindex);
	push(@{$index_matrix[$sample]},$individual);
      }
      print( FILE join( ',', @{$index_matrix[$sample]} ), "\n" );
    }
    close (FILE);
  }
}
# line 1018 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_bootstrap_index_matrix');
	# End of Non-Dia code #

	return \@index_matrix;
}

sub get_sorted_original_eta_matrix {
	my $self = shift;
	my @eta_matrix_T;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_sorted_original_eta_matrix');
# line 339 "lib/tool/nonpb_subs.pm" 
{
  #this is for section 8 URS nonp_bootstrap_v2
  #this is for section 4 URS nonp_bootstrap_v1
  #no input
  #output is eta_matrix_T
  #number of rows should be $self->etas, length of rows should be n_individuals

  my $filename;
  if ($self->nonpb_version == 2) {
    $filename= $self->directory . 'original.patab';
  } else {
    #version 1
    $filename = $self->pind_directories->[0].'m1/original.patab'; 
  }

  my $table = data -> new(filename=>$filename);

  for (my $i = 1; $i <= $self->etas; $i++) {
    my $col="ETA$i";
    my $etavec = $table -> column_to_array('column'=>$col);
    unless (scalar (@{$etavec}) == $self->n_individuals){
      croak("Error get_sorted_original_eta_matrix: ".
		    "wrong number of $col values found in tablefile.");
    }

    my @sorted_etavec = sort {$a <=> $b} @{$etavec};
    push(@eta_matrix_T,\@sorted_etavec);
  }

}
# line 1062 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_sorted_original_eta_matrix');
	# End of Non-Dia code #

	return \@eta_matrix_T;
}

sub get_index_first_lower {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_eta' => 'SCALAR', 'old_etavec' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->get_index_first_lower: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->get_index_first_lower: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->get_index_first_lower: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->get_index_first_lower: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->get_index_first_lower: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $new_eta = $parm{'new_eta'};
	my $old_etavec = $parm{'old_etavec'};
	my $index;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_index_first_lower');
# line 311 "lib/tool/nonpb_subs.pm" 
{
  #used in sec 8
  #input scalar new_eta
  #input vector old_etavec
  #return scalar index

  unless (scalar(@{$old_etavec})== $self->n_individuals){
    croak("Error get_index_first_lower: Input vector wrong length");
  }

  $index = 0; #in case no value lower

  #search backwards since sorted ascending
  for (my $i=$self->n_individuals-1; $i>=0; $i--){
    if ($old_etavec->[$i] < $new_eta){
      $index = $i;
      last;
    }
  }
}
# line 1123 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_index_first_lower');
	# End of Non-Dia code #

	return $index;
}

sub get_id_eta_botp_matrix {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'table_file' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->get_id_eta_botp_matrix: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->get_id_eta_botp_matrix: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->get_id_eta_botp_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->get_id_eta_botp_matrix: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->get_id_eta_botp_matrix: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $table_file = $parm{'table_file'};
	my @id_eta_botp_matrix;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@id_eta_botp_matrix;
}

sub get_P_values_bootstrapped_and_rowsums {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'filename' => 'm_SCALAR', 'id' => 'm_SCALAR',
			'index_vector' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->get_P_values_bootstrapped_and_rowsums: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->get_P_values_bootstrapped_and_rowsums: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->get_P_values_bootstrapped_and_rowsums: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->get_P_values_bootstrapped_and_rowsums: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->get_P_values_bootstrapped_and_rowsums: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $filename = $parm{'filename'};
	my $id = $parm{'id'};
	my $index_vector = $parm{'index_vector'};
	my @P_values_bootstrapped;
	my @P_values_rowsums;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_P_values_bootstrapped_and_rowsums');
# line 435 "lib/tool/nonpb_subs.pm" 
{
  #section 6.1 and 6.2 URS nonp_bootstrap_v2
  #section 3 (3.0, 3.1, 3.2) URS nonp_bootstrap_v1
  #input is scalar $id for sample index (start with 1)
  #input filename, including path, of relevant P_values.csv
  #input index_vector , ref of vector with indicides of sample $id
  #return P_values_bootstrapped and P_values_rowsums

  open (PVAL, '<', $filename) || 
      croak("Couldn't open $filename for reading: $!");

  my @pval;
  while (<PVAL>) {
    chomp;
    my @values = split(',',$_);
    push (@pval,\@values);
  }
  close PVAL;

  unless ( scalar(@{$index_vector}) == $self->n_individuals){
    croak("Error get_P_values_bootstrapped_and_rowsums: ".
		 "wrong number of elements in input index_vector");
  }

  #initiate P_values_bootstrapped by pushing $n_individuals empty rows
  foreach my $ind (@{$index_vector}){
    push (@P_values_bootstrapped,[]);
  }

  #indices start with 0 after processing
  foreach my $ind_col (@{$index_vector}){
    for (my $row=0; $row < $self->n_individuals; $row++){
      push (@{$P_values_bootstrapped[$row]},$pval[$row]->[$ind_col]);
    }
  }

  my $fname="intermediate_files/P_values_bootstrapped_$id.csv";
  open (BVAL,'>',$fname) || croak("Could not open $fname for writing: $!");
  foreach my $row (@P_values_bootstrapped){
    print( BVAL join( ',', @{$row} ), "\n" );
  }
  close (BVAL);

  foreach my $row (@P_values_bootstrapped){
    push(@P_values_rowsums,$self->sum_vector_entries(vector=>$row));
  }
  
}
# line 1251 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_P_values_bootstrapped_and_rowsums');
	# End of Non-Dia code #

	return \@P_values_bootstrapped ,\@P_values_rowsums;
}

sub create_bootstrapped_np_probabilities_T {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'id' => 'm_SCALAR', 'filename' => 'SCALAR',
			'P_values_rowsums' => 'REF', 'index_vector' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->create_bootstrapped_np_probabilities_T: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->create_bootstrapped_np_probabilities_T: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->create_bootstrapped_np_probabilities_T: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->create_bootstrapped_np_probabilities_T: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->create_bootstrapped_np_probabilities_T: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $id = $parm{'id'};
	my $filename = $parm{'filename'};
	my $P_values_rowsums = $parm{'P_values_rowsums'};
	my $index_vector = $parm{'index_vector'};
	my @bootstrapped_np_probabilities_T;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> create_bootstrapped_np_probabilities_T');
# line 490 "lib/tool/nonpb_subs.pm" 
{
  #section 6.3 of URS nonp_bootstrap_v2
  #section 3.2 of URS nonp_bootstrap_v1
  #input is scalar $id for sample
  #input is vector P_values_rowsums
  #input is index_vector, this is not used anymore
  #input is filename including path for relevant patab
  #return matrix bootstrapped_np_probabilities_T T denotes transpose 

  my $number_of_individuals = scalar(@{$P_values_rowsums});
  unless ($number_of_individuals > 0){
    croak("Error get_bootstrapped_np_probabilities: ".
		  "empty input vector P_values_rowsums.");
  }
  unless ($number_of_individuals == $self->n_individuals){
    croak("Error get_bootstrapped_np_probabilities: ".
		  "wrong number of values in P_values_rowsums.");
  }

  unless ( -e $filename ){
    croak("Error get_bootstrapped_np_probabilities: ".
		   "Cannot find file $filename");
  }
  my $table = data -> new(filename=>$filename); #absolute path ok?

  my @id_vector = @{$table -> column_to_array('column'=>'ID')};


  unless (scalar (@id_vector) == $number_of_individuals){
    croak("Error get_bootstrapped_np_probabilities_cbjd: ".
		  "wrong number of id values found.");
  }

  $bootstrapped_np_probabilities_T[0] = \@id_vector; #return matrix

  my @bootstrapped_np_probabilities; #temporary matrix for writing to file
  foreach my $val (@id_vector){
    push (@bootstrapped_np_probabilities,[$val]);
  }

  #put ETAs in matrix
  for (my $i=1; $i<=$self->etas;$i++){
    my $col="ETA$i";
    my $etavec = $table -> column_to_array('column'=>$col);
    unless (scalar (@{$etavec}) == $number_of_individuals){
      croak("Error get_bootstrapped_np_probabilities: ".
		    "wrong number of $col values found in tablefile.");
    }

    for (my $j=0; $j<$number_of_individuals;$j++){
      push (@{$bootstrapped_np_probabilities[$j]},$etavec->[$j]);
    }
    $bootstrapped_np_probabilities_T[$i] = $etavec; #return matrix
  }

  for (my $j=0; $j<$number_of_individuals;$j++){
    push (@{$bootstrapped_np_probabilities[$j]},$P_values_rowsums->[$j]);
  }
  $bootstrapped_np_probabilities_T[($self->etas + 1)] = $P_values_rowsums; #return matrix

  my $fname="intermediate_files/bootstrapped_np_probabilities_$id.csv";
  open (BVAL,'>',$fname) || croak("Could not open $fname for writing: $!");
  foreach my $row (@bootstrapped_np_probabilities){
    print( BVAL join( ',', @{$row} ), "\n" );
  }
  close (BVAL);

}
# line 1363 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> create_bootstrapped_np_probabilities_T');
	# End of Non-Dia code #

	return \@bootstrapped_np_probabilities_T;
}

sub create_cbjd {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'id' => 'm_SCALAR', 'bootstrapped_np_probabilities_T' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::nonpb->create_cbjd: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::nonpb->create_cbjd: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::nonpb->create_cbjd: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->create_cbjd: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::nonpb->create_cbjd: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $id = $parm{'id'};
	my $bootstrapped_np_probabilities_T = $parm{'bootstrapped_np_probabilities_T'};
	my @sorted_eta_matrix_T;
	my @cbjd_matrix_T;
	my @id_matrix_T;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> create_cbjd');
# line 566 "lib/tool/nonpb_subs.pm" 
{
  #section 7 of URS nonp_bootstrap_v2
  #input is scalar $id for sample
  #input is matrix bootstrapped_np_probabilities_T T denotes transpose 
  #return sorted_eta_matrix_T 
  #return cbjd_matrix_T
  #return id_matrix_T

  my $id_vector = $bootstrapped_np_probabilities_T->[0];
  my $number_of_individuals = scalar(@{$id_vector});
  unless ($number_of_individuals > 0){
    croak("Error create_cbjd: ".
		  "empty input matrix bootstrapped_np_probabilities.");
  }
  unless ($number_of_individuals == $self->n_individuals){
    croak("Error create_cbjd: ".
		  "wrong number of values in bootstrapped_np_probabilities.");
  }

  my $P_values_rowsums = $bootstrapped_np_probabilities_T->[($self->etas+1)];
  unless (scalar (@{$P_values_rowsums}) == $number_of_individuals){
    croak("Error create_cbjd: ".
		  "wrong number of rowsum values found in input matrix.");
  }

  open (LOG,'>>',$self->logfile->[0]);
  #compute cumulative probabilities for section 7
  for (my $i=1; $i<=$self->etas;$i++){
    my $col="ETA$i";
    my $etavec = $bootstrapped_np_probabilities_T->[$i];
    unless (scalar (@{$etavec}) == $number_of_individuals){
      croak("Error create_cbjd: ".
		    "wrong number of $col values found in input matrix.");
    }

    #compute cumulative probabilities sec 7, write to file 
    my %index;
    for (my $j=0; $j<$number_of_individuals;$j++){
      $index{$j} = $etavec -> [$j];
    }
    my @order = sort( {$index{$a} <=> $index{$b}} keys %index );

    my $fname="intermediate_files/bootstrap_CBJD_$col"."_$id.csv";
    open (BVAL,'>',$fname) || croak("Could not open $fname for writing: $!");

    my (@id_col,@eta_col,@cbjd_col);
    my $cumsum=0;
    foreach my $idx ( @order ){
      $cumsum += $P_values_rowsums->[$idx];
      push(@id_col,$id_vector->[$idx]);
      push(@eta_col,$index{$idx});
      push(@cbjd_col,$cumsum);
      print( BVAL $id_vector->[$idx].','.$index{$idx}.",$cumsum\n" );
    }
    push (@sorted_eta_matrix_T,\@eta_col);
    push (@cbjd_matrix_T,\@cbjd_col);
    push (@id_matrix_T,\@id_col);

    close (BVAL);
    #verification
    if (abs($cumsum-1)>0.01){
      print (LOG "Verification CBJD failed $col sample $id, cumulative probability is $cumsum\n");
    }
    
  }
  close (LOG);

}
# line 1474 libgen/tool/nonpb.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> create_cbjd');
	# End of Non-Dia code #

	return \@sorted_eta_matrix_T ,\@cbjd_matrix_T ,\@id_matrix_T;
}

1;

