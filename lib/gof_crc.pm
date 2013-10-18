package gof_crc;

use strict;

sub test {
  return sub {
    my ( $calling_scm, $direction, $basic_model, $model_number, $ofv_ch_ref ) = @_;
    my @ofv_changes = @{$ofv_ch_ref};

    my %ofv_test     = ( 'forward'  => 0.05,
			 'backward' => 0.01 );
    my %iiv_test     = ( 'IVCL' => { 'forward'  => -0.05,
				     'backward' => 0.1 },
			 #		     'IVKA' => { 'forward'  => -0.15,
			 #				 'backward' => 0.3 },
			 'IVV'  => { 'forward'  => -0.10,
				     'backward' => 0.2 } );
    my %extreme_test = ( 'TVCL' => { 'forward'  => 0.1,
				     'backward' => -0.2 },
			 #		       'TVKA' => { 'forward'  => 0.20,
			 #		       'backward' => -0.4 },
			 'TVV'  => { 'forward'  => 0.15,
				     'backward' => -0.3 }, );

    my @step_relations   = @{$calling_scm -> step_relations};
    my @competing_models = @{$calling_scm -> prepared_models -> [$model_number-1]{'own'}};


    my ( $junk, $junk, $junk, $os_ref, $ofv_ref, @junk ) =
	$calling_scm -> gof_ofv( $direction,
				 $basic_model,
				 $model_number,
				 $ofv_ch_ref );
#     my ( $os_ref, $ofv_ref ) = &sign_ofv_drops( calling_scm  => $calling_scm,
# 						model_number => $model_number,
# 						direction    => $direction,
# 						ofv_test     => \%ofv_test );
    my ( $is_ref, $id_ref ) = &sign_iiv_changes( calling_scm  => $calling_scm,
						 model_number => $model_number,
						 direction    => $direction,
						 iiv_test     => \%iiv_test );
    my ( $es_ref, $ed_ref ) = &sign_extreme_changes( calling_scm  => $calling_scm,
						     model_number => $model_number,
						     direction    => $direction,
						     extreme_test => \%extreme_test );

    my %ofv_sign     = %{$os_ref};
    my @ofv_drop     = @{$ofv_ref};
    my %iiv_sign     = %{$is_ref};
    my @iiv_drop     = @{$id_ref};
    my %extreme_sign = %{$es_ref};
    my @extreme_drop = @{$ed_ref};


    my $chosen_ofv;
    my $resulting_model;
    my ( $chosen_parameter, $chosen_covariate );
    my %chosen_iiv_sign;
    my %chosen_stats_sign;
    my %chosen_iiv;
    my %chosen_extreme;
#    my %clin_sign = ( %iiv_sign, %extreme_sign );

    # Båda riktningarna:
    # OFV sign: om droppen är större än den angivna gränsen
    # IIV_sign: om droppen (skillnaden) är större än den angivna gränsen
    # extreme_sign: om droppen (skillnaden) är större än den angivna gränsen

    open( LOG, ">>".$calling_scm->logfile->[$model_number-1] );
    my %sign;
    if ( $direction eq 'forward' ) {
      # Forward:
      # Om droppen är ofv_sign och antingen iiv_sign eller extreme_sign:
      # välj den med störst positivt drop.
      print LOG   "CRITERIA:  IF( ofv_sign AND ( iiv_sign OR extreme_sign ) )\n";
      for ( my $i = 0; $i <= $#competing_models; $i++ ) {
	my $od = defined $ofv_sign{$i} ? 1 : 0;
	my $id = defined $iiv_sign{$i} ? 1 : 0;
	my $ed = defined $extreme_sign{$i} ? 1 : 0;
	print LOG sprintf("%-8s",$step_relations[$i]{'parameter'}.
			    $step_relations[$i]{'covariate'}.'-'.
			    $step_relations[$i]{'state'}),
			    "     (    $od     AND (    $id     OR      $ed       ) ) = ",
	  ($od and ($id or $ed ) ),"\n";
	$sign{$i} = $ofv_drop[$i] if ($od and ($id or $ed ) );
      }
    } else {
      # Backward:
      # Om droppen är ofv_(un)sign eller ( iiv_(un)sign och extreme_(un)sign för alla tester):
      # välj den med minst negativt drop
      print LOG   "CRITERIA:  IF( ofv_sign OR ( iiv_sign AND extreme_sign ) )\n";
      for ( my $i = 0; $i <= $#competing_models; $i++ ) {
	my $od = defined $ofv_sign{$i} ? 1 : 0;
	my ( $id, $ed ) = (1,1);
	while ( my ( $name, $ref ) = each %iiv_test ) {
	  $id = 0 unless ( defined $iiv_sign{$i}{$name} );
	}
	while ( my ( $name, $ref ) = each %extreme_test ) {
	  $ed = 0 unless ( defined $extreme_sign{$i}{$name}{'upper_relative_range'} );
	  $ed = 0 unless ( defined $extreme_sign{$i}{$name}{'lower_relative_range'} );
	}
	print LOG sprintf("%-8s",$step_relations[$i]{'parameter'}.
			    $step_relations[$i]{'covariate'}.'-'.
			    $step_relations[$i]{'state'}),
			    "     (    $od     OR (    $id     AND      $ed       ) ) = ",
	  ($od or ($id and $ed ) ),"\n";
	$sign{$i} = $ofv_drop[$i] if ( $od or ( $id and $ed ) );
      }
    }
    close (LOG);
    if ( scalar keys %sign > 0) {
      my @sorted_ids = sort { $sign{$b} <=> $sign{$a} } keys %sign;
      $resulting_model = $competing_models[$sorted_ids[0]];
      $chosen_ofv = $competing_models[$sorted_ids[0]] -> outputs -> [0] -> ofv -> [0][0];
      $chosen_parameter = $step_relations[$sorted_ids[0]]{'parameter'};
      $chosen_covariate = $step_relations[$sorted_ids[0]]{'covariate'};
      %chosen_extreme = %{$extreme_drop[$sorted_ids[0]]} if ( defined $extreme_drop[$sorted_ids[0]] );
      %chosen_iiv = %{$iiv_drop[$sorted_ids[0]]} if ( defined $iiv_drop[$sorted_ids[0]] );
    }

    my $base_ofv;
    if ( defined $calling_scm -> base_criteria_vals and
	 defined $calling_scm -> base_criteria_vals ->
	 [$model_number-1] -> {'ofv'} ) {
      $base_ofv = $calling_scm -> base_criteria_vals ->
	  [$model_number-1] -> {'ofv'};
    } else {
      $base_ofv = $calling_scm -> models -> [$model_number-1] -> outputs -> [0] -> ofv -> [0][0];
    }
#    print "BOFV1: ",$base_ofv,"\n";
#    print "BOFV2: ",$basic_model -> outputs -> [0] -> ofv -> [0][0],"\n";
    return ( $resulting_model,
	     $chosen_parameter,
	     $chosen_covariate,
	     \%sign,
	     [\@ofv_drop, \@iiv_drop, \@extreme_drop],
	     'CRC',
	     { 'BASE_MODEL_OFV'   => $base_ofv,
	       'CHOSEN_MODEL_OFV' => $chosen_ofv,
	       %chosen_iiv,
	       %chosen_extreme },
	     {'ofv'       => $chosen_ofv,
	      'estimates' => \%chosen_iiv,
	      'stats'     => \%chosen_extreme } );
  }
};

sub sign_iiv_changes {
  my %parm = @_;
  my $calling_scm  = $parm{'calling_scm'};
  my $model_number = $parm{'model_number'};
  my $direction    = $parm{'direction'};
  my %iiv_test     = %{$parm{'iiv_test'}};

  my %iiv_sign; # Flag 0|1 for (un)significance

  my %base_ests;
  if ( defined $calling_scm -> base_criteria_vals and
       defined $calling_scm -> base_criteria_vals ->
       [$model_number-1] -> {'estimates'} ) {
    %base_ests = %{$calling_scm -> base_criteria_vals ->
		     [$model_number-1] -> {'estimates'}};
  } elsif ( $direction eq 'backward' ) {
    die "GOF_CRC: backward search needs a 'base' IIV estimate\n";
  } else {
    if ( defined $calling_scm -> models -> [$model_number-1] ->
	 name_val( parameter_type =>  'theta' )->[0][0] and 
	 defined $calling_scm -> models -> [$model_number-1] ->
	 name_val( parameter_type =>  'omega' )->[0][0] ) {
      %base_ests = ( %{$calling_scm -> models -> [$model_number-1] ->
			 name_val( parameter_type =>  'omega' )->[0][0]} );
    } else {
      die "GOF_CRC: OMEGA parameter estimates not available from model",
	$calling_scm -> models -> [$model_number-1] -> full_name ,"\n";
    }
  }

  my @models = @{$calling_scm -> prepared_models -> [$model_number-1]{'own'}};
  my @ests;
  foreach my $model ( @models ) {
    my %mod_name_val = ( %{$model -> name_val( parameter_type =>  'omega' )->[0][0]} );
    push( @ests, \%mod_name_val );
  }

  my @step_relations   = @{$calling_scm -> step_relations};
  # Clinical relevance of effect on iiv
  open( LOG, ">>".$calling_scm->logfile->[$model_number-1] );
  my $un = $direction eq 'backward' ? '(UN)' : '';
  print LOG sprintf("%-8s",'MODEL'),
    sprintf("%12s",'TEST NAME'),
      sprintf("%12s",'BASE VAL'),
	sprintf("%12s",'NEW VAL'),
	  sprintf("%50s",'TEST VAL (RELATIVE CHANGE OF SQUARED VALUES)'),
	    sprintf("%10s","GOAL"),
	      sprintf("%14s"," $un".'SIGNIFICANT'),"\n";
  my $direction = $calling_scm -> search_direction;
  for ( my $i = 0; $i <= $#models; $i++ ) {
    my $iiv_significant = 0;
    while ( my ( $name, $change_ref ) = each %iiv_test ) {
      die "No such variable name $name defined in base inter-individual variability\n"
	unless defined $base_ests{$name};
      my $change = $change_ref -> {$direction};
      my $test_val = (sqrt($ests[$i]{$name})-sqrt($base_ests{$name}))
	/sqrt($base_ests{$name});
      print LOG sprintf("%-8s",$step_relations[$i]{'parameter'}.
			    $step_relations[$i]{'covariate'}.'-'.
			    $step_relations[$i]{'state'}),
	sprintf("%12s",$name.'  '),
	  sprintf("%12.5f",$base_ests{$name}),
	    sprintf("%12.5f",$ests[$i]{$name}),
	      sprintf("%47.5f",$test_val),'  <',
		sprintf("%10.5f",$change);
      if ( $test_val < $change ) {
	$iiv_significant++;
	print LOG sprintf("%12s",'YES!  ');
	$iiv_sign{$i}{$name} = 1;
      }
      print LOG "\n";
    }
  }
  print LOG "\n";
  close(LOG);

  return ( \%iiv_sign, \@ests );
}

sub sign_extreme_changes {
  my %parm = @_;
  my $calling_scm = $parm{'calling_scm'};
  my $model_number = $parm{'model_number'};
  my $direction = $parm{'direction'};
  my %extreme_test = %{$parm{'extreme_test'}};

  my %extreme_sign; # Flag 0|1 for (un)significance

  my @stats;
  my %base_stats;

  if ( defined $calling_scm -> base_criteria_vals and
       defined $calling_scm -> base_criteria_vals ->
       [$model_number-1] -> {'stats'} ) {
    %base_stats = %{$calling_scm -> base_criteria_vals ->
		      [$model_number-1] -> {'stats'}};
  } else {
    die "GOF_CRC: No table files defined in ",
      $calling_scm -> models -> [$model_number-1] -> full_name,"\n" unless
	( defined $calling_scm -> models -> [$model_number-1] -> table_files and
	  scalar @{$calling_scm -> models -> [$model_number-1] -> table_files} > 0 and
	  scalar @{$calling_scm -> models -> [$model_number-1] -> table_files -> [0]} > 0 );
    $calling_scm -> models -> [$model_number-1] -> table_files ->[0][0] -> target( 'mem' );
    my @stat_names = ( 'max', 'min', 'median' );
    while ( my ( $name, $change ) = each %extreme_test ) {
      foreach my $stat_name ( @stat_names ) {
	$base_stats{$name}{$stat_name} = $calling_scm -> models -> [$model_number-1] ->
	  table_files ->[0][0] -> $stat_name( column_head => $name );
      }
      $base_stats{$name}{'upper_relative_range'} =
	($base_stats{$name}{'max'}-$base_stats{$name}{'median'})/
	  $base_stats{$name}{'median'};
      $base_stats{$name}{'lower_relative_range'} =
	($base_stats{$name}{'median'}-$base_stats{$name}{'min'})/
	  $base_stats{$name}{'median'};
    }
    $calling_scm -> models -> [$model_number-1] -> table_files ->[0][0] -> target( 'disk' );
  }

  my @models = @{$calling_scm -> prepared_models -> [$model_number-1]{'own'}};
  for ( my $i = 0; $i <= $#models; $i++ ) {
    $models[$i] -> table_files -> [0][0] -> target( 'mem' );
    while ( my ( $name, $change ) = each %extreme_test ) {
      foreach my $stat_name ( 'max', 'min', 'median' ) {
	$stats[$i]{$name}{$stat_name} = $models[$i] -> table_files ->[0][0] ->
	  $stat_name( column_head => $name );
      }
      $stats[$i]{$name}{'upper_relative_range'} = 
	($stats[$i]{$name}{'max'}-$stats[$i]{$name}{'median'})/
	  $stats[$i]{$name}{'median'};
      $stats[$i]{$name}{'lower_relative_range'} = 
	($stats[$i]{$name}{'median'}-$stats[$i]{$name}{'min'})/
	  $stats[$i]{$name}{'median'};
    }
    $models[$i] -> table_files -> [0][0] -> target( 'disk' );
  }

  my @step_relations   = @{$calling_scm -> step_relations};

  # Clinical relevance of the effect on variuos statistics of the estimates
  open( LOG, ">>".$calling_scm->logfile->[$model_number-1] );
  my $un = $direction eq 'backward' ? '(UN)' : '';
  print LOG sprintf("%-8s",'MODEL'),
    sprintf("%12s",'TEST NAME'),
      sprintf("%12s",'BASE VAL'),
	sprintf("%12s",'NEW VAL'),
	  sprintf("%50s",'TEST VAL (ABSOLUTE CHANGE)'),
	    sprintf("%10s","GOAL"),
	      sprintf("%14s"," $un".'SIGNIFICANT'),"\n";
  for ( my $i = 0; $i <= $#models; $i++ ) {
    my $extreme_significant = 0;
    while ( my ( $name, $change_ref ) = each %extreme_test ) {
      my $upper_change = $stats[$i]{$name}{'upper_relative_range'} -
	$base_stats{$name}{'upper_relative_range'};
      my $lower_change = $stats[$i]{$name}{'lower_relative_range'} -
	$base_stats{$name}{'lower_relative_range'};
      my $change = $change_ref -> {$direction};
      print LOG sprintf("%-8s",$step_relations[$i]{'parameter'}.
			    $step_relations[$i]{'covariate'}.'-'.
			    $step_relations[$i]{'state'}),
	sprintf("%12s",$name.'_lower'),
	  sprintf("%12.5f",$base_stats{$name}{'lower_relative_range'}),
	    sprintf("%12.5f",$stats[$i]{$name}{'lower_relative_range'}),
	      sprintf("%47.5f",$lower_change),'  >',
		sprintf("%10.5f",$change);
      if ( $lower_change > $change ) {
	print LOG sprintf("%12s",'YES!  ');
	$extreme_significant++;
	$extreme_sign{$i}{$name}{'lower_relative_range'} = 1;
      }
      print LOG "\n";
      print LOG sprintf("%-8s",$step_relations[$i]{'parameter'}.
			    $step_relations[$i]{'covariate'}.'-'.
			    $step_relations[$i]{'state'}),
	sprintf("%12s",$name.'_upper'),
	  sprintf("%12.5f",$base_stats{$name}{'upper_relative_range'}),
	    sprintf("%12.5f",$stats[$i]{$name}{'upper_relative_range'}),
	      sprintf("%47.5f",$upper_change),'  >',
		sprintf("%10.5f",$change);
      if ( $upper_change > $change ) {
	print LOG sprintf("%12s",'YES!  ');
	$extreme_significant++;
	$extreme_sign{$i}{$name}{'upper_relative_range'} = 1;
      }
      print LOG "\n";
    }
  }
  print LOG "\n";
  close(LOG);
  return ( \%extreme_sign, \@stats );
}

1;
