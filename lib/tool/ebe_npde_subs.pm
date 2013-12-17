# {{{ include statements

start include statements
#use Carp;
	use PsN;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Config;
use linear_algebra;
end include statements

# }}} include statements

# {{{ new

start new

$this->have_CDF(1) if eval('require Statistics::Distributions'); #enough, now loaded

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


if ( scalar (@{$this->models->[0]->problems}) > 2 ){
  croak('Cannot have more than two $PROB in the input model.');
}elsif  (scalar (@{$this->models->[0]->problems}) == 2 ){
  if ((defined $this->models->[0]->problems->[0]->priors()) and 
      scalar(@{$this->models->[0]->problems->[0]->priors()})>0 ){
    my $tnpri=0;
    foreach my $rec (@{$this->models->[0]->problems->[0] -> priors()}){
      unless ((defined $rec) &&( defined $rec -> options )){
	carp("No options for rec \$PRIOR" );
      }
      foreach my $option ( @{$rec -> options} ) {
	if ((defined $option) and 
	    (($option->name eq 'TNPRI') || (index('TNPRI',$option ->name ) == 0))){
	  $tnpri=1;
	}
      }
    }

    $this->have_tnpri(1) if ($tnpri);
  }
  if ($this->have_tnpri()){
    unless( defined $this->models->[0]->extra_files ){
      croak('When using $PRIOR TNPRI you must set option -extra_files to '.
		     'the msf-file, otherwise the msf-file will not be copied to the NONMEM '.
		     'run directory.');
    }

  }else{
    croak('The input model must contain exactly one problem, unless'.
	' first $PROB has $PRIOR TNPRI');
  }
  my $est_record = $this->models->[0]->record( problem_number => (1+$this->have_tnpri()),
						     record_name => 'estimation' );
  unless (defined $est_record and scalar(@{$est_record})>0){
    croak('Input model must have an estimation record');
  }

}

my $meth = $this->models->[0]->get_option_value( record_name  => 'estimation',
						       problem_index => (0+$this->have_tnpri()),
						       option_name  => 'METHOD',
						       option_index => 0);
if (not (defined $meth) or ($meth eq '0') or ($meth =~ /^ZE/)){
  croak('Cannot run ebe_npde if METHOD=0, all ETAs will be 0');
}

end new

# }}}



start modelfit_setup
{ 
  my $model = $self->models->[$model_number-1];
  my ( @seed, $new_datas, $skip_ids, $skip_keys, $skip_values );
  my @orig_and_sim_models;
  my $orig_model;
  my $sim_model;
  my @msfo_stems_original;
  
  $self->probnum(2) if ($self->have_tnpri());
  
  my @table_header;
  my @all_iwres_files;
  my @orig_table_names;
  
  my $newthetanum=$model->nthetas(problem_number => $self->probnum())+1;
  my $sim_record;
  my $simdirname='simulation_dir'; 
  my $shrinkage_value;
  $self->first_callback(1);
  
  $orig_model = $model ->
      copy( filename    => $self->directory . 'm' . $model_number . '/original.mod',
	    target      => 'disk',
	    copy_data   => 1,
	    copy_output => 0);
  
  $orig_model -> drop_dropped unless $orig_model->skip_data_parsing();
  
  
  #create sim record if not present
  $sim_record = $orig_model -> record( problem_number => $self->probnum(),
				       record_name => 'simulation' );
  if( scalar(@{$sim_record}) > 0 ){
    $sim_record = $sim_record->[0];
    foreach my $altopt ('SUBPROBLEMS','SUBPROBS','NSUBPROBLEMS','NSUBPROBS','NSUBS'){
      #NONMEM accepts a heck of a lot of alternatives...
      $orig_model -> remove_option(record_name => 'simulation',
				   option_name => $altopt,
				   fuzzy_match => 1,
				   problem_numbers => [$self->probnum()]);
      
    }
    if ($self->have_nwpri() or $self->have_tnpri()){
      $orig_model -> remove_option(record_name => 'simulation',
				   option_name => 'TRUE',
				   fuzzy_match => 1,
				   problem_numbers => [$self->probnum()]);
      
    }
  }else{
    # set $SIMULATION record
    my @arr=('(000)');
    $sim_record = \@arr;#dummy seed
  }
  $sim_record->[0] .= ' SUBPROB=1';
  
  if ($self->have_nwpri() or $self->have_tnpri()){
    $sim_record->[0] .= ' TRUE=PRIOR';
  }
  $orig_model -> remove_records( type => 'simulation' );
  
  $orig_model -> remove_option( record_name  => 'estimation',
				option_name  => 'MSFO',
				fuzzy_match => 1,
				problem_numbers => [($self->probnum())],
				record_number => 0); #0 means all
  # set $TABLE record
  
  my $oprob = $orig_model -> problems -> [$self->probnum()-1];
  if( defined $oprob -> inputs and defined $oprob -> inputs -> [0] -> options ) {
    foreach my $option ( @{$oprob -> inputs -> [0] -> options} ) {
      push( @table_header, $option -> name ) unless 
	  (($option -> value eq 'DROP' or $option -> value eq 'SKIP'
	    or $option -> name eq 'DROP' or $option -> name eq 'SKIP'));
    }
  } else {
    croak("Trying to construct table for simulation".
		  " but no headers were found in \$model_number-INPUT" );
  }
  #never IWRES in orig model, only in sims
  $oprob -> add_records( type           => 'table',
			 record_strings => [ join( ' ', @table_header ).
					     ' IPRED PRED NOPRINT NOAPPEND ONEHEADER FILE=orig_pred.dta']);
  $oprob -> add_records( type           => 'table',
			 record_strings => ['IWRES ID MDV NOPRINT NOAPPEND ONEHEADER FILE=original_iwres.dta']);


  $oprob -> add_records( type           => 'table',
			 record_strings => ['ID NOPRINT NOAPPEND ONEHEADER FIRSTONLY FILE=original.ids']);
  my $etaheader= '';

  my @use_etas=();
  my @these=();
  my @prev=();
  foreach my $record (@{$oprob->omegas}){
    @these=();
    my $all_fix=0;
    last if ($record->prior());
    if  ($record->same() ){
      push(@these,@prev);
    }else{
      if  ($record->fix()){
	$all_fix = 1; 
      }
      foreach my $option (@{$record -> options()}) {
	if ($option->on_diagonal()){
	  if (($option->fix() or $all_fix )and ($option->init() == 0)){
	    push(@these,0);
	  }else{
	    push(@these,1);
	  }
	}
      }
    }
    push(@use_etas,@these);
    @prev = @these;
  }
  
  for (my $i=1; $i<=scalar(@use_etas); $i++){
    $etaheader .= 'ETA'.$i.' ' if ($use_etas[$i-1] == 1);
  } 

  $oprob -> add_records( type           => 'table',
			 record_strings => [$etaheader.'NOPRINT NOAPPEND ONEHEADER FIRSTONLY FILE=original.etas']);
      
  my $orig_model_output;
  if (defined  $self->lst_file()){
    $orig_model_output= output -> new(filename => '../'.$self->lst_file);
    unless ($orig_model_output->parsed_successfully()){
      croak("lst file " . $self->lst_file . " could not be parsed.");
    }
    $orig_model -> update_inits ( from_output => $orig_model_output,
				  problem_number => $self->probnum());
    $orig_model -> _write( write_data => 1 );
    push( @orig_and_sim_models, $orig_model );
    $simdirname='orig_and_simulation_dir'; 
  }elsif (defined $model ->outputs() and 
      defined $model->outputs()->[0] and
      $model->outputs()->[0]-> have_output()){
        #we do not need to run original before sims, because already have final ests
    $orig_model_output = $model->outputs()->[0];
    $orig_model -> update_inits ( from_output => $orig_model_output,
				  problem_number => $self->probnum(),
				  ignore_missing_parameters => 1);
    $orig_model -> _write( write_data => 1 );
    push( @orig_and_sim_models, $orig_model );
    $simdirname='orig_and_simulation_dir'; 
  }elsif ($self->estimate_input()) {
    $orig_model -> _write( write_data => 1 );
    #run original here to get param estimates for sim

    my $run_orig = tool::modelfit -> new( 
      %{common_options::restore_options(@common_options::tool_options)},
      top_tool         => 0,
      models           => [$orig_model],
      base_directory   => $self->directory,
      directory        => $self->directory . 'original_dir' . $model_number, 
      parent_tool_id   => $self->tool_id,
      logfile	         => undef,
      raw_results_file     => [$self ->raw_results_file()->[0]],
      prepared_models       => undef,
      _raw_results_callback => $self ->
      _modelfit_raw_results_callback( model_number => $model_number ),
      data_path =>'../../m'.$model_number.'/',
      abort_on_fail => $self->abort_on_fail);
    
    ui -> print( category => 'ebe_npde',
		 message  => "Running original model to get final parameter estimates for simulation" );
    
    $run_orig -> run;
    $self->first_callback(0);
    
    unless (defined $run_orig->raw_results) {
      croak("Running original model failed. Check output in ".$run_orig->directory());
    }
    
    if (defined $orig_model->outputs() and 
	defined $orig_model->outputs()->[0] and
	$orig_model->outputs()->[0]-> have_output()){
      $orig_model_output = $orig_model->outputs()->[0];
      $orig_model -> update_inits ( from_output => $orig_model_output,
				    problem_number => $self->probnum());
    }
  }else{
    #must in any case run original to get ETAs, IPRED etc, but here we dont update inits
    $orig_model -> _write( write_data => 1 );
    push( @orig_and_sim_models, $orig_model );
    $simdirname='orig_and_simulation_dir'; 
  }

  my $samples = $self -> samples();
  
  my @all_eta_files = ($self->directory . 'm' . $model_number . '/original.etas');
  for( my $sim_no = 1; $sim_no <= $samples ; $sim_no++ ) {
      
    my $sim_name = "sim-$sim_no.mod";
    my $sim_out = "sim-$sim_no.lst";
    my $etafile = 'sim-'.$sim_no.'.etas';
    push(@all_eta_files,$self->directory . 'm' . $model_number . '/' . $etafile);
    
    if( $sim_no == 1 ) {
      $sim_model = $orig_model->
	  copy( filename    => $self->directory . 'm' . $model_number . '/' . $sim_name,
		target      => 'disk',
		copy_data   => 0,
		copy_output => 0);
      $sim_model -> remove_records( type => 'table' );
      $sim_model -> remove_records( type => 'covariance' );
      $sim_model -> shrinkage_stats( enabled => 0 );
      
      if (0){
	#set IGNORE=@ since datafile will
	#get a header during copying. Keep IGNORE=LIST
	
	my $sim_ignorelist = $orig_model -> get_option_value( record_name  => 'data',
							      problem_index => ($self->probnum()-1),
							      option_name  => 'IGNORE',
							      option_index => 'all');
	$sim_model -> remove_option( record_name  => 'data',
				     problem_numbers => [($self->probnum())],
				     option_name  => 'IGNORE',
				     fuzzy_match => 1);
	
	if ((defined $sim_ignorelist) and scalar (@{$sim_ignorelist})>0){
	  foreach my $val (@{$sim_ignorelist}){
	    unless (length($val)==1){
	      #unless single character ignore, cannot keep that since need @
	      $sim_model -> add_option( record_name  => 'data',
					problem_numbers => [($self->probnum())],
					option_name  => 'IGNORE',
					option_value => $val);
	    }
	  }
	}
	$sim_model -> add_option( record_name  => 'data',
				  problem_numbers => [($self->probnum())],
				  option_name  => 'IGNORE',
				  option_value => '@');
      }

      # set $TABLE record
      
      $sim_model -> add_records( type           => 'table',
				 problem_numbers => [($self->probnum())],
				 record_strings => ['IWRES ID NOPRINT NOAPPEND ONEHEADER FILE=dummy']);
      $sim_model -> add_records( type           => 'table',
				 problem_numbers => [($self->probnum())],
				 record_strings => [$etaheader.'NOPRINT NOAPPEND ONEHEADER FIRSTONLY FILE=dummy']);
      
      unless ($self->reminimize()){
	$sim_model -> set_maxeval_zero(print_warning => 1,
				       last_est_complete => $self->last_est_complete(),
				       niter_eonly => $self->niter_eonly(),
				       need_ofv => 1);
      }


    }else{
      $sim_model = $orig_and_sim_models[$#orig_and_sim_models]->
	  copy( filename    => $self->directory . 'm' . $model_number . '/' . $sim_name,
		target      => 'disk',
		copy_data   => 0,
		copy_output => 0);


    }#end if elsesim_no==1


    $sim_model -> ignore_missing_files( 1 );
    $sim_model -> outputfile( $self->directory . 'm' . $model_number . '/' . $sim_out );
    $sim_model -> ignore_missing_files( 0 );
    my $prob = $sim_model -> problems -> [$self->probnum()-1];

    my @new_record;
    foreach my $sline ( @{$sim_record } ){
      my $new_line;
      my $sim_line = $sline;
      while( $sim_line =~ /([^()]*)(\([^()]+\))(.*)/g ){
	my $head = $1;
	my $old_seed = $2;
	$sim_line = $3;
	$new_line .= $head;
	
	while( $old_seed =~ /(\D*)(\d+)(.*)/ ){
	  $new_line .= $1;
	  $new_line .= random_uniform_integer( 1, 0, 1000000 ); # Upper limit is from nmhelp 
	  $old_seed = $3;
	}
	
	$new_line .= $old_seed;
	
      }
      push( @new_record, $new_line.$sim_line );
    }
    
    $prob -> set_records( type => 'simulation',
			  record_strings => \@new_record );

    #order of table records important, 1 is iwres 2 is eta
    my $iwres_file = "iwres-$sim_no.dta";
    $prob -> remove_option( record_name  => 'table',
			    option_name  => 'FILE',
			    fuzzy_match => 1,
			    record_number => 1);
      
    $prob -> add_option(record_name  => 'table',
			record_number  => 1,
			option_name  => 'FILE',
			option_value => $iwres_file );   

    $prob -> remove_option( record_name  => 'table',
			    option_name  => 'FILE',
			    fuzzy_match => 1,
			    record_number => 2);
      
    $prob -> add_option(record_name  => 'table',
			record_number  => 2,
			option_name  => 'FILE',
			option_value => $etafile );   
    
    
    push( @all_iwres_files, $self->directory . 'm' . $model_number . '/' . $iwres_file );


    $sim_model -> _write( write_data => 0 );
    push( @orig_and_sim_models, $sim_model );



  } #end loop over number of simulations

  my $run_sim = tool::modelfit -> new( 
	  %{common_options::restore_options(@common_options::tool_options)},
	  top_tool         => 0,
	  models           => \@orig_and_sim_models,
	  base_directory   => $self->directory,
	  nmtran_skip_model => 3,
	  directory        => $self->directory . $simdirname . $model_number, 
	  parent_tool_id   => $self->tool_id,
	  logfile	         => undef,
	  raw_results_file     => [$self ->raw_results_file()->[0]], #change??
	  prepared_models       => undef,
	  shrinkage => 0,
	  _raw_results_callback => $self ->
	  _modelfit_raw_results_callback( model_number => $model_number ),
	  data_path =>'../../m'.$model_number.'/',
	  abort_on_fail => $self->abort_on_fail);
  
  ui -> print( category => 'ebe_npde',
	       message  => "Running simulations and reestimations" );
  
  $run_sim -> run;
  $self->first_callback(0);
  
  unless (defined $run_sim->raw_results){
    croak("Running simulations failed. Check output in ".$run_sim->directory());
  }
  my @matrix;
  my @sums;
  my $nsim=0;
  my $header;
  foreach my $file (@all_iwres_files){
    #this is only for simulated, not original
    #need not do filtering, as long as can handle strange values for nonobs
    #must keep same number of rows in shrinkage col
    open( IWR, $file ) or croak("Could not find $file.");
    $nsim++;
    my $obs_index = 0;
    while (my $row = <IWR>){
      chomp $row;
      next if ($row =~ /TABLE NO/);
      if ($row =~ /IWRE/){
		  $header = $row."\n";
		  next;
      }
      #order is IWRES ID ...
      $row =~ s/^\s*//;
      my ($iwres,$rest)=split(/\s+/,$row,2);
      if ($nsim>1){
		  push(@{$matrix[$obs_index]},$iwres);
		  $sums[$obs_index] += $iwres;
      }else{
		  $matrix[$obs_index]=[$iwres];
		  $sums[$obs_index]=$iwres;
      }
      $obs_index++;
    }
    close(IWR);
  }
  my @original_iwres;
  my @original_id;
  my @original_mdv;
  my $ofile = $self->directory . 'm' . $model_number . '/original_iwres.dta';
  open( IWR, $ofile ) or croak("Could not find $ofile.");
  while (my $row = <IWR>){
    chomp $row;
    next if ($row =~ /TABLE NO/);
    if ($row =~ /IWRE/){
      next;
    }
    #order is IWRES ID MDV ...
    $row =~ s/^\s*//;
    chomp($row);
    my ($iwres,$id,$mdv)=split(/\s+/,$row);
    push(@original_iwres,$iwres);
    push(@original_id,$id);
    push(@original_mdv,$mdv);
  }
  close(IWR);

  my @shrinkage_arr = (' ',' ');
  my @iwres_npde_arr;
  my @iwres_npd_arr;
  my @decorrelated_iwres;
  my @untrans_iwres;
  for (my $obs=0;$obs<scalar(@sums);$obs++){
    my $mean=$sums[$obs]/$nsim;
    my $sum_errors_pow2=0;
    #$matrix[$obs_index]=[$iwres,...,];
    foreach my $val (@{$matrix[$obs]}){
      $sum_errors_pow2 += ($val - $mean)**2;
    }
    my $stdev=0;
    unless( $sum_errors_pow2 <= 0 ){
      $stdev= sqrt ($sum_errors_pow2/($nsim-1)); #root of variance
    }
    push(@shrinkage_arr,sprintf("%.8f",(1-$stdev)));

    if ($stdev > 0){
      #must subtract mean before transform

      my $original = ($original_iwres[$obs]-$mean)/$stdev;
      my $untrans_original = $original_iwres[$obs];
      my $pde=0;
      my $pd=0;

      foreach my $val (@{$matrix[$obs]}){
		  my $transf= ($val-$mean)/$stdev;
		  $pde++ if ($transf < $original);
		  $pd++ if ($val < $untrans_original);
      }
      $pde = 1 if ($pde == 0);
      $pde = ($nsim-1) if ($pde == $nsim);
      $pde=$pde/$nsim;
	  
      $pd = 1 if ($pd == 0);
      $pd = ($nsim-1) if ($pd == $nsim);
      $pd=$pd/$nsim;

      push(@decorrelated_iwres,sprintf("%.6f",$original));
      push(@iwres_npde_arr,
	   sprintf("%.6f",-(Statistics::Distributions::udistr($pde))))
	  if ($self->have_CDF());

      push(@iwres_npd_arr,
	   sprintf("%.6f",-(Statistics::Distributions::udistr($pd))))
	  if ($self->have_CDF());

      push(@untrans_iwres,sprintf("%.6f",$untrans_original));
    }else{
      push(@iwres_npde_arr,sprintf("%.6f",-99));
      push(@decorrelated_iwres,sprintf("%.6f",-99));
      push(@iwres_npd_arr,sprintf("%.6f",-99));
      push(@untrans_iwres,sprintf("%.6f",-99));
    }
  }

  if ($self->have_CDF()){
    open(DAT, ">iwres_npde.csv") || 
	die("Couldn't open iwres_npde.csv : $!");
    print DAT "ID,MDV,NPDE\n";
    for (my $i=0; $i<scalar(@iwres_npde_arr);$i++){
      print DAT $original_id[$i].','.$original_mdv[$i].','.$iwres_npde_arr[$i]."\n";
    }
    close (DAT);
  }
  if ($self->have_CDF()){
    open(DAT, ">iwres_npd.csv") || 
	die("Couldn't open iwres_npd.csv : $!");
    print DAT "ID,MDV,NPD\n";
    for (my $i=0; $i<scalar(@iwres_npd_arr);$i++){
      print DAT $original_id[$i].','.$original_mdv[$i].','.$iwres_npd_arr[$i]."\n";
    }
    close (DAT);
  }

  open(ORI, ">decorrelated_original_iwres.csv") || 
      die("Couldn't open decorrelated_original_iwres.csv : $!");
  print ORI "ID,MDV,IWRES_STAR\n";
  open(ORI2, ">raw_original_iwres.csv") || 
      die("Couldn't open raw_original_iwres.csv : $!");
  print ORI2 "ID,MDV,IWRES\n";
  for (my $i=0; $i<scalar(@decorrelated_iwres);$i++){
    print ORI $original_id[$i].','.$original_mdv[$i].','.$decorrelated_iwres[$i]."\n";
    print ORI2 $original_id[$i].','.$original_mdv[$i].','.$untrans_iwres[$i]."\n";
  }

  close ORI;
  close ORI2;

      
  #append to datafile, also print to own file
  my $fname = 'm'.$model_number.'/orig_pred.dta'; 
  if (-e $fname){
    my @tmp = OSspecific::slurp_file($fname);
    my $first=1;
    open(EBE_NPDE, '>'.$self->gls_data_file()) || die("Couldn't open ".$self->gls_data_file()." : $!");
    open(DAT, ">ind_iwres_shrinkage.dta") || 
	die("Couldn't open ind_iwres_shrinkage.dta : $!");
    chomp $tmp[1];
    print EBE_NPDE $tmp[1]."       ISHR\n";
    print DAT "ISHR\n";
    for (my $i=2; $i<scalar(@tmp); $i++){
      chomp $tmp[$i];
      print EBE_NPDE $tmp[$i]." ".$shrinkage_arr[$i]."\n";
      print DAT $shrinkage_arr[$i]."\n";
    }
    close (EBE_NPDE);
    close (DAT);
  }else{
    die "$fname does not exist\n";
  }
  

  my $etamatrix;
  my $transf_etamatrix;
  my $untransf_etamatrix;
  my $nsamples=0;
  $header=undef;
  my $neta=0;
  my @idcol;
  

  my $idfile = ($self->directory . 'm' . $model_number . '/original.ids');
  open( PHI, $idfile ) or croak("Could not find $idfile");
  while (my $row = <PHI>){
    chomp $row;
    next if ($row =~ /TABLE NO/);
    next if ($row =~ /ID/);
    $row =~ s/\s+//g;
    push(@idcol,$row);
  }
  close(PHI);

  my $mean_array;
  #TODO must skip etas for OMEGA 0 FIX here
  my $printid=-2;
  foreach my $file (@all_eta_files){
	  #first is original
	  open( PHI, $file ) or croak("Could not find $file");
	  $nsamples++;
	  my $id_index = 0;
	  while (my $row = <PHI>){
		  chomp $row;
		  next if ($row =~ /TABLE NO/);
		  $row =~ s/^\s*//;
		  my @values=split(/\s+/,$row);
		  if ($row =~ /ETA/){
			  unless (defined $header){
				  $header = 'ID';
				  $neta = scalar(@values);
				  $header .= ','.join(',',@values);
			  }
			  next;
		  }
		  
		  print "\n$id_index\n" if ($nsamples==6 and $id_index==$printid);
		  if ($nsamples>1){
			  for (my $j=0;$j<$neta;$j++){
				  push(@{$etamatrix->[$j][$id_index]},$values[$j]);
				  print join(',',@{$etamatrix->[$j][$id_index]})."\n" if ($nsamples==6 and $id_index==$printid);
				  $mean_array->[$j][$id_index] += $values[$j];
			  }
		  }else{
			  for (my $j=0;$j<$neta;$j++){
				  $etamatrix->[$j][$id_index]=[$values[$j]];
				  $mean_array->[$j][$id_index] = 0; #do not count original in mean
			  }
		  }
		  $id_index++;
		  print "\n\n" if ($nsamples==6 and $id_index==$printid);
	  }
	  close(PHI);
	  
  }
  

  #loop over individuals

  my $sqrt=sqrt($self->samples()-1);
  for (my $id=0;$id<scalar(@idcol);$id++){
    my @Amat;
    my @original;
    my @untrans_original;
    my @meanvec;
    for (my $j=0;$j<$neta;$j++){
      my $mean = $mean_array->[$j][$id]/($self -> samples());
      push (@meanvec,$mean);
      my $oval = shift(@{$etamatrix->[$j]->[$id]});
      push(@original,($oval-$mean));
      push(@untrans_original,$oval);
      if (0){
	print "\n sum ".$mean_array->[$j][$id]." mean $mean samples ".$self -> samples()."\n";
	print "\n";
      }
      foreach my $val (@{$etamatrix->[$j]->[$id]}){
	printf("%.9f ",$val) if ($id == $printid);
	push(@{$Amat[$j]},$val-$mean);
      }
      print "\n" if ($id == $printid);
    }
    if($id == $printid){
      print "\n";
      print "original \n";
      print join(' ',@untrans_original)."\n";
    }
    my $Rmat = [];
    my $numerr = linear_algebra::QR_factorize(\@Amat,$Rmat);
    #TODO handle numerr

    #want to multiply with inverse 'square root' (in a loose sense) of 
    #empirical variance-covariance matrix of A'A
    #i.e. we want to multiply with inv(R*diag(1/sqrt(N-1))
    #i.e. we want to solve orig=(R*diag(1/sqrt(N-1))*transf

    

    #Rmat->[$col][$row]

    #have already subtracted $mean from original

    my $ncol = scalar(@{$Rmat});

    #solve diag(1/sqrt(N-1))*bvec=yvec
    for (my $j=0;$j<$ncol;$j++){
      $original[$j]=$original[$j]*$sqrt;
    }

    #solve R'* transf=bvec
    #Golub p 88
    
    if (0){
	#naive division by 0 protection
	if ($Rmat->[0][0] != 0) {
	    $original[0]=$original[0]/$Rmat->[0][0];
	} else {
	    $original[0]=0;
	}
	
	for (my $i=1;$i<$ncol;$i++){
	    my $sum=0;
	    for (my $j=0;$j<$i;$j++){
		$sum += ($Rmat->[$i][$j])*$original[$j];
	    }
	    if ($Rmat->[$i][$i] != 0) {
		$original[$i]=($original[$i]-$sum)/$Rmat->[$i][$i];
	    } else {
		$original[$i]=0;
	    }
	    
	}
    }else{
	my $numerr = linear_algebra::upper_triangular_transpose_solve($Rmat,\@original);
	#TODO handle numerr
    }

 
    for (my $j=0;$j<$neta;$j++){
      $transf_etamatrix->[$j][$id]=[$original[$j]];
      $untransf_etamatrix->[$j][$id]=[$untrans_original[$j]];
    }
    #transform etamatrix for each simulation of id $id
    
    for (my $k=0;$k<$nsim;$k++){
		my @simvec;
		for (my $j=0;$j<$neta;$j++){
			#must subtract mean here also
			my $val = shift(@{$etamatrix->[$j][$id]});
			push(@{$untransf_etamatrix->[$j][$id]},$val);
			push(@simvec,$val-$meanvec[$j]);
		}
		
		#solve R'*x=simvec
		
		if(0){
			#naive division by 0 protection
			if ($Rmat->[0][0] != 0) {
				$simvec[0]=$simvec[0]/$Rmat->[0][0];
			} else {
				$simvec[0]=0;
			}
			
			for (my $i=1;$i<$ncol;$i++){
				my $sum=0;
				for (my $j=0;$j<$i;$j++){
					$sum += ($Rmat->[$i][$j])*$simvec[$j];
				}
				if ($Rmat->[$i][$i] != 0) {
					$simvec[$i]=($simvec[$i]-$sum)/$Rmat->[$i][$i];
				} else {
					$simvec[$i]=0;
				}
			}
		}else{
			my $numerr = linear_algebra::upper_triangular_transpose_solve($Rmat,\@simvec);
			#TODO handle numerr
		}
		
		
		#solve diag(1/sqrt(N-1))*transf=x
		for (my $j=0;$j<$ncol;$j++){
			$simvec[$j]=$simvec[$j]*$sqrt;
		}
		
		
		for (my $j=0;$j<$neta;$j++){
			push(@{$transf_etamatrix->[$j][$id]},$simvec[$j]);
		}
    }
	
  } #end loop over id


  open(DAT, ">eta_npde.csv") || 
      die("Couldn't open eta_npde.csv : $!");
  print DAT "$header\n";
  open(RANK, ">eta_pde.csv") || 
      die("Couldn't open eta_pde.csv : $!");
  print RANK "$header\n";

  open (ORI, ">decorrelated_original_eta.csv") || 
      die("Couldn't open decorrelated_original_eta.csv : $!");
  print ORI "$header\n";
  open(DAT2, ">eta_npd.csv") || 
      die("Couldn't open eta_npd.csv : $!");
  print DAT2 "$header\n";

  open (ORI2, ">raw_original_eta.csv") || 
      die("Couldn't open raw_original_eta.csv : $!");
  print ORI2 "$header\n";

  for (my $i=0;$i<scalar(@idcol);$i++){
    print DAT $idcol[$i];
    print RANK $idcol[$i];
    print ORI $idcol[$i];
    print DAT2 $idcol[$i];
    print ORI2 $idcol[$i];

    for (my $j=0;$j<$neta;$j++){
      my $original = shift(@{$transf_etamatrix->[$j]->[$i]});
      my $pde=0;

      foreach my $val (@{$transf_etamatrix->[$j]->[$i]}){
		  $pde++ if ($val < $original);
      }
      $pde = 1 if ($pde == 0);
      $pde = ($nsim-1) if ($pde == $nsim);
      $pde=$pde/$nsim;
	  
      print RANK ','.sprintf("%.6f",$pde);
      print DAT ','.sprintf("%.6f",-(Statistics::Distributions::udistr($pde)))
		  if ($self->have_CDF());
      print ORI ','.sprintf("%.6f",$original);
    }
    for (my $j=0;$j<$neta;$j++){
		my $original = shift(@{$untransf_etamatrix->[$j]->[$i]});
		my $pd=0;
		
		my $ordernum=1;
		foreach my $val (@{$untransf_etamatrix->[$j]->[$i]}){
			$pd++ if ($val < $original);
		}
		$pd = 1 if ($pd == 0);
		$pd = ($nsim-1) if ($pd == $nsim);
		$pd=$pd/$nsim;
		print DAT2 ','.sprintf("%.6f",-(Statistics::Distributions::udistr($pd)))
			if ($self->have_CDF());
		print ORI2 ','.sprintf("%.6f",$original);
    }
    print DAT "\n";
    print RANK "\n";
    print ORI "\n";
    print DAT2 "\n";
    print ORI2 "\n";
  }
  close DAT;
  close RANK;
  close ORI;
  close DAT2;
  close ORI2;
  
}
end modelfit_setup



start cleanup
{
  #remove tablefiles in simulation NM_runs, they are 
  #copied to m1 by modelfit and read from there anyway.
  for (my $samp=1;$samp<=$self->samples(); $samp++){
    unlink $self->directory . "/simulation_dir1/NM_run" . $samp . "/mc-sim-" . $samp . ".dat";
    unlink $self->directory . "/simulation_dir1/NM_run" . $samp . "/mc-sim-" . $samp . "-1.dat"; #retry
  }

}
end cleanup




# {{{ _modelfit_raw_results_callback

start _modelfit_raw_results_callback

# Use the mc's raw_results file.
my ($dir,$file) = 
    OSspecific::absolute_path( $self->directory,
			       $self -> raw_results_file->[$model_number-1] );
my ($npdir,$npfile) = 
    OSspecific::absolute_path( $self->directory,
			       $self -> raw_nonp_file -> [$model_number-1]);


$subroutine = sub {
  #can have 2 $PROB if tnpri and est_sim, interesting with 2nd $PROB only
  my $modelfit = shift;
  my $mh_ref   = shift;
  my %max_hash = %{$mh_ref};
  $modelfit -> raw_results_file( [$dir.$file] );
  $modelfit -> raw_nonp_file( [$npdir.$npfile] );
  $modelfit -> raw_results_append( 1 ) if ($self->first_callback eq '0');
  my $totsamples=1;
  $totsamples = $self -> samples() if (defined $self -> samples());


  # a column with run type, original or sim is prepended. 

  #if prior tnpri nothing will be in raw_results for first $PROB, can
  #take first row for model as final estimates as usual, even if
  #it happens to be from second $PROB

  if ( defined $modelfit -> raw_results() ) {
    $self->stop_motion_call(tool=>'ebe_npde',message => "Preparing to rearrange raw_results in memory, adding ".
			    "model name information")
	if ($self->stop_motion());
    
    my $n_rows = scalar(@{$modelfit -> raw_results()});

    my $last_model= 0;
    my $sample = 0; 

    if ($self->first_callback){
      unshift( @{$modelfit->raw_results_header}, 'run_type' );
    }

    my $type;
    if ($self->first_callback) {
      $type='original';
    }else{
      $type='simulation';
    }
    for (my $i=0; $i< $n_rows; $i++){
      my $this_model = $modelfit -> raw_results()->[$i]->[0]; 
      my $step= ($this_model-$last_model);
      if ($last_model > 0 and $step>0){
	$type='simulation';
      }
      if ($step < 0){
	ui -> print( category => 'ebe_npde',
		     message  => "Warning: It seems the raw_results is not sorted");
      }else {

	$sample += $step; #normally +1, sometimes 0,sometimes 2 or more
	unshift( @{$modelfit -> raw_results()->[$i]}, $type );

      }
      $last_model=$this_model;
    }

    if ($self->first_callback){
      $self->raw_line_structure($modelfit->raw_line_structure);

      my $laststart=0;
      foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure})){
	foreach my $category (keys %{$self->raw_line_structure -> {$mod}}){
	  next if ($category eq 'line_numbers');
	  my ($start,$len) = split(',',$self->raw_line_structure -> {$mod}->{$category});
	  $self->raw_line_structure -> {$mod}->{$category} = ($start+1).','.$len; #add 1 for hypothesis
	  $laststart=($start+$len) if (($start+$len)> $laststart);
	}
	$self->raw_line_structure -> {$mod}->{'run_type'} = '0,1';
      }

      $self->raw_line_structure->write( $dir.'raw_results_structure' );
    }
  } #end if defined modelfit->raw_results

  if ( defined $modelfit -> raw_nonp_results() ) {
    
    my $n_rows = scalar(@{$modelfit -> raw_nonp_results()});

    my $last_model= 0;
    my $sample = 0; 
    my $type;
    if ($self->first_callback ){
      $type='original';
    }else{
      $type='simulation';
    }

    unshift( @{$modelfit->raw_nonp_results_header}, 'run_type' );
    
    for (my $i=0; $i< $n_rows; $i++){
      my $this_model = $modelfit -> raw_nonp_results()->[$i]->[0]; 
      my $step= ($this_model-$last_model);
      if ($last_model > 0 and $step>0){
				$type='simulation';
      }
      if ($step < 0){
				ui -> print( category => 'ebe_npde',
		     message  => "Warning: It seems the raw_nonp_results is not sorted");
      } else {
				$sample += $step; #normally +1, sometimes 0,sometimes 2 or more
				unshift( @{$modelfit -> raw_nonp_results()->[$i]}, $type );

      }
      $last_model=$this_model;
    }

  } #end if defined modelfit->raw_nonp_results


  @{$self->raw_results_header} = @{$modelfit->raw_results_header};
  @{$self->raw_nonp_results_header} = @{$modelfit->raw_nonp_results_header};
  #  New header
  
};
return $subroutine;

end _modelfit_raw_results_callback

# }}} _modelfit_raw_results_callback

# {{{ prepare_results

start prepare_results
{ 
  $self -> cleanup();


} 
end prepare_results

# }}}

start house
{ 
  #add checking for 0 div here
  my $n=scalar(@{$xvec});
  my $sigma=0;
  $vvec[0]=1;
  for (my $i=1;$i<$n;$i++){
    $sigma += ($xvec->[$i])**2;
    $vvec[$i]=$xvec->[$i];
  }
  if ($sigma == 0){
    $beta=0;
  }else{
    my $mu = sqrt(($xvec->[0])**2 + $sigma);
    if ($xvec->[0] <= 0){
      $vvec[0] = $xvec->[0]-$mu;
    }else{
      $vvec[0] = -$sigma/($xvec->[0]+$mu);
    }
    $beta = 2*($vvec[0])**2/($sigma+($vvec[0])**2);
    for (my $i=1;$i<$n;$i++){
      $vvec[$i]=$vvec[$i]/$vvec[0];
    }
    $vvec[0]=1;
  }

} 
end house

start QR_factorize
{ 

  #verified against matlab for small matrices

  #house transform
  #assume Amatrix column format

  my $ncol= scalar(@Amatrix);
  my $endcol=$ncol-1;
  my $mrow = scalar(@{$Amatrix[0]});
  my $endrow=$mrow-1;

  if (0){
    print "\n";
    for (my $j=0;$j<$mrow;$j++){
      for (my $i=0;$i<$ncol;$i++){
	printf("  %.6f",$Amatrix[$i][$j]);
      }
      print "\n";
    }
    print "\n";
    print "\n";
    print "\n";
  }

  for (my $j=0;$j<$ncol;$j++){
    my @xvec = @{$Amatrix[$j]}[$j..$endrow];
    my ($vvec,$beta)=$self->house(xvec => \@xvec);
    #house transform A(j:endrow,j:endcol)
    #for first col know only first comp is number, rest is 0 (R matrix)
    #w=beta Atrans v
    my @wvec;
    for (my $i=0;$i<($ncol-$j);$i++){
      my $col=$i+$j;
      $wvec[$i]=0;
      for (my $k=0;$k<($mrow-$j);$k++){
	$wvec[$i] += $beta*($vvec->[$k])*$Amatrix[$col][$k+$j];
      }      
    }
    #col $j gives R
    my @rcol;
    @rcol = @{$Amatrix[$j]}[0..($j-1)] if ($j>0);
    push(@rcol,($Amatrix[$j][$j]-$wvec[0]*$vvec->[0]));
    push(@Rmatrix,\@rcol);
    #check that rest practically 0 
    for (my $k=1;$k<($mrow-$j);$k++){
      my $val = $Amatrix[$j][$j+$k]-$wvec[0]*$vvec->[$k];
      unless ( $val < 0.00001){
	print "error in house transformation j $j k $k val $val\n";
      }
    }
    #tranform rest of A cols for next iteration
    for (my $i=1;$i<($ncol-$j);$i++){
      for (my $k=0;$k<($mrow-$j);$k++){
	$Amatrix[$i+$j][$j+$k] = $Amatrix[$i+$j][$j+$k]-$wvec[$i]*$vvec->[$k];
      }
    }
  }

  if (0){
    for (my $j=0;$j<scalar(@Rmatrix);$j++){
      for (my $i=$j;$i<scalar(@Rmatrix);$i++){
	printf("  %.6f",$Rmatrix[$i]->[$j]);
      }
      print "\n";
    }
    print "\n";
  }


} 
end QR_factorize


# {{{ rmse_percent

start rmse_percent
{
  #input is integers $column_index, $start_row_index, $end_row_index and scalar float $initial_value
  #output is scalar $rmse_percent

  unless ($end_row_index) {
		$self->raw_results([]) unless defined $self->raw_results;
    $end_row_index = $#{$self->raw_results};
  }

  croak("Bad row index input") if ($start_row_index > $end_row_index);
  my $row_count = 0;
  my $sum_squared_errors=0;
  for (my $i=$start_row_index; $i<=$end_row_index; $i++){
    if ($use_runs[$i-$start_row_index]){
      if (defined $self->raw_results->[$i][$column_index]) {
				$sum_squared_errors += ($self->raw_results->[$i][$column_index] - $initial_value)**2;
				$row_count++;
      }else{
      }
    }
  }
  
  if ($row_count == 0){
    $rmse_percent='NA';
  }elsif ($initial_value == 0){
    carp("Initial value 0, returning absolute rmse instead of relative.");
    $rmse_percent= sqrt($sum_squared_errors/$row_count);
  }else{
    $rmse_percent= (sqrt($sum_squared_errors/$row_count))*100/abs($initial_value);
  }
}
end rmse_percent

# }}} rmse_percent

# {{{ bias_percent

start bias_percent
{
  #input is integers $column_index, $start_row_index, $end_row_index and scalar float $initial_value
  #output is scalar $bias_percent

  unless( $end_row_index ){
		$self->raw_results([]) unless defined $self->raw_results;
    $end_row_index = $#{$self->raw_results};
  }
  
  croak("Bad row index input") if ($start_row_index > $end_row_index);
  
  my $row_count = 0;
  my $sum_errors=0;
  for (my $i=$start_row_index; $i<=$end_row_index; $i++){
    if ($use_runs[$i-$start_row_index]){
      if (defined $self->raw_results->[$i][$column_index]){
				$sum_errors += ($self->raw_results->[$i][$column_index] - $initial_value);
				$row_count++;
      }else{
      }
    }
  }

  if ($row_count == 0){
    $bias_percent='NA';
  }elsif ($initial_value == 0){
    carp("Initial value 0, returning absolute bias instead of relative.");
    $bias_percent= ($sum_errors/$row_count);
  }else{
    $bias_percent= ($sum_errors/$row_count)*100/abs($initial_value);
  }
}
end bias_percent

# }}} bias_percent

# {{{ skewness_and_kurtosis

start skewness_and_kurtosis
{
  #input is integers $column_index, $start_row_index, $end_row_index 
  
  unless( $end_row_index ){
		$self->raw_results([]) unless defined $self->raw_results;
    $end_row_index = $#{$self->raw_results};
  }

  croak("Bad row index input") if ($start_row_index >= $end_row_index);

  my $row_count = 0;
  my $sum_values=0;
  for (my $i=$start_row_index; $i<=$end_row_index; $i++){
    if ($use_runs[$i-$start_row_index]){
      if (defined $self->raw_results->[$i][$column_index]){
	$sum_values += $self->raw_results->[$i][$column_index];
	$row_count++;
      }else{
	$warn=$i-$start_row_index+1;
      }
    }
  }

  if ($row_count < 2){
    $stdev='NA';
    $skewness='NA';
    $kurtosis='NA';
    return;
  }

  $mean=$sum_values/$row_count;

  my $error=0;
  my $sum_errors_pow2=0;
  my $sum_errors_pow3=0;
  my $sum_errors_pow4=0;

  for (my $i=$start_row_index; $i<=$end_row_index; $i++){
    if (defined $self->raw_results->[$i][$column_index]){
      $error = ($self->raw_results->[$i][$column_index] - $mean);
      $sum_errors_pow2 += $error**2;
      $sum_errors_pow3 += $error**3;
      $sum_errors_pow4 += $error**4;
    }
  }

  ## TODO fråga om missing values. och om SD

  $stdev=0;
  unless( $sum_errors_pow2 == 0 ){

    $stdev= sqrt ($sum_errors_pow2/($row_count-1));
    $skewness = $sum_errors_pow3/($row_count*($stdev**3));
    $kurtosis = -3 + $sum_errors_pow4/($row_count*($stdev**4));

  }
}
end skewness_and_kurtosis

# }}} skewness_and_kurtosis

# {{{ max_and_min

start max_and_min
{
  #input is integers $column_index, $start_row_index, $end_row_index 
  
  unless( $end_row_index ){
		$self->raw_results([]) unless defined $self->raw_results;
    $end_row_index = $#{$self->raw_results};
  }

  croak("Bad row index input") if ($start_row_index >= $end_row_index);

  $maximum = -1000000000;
  $minimum =  1000000000;
  for (my $i=$start_row_index; $i<=$end_row_index; $i++){
    if ($use_runs[$i-$start_row_index]) {
      if (defined $self->raw_results->[$i][$column_index]){
				$maximum = $self->raw_results->[$i][$column_index] if ($self->raw_results->[$i][$column_index] > $maximum); 
				$minimum = $self->raw_results->[$i][$column_index] if ($self->raw_results->[$i][$column_index] < $minimum); 
      } else {
      }
    }
  }
}
end max_and_min

# }}} max_and_min


# {{{ median

start median
{
  #input is integers $column_index, $start_row_index, $end_row_index 
  
  unless( $end_row_index ){
		$self->raw_results([]) unless defined $self->raw_results;
    $end_row_index = $#{$self->raw_results};
  }
  
  croak("Bad row index input") if ($start_row_index >= $end_row_index);

  my @temp;
  
  for (my $i=$start_row_index; $i<=$end_row_index; $i++){
    if ($use_runs[$i-$start_row_index]){
      if (defined $self->raw_results->[$i][$column_index]){
				push( @temp, $self->raw_results->[$i][$column_index] );
      }else{
      }
    }
  }
  
  @temp = sort({$a <=> $b} @temp);
  if( scalar( @temp ) % 2 ){
    $median = $temp[$#temp/2];
  } else {
    $median = ($temp[@temp/2]+$temp[(@temp-2)/2]) / 2;
  }
 
}
end median

# }}} median
