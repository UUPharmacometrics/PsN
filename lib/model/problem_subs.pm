# {{{ Include

start include statements
use Carp;
use Data::Dumper;
my @print_order_omega_before_pk = ('sizes','problem','abbreviated','input','bind','data','msfi','contr','subroutine','prior','model','tol','infn','omega','anneal','pk','level','aesinitial','aes','des','error','pred','mix','theta','sigma','etas','phis','simulation','estimation','covariance','nonparametric','table','scatter');
my @print_order = ('sizes','problem','abbreviated','input','bind','data','msfi','contr','subroutine','prior','model','tol','infn','pk','level','aesinitial','aes','des','error','pred','mix','theta','omega','anneal','sigma','etas','phis','simulation','estimation','covariance','nonparametric','table','scatter');
my @sde_print_order = ('sizes','problem','abbreviated','input','bind','data','msfi','contr','subroutine','prior','model','tol','infn','theta','omega','anneal','sigma','etas','phis','pk','level','aesinitial','aes','des','error','pred','mix','simulation','estimation','covariance','nonparametric','table','scatter');
my %abbreviations;
my %unsupported_records;

# Here we intialize a hash used to find long names for abbreviated
# record names. We use the print_order array which contains all
# allowed record types.

foreach my $record_name( @print_order,'warnings','finedata' ){
  my $uc_short_type = substr(uc($record_name),0,3);;
  $uc_short_type = $uc_short_type.' ' if ( $record_name eq 'aes' );
  $uc_short_type = $uc_short_type.'I' if ( $record_name eq 'aesinitial' );
  $abbreviations{$uc_short_type} = $record_name;
}
foreach my $rec ('THETAI','THI','THETAR','THR','THETAP','THETAPV','OMEGAP','OMEGAPD','SIGMAP','SIGMAPD'){
    $unsupported_records{$rec}=1;
}


end include statements

# }}} include statements

# {{{ new

start new
    {
      
      unless ( defined $parm{'problems'} ) {
	# Parse given problem lines.
	$this -> _read_records();

	delete $this -> {'prob_arr'};		# FIXME: This is better fixed with Moose

	$this->update_prior_information();

	if (defined $this -> estimations() and ($PsN::nm_major_version > 6)){
	  my $default_format='s1PE12.5';
	  my $reset_file = 0;
	  my $reset_nolabel = 0;
	  my $reset_notitle = 0;
	  my $reset_format = 0;
	  my $found_order = 0;
	  my @estims = @{$this -> estimations()};
	  for (my $i=0; $i <= $#estims; $i++){
	    my $est = $estims[$i];
	    my $opt_ref = $est -> options();
	    if ( defined $opt_ref ) {
	      foreach my $option ( @{$opt_ref} ) {
		if (index('ORDER',$option->name())==0){
		  $found_order = 1 ;
		  next;
		}
		if (index('FILE',$option->name())==0){
		  $reset_file = 1 ;
		  if ($i == $#estims){
		    $option->value('psn.ext');
		    $reset_file = 0; #already done
		  }
		  next;
		}
		if (index('NOTITLE',$option->name())==0){
		  $reset_notitle = 1 ;
		  if ($i == $#estims){
		    $option->value('0');
		    $reset_notitle = 0; #already done
		  }
		  next;
		}
		if (index('NOLABEL',$option->name())==0){
		  $reset_nolabel = 1 ;
		  if ($i == $#estims){
		    $option->value('0');
		    $reset_nolabel = 0; #already done
		  }
		  next;
		}
	      } #end loop options
	    }
	    if ($i == $#estims){
	      #now we know that if we need to reset something, that option was not 
	      #set in last $EST, only in a previous one. Means we can add option
	      $est->_add_option(option_string => 'NOLABEL=0') if ($reset_nolabel);
	      $est->_add_option(option_string => 'NOTITLE=0') if ($reset_notitle);
	      $est->_add_option(option_string => 'FILE=psn.ext') if ($reset_file);
	      $est->_add_option(option_string => 'FORMAT='.$default_format) if ($reset_format);
	    }
	  }
	  if ($found_order){
	    $this -> remove_option( record_name => 'estimation',
				    record_number => 0,
				    option_name => 'ORDER',
				    fuzzy_match => 1 );
	    print "\n***Warning***\n".
		"Option ORDER in \$EST is not yet supported by PsN. It has been removed from ".
		"the control stream.\n";
	  }
	}
      }
      # Initialize table file objects (if any)
      $this -> _read_table_files( ignore_missing_files =>
				  $this->ignore_missing_output_files );

      if ( $this->cwres ) {

	$this -> add_cwres_module( 'init_data' => { problem => $this,
						    mirror_plots => $this->mirror_plots } );
	
      }

      if( $this->tbs ){
	if (defined $this->nwpri_ntheta()){
	  ui -> print( category => 'all',
		       message => "Warning: \$PRIOR NWPRI is not supported in combination with -tbs.",
		       newline => 1);
	}

	$this -> tbs_transform();
      }

    }
end new

# }}} new

start tbs_transform
    {
      if ((defined $self->priors()) and scalar(@{$self -> priors()})>0 ){
	croak("The current version of PsN does not support \$PRIOR and ".
	    "option -tbs in combination");
      }      

      my $newthetanum;
      $newthetanum=$self -> record_count( record_name => 'theta' )+1;
      $self->tbs_thetanum($newthetanum);
      if (defined $self->tbs_param()){
	$self->add_records( type => 'theta',
			    record_strings => [$self->tbs_param().' ; tbs_lambda'] );
      }else{
	$self->add_records( type => 'theta',
			    record_strings => ['1 ; tbs_lambda'] );
      }
      #PRED or ERROR
      my @code;
      @code = @{$self -> errors()->[0]->code()} if (defined $self -> errors());
      my $use_pred = 0;
      unless ( $#code > 0 ) {
	@code = @{$self -> preds()->[0]->code()} if (defined $self -> preds());
	$use_pred = 1;
      }
      if ( $#code <= 0 ) {
	croak("Neither ERROR or PRED defined, cannot use -tbs\n" );
      }

      #locate first use IWRES in right-hand-side, same for W, IPRED. Check Uppsala style
      #IF ELSE around IPRED, IWRES and Y

      my $found=0;
      for (my $i=$#code; $i>=0;$i--){
	if (($code[$i] =~ /[^a-zA-Z_0-9]Y\s*=/) or
	    ($code[$i] =~ /^Y\s*=/)){
	  if ($found){
	    croak("Cannot handle multiple-line definitions of Y with option -tbs.");
	  }else{
	    #remove Y line
	    @code =  (@code[0..$i-1],
		      @code[$i+1..$#code]);
	    $found=1;
	  }
	}
      }
      croak("Failed to find Y definition row in \$PK/\$ERROR") unless ($found);

      my $ipredrow=0;
      for (my $i=$#code; $i>=0;$i--){
	if (($code[$i] =~ /[^a-zA-Z_0-9]IPRED\s*=/) or
	    ($code[$i] =~ /^IPRED\s*=/)){
	  @code =  (@code[0..$i],
		    " LAMBDA = THETA($newthetanum)\n",
		    " IPRTR=IPRED\n",
		    " IF (LAMBDA .NE. 0 .AND. IPRED .NE.0) THEN\n",
		    "    IPRTR=(IPRED**LAMBDA-1)/LAMBDA\n",
		    " ENDIF\n",
		    " IF (LAMBDA .EQ. 0 .AND. IPRED .NE.0) THEN\n",
		    "    IPRTR=LOG(IPRED)\n",
		    " ENDIF\n",
		    " IF (LAMBDA .NE. 0 .AND. IPRED .EQ.0) THEN\n",
		    "    IPRTR=-1/LAMBDA\n",
		    " ENDIF\n",
		    " IF (LAMBDA .EQ. 0 .AND. IPRED .EQ.0) THEN\n",
		    "    IPRTR=-1000000000\n",
		    " ENDIF\n",
		    " IPRED=IPRTR\n",
		    @code[$i+1..$#code]);
	  $ipredrow = $i+14;
	  $found=1;
	  last;
	}
      }
      croak("Failed to find IPRED definition row in \$PK/\$ERROR. ".
		 "Do not know where in code to add IPRED transformation.") unless ($found);
      $found=0;
      for (my $i=$#code; $i>=0;$i--){
	if (($code[$i] =~ /[^a-zA-Z_0-9]IWRES\s*=/) or
	    ($code[$i] =~ /^IWRES\s*=/)){
	  my $row=$i;
	  $row=$ipredrow if ($ipredrow > $i);
	  @code =  (@code[0..$row],
		    " IWRTR=IWRES\n",
		    " IF (LAMBDA.NE.0 .AND. DV.NE.0 .AND. W.NE.0) THEN\n",
		    "    IWRTR=((DV**LAMBDA-1)/LAMBDA-IPRED)/W\n",
		    " ENDIF\n",
		    " IF (LAMBDA.EQ.0 .AND. DV.NE.0 .AND. W.NE.0) THEN\n",
		    "    IWRTR=(LOG(DV)-IPRED)/W\n",
		    " ENDIF\n",
		    " IF (LAMBDA.NE.0 .AND. DV.EQ.0 .AND. W.NE.0) THEN\n",
		    "    IWRTR=(-1/LAMBDA-IPRED)/W\n",
		    " ENDIF\n",
		    " IF (LAMBDA.EQ.0 .AND. DV.EQ.0 .AND. W.NE.0) THEN\n",
		    "    IWRTR=(-1000000000-IPRED)/W\n",
		    " ENDIF\n",
		    " IWRES=IWRTR\n",
		    " Y=IPRED+EPS(1)*W\n",
		    @code[$row+1..$#code]);
	  $found=1;
	  last;
	}
      }
      croak("Failed to find IWRES definition row in \$PK/\$ERROR. ".
		 "Do not know where in code to add IWRES transformation.") unless ($found);

      if ( $use_pred ) {
	$self -> preds ->[0] ->code (\@code );
      } else {
	$self -> errors ->[0] ->code (\@code );
      }

      #$SUBS. check NM major version. Option to modelfit so do not need to print copy 
      # everywhere?
      #need to copy ccontr etc also.

      $self-> add_option(option_name  => 'CONTR',
			 option_value => 'contr.txt',
			 record_name => 'subroutine',
			 add_record => 1,
			 record_number => -1);
      
      my $val = 'ccontra_nm7.txt';
      if ($PsN::nm_major_version < 7){
	$val = 'ccontra_nm6.txt';
      }
      $self-> add_option(option_name  => 'CCONTR',
			 option_value => $val,
			 record_name => 'subroutine',
			 record_number => -1);

    }
end tbs_transform

start update_prior_information
    {
      #if $PRIOR NWPRI then get NTHETA and NETA
      #loop through $THETA and $OMEGA and set prior=1 for 
      #all params after the estimated ones


      if ((defined $self->priors()) and scalar(@{$self -> priors()})>0 ){
	my $nwpri=0;
	foreach my $rec (@{$self -> priors()}){
	  unless ((defined $rec) &&( defined $rec -> options )){
	    carp("No options for rec \$PRIOR" );
	  }
	  foreach my $option ( @{$rec -> options} ) {
	    if ((defined $option) and 
		(($option->name eq 'NWPRI') || (index('NWPRI',$option ->name ) == 0))){
	      $nwpri=1;
	    }
	  }
	}
	if ($nwpri){
	  my $neta;
	  my $ntheta;
	  foreach my $rec (@{$self -> priors()}){
	    foreach my $option ( @{$rec -> options} ) {
	      if (defined $option){
		if  ($option->name eq 'NETA'){ #NONMEM does not allow abbrev
		  if ( (defined $option->value) and ($option->value ne '')){
		    #now we must split on ,
		    my @rest;
		    ($neta,@rest) = split(',',$option->value);
		  }
		}elsif  ($option->name eq 'NTHETA'){ #NONMEM does not allow abbrev
		  if ( (defined $option->value) and ($option->value ne '')){
		    #now we must split on ,
		    my @rest;
		    ($ntheta,@rest) = split(',',$option->value);
		  }
		}
		if ( (defined $option->value) and ($option->value ne '')){
		  if ($option->value =~  /NETA/){
		    my @opts = split(',',$option->value);
		    foreach my $opt (@opts){
		      if ($opt =~ /^NETA=([0-9]+)$/){
			$neta = $1;
		      }
		    }
		  }
		  if ($option->value =~  /NTHETA/){
		    my @opts = split(',',$option->value);
		    foreach my $opt (@opts){
		      if ($opt =~ /^NTHETA=([0-9]+)$/){
			$ntheta = $1;
		      }
		    }
		  }
		}
	      }
	    }
	  }
	  unless (defined $neta){
	    print "\nWarning: Did not find NETA in \$PRIOR\n";
	    $neta=0;
	  }
	  unless (defined $ntheta){
	    print "\nWarning: Did not find NTHETA in \$PRIOR\n";
	    $ntheta=0;
	  }
	  $self->nwpri_ntheta($ntheta);
	  $self->nwpri_neta($neta);
	  
	  #set prior in params
	  if( defined $self->thetas ){
	    my $counter=0;
	    foreach my $record ( @{$self->thetas} ){
	      if( defined $record -> options ){
		foreach my $opt( @{$record -> options}){
		  $counter++;
		  if ($counter > $ntheta){
		    #set prior
		    $opt->prior(1);
		  }
		}
	      }
	    }
	  }
	  if( defined $self->omegas ){
	    my $prevdiag =undef;
	    my $ndiag=0;
	    my $exact=0;
	    foreach my $omega ( @{$self->omegas} ) {
	      my $size = $omega -> size;
	      my $type = $omega -> type;
	      
	      #assume do not start priors in the middle of a block.
	      #Only check after each new record if reached $neta
	      if ($omega->same()) {
		croak("First \$OMEGA cannot be SAME")
		    unless (defined $prevdiag);
		$ndiag += $prevdiag;
	      } elsif( defined $size ) {
		$ndiag += $size;
		$prevdiag = $size;
	      } elsif (defined $omega->options) {
		$ndiag += scalar @{$omega -> options};
		$prevdiag = scalar @{$omega -> options};
	      } else {
		croak("Failed to parse \$OMEGA." );
	      }
	      if ($ndiag == $neta){
		$exact = 1;
	      }elsif ($ndiag > $neta){
		croak("It seems that one \$OMEGA record contains both ".
			   "estimated and prior OMEGAs. PsN requires that this ".
			   "\$OMEGA is split into two so that priors are separate.")
		    unless ($exact);
		$omega->prior(1);
	      }
	    }
	  }
	}
      }

    }
end update_prior_information


start add_prior_distribution
    {
      #First check that do not already have prior
      if ((defined $self->priors()) and scalar(@{$self -> priors()})>0 ){
	print "Warning: Cannot add prior to \$PROB which already has a \$PRIOR\n";
	return;
      }
      #Set ntheta=number of THETAs in input model.
      #Set neta=number of diagonal OMEGAs in input model.
      #Set neps=0
      #Set nthp=ntheta
      #Set netp=neta
      my $ntheta = $self->record_count(record_name=> 'theta');
      my $nthp = $ntheta;
      my $neta = $self->nomegas('with_same'=>1, 'with_correlations' => 0);
      my $netp=$neta;
      my $neps=0;
      #Add record to simulation model
      #$PRIOR NWPRI NTHETA=ntheta,NETA=neta,NEPS=neps,NTHP=nthp,NETP=netp
      #Need to set NPEXP?? number of prior experiments
      my $plev='';
      $plev= 'PLEV=0.99' 
	  if ((defined $self->simulations()) and scalar(@{$self -> simulations()})>0 );
      my $record_string=" NWPRI NTHETA=$ntheta NETA=$neta NTHP=$nthp NETP=$netp $plev";
      $self -> set_records( 'type' => 'prior',
			    'record_strings' => [$record_string] );


      #Add ntheta new $THETA FIX. Initial estimates are final THETA estimates from lst-file.
      my $ref = $from_output->get_single_value(attribute=>'thetacoordval',
					       problem_index => ($problem_number-1),
					       subproblem_index => 0);
      unless (defined $ref){
	print "Cannot add prior if output data lacks THETA estimates\n";
	return;
      }
      my %thetas = %{$ref};
      for (my $i=1;$i<=$nthp;$i++){
	my $val=$thetas{'THETA'.$i};
	$self -> add_records(type => 'theta',
			     record_strings => ["$val FIX"]);
      }
      
      my $ref = $from_output->get_single_value(attribute=>'covariance_matrix',
					       problem_index=>($problem_number-1),
					       subproblem_index=>0);
      unless (defined $ref){
	print "Cannot add prior if output data lacks covariance matrix\n";
	return;
      }

      #Add diagonal $OMEGA FIX where initial estimates is the leading ntheta block 
      #from the variance-covariance matrix in .cov
      my @record_strings = ();
      my $index=0;
      for (my $i=0;$i<$nthp;$i++){
	my @arr;
	for (my $j=0;$j<=$i;$j++){
	  if ($j == $i){
	    my $init = sprintf("%.15G",$ref->[$index]);#must format with E here
	    push(@arr,$init,'FIX');
	  }
	  $index++;
	}
	push(@record_strings,join(' ',@arr));
      }
      my @omega_variance;
      for (my $i=1+$nthp;$i<=($netp+$nthp);$i++){
	my $index=-1+$i*$i/2;
	push(@omega_variance,$ref->[$index]);
      }
      $self -> add_records(type => 'omega',
			   record_strings => \@record_strings);

      #Add $OMEGA FIX where size is neta and initial estimates are final $OMEGA estimate 
      #from lst. Form must match original $OMEGA form in lst.
      my $ref = $from_output->get_single_value(attribute=>'omegacoordval',
					       problem_index => ($problem_number-1),
					       subproblem_index => 0);

      unless (defined $ref){
	print "Cannot add prior if output data lacks OMEGA estimates\n";
	return;
      }
      my %omegas = %{$ref};

      #loop over this models omega records
      #create copy of record
      #use coordinate strings to replace inits with values from output

      my $set_prior_etas=0;
      my @all_formatted;
      my $size=0;
      foreach my $record (@{$self->omegas()}){
	last if ($set_prior_etas >= $neta);
	my @record_strings;
	my $block = 0;
	if ($record->type() eq 'BLOCK'){
	  $block = 1;
	  if ($record->same()){
	    @record_strings = ('BLOCK SAME');
	    $self->add_records(type=> 'omega',
			       record_strings => \@record_strings);
	    $set_prior_etas += $size;
	    next;
	  }else{
	    $size = $record->size();
	    @record_strings = ("BLOCK($size) FIX");
	  }
	}else{
	  $size = 0;
	}
	foreach my $opt (@{$record->options()}){
	  my $label = $opt->coordinate_string();
	  my $init = sprintf("%.15G",$omegas{$label});#must format with E here
	  if ($block){
	    push(@record_strings,"$init");
	  }else{
	    $size++;
	    push(@record_strings,"$init FIX");
	  }
	}
	$set_prior_etas += $size;
	print "Error too many new omegas\n" if ($set_prior_etas > $neta);
	$self->add_records(type=> 'omega',
			   record_strings => \@record_strings);
      }

      my @dflist = split(/,/,$df_string);
      foreach my $df (@dflist){
	$self -> add_records(type => 'theta',
			     record_strings => ["$df FIX"]);
      }


      #immediately after adding $PRIOR etc must run update_prior_information on problem
      $self->update_prior_information();

    }
end add_prior_distribution

# {{{ add_marginals_code

start add_marginals_code

# add_marginals_code takes one argument.
#
# - nomegas which is the number of (diagonal-element)
# omegas.
#
# For each omega, verbatim code is added to make the marginals
# available for printing (e.g. to a table file). COM(1) will hold the
# nonparametric density, COM(2) the marginal cumulative value for the
# first eta, COM(2) the marginal cumulative density for the second eta
# and so on.
# The code is added to the $ERROR record

my $record_ref = $self -> errors;
if( defined $record_ref and defined $record_ref -> [0] ) {
  my ( @first_params, @last_params );
  $last_params[0] = '"   COM(1) = DENM';
  $first_params[0] = '"     X ';
  my $j = 0;
  my $comma;
  for( my $i = 1; $i <= $nomegas; $i++ ) {
    $comma = $i == $nomegas ? '' : ',';
    if( not ($i % 4) ) { # break line every fifth omega
      $j++;
      $first_params[$j] = '"     X ';
    }
    $first_params[$j] = $first_params[$j]."DEN$i$comma";
    push( @last_params, '"   COM('.($i+1).") = DEN$i" );
  }
  my $first_code = $record_ref -> [0] -> verbatim_first;
  push( @{$first_code}, ( '"  COMMON /ROCM18/ DENM,', @first_params,
			  '"  DOUBLE PRECISION DENM,', @first_params ) );
  $record_ref -> [0] -> verbatim_first( $first_code );
  my $last_code = $record_ref -> [0] -> verbatim_last;
  push( @{$last_code}, @last_params );
  $record_ref -> [0] -> verbatim_last( $last_code );
  last; # Only insert the code in the first record found (of the ones specified above)
} else {
  carp("No \$ERROR record was found. Can't add verbatim code".
		    " to access nonparametric marginals" );
}

end add_marginals_code

# }}} add_marginals_code



# {{{ dropped_columns

start dropped_columns
      {
	my $inp_ref = $self -> inputs;
	if ( defined $inp_ref and defined $inp_ref -> [0] ) {
	  my $input = $inp_ref -> [0];
	  my $opt_ref = $input -> options;
	  if ( defined $opt_ref ) {
	    my @options = @{$opt_ref};
	    foreach my $option ( @options ) {
	      my $dropped = ( $option -> value eq 'DROP' or
			      $option -> value eq 'SKIP' ) ? 1 : 0;
	      push ( @dropped_columns, $dropped );
	    }
	  }
	}
      }
end dropped_columns

# }}} dropped_columns

# {{{ drop_dropped

start drop_dropped
      {
	my $inp_ref = $self -> inputs;
	# Important that the drop_dropped method of the data class is
	# in sync with this method.
	if ( defined $inp_ref and defined $inp_ref -> [0] ) {
	  my $input = $inp_ref -> [0];
	  my $opt_ref = $input -> options;
	  if ( defined $opt_ref ) {
	    my @options = @{$opt_ref};
	    my @keep;
	    foreach my $option ( @options ) {
	      push ( @keep, $option ) if ( not ($option -> value eq 'DROP' or $option -> value eq 'SKIP'
					   or $option -> name eq 'DROP' or $option -> name eq 'SKIP') or
					   $option -> name =~ /DAT(E|1|2|3)/ );
	    }
	    $input -> options( \@keep );
	  }
	}
      }
end drop_dropped

# }}} drop_dropped

# {{{ read_table_files

start _read_table_files
      {
	$self->table_files([]);
	my ( $table_name_ref, $junk ) = $self -> _option_val_pos( record_name => 'table',
								  name        => 'FILE' );
	if ( defined $table_name_ref and scalar @{$table_name_ref} >= 0 ) {
	  $self->table_files([]);
	  foreach my $table_name ( @{$table_name_ref} ) {
	    carp("Creating new table_file object from $table_name" );
	    my $new_table = data -> new( directory            => $self->directory,
					 filename             => $table_name,
					 ignore_missing_files => $ignore_missing_files,
					 target               => 'disk',
					 table_file           => 1 );
	    push( @{$self->table_files}, $new_table );
	  }
	}
      }
end _read_table_files

# }}}

# {{{ add_records
start add_records
    {
      # add_records( type => 'subroutine',
      #               record_strings => ['OTHER=get_cov', 'OTHER=read'] )
      # TODO change name from record to records.

      # To read add a record, we figure out what its full class name
      # is. Then we check if we have an accessor for the record type,
      # if we do then the record is valid and we call the appropriate
      # contructor. Both record_strings an type are mandatory.

      my $rec_class = "model::problem::$type";
      my $accessor = $type.'s';
      my $n_previous_rows = 0;
      if ($type eq 'omega'){
				$n_previous_rows = $self->nomegas('with_correlations' => 0,'with_same' => 1);
      }elsif ($type eq 'sigma'){
				$n_previous_rows = $self->nsigmas('with_correlations' => 0,'with_same' => 1);
      }elsif ($type eq 'theta'){
				#this will be with priors
				$n_previous_rows = $self->record_count('record_name' => 'theta');
      }

      if( $self -> can($accessor) ){
				$self->$accessor([]) unless defined $self->$accessor;
				if (($type eq 'omega') or ($type eq 'sigma') or ($type eq 'theta')){
					push( @{$self->$accessor}, $rec_class -> new ( record_arr => \@record_strings,
								n_previous_rows => $n_previous_rows));
				} else {
					push( @{$self->$accessor}, $rec_class -> new ( record_arr => \@record_strings ));
				} 
      } else {
				croak("Trying to add unknown record: $type" );
      }
    }
end add_records
# }}} add_records

# {{{ covariance
start covariance
      {
	my @records;
	if( defined $self->covariances ) {
	  @records = @{$self->covariances} ;
	}
	if ( defined $enabled ) {
	  if ( $enabled and $#records < 0 ) {
	    $self -> add_records( type           => 'covariance',
				  record_strings => [''] );
	  } elsif ( not $enabled and $#records >= 0 ) {
	    $self -> {'covariances'} = undef;	# FIXME: Fix this with Moose
	  }
	} else {
	  if ( $#records >= 0 ) {
	    $indicator = 1;
	  } else {
	    $indicator = 0;
	  }
	}
      }
end covariance
# }}} covariance

# {{{ eigen

start eigen
      {
	my ( $print_ref, $position ) = $self -> _option_val_pos( record_name => 'covariance',
							     name        => 'PRINT' );
      }
end eigen

# }}} eigen

# {{{ header
start header
      {
	my $inp_ref = $self -> inputs;
	if ( defined $inp_ref and defined $inp_ref -> [0] ) {
	  my $input = $inp_ref -> [0];
	  my $opt_ref = $input -> options;
	  if ( defined $opt_ref ) {
	    my @options = @{$opt_ref};
	    foreach my $option ( @options ) {
	      push ( @header, [$option -> name, $option -> value] );
	    }
	  }
	}
      }
end header
# }}} header

# {{{ indexes

start indexes
  # The Indexes method returns the coordinate_string for all init_options
  # THETA1, THETA2 or OMEGA(1,1) etc or SIGMA(1,1) SIGMA(2,1)etc
  # indexes are also returned if BLOCK SAME 
      {
	my $row = 1;
	my $accessor = $parameter_type.'s';
	my $previous_size = 0;
	my @previous;
	my $done=0;

	if( defined $self->$accessor ){

	  foreach my $record ( @{$self -> $accessor} ) {
	    last if ($done);
	    last if ($record->prior() and (not $with_priors));
	    if( $record -> same() ) {
	      if( $previous_size == 0 ){
		croak("You can't have an $parameter_type ".
				"estimate defined as SAME if there is no previous estimate" );
	      }
	      #add $previous_size to all @previous rows and cols
	      my @these=();
	      foreach my $coord ( @previous ) {
		if ($coord =~ /THETA(\d+)/){
		  push( @these, 'THETA'.($1+$previous_size)); 
		  
		}elsif ($coord =~ /(OMEGA|SIGMA)\((\d+)\,(\d+)\)/ ){
		  push( @these, $1.'('.($2+$previous_size).','.($3+$previous_size).')');
		} else {
		  croak("Unknown coordinate string $coord");
		}
	      }
	      push( @indexes,@these);
	      @previous = @these;
	    }elsif ( defined $record -> options() ) {
	      if (defined $record ->size()){
		$previous_size = $record ->size();
	      }else {
		#if no size and not same then must be diagonal
		$previous_size = scalar(@{$record -> options()});
	      }
	      @previous = ();
	      foreach my $option ( @{$record -> options()} ) {
		if ($option->prior() and (not $with_priors)){
		  $done=1;
		  last;
		}
#		print "str ".$option->coordinate_string()."\n";
		push( @indexes, $option->coordinate_string() );
		push( @previous, $option->coordinate_string() );
	      }
	    }
	  }
	}
	
	if ( scalar @parameter_numbers > 0 ) {
	  my @part_indexes = ();
	  foreach my $num ( @parameter_numbers ) {
	    if ( $num < 1 or $num > scalar @indexes ) {
	      croak("$parameter_type number " . $num . " does not exist in this model::problem\n" .
			      "(" . scalar @indexes . " exists)\n" );
	    }
	    push( @part_indexes, $indexes[$num -1] );
	  }
	  @indexes = @part_indexes;
	} else {
	  carp("Model::problem -> indexes: parameter_numbers undefined, using all." );
	}
      }
end indexes

# }}} indexes

# {{{ indexes_old

start indexes_old
  # The Indexes method calculates the index for a parameter. Off-diagonal elements
  # will get a index 'i_j', where i is the row number and j is the column number
      {
	my $row = 1;
	my $accessor = $parameter_type.'s';

	if( defined $self->$accessor ){

	  # If we hit a "SAME" parameter we need to remember the
	  # previous parameter size. ( calculated as "row - previous_row" )

	  my $previous_row = 0;
	  my $previous_type = 'none';

	  foreach my $record ( @{$self -> $accessor} ) {
	    # If not a block or if the block is of size 1 use normal numbering
	    
	    if( $record -> same ) {

	      if( $previous_row == 0 ){
		croak("You can't have an $parameter_type estimate defined as SAME if it's the first estimate" );
	      }
	      my $size = $row - $previous_row;
	      $previous_row = $row;
	      if ($previous_type eq 'BLOCK'){
		#Kajsa 2008-09-09
		#Block indexing
		for ( my $i = $row; $i <= $row + $size - 1; $i++ ) {
		  for ( my $j = $row; $j <= $i; $j++ ) {
		    if ( $j == $i ) {
		      push( @indexes, "$i" );
		    } else {
		      push( @indexes, "$i".'_'."$j" );
		    }
		  }
		}
		$row += $size;
	      } else {
		for( 1..$size ){
		  push( @indexes, $row++);
		}
	      }
	      $previous_type = $record -> type;
	      
	    } elsif ( ( ! defined $record -> size ) or
		      ( $record -> size < 2 ) or
		      ( $record -> type eq 'DIAGONAL' )) { #Kajsa 2008-09-08 added, handle DIAGONAL('size')
	      if ( defined $record -> options ) {
		$previous_row = $row;
		$previous_type = $record -> type;
		foreach my $option ( @{$record -> options} ) {
		  push( @indexes, $row++ );
		}
	      }
	    } else {
	      # ... else use off-diagonal indexing where so is necessary
	      my $size = $record -> size;
	      for ( my $i = $row; $i <= $row + $size - 1; $i++ ) {
		for ( my $j = $row; $j <= $i; $j++ ) {
		  if ( $j == $i ) {
		    push( @indexes, "$i" );
		  } else {
		    push( @indexes, "$i".'_'."$j" );
		  }
		}
	      }
	      $previous_row = $row;
	      $previous_type = $record -> type;
	      $row += $size;
	    }
	  }
	  if ( scalar @parameter_numbers > 0 ) {
	    my @part_indexes = ();
	    foreach my $num ( @parameter_numbers ) {
	      if ( $num < 1 or $num > scalar @indexes ) {
		croak("$parameter_type number " . $num . " does not exist in this model::problem\n" .
				"(" . scalar @indexes . " exists)\n" );
	      }
	      push( @part_indexes, $indexes[$num -1] );
	    }
	    @indexes = @part_indexes;
	  } else {
	    carp("Model::problem -> indexes: parameter_numbers undefined, using all." );
	  }
	}
      }
end indexes_old

# }}} indexes_old


# {{{ nomegas

start nomegas


my $prev =undef;
$nomegas=0;
$self->omegas([]) unless defined $self->omegas;
foreach my $omega ( @{$self->omegas} ) {
  last if ($omega->prior() and (not $with_priors));

  my $size = $omega -> size;
  my $type = $omega -> type;
  if ($omega->same()){
    croak("First \$OMEGA cannot be SAME")
	unless (defined $prev);
    $nomegas += $prev if $with_same;
  } elsif( defined $size ) {
      
    # If the record has a size, it is of block form with diagonal of
    # length given by $size. The actual values in the model file is
    # then the arithmetic sum: (n*(n+1))/2
    
    #Kajsa: size also for diagonal matrix! Added type check below.
    
    if( $with_correlations and ($type eq 'BLOCK')){
      $nomegas += ($size*($size+1))/2; 
      $prev = ($size*($size+1))/2;
    } else {
      $nomegas += $size;
      $prev = $size;
    }
  } elsif (defined $omega->options) {
    $nomegas += scalar @{$omega -> options};
    $prev = scalar @{$omega -> options};
  } else {
    croak("Failed to parse \$OMEGA." );
  }
}


end nomegas

# }}} nomegas

# {{{ nsigmas

start nsigmas

my $prev =undef;
$self->sigmas([]) unless defined $self->sigmas;
foreach my $sigma ( @{$self->sigmas} ) {
  my $size = $sigma -> size;
  my $type = $sigma -> type;
  if ($sigma->same() and (defined $prev) ) {
    $nsigmas += $prev if $with_same;
  } elsif( defined $size ) {

    # If the record has a size, it is of block form with diagonal of
    # length given by $size. The actual values in the model file is
    # then the arithmetic sum: (n*(n+1))/2
    #Kajsa: size also for diagonal matrix! Added check below.

    if( $with_correlations and ($type eq 'BLOCK')){
      $nsigmas += ($size*($size+1))/2;
      $prev = ($size*($size+1))/2;
    } else {
      $nsigmas += $size;
      $prev = $size;
    }
  } elsif (defined $sigma->options) {
    $nsigmas += scalar @{$sigma -> options};
    $prev = scalar @{$sigma -> options};
  } else {
    croak("Failed to parse \$SIGMA." );
  }
}

end nsigmas

# }}} nsigmas


# {{{ record_count
start record_count
    {
	$return_value = 0;
	my $accessor = $record_name . 's';
	if( defined $self->$accessor ){
	    foreach my $record ( @{$self->$accessor} ){
		if( defined $record -> options ){
		    $return_value += @{$record -> options};
		}
	    }
	}
    }
end record_count
# }}} record_count

# {{{ restore_inits
start restore_inits
      {
	if ( defined $self->thetas ) {
	  foreach my $theta ( @{$self->thetas} ) {
	    $theta -> restore_inits;
	  }
	}
	if ( defined $self->omegas ) {
	  foreach my $omega ( @{$self->omegas} ) {
	    $omega -> restore_inits;
	  }
	}
	if ( defined $self->sigmas ) {
	  foreach my $sigma ( @{$self->sigmas} ) {
	    $sigma -> restore_inits;
	  }
	}
      }
end restore_inits
# }}} restore_inits

# {{{ set_random_inits
start set_random_inits
      {
	if ( defined $self->thetas ) {
	  foreach my $theta ( @{$self->thetas} ) {
	    $theta -> set_random_inits( degree => $degree );
	  }
	}
	if ( defined $self->omegas ) {
	  foreach my $omega ( @{$self->omegas} ) {
	    $omega -> set_random_inits( degree => $degree );
	  }
	}
	if ( defined $self->sigmas ) {
	  foreach my $sigma ( @{$self->sigmas} ) {
	    $sigma -> set_random_inits( degree => $degree );
	  }
	}

	$self->ensure_diagonal_dominance();
      }
end set_random_inits
# }}} set_random_inits

# {{{ ensure_diagonal_dominance
start ensure_diagonal_dominance
      {

	#check here that omega and sigma strictly diagonally dominant
	#otherwise decrease magnitude of diagonal elements 
	# by enough to achieve strict diagonal dominance
	# do row by row
	#check number of diagonal elements
	#make array of zeros for off_diagonal_sums, one per diagonal value
	#go through records
	#check that block, and not fixed or same
	#need only check values wihtin block, row sums not affected by anything outside
	#go through all options, if on-diagonal then skip. get row, col indexes
	#add value  to sum for diagonal(row) and diagonal (col)
	#loop through options again, if not on-diagonal, skip.
	#if on-diagonal, check that value strictly greater than sum.
	  #otherwise compute deflation factor

	#dont touch priors
	foreach my $param ('omega','sigma') {
	  my $adjusted = 0;
	  my $accessor = $param.'s';
	  my $size_accessor = 'n'.$param.'s';
	  my @records;
	  if (defined $self -> $accessor()) {
	    @records = @{$self -> $accessor()};
	  }
	  next unless (scalar(@records) > 0); #no parameter in this problem
	  my $size = $self->$size_accessor('with_correlations' => 0,'with_same' => 1);
	  my @off_diagonal_sum = 0 x $size; 
	  foreach my $record (@records){
	    next unless ($record -> type() eq 'BLOCK');
	    if  ($record->same() or $record->fix() or $record->prior()){
	      next;
	    }
	    next if ($record->size() < 2);
	    unless (defined $record -> options()){
	      croak("$param record has no values");
	    }
	    foreach my $option (@{$record -> options()}) {
	      next if ($option->on_diagonal());
	      my $name = $option -> coordinate_string();
	      croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
	      croak("row in $name outside size $size") if ($1 > $size );
	      croak("col in $name outside size $size") if ($2 > $size );
	      my $val = abs($option ->init());
	      $off_diagonal_sum[($1-1)] += $val;
	      $off_diagonal_sum[($2-1)] += $val;
	    }
	    my %adjust_row = {};
	    foreach my $option (@{$record -> options()}) {
	      next unless ($option->on_diagonal());
	      my $name = $option -> coordinate_string();
	      croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
	      croak("row in $name outside size $size") if ($1 > $size );
	      croak("col and row in $name not diagonal element") unless ($2 == $1 );
	      my $val = $option ->init();
	      unless ($val > $off_diagonal_sum[($1-1)] ){
		  my $ratio = $val/$off_diagonal_sum[($1-1)]; # less than 1, larger than 0 (abs sum, pos diag)
		  $adjust_row{$1} = $ratio*(0.99);
		  #skip inflation
		  if (0){
		      my $new_val = 1.05*$off_diagonal_sum[($1-1)]; #five percent larger than sum
		      $option -> check_and_set_init( new_value => $new_val );
		  }
	      }
	    }
	    #new loop here to decrease off-diag
	    if (1){
		foreach my $option (@{$record -> options()}) {
		    next if ($option->on_diagonal());
		    my $name = $option -> coordinate_string();
		    croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
		    croak("row in $name outside size $size") if ($1 > $size );
		    croak("col in $name outside size $size") if ($2 > $size );
		    my $deflate = 1;
		    foreach my $row (keys %adjust_row){
			if ($row == $1 or $row == $2){
			    $deflate = $adjust_row{$row} if ($adjust_row{$row} < $deflate);
			}
		    }
		    next unless ($deflate < 1);
		    my $val = $option ->init();
		    my $value = $val*$deflate;
		    if ($value < 1 and $value > 0){
			$value = sprintf "%.5f", $value; #need to control so dont get e notation
			$value     = '0' if eval($value) == 0;
		    }elsif ($value > -1 and $value < 0){
			$value = sprintf "%.4f", $value; #need to control so dont get e notation
			$value     = '0' if eval($value) == 0;
		    }else{
			$value = sprintf "%6.2f", $value; #need to control so dont get e notation
			my ($big,$small) = split('\.',$value);
			$small           = substr($small,0,3);
			if ((length($big)+ length($small)) > 7){
			    $value = $big;
			}else{
			    $value     = $big.'.'.$small;
			}
			$value     = '0' if eval($value) == 0;
		    }
		    
		    $adjusted = 1;
		    $option -> check_and_set_init( new_value => $value );
		    
		}
	    }
	  }
	  if ($adjusted and $verbose){
	      print "Decreased off-diagonal values of $param from input to ensure strict diagonal dominance in output model.\n";
	  }
	}
      }
end ensure_diagonal_dominance
# }}} ensure_diagonal_dominance


# {{{ set_records

start set_records
      {
	  my $rec_class = "model::problem::$type";
	  my $accessor = $type.'s';
	  if( $self -> can($accessor) ){
	    $self->$accessor([$rec_class -> new ( record_arr => \@record_strings) ]);
	  } else {
	      die "Error in problem -> set_records: Trying to set unknown record: $type\n";
	  }
      }
end set_records

# }}} set_records
    
# {{{ remove_records
start remove_records
      {
	  my $rec_class = "model::problem::$type";
	  my $accessor = $type.'s';
	  if( $self -> can($accessor) ){
	    if ($keep_last){
	      my @recs;
	      my $last_rec = undef;
	      @recs = @{$self -> $accessor} if (defined $self -> $accessor); 
	      $last_rec = $recs[-1] if (defined $recs[-1]);
	      $self -> $accessor([$last_rec]);
	    }else {
	      $self -> {$accessor} = undef;		# FIXME: Fix this with Moose
	    }
	  } else {
	      die "Error in problem -> remove_records: Trying to remove unknown record: $type\n";
	  }
      }
end remove_records
# }}} remove_records

# {{{ add_option

start add_option

my $accessor = $record_name.'s';
unless( $self -> can($accessor) ){
  croak("Unknown record name: $record_name" );
}
if( defined $self->$accessor ) {
  my @records = @{$self->$accessor};
  my @record_numbers;

  if ($record_number > 0){
    push(@record_numbers,$record_number);
  }elsif ($record_number == 0){
    @record_numbers = 1 .. scalar(@records);
  }elsif ($record_number == -1){
    #last
    push(@record_numbers,scalar(@records));
  }else {
    croak("illegal input record_number $record_number to add_option");
  }

  foreach my $recnum ( @record_numbers ) {
    #numbering starts at 1, unlike array indices
    $records[$recnum-1]-> add_option( init_data => { name  => $option_name,
						     value => $option_value } );
  }
} else {
  if( $add_record ) {
    $self -> add_records( type           => $record_name,
			  record_strings => ["$option_name=$option_value"] );
  } else {
    carp("No records of type $accessor and add_option ".
		      "set not to add one" );
  }
}

end add_option

# }}} add_option

# {{{ remove_option

start remove_option

my $accessor = $record_name.'s';
unless( $self -> can($accessor) ){
  croak("Unknown record name: $record_name" );
}
if( defined $self->$accessor ) {
  my @records = @{$self->$accessor};
  my @record_numbers;
  if ($record_number > 0){
    push(@record_numbers,$record_number);
  }elsif ($record_number == 0){
    @record_numbers = 1 .. scalar(@records);
  }elsif ($record_number == -1){
    #last
    push(@record_numbers,scalar(@records));
  }else {
    croak("illegal input record_number $record_number to remove_option");
  }

  foreach my $recnum ( @record_numbers ) {
    #numbering starts at 1, unlike array indices
    $records[$recnum-1] -> remove_option( name => $option_name,
					  fuzzy_match => $fuzzy_match );
  }
} else {
  carp("No records of type $accessor" );
}

end remove_option

# }}} remove_option

# {{{ store_inits
start store_inits
      {
	if ( defined $self->thetas ) {
	  foreach my $theta ( @{$self->thetas} ) {
	    $theta -> store_inits;
	  }
	}
	if ( defined $self->omegas ) {
	  foreach my $omega ( @{$self->omegas} ) {
	    $omega -> store_inits;
	  }
	}
	if ( defined $self->sigmas ) {
	  foreach my $sigma ( @{$self->sigmas} ) {
	    $sigma -> store_inits;
	  }
	}
      }
end store_inits
# }}} store_inits

# {{{ _format_problem

start _format_problem
      {
	# problem::_format_problem()

	# format_problem will return an array of strings of the
	# problem in NONMEM modelfile format.

	# Loop over the print_order array that contains strings of
	# valid record types in the order they should appear in a
	# NONMEM modelfile. So if the order of some records are
	# interchangable and the file from which the object was
	# initialized has records in an order different from
	# print_order, the file will still be valid, but will look
	# different from what it used to.
	my $record_order = \@print_order;
	if ($self->sde){
	  $record_order = \@sde_print_order;
	}elsif ($self->omega_before_pk){
	  $record_order = \@print_order_omega_before_pk;
	}
	foreach my $type ( @${record_order} ) {
	  # Create an accessor string for the record being formatted
	  my $accessor = $type.'s';

	  # Se if we have one or more records of the type given in
	  # print_order
	  if ( defined $self->$accessor ) {
	    # Loop over all such records and call on the record object
	    # to format itself.

	    foreach my $record ( @{$self->$accessor} ){
	      push( @formatted,
		    @{$record ->
			  _format_record( number_format => $number_format,
					  nonparametric_code => $self->nonparametric_code,
					  shrinkage_code     => $self->shrinkage_code,
					  eigen_value_code   => $self->eigen_value_code ) } );
	    }
	  }
	  if( $self->shrinkage_module -> enabled and $type eq 'table' ) {
	    push( @formatted,
		  @{$self->shrinkage_module -> format_shrinkage_tables } );
	  }	
	}

	if( $self->cwres_modules ){
	  $self->cwres_modules -> [0] -> post_process;
	}
	
      }
end _format_problem

# }}} _format_problem

# {{{ _init_attr

start _init_attr
      {
	# Private method, should preferably not be used outside model.pm
	# The add_if_absent argument tells the method to add an init (theta,omega,sigma)
	# if the parameter number points to a non-existing parameter with parameter number
	# one higher than the highest presently included. Only applicatble if
	# new_values are set. Default value = 0;
	
	my $accessor = $parameter_type.'s';
	unless( $self -> can($accessor) ){
	    croak("problem -> _init_attr: Error unknown parameter type: $parameter_type" );
	}

	my @records;
	if( defined $self->$accessor ){
	  @records = @{$self->$accessor};
	} else {
	  @records = ();
	}
	
	my @options = ();

	# {{{ Check that the size of parameter_numbers and new_values match
	my %num_val;
	if ( $#parameter_numbers >= 0 and $#new_values >= 0 ) {
	  if ( $#parameter_numbers == $#new_values ) {
	    for ( my $i = 0; $i <= $#new_values; $i++ ) {
	      $num_val{$parameter_numbers[$i]} = $new_values[$i];
	    }
	  } else {
	    die "Model::problem -> _init_attr: The number of specified ".
	      "parameters (@parameter_numbers) and values (@new_values) do not match for parameter $parameter_type".
		" and attribute $attribute\n";
	  }
	}
	# }}}

	my $prev_size = 1;
	if ( scalar @new_values > 0 ) {
	  # {{{ Update values

	  # OBS! We are using 'normal' numbering in parameter_numbers, i.e. they begin
	  # at one (1).
	  my $opt_num = 1;
	  # Ugly solution to add non-existing options:
	  my %found;
	  foreach my $num ( @parameter_numbers) {
#	    print "inpn: $num\n";
	    $found{$num} = 0;
	  }

	  my @diagnostics = ();
	  foreach my $record ( @records ) {
	    if ( $record -> same() ) {
	      # SAME == true: Nothing to be done. Just move forward to next $OMEGA but
	      # increase counter first

	      $opt_num += $prev_size;
	    } else {
	      foreach my $option ( @{$record -> options} ) {
		if ( scalar @parameter_numbers > 0 ) {
		  foreach my $num ( @parameter_numbers ) {
		    if ( $num == $opt_num ) {
		      $found{$num}++;
		      if ( $attribute eq 'init' ) {
			push( @diagnostics,
			      $option -> check_and_set_init( new_value => $num_val{$num} ) );
		      } elsif( $attribute eq 'fix' and defined $record -> size() 
			       and ($record-> type() eq 'BLOCK') ){
			# size() tells us this MIGHT be a block and we must fix on record level.
			#check type also
			$record -> fix( $num_val{$num} );
		      } else {
			$option -> $attribute( $num_val{$num} );
		      }
		    }
		  }
		} else {
		  if ( $attribute eq 'init' ) {
		    push( @diagnostics,
			  $option -> check_and_set_init( new_value => shift( @new_values ) ) );
		  } elsif( $attribute eq 'fix' and defined $record -> size()
			   and ($record-> type() eq 'BLOCK')){
		    # size() tells us this MIGHT a block and we must fix on record level.Check type also
		    $record -> fix( shift( @new_values ) );
		  } else {
		    $option -> $attribute( shift( @new_values ) );
		  }
		}
		$opt_num++;
	      }
	      if( $parameter_type eq 'theta' ){
		$prev_size = scalar @{$record -> options};
	      } else {
		my $size = $record -> size;
		if( (defined $size) and ($record->type eq 'BLOCK') ) {
		  $prev_size = ($size*($size+1))/2;
		} else {
		  $prev_size = scalar @{$record -> options};
		}
	      }
	    }
	  }
	  # If $add_if_absent is set, any parameters that were not found above are
	  # added below:
	  
	  my @nums = sort {$a<=>$b} keys %found;
	  my $new_record = "model::problem::$parameter_type" -> new();
	  my $do_add_record;
	  my $added_thetas=1;
	  my $added_sigmas=1;
	  my $added_omegas=1;
	  foreach my $num ( @nums ) {
	    if ( $add_if_absent and
		 not $found{$num} ) {
	      $do_add_record = 1;
	      unless($num == $opt_num) {
		croak("Attempt to add a parameter with higher number ($num) than the number\n".
				           "of parameters + 1 ($opt_num)\n" );
	      }
	      # Get the last record of $parameter_type
	      # my $new_record = $records[$#records];
	      my $option_class;

	      my $coordinate_string;
	      if( $parameter_type eq 'theta' ){
		$option_class = 'model::problem::record::theta_option';
		my $index = $self->record_count('record_name' => 'theta')+$added_thetas;
		$coordinate_string='THETA'.$index;
		$added_thetas++;
	      } else {
		$option_class = 'model::problem::record::init_option';
		if( $parameter_type eq 'omega' ){
		  my $index = $self->nomegas('with_correlations' => 0,'with_same' => 1)+$added_omegas;
		  $coordinate_string='OMEGA('.$index.','.$index.')';
		  $added_omegas++;
		}else {
 		  my $index = $self->sigmas('with_correlations' => 0,'with_same' => 1)+$added_sigmas;
		  $coordinate_string='SIGMA('.$index.','.$index.')';
		  $added_sigmas++;
		}
	      }

	      # Push a new option to this last record
	      my $option = $option_class -> new(coordinate_string => $coordinate_string);
	      if ( $attribute eq 'init' ) {
		$option -> check_and_set_init( new_value => $num_val{$num} );
	      } elsif( $attribute eq 'fix' and defined $new_record -> size()
		       and ($new_record-> type() eq 'BLOCK')){

		# size() tells us this is MIGHT be a block and we must fix on
		# record level. This will never happen, as we can't
		# add BLOCKS, at least not like this.

		$new_record -> fix( $num_val{$num} );
	      } else {
		$option -> $attribute( $num_val{$num} );
	      }
	      $new_record->options([]) unless (defined $new_record->options());
	      push( @{$new_record->options}, $option );

	      # So we've added a parameter. Possible to add more,
	      # lets increase the highest found:
	      $opt_num++;
	    }
	  }
	  if ( $attribute eq 'init' ) {
	    # We're updating but might be returning diagnostics
	    # Use the default return parameter parameter_values for this
	    @parameter_values = @diagnostics;
	  }

	  if( $do_add_record ){
	    push( @records, $new_record );
	    $self->$accessor(\@records);
	  }

	  # }}} Update values
	} else {
	  # {{{ Retrieve values

	  my @prev_values = ();
	  my $done=0;
	  foreach my $record ( @records ) {
	    last if ($done);
	    last if ($record->prior() and (not $with_priors));
	    unless ( $record -> same() ) {
	      @prev_values = ();
	      if ( defined $record -> options ) {
		foreach my $option ( @{$record -> options} ) {
		  if ($option->prior() and (not $with_priors)){
		    $done=1;
		    last;
		  }
		  push( @prev_values, $option -> $attribute );
		}
	      } else {
		carp("Trying to get attribute $attribute, ".
				 "but no options defined in record ".ref($record) );
	      }
	      $prev_size = $record -> size unless ( $record -> same );
	    }
	    if( $record -> same() and (not $get_same)) {
	      for( my $i = 0; $i <= $#prev_values; $i++ ) {
		$prev_values[$i] = undef;
	      }
	    }
	    push( @parameter_values, @prev_values );
	  }
	  
	  if ( scalar @parameter_numbers > 0 ) {
	    my @part_vals = ();
	    foreach my $num ( @parameter_numbers ) {
	      push( @part_vals, $parameter_values[$num -1] );
	    }
	    @parameter_values = @part_vals;
	  } else {
	    carp("Model::problem -> _init_attr: parameter_numbers undefined, using all." );
	  }
	  
	  # }}} Retrieve values
	}
      }
end _init_attr

# }}} _init_attr

# {{{ name_val

start name_val
end name_val

# }}} name_val

# {{{ _normalize_record_name

start _normalize_record_name
    {

      # This code takes a recordname (which likely is uppercase and
      # semilong), creates its short uppercase format and looks up the
      # long, lowercase, name in the abbreviations hash that was
      # initialized in "new". The name is assumed to be valid, if its
      # not, an empty string will be returned, but no error produced (
      # a warning might be nice though ) (Errorhandling is now done in
      # "read_records".

	if ($unsupported_records{uc($record_name)} > 0){
	    debug->die(message => "\nPsN does not yet support record \$".$record_name." in the control stream, but adding support is on the todo-list.\n");
	}
      my $uc_short_type = substr(uc($record_name),0,3);
      if ($uc_short_type eq 'AES'){
	  if (length($record_name)>3){
	      #this must be aesinitial
	      $uc_short_type = $uc_short_type.'I';
	  }else{
	      #must be aes
	      $uc_short_type = $uc_short_type.' ' ;
	  }
      }
      $normalized_name = $abbreviations{$uc_short_type};
      unless (length($normalized_name)>0){
	  debug->die(message => "\nPsN does not support record \$".$record_name." in the control stream\n");

      }

#      print "normal $normalized_name short $uc_short_type inputname $record_name\n";
    }
end _normalize_record_name

# }}}

# {{{ _read_records

start _read_records
    {

      # We parse the lines of a problem by looping over the them and
      # look for records(lines starting with a $). When a record is
      # found we set its index in the array as the end of the previous
      # record we found. We then know which lines to send to the
      # record object constructor. Then we set the end index of the
      # previous record as the start index of the next record. It is
      # assumed that the first record starts at line zero. The end of
      # the last record is the last line.

      my $start_index = 0;
      my $record_index = 0;
      my $end_index;
      my $first = 1;

      # It may look like the loop takes one step to much, but its a
      # trick that helps parsing the last record.
			$self->prob_arr([]) unless defined $self->prob_arr;
      for( my $i = 0; $i <= @{$self->prob_arr}; $i++ ){

	# This if statement makes sure we dont access the array in the
	# last iteration of the loop. In all other iterations we need
	# a line of code to look for records starting lines.

	if( $i <= $#{$self->prob_arr} ){
	  $_ = $self->prob_arr -> [$i];
	}

	# In this if statement we use the lazy evaluation of logical
	# or to make sure we only execute search pattern when we have
	# a line to search. Which is all cases but the very last loop
	# iteration.

	if( $i > $#{$self->prob_arr} or /^\s*\$(\w+)/ ){
	  $end_index = $i;

	  # The if statement here is only necessary in the first loop
	  # iteration. When start_index == end_index == 0 we want to
	  # skip to the next iteration looking for the actual end of
	  # the first record.

	  if( $end_index > $start_index and not $first){
	    # extract lines of code:
	    my @record_lines = @{$self->prob_arr}[$start_index .. $end_index-1];
	    # extract the record name and get its long name:
	    $self->prob_arr -> [$record_index] =~ /^\s*\$(\w+)/;
	    my $record_name = $1;
	    my $record_type = $self -> _normalize_record_name( record_name => $record_name );
	    
	    unless( length($record_type) > 0 ){
	      croak("Record $record_name is not valid" );
	    }

	    # reset the search for records by moving the record start
	    # forwards:
	    $start_index = $i;
	    
	    # let add_records create the object if appropriate

	    if( $record_type eq 'warnings' ) {
	      print "\nWarning: Record \$WARNINGS is deleted by PsN.\n";
	    }elsif( $record_type eq 'finedata' ) {
	      print "\nWarning: Record \$FINEDATA is deleted by PsN.\n";
	    } elsif( $record_type eq 'table' ) {
	      my $et_found = 0;
	      my $wr_found = 0;
	      if (defined $self->shrinkage_module){
		  my $eta_name  = $self->shrinkage_module -> eta_tablename;
		  my $wres_name = $self->shrinkage_module -> wres_tablename;
		  foreach my $row ( @record_lines ) {
		      $et_found++ if( $row =~ /$eta_name/ );
		      $wr_found++ if( $row =~ /$wres_name/ );
		  }
	      }
	      if( $et_found or $wr_found ) {
		$self->shrinkage_module -> enable;
	      } else {
		$self -> add_records( record_strings => \@record_lines, 
				      type => $record_type );
	      }
	    } else {
	      $self -> add_records( record_strings => \@record_lines, 
				    type => $record_type );
	    }
	  }
	  $first = 0;
	  $record_index = $i;
	}  
      }
    }
end _read_records

# }}} _read_records

start get_full_omega
{
    #create one big full omega block as new_omega->[$row][$col]
    #input is optional covmatrix to be used for 0 off-diags
    #if old off-diagonals not present then set small values to ensure strict diagonal dominance
    my $size = $self->nomegas(with_correlations => 0, with_same => 1);

    for (my $i=0; $i < $size; $i++){
	push(@new_omega,[0 x $size]);
    }
    

    ##Determine minimum difference between off-diagonal absolute sum and diagonal,
    #needed to determine appropriate values to fill in
    #at the same time store current values in matrix @new_omega
    my $minimum_difference;
    my @records;
    if (defined $self -> omegas()) {
	@records = @{$self -> omegas()};
    }
    my @off_diagonal_sum = 0 x $size; 
    my @diagonal_value = 0 x $size; 
    my $block_size;
    my $prev_rows=0;
    foreach my $record (@records){
	if  ($record->same() ){
	    #store values in new_omega
	    my $old_start = $prev_rows - $block_size;
	    for (my $row=0; $row< $block_size; $row++){
		for (my $col=0; $col<=$row; $col++){
		    my $value = $new_omega[$old_start+$row][$old_start+$col];
		    $new_omega[$prev_rows+$row][$prev_rows+$col] = $value;
		}
	    }
	    next; #need not compute sums if same
	}
	unless (defined $record -> options()){
	    croak("OMEGA record has no values");
	}

	if ($record -> type() eq 'BLOCK'){
	    $block_size = $record->size();
	}

	foreach my $option (@{$record -> options()}) {
	    my $name = $option -> coordinate_string();
	    croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
	    croak("row in $name outside size $size") if ($1 > $size );
	    croak("col in $name outside size $size") if ($2 > $size );
	    my $value = $option ->init();
	    $new_omega[($1-1)][($2-1)] = $value;
	    my $val = abs($value);
	    if ($option->on_diagonal()){
		croak("col and row in $name not diagonal element") unless ($2 == $1 );
		$diagonal_value[($1-1)] = $val;
		$prev_rows++;
	    }else{
		$off_diagonal_sum[($1-1)] += $val;
		$off_diagonal_sum[($2-1)] += $val;
	    }
	}
    }

    $minimum_difference = $diagonal_value[0]-$off_diagonal_sum[0];
    for (my $i=1; $i<$size; $i++){
	my $diff = $diagonal_value[$i]-$off_diagonal_sum[$i];
	$minimum_difference = $diff if ($diff< $minimum_difference and ($diff>0));
    }

    my $max_off_diagonal = 0.01; #check Ron's hands on for typical value here
    my $temp = ($minimum_difference/($size-1));
    $max_off_diagonal = $temp*(0.9) if ($temp < $max_off_diagonal);
    #print "max off diag is $max_off_diagonal\n";
    #fill off-diagonals in new_omega
    my $k=1;
    for (my $row=0; $row< $size; $row++){
	for (my $col=0; $col<$row; $col++){
	    if ($new_omega[$row][$col] == 0){
		if (defined $covmatrix){
		    $new_omega[$row][$col] = $covmatrix->[$row][$col];
		}else{
		    $new_omega[$row][$col] = ($max_off_diagonal - 0.0001*($k % 10));
		    $k++;
		}
	    }
	}
    }

    if (0){
	print "prob: printing new_omega\n";
	for (my $row=0; $row< $size; $row++){
	    for (my $col=0; $col<$size; $col++){
		printf("  %.4f",$new_omega[$row][$col]); #matlab format
	    }
	    print "\n";
	}
	print "\n";
    }
}
end get_full_omega

start add_omega_block
{
    #input is $new_omega
    #
    # add new BLOCK(size)

    my $size = scalar(@{$new_omega});
    return if ($size < 1);
    my @record_lines=();
    push(@record_lines,'BLOCK('.$size.') ');
    for (my $row=0; $row< $size; $row++){
	my $line;
	for (my $col=0; $col<=$row; $col++){
	    my $str= sprintf("  %.6f",$new_omega->[$row][$col]);
	    $line = $line.' '.$str;
	}
	my $comment ='';
	$comment = '; '.$labels->[$row] if (defined $labels and scalar(@{$labels}) > $row);
	push(@record_lines,$line.$comment);
    }
    $self -> add_records( record_strings => \@record_lines, 
			  type => 'omega' );

}
end add_omega_block

# {{{ _option_val_pos

start _option_val_pos
      {
	#
	# _option_val_pos( record_name => 'subroutine',
	#                  name => 'OTHER',
	#                  val => 'get_cov')
	#

	# _option_val_pos sets, or gets, the value of an option (given
	# as the 'name' parameter. Name must be uppercase) in a record
	# (given as the 'record_name' parameter. Record name should be
	# the record class name in the model diagram.)
	
	my $accessor = $record_name.'s';
	unless( $self -> can($accessor) ){
	  croak("Unknown record name: $record_name" );
	}

	my @records;
	if( defined $self->$accessor ) {
	  @records = @{$self->$accessor} ;
	} else {
	  carp("No records of type $accessor" );
	  @records = ();
	}
	my @options = ();

	# {{{ Check that the size of instance_numbers and new_values match

	my %num_val;
	if ( $#instance_numbers >= 0 and $#new_values >= 0 ) {
	  if ( $#instance_numbers == $#new_values ) {
	    for ( my $i = 0; $i <= $#new_values; $i++ ) {
	      $num_val{$instance_numbers[$i]} = $new_values[$i];
	    }
	  } else {
	    croak("Model::problem -> _option_val_pos: The number of specified " .
			    "parameters " . $#instance_numbers+1 . " and values " .
			    $#new_values+1 . " do not match" );
	  }
	}

	# }}}

	if ( scalar @new_values > 0 ) {
	  # {{{ Update values

	  my $opt_num = 1;
	  foreach my $record ( @records ) {
	    foreach my $option ( @{$record -> options} ) {
	      my $test_name = $exact_match ? uc($option -> name) :
		  uc(substr($option -> name,0,length($name)));
	      if ( $test_name eq $name) {
		if ( scalar @instance_numbers > 0 ) {
		  foreach my $num ( @instance_numbers ) {
		    $option -> value( $num_val{$num} ) if $num == $opt_num;
		  }
		} else {
		  $option -> value( shift( @new_values ) );
		}
		$opt_num++;
	      }
	    }
	  }

	  # }}} Update values
	} else {
	  # {{{ Retrieve values

	  foreach my $record ( @records ) {
	    my $i = 1;
	    if ( defined $record -> options ) {
	      foreach my $option ( @{$record -> options} ) {
		my $test_name = $exact_match ? uc($option -> name) :
		  uc(substr($option -> name,0,length($name)));
		if ( $test_name eq $name) {
		  push( @values, $option -> value );
		  push( @positions, $i );
		}
		$i++;
	      }
	    }
	  }
	  if ( $#instance_numbers > 0 ) {
	    my @part_vals = ();
	    my @part_pos = ();
	    foreach my $num ( @instance_numbers ) {
	      push( @part_vals, $values[$num -1] );
	      push( @part_pos, $positions[$num -1] );
	    }
	    @values = @part_vals;
	    @positions = @part_pos;
	  } 

	  # }}} Retrieve values
	}
      }	
end _option_val_pos

# }}} _option_val_pos

# {{{ eta_shrinkage

start eta_shrinkage

@eta_shrinkage = @{$self->shrinkage_module -> eta_shrinkage( model => $model, 
								   probnum => $probnum,
								   directory => $directory,
								   eta_filename => $eta_filename) };

end eta_shrinkage

# }}} eta_shrinkage

# {{{ iwres_shrinkage

start iwres_shrinkage

    @iwres_shrinkage = @{$self->shrinkage_module -> iwres_shrinkage( model => $model, 
									   probnum => $probnum,
									   directory => $directory,
									   iwres_filename => $iwres_filename)};

end iwres_shrinkage

# }}} iwres_shrinkage
