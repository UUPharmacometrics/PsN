package tool::pind;

use include_modules;
use Data::Dumper;
use Math::Random;
use strict;
use tool::modelfit;
use model;
use ui;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'lst_file' => ( is => 'rw', required => 1, isa => 'Str' );
has 'njd' => ( is => 'rw', isa => 'Str' );
has 'ignore_individuals_above' => ( is => 'rw', isa => 'Int' );
has 'ind_param' => ( is => 'rw', isa => 'Str', default => 'eta' );
has 'n_individuals' => ( is => 'rw', isa => 'Int' );
has 'tablename' => ( is => 'rw', isa => 'Str', default => 'nptab' );
has 'modelname' => ( is => 'rw', isa => 'Str', default => 'np' );
has 'jd_model' => ( is => 'rw', isa => 'model' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['pind.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'pind_results.csv' );


sub BUILD
{
	my $self  = shift;

	if ( scalar (@{$self->models->[0]->problems}) != 1 ) {
		croak('The input model must contain exactly one problem.');
	}

	if ($self->ind_param eq 'eta') {
		my $record_ref = $self->models->[0]->record(record_name => 'omega' );
		unless ( scalar(@{$record_ref}) > 0 ){ 
			croak('The input model must contain at least one $OMEGA record.');
		}
	} elsif ($self->ind_param eq 'theta') {
		my $record_ref = $self->models->[0]->record(record_name => 'theta' );
		unless ( scalar(@{$record_ref}) > 0 ){ 
			croak('The input model must contain at least one $THETA record.');
		}
	} else {
		croak('Illegal choice for option -ind_param.');
	}

	unless ($self->lst_file eq '0') {
		#allow setting file string to 0. In this case no updating of initial estimates will be done
		#used when calling pind from nonp_bootstrap
		unless ( -e $self->lst_file ) {
			croak("Cannot find lst-file " . $self->lst_file);
		}
	}

	unless (length($self->tablename) > 0) {
		croak("Length of string tablename is 0.");
	}

	if (defined $self->ignore_individuals_above) {
		unless (defined $self->njd) {
			croak("Option -ignore_individuals_above only allowed toghether with -njd");
		}
	}
}

sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	if (defined $self->njd) {
		unless (defined $self->ignore_individuals_above) {
			$self->ignore_individuals_above(100);
		}
	}
	#this is sec 1 with 1.1-1.8 in URS individual probability script
	#1.0
	my $filename = $self->modelname . '.mod';
	my $copy = $self->models->[0]->copy(filename => $filename, 
										directory => 'm1',
										write_copy => 0,
										copy_output => 0,
										copy_datafile =>0);
	
  if (defined $self->models->[0]->extra_files()) {
    my @extra_files;
    foreach my $extra (@{$self->models->[0]->extra_files()}) {
      push (@extra_files,$extra);
    }
    $copy->extra_files(\@extra_files);
  }

  my $number_of_etas = $copy -> nomegas -> [0];
  my $number_of_thetas = $copy -> nthetas;

  unless ($number_of_etas > 0){
    croak("Error modelfilt_setup: must be at least one omega/eta in modelfile");
  }
  
  #1.1 update initial values in model and FIX them. Important to FIX after updating,
  #because PsN does not update fixed values

  unless ($self->lst_file eq '0') {
    #possibility to set to 0 can be used when calling pind from nonp_bootstrap
    #then inits are updated from output object instead of lst-file 
    #create output object to check that can be parsed correctly
    my $outputObject = output -> new(filename => $self->lst_file);
    unless ($outputObject->parsed_successfully()){
      croak("lst file " . $self->lst_file . " could not be parsed.");
    }
    $copy -> update_inits ( from_output => $outputObject);
  }

  $copy -> fixed( parameter_type => 'theta',
		  new_values => [[(1) x $number_of_thetas ]] );
  $copy -> fixed( parameter_type => 'omega',
		  new_values => [[(1) x $number_of_etas ]] );
  $copy -> fixed( parameter_type => 'sigma',
		  new_values => [[(1) x $copy -> nsigmas -> [0] ]] );

  #1.2 set MAXEVALS=0
  my $record_ref = $copy -> record(record_name => 'estimation' );
  if ( scalar(@{$record_ref}) > 0 ){ 
    $copy -> set_option(record_name => 'estimation',
			option_name => 'MAXEVALS',
			fuzzy_match => 1,
			option_value => '0');
  } else {
    $copy -> add_records(type => 'estimation',
			 record_strings => ['MAXEVAL=0'] );
  }
  
  #1.3 add $NONPARAMETIC UNCONDITIONAL

  my $record_ref = $copy -> record(record_name => 'nonparametric' );
  if ( scalar(@{$record_ref}) > 0 ){ 
    $copy -> set_option(record_name => 'nonparametric',
			option_name => 'UNCONDITIONAL',
			fuzzy_match => 1);
  } else {
    $copy -> add_records(type => 'nonparametric',
			 record_strings => ['UNCONDITIONAL'] );
  }

  my $code_block;
  my $use_pk = 0;
  
  $record_ref = $copy -> record(record_name => 'pk' );
  if ( scalar(@{$record_ref}) > 0 ){ 
    $code_block = $copy -> pk;
    $use_pk = 1;
  } else {
    $record_ref = $copy -> record(record_name => 'pred' );
    if ( scalar(@{$record_ref}) > 0 ){ 
      $code_block = $copy -> pred;
    } else {
      croak("Error: Neither \$PK nor \$PRED found in modelfile.");
    }
  }
  push( @{$code_block},'   JD = DEN_');

  my $n_parameters = ($self->ind_param eq 'eta') ? $number_of_etas : $number_of_thetas;
  
  for( 1..$n_parameters ){
    push(@{$code_block},"   DN$_=CDEN_($_)" );
  }
  
  if( $use_pk ){
    $copy -> pk( new_pk => $code_block );
  } else {
    $copy -> pred( new_pred => $code_block );
  }

  #1.6
  $copy -> remove_records( type => 'covariance' );
  
  #1.7
  $copy -> remove_records( type => 'table' );

  #Substitute for 1.5: O
  #Output ETAs and JD with higher precision. 
  #Copied from cwres_module_subs.pm and modified

  # Figure out wheter we have an 'ADVAN' option. By not using
  # "exact_match" we can search for a prefix of the different ADVAN
  # options.

  my ($advan,$junk) = $copy -> _option_val_pos( record_name => 'subroutine',
						name => 'ADVAN',
						exact_match => 0);
  my $have_advan = scalar(@{$advan}) > 0;
  my $code;
  my $problem = $copy -> problems ->[0];

  if( $have_advan ){
    unless( $problem -> infns ){
      $problem -> add_records( type => 'infn',
			    record_strings => [] );
    }
    $problem = $problem -> infns -> [0] -> code;
  } else {
    $problem = $problem -> preds -> [0] -> code;
  }

  my $eta_header='ETA1';
  for( 2..$n_parameters ){
    $eta_header = $eta_header . " ETA$_";
  }

  push( @{$problem}, 
	'IF (ICALL.EQ.3) THEN',
	'  OPEN(50,FILE=\'jdtab.est\')',
	'  WRITE (50,*) \'JD\'',
	'  DO WHILE(DATA)',
	'    IF (NEWIND.LE.1) WRITE (50,*) JD',
	'  ENDDO',                                
	'ENDIF' );
  push( @{$problem}, 
	'IF (ICALL.EQ.3) THEN',
	'  OPEN(50,FILE=\'etatab.est\')',
	"  WRITE (50,*) \'$eta_header\'",
	'  DO WHILE(DATA)',
	'    IF (NEWIND.LE.1) WRITE (50,*) ETA',
	'  ENDDO',                                
	'ENDIF' );
    
  #1.5 old version, keep this for now
  my $dn_string = 'DN1';
  for( 2..$n_parameters ){
    $dn_string = $dn_string . " DN$_";
  }
  my $table_string;
  if ($self->ind_param eq 'eta' ) {
    $table_string = 'ETA1';
    for( 2..$n_parameters ){
      $table_string = $table_string . " ETA$_";
    }
  } else {
    my $theta_labels = $copy -> labels( 'parameter_type' => 'theta' );
    
    foreach ( @{$theta_labels -> [0]} ){
      if( /TH\d+/ ){
	print "\nWarning: There is a generic theta label($_) in the model. ".
	    "(you might want to use the 'theta_names' option).\n\n" ;
      }
    }

    $table_string .= ' ' . join( ' ', @{$theta_labels -> [0]} );

  }
  $table_string = $table_string . " " . $dn_string;
  my $filestring = 'FILE=' . $self->tablename;
  $copy -> add_records( type => 'table',
			record_strings=>["ID JD $table_string NOPRINT ONEHEADER NOAPPEND FIRSTONLY $filestring"]);

  $copy -> _write;
      
  #1.8

  #run starting model explicitly here, then do setup of sequence of runs from where to process stuff.
  #compare sse general_setup
  
  my $temp_clean = ($self->clean > 2) ? 2 : $self->clean;

  $self->jd_model($copy);
  my $jd_run = tool::modelfit -> new( %{common_options::restore_options(@common_options::tool_options)},
				      top_tool         => 0,
				      models           => [$copy],
				      base_directory   => $self->directory,
				      directory        => $self->directory . '/jd_dir', 
				      parent_tool_id   => $self->tool_id,
				      retries          => 1,
				      logfile	         => undef,
				      raw_results      => undef,
				      prepared_models  => undef,
				      clean            => $temp_clean);
    
  $jd_run -> run;
  
  open( TMP, ">", 'jd.done' );
  print TMP "1"; 
  close( TMP );

  my ($jd_vector,$eta_matrix) = 
      $self-> get_jd_vector_eta_matrix(number_of_etas => $n_parameters,
				       table_file => $self->tablename);
  $self->n_individuals(scalar(@{$jd_vector}));


  unless (-e 'ofv.done'){
    $self->create_iofvcont(); 

    my $new_ofv_models = 
	$self -> setup_ind_ofv_models (number_of_individuals => $self->n_individuals,
				       number_of_etas => $n_parameters,
				       eta_matrix => $eta_matrix);

	my $clean= $self->clean;
	$clean = 2 if ($clean > 2); #clean cannot be more than 2, then important intermediate results are deleted
		$self->tools([]) unless defined $self->tools;
    push( @{$self->tools}, tool::modelfit ->
		  new( %{common_options::restore_options(@common_options::tool_options)},
			   models		   => $new_ofv_models,
			   nmtran_skip_model => 2,
			   parent_threads        => 1,
			   base_directory   => $self->directory,
			   directory        => $self->directory . 'ofv_dir', 
			   raw_results           => undef,
			   prepared_models       => undef,
			   top_tool              => 0,
			   clean                => $clean,
			   prepend_model_file_name => 1
		  ) );
  }
}

sub _modelfit_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	return \&subroutine;
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

  my $ind_ofv_matrix = $self-> get_ind_ofv_matrix(number_of_individuals => $self->n_individuals);

  my @jd_vector;
  #read jdtab
  open( JDFILE, $self->directory . "/jdtab.csv" ) or 
      croak("Could not find jdtab.csv.");
  while (my $val = <JDFILE>){
    chomp $val;
    push (@jd_vector,$val);
  }
  close(JDFILE);
  unless (scalar(@jd_vector) == $self->n_individuals){
    croak("Wrong number of rows in jdtab.csv");
  }

  $self -> 
      create_LxP_matrix_sum_LP_vector_P_values_matrix(number_of_individuals => $self->n_individuals,
						      ind_ofv_matrix => $ind_ofv_matrix,
						      jd_vector => \@jd_vector);

  # This function is a fast hack to get mean and variance of
  # individual contributions. For this to work I saved "eta_matrix"
  # and "P_values" in the $self hash. While not ideal from a memory
  # standpoint I think we are ok unless we have huge number of
  # individuals. / Pontus
}

sub sum_vector_entries
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 vector => { isa => 'Ref', optional => 1 }
	);
	my $vector = $parm{'vector'};
	my $sum_entries;

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

	return $sum_entries;
}

sub scale_vector
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 vector => { isa => 'Ref', optional => 1 },
		 factor => { isa => 'Num', optional => 0 }
	);
	my $vector = $parm{'vector'};
	my $factor = $parm{'factor'};
	my @return_vector;

  #input $vector is reference to single-dimension array
  #additional input is scalar $factor , e.g $individuals
  # output is return_vector

  unless (scalar(@{$vector}) > 0){
    croak("Error scale_vector: Empty vector.");
  }

  foreach my $val (@{$vector}){
    push (@return_vector,$factor*$val);
  }

	return \@return_vector;
}

sub get_raw_ofv_vector
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_of_individuals => { isa => 'Num', optional => 0 }
	);
	my $number_of_individuals = $parm{'number_of_individuals'};
	my @ofv_array;

  # input number_of_individuals
  #output ofv_array 

  my $raw_line_struct = $self->tools->[0]->raw_line_structure;
  my ($ofv_pos, $length) = split(/,/, $raw_line_struct -> {1} -> {'ofv'});

  if( not (defined $ofv_pos) || $ofv_pos == 0 ){
    croak("Something went wrong. There seem to ".
		 "be OFV values missing from the lst-files of the ofv_model runs.");
  }

  my $raw_file = $self->directory . 'ofv_dir/raw_results.csv'; #ugly, should use variable...
  unless ( -e $raw_file ){
    croak("File $raw_file does not exist.");
  }
  open( RAW, $raw_file ) or croak("Could not open $raw_file.");

  my $dirt = <RAW>; #skip first line, header
  while (my $row = <RAW>){
    chomp $row;
    my @temp = split(/,/,$row);
    #check ofv is a reasonable number 
    if ($temp[$ofv_pos] eq '0'){
      my $n=1+scalar(@ofv_array);
      ui -> print (category=>'pind', 
		   message=>"Warning: OFV value = 0 from lst-file ofv_model_$n.lst");
    }
    if ($temp[$ofv_pos] =~ /^-?\d*\.?\d*$/){ #is a number
      push (@ofv_array,$temp[$ofv_pos]);
    } else {
      my $n=1+scalar(@ofv_array);
      my $mess = "OFV value number $n in \n$raw_file \n is not a number. ".
	  "Check if running ofv_model_$n.mod failed.";
      ui -> print (category=>'pind', message=>$mess);
      push (@ofv_array,-99);
    }
  }
  close(RAW);
  unless (scalar(@ofv_array) == $number_of_individuals){
    my $mess = "Wrong number of ofv values read from $raw_file.";
    ui -> print (category=>'pind', 
		 message=>"Error: $mess");
  }

	return \@ofv_array;
}

sub create_iofvcont
{
	my $self = shift;

  #code for custom CONTR routine that will print individual ofv values
  #do not indent, NONMEM is sensitive...
  #print lots of decimals for high precision iofv
  #no input no output parameters

  mkdir( 'm2' ) unless( -d 'm2' );

  unless( -e 'm2/iofvcont.f' ){
    open(IOFVCONT , '>', 'm2/iofvcont.f');
    
    print IOFVCONT <<'EOF';
      subroutine contr (icall,cnt,ier1,ier2)
      parameter (no=1000)
      common /rocm1/ y(no),data(no,3),nobs
      integer nobs, un
      double precision cnt,y
      DATA un /80/
      if (icall.le.1) return
      call ncontr (cnt,ier1,ier2,l2r)
      write(un,10) data(1,1),cnt
   10 FORMAT(1F6.2,1F27.12)
      return
      end
EOF
    close( IOFVCONT );
  }
}

sub scale_exp_half_neg_vector
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 vector => { isa => 'Ref', optional => 1 },
		 factor => { isa => 'Num', optional => 0 }
	);
	my $vector = $parm{'vector'};
	my $factor = $parm{'factor'};
	my @l_vector;

  #this is used in 2.5.1
  #input $vector is reference to single-dimension array
  #additional input is scalar $factor 
  #output is l_vector

  unless (scalar(@{$vector}) > 0){
    croak("Error scale_exp_half_neg_vector: Empty vector.");
  }

  foreach my $val (@{$vector}){
    push (@l_vector ,$factor*exp(-0.5*$val));
  }

	return \@l_vector;
}

sub column_sums
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 matrix => { isa => 'Ref', optional => 1 }
	);
	my $matrix = $parm{'matrix'};
	my @column_sum_vector;

  #input $matrix is array of references to row value arrays 
  #output is reference to vector column_sum_vector
  #sorting is used to avoid numerical problems which are otherwise likely

  unless (scalar(@{$matrix}) > 0){
    croak("Error column_sums: Empty matrix (no rows).");
  }

  my $n_columns = scalar(@{$matrix->[0]});
  unless ($n_columns > 0){
    croak("Error column_sums: Empty first matrix row.");
  }

  @column_sum_vector = (0) x $n_columns;

  #this is a slow algorithm, but numerically most stable which is important in pind
  foreach my $i (0 .. ($n_columns -1)){
    my @temp_vec;
    foreach my $row (@{$matrix}){
      if ($i == 0){ #first time check number of entries in row
	unless (scalar(@{$row}) == $n_columns){
	  croak("Error column_sums: Number of entries differ between rows.");
	}
      }
      push (@temp_vec,$row->[$i]);
    }
    foreach my $val (sort {$a <=> $b} @temp_vec){
      $column_sum_vector[$i] += $val;
    }
  }

	return \@column_sum_vector;
}

sub inverse_scale_columns
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 matrix => { isa => 'Ref', optional => 1 },
		 denominator_vector => { isa => 'Ref', optional => 1 }
	);
	my $matrix = $parm{'matrix'};
	my $denominator_vector = $parm{'denominator_vector'};
	my @scaled_matrix;

  #input is vector reference $denominator_vector
  #input $matrix is ref of array of references to row value arrays 
  #output is $scaled_matrix

  my $n_rows = scalar(@{$matrix});
  unless ( $n_rows > 0){
    croak("Error inverse_scale_columns: Empty matrix (no rows).");
  }

  my $n_columns = scalar(@{$denominator_vector});
  unless ($n_columns > 0){
    croak("Error inverse_scale_columns: Empty denominator vector.");
  }
  foreach my $val (@{$denominator_vector}){
    croak("Error inverse_scale_columns: 0 entry in denominator vector.")
	if ($val == 0);    
  }

  foreach my $row (@{$matrix}){
    unless (scalar(@{$row}) == $n_columns){
      croak("Error inverse_scale_columns: Number of row entries differ.");
    }
    my @new_row;
    foreach my $j (0 .. ($n_columns -1)){
      push (@new_row, ($row->[$j])/($denominator_vector->[$j]));
    }
    push (@scaled_matrix,\@new_row);
  }

	return \@scaled_matrix;
}

sub get_ind_ofv_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_of_individuals => { isa => 'Int', optional => 0 }
	);
	my $number_of_individuals = $parm{'number_of_individuals'};
	my @ind_ofv_matrix;

  #this is sec 2.2.7 + section 2.3 in URS individual probability script
  #post-processing of individual fort.80
  #input is scalar $number_of_individuals
  #need input directory??
  #return ind_ofv_matrix

  for (my $id=1; $id<=$number_of_individuals; $id++ ){
    open (IND, '<', "m2/ofv_model_$id.fort.80") || 
	    croak("Couldn't open m2/ofv_model_$id.fort.80 for reading: $!");

    my @ind_ofv;
    while(<IND>) {
      chomp;
      my ($junk,$ind_ofv) = split(' ',$_); #first col is what???
      push @ind_ofv, $ind_ofv;
    }
    close IND;
    @ind_ofv = splice(@ind_ofv,-$number_of_individuals); #only last section is what we want
    push( @ind_ofv_matrix, \@ind_ofv );
  }

  $self->verify_ind_ofv_matrix(ind_ofv_matrix => \@ind_ofv_matrix,
			       number_of_individuals => $number_of_individuals);

  open( FILE, ">","ind_ofv.csv" );
  foreach my $sub_arr (@ind_ofv_matrix ){
    print( FILE join( ',', @{$sub_arr} ), "\n" );
  }
  close( FILE );

	return \@ind_ofv_matrix;
}

sub get_jd_vector_eta_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_of_etas => { isa => 'Int', optional => 0 },
		 table_file => { isa => 'Str', optional => 0 }
	);
	my $number_of_etas = $parm{'number_of_etas'};
	my $table_file = $parm{'table_file'};
	my @jd_vector;
	my @eta_matrix;

  #this is sec 2.1 in URS individual probability script plus ETA reading.
  #input is file name for table file with ID JD ETA1..ETAX DN1..DNX  make sure table has header
  #input is scalar $number_of_etas
  #perhaps add scalar $number_of_individuals
  #what does firstonly in tablerecord mean??? only first observation that individual???
  #output is jd_vector and eta_matrix

  unless ($number_of_etas > 0){
      croak("Error get_jd_vector_eta_matrix: must be at least one eta");
  }

  my $tabfile = (defined $self->njd) ? $self->njd : $self->jd_model->directory().$table_file;
  unless ( -e  $tabfile){
    croak("File $tabfile does not exist.");
  }
	my $table = data -> new(filename => $tabfile, ignoresign => '@', idcolumn => 1); #this is a table we made, know idcol is 1
  my $header = (defined $self->njd) ? 'NJD': 'JD';

  @jd_vector = @{$table -> column_to_array('column'=>$header)};
  my $number_of_individuals = scalar(@jd_vector);
  unless ($number_of_individuals > 0){
      croak("Error get_jd_vector_eta_matrix: no $header values found in $tabfile.");
  }

  #verify sum of elements is 1
  my $jd_sum = $self->sum_vector_entries(vector => \@jd_vector);
  #what should tolerance be? NONMEM outputs 5 sig digits -> rel error 1 in 5th for each...
  my $rel_error = ($jd_sum-1)/1;
  if ($rel_error > 0.0001){
    ui -> print (category=>'pind', 
		 message=>"\nsum $header which should be 1 is $jd_sum, rel error $rel_error");
  }
  #print jd / njd
  open( FILE, ">","jdtab.csv" );
  foreach my $val (@jd_vector ){
    print( FILE "$val\n" );
  }
  close( FILE );

  #first eta
  my $theta_labels = $self->models->[0]->labels( 'parameter_type' => 'theta' );
  my $col;
  if ($self->ind_param eq 'eta') {
    $col='ETA1';
  } else {
    $col= $theta_labels -> [0]->[0];
  }
  my $temp_vec = $table -> column_to_array('column'=>$col);
  
  unless (scalar(@{$temp_vec}) > 0 ){
      croak("Error get_jd_vector_eta_matrix: No $col ".
		    "values found in tablefile.");
  }
  unless ($number_of_individuals == scalar(@{$temp_vec})){
      croak("Error get_jd_vector_eta_matrix: Different number of ".
		    "$col and JD values found in tablefile.");
  }

  foreach my $eta (@{$temp_vec}){
    push (@eta_matrix,[$eta]);
  }

  for (my $j=2; $j<=$number_of_etas; $j++){
    my $column_name;
    if ($self->ind_param eq 'eta') {
      $column_name='ETA'.$j;
    } else {
      $column_name= $theta_labels -> [0]->[$j-1];
    }
    my $column_data = $table -> column_to_array('column'=> $column_name);
    unless ($number_of_individuals == scalar(@{$column_data})){
      croak("Error get_jd_vector_eta_matrix: Different number of ".
		    "$column_name and JD values found in tablefile.");
    }
    for (my $i=0; $i<$number_of_individuals; $i++){
      push (@{$eta_matrix[$i]},$column_data->[$i]);
    }
  }

	return \@jd_vector ,\@eta_matrix;
}

sub alt_get_jd_vector_eta_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_of_etas => { isa => 'Int', optional => 0 },
		 table_file => { isa => 'Str', optional => 0 }
	);
	my $number_of_etas = $parm{'number_of_etas'};
	my $table_file = $parm{'table_file'};
	my @jd_vector;
	my @eta_matrix;

  #NOT USED!!! intented for increased number of significant digits in values
  #this is sec 2.1 in URS individual probability script plus ETA reading.
  #input is file name for table file with ID JD ETA1..ETAX DN1..DNX  make sure table has header
  #input is scalar $number_of_etas
  #perhaps add scalar $number_of_individuals
  #what does firstonly in tablerecord mean??? only first observation that individual???
  #output is jd_vector and eta_matrix

  unless ($number_of_etas > 0){
      croak("Error get_jd_vector_eta_matrix: must be at least one eta");
  }

  open( FILE, $self->directory . "/jd_dir/NM_run1/jdtab.est" ) or 
      croak("Could not find jdtab.est.");
  my $dirt = <FILE>; #skip first line, header
  while (my $val = <FILE>){
    chomp $val;
    push (@jd_vector,$val);
  }
  close(FILE);

  my $number_of_individuals = scalar(@jd_vector);
  unless ($number_of_individuals > 0){
      croak("Error get_jd_vector_eta_matrix: no jd values found in jdtab.est.");
  }

  #verify sum of elements is 1
  my $jd_sum = $self->sum_vector_entries(vector => \@jd_vector);
  #what should tolerance be? NONMEM outputs 5 sig digits -> rel error 1 in 5th for each...
  my $rel_error = ($jd_sum-1)/1;
  if ($rel_error > 0.0001){
    ui -> print (category=>'pind', 
		 message=>"\nsum jd which should be 1 is $jd_sum, rel error $rel_error");
  }
  #print jd
  open( FILE, ">","jdtab.csv" );
  foreach my $val (@jd_vector ){
    print( FILE "$val\n" );
  }
  close( FILE );

  #first eta
  my $col;
  unless ($self->ind_param eq 'eta') {
    croak("Alternative not implemented, cannot use theta and alt jd and eta.");
  }

  open( FILE, $self->directory . "/jd_dir/NM_run1/etatab.est" ) or 
      croak("Could not find etatab.est.");
  my $dirt = <FILE>; #skip first line, header
  while (my $line = <FILE>){
    chomp $line;
    my @row=split(' ',$line);
    push (@eta_matrix,\@row);
  }
  close(FILE);
  
  unless (scalar(@eta_matrix) > 0 ){
      croak("Error get_jd_vector_eta_matrix: No ".
		    "values found in etatab.est.");
  }
  unless ($number_of_individuals == scalar(@eta_matrix)){
      croak("Error get_jd_vector_eta_matrix: Different number of ".
		    "ETA and JD values found.");
  }

	return \@jd_vector ,\@eta_matrix;
}

sub setup_ind_ofv_models
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_of_individuals => { isa => 'Int', optional => 0 },
		 number_of_etas => { isa => 'Int', optional => 0 },
		 eta_matrix => { isa => 'Ref', optional => 1 }
	);
	my $number_of_individuals = $parm{'number_of_individuals'};
	my $number_of_etas = $parm{'number_of_etas'};
	my $eta_matrix = $parm{'eta_matrix'};
	my @new_ofv_models;

  #this is sec 2.2 with 2.2.1-2.2.6 (not 2.2.7 )in URS individual probability script
  #run command is the main run in bin/pind script
  #input scalar number_of_individuals scalar number_of_etas
  #input ref to matrix $eta_matrix
  #output is array of models to run

  foreach my $id( 0..($number_of_individuals-1) ){
      
	  my $copy = $self->jd_model->copy( filename => 'ofv_model_'.($id+1).'.mod',
										directory => 'm2',
										copy_datafile => 0,
										copy_output => 0,
										write_copy => 0);
	  
      my $number_of_omegas = $copy -> nomegas -> [0];

      #2.2.1 replace each ETAX with the value for that ETAX from nptab-file
      #do formatting to ensure ok if using high precision
      # look for ETA(N) in $PK, $PRED, $ERROR and replace ETA(N) with correct number

      unless (scalar(@{$eta_matrix->[$id]}) == $number_of_etas){
				croak("Error create_and_run_ind_ofv_models: wrong number of etas ind ($id+1)");
      }

      #the regular expression here cannot handle case where line starts with ETA(X) 
      #without even a leading space. Think that case can never occur, since
      #no assignment done to ETAs in PK/PRED/ERROR
			if ($self->ind_param eq 'eta') {
				my $record_ref = $copy -> record(record_name => 'pk' );
				if ( scalar(@{$record_ref}) > 0 ){ 
					my $code_block;
					foreach my $line (@{$copy -> pk}){
						my $new_line;
						while( $line =~ /(.*[^A-Z]+)ETA\((\d+)\)(.*)/g ){
							my $eta_index = $2-1; #does conversion work here??
							$line = $3;
							my $etastring = sprintf "(%12.6E)",$eta_matrix->[$id]->[$eta_index];
							$new_line .= $1.$etastring; 
						}
						push(@{$code_block},$new_line.$line );
					}
					$copy -> pk( new_pk => $code_block );
				}

				$record_ref = $copy -> record(record_name => 'pred' );
				if ( scalar(@{$record_ref}) > 0 ){ 
					my $code_block;
					foreach my $line (@{$copy -> pred}){
						my $new_line;
						#ok empty set []???
						while( $line =~ /(.*[^A-Z]+)ETA\((\d+)\)(.*)/g ){
							my $eta_index = $2-1; #does conversion work here??
							$line = $3;
							$new_line .= $1. "$eta_matrix->[$id]->[$eta_index]";
						}
						push(@{$code_block},$new_line.$line );
					}
					$copy -> pred( new_pred => $code_block );
				}
				$record_ref = $copy -> record(record_name => 'error' );
				if ( scalar(@{$record_ref}) > 0 ){ 
					my $code_block;
					foreach my $line (@{$copy -> problems->[0]->errors->[0]->code}){
						my $new_line;
						while( $line =~ /(.*[^A-Z]+)ETA\((\d+)\)(.*)/g ){
							my $eta_index = $2-1; #does conversion work here??
							$line = $3;
							$new_line .= $1. "$eta_matrix->[$id]->[$eta_index]";
						}
						push(@{$code_block},$new_line.$line );
					}
					$copy -> set_records(type => 'error',
						record_strings => $code_block);
				}
			} else {
				#fix thetas to new values

				my @theta_inits;
				for (my $j; $j<$number_of_etas; $j++){
					push(@theta_inits,$eta_matrix->[$id]->[$j]);
				}
				$copy -> initial_values( parameter_type => 'theta',
					parameter_numbers => [[1..$number_of_etas]], 
					new_values => [\@theta_inits]) ;

			}

      #Temporary for Paul 2008-09-26
      #Add IGNORE(ID.GT.X) to $DATA
      if (defined $self->ignore_individuals_above && $self->ignore_individuals_above > 0) {
				my $ignore_string='(ID.GT.'.$self->ignore_individuals_above.')';
				$copy -> add_option( record_name => 'data',
			     option_name => 'IGNORE',
			     option_value => $ignore_string );
      }

      #2.2.2 Add DATA=(ID) to $CONTR
      $copy -> add_records( type => 'contr',
			    record_strings => ['DATA=(ID)'] );
      
      #2.2.3 Add custom CONTR routine, that prints individual ofv-values to fort.80, to $SUBROUTINE
      #first check if have $SUBROUTINE
      my $record_ref = $copy -> record(record_name => 'subroutine' );
			if ( scalar(@{$record_ref}) > 0 ){ 
				$copy -> add_option( record_name => 'subroutine',
					option_name => 'CONTR',
					option_value => 'iofvcont.f' );
			} else {
				$copy -> add_records( type => 'subroutine',
					record_strings => ['CONTR=iofvcont.f'] );
      }
      
      $copy -> extra_output( ['fort.80'] );
 
      #2.2.4 fix omegas to 0
      #values are already fixed from previous model, but we'll do it again
      #inc case code above is ever changed
      $copy -> initial_values( parameter_type => 'omega',
			       parameter_numbers => [[1 .. $number_of_omegas]],
			       new_values => [[(0) x $number_of_omegas]] );
      $copy -> fixed( parameter_type => 'omega',
		      new_values => [[(1) x $number_of_omegas ]] );

      #2.2.5 remove $TABLE and $NONPARAMETRIC 
      $copy -> remove_records( type => 'table' );
      $copy -> remove_records( type => 'nonparametric' );

      #2.2.6 set MAXEVALS=0 (might be done already)
      #can we assume there is an estimation record????
      $copy -> set_option(record_name => 'estimation',
			  option_name => 'MAXEVALS',
			  fuzzy_match => 1,
			  option_value => '0');
      
      $copy -> _write;
      
      push( @new_ofv_models, $copy );
    }
    
	return \@new_ofv_models;
}

sub verify_ind_ofv_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_of_individuals => { isa => 'Int', optional => 0 },
		 ind_ofv_matrix => { isa => 'Ref', optional => 1 }
	);
	my $number_of_individuals = $parm{'number_of_individuals'};
	my $ind_ofv_matrix = $parm{'ind_ofv_matrix'};

  #this is verification described between sec 2.3 and 2.5 in URS for p_individuals
  #input is reference to ind_ofv_matrix and scalar number of individuals

  my $rel_tol = 0.00001; #motivation: 14 sig figs in ind_ofv, but raw_res only 
  #6, i.e. error is rounding error in raw_res. 

  my $ofv_array = $self->get_raw_ofv_vector(number_of_individuals => $number_of_individuals);
 
  for (my $id=0; $id<$number_of_individuals; $id++ ){
    my $sum_ofv = $self->sum_vector_entries(vector => $ind_ofv_matrix->[$id]);
    unless (abs(($ofv_array->[$id] - $sum_ofv)/$ofv_array->[$id])< $rel_tol){
      my $mess = "Verification failed for ind ($id+1), ".
		    "relative difference between $sum_ofv and lst-file value ".
		    "$ofv_array->[$id] larger than relative tolerance $rel_tol.";
      ui -> print (category=>'pind',message=>$mess);
    }
  }
}

sub create_LxP_matrix_sum_LP_vector_P_values_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_of_individuals => { isa => 'Int', optional => 0 },
		 ind_ofv_matrix => { isa => 'Ref', optional => 1 },
		 jd_vector => { isa => 'Ref', optional => 1 }
	);
	my $number_of_individuals = $parm{'number_of_individuals'};
	my $ind_ofv_matrix = $parm{'ind_ofv_matrix'};
	my $jd_vector = $parm{'jd_vector'};

  #this is sec 2.5.1 and 2.5.2 and 2.6 of URS individual probability script
  #input is scalar $number_of_individuals
  #input reference to matrix $ind_ofv_matrix 
  #input reference to vector $jd_vector
  #no return values

  my @L_times_P_matrix;
  
  my $n_rows = scalar(@{$ind_ofv_matrix});
  unless ( $n_rows >0 ){
    croak("Error get_L_times_P_matrix : Empty matrix (no rows).");
  }
  unless ( $n_rows == $number_of_individuals){
    croak("Error get_L_times_P_matrix : ".
		  "Number of matrix rows $n_rows differs from ".
		  "number of individuals $number_of_individuals.");
  }
  
  unless (scalar(@{$jd_vector}) == $number_of_individuals){
    croak("Error get_L_times_P_matrix: Length of jd_vector differs from number of ".
		  "individuals $number_of_individuals.");
  }
  
  ###
  ##this is for error propagation analysis
  my @jd_rel_error;
  my @jd_abs_error;
  foreach my $val (@{$jd_vector}){
    my $n_digits = 5;
    my $log10 = int(log($val)/log(10)); #chop decimals
    my $abs_err = (10**($log10-$n_digits-1))*5;
    my $rel_err = $abs_err/($val-$abs_err);
    push(@jd_rel_error,$rel_err);
    push(@jd_abs_error,$abs_err);
  }
  #end error prop

  
  #2.5.1

  my @abs_err_L_times_P;
  my @rel_err_L_times_P;

  my $id=-1;
  foreach my $row (@{$ind_ofv_matrix}){
    $id++;
    my $newline = $self->scale_exp_half_neg_vector(vector => $row, factor=>$jd_vector->[$id]);
    push (@L_times_P_matrix,$newline);

    #only for error propagation
    #assume all ind_ofv values positive
    my $error_factor = $jd_rel_error[$id]/(1-$jd_rel_error[$id]);
    my $abs_line = $self->scale_vector(vector => $newline, factor=>$error_factor);
    push (@abs_err_L_times_P,$abs_line);
    my @rel_line;
    for (my $k=0; $k<$number_of_individuals; $k++){
      if ($newline->[$k] == 0){
	push (@rel_line,0);
      } else {
	my $denom=($newline->[$k] - $abs_line->[$k]);
	push (@rel_line,$abs_line->[$k]/$denom);
      }
    }
    push (@rel_err_L_times_P,\@rel_line);

    #end error prop
  }

  
  open( FILE, ">","L_times_P.csv" );
  foreach my $sub_arr (@L_times_P_matrix ){
    print( FILE join( ',', @{$sub_arr} ), "\n" );
  }
  close( FILE );
  
  #2.5.2
  my $sum_LP_vector = $self->column_sums(matrix => \@L_times_P_matrix);

  #error prop
  my $abs_err_sum_LP = $self->column_sums(matrix => \@abs_err_L_times_P);
  my @rel_err_sum_LP;
  for (my $k=0; $k<$number_of_individuals; $k++){
    push (@rel_err_sum_LP,(($abs_err_sum_LP->[$k])/($sum_LP_vector->[$k] - $abs_err_sum_LP->[$k])));
  }
  #end prop

  open( FILE, ">","sum_LP.csv" );
  print( FILE join( ',', @{$sum_LP_vector} ), "\n" );
  close( FILE );  

  #extra verification, variant of row verification P_values
  my $ofv_array = $self->get_raw_ofv_vector(number_of_individuals => $number_of_individuals);
  my $id=0;
  foreach my $row (@{$ind_ofv_matrix}){
    my $vec = $self->scale_exp_half_neg_vector(vector => $row, factor=>1);
    my $colnum=0;
    my @temp_vec;
    my @abs_err_vec;
    foreach my $val (@{$vec}){
      my $colval = $val/($sum_LP_vector->[$colnum]);
      push (@temp_vec,$colval);
      push (@abs_err_vec, ($colval*($rel_err_sum_LP[$colnum]/(1-$rel_err_sum_LP[$colnum]))));
      $colnum++;	    
    }
    my $maxofv=-10000000;
    foreach my $val (@{$row}){
      $maxofv = $val if ($val > $maxofv);
    }
    my $checksum= $self->sum_vector_entries (vector=>\@temp_vec);
    my $errsum= $self->sum_vector_entries (vector=>\@abs_err_vec);
    my $rel=$errsum/($checksum-$errsum);
    my $realerr=$checksum-$number_of_individuals;
    my $ratio=$realerr/$maxofv;
    $id++;
  }

  #error prop
  my @rel_err_P_values;
  foreach my $row (@rel_err_L_times_P){
    my @newline;
    for (my $k=0; $k<$number_of_individuals; $k++){
      push (@newline,($row->[$k]+$rel_err_sum_LP[$k]));
    }
    push (@rel_err_P_values,\@newline);
  }
  #end error prop

  #2.6
  my $denominator = $self-> scale_vector(vector =>  $sum_LP_vector,
					 factor => $number_of_individuals);
  my $P_values_matrix = $self->inverse_scale_columns(matrix => \@L_times_P_matrix,
						       denominator_vector => $denominator);

  #error propagation
  my @abs_err_P_values;
  my @abs_err_P_rows;
  my @rel_err_P_rows;
  my $i=-1;
  foreach my $row (@{$P_values_matrix}){ 
    $i++;
    my @newline;
    for (my $j=0; $j<$number_of_individuals; $j++){
      my $re = $rel_err_P_values[$i]->[$j];
      if ($re < 1){
	push(@newline,($re*($row->[$j]))/(1-$re));
      } else {
	push(@newline,($re*($row->[$j])));
      }      
    }
    push(@abs_err_P_values,\@newline);
    push(@abs_err_P_rows,$self->sum_vector_entries(vector => \@newline));
  }

  for (my $j=0; $j<$number_of_individuals; $j++){
    my $val = $abs_err_P_rows[$j];
    my $rv = $self->sum_vector_entries(vector => $P_values_matrix->[$j]); 
    my $abs_err = $jd_vector->[$j] - $rv;
    my $actual_rel = abs($abs_err)/($jd_vector->[$j]);
    my $round_rel = $val/($rv-$val);
  }

  $self->verify_P_values_matrix(P_values_matrix => $P_values_matrix,
				number_of_individuals => $number_of_individuals,
				jd_vector => $jd_vector);

  open( FILE, ">","P_values.csv" );
  foreach my $sub_arr (@{$P_values_matrix} ){
    print( FILE join( ',', @{$sub_arr} ), "\n" );
  }
  close( FILE );
}

sub verify_P_values_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_of_individuals => { isa => 'Int', optional => 0 },
		 P_values_matrix => { isa => 'Ref', optional => 1 },
		 jd_vector => { isa => 'Ref', optional => 1 }
	);
	my $number_of_individuals = $parm{'number_of_individuals'};
	my $P_values_matrix = $parm{'P_values_matrix'};
	my $jd_vector = $parm{'jd_vector'};

  #this is verification described after sec 2.6 in URS for p_individuals
  #input is reference to P_values_matrix and scalar number of individuals
  #input reference to jd_vector
  
  my $col_tol = 0.000001; #here we just check normalization within script
  # where substeps are never written and reread. Can have high standards!!!
  #what is precision in Perl?????
  unless ($number_of_individuals > 0){
    croak("Error verify_P_values_matrix: ".
		  "number of individuals $number_of_individuals is not greater than 0.");
  }

  unless (scalar(@{$jd_vector}) == $number_of_individuals){
    croak("Error verify_P_values_matrix: Length of input jd_vector differs from ".
		  "number of individuals $number_of_individuals.");
  }
  unless (scalar(@{$P_values_matrix}) == $number_of_individuals){
    croak("Error verify_P_values_matrix: Number of rows in P_values_matrix differs from ".
		  "number of individuals $number_of_individuals.");
  }
  
  my $column_sum_vector = $self->column_sums(matrix => $P_values_matrix);
  unless (scalar(@{$column_sum_vector}) == $number_of_individuals){
    croak("Error verify_P_values_matrix: Length of column_sum_vector differs from ".
		  "number of individuals $number_of_individuals.");
  }

  #each column sum should be equal to 1/$number_of_individuals within tolerance. 
  my $inverse_individuals = 1/$number_of_individuals;
  my $col_counter=1;
  foreach my $sum (@{$column_sum_vector}){
    unless (abs ($sum - $inverse_individuals) < $col_tol){
      my $mess = "Error verify_P_values_matrix: Difference column sum $sum and ".
		    "inverse_individuals $inverse_individuals larger than tolerance $col_tol ".
		    "in column $col_counter.";
      ui -> print (category=>'pind',message=>$mess);
    }
    $col_counter++;
  }
  
  #the sum of each row should be within row_tol of jd
  #the error is perhaps rounding error in jdtab-> check relative error instead, 
  #jdtab has five digits i.e. (approx/jd < 0.0001first 4 in ratio should be ok?
  my $row_rel_tol = (0.01);
  my $header = "JD,abs error,rel error percent,individual\n";
  my $header_printed = 0;
  for (my $i=0; $i<$number_of_individuals; $i++){
    my $sum = $self->sum_vector_entries(vector => $P_values_matrix->[$i]);
    my $jd = $jd_vector->[$i];
    my $abs_error = abs ($sum - $jd);
    my $ratio = $abs_error/$jd;
    unless ( $ratio < $row_rel_tol){
      my $perc = $ratio*100;
      my $line = sprintf "%4.2e, %4.2e ,%5.2f, %i\n", $jd, $abs_error, $perc, $i+1 ;
      unless ($header_printed){
	$header_printed = 1;
      }
    }
  }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
