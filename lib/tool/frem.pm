package tool::frem;

use include_modules;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Config;
use linear_algebra;
use ui;
use File::Copy qw/cp mv/;
use File::Path 'rmtree';


use Moose;
use MooseX::Params::Validate;

extends 'tool';

my $joindata = 'frem_vpc.dta';
my $fremtype = 'FREMTYPE'; 
my $name_check_model = 'check_data.mod';
my $name_model0 = 'model_0.mod';
my $name_model1 = 'model_1.mod';
my $name_model2_all = 'model_2.mod';
my $name_model2_timevar = 'model_2_timevar.mod';
my $name_model2_invar = 'model_2_invariant.mod';
my $name_model3 = 'model_3.mod';
my $name_modelvpc_1 = 'vpc_model_1.mod';
my $name_modelvpc_2 = 'vpc_model_2_template.mod';
my $name_vpc_final = 'frem_vpc.mod';
my $data2name = 'frem_data2.dta';
my $undefined = 'undefined';
my $smallval = 0.0001;

has 'start_eta' => ( is => 'rw', isa => 'Int');
has 'done_file' => ( is => 'rw', isa => 'Str', default => 'template_models_done.pl');
#has 'original_data' => ( is => 'rw', isa => 'Str');
has 'filtered_datafile' => ( is => 'rw', isa => 'Str' );
has 'estimate' => ( is => 'rw', isa => 'Int', default => 3 );
has 'typeorder' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'occasionlist' => ( is => 'rw', isa => 'ArrayRef[Int]', default => sub { [] } );
has 'extra_input_items' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'invariant_median' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'timevar_median' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'invariant_covmatrix' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'timevar_covmatrix' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'check' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'vpc' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'DV' );
has 'occasion' => ( is => 'rw', isa => 'Str', default => 'OCC' );
has 'parameters' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'time_varying' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'invariant' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef', default => sub { ['frem.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'frem_results.csv' );


sub BUILD
{
	my $self  = shift;

	for my $accessor ('logfile','raw_results_file','raw_nonp_file'){
		my @new_files=();
		my @old_files = @{$self->$accessor};
		for (my $i=0; $i < scalar(@old_files); $i++){
			my $name;
			my $ldir;
			( $ldir, $name ) =
			OSspecific::absolute_path( $self ->directory(), $old_files[$i] );
			push(@new_files,$ldir.$name) ;
		}
		$self->$accessor(\@new_files);
	}	

	foreach my $model ( @{$self -> models} ) {
		foreach my $problem (@{$model->problems()}){
			if (defined $problem->nwpri_ntheta()){
				ui -> print( category => 'all',
					message => "Warning: frem does not support \$PRIOR NWPRI.",
					newline => 1);
				last;
			}
		}
	}

	if (defined $self->start_eta() and (scalar(@{$self->invariant})<1)){
		croak('No allowed to set option -start_eta when option -invariant is not set');
	}
	if ( scalar (@{$self -> models->[0]-> problems}) > 1 ){
		croak('Cannot have more than one $PROB in the input model.');
	}

	#checks left for frem->new:
#checks left for frem->new: occ and dv exist in $INPUT if needed. type exists in $INPUT if given. 
	#if bov_parameters is > 0 then must have occasion in $input
	#must have dv in $input

	my $occ_ok=1;
	my $dv_ok=0;

	if (scalar(@{$self->parameters()})>0){
		$occ_ok=0;
	}

	my $prob = $self -> models->[0]-> problems -> [0];
	if (defined $prob->priors()){
		croak("frem does not support \$PRIOR");
	}

	if( defined $prob -> inputs and defined $prob -> inputs -> [0] -> options ) {
		foreach my $option ( @{$prob -> inputs -> [0] -> options} ) {
			unless (($option -> value eq 'DROP' or $option -> value eq 'SKIP'
						or $option -> name eq 'DROP' or $option -> name eq 'SKIP')){
				$dv_ok = 1 if ($option -> name() eq $self->dv()); 
#				$type_ok = 1 if ($option -> name() eq $self->type()); 
				$occ_ok = 1 if ($option -> name() eq $self->occasion()); 
			}
		}
#		croak("type column ".$self->type()." not found in \$INPUT" ) unless $type_ok;
		croak("dependent column ".$self->dv()." not found in \$INPUT" ) unless $dv_ok;
		croak("occasion column ".$self->occasion()." not found in \$INPUT" ) unless $occ_ok;
	} else {
		croak("Trying to check parameters in input model".
			" but no headers were found in \$INPUT" );
	}
}

sub create_template_models
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'Ref', optional => 0 }
	);
	my $model = $parm{'model'};


	#variables to store 
	my $n_invariant	= scalar(@{$self->invariant()});
	my $n_time_varying = scalar(@{$self->time_varying()});
	my $n_occasions;
	my $total_orig_etas;
	my $start_omega_record;
	my $use_pred;
	my $start_eta = $self->start_eta();
	my $bsv_parameters;
	my $bov_parameters = scalar(@{$self->parameters()});
	my @occasion_labels = ();
	my @bsv_par_labels=(); 
	my @bsv_cov_labels=(); 
	my @bov_par_labels=();
	my @bov_cov_labels=();
	my $model1_updated=0;
	my @CTV_parameters;


	#local objects
	my $frem_model0;
	my $output_0;
	my $frem_model1;
	my $frem_model2;
	my $frem_model2_timevar;
	my $frem_model2_invar;
	my $frem_model3;
	my $frem_vpc_model1;
	my $frem_vpc_model2;
	my @leading_omega_records=();
	my $filter_data_model;
	my $data_check_model;
	my $data2_data_record;
	my $mod0_ofv;

	if ($model -> is_run()){
		$output_0 =  $model ->outputs()->[0];
		if ( defined $output_0){
			$mod0_ofv = $output_0->get_single_value(attribute=> 'ofv');
		}else{
			$mod0_ofv = $undefined;
		}
	}else{
		$mod0_ofv = $undefined;
	}
	$total_orig_etas = $model->problems->[0]->nomegas( with_correlations => 0,
													   with_same => 1);
	
	#copy original data to m1. Then only use that file, use relative data path in modelfits
	#only set data record string in all models, do not mess with data object and chdir and such
	#since we are not running any models here

	my $original_data_name = $model->datafiles(absolute_path=>1)->[0];
	if ($n_invariant > 0){
		$start_eta = 1 unless (defined $start_eta);
		$bsv_parameters = ($total_orig_etas-$start_eta+1);
		$start_omega_record = $model-> problems -> [0]->check_start_eta(start_eta => $start_eta);
	}else{
		#no invariant stuff
		$start_eta = $total_orig_etas+1; #not yet exist
		$bsv_parameters = 0;
		$start_omega_record = scalar(@{$model-> problems -> [0]->omegas()})+1; #not yet exist
	}

	
	##########################################################################################
	#Create Model 0
	##########################################################################################
	$frem_model0 = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model0,
									output_same_directory => 1,
									write_copy => 0,
									copy_datafile   => 1,
									copy_output => 0);
	#Update inits from output, if any
	if (defined $output_0){
		$frem_model0 -> update_inits ( from_output => $output_0,
									   problem_number => 1);
	}
	#since copy_datafile was set, the physical datafile will be copied to m1 and the model written with relative data path
	$frem_model0 ->_write();

	#find etas already in model and set bsv_labels
	#find parameters that have ETAs on them already
	#search in Model 0 because there we have not added BOV ETAS
	my @code;
	@code = @{$frem_model0 -> pk( problem_number => 1 )};
	$use_pred = 0;
	unless ( $#code > 0 ) {
		@code = @{$frem_model0 -> pred( problem_number => 1 )};
		$use_pred = 1;
	}
	if ( $#code <= 0 ) {
		croak("Neither PK or PRED defined in model 0");
	}

	my %CTV_par;
	foreach my $p (@{$self->parameters()}){
		$CTV_par{$p}=1;
	}
	
	my @TVPAR=();
	#find all TV par on left hand side. Allow IF lines
	for (my $i=0; $i<scalar(@code); $i++) {
		next if ( $code[$i] =~ /^\s*\;/); #comment line
		if ( $code[$i] =~ /^[^;]*\bTV(\w+)\s*=/  ){
			push(@TVPAR,$1);
		}
	}
	#find all par = lines and check if have ETA, then add to CTV hash. Allow IF lines
	my %etanum_to_parameter;
	foreach my $par (@TVPAR){
#		next if (defined $CTV_par{$par}); #we already know this has BOV ETA but we want to get etanum mapping
		for (my $i=0; $i<scalar(@code); $i++) {
			next if ( $code[$i] =~ /^\s*\;/); #comment line
			if ( $code[$i] =~ /^[^;]*\b$par\s*=.*\bETA\((\d+)\)/  ){
				my $num = $1;
				$etanum_to_parameter{$1}=$par;
				$CTV_par{$par} =1;
				last;
			}
		}
	}
	@CTV_parameters = (keys %CTV_par);

	##########################################################################################
	#Create data set 2
	##########################################################################################
	$self->create_data2(model=>$frem_model0,
						filename => $self -> directory().'m1/filter_data_model.mod',
						bov_parameters => $bov_parameters);
	
	$n_occasions = scalar(@{$self->occasionlist()}); #attribute set inside createdata2

	##########################################################################################
	#Create labels
	##########################################################################################
	if ($n_occasions >0){
		for (my $i=0; $i< $n_occasions; $i++){
			push(@occasion_labels,$self->occasion().'='.$self->occasionlist()->[$i]);
		}
	}

	if ($n_invariant > 0){
		for (my $i=$start_eta;$i< ($start_eta + $bsv_parameters);$i++){
			if (defined $etanum_to_parameter{$i}){
				push(@bsv_par_labels,'BSV par '.$etanum_to_parameter{$i});
			}else{
				push(@bsv_par_labels,'BSV par ');
			}
		}
	}

	if ($n_invariant > 0){
		for (my $i=0; $i< $n_invariant; $i++){
			push(@bsv_cov_labels,'BSV cov '.$self->invariant()->[$i]);
		}
	}
	if ($bov_parameters > 0){
		for (my $i=0; $i< $bov_parameters; $i++){
			push(@bov_par_labels,'BOV par '.$self->parameters()->[$i]);
		}
	}
	if ($n_time_varying > 0){
		for (my $i=0; $i< $n_time_varying; $i++){
			push(@bov_cov_labels,'BOV cov '.$self->time_varying()->[$i]);
		}
	}

	##########################################################################################
	#Create Data check model
	##########################################################################################
	
	$data_check_model = $frem_model0 ->  copy( filename    => $self -> directory().'m1/'.$name_check_model,
											   output_same_directory => 1,
											   write_copy => 0,
											   copy_datafile   => 0,
											   copy_output => 0);
	

	# have filtered data so can skip old accept/ignores. Need ignore=@ since have a header
	#have only one $PROB by input check
	$data_check_model->datafiles(problem_numbers => [1],
								 new_names => [$self -> directory().'m1/'.$data2name]);
	$data_check_model->problems->[0]->datas->[0]->ignoresign('@');
	$data_check_model->remove_option( record_name => 'data',
									  option_name => 'ACCEPT',
									  fuzzy_match => 1);
	$data_check_model->set_option( record_name => 'data',
								   option_name => 'IGNORE',
								   option_value => '('.$fremtype.'.GT.0)',
								   fuzzy_match => 1);
	
	foreach my $item (@{$self->extra_input_items()}){
		$data_check_model -> add_option(problem_numbers => [1],
										record_name => 'input',
										option_name => $item);
	}
	$data_check_model ->_write();

	my @run1_models=();
	my $run1_message = "\nExecuting ";
	my $run1_name = '';

	if ((not defined $output_0) and (($self->estimate() >= 0) or ($self->check()))){
		#estimate model 0 if no lst-file found and estimation is requested or check of dataset is requested
		push(@run1_models,$frem_model0);
		$run1_message = 'FREM Model 0 ';
		$run1_message .= 'and ' if ($self->check());
		$run1_name='model0';
		$run1_name .= '_' if ($self->check());
	}

	if ($self->check()){
		$run1_message .= 'Data2 check model';
		push(@run1_models,$data_check_model);
		$run1_name .= 'data2check';
	}
	if (scalar (@run1_models)>0){
	  ##########################################################################################
	  #Estimate model 0 and/or data check model
	  ##########################################################################################
		my $run1_fit = tool::modelfit ->
			new( %{common_options::restore_options(@common_options::tool_options)},
				 base_directory	 => $self -> directory(),
				 directory		 => $self -> directory().'/'.$run1_name.'_modelfit_dir1',
				 copy_data     => 0,
				 models		 => \@run1_models,
				 top_tool              => 0);
#		   raw_results           => undef,
		
		ui -> print( category => 'all', message =>  $run1_message);
		$run1_fit -> run;
		if ($frem_model0 -> is_run()){
			$output_0 = $frem_model0 -> outputs -> [0] ;
		}
	}
	if ($self->check()){
		#compare ofv. print this to log file
		my $mod0_ofv;
		my $check_ofv;
		if ( defined $output_0){
			$mod0_ofv = $output_0->get_single_value(attribute=> 'ofv');
		}else{
			$mod0_ofv = 'undefined';
		}
		if ($data_check_model->is_run()){
			$check_ofv = $data_check_model->outputs -> [0]->get_single_value(attribute=> 'ofv');
		}else{
			$check_ofv = 'undefined';
		}
		print "\nModel 0 ofv is    $mod0_ofv\n";
		print   "Data check ofv is $check_ofv\n";
	}

	##########################################################################################
	#Create Model 1
	##########################################################################################

	#here we use original data file. It has been copied before to m1
	$frem_model1 = $frem_model0 ->  copy( filename    => $self -> directory().'m1/'.$name_model1,
										  output_same_directory => 1,
										  write_copy => 0,
										  copy_datafile   => 0,
										  copy_output => 0);
	
	#Update inits from output, if any
	if (defined $output_0){
		$frem_model1 -> update_inits ( from_output => $output_0,
									   problem_number => 1);
		$model1_updated=1;
	}

	if ($n_invariant > 0){
		#	  print "using empirical omega fill\n";
		my $BSV_par_block = $frem_model1-> problems -> [0]->get_filled_omega_matrix(start_eta => $start_eta);
		
		@leading_omega_records = ();
		for (my $i=0; $i< ($start_omega_record-1);$i++){
			#if start_omega_record is 1 we will push nothing
			#if no invariant stuff we will push all
			push(@leading_omega_records,$frem_model1-> problems -> [0]->omegas->[$i]);
		}

		#reset $start_omega_record and on, not not kill all
		$frem_model1 -> problems -> [0]-> omegas(\@leading_omega_records);
	
		$frem_model1-> problems -> [0]->add_omega_block(new_omega => $BSV_par_block,
														labels => \@bsv_par_labels);
	
	}

	$frem_model1 ->_write();

	##########################################################################################
	#Create Model 2
	##########################################################################################

	$frem_model2 = $frem_model1 ->  copy( filename    => $self -> directory().'m1/'.$name_model2_all,
										  output_same_directory => 1,
										  copy_datafile   => 0,
										  write_copy => 0,
										  copy_output => 0);

	#SETUP
	my $ntheta = $frem_model2 ->nthetas(problem_number => 1);
	my $epsnum = 1 + $frem_model2->problems()->[0]->nsigmas(with_correlations => 0,
															with_same => 1);
	
	#DATA changes
	#we want to clear all old options from DATA
	$frem_model2->problems->[0]->datas->[0]->options([]);
	$frem_model2->problems->[0]->datas->[0]->ignoresign('@');
	$frem_model2->datafiles(problem_numbers => [1],
							new_names => [$self -> directory().'m1/'.$data2name]);

	#set theta omega sigma code input

	$self->set_frem_records(model => $frem_model2,
							n_time_varying => $n_time_varying,
							n_invariant => $n_invariant,
							start_eta => $start_eta,
							epsnum => $epsnum,
							ntheta => $ntheta,
							vpc => 0,
							bsv_parameters => $bsv_parameters,
							bov_parameters => $bov_parameters,
							model_type =>2);

	$frem_model2->_write();


	##########################################################################################
	#Create Model 2 only invariant
	##########################################################################################

	if ($n_invariant > 0){
		$frem_model2_invar = $frem_model1 ->  copy( filename    => $self -> directory().'m1/'.$name_model2_invar,
													output_same_directory => 1,
													write_copy => 0,
													copy_datafile   => 0,
													copy_output => 0);

		#DATA changes
		#we want to clear all old options from DATA
		$frem_model2_invar->problems->[0]->datas->[0]->options([]);
		$frem_model2_invar->datafiles(problem_numbers => [1],
									  new_names => [$self -> directory().'m1/'.$data2name]);
		$frem_model2_invar->problems->[0]->datas->[0]->ignoresign('@');
		$frem_model2_invar->add_option( record_name => 'data',
										option_name => 'IGNORE',
										option_value => '('.$fremtype.'.GT.'.$n_invariant.')');
		
		#set theta omega sigma code input
		$self->set_frem_records(model => $frem_model2_invar,
								start_eta => $start_eta,
								n_time_varying => 0,
								n_invariant => $n_invariant,
								bsv_parameters => $bsv_parameters,
								bov_parameters => $bov_parameters,
								epsnum => $epsnum,
								ntheta => $ntheta,
								vpc => 0,
								model_type =>2);
		
		$frem_model2_invar->_write();
	}
	##########################################################################################
	#Create Model 2 only time-varying
	##########################################################################################
	
	if ($n_time_varying > 0){
		#base on model 0
		$frem_model2_timevar = $frem_model0 ->  copy( filename    => $self -> directory().'m1/'.$name_model2_timevar,
													  output_same_directory => 1,
													  write_copy => 0,
													  copy_datafile   => 0,
													  copy_output => 0);
		
		#DATA changes
		#we want to clear all old options from DATA
		$frem_model2_timevar->problems->[0]->datas->[0]->options([]);
		$frem_model2_timevar->datafiles(problem_numbers => [1],
									  new_names => [$self -> directory().'m1/'.$data2name]);
		$frem_model2_timevar->problems->[0]->datas->[0]->ignoresign('@');
		$frem_model2_timevar->add_option( record_name => 'data',
										option_name => 'ACCEPT',
										option_value => '('.$fremtype.'.LT.1,'.$fremtype.'.GT.'.$n_invariant.')');

		#set theta omega sigma code input
		$self->set_frem_records(model => $frem_model2_timevar,
								n_time_varying => $n_time_varying,
								start_eta => $start_eta,
								n_invariant => 0,
								epsnum => $epsnum,
								bsv_parameters => $bsv_parameters,
								bov_parameters => $bov_parameters,
								vpc => 0,
								ntheta => $ntheta,
								model_type =>2);
		
		$frem_model2_timevar->_write();
	}
	##########################################################################################
	#Create Model 3
	##########################################################################################

	$frem_model3 = $frem_model1 ->  copy( filename    => $self -> directory().'m1/'.$name_model3,
										  output_same_directory => 1,
										  write_copy => 0,
										  copy_datafile   => 0,
										  copy_output => 0);

	$frem_model3->problems->[0]->datas->[0]->options([]);
	$frem_model3->datafiles(problem_numbers => [1],
							new_names => [$self -> directory().'m1/'.$data2name]);
	$frem_model3->problems->[0]->datas->[0]->ignoresign('@');
	
	#set theta omega sigma code input
	$self->set_frem_records(model => $frem_model3,
							n_time_varying => $n_time_varying,
							n_invariant => $n_invariant,
							start_eta => $start_eta,
							epsnum => $epsnum,
							ntheta => $ntheta,
							bsv_parameters => $bsv_parameters,
							vpc => 0,
							bov_parameters => $bov_parameters,
							model_type =>3);
#	
	
	$frem_model3->_write();

	##########################################################################################
	#Create Model 1 vpc
	##########################################################################################
	
	$frem_vpc_model1 = $frem_model3 ->  copy( filename    => $self -> directory().'m1/'.$name_modelvpc_1,
											  output_same_directory => 1,
											  copy_datafile   => 0,
											  write_copy => 0,
											  copy_output => 0);      

	$frem_vpc_model1->add_option( record_name => 'data',
								  option_name => 'IGNORE',
								  option_value => '('.$fremtype.'.GT.0)');

	#fix omega
	foreach my $rec (@{$frem_vpc_model1->problems()->[0]->omegas()}){
		$rec->fix(1) unless $rec->same();
	}
	#fix theta 
	foreach my $rec (@{$frem_vpc_model1->problems()->[0]->thetas()}){
		foreach my $opt (@{$rec->options()}){
			$opt->fix(1);
		}
	}
	#unfix all sigma
	foreach my $rec (@{$frem_vpc_model1->problems()->[0]->sigmas()}){
		$rec->fix(0) unless $rec->same();
		foreach my $opt (@{$rec->options()}){
			$opt->fix(0);
		}
	}
	
	$frem_vpc_model1 -> remove_records(type => 'covariance');
	
	#To create combined data simply print table with both filtered input data and new conditional data
	#The conditional headers will have wrong headers in table file to be used as data, but that is fine
	#as long as $INPUT in vpc2 is correct
	my @vpc1_table_params =();
	my @vpc2_input_params =();
	if( defined $frem_vpc_model1->problems()->[0] -> inputs and 
		defined $frem_vpc_model1->problems()->[0] -> inputs -> [0] -> options ) {
		foreach my $option ( @{$frem_vpc_model1->problems()->[0] -> inputs -> [0] -> options} ) {
			unless ( $option -> name eq 'DROP' or $option -> name eq 'SKIP' or
					 $option -> value eq 'DROP' or $option -> value eq 'SKIP' or
					 $option -> name eq $fremtype){
				push(@vpc1_table_params,$option -> name);
				push( @vpc2_input_params, $option -> name );
			}
		}
	} else {
		croak("Trying to construct table for filtering data".
			  " but no headers were found in \$INPUT" );
	}
	
	foreach my $par (@CTV_parameters){
		push(@vpc1_table_params,$par);
		push( @vpc2_input_params, 'CTV'.$par);
	}
	
	$frem_vpc_model1 -> add_records( type           => 'table',
									 record_strings => [ join( ' ', @vpc1_table_params ).
														 ' NOAPPEND NOPRINT ONEHEADER FORMAT=sG15.7 FILE='.$joindata]);
	
	$frem_vpc_model1->_write();

	##########################################################################################
	#Create Model 2 vpc
	##########################################################################################
	
	$frem_vpc_model2 = $frem_model1 ->  copy( filename    => $self -> directory().'m1/'.$name_modelvpc_2,
											  output_same_directory => 1,
											  write_copy => 0,
											  copy_datafile   => 0,
											  copy_output => 0);

	$frem_vpc_model2->ignore_missing_data(1);

	#DATA changes
	$frem_vpc_model2->problems->[0]->datas->[0]->options([]);
	$frem_vpc_model2->datafiles(problem_numbers => [1],
								new_names => [$self -> directory().'m1/'.$joindata]);
	$frem_vpc_model2->problems->[0]->datas->[0]->ignoresign('@');

	$self->set_frem_records(model => $frem_vpc_model2,
							n_time_varying => $n_time_varying,
							n_invariant => $n_invariant,
							start_eta => $start_eta,
							epsnum => $epsnum,
							ntheta => $ntheta,
							bsv_parameters => $bsv_parameters,
							vpc => 1,
							bov_parameters => $bov_parameters,
							model_type =>2);

	my $new_input_string = join(' ',@vpc2_input_params);
	$frem_vpc_model2->problems()->[0]->set_records(type => 'input',
												   record_strings => [$new_input_string]);



	#fix theta  ??
	#must at least fix the ones that are replace by CTVPAR, since NM error otherwise?
	foreach my $rec (@{$frem_vpc_model2->problems()->[0]->thetas()}){
		foreach my $opt (@{$rec->options()}){
			$opt->fix(1);
		}
	}
	#fix all sigma ??
	#foreach my $rec (@{$frem_vpc_model2->problems()->[0]->sigmas()}){
	#	$rec->fix(1) unless $rec->same();
	#}
	
	$frem_vpc_model2 -> remove_records(type => 'covariance');
	
	
	#replace TVpar with CTVpar
	my @code;
	@code = @{$frem_vpc_model2 -> pk( problem_number => 1 )};
	my $use_pred = 0;
	unless ( $#code > 0 ) {
		@code = @{$frem_vpc_model2 -> pred( problem_number => 1 )};
		$use_pred = 1;
	}
	if ( $#code <= 0 ) {
		croak("Neither PK or PRED defined in vpc model 1");
	}

	foreach my $par (@CTV_parameters){
		my $ctv = 'CTV'.$par;
		my $tv = 'TV'.$par;
		my $found = 0;
		for (my $i=0; $i<scalar(@code); $i++) {
			next if ( $code[$i] =~ /^\s*\;/); #comment line
			if ( $code[$i] =~ /^([^;]*)\b$tv\s*=/ ){
				$code[$i] = $1.$tv.'='.$ctv;
				$found = 1;
				#do not break here, may be multiple definitions, replace all
			}
		}
		croak("could not find where to set\n".$tv.'='.$ctv) unless $found;
	}
	if ( $use_pred ) {
		$frem_vpc_model2 -> pred( problem_number => 1,
								  new_pred       => \@code );
	} else {
		$frem_vpc_model2 -> pk( problem_number => 1,
								new_pk         => \@code );
	}
	
	$frem_vpc_model2->_write();

	my @dumper_names=qw(n_invariant n_time_varying n_occasions total_orig_etas start_omega_record use_pred start_eta bsv_parameters bov_parameters *occasion_labels *bsv_par_labels *bsv_cov_labels *bov_par_labels *bov_cov_labels model1_updated *CTV_parameters);
	open(FH, '>'.$self->done_file()) or die "Could not open file ".$self->done_file()." for writing.\n";
	print FH Data::Dumper->Dump(
		[$n_invariant,$n_time_varying,$n_occasions,$total_orig_etas,$start_omega_record,$use_pred,$start_eta,$bsv_parameters,$bov_parameters,\@occasion_labels,\@bsv_par_labels,\@bsv_cov_labels,\@bov_par_labels,\@bov_cov_labels,$model1_updated,\@CTV_parameters],
		\@dumper_names
		);
	close FH;
	
}


sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	my $model = $self -> models -> [$model_number-1];


	#declare variables
	#these should be same set as "variables to store" in create_template_models
	my $n_invariant;
	my $n_time_varying;
	my $n_occasions;
	my $total_orig_etas;
	my $start_omega_record;
	my $use_pred;
	my $start_eta;
	my $bsv_parameters;
	my $bov_parameters;
	my @occasion_labels;
	my @bsv_par_labels; 
	my @bsv_cov_labels; 
	my @bov_par_labels;
	my @bov_cov_labels;
	my $model1_updated;
	my @CTV_parameters;

	#read from done_file, do eval on string to slurp variable values
	my $rundir;

	unless (-e $self->done_file()){
		$self->create_template_models( model => $self -> models -> [$model_number-1]);
	}
	unless (-e $self->done_file()){
		croak("Could not find ".$self->done_file());
	}
	open(FH, $self->done_file()) or croak("Could not open file ".$self->done_file()." for reading.\n");
	my $string = join(' ',<FH>);
	close(FH);
	eval $string;
	
    my $frem_model0 = model -> new ( %{common_options::restore_options(@common_options::model_options)},
									 filename                    => 'm1/'.$name_model0,
									 ignore_missing_output_files => 1 );
    my $frem_model1 = model -> new ( %{common_options::restore_options(@common_options::model_options)},
									 filename                    => 'm1/'.$name_model1,
									 ignore_missing_output_files => 1 );
    my $frem_model2 = model -> new ( %{common_options::restore_options(@common_options::model_options)},
									 filename                    => 'm1/'.$name_model2_all,
									 ignore_missing_output_files => 1 );
    my $frem_model2_timevar;

	if (-e 'm1/'.$name_model2_timevar){
		$frem_model2_timevar = model -> new ( %{common_options::restore_options(@common_options::model_options)},
											  filename                    => 'm1/'.$name_model2_timevar,
											  ignore_missing_output_files => 1 );
	}
	my $frem_model2_invar;

	if (-e 'm1/'.$name_model2_invar){
		$frem_model2_invar = model -> new ( %{common_options::restore_options(@common_options::model_options)},
											filename                    => 'm1/'.$name_model2_invar,
											ignore_missing_output_files => 1 );
	}

    my $frem_model3 = model -> new ( %{common_options::restore_options(@common_options::model_options)},
									 filename                    => 'm1/'.$name_model3,
									 ignore_missing_output_files => 1 );

    my $frem_vpc_model1 = model -> new ( %{common_options::restore_options(@common_options::model_options)},
										 filename                    => 'm1/'.$name_modelvpc_1,
										 ignore_missing_output_files => 1 );
    my $frem_vpc_model2 = model -> new ( %{common_options::restore_options(@common_options::model_options)},
										 filename                    => 'm1/'.$name_modelvpc_2,
										 ignore_missing_data =>1,
										 ignore_missing_output_files => 1 );


#	my $data2_data_record;
	my $output_0;
	my $output_1;
	my $output_2;
	my $output_3;
	my $output_vpc_1;
	my $eta_covariance_3;
	my @leading_omega_records=();
	my $eta_covariance_0;
	my $eta_covariance_1;
	my $eta_covariance_2;



	##########################################################################################
	#Run Model 0
	##########################################################################################

	if ($frem_model0->is_run()){
		$output_0 = $frem_model0 -> outputs -> [0];
	}elsif ($self->estimate() >= 0 and (not $model1_updated)){
		$rundir = $self -> directory().'/model_0_modelfit_dir1';
		rmtree([ "$rundir" ]) if (-e $rundir);

		my $zero_fit = tool::modelfit ->
			new( %{common_options::restore_options(@common_options::tool_options)},
				 base_directory	 => $frem_model0 -> directory(),
				 directory		 => $rundir,
				 models		 => [$frem_model0],
				 copy_data    => 0,
				 top_tool              => 0);
		
		ui -> print( category => 'all', message => "\nExecuting FREM Model 0" );
		$zero_fit -> run;
		$output_0 = $frem_model0 -> outputs -> [0] if ($frem_model0 -> is_run());
	}

	
	##########################################################################################
	#Update and run Model 1
	##########################################################################################

	if ($frem_model1->is_run()){
		$output_1 = $frem_model1 -> outputs -> [0];
	}else{
		if (defined $output_0 and (not $model1_updated)){
			$frem_model1 -> update_inits ( from_output => $output_0,
										   ignore_missing_parameters => 1,
										   update_fix => 1,
										   skip_output_zeros => 1,
										   problem_number => 1);
			$frem_model1->_write(overwrite => 1);
		}
		if ($self->estimate() >= 1){
			#estimate model 1 if estimation is requested
			$rundir = $self -> directory().'/model1_modelfit_dir1';
			rmtree([ "$rundir" ]) if (-e $rundir);
			my $one_fit = tool::modelfit ->
				new( %{common_options::restore_options(@common_options::tool_options)},
					 base_directory	 => $frem_model1 -> directory(),
					 directory		 => $rundir,
					 copy_data    => 0,
					 models		 => [$frem_model1],
					 top_tool              => 0);
			
			ui -> print( category => 'all', message => "\nExecuting FREM Model 1" );
			$one_fit -> run;
			$output_1 = $frem_model1 -> outputs -> [0] if ($frem_model1 -> is_run());
		}
	}

	##########################################################################################
	#Update and run Model 2
	##########################################################################################

	if ($frem_model2->is_run()){
		$output_2 = $frem_model2 -> outputs -> [0];
	}else{
		if (defined $output_1){
			$frem_model2 -> update_inits ( from_output => $output_1,
										   ignore_missing_parameters => 1,
										   update_fix => 1,
										   skip_output_zeros => 1,
										   problem_number => 1);
			$frem_model2 ->_write(overwrite => 1);
		}

		if ($self->estimate() >= 2){
			#estimate model 2 if estimation is requested
			$rundir = $self -> directory().'/model2_modelfit_dir1';
			rmtree([ "$rundir" ]) if (-e $rundir);
			my $two_fit = tool::modelfit ->
				new( %{common_options::restore_options(@common_options::tool_options)},
					 base_directory	 => $frem_model2 -> directory(),
					 directory		 => $rundir,
					 copy_data      => 0,
					 models		 => [$frem_model2],
					 top_tool              => 0);
			
			ui -> print( category => 'all', message => "\nExecuting FREM Model 2" );
			$two_fit -> run;
			$output_2 = $frem_model2 -> outputs -> [0] if ($frem_model2 -> is_run());
		}
	}

	##########################################################################################
	#Update Model 2 only invariant
	##########################################################################################

	#Update inits from output, if any
	if (defined $output_1 and defined $frem_model2_invar){
		$frem_model2_invar -> update_inits ( from_output => $output_1,
											 ignore_missing_parameters => 1,
											 problem_number => 1);
		$frem_model2_invar->_write(overwrite => 1);
	}
   

	##########################################################################################
	#Update Model 2 only time-varying
	##########################################################################################

	#Update inits from output, if any
	#based on mod 0, there should be no fill in of block as in mod 1
	if (defined $output_0 and defined $frem_model2_timevar and (not $model1_updated)){
		$frem_model2_timevar -> update_inits ( from_output => $output_0,
											   ignore_missing_parameters => 1,
											   update_fix => 1,
											   skip_output_zeros => 1,
											   problem_number => 1);
		$frem_model2_timevar->_write(overwrite => 1);
	}
	
	
	
	##########################################################################################
	#Update and run Model 3
	##########################################################################################

	if ($frem_model3->is_run()){
		$output_3 = $frem_model3 -> outputs -> [0];
	}else{
		#Update inits from output, if any
		if (defined $output_2){
			
			$frem_model3 -> update_inits ( from_output => $output_2,
										   ignore_missing_parameters => 0,
										   update_fix => 1,
										   skip_output_zeros => 0,
										   update_omegas => 0,
										   update_sigmas => 1,
										   update_thetas => 1,
										   problem_number => 1);

			@leading_omega_records=();
			my $mod3_bov_record = $start_omega_record; #record number in mod 3, model to update
			$mod3_bov_record = $start_omega_record+1 if($n_invariant > 0);
			if ($mod3_bov_record > 1){
				#update all *before* bov blocks
				$frem_model3 -> update_inits ( from_output => $output_2,
											   ignore_missing_parameters => 1,
											   update_fix => 1,
											   skip_output_zeros => 1,
											   update_omegas => 1,
											   update_sigmas => 0,
											   update_thetas => 0,
											   start_record => 1,
											   end_record => ($mod3_bov_record-1),
											   problem_number => 1);
				
			}


			#create new BOV_all_occ1 to add
			if ($n_time_varying > 0){
				for (my $i=0; $i< ($mod3_bov_record-1);$i++){
					push(@leading_omega_records,$frem_model3-> problems -> [0]->omegas->[$i]);
				}
				#reset omega
				$frem_model3 -> problems -> [0]-> omegas(\@leading_omega_records);
				#first update intits in mod2
				$frem_model2 -> update_inits ( from_output => $output_2,
											   ignore_missing_parameters => 0,
											   update_fix => 1,
											   problem_number => 1);

				my $mod2_bov_record=$start_omega_record;
				$mod2_bov_record=$start_omega_record+2 if ($n_invariant>0);
				my $BOV_par_occ1 = $frem_model2->problems->[0]->get_record_matrix(type => 'omega',
																				  row_format => 1,
																				  record_number => $mod2_bov_record);
				my $BOV_cov_occ1 = $frem_model2->problems->[0]->get_record_matrix(type => 'omega',
																				  row_format => 1,
																				  record_number => ($mod2_bov_record+$n_occasions));

				#create full block to add to mod3
				my $dim1 = scalar(@{$BOV_par_occ1});
				my $new_size = $dim1+scalar(@{$BOV_cov_occ1});
				my $BOV_all_occ1=[];
				for (my $i=0 ; $i< $new_size; $i++){
					push(@{$BOV_all_occ1},[(0) x $new_size]);
				}
				for (my $i=0 ; $i< $dim1; $i++){
					for (my $j=0 ; $j<=$i; $j++){
						$BOV_all_occ1->[$i][$j] = $BOV_par_occ1->[$i][$j];
					}
				}
				for (my $i=$dim1; $i< $new_size; $i++){
					for (my $j=0 ; $j<$dim1; $j++){
						$BOV_all_occ1->[$i][$j] = $smallval;
					}
					for (my $j=$dim1 ; $j<=$i; $j++){
						$BOV_all_occ1->[$i][$j] = $BOV_cov_occ1->[$i-$dim1][$j-$dim1];
					}
				}
				my @all_labels = @bov_par_labels;
				push(@all_labels,@bov_cov_labels);
				$frem_model3 -> problems -> [0]-> add_omega_block(new_omega => $BOV_all_occ1,
																  labels=>\@all_labels);

				#BOV_all_occ2-end
				for (my $i=1; $i< $n_occasions; $i++){
					$frem_model3 -> add_records (type => 'omega',
												 record_strings => ['BLOCK SAME ;'.$occasion_labels[$i]]);
					
				}

			}

			$frem_model3->_write(overwrite => 1);
		}elsif (defined $output_1){ 
			$frem_model3 -> update_inits ( from_output => $output_1,
										   ignore_missing_parameters => 1,
										   update_fix => 1,
										   skip_output_zeros => 1,
										   problem_number => 1);
			$frem_model3->_write(overwrite => 1);
		}


		if ($self->estimate() >= 3){
			#estimate model 3 if estimation is requested
			$rundir = $self -> directory().'/model3_modelfit_dir1';
			rmtree([ "$rundir" ]) if (-e $rundir);
			my $three_fit = tool::modelfit ->
				new( %{common_options::restore_options(@common_options::tool_options)},
					 base_directory	 => $frem_model3 -> directory(),
					 directory		 => $rundir,
					 copy_data		 => 0,
					 models		 => [$frem_model3],
					 top_tool              => 0);
			
			ui -> print( category => 'all', message => "\nExecuting FREM Model 3" );
			$three_fit -> run;
			$output_3 = $frem_model3 -> outputs -> [0] if ($frem_model3 -> is_run());
		}
	}

	if ($self->vpc()){
		##########################################################################################
		#Update and run  Model 1 vpc
		##########################################################################################

		if ($frem_vpc_model1->is_run()){
			$output_vpc_1 = $frem_vpc_model1 -> outputs -> [0];
		}else{

			#Update inits from output3, if any
			if (defined $output_3){
				$frem_vpc_model1 -> update_inits ( from_output => $output_3,
												   ignore_missing_parameters => 1,
												   update_fix => 1,
												   problem_number => 1);
				$frem_vpc_model1->_write(overwrite => 1);
			}
			$rundir = $self -> directory().'/vpc1_modelfit_dir1';
			rmtree([ "$rundir" ]) if (-e $rundir);

			my $vpc1_fit = tool::modelfit ->
				new( %{common_options::restore_options(@common_options::tool_options)},
					 base_directory	 => $frem_vpc_model1 -> directory(),
					 directory	 => $rundir,
					 copy_data	 => 0,
					 models		 => [$frem_vpc_model1],
					 top_tool              => 0);
			
			ui -> print( category => 'all', message => "\nExecuting FREM vpc model 1" );
			$vpc1_fit -> run;
			$output_vpc_1 = $frem_vpc_model1 -> outputs -> [0] if ($frem_vpc_model1 -> is_run());
			
		}
		unless (-e $frem_vpc_model1->directory().$joindata){
			die ($frem_vpc_model1->directory().$joindata." does not exist\n");
		}

		##########################################################################################
		#Update Model 2 vpc
		##########################################################################################


		#Update inits from output_vpc_1, if any
		#otherwise no point in doing anything
		if (defined $output_vpc_1){
			@leading_omega_records=();
			$frem_vpc_model2 -> update_inits ( from_output => $output_vpc_1,
											   ignore_missing_parameters => 1,
											   update_fix => 1,
											   skip_output_zeros => 0,
											   update_omegas => 0,
											   update_sigmas => 1,
											   update_thetas => 1,
											   problem_number => 1);
			if ($start_omega_record > 1){
				#update all *before* start_omega_record  
				$frem_vpc_model2 -> update_inits ( from_output => $output_vpc_1,
												   ignore_missing_parameters => 1,
												   update_fix => 1,
												   skip_output_zeros => 0,
												   update_omegas => 1,
												   update_sigmas => 0,
												   update_thetas => 0,
												   start_record => 1,
												   end_record => ($start_omega_record-1),
												   problem_number => 1);
				for (my $i=0; $i< ($start_omega_record-1);$i++){
					push(@leading_omega_records,$frem_vpc_model2-> problems -> [0]->omegas->[$i]);
				}

			}
			#reset $start_omega_record and on, not not kill all
			$frem_vpc_model2 -> problems -> [0]-> omegas(\@leading_omega_records);

			#compute conditional omega blocks

			#first update intits in vpc1. NEcessary since fix????
			$frem_vpc_model1 -> update_inits ( from_output => $output_vpc_1,
											   ignore_missing_parameters => 0,
											   update_fix => 1,
											   problem_number => 1);
	
			my $bov_record;
			#TODO can get something not pos def from here.... round up diagonals in get_record_matrix???
			if ($n_invariant > 0){
				$bov_record = $start_omega_record+1;
				my $BSV_all = $frem_vpc_model1->problems->[0]->get_record_matrix(type => 'omega',
																				 row_format => 0,
																				 record_number => $start_omega_record);
				my $new_BSV_par = [];
				my $res = linear_algebra::frem_conditional_omega_block($BSV_all,$bsv_parameters,$new_BSV_par);
				if ($res == 1){
					print "\nError when calling frem_conditional_omega_block for BSV, probably BSV_all part of omega from Model 3 ".
						"was not positive definite. Take care of this manually and restart frem.\n";
					exit;
				}
				if ($res == 2){
					croak("\nInput error when calling frem_conditional_omega_block for BSV, this is a bug.\n");
				}
			
				$frem_vpc_model2 -> problems -> [0]-> add_omega_block(new_omega => $new_BSV_par,
																	  labels => \@bsv_par_labels);
			}else{
				$bov_record = $start_omega_record;
			}
			if ($n_time_varying > 0){
				my $BOV_all_occ1 = $frem_vpc_model1->problems->[0]->get_record_matrix(type => 'omega',
																					  row_format => 0,
																					  record_number => $bov_record);

				my $new_BOV_par_occ1 = [];
				my $res = linear_algebra::frem_conditional_omega_block($BOV_all_occ1,$bov_parameters,$new_BOV_par_occ1);
				if ($res == 1){
					print "\nError when calling frem_conditional_omega_block for BOV, ".
						"probably BOV_all_occ1 part of omega from Model 3 ".
						"was not positive definite. Take care of this manually and restart frem.\n";
					exit;
				}
				if ($res == 2){
					croak("\nInput error when calling frem_conditional_omega_block for BOV, this is a bug.\n");
				}

				$frem_vpc_model2 -> problems -> [0]-> add_omega_block(new_omega => $new_BOV_par_occ1,
																	  labels => \@bov_par_labels);
				#add block same
				#BOV_par_occ2-end
				for (my $i=1; $i< $n_occasions; $i++){
					$frem_vpc_model2 -> add_records (type => 'omega',
													 record_strings => ['BLOCK SAME ;'.$occasion_labels[$i]]);
					
				}
			}

			$frem_vpc_model2->_write(overwrite => 1);
			#copy model and move data to final names in run dir
			cp($frem_vpc_model2->full_name(),$name_vpc_final);
			mv($frem_vpc_model1->directory().$joindata,$joindata);

		}else{
			croak("No output from frem_vpc1 run, cannot create final vpc model");

		}


	}#end if vpc
}

sub _modelfit_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $subroutine;


    #this is just a placeholder
my ($dir,$file) = 
    OSspecific::absolute_path( $self -> directory,
			       $self -> raw_results_file->[$model_number-1] );
my ($npdir,$npfile) = 
    OSspecific::absolute_path( $self -> directory,
			       $self -> raw_nonp_file->[$model_number -1]);


#my $orig_mod = $self -> models[$model_number-1];
$subroutine = sub {

  my $modelfit = shift;
  my $mh_ref   = shift;
  my %max_hash = %{$mh_ref};
  
};
return $subroutine;


	return \&subroutine;
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};


}

sub prepare_results
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		arg1  => { isa => 'Int', optional => 1 }
	);
}

sub create_data2
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'Ref', optional => 0 },
							  filename => { isa => 'Str', optional => 0 },
							  bov_parameters => { isa => 'Int', optional => 0 }
		);
	#filename created is module global $data2name

	my $model = $parm{'model'};
	my $filename = $parm{'filename'};
	my $bov_parameters = $parm{'bov_parameters'};

	#in ref of model, 
	#filename of new filter model
	#out name of data file $outdatafile with full path

	my $filtered_data_model = $model -> copy ( filename => $filename,
											   output_same_directory => 1,
											   write_copy => 0,
											   copy_datafile          => 0,
											   copy_output        => 0);

	die "no problems" unless defined $filtered_data_model->problems();
	die "more than one problem" unless (scalar(@{$filtered_data_model->problems()})==1);

	$self->filtered_datafile('filtered_plus_type0.dta');

	my @filter_table_header;

	#need to handle DROP SKIP without value
	my $dummycounter=0;
	if( defined $filtered_data_model->problems()->[0] -> inputs and 
		defined $filtered_data_model->problems()->[0] -> inputs -> [0] -> options ) {
		foreach my $option ( @{$filtered_data_model->problems()->[0] -> inputs -> [0] -> options} ) {
			if ($option->name eq 'DROP' or $option->name eq 'SKIP'){
				if (defined $option->value and $option->value ne '' and not ($option->value eq 'DROP' or $option->value eq 'SKIP') ){
					push( @filter_table_header, $option -> value );
				}else{
					#simple drop used in $INPUT without name. Set dummy name as placeholder here.
					$dummycounter++;
					$option->value('DUMMY'.$dummycounter);
					push( @filter_table_header, $option -> value );
				}
			}else{
				push( @filter_table_header, $option -> name );
			}
		}
	} else {
		croak("Trying to construct table for filtering data".
			" but no headers were found in \$INPUT" );
	}
	my $new_input_string = join(' ',@filter_table_header);
	#do not want to drop anything, keep everything for table. Unset DROP in $INPUT of the model
	$filtered_data_model->problems()->[0]->set_records(type => 'input',
		record_strings => [$new_input_string]);

	$self->typeorder([$self->dv()]); #index 0 is original obs column name
	if (scalar(@{$self->invariant()})>0){
		push(@{$self->typeorder()},@{$self->invariant()}); #add list of invariant covariate names to typeorder
	}
	my $first_timevar_type = scalar(@{$self->typeorder()});
	if (scalar(@{$self->time_varying()})>0){
		push(@{$self->typeorder()},@{$self->time_varying()}); #add list of time_varying covariate names to typeorder
	}
	my @cov_indices = (-1) x scalar(@{$self->typeorder()}); #initiate with invalid indices

	my $evid_index;
	my $mdv_index;
	my $type_index;
	my $occ_index;
	for (my $i=0; $i< scalar(@filter_table_header); $i++){
		if ($filter_table_header[$i] eq 'EVID'){
			$evid_index = $i;
		}elsif($filter_table_header[$i] eq 'MDV'){
			$mdv_index = $i;
		}elsif($filter_table_header[$i] eq $fremtype){
			$type_index = $i;
		}elsif($filter_table_header[$i] eq $self->occasion()){
			$occ_index = $i;
		}else{
			#0 is dv
			for (my $j=0; $j< scalar(@cov_indices); $j++){
				if($filter_table_header[$i] eq $self->typeorder()->[$j]){
					$cov_indices[$j] = $i;
					last;
				}
			}
		}
	}
	my $add_mdv=0;
	unless (defined $evid_index or defined $mdv_index){
		push(@filter_table_header,'MDV');
		$mdv_index = $#filter_table_header;
		push(@{$self->extra_input_items()},'MDV');
		$add_mdv=1;
	}
	if (defined $type_index){
		croak($fremtype." already defined in input model, not allowed.");
	}else{
		push(@filter_table_header,$fremtype);
		$type_index = $#filter_table_header;
		push(@{$self->extra_input_items()},$fremtype);
	}
	unless (defined $occ_index or ($bov_parameters<1)){
		croak("occasion column ".$self->occasion()." not found in input model.");
	}
	if ($cov_indices[0] < 0){
		croak("dependent value ".$self->dv()." not found in input model.");
	}
	for (my $j=1; $j< scalar(@cov_indices); $j++){
		if ($cov_indices[$j] < 0){
			croak("covariate column ".$self->typeorder()->[$j]." not found in input model.");
		}
	}

	my $message;
	if ($add_mdv){
		#cannot have dummy model, NONMEM cannot append MDV
		foreach my $remove_rec ('simulation','covariance','table','scatter','estimation'){
			$filtered_data_model -> remove_records(type => $remove_rec);
		}
		my @code;
		@code = @{$filtered_data_model -> pk( problem_number => 1 )};
		my $use_pred = 0;
		unless ( $#code > 0 ) {
			@code = @{$filtered_data_model -> pred( problem_number => 1 )};
			$use_pred = 1;
		}
		if ( $#code <= 0 ) {
			croak("Neither PK or PRED defined in model 0");
		}
		push(@code,$fremtype.'=0');
		if ( $use_pred ) {
			$filtered_data_model -> pred( problem_number => 1,
										  new_pred       => \@code );
		} else {
			$filtered_data_model -> pk( problem_number => 1,
										new_pk         => \@code );
		}
		
		$message = "Running with MAXEVAL=0 to filter data and add MDV ".$fremtype." for Data set 2";
	}else{
		foreach my $remove_rec ('abbreviated','msfi','contr','subroutine','prior','model','tol','infn','omega','pk','aesinitial','aes','des','error','pred','mix','theta','sigma','simulation','estimation','covariance','nonparametric','table','scatter'){
			$filtered_data_model -> remove_records(type => $remove_rec);
		}
		
		$filtered_data_model -> add_records(type => 'pred',
											record_strings => [$fremtype.'=0','Y=THETA(1)+ETA(1)+EPS(1)']);
		
		$filtered_data_model -> add_records(type => 'theta',
											record_strings => ['1']);
		$filtered_data_model -> add_records(type => 'omega',
											record_strings => ['1']);
		$filtered_data_model -> add_records(type => 'sigma',
											record_strings => ['1']);
		$message = "Running dummy model to filter data and add ".$fremtype." for Data set 2";
	}
	$filtered_data_model -> add_records(type => 'estimation',
										record_strings => ['MAXEVALS=0 METHOD=ZERO']);

	# set $TABLE record

	$filtered_data_model -> add_records( type           => 'table',
		record_strings => [ join( ' ', @filter_table_header ).
			' NOAPPEND NOPRINT ONEHEADER FORMAT=sG15.7 FILE='.$self->filtered_datafile]);

	$filtered_data_model->_write();
	# run model in data_filtering_dir clean=3
	my $filter_fit = tool::modelfit -> new
		( %{common_options::restore_options(@common_options::tool_options)},
		  base_directory => $self->directory(),
		  directory      => $self->directory().'/create_data2_dir/',
		  models         => [$filtered_data_model],
		  top_tool       => 0,
		  copy_data      => 0,
		  clean => 2  );
	ui -> print( category => 'all',
				 message  => $message,
				 newline => 1 );
	$filter_fit -> run;

	my $resultref = data::frem_compute_covariate_properties(directory  => $filtered_data_model->directory,
															filename => $self->filtered_datafile,
															idcolumn => $model->idcolumns->[0],
															invariant_covariates => $self->invariant,
															occ_index => $occ_index,
															data2name => $data2name,
															evid_index => $evid_index,
															mdv_index => $mdv_index,
															type_index => $type_index,
															cov_indices => \@cov_indices,
															first_timevar_type => $first_timevar_type,
															missing_data_token => $self->missing_data_token());
	
	if (defined $resultref){
		$self->occasionlist($resultref->{'occasionlist'}) if (defined $resultref->{'occasionlist'}); 
		$self->invariant_median($resultref->{'invariant_median'}) if (defined $resultref->{'invariant_median'});
		$self->invariant_covmatrix($resultref->{'invariant_covmatrix'}) if (defined $resultref->{'invariant_covmatrix'});
		$self->timevar_median($resultref->{'timevar_median'}) if (defined $resultref->{'timevar_median'});
		$self->timevar_covmatrix($resultref->{'timevar_covmatrix'}) if (defined $resultref->{'timevar_covmatrix'});
	}
}

sub set_frem_records
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'Ref', optional => 0 },
							  n_invariant => { isa => 'Int', optional => 0 },
							  n_time_varying => { isa => 'Int', optional => 0 },
							  model_type => { isa => 'Int', optional => 0 },
							  vpc => { isa => 'Bool', optional => 0 },
							  start_eta => { isa => 'Int', optional => 0 },
							  epsnum => { isa => 'Int', optional => 0 },
							  ntheta => { isa => 'Int', optional => 0 },
							  bsv_parameters => { isa => 'Int', optional => 0 },
							  bov_parameters => { isa => 'Int', optional => 0 }
		);
	my $model = $parm{'model'}; #this will always be model1
	my $n_invariant = $parm{'n_invariant'};
	my $n_time_varying = $parm{'n_time_varying'};
	my $model_type = $parm{'model_type'};
	my $epsnum = $parm{'epsnum'};
	my $ntheta = $parm{'ntheta'};
	my $bsv_parameters = $parm{'bsv_parameters'};
	my $bov_parameters = $parm{'bov_parameters'};
	my $start_eta = $parm{'start_eta'};
	my $vpc = $parm{'vpc'};

    #in is ref of model
    #model_type 2 or 3
    #local number n_time_varying. If 0 then locally ignore bov_parameters
    #local number n_invariant
    #epsnum
    #ntheta
    #this sets theta omega sigma code input

    unless ($model_type == 2 or $model_type ==3){
		croak("invalid model_type $model_type input to set_frem_records");
    }
    my $n_occasions = scalar(@{$self->occasionlist()});

    #INPUT changes. This would be undone for vpc2, but does not hurt to do it here
	if (not $vpc){
		foreach my $item (@{$self->extra_input_items()}){
			#mdv and fremtype
			$model -> add_option(problem_numbers => [1],
								 record_name => 'input',
								 option_name => $item);
		}
	}
    
    #SIGMA changes
    $model->add_records(type => 'sigma',
						problem_numbers => [1],
						record_strings => ['0.0000001 FIX ; EPSCOV']);


    #THETA changes from Mod 1
	
	if (not $vpc){
		my @theta_strings =();
		for (my $i=0; $i< $n_invariant; $i++){
			my $val=$self->invariant_median()->[$i];
			$val=0.001 if ($val==0);
			push(@theta_strings,' '.sprintf("%.12G",$val).'; TV'.$self->invariant()->[$i]);
		}
		for (my $i=0; $i< $n_time_varying; $i++){
			my $val=$self->timevar_median()->[$i];
			$val=0.001 if ($val==0);
			push(@theta_strings,' '.sprintf("%.12G",$val).'; TV'.$self->time_varying()->[$i]);
		}
		$model->add_records(type => 'theta',
							problem_numbers => [1],
							record_strings => \@theta_strings);
	}

    #OMEGA changes starting from Mod1
	#BSV part
    if (($n_invariant > 0) and (not $vpc)){
		#this is BSV_cov
		my @labels=();
		foreach my $cov (@{$self->invariant()}){
			push(@labels,"BSV cov $cov");
		}
		$model-> problems -> [0]->add_omega_block(new_omega => $self->invariant_covmatrix(),
												  labels => \@labels);
		if($model_type == 3){
			#replace with BSV_all (full block from BSV_par + BSV_cov
			my $BSV_all_block = $model-> problems -> [0]->get_filled_omega_matrix(start_eta => $start_eta);

			my $start_omega_record = $model-> problems -> [0]->check_start_eta(start_eta => $start_eta);
			my @leading_omega_records=();
			for (my $i=0; $i< ($start_omega_record-1);$i++){
				#if start_omega_record is 1 we will push nothing
				push(@leading_omega_records,$model-> problems -> [0]->omegas->[$i]);
			}
			#reset $start_omega_record and on, do not kill all
			$model -> problems -> [0]-> omegas(\@leading_omega_records);
			
			$model -> problems -> [0]->add_omega_block(new_omega => $BSV_all_block);
		}
    }


    #OMEGA changes starting from Mod1
	#BOV part
    if ($n_time_varying > 0){

		if ($model_type == 2){ #2 or vpc2
			my $BOV_par_block;
			my @bovlabels=();
			for (my $i=0 ; $i< $bov_parameters; $i++){
				push(@{$BOV_par_block},[($smallval) x $bov_parameters]);
				$BOV_par_block->[$i][$i] = 0.01;
				push(@bovlabels,'BOV par '.$self->parameters()->[$i]);
			}
			#BOV_par_occ1
			$model-> problems -> [0]->add_omega_block(new_omega => $BOV_par_block,
													  labels => \@bovlabels);
			#BOV_par_occ2-end
			for (my $i=1; $i< $n_occasions; $i++){
				$model -> add_records (type => 'omega',
									   record_strings => ['BLOCK SAME ; '.$self->occasion().'='.$self->occasionlist()->[$i]]);
				
			}
			if (not $vpc){
				#BOV_cov, do not add this for vpc2
				#BOV_cov_occ1
				my @labels=();
				foreach my $cov (@{$self->time_varying()}){
					push(@labels,"BOV cov $cov");
				}
				$model-> problems -> [0]->add_omega_block(new_omega => $self->timevar_covmatrix,
														  labels => \@labels);
				#BOV_cov_occ2-end
				for (my $i=1; $i< $n_occasions; $i++){
					$model -> add_records (type => 'omega',
										   record_strings => ['BLOCK SAME ; '.$self->occasion().'='.$self->occasionlist()->[$i]]);
					
				}
			}
		}elsif ($model_type == 3){
			my $BOV_all_block;
			my @bovlabels=();
			
			for (my $i=0 ; $i< ($bov_parameters+$n_time_varying); $i++){
				push(@{$BOV_all_block},[($smallval) x ($bov_parameters+$n_time_varying)]);
				$BOV_all_block->[$i][$i] = 0.01;
				if ($i < $bov_parameters){
					push(@bovlabels,'BOV par '.$self->parameters()->[$i]);
				}else {
					push(@bovlabels,'BOV cov '.$self->time_varying()->[$i-($bov_parameters)]);
				}
			}
			#replace part with ->timevar_covmatrix 
			for (my $i=0 ; $i<$n_time_varying; $i++){
				for (my $j=0 ; $j<= $i ; $j++){
					$BOV_all_block->[$bov_parameters+$i][$bov_parameters+$j] = 
						$self->timevar_covmatrix()->[$i][$j];
				}
			}

			$model-> problems -> [0]->add_omega_block(new_omega => $BOV_all_block,
													  labels => \@bovlabels);
			for (my $i=1; $i< $n_occasions; $i++){
				$model -> add_records (type => 'omega',
									   record_strings => ['BLOCK SAME ; '.$self->occasion().'='.$self->occasionlist()->[$i]]);
				
			}
		}else{
			croak("bug in loop set_theta_omega_code");
			
		}

    }

	
    my @code;
    @code = @{$model -> pk( problem_number => 1 )};
    my $use_pred = 0;
    unless ( $#code > 0 ) {
		@code = @{$model -> pred( problem_number => 1 )};
		$use_pred = 1;
    }
    if ( $#code <= 0 ) {
		croak("Neither PK or PRED defined in input model");
    }


	
    #PK/PRED changes at beginning A
	my @begin_code =(';;;FREM CODE BEGIN A');
	
	if (not $vpc and ($n_invariant > 0)){
		#this is for BSV_cov
		for (my $i=0; $i< $n_invariant; $i++){
			push(@begin_code,'BSV'.$self->invariant()->[$i].' = ETA('.($start_eta+$bsv_parameters+$i).')' );
		}
	}
	if ($n_time_varying > 0){
		for (my $i=0 ; $i< $bov_parameters; $i++){
			push(@begin_code,'BOV'.$self->parameters()->[$i].' = 0' );
		}

		if (not $vpc){
			for (my $i=0 ; $i< $n_time_varying; $i++){
				push(@begin_code,'BOV'.$self->time_varying()->[$i].' = 0' );
			}
		}
		if ($model_type == 2){
			#for BOV_par
			for (my $i=0; $i< $n_occasions; $i++){
				push(@begin_code,'IF ('.$self->occasion().'.EQ.'.$self->occasionlist()->[$i].') THEN' );
				my $offset = ($start_eta-1)+$bsv_parameters+$n_invariant+$i*$bov_parameters;
				if ($vpc){
					#smaller offset if vpc
					$offset = ($start_eta-1)+$bsv_parameters+$i*$bov_parameters;
				}
				for (my $j=0 ; $j< $bov_parameters; $j++){
					push(@begin_code,'   BOV'.$self->parameters()->[$j].' = ETA('.($offset+$j+1).')');
				}
				push(@begin_code,'END IF' );
			}
			
			if (not $vpc){
				#BOV_cov
				for (my $i=0; $i< $n_occasions; $i++){
					push(@begin_code,'IF ('.$self->occasion().'.EQ.'.$self->occasionlist()->[$i].') THEN' );
					my $offset = ($start_eta-1)+$bsv_parameters+$n_invariant;
					$offset = $offset+$bov_parameters*$n_occasions+$i*$n_time_varying;
					for (my $j=0 ; $j< $n_time_varying; $j++){
						push(@begin_code,'   BOV'.$self->time_varying()->[$j].' = ETA('.($offset+$j+1).')');
					}
					push(@begin_code,'END IF' );
				}
			}
			
		}elsif ($model_type == 3){
			for (my $i=0; $i< $n_occasions; $i++){
				push(@begin_code,'IF ('.$self->occasion().'.EQ.'.$self->occasionlist()->[$i].') THEN' );
				my $offset = ($start_eta-1)+$bsv_parameters+$n_invariant+$i*($bov_parameters+$n_time_varying);
				for (my $j=0 ; $j< $bov_parameters; $j++){
					push(@begin_code,'   BOV'.$self->parameters()->[$j].' = ETA('.($offset+$j+1).')');
				}
				$offset = $offset+$bov_parameters;
				for (my $j=0 ; $j< $n_time_varying; $j++){
					push(@begin_code,'   BOV'.$self->time_varying()->[$j].' = ETA('.($offset+$j+1).')');
				}
				push(@begin_code,'END IF' );
			}
		}else{
			croak("bug in loop set_theta_omega_code");
		}
	}
	push(@begin_code,';;;FREM CODE END A' );


    my @end_code =();
    #ERROR/PRED changes at end
	if (not $vpc){
		@end_code = (';;;FREM CODE BEGIN C');
		for (my $i=0; $i< $n_invariant; $i++){
			push(@end_code,'Y'.($i+1).' = THETA('.($ntheta+$i+1).') + BSV'.$self->invariant()->[$i]);
		}
		for (my $i=0; $i< $n_time_varying; $i++){
			push(@end_code,'Y'.($n_invariant+$i+1).' = THETA('.($ntheta+$n_invariant+$i+1).') + BOV'.$self->time_varying()->[$i]);
		}
		for (my $i=1; $i<=($n_invariant+$n_time_varying); $i++){
			push(@end_code,'IF ('.$fremtype.'.EQ.'.$i.') THEN' );
			push(@end_code,'   Y = Y'.$i.'+EPS('.$epsnum.')' );
			push(@end_code,'   IPRED = Y'.$i );
			push(@end_code,'END IF' );
		}
		
		push(@end_code,';;;FREM CODE END C' );
	}


    #PK/PRED changes at beginning B: Add BOV on parameters
    if ( $n_time_varying > 0){
		foreach my $parameter (@{$self->parameters()}){
			my $success = 0;
			my $etanum = 0;
			my $bov= 'BOV'.$parameter;
			#	for (reverse  @code ) {
			for (my $i=$#code; $i>=0 ; $i--) {
				next if ( $code[$i] =~ /^\s*;/); #comment line
				$_ = $code[$i];
				if ( /^\s*(\w+)\s*=\s*/ and ($1 eq $parameter) ){
					s/^(\s*\w+\s*=\s*)//;
					my $left = $1;
					my ($right,$comment) = split( ';', $_, 2 );

					if ($right =~ /\bETA\(([0-9]+)\)/){
						#add BOV
						$etanum = $1;
						$right =~ s/ETA\($etanum\)/(ETA($etanum)+$bov)/;
					}elsif ($right =~ /\(0\)/){
						#replace 0 with BOV
						$right =~ s/\(0\)/($bov)/;
					}else{
						croak("Could not find an appropriate place to add $bov on the line ".
							  $parameter."= ...");
					}
					$success=1;
					$code[$i] = $left.$right;
					$code[$i] = $code[$i].';'.$comment if (length($comment)>0);
					last;
				}
				
			}
			unless ( $success ) {
				my $mes = "Could not find $parameter=... line to add $bov\n";
				croak($mes );
			}
		}
    }

    my $found_anchor = -1;
    my $i = 0;
    for ( @code ) {
		if ( /^;;;FREM-ANCHOR/) {
			$found_anchor = $i;
			last;
		}
		$i++
    }
    if ($found_anchor >= 0){
		my @block1 =  (@code[0..$found_anchor]);
		my @block2 =  (@code[($found_anchor+1)..$#code]);
		@code = (@block1,@begin_code,@block2);
    }else{
		unshift(@code,@begin_code);
    }
    
    if ( $use_pred ) {
		push(@code,@end_code);
		$model -> pred( problem_number => 1,
						new_pred       => \@code );
    } else {
		$model -> pk( problem_number => 1,
					  new_pk         => \@code );
		my @error = @{$model -> error( problem_number => 1 )};
		push(@error,@end_code);
		$model -> error( problem_number => 1,
						 new_error         => \@error );
    }
}

sub cleanup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		  arg1 => { isa => 'Int', optional => 1 }
	);

  #remove tablefiles in simulation NM_runs, they are 
  #copied to m1 by modelfit and read from there anyway.
  for (my $samp=1;$samp<=$self->samples(); $samp++){
    unlink $self -> directory."/simulation_dir1/NM_run".$samp."/mc-sim-".$samp.".dat";
    unlink $self -> directory."/simulation_dir1/NM_run".$samp."/mc-sim-".$samp."-1.dat"; #retry
  }

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
