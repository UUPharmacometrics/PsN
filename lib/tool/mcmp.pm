package tool::mcmp;

use include_modules;
use Data::Dumper;
use Math::Random;
use strict;
use tool::modelfit;
use model;
use ui;
use Config;
use File::Copy qw/cp mv/;
use OSspecific;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'samples_hash' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'df' => ( is => 'rw', isa => 'Int', default => 1 );
has 'rounding' => ( is => 'rw', isa => 'Int', default => 1 );
has 'strata_ofv' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'strata_to_index' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'index_to_strata' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'n_individuals' => ( is => 'rw', isa => 'Int' );
has 'significance_level' => ( is => 'rw', isa => 'Num', default => 5 );
has 'significance_index' => ( is => 'rw', isa => 'Int', default => 0 );
has 'simdata' => ( is => 'rw', isa => 'Str' );
has 'table_full' => ( is => 'rw', isa => 'Str' );
has 'table_reduced' => ( is => 'rw', isa => 'Str' );
has 'table_strata' => ( is => 'rw', isa => 'Str' );
has 'n_bootstrap' => ( is => 'rw', isa => 'Int', default => 10000 );
has 'stratify_on' => ( is => 'rw', isa => 'Str' );
has 'increment' => ( is => 'rw', isa => 'Int' );
has 'start_size' => ( is => 'rw', isa => 'Int' );
has 'max_size' => ( is => 'rw', isa => 'Int' );
has 'target_power' => ( is => 'rw', isa => 'Int', default => 98 );
has 'critical_ofv' => ( is => 'rw', isa => 'Num' );
has 'critical_array' => ( is => 'rw', isa => 'ArrayRef[Num]', default => sub { [] } );
has 'algorithm' => ( is => 'rw', isa => 'Int', default => 1 );
has 'curve' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'reduced_model' => ( is => 'rw', isa => 'model' );
has 'full_model' => ( is => 'rw', isa => 'model' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['mcmp.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'mcmp_results.csv' );

sub BUILD
{
	my $self  = shift;

	if (defined $self->table_full){
		unless ( -e $self->table_full ){
			croak("Full model table file ".$self->table_full." could not be found.");
		}
		if (defined $self -> full_model){
			croak("Ambigous input. Cannot define both -table_full and -full_model");
		}
	}elsif (not (defined $self -> full_model)){
		croak("Either -table_full or -full_model must be defined");
	}

	croak("target_power cannot exceed 100%") if
		($self->target_power > 100);

	if (defined $self->simdata){
		croak("Option simdata set to ".$self->simdata.
			  " but file does not exist.\n") unless (-e $self->simdata);
		croak("Cannot set both option -simulation_model and ".
			  "option -simdata in the same run")
			unless ($self -> models -> [0]->filename() eq 'dummy');
	}

	if (defined $self->table_reduced){
		unless ( -e $self->table_reduced ){
			croak("Reduced model table file ".$self->table_reduced." could not be found.");
		}
		if (defined $self -> reduced_model){
			croak("Ambigous input. Cannot define both -table_reduced and -reduced_model");
		}
	}elsif (not (defined $self -> reduced_model)){
		croak("Either -table_reduced or -reduced_model must be defined");
	}

	if (defined $self->table_strata){
		unless ( -e $self->table_strata ){
			croak("Strata table file ".$self->table_strata." could not be found.");
		}
	}elsif (not (defined $self -> reduced_model or defined $self->full_model)){
		croak("When -table_strata is not defined, either -full_model or -reduced_model must be defined") if (defined $self->stratify_on);
	}

	if ($self->df < 1){
		croak("option -df, degrees of freedom, cannot be less than 1");
	}

	unless ($self->algorithm > 0 and $self->algorithm < 3){
		croak("option -algorithm must be 1 or 2");
	}

	if (defined $self->critical_ofv){
		if  ($self->df >1){
			ui -> print (category=>'mcmp', 
						 message=>"Warning: When option -critical_ofv is used, option -df is ignored");
		}
		if  ($self->significance_level != 5){
			ui -> print (category=>'mcmp', 
						 message=>"Warning: When option -critical_ofv is used, option -significance_level is ignored");
		}
	}else{
		if  (not ($self->significance_level == 5
				  or $self->significance_level == 1
				  or $self->significance_level == 0.1)){
			croak("option -significance_level must be either 5, 1 or 0.1");
		}
	}

	if ($self->n_bootstrap < 1){
		croak("option -n_bootstrap cannot be less than 1");
	}

	if ((defined $self->increment) and  $self->increment < 1){
		croak("option -increment cannot be smaller than 1.");
	}
	if ((defined $self->start_size) and  $self->start_size < 1){
		croak("option -start_size cannot be smaller than 1.");
	}

	if (defined $self->max_size){
		if (defined $self->start_size){
			croak("option -start_size cannot be larger than -max_size") if
				($self->start_size > $self->max_size);
		}elsif ((defined $self->increment) and  $self->increment > $self->max_size){ 
			croak("option -increment cannot be larger than -max_size");
		}
	}
	if (defined $self->stratify_on and ($PsN::nm_major_version < 7)){
		croak("Unless NONMEM7 is used, -stratify_on must be at most 4 characters")
			if (length($self->stratify_on)>4);
	}
}

sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 }
		);
	my $model_number = $parm{'model_number'};

	if (defined $self->critical_ofv()){
		if ($self->critical_ofv()<0){
			$self->critical_array([(-1*$self->critical_ofv())]);
		}else{
			$self->critical_array([($self->critical_ofv())]);
		}
		$self->significance_index(0);
	}else{
		#store table
		my %df_table;
		#,,,"Chi-Square Table ",
		#,"one-tailed",0.050,0.010,0.001
		$df_table{1}=[(3.84,6.63490,10.828)];
		$df_table{2}=[(5.99,9.21,13.816)];
		$df_table{3}=[(7.81,11.34,16.266)];
		$df_table{4}=[(9.49,13.28,18.467)];
		$df_table{5}=[(11.07,15.09,20.515)];
		$df_table{6}=[(12.59,16.81,22.458)];
		$df_table{7}=[(14.07,18.48,24.322)];
		$df_table{8}=[(15.51,20.09,26.125)];
		$df_table{9}=[(16.9190,21.6660,27.877)];
		$df_table{10}=[(18.3070,23.2093,29.588)];
		$df_table{11}=[(19.68,24.7250,31.264)];
		$df_table{12}=[(21.03,26.2170,32.909)];
		$df_table{13}=[(22.36,27.6883,34.528)];
		$df_table{14}=[(23.68,29.1413,36.123)];
		$df_table{15}=[(25,30.5779,37.697)];
		$df_table{16}=[(26.3,31.9999,39.252)];
		$df_table{17}=[(27.59,33.4087,40.790)];
		$df_table{18}=[(28.87,34.8053,42.312)];
		$df_table{19}=[(30.14,36.1908,43.820)];
		$df_table{20}=[(31.41,37.5662,45.315)];
		$df_table{21}=[(32.67,38.9321,46.797)];
		$df_table{22}=[(33.92,40.2894,48.268)];
		$df_table{23}=[(35.17,41.6384,49.728)];
		$df_table{24}=[(36.42,42.9798,51.179)];
		$df_table{25}=[(37.65,44.3141,52.620)];
		$df_table{26}=[(38.89,45.6417,54.052)];
		$df_table{27}=[(40.11,46.9630,55.476)];
		$df_table{28}=[(41.34,48.2782,56.892)];
		$df_table{29}=[(42.56,49.5879,58.302)];
		$df_table{30}=[(43.77,50.8922,59.703)];
		$df_table{40}=[(55.76,63.6907,73.402)];
		$df_table{50}=[(67.5,76.1539,86.661)];
		$df_table{60}=[(79.08,88.3794,99.607)];
		$df_table{70}=[(90.53,100.425,112.317)];
		$df_table{80}=[(101.88,112.329,124.839)];
		$df_table{90}=[(113.15,124.116,137.208)];
		$df_table{100}=[(124.34,135.807,149.449)];
		croak("No internal value for critical ofv at ".$self->df().
			  " degrees of freedom") unless (defined $df_table{$self->df()});
		$self->critical_array($df_table{$self->df()});

		if ($self->significance_level() == 5){
			$self->significance_index(0);
		}elsif ($self->significance_level() == 1){
			$self->significance_index(1);
		}elsif ($self->significance_level() == 0.1){
			$self->significance_index(2);
		}else{
			croak("Illegal value for -significance_level");
		}

	}



	$self->target_power($self->round(number=> $self->target_power()));

	return if (defined $self->table_full() and defined $self->table_reduced());

	my $simulated_file;
	my $time_in_input=0;
	my $datx_in_input=0;
	my @table_header=();
	unless ($self -> models -> [0]->is_dummy){
		my $sim_model = $self -> models -> [0] ->copy( filename    => $self -> directory.'m1/simulation.mod',
													   copy_datafile   => 1,
													   write_copy => 0,
													   copy_output => 0);
		
		if ($sim_model-> is_option_set(record=>'input',name=>'TIME')){
			#this assumes no synonym, and TIME is always option, not value.
			$time_in_input=1;
		}
		foreach my $col ('DATE','DAT1','DAT2','DAT3'){
			if ($sim_model-> is_option_set(record=>'input',name=>$col)){
				#this assumes no synonym, and name always options, not value.
				$datx_in_input=1;
				last;
			}
		}

		#set IGNORE=@ since datafile will
		#get a header during copying. Keep IGNORE=LIST
		for (my $probi=0; $probi < scalar(@{$sim_model->problems}); $probi++){
			$sim_model->problems->[$probi]->datas->[0]->ignoresign('@');
		}

		my $prob = $sim_model -> problems -> [0];

		# set $SIMULATION record

		my $sim_record = $sim_model -> record( problem_number => 1,
											   record_name => 'simulation' );

		if( scalar(@{$sim_record}) > 0 ){
			my @new_record;
			foreach my $sim_line ( @{$sim_record -> [0]} ){
				my $new_line;
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
		} else {

			my $seed = random_uniform_integer( 1, 0, 1000000 ); # Upper limit is from nmhelp
			$prob -> set_records( type           => 'simulation',
								  record_strings => [ '(' . $seed .
													  ') ONLYSIMULATION' ] );
		}

		if( $sim_model -> is_option_set( problem_number => 1,record => 'estimation',
										 name => 'LIKELIHOOD',fuzzy_match => 1 )
			or
			$sim_model -> is_option_set( problem_number => 1,record => 'estimation',
										 name => '-2LOGLIKELIHOOD',fuzzy_match => 1 )
			or
			$sim_model -> is_option_set( problem_number => 1, record => 'estimation',
										 name => '-2LLIKELIHOOD',fuzzy_match => 1 )
			or
			$sim_model -> is_option_set( problem_number => 1, record => 'estimation',
										 name => 'LAPLACIAN',fuzzy_match => 1 )
			){
			#set_nopred_onlysim
			unless ($sim_model -> is_option_set( problem_number => 1,record => 'simulation',
												 name => 'NOPREDICTION',fuzzy_match => 1 )){
				$sim_model -> set_option(record_name => 'simulation',
										 option_name => 'NOPRED');
			}
			unless ($sim_model -> is_option_set( problem_number => 1,record => 'simulation',
												 name => 'ONLYSIMULATION',fuzzy_match => 1 )){
				$sim_model -> set_option(record_name => 'simulation',
										 option_name => 'ONLYSIM');
			}
		}

		# remove $EST and $COV
		$prob -> remove_records(type => 'estimation');
		$prob -> remove_records(type => 'covariance');
		$prob -> remove_records(type => 'nonparametric');

		# set $TABLE record
		#when copying $INPUT to $TABLE: remove DATX
		#if not TIME present, remove TIME
		#if not TIME present but DATX then add back TIME at the end

		@table_header=();
		if( defined $prob -> inputs and defined $prob -> inputs -> [0] -> options ) {
			foreach my $option ( @{$prob -> inputs -> [0] -> options} ) {
				push( @table_header, $option -> name ) unless 
					(($option -> value eq 'DROP' or $option -> value eq 'SKIP'
					  or $option -> name eq 'DROP' or $option -> name eq 'SKIP') ||
					 ($option -> name =~ /DAT(E|1|2|3)/) ||
					 ((not $time_in_input) && ($option -> name =~ /TIME/)));
			}
			if ((not $time_in_input) && ($datx_in_input )){
				push( @table_header, 'TIME');
			}
		} else {
			croak("Trying to construct table for monte-carlo simulation".
				  " but no headers were found in \$model_number-INPUT" );
		}

		$simulated_file = "mcmp-sim.dat";
		$prob -> set_records( type           => 'table',
							  record_strings => [ join( ' ', @table_header ).
												  ' NOPRINT NOAPPEND ONEHEADER FILE='.
												  $simulated_file ] );
		$sim_model -> _write();

		my $mod_sim = tool::modelfit -> new( %{common_options::restore_options(@common_options::tool_options)},
											 top_tool         => 0,
											 models           => [$sim_model],
											 base_directory   => $self -> directory,
											 directory        => $self -> directory.
											 'simulation_dir'.$model_number, 
											 retries          => 1,
											 logfile	         => undef,
											 raw_results           => undef,
											 prepared_models       => undef,
											 threads          => 1,
											 copy_data        => 0);
		ui -> print (category=>'mcmp', message=> "Simulating data:");
		$mod_sim -> run;
		unless (-e $self -> directory.'m1/'."$simulated_file"){
			croak("It appears the simulation part of mcmp failed.".
				  " (The file ".$self -> directory.'m1/'."$simulated_file"." is missing.)".
				  " Check the raw_results file in ".$self -> directory.".");
		}

	}

	my @estimate_models;
	my @table_strings;

	if (defined $self->stratify_on() and (not defined $self->table_strata())){
		$self->table_strata('strata.tab');
		@table_strings = ('ID',$self->stratify_on(),'FIRSTONLY','NOAPPEND',
						  'ONEHEADER','NOPRINT',
						  'FILE='.$self->table_strata());
	}
	#reduced model
	if (defined $self->reduced_model()){
		$self->reduced_model()->directory($self->directory().'m1/');
		$self->reduced_model()->filename('reduced.mod');
		$self->reduced_model() -> ignore_missing_files( 1 );
		$self->reduced_model() -> outputfile( $self -> reduced_model()->directory().'reduced.lst');
		$self->reduced_model() -> ignore_missing_files( 0 );
		my @extra_output=();
		if (scalar(@table_strings)>0){
			$self->reduced_model() -> add_records(type => 'table',
												  record_strings => \@table_strings);
			push(@extra_output,$self->table_strata());
		}
		$self->table_strata($self -> reduced_model()->directory().'reduced.'.$self->table_strata());
		if ($PsN::nm_major_version < 7){
			push(@extra_output,'iotab1');
		}else{
			push(@extra_output,'psn.phi');
		}
		$self->reduced_model()->extra_output(\@extra_output);

		push(@estimate_models,$self->reduced_model());
	}
	if (defined $self->full_model()){
		$self->full_model()->directory($self->directory().'m1/');
		$self->full_model()->filename('full.mod');
		$self->full_model() -> ignore_missing_files( 1 );
		$self->full_model() -> outputfile( $self -> full_model()->directory().'full.lst');
		$self->full_model() -> ignore_missing_files( 0 );
		my @extra_output=();
		if ((scalar(@table_strings)>0) and not (defined $self->reduced_model())){
			$self->full_model()-> add_records(type => 'table',
											  record_strings => \@table_strings);
			push(@extra_output,$self->table_strata());
			$self->table_strata($self -> full_model()->directory().'full.'.$self->table_strata());
		}
		if ($PsN::nm_major_version < 7){
			push(@extra_output,'iotab1');
		}else{
			push(@extra_output,'psn.phi');
		}
		$self->full_model()->extra_output(\@extra_output);
		push(@estimate_models,$self->full_model());
	}


	foreach my $mod (@estimate_models){
		if (defined $simulated_file or defined $self->simdata()){
			#remove any DATX in $INPUT 
			foreach my $col ('DATE','DAT1','DAT2','DAT3'){
				$mod -> remove_option(record_name => 'input',
									  problem_numbers => [(1)],
									  option_name => $col);
			}
			#if added time then remove TIME (if present) and then add TIME (must be last in list)
			if ((not $time_in_input) && ($datx_in_input)){
				$mod -> remove_option(record_name => 'input',
									  problem_numbers => [(1)],
									  option_name => 'TIME');
				$mod -> set_option(record_name => 'input',
								   problem_numbers => [(1)],
								   option_name => 'TIME');
			}

			$mod -> remove_records(problem_numbers => [(1)],
								   type => 'simulation' );

			#ignore @ since simdata contains header rows. 
			#keep old ignores. It is up to the user to make sure datasets are comparable
			for (my $probi=0; $probi < scalar(@{$mod->problems}); $probi++){
				$mod->problems->[$probi]->datas->[0]->ignoresign('@');
			}


			$mod->ignore_missing_files(1);
			my $sim_file;
			if (defined $self->simdata()){
				#simdata has global path, fixed in mcmp
				my $dirt;
				($dirt, $simulated_file) =
					OSspecific::absolute_path('', $self -> simdata() );
				cp($self -> simdata(),$self -> directory.'m1/'.$simulated_file);
			}
			$sim_file= $self -> directory.'m1/'.$simulated_file;
			my @new_names = ($sim_file) x scalar(@{$mod ->problems});
			$mod -> datafiles(new_names => \@new_names); #one for each $PROB

		}else{
			ui -> print (category=>'mcmp', message=> "\n Warning: No simulation data defined\n");
		}

		$mod -> _write;
	}

	#set filename/dir to m1, set extra_output iotab eller phi
	#_write
	my %hash = %{common_options::restore_options(@common_options::tool_options)};
	my $nmoutopt = $hash{'nm_output'};
	if (defined $nmoutopt and length($nmoutopt)>0){
		$nmoutopt .= ',phi'; #ok to append even if there already
	}else{
		$nmoutopt .= 'phi';
	}

	$self->tools([]) unless (defined $self->tools);
	push( @{$self -> tools},
		  tool::modelfit ->
		  new( %{common_options::restore_options(@common_options::tool_options)},
			   base_directory	 => $self -> directory,
			   directory		 => $self -> directory.
			   '/modelfit_dir'.$model_number,
			   models		 => \@estimate_models,
			   parent_threads        => 1,
			   raw_results           => undef,
			   prepared_models       => undef,
			   top_tool              => 0,
			   nm_output => $nmoutopt,
			   prepend_model_file_name => 1,
			   copy_data => 0
		  ) );
	ui -> print (category=>'mcmp', message=> "\nEstimating:");
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Num', optional => 1 }
		);
	my $model_number = $parm{'model_number'};

	$self -> read_data(); #creates $self variables

	my %strata_size;
	my %strata_ofv = %{$self->strata_ofv};
	$self->strata_ofv({});
	my %strata_to_index = %{$self->strata_to_index};
	$self->strata_to_index({});
	my %index_to_strata = %{$self->index_to_strata};
	$self->index_to_strata({});
	my $n_strata = scalar(keys %strata_ofv);
	for (my $i=0;$i< $n_strata; $i++){
		$strata_size{$i}=scalar(@{$strata_ofv{$i}});
	}

	unless (defined $self->increment()){
		$self->increment($n_strata);
	}
	if ($self->increment() < $n_strata){
		print "\nWarning\nThe option increment (".$self->increment().") is set smaller than the number ".
			"of strata ($n_strata),\n".
			"which does not make sense according to the intended use of this option (see the userguide).\n".
			"The results will be correct but the computations may be inefficient.\n\n";
		sleep(2);
	}

	my $n_critical = scalar(@{$self->critical_array});


	if (-e $self->results_file()){
		my $fname = $self->results_file();
		$fname =~ s/\.csv$// ;

		my $addnum=1;
		while (-e $self->directory."/$fname"."-old$addnum".'.csv'){
			$addnum++;
		}

		my $newname = "$fname"."-old$addnum".'.csv';
		mv( $fname.'.csv', $newname);

		ui -> print (category=>'mcmp', 
					 message=>"Renamed old $fname".
					 ".csv to $newname to protect old output. New output is $fname".".csv.");
	}



	open( RES, ">".$self->results_file()) or die "could not open ".$self->results_file();
	print RES "total_X,";
	if ($n_critical == 1){
		print RES "power";
	}else{
		print RES "power at 5%,power at 1%,power at 0.1%";
	}
	if (defined $self->stratify_on()){
		foreach my $strata (0 .. ($n_strata-1)){
			print RES ",N ".$self->stratify_on()."=".(sprintf "%d",$index_to_strata{$strata});
		}
	}
	print RES "\n";
	print "total_X,power\n";

	unless (defined $self->start_size()){
		$self->start_size(3*$self->increment());
	}

	my $n_consecutive=0;

	my @indices;
	my $converged = 0;    
	my @last_N=(0,0);
	my @last_Y=(0,0);
	my $step_index=0;
	my $n_consecutive=0;
	my %best;
	$best{'N_below'}=0;
	$best{'N_above'}=$self->max_size();
	$best{'Y_below'}=0;
	$best{'Y_above'}=2;


	while (not $converged){
		my @n_above_critical= (0) x $n_critical;
		my $goal_total_samples;
		if ($self->curve()){
			#get next total samples by simple stepping
			$goal_total_samples = ($self->start_size()+$step_index*$self->increment());
			$step_index++;
			$converged = 1 if (($self->start_size()+$step_index*$self->increment()) 
							   > $self->max_size()); #do not go beyond max_size regardless of results
		}else{
			#get next total samples by secant method
			$goal_total_samples= $self->get_total_samples('last_N' => \@last_N,
														  'last_Y' => \@last_Y);
			last if ($goal_total_samples < 1); #error code
		}

		my $total_samples=0;
		my @strata_N= (0) x $n_strata;


		if ($self->algorithm()==1 ){
			#alg 1
			my @bootstrap_ofv = (0) x $self->n_bootstrap();

			foreach my $strata (0 .. ($n_strata-1)){
				my $strata_samples= 
					$self->round(number=>($goal_total_samples*$strata_size{$strata}/$self->n_individuals));
				$strata_N[$strata] = $strata_samples;
				$total_samples +=$strata_samples;
				foreach (1 .. $strata_samples){
					@indices = random_uniform_integer($self->n_bootstrap(),0,($strata_size{$strata}-1));
					foreach my $j (0 .. ($self->n_bootstrap()-1)){
						$bootstrap_ofv[$j] += $strata_ofv{$strata}->[$indices[$j]];
					}
				}
			}

			if ($total_samples == 0){
				if ($self->curve()){
					next;
				}else{
					die "error alg 1 no-curve\n";
				}
			}

			foreach my $val (@bootstrap_ofv){
				foreach my $k (0 .. ($n_critical-1)){
					last if $val <= $self->critical_array->[$k];
					$n_above_critical[$k]++; 
				}
			}
		} else {
			#algorithm 2
			foreach my $str (0 .. ($n_strata-1)){
				my $strata_samples= 
					$self->round(number=>($goal_total_samples*$strata_size{$str}/$self->n_individuals));
				$strata_N[$str] = $strata_samples;
				$total_samples +=$strata_samples;
			}
			if ($total_samples == 0){
				if ($self->curve()){
					next;
				}else{
					die "error alg 2 no-curve\n";
				}
			}

			my $delta_ofv=0;
			my $strata;
			my @indices=();
			foreach my $i (1 .. $self->n_bootstrap()){
				$delta_ofv=0;
				foreach $strata (0 .. ($n_strata-1)){
					@indices = random_uniform_integer($strata_N[$strata],0,($strata_size{$strata}-1));
					foreach my $ind (@indices){
						$delta_ofv += $strata_ofv{$strata}->[$ind];
					}
				}

				foreach my $k (0 .. ($n_critical-1)){
					last if $delta_ofv <= $self->critical_array->[$k];
					$n_above_critical[$k]++; 
				}
			}
		} #end algorithm 2

		################
		#print results
		my @row=("$total_samples");
		foreach my $k (0 .. ($n_critical-1)){
			push(@row,sprintf "%5.1f",($n_above_critical[$k]*100/$self->n_bootstrap()));
		}
		print "$total_samples,".$row[$self->significance_index+1]."\n";
		if (defined $self->stratify_on()){
			push(@row,@strata_N);
		}
		print RES (join ',',@row)."\n";

		##################
		#convergence tests
		$last_N[1] = $last_N[0];
		$last_N[0]=$total_samples;
		if ($self->curve()){
			if ( ($row[$self->significance_index+1] > $self->target_power())
				 or ($row[$self->significance_index+1] >= 100 )){
				$n_consecutive++;
			}else{
				$n_consecutive=0;
			}
			$converged=1 if ($n_consecutive > 2);
		}else{
			#no curve, secant method instead
			#until two last values are not the same but either adjacent and on
			# either side of power goal or both are at goal if rounded to zero decimals
			$last_Y[1] = $last_Y[0];
			$last_Y[0]= $n_above_critical[$self->significance_index]/$self->n_bootstrap();
			if ($last_Y[0]< ($self->target_power()/100)){
				if ($last_Y[0] >= $best{'Y_below'} and $last_N[0] > $best{'N_below'}){
					$best{'Y_below'} =  $last_Y[0];
					$best{'N_below'} =  $last_N[0];
				}
			}elsif($last_Y[0] <= $best{'Y_above'} and $last_N[0] < $best{'N_above'}){
				$best{'Y_above'} =  $last_Y[0];
				$best{'N_above'} =  $last_N[0];
			}

			if ( abs($best{'N_above'}-$best{'N_below'}) < 1.5*$self->increment()){
				$converged =1;
				print "Convergence achieved.\nTarget power ".$self->target_power()."% is obtained for a total ".
					"sample size between ".$best{'N_below'}." (power ".
					(sprintf "%5.1f",$best{'Y_below'}*100)."%) and ".
					$best{'N_above'}." (power ".
					(sprintf "%5.1f",$best{'Y_above'}*100)."%).\n";
			}
		} #end convergence test no curve

	} #end while not converged 

	print "See mcmp_results.csv for more detailed results.\n";
	close(RES);
	$self->cleanup();
}

sub round
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  number => { isa => 'Num', optional => 0 }
		);
	my $number = $parm{'number'};
	my $integer_out;

	my $floor=int($number);
	my $rem=$number-$floor;
	if ($rem >= 0){
		$integer_out = ($rem >= 0.5)? $floor+1 : $floor;
	} else {
		$integer_out = (abs($rem) >= 0.5)? $floor-1 : $floor;
	}

	return $integer_out;
}

sub ceil
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  number => { isa => 'Num', optional => 0 }
		);
	my $number = $parm{'number'};
	my $integer_out;

	my $floor=int($number);
	my $rem=$number-$floor;
	if ($rem > 0){
		$integer_out = $floor+1;
	} else {
		#equal or  neg
		$integer_out = $floor;
	} 

	return $integer_out;
}

sub get_total_samples
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  last_N => { isa => 'Ref', optional => 1 },
							  last_Y => { isa => 'Ref', optional => 1 }
		);
	my $last_N = $parm{'last_N'};
	my $last_Y = $parm{'last_Y'};
	my $total_samples;

	if ($last_N->[0] == 0){ #most recent step
		#this is the first iteration
		$total_samples = $self->ceil(number=> (10/$self->increment()))*$self->increment();
		$self->rounding(1);
	}elsif ($last_N->[1] == 0){ #step before most recent
		#this is the second iteration
		#second (y range 0 to 1) from y2=exp(-a/N2), a=-ln(y1)*N1, 
		#N2=-a/ln(power_goal)=ln(y1)*N1/ln(power_goal), roudn to multiple of increment 
		#If larger than max_size then warn and reduce to 50 rounded up to mult increment
		die "Y out of range" unless ($last_Y->[0] > 0 and $last_Y->[0]<=1);
		my $N=log($last_Y->[0])*$last_N->[0]/log($self->target_power()/100);
		$total_samples = int($N/$self->increment())*$self->increment(); #round down
		$total_samples = int(50/$self->increment())*$self->increment() 
			if ($total_samples > $self->max_size());
		$self->rounding(-1*$self->rounding);      
	}else{
		#standard iteration
		return -1 unless ($last_Y->[0] > 0 and $last_Y->[0]<=1);
		return -1 unless ($last_Y->[1] > 0 and $last_Y->[1]<=1);
		return -1 if ($last_Y->[0] == $last_Y->[1]);
		my $N=$last_N->[0]+(($self->target_power()/100)-$last_Y->[0])*($last_N->[0]-$last_N->[1])/($last_Y->[0]-$last_Y->[1]);
		$total_samples = $self->ceil(number=> ($N/$self->increment()))*$self->increment();
		$self->rounding(-1*$self->rounding);      
	}
	$self->rounding(1) if($total_samples <= $self->increment());

	while (defined $self->samples_hash->{$total_samples} or ($total_samples < $self->increment())){
		$total_samples += ($self->rounding)*$self->increment();
		$self->rounding(1) if ($total_samples <= 0);
		$self->rounding(-1) if ($total_samples > $self->max_size());
	}
	$self->samples_hash->{$total_samples}=1;
    
	return $total_samples;
}

sub read_data
{
	my $self = shift;

	my ($full_file,$reduced_file);
	my @strata;
	my $n_individuals;
	my $n_strata=1;
	my %index_to_strata_hash;
	my %strata_to_index_hash;

	if (defined $self->table_full()){
		$full_file = $self->table_full();
	} else {
		if ($PsN::nm_major_version < 7){
			$full_file = $self->full_model()->directory().'full.iotab1';
		}else{
			$full_file = $self->full_model()->directory().'full.phi';
		}
	}
	unless ( -e $full_file ){
		croak("File $full_file \nwith iofv output for full model does not exist.");
	}
    
	if (defined $self->table_reduced()){
		$reduced_file = $self->table_reduced();
	} else {
		if ($PsN::nm_major_version < 7){
			$reduced_file = $self->reduced_model()->directory().'reduced.iotab1';
		}else{
			$reduced_file = $self->reduced_model()->directory().'reduced.phi';
		}
	}

	unless ( -e $reduced_file ){
		croak("File $reduced_file \nwith iofv output for reduced model does not exist.");
	}
    
	if (defined $self->stratify_on) {
		unless (-e $self->table_strata) {
			croak("File " . $self->table_strata . " \nwith stratification data does not exist.");
		}
		my $d = data->new(filename => $self->table_strata, ignoresign => '@', idcolumn => 1); #defined format table ID first
		
		$n_individuals= scalar(@{$d->individuals});
		@strata = @{$d -> column_to_array('column'=>$self->stratify_on())};
		$d = undef;
		%index_to_strata_hash = 
			%{$self->create_unique_values_hash(sorted_column => [(sort {$a <=> $b} @strata)])}; 
		$n_strata = scalar(keys %index_to_strata_hash );
		foreach my $key (keys %index_to_strata_hash){
			$strata_to_index_hash{$index_to_strata_hash{$key}}=$key;
		}
	}
	
	my @delta_ofv = (0) x $n_individuals;

	my $line_i=0;
	open(FH, $reduced_file ) or croak("Could not open reduced file.");
	while (<FH>){
		next unless (/^\s*[0-9]/);
		#split on space, take the last value
		my @arr = split;
		$delta_ofv[$line_i]=$arr[-1];
		$line_i++;
	}
	close(FH);
	if (defined $self->stratify_on()){
		if ($line_i != $n_individuals){
			croak("The number of individuals in reduced.phi ($line_i) and strata.tab ($n_individuals) are not the same.\nCheck the files manually.\n".
				  "Possibly rerun reduced.mod manually to investigate the error.\n".
				  "$reduced_file\n".
				  $self->table_strata()."\n");
		}
	}else{
		$n_individuals = $line_i;
	}  

	my %strata_ofv;
	for (my $i=0;$i< $n_strata; $i++){
		$strata_ofv{$i}=[()];
	}
	
	$line_i=0;
	open(FH, $full_file ) or croak("Could not open full file.");
	while (<FH>){
		next unless (/^\s*[0-9]/);
		#split on space, take the last value
		#stratify here already
		my @arr = split;
		if (defined $self->stratify_on()){
			push(@{$strata_ofv{$strata_to_index_hash{$strata[$line_i]}}},
				 ($delta_ofv[$line_i] - $arr[-1]));
		}else{
			push(@{$strata_ofv{0}},($delta_ofv[$line_i] - $arr[-1]));
		}
		$line_i++;
	}
	close(FH);
	if ($line_i != $n_individuals){
		croak("The number of individuals in $reduced_file and ".
			  "$full_file are not the same.");
	}  
	unless (defined $self->max_size()){
		$self->max_size($n_individuals);
	}
	$self->strata_ofv(\%strata_ofv);
	$self->strata_to_index(\%strata_to_index_hash);
	$self->index_to_strata(\%index_to_strata_hash);
	$self->n_individuals($n_individuals);

	ui -> print (category=>'mcmp', message=> "Done reading and stratifying iofv.");
}

sub cleanup
{
	my $self = shift;

	unlink $self->directory . "simulation_dir1/NM_run1/mcmp-sim.dat";
	unlink $self->directory . "simulation_dir1/NM_run1/mcmp-sim-1.dat";
	unlink $self->directory . "modelfit_dir1/NM_run1/iotab1";
	unlink $self->directory . "modelfit_dir1/NM_run1/iotab1-1";
	unlink $self->directory . "modelfit_dir1/NM_run2/iotab1";
	unlink $self->directory . "modelfit_dir1/NM_run2/iotab1-1";
}

sub create_unique_values_hash
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  sorted_column => { isa => 'Ref', optional => 1 }
		);
	my %value_hash;
	my $sorted_column = $parm{'sorted_column'};

	#in @sorted_column
	#out %value_hash
	my $value_index = 0;

	foreach my $val  (@{$sorted_column}){
		if ($value_index == 0){
			$value_hash{$value_index}=$val;
			$value_index++;
			next;
		}
		unless ($val == $value_hash{($value_index-1)}){
			$value_hash{$value_index}=$val;
			$value_index++;
		}
		last if ($val == $sorted_column->[-1])
	}

	return \%value_hash;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
