package tool::gls;

use include_modules;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Config;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'samples' => ( is => 'rw', isa => 'Int' );
has 'gls_model' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'set_simest' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'ind_shrinkage' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'additional_callback' => ( is => 'rw', isa => 'Int', default => 0 );
has 'sim_table' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'reminimize' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'iwres_shrinkage' => ( is => 'rw', isa => 'number' );
has 'additive_theta' => ( is => 'rw', isa => 'number' );
has 'have_nwpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_tnpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'probnum' => ( is => 'rw', isa => 'Int', default => 1 );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['gls.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'gls_results.csv' );

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
							 message => "Warning: gls does not support \$PRIOR NWPRI.",
							 newline => 1);
				last;
			}
		}
	}

	if ( scalar (@{$self -> models->[0]-> problems}) > 2 ){
		croak('Cannot have more than two $PROB in the input model.');
	}elsif  (scalar (@{$self -> models->[0]-> problems}) == 2 ){
		if ((defined $self -> models->[0]-> problems->[0]->priors()) and 
			scalar(@{$self -> models->[0]-> problems->[0] -> priors()})>0 ){
			my $tnpri=0;
			foreach my $rec (@{$self -> models->[0]-> problems->[0] -> priors()}){
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

			$self->have_tnpri(1) if ($tnpri);
		}
		if ($self->have_tnpri()){
			unless( defined $self -> models->[0]-> extra_files ){
				croak('When using $PRIOR TNPRI you must set option -extra_files to '.
					  'the msf-file, otherwise the msf-file will not be copied to the NONMEM '.
					  'run directory.');
			}

		}else{
			croak('The input model must contain exactly one problem, unless'.
				  ' first $PROB has $PRIOR TNPRI');
		}
		my $est_record = $self->models->[0] -> record( problem_number => (1+$self->have_tnpri()),
													   record_name => 'estimation' );
		unless (defined $est_record and scalar(@{$est_record})>0){
			croak('Input model must have an estimation record');
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

	my $model = $self -> models -> [$model_number-1];
	my ( @seed, $new_datas, $skip_ids, $skip_keys, $skip_values );
	my @orig_and_sim_models;
	my $orig_model;
	my $sim_model;
	my @msfo_stems_original;

	$self->probnum(2) if ($self->have_tnpri());

	my @table_header;
	my @all_iwres_files;
	my @orig_table_names;

	my $gls_model;
	my $newthetanum=$model->nthetas(problem_number => $self->probnum())+1;
	my $sim_record;
	my $simdirname='simulation_dir'; 
	my $shrinkage_value;
	$self->additional_callback(0);


	my $gls_estimation_string ='';
	my $sim_simulation_string ='';
	my $sim_estimation_string ='';
	if ($self->set_simest()){
		my @gls_lines;
		my @sim_lines;

		open(MOD, $self-> models->[0]->full_name()) || 
			die("Couldn't open ".$self-> models->[0]->full_name()." : $!");

		while(<MOD>) {
			if (s/^\s*\;+\s*(gls-|Gls-|GLS-)//){
				#removed first part
				my $lead=$1;
				if (s/^(sim|Sim|SIM)\s+;*\s*//){
					push(@sim_lines,$_);
				}elsif(s/^(final|Final|FINAL)\s+;*\s*//){
					push(@gls_lines,$_);
				}else{
					ui->print(message => "Warning: Tag on line ".$lead.$_." not recognized, ignored",
							  newline=> 1);
				}
			}
		}
		close(MOD);

		if (scalar(@gls_lines)>0){
			unless ($gls_lines[0] =~ /^\$EST[A-Z]*/){
				croak("First line of tagged gls-final is ".$gls_lines[0]." which ".
					  "is not recognized as a \$EST record.");
			}
			$gls_lines[0] =~ s/^\$EST[A-Z]*//;
			foreach my $line (@gls_lines){
				if ($line =~ /^\$/){
					croak("Error: More than one line found starting with \n".
						  ";gls-final \$<something>\n".
						  "gls program cannot handle multiple NONMEM ".
						  "records with tag ;gls-final");
				}
				chomp $line;
				$gls_estimation_string .= ' '.$line; 
			}
		}
		if (scalar(@sim_lines)>0){
			unless ($sim_lines[0] =~ /^\$[A-Z]+/){
				croak("First line of tagged gls-sim is ".$sim_lines[0]." which ".
					  "is not recognized as a \$ NONMEM record.");

			}
			my $is_est=0;
			my $is_sim=0;
			foreach my $line (@sim_lines){
				if ($line =~ /^\$/){
					$line =~ s/^\$([A-Z]*)//;
					my $record = $1;
					if ($record =~ /^SIM/){
						croak("Cannot have more than one \$SIM record ".
							  "in combination with tag ;gls-sim")
							if ($is_sim > 0);
						$is_sim=1;
						$is_est = 2 if ($is_est == 1); #stop storing est if did before
					}elsif ($record =~ /^EST/){
						croak("Cannot have more than one \$EST record ".
							  "in combination with tag ;gls-sim")
							if ($is_est > 0);
						$is_est=1;
						$is_sim = 2 if ($is_sim == 1); #stop storing sim if did before.
					}else{
						croak("Cannot have \$"."$record in combination with tag ;gls-sim");
					}
				}
				chomp $line;
				$sim_estimation_string .= ' '.$line if ($is_est == 1); 
				if ($is_sim == 1){
					#remove NSUBS setting, if any
					$line =~ s/SUBP[A-Z]*=[0-9]+//;
					$line =~ s/NSUB[A-Z]*=[0-9]+//;
					$line =~ s/TRUE=[A-Z]+//;
					$sim_simulation_string .= ' '.$line ; 
				}

			}
		}

	}


	if ($self->gls_model()){
		$gls_model = $model ->copy( filename    => $self -> directory.'m'.$model_number.'/gls.mod',
									copy_datafile   => 1,
									write_copy => 0,
									output_same_directory => 1,
									copy_output => 0);

		$gls_model -> remove_option( record_name  => 'estimation',
									 option_name  => 'MSFO',
									 fuzzy_match => 1,
									 problem_numbers => [($self->probnum())],
									 record_number => 0); #0 means all

		if (defined $self->additive_theta()){

			$gls_model -> initial_values( parameter_numbers => [[$newthetanum]],
										  new_values        => [[$self->additive_theta()]],
										  add_if_absent     => 1,
										  parameter_type    => 'theta',
										  problem_numbers   => [$self->probnum()]);
			$gls_model -> labels( parameter_type    => 'theta',
								  parameter_numbers => [[$newthetanum]],
								  problem_numbers   => [$self->probnum()],
								  new_values        => [["$newthetanum add_err"]] );
			$gls_model->fixed(parameter_type => 'theta',
							  parameter_numbers => [[$newthetanum]],
							  new_values => [[1]] );

		}
		if (defined $gls_model ->outputs() and 
			defined $gls_model->outputs()->[0] and
			$gls_model->outputs()->[0]-> have_output()){
			$gls_model -> update_inits ( from_output => $gls_model->outputs()->[0],
										 problem_number => $self->probnum());
		}

	}else{
		#no gls_model
		$orig_model = $model ->	copy( filename    => $self -> directory.'m'.$model_number.'/original.mod',
									  copy_datafile   => 1,
									  output_same_directory => 1,
									  write_copy => 0,
									  copy_output => 0);

		if ($self->ind_shrinkage()){

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
				my @arr=('(000 NEW)');
				$sim_record = \@arr;#dummy seed
			}
			if (length($sim_simulation_string)> 0){
				my @arr=($sim_simulation_string);
				$sim_record = \@arr;
			}
			$sim_record->[0] .= ' SUBPROB=1';

			if ($self->have_nwpri() or $self->have_tnpri()){
				$sim_record->[0] .= ' TRUE=PRIOR';
			}
		} #end if ind_shrinkage

		$orig_model -> remove_option( record_name  => 'estimation',
									  option_name  => 'MSFO',
									  fuzzy_match => 1,
									  problem_numbers => [($self->probnum())],
									  record_number => 0); #0 means all

		if (defined $self->additive_theta()){
			$orig_model -> initial_values( parameter_numbers => [[$newthetanum]],
										   new_values        => [[$self->additive_theta()]],
										   add_if_absent     => 1,
										   parameter_type    => 'theta',
										   problem_numbers   => [$self->probnum()]);
			$orig_model -> labels( parameter_type    => 'theta',
								   parameter_numbers => [[$newthetanum]],
								   problem_numbers   => [$self->probnum()],
								   new_values        => [['additive_error']] );
			$orig_model->fixed(parameter_type => 'theta',
							   parameter_numbers => [[$newthetanum]],
							   new_values => [[1]] );


		}

		$gls_model = $orig_model -> copy( filename    => $self -> directory.'m'.$model_number.'/gls.mod',
										  copy_datafile   => 0,
										  write_copy => 0,
										  output_same_directory => 1,
										  copy_output => 0);
		$gls_model -> remove_records( type => 'simulation' );
		if (length($gls_estimation_string)>1){
			$gls_model -> set_records (type => 'estimation',
									   record_strings => [$gls_estimation_string],
									   problem_numbers => [($self->probnum())]);
		}
		#only allow PLEV if simulating
		if ($self->have_tnpri() or $self->have_nwpri()){
			$gls_model -> remove_option( record_name  => 'prior',
										 problem_numbers => [(1)],
										 option_name  => 'PLEV',
										 fuzzy_match => 1);
		}

		$orig_model -> shrinkage_stats( enabled => 1 );

		$orig_model -> remove_records( type => 'covariance' );

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
												   ' IPRED PRED NOPRINT NOAPPEND ONEHEADER FILE=glsinput.dta']);

		my $orig_model_output;
		if (defined $model ->outputs() and 
			defined $model->outputs()->[0] and
			$model->outputs()->[0]-> have_output()
			and $self->ind_shrinkage()){
			#we do not need to run original before sims, because already have final ests
			$orig_model_output = $model->outputs()->[0];
			$orig_model -> update_inits ( from_output => $orig_model_output,
										  problem_number => $self->probnum(),
										  ignore_missing_parameters => 1);
			$orig_model -> _write(); 
			push( @orig_and_sim_models, $orig_model );
			$simdirname='orig_and_simulation_dir'; 
		}else{
			$orig_model -> _write();
			#run original here to get param estimates for sim
			my $run_orig = tool::modelfit -> new( 
				%{common_options::restore_options(@common_options::tool_options)},
				top_tool         => 0,
				models           => [$orig_model],
				base_directory   => $self -> directory,
				directory        => $self -> directory.'original_dir'.$model_number, 
				parent_tool_id   => $self -> tool_id,
				logfile	         => undef,
				raw_results_file     => [$self ->raw_results_file()->[$model_number-1]],
				prepared_models       => undef,
				shrinkage => 1,
				_raw_results_callback => $self ->
				_modelfit_raw_results_callback( model_number => $model_number ),
				copy_data => 0,
				abort_on_fail => $self->abort_on_fail);

			ui -> print( category => 'gls',
						 message  => "Running original model" );

			$run_orig -> run;
			$self->additional_callback(1);

			unless (defined $run_orig -> raw_results){
				croak("Running original model failed. Check output in ".$run_orig->directory());
			}
			unless (defined $self->iwres_shrinkage() or $self->ind_shrinkage){
				my $cols = scalar @{$run_orig -> raw_results -> [0]}; # first non-header row
				my $line_structure = $run_orig->raw_line_structure;
				my ($start,$len) = split(',',$run_orig->raw_line_structure->{'1'}->{'shrinkage_iwres'});
				croak("undef shrinkage_iwres") unless (defined $start);
				$shrinkage_value = ($run_orig -> raw_results -> [0][$start])/100; #value is in percent
			}

			if (defined $orig_model ->outputs() and 
				defined $orig_model->outputs()->[0] and
				$orig_model->outputs()->[0]-> have_output()){
				$orig_model_output = $orig_model->outputs()->[0];
				$orig_model -> update_inits ( from_output => $orig_model_output,
											  problem_number => $self->probnum());
			}
		}

		if (defined $orig_model_output){
			$gls_model -> update_inits ( from_output => $orig_model_output,
										 problem_number => $self->probnum());
		}

		#change table FILE in gls if table present. Left original model as is.
		my $tbl_nm_ref = 
			$gls_model -> get_option_value( record_name  => 'table',
											option_name  => 'FILE',
											record_index => 'all',
											problem_index => ($self->probnum()-1));

		if( defined $tbl_nm_ref ){
			for (my $k=0; $k<scalar(@{$tbl_nm_ref}); $k++){
				if (defined $tbl_nm_ref->[$k]){
					my $name = $tbl_nm_ref->[$k];
					$name =~ s/[0-9]*$//;
					$gls_model -> remove_option( record_name  => 'table',
												 option_name  => 'FILE',
												 fuzzy_match => 1,
												 record_number => ($k+1));

					$gls_model -> add_option(record_name  => 'table',
											 record_number  => ($k+1),
											 option_name  => 'FILE',
											 problem_numbers => [($self->probnum())],
											 option_value => $name.'-gls' );   
				}
			}
		}

		#ignore @ since simdata contains header rows. can skip old ignores since filtered
		#set for all $PROB
		for (my $probi=0; $probi < scalar(@{$gls_model->problems}); $probi++){
			$gls_model->problems->[$probi]->datas->[0]->ignoresign('@');
		}

		foreach my $modprob (@{$gls_model->problems()}){
			my $inp_ref =  $modprob -> inputs();
			if ( defined $inp_ref and defined $inp_ref -> [0] ) {
				my $input = $inp_ref -> [0];
				my $opt_ref = $input -> options;
				if ( defined $opt_ref ) {
					my @options = @{$opt_ref};
					my @keep;
					foreach my $option ( @options ) {
						push ( @keep, $option ) if ( not ($option -> value eq 'DROP' or $option -> value eq 'SKIP'
														  or $option -> name eq 'DROP' or $option -> name eq 'SKIP'));
					}
					$input -> options( \@keep );
					$input -> _add_option( option_string => 'PIPR' );
					$input -> _add_option( option_string => 'PPRE' );
					if ($self->ind_shrinkage()){
						$input -> _add_option( option_string => 'ISHR' );
					}
				}
			}
		}


	}  #done if not gls_model    

	$gls_model -> add_option( record_name  => 'data',
							  problem_numbers => [($self->probnum())],
							  option_name  => 'IGNORE',
							  option_value => '(PIPR.LE.0.000000001)');

	my $samples=0;
	$samples = $self -> samples() if ($self->ind_shrinkage() and not $self->gls_model());

	for( my $sim_no = 1; $sim_no <= $samples ; $sim_no++ ) {

		my $sim_name = "simulation-$sim_no.mod";
		my $sim_out = "simulation-$sim_no.lst";

		if( $sim_no == 1 ) {
			$sim_model = $orig_model->
				copy( filename    => $self -> directory.'m'.$model_number.'/'.$sim_name,
					  copy_datafile   => 0,
					  write_copy => 0,
					  copy_output => 0);
			$sim_model -> remove_records( type => 'table' );
			$sim_model -> remove_records( type => 'covariance' );
			$sim_model -> shrinkage_stats( enabled => 1 );

			#set IGNORE=@ since datafile will
			#get a header during copying. Keep IGNORE=LIST
			for (my $probi=0; $probi < scalar(@{$sim_model->problems}); $probi++){
				$sim_model->problems->[$probi]->datas->[0]->ignoresign('@');
			}

			# set $TABLE record

			$sim_model -> add_records( type           => 'table',
									   problem_numbers => [($self->probnum())],
									   record_strings => ['IWRES ID NOPRINT NOAPPEND ONEHEADER FILE=dummy']);

			if ($self->sim_table()){
				$sim_model -> add_records( type           => 'table',
										   problem_numbers => [($self->probnum())],
										   record_strings => ['ID TIME IPRED W IWRES NOPRINT ONEHEADER FILE=dummy2']);
			}

			if (length($sim_estimation_string)>1){
				$sim_model -> set_records (type => 'estimation',
										   record_strings => [$sim_estimation_string],
										   problem_numbers => [($self->probnum())]);
			}else{
				unless ($self->reminimize()){
					$sim_model -> set_maxeval_zero(print_warning => 1,
												   last_est_complete => $self->last_est_complete(),
												   niter_eonly => $self->niter_eonly(),
												   need_ofv => 1);
				}
			}

		}else{
			$sim_model = $orig_and_sim_models[$#orig_and_sim_models]->
				copy( filename    => $self -> directory.'m'.$model_number.'/'.$sim_name,
					  copy_datafile   => 0,
					  write_copy => 0,
					  output_same_directory => 1,
					  copy_output => 0);

		}#end if elsesim_no==1
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


		if( $sim_model -> is_option_set( record => 'simulation', 
										 name => 'ONLYSIMULATION',
										 fuzzy_match => 1) ){
			$sim_model -> remove_records( type => 'estimation' );
		}

		my $iwres_file = "iwres-$sim_no.dta";
		$prob -> remove_option( record_name  => 'table',
								option_name  => 'FILE',
								fuzzy_match => 1,
								record_number => 1);

		$prob -> add_option(record_name  => 'table',
							record_number  => 1,
							option_name  => 'FILE',
							option_value => $iwres_file );   

		if ($self->sim_table()){
			my $tab_file = "sdtab-sim$sim_no.dta";
			$prob -> remove_option( record_name  => 'table',
									option_name  => 'FILE',
									fuzzy_match => 1,
									record_number => 2);

			$prob -> add_option(record_name  => 'table',
								record_number  => 2,
								option_name  => 'FILE',
								option_value => $tab_file );   
		}

		push( @all_iwres_files, $self -> directory.'m'.$model_number.'/'.
			  $iwres_file );

		$sim_model -> _write();
		push( @orig_and_sim_models, $sim_model );

		if( $sim_no == $samples ) {
			my $run_sim = tool::modelfit -> new( 
				%{common_options::restore_options(@common_options::tool_options)},
				top_tool         => 0,
				models           => \@orig_and_sim_models,
				base_directory   => $self -> directory,
				directory        => $self -> directory.$simdirname.$model_number, 
				parent_tool_id   => $self -> tool_id,
				nmtran_skip_model => 2,
				logfile	         => undef,
				raw_results_file     => [$self ->raw_results_file()->[$model_number-1]], #change??
				prepared_models       => undef,
				shrinkage => 1,
				_raw_results_callback => $self ->
				_modelfit_raw_results_callback( model_number => $model_number ),
				copy_data =>0,
				abort_on_fail => $self->abort_on_fail);

			ui -> print( category => 'gls',
						 message  => "Running simulations to compute shrinkage" );

			$run_sim -> run;
			$self->additional_callback(2);

			unless (defined $run_sim -> raw_results){
				croak("Running simulations failed. Check output in ".$run_sim->directory());
			}
			my @matrix;
			my @sums;
			my $nsim=0;
			my $append_header;
			foreach my $file (@all_iwres_files){
				#need not do filtering, as long as can handle strange values for nonobs
				#must keep same number of rows in shrinkage col
				open( IWR, $file ) or croak("Could not find $file.");
				$nsim++;
				my $index = 0;
				while (my $row = <IWR>){
					chomp $row;
					next if ($row =~ /TABLE NO/);
					if ($row =~ /IWRE/){
						$append_header = $row."\n";
						next;
					}
					#order is IWRES ID ...
					$row =~ s/^\s*//;
					my ($iwres,$rest)=split(/\s+/,$row,2);
					if ($nsim>1){
						push(@{$matrix[$index]},$iwres);
						$sums[$index] += $iwres;
					}else{
						$matrix[$index]=[$iwres];
						$sums[$index]=$iwres;
					}
					$index++;
				}
				close(IWR);
			}
			my @shrinkage_arr = (' ',' ');
			for (my $i=0;$i<scalar(@sums);$i++){
				my $mean=$sums[$i]/$nsim;
				my $sum_errors_pow2=0;
				foreach my $val (@{$matrix[$i]}){
					$sum_errors_pow2 = $sum_errors_pow2+($val - $mean)**2;
				}
				my $stdev=0;
				unless( $sum_errors_pow2 <= 0 ){
					$stdev= sqrt ($sum_errors_pow2/($nsim-1));
				}
				push(@shrinkage_arr,sprintf("%.8f",(1-$stdev)));
			}
			#append to glsinput.dta, also print to own file
			my $fname = 'm'.$model_number.'/glsinput.dta'; 
			if (-e $fname){
				my @tmp = OSspecific::slurp_file($fname);
				my $first=1;
				open(GLS, ">$fname") || die("Couldn't open $fname : $!");
				open(DAT, ">ind_iwres_shrinkage.dta") || 
					die("Couldn't open ind_iwres_shrinkage.dta : $!");
				print GLS join(' ',@table_header);
				print GLS " PIPR PPRE ISHR\n";
				print DAT "ISHR\n";
				for (my $i=2; $i<scalar(@tmp); $i++){
					chomp $tmp[$i];
					print GLS $tmp[$i]." ".$shrinkage_arr[$i]."\n";
					print DAT $shrinkage_arr[$i]."\n";
				}
				close (GLS);
				close (DAT);
			}else{
				die "$fname does not exist\n";
			}

		}

	} #end loop over number of simulations

	unless ($self->gls_model()){
		my @new_names = ('m1/glsinput.dta') x scalar(@{$gls_model ->problems});
		$gls_model -> datafiles(new_names => \@new_names); #one for each $PROB
	}
	$gls_model -> shrinkage_stats( enabled => 1 );

	my $shrinkage;
	if (defined $self->iwres_shrinkage()){
		$shrinkage = $self->iwres_shrinkage();
	}elsif (not $self->ind_shrinkage()){
		$shrinkage = $shrinkage_value;
		$shrinkage = sprintf("%.8f",$shrinkage);
	}else{
		$shrinkage = 'ISHR'; 
	}
	my @newcode = ("SHRI=$shrinkage\n",
				   "IF(SHRI.LE.0) SHRI = 0\n");
	push(@newcode,"GLSP = SHRI*PPRE + (1-SHRI)*PIPR\n");

	#change W, set GLSP here
	#can look for ADVAN<any number> this way
	my ($advan,$junk) = $gls_model->problems->[0] -> _option_val_pos( record_name => 'subroutine',
																	  name => 'ADVAN',
																	  exact_match => 0);
	my $have_advan = scalar(@{$advan}) > 0;

	my @code;
	my $use_pred=0;
	if( $have_advan ){
		# We have an ADVAN option in $SUBROUTINE, get $ERROR code
		@code = @{$gls_model -> error( problem_number => 1 )};
	}
	unless ($have_advan and ( $#code > 0 )) {
		@code = @{$gls_model -> pred( problem_number => 1 )};
		$use_pred = 1;
	}

	my $found_W;
	my $i = 0;
	for ( @code ) {
		if ( /^\s*W\s*=/) {
			if ( /^\s*W\s*=\s*SQRT\(/ and /IPRED/) {
				$found_W = $i;
				s/IPRED/GLSP/;
				if (defined $self->additive_theta){
					my $newexp = "=SQRT(THETA($newthetanum)**2+";
					s/=\s*SQRT\(/$newexp/;
				}

				#keep looking, may be more than one W definition line
			}else{
				croak("W definition does not match the pattern W = SQRT(...IPRED...)");
			}
		}
		push(@newcode,$_);
		$i++;
	}
	unless ( defined $found_W ) {
		croak("Could not determine a good place to add the GLS code,\n".
			  " i.e. no W= line was found\n" );
	}

	if ( $use_pred ) {
		$gls_model -> pred( problem_number => 1,
							new_pred       => \@newcode );
	} else {
		$gls_model -> pk( problem_number => 1,
						  new_error         => \@newcode );
	}

	$gls_model -> _write(); #data is local


	my $subdir = 'modelfit';

	my @subtools = ();
	@subtools = @{$self -> subtools} if (defined $self->subtools);
	shift( @subtools );
	my %subargs = ();
	if ( defined $self -> subtool_arguments ) {
		%subargs = %{$self -> subtool_arguments};
	}

	$self->stop_motion_call(tool=>'gls',message => "Preparing to run gls model ")
		if ($self->stop_motion());
	$self->tools([]) unless (defined $self->tools);
	push( @{$self -> tools},
		  tool::modelfit -> new(
			  %{common_options::restore_options(@common_options::tool_options)},
			  top_tool         => 0,
			  logfile	         => undef,
			  raw_results_file     => [$self ->raw_results_file()->[0]],
			  prepared_models  => undef,
			  rerun => 1,
			  models         => [$gls_model],
			  copy_data => 0,
			  base_directory => $self -> directory.'/m'.$model_number.'/',
			  directory      => $self -> directory.'/'.$subdir.'_dir'.$model_number,
			  subtools       => $#subtools >= 0 ? \@subtools : undef,
			  shrinkage      => 1,
			  _raw_results_callback => $self ->
			  _modelfit_raw_results_callback( model_number => $model_number ),
			  %subargs ) );

	ui -> print( category => 'gls',
				 message  => "\nRunning gls model" );
}

sub _modelfit_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 }
		);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Use the mc's raw_results file.
	my ($dir,$file) = 
		OSspecific::absolute_path( $self -> directory,
								   $self -> raw_results_file->[$model_number-1] );
	my ($npdir,$npfile) = 
		OSspecific::absolute_path( $self -> directory,
								   $self -> raw_nonp_file->[$model_number-1]);

	$subroutine = sub {
		#can have 2 $PROB if tnpri and est_sim, interesting with 2nd $PROB only
		my $modelfit = shift;
		my $mh_ref   = shift;
		my %max_hash = %{$mh_ref};
		$modelfit -> raw_results_file( [$dir.$file] );
		$modelfit -> raw_nonp_file( [$npdir.$npfile] );
		$modelfit -> raw_results_append( 1 ) if ($self->additional_callback > 0);
		my $totsamples=1;
		$totsamples = $self -> samples() if (defined $self -> samples());


		# a column with run type, original or gls or sim is prepended. 

		#if prior tnpri nothing will be in raw_results for first $PROB, can
		#take first row for model as final estimates as usual, even if
		#it happens to be from second $PROB

		if ( defined $modelfit -> raw_results() ) {
			$self->stop_motion_call(tool=>'gls',message => "Preparing to rearrange raw_results in memory, adding ".
									"model name information")
				if ($self->stop_motion());

			my $n_rows = scalar(@{$modelfit -> raw_results()});

			my $last_model= 0;
			my $sample = 0; 

			if ($self->additional_callback < 1){
				unshift( @{$modelfit -> raw_results_header}, 'run_type' );
			}

			my $type;
			if (($self->additional_callback < 1) and (not $self->gls_model()) ){
				$type='original';
			}elsif ($self->additional_callback == 1 and $self->ind_shrinkage()){
				#never run orig model if gls_model
				$type='simulation';
			}else{
				$type='gls';
			}
			for (my $i=0; $i< $n_rows; $i++){
				my $this_model = $modelfit -> raw_results()->[$i]->[0]; 
				my $step= ($this_model-$last_model);
				if ($last_model > 0 and $step>0){
					$type='simulation';
				}
				if ($step < 0){
					ui -> print( category => 'gls',
								 message  => "Warning: It seems the raw_results is not sorted");
				}else {
					$sample += $step; #normally +1, sometimes 0,sometimes 2 or more
					unshift( @{$modelfit -> raw_results()->[$i]}, $type );
				}
				$last_model=$this_model;
			}

			if ($self->additional_callback < 1){
				$self->raw_line_structure($modelfit -> raw_line_structure);

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

				$self->raw_line_structure -> write( $dir.'raw_results_structure' );
			}
		} #end if defined modelfit->raw_results

		if ( defined $modelfit -> raw_nonp_results() ) {

			my $n_rows = scalar(@{$modelfit -> raw_nonp_results()});

			my $last_model= 0;
			my $sample = 0; 
			my $type;
			if (($self->additional_callback < 1) and (not $self->gls_model()) ){
				$type='original';
			}elsif ($self->additional_callback == 1){
				$type='simulation';
			}else{
				$type='gls';
			}

			unshift( @{$modelfit -> raw_nonp_results_header}, 'run_type' );

			for (my $i=0; $i< $n_rows; $i++){
				my $this_model = $modelfit -> raw_nonp_results()->[$i]->[0]; 
				my $step= ($this_model-$last_model);
				if ($last_model > 0 and $step>0){
					$type='simulation';
				}
				if ($step < 0){
					ui -> print( category => 'gls',
								 message  => "Warning: It seems the raw_nonp_results is not sorted");
				}else {
					$sample += $step; #normally +1, sometimes 0,sometimes 2 or more
					unshift( @{$modelfit -> raw_nonp_results()->[$i]}, $type );

				}
				$last_model=$this_model;
			}

		} #end if defined modelfit->raw_nonp_results


		$self -> raw_results_header(\@{$modelfit -> raw_results_header});
		$self -> raw_nonp_results_header(\@{$modelfit -> raw_nonp_results_header});
		#  New header

	};
	return $subroutine;
}

sub cleanup
{
	my $self = shift;

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
