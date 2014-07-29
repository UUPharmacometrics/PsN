package tool::ebe_npde;

use include_modules;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Config;
use linear_algebra;
use npde_util;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'samples' => ( is => 'rw', isa => 'Int' );
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'estimate_input' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'have_CDF' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'reminimize' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'gls_data_file' => ( is => 'rw', isa => 'Str', default => 'gls_data.dta' );
has 'have_nwpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_tnpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'probnum' => ( is => 'rw', isa => 'Int', default => 1 );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['ebe_npde.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'ebe_npde_results.csv' );

sub BUILD
{
	my $self = shift;

	$self->have_CDF(1) if eval('require Statistics::Distributions'); #enough, now loaded

	for my $accessor ('logfile','raw_results_file','raw_nonp_file') {
		my @new_files = ();
		my @old_files = @{$self->$accessor};
		for (my $i = 0; $i < scalar(@old_files); $i++){
			my $name;
			my $ldir;
			($ldir, $name) = OSspecific::absolute_path($self->directory, $old_files[$i]);
			push(@new_files, $ldir.$name) ;
		}
		$self->$accessor(\@new_files);
	}	

	if ( scalar (@{$self->models->[0]->problems}) > 2 ){
		croak('Cannot have more than two $PROB in the input model.');
	}elsif  (scalar (@{$self->models->[0]->problems}) == 2 ){
		if ((defined $self->models->[0]->problems->[0]->priors()) and 
			scalar(@{$self->models->[0]->problems->[0]->priors()})>0 ){
			my $tnpri=0;
			foreach my $rec (@{$self->models->[0]->problems->[0] -> priors()}){
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
			unless( defined $self->models->[0]->extra_files ){
				croak('When using $PRIOR TNPRI you must set option -extra_files to '.
					  'the msf-file, otherwise the msf-file will not be copied to the NONMEM '.
					  'run directory.');
			}

		}else{
			croak('The input model must contain exactly one problem, unless'.
				  ' first $PROB has $PRIOR TNPRI');
		}
		my $est_record = $self->models->[0]->record( problem_number => (1+$self->have_tnpri()),
													 record_name => 'estimation' );
		unless (defined $est_record and scalar(@{$est_record})>0){
			croak('Input model must have an estimation record');
		}

	}

	my $meth = $self->models->[0]->get_option_value( record_name  => 'estimation',
													 problem_index => (0+$self->have_tnpri()),
													 option_name  => 'METHOD',
													 option_index => 0);
	if (not (defined $meth) or ($meth eq '0') or ($meth =~ /^ZE/)){
		croak('Cannot run ebe_npde if METHOD=0, all ETAs will be 0');
	}
}

sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 }
		);
	my $model_number = $parm{'model_number'};

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

	$orig_model = $model ->	copy( filename    => $self->directory . 'm' . $model_number . '/original.mod',
								  copy_datafile   => 1,
								  write_copy => 0,
								  copy_output => 0);
	
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
	
	push( @all_iwres_files, $self->directory . 'm' . $model_number . '/original_iwres.dta' );
	
	my @use_etas=();
	my @these=();
	my @prev=();
	my @eta_headers=();
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
		push(@eta_headers,'ETA('.$i.')') if ($use_etas[$i-1] == 1);
	} 

	my $orig_model_output;
	if (defined  $self->lst_file()){
		$orig_model_output= output -> new(filename => '../'.$self->lst_file);
		unless ($orig_model_output->parsed_successfully()){
			croak("lst file " . $self->lst_file . " could not be parsed.");
		}
		$orig_model -> update_inits ( from_output => $orig_model_output,
									  problem_number => $self->probnum());
		$orig_model -> _write();
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
		$orig_model -> _write();
		push( @orig_and_sim_models, $orig_model );
		$simdirname='orig_and_simulation_dir'; 
	}elsif ($self->estimate_input()) {
		$orig_model -> _write();
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
			copy_data => 0,
			abort_on_fail => $self->abort_on_fail);

		if (defined $run_orig->nm_output and length($run_orig->nm_output)>0){
			my $old = $run_orig->nm_output;
			unless ($run_orig->nm_output =~ /phi/){
				$old .= ',phi';
			}
			unless ($run_orig->nm_output =~ /ext/){
				$old .= ',ext';
			}
			$run_orig->nm_output($old);
		}else{
			$run_orig->nm_output('phi,ext');
		}

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
		$orig_model -> _write();
		push( @orig_and_sim_models, $orig_model );
		$simdirname='orig_and_simulation_dir'; 
	}

	my $samples = $self -> samples();

	my @all_eta_files = ($self->directory . 'm' . $model_number . '/original.phi');

	for( my $sim_no = 1; $sim_no <= $samples ; $sim_no++ ) {

		my $sim_name = "sim-$sim_no.mod";
		my $sim_out = "sim-$sim_no.lst";
		my $etafile = 'sim-'.$sim_no.'.phi';
		push(@all_eta_files,$self->directory . 'm' . $model_number . '/' . $etafile);

		if( $sim_no == 1 ) {
			$sim_model = $orig_model->
				copy( filename    => $self->directory . 'm' . $model_number . '/' . $sim_name,
					  output_same_directory => 1,
					  copy_datafile   => 0,
					  write_copy => 0,
					  copy_output => 0);
			$sim_model -> remove_records( type => 'table' );
			$sim_model -> remove_records( type => 'covariance' );
			$sim_model -> shrinkage_stats( enabled => 0 );

			#set IGNORE=@ since datafile will
			#get a header during copying. Keep IGNORE=LIST
			
			for (my $k=0; $k< scalar(@{$sim_model->problems}); $k++){
				$sim_model -> problems -> [$k]->datas->[0]->ignoresign('@');
			}

			# set $TABLE record

			$sim_model -> add_records( type           => 'table',
									   problem_numbers => [($self->probnum())],
									   record_strings => ['IWRES ID NOPRINT NOAPPEND ONEHEADER FILE=dummy']);

			unless ($self->reminimize()){
				$sim_model -> set_maxeval_zero(print_warning => 1,
											   last_est_complete => $self->last_est_complete(),
											   niter_eonly => $self->niter_eonly(),
											   need_ofv => 1);
			}

		} else {
			$sim_model = $orig_and_sim_models[$#orig_and_sim_models]->
				copy( filename    => $self->directory . 'm' . $model_number . '/' . $sim_name,
					  copy_datafile   => 0,
					  output_same_directory => 1,
					  write_copy => 0,
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

		my $iwres_file = "iwres-$sim_no.dta";
		$prob -> remove_option( record_name  => 'table',
								option_name  => 'FILE',
								fuzzy_match => 1,
								record_number => 1);

		$prob -> add_option(record_name  => 'table',
							record_number  => 1,
							option_name  => 'FILE',
							option_value => $iwres_file );   

		push( @all_iwres_files, $self->directory . 'm' . $model_number . '/' . $iwres_file );


		$sim_model -> _write();
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
		_raw_results_callback => $self -> _modelfit_raw_results_callback( model_number => $model_number ),
		copy_data =>0,
		abort_on_fail => $self->abort_on_fail);
	if (defined $run_sim->nm_output and length($run_sim->nm_output)>0){
		my $old = $run_sim->nm_output;
		unless ($run_sim->nm_output =~ /phi/){
			$old .= ',phi';
		}
		unless ($run_sim->nm_output =~ /ext/){
			$old .= ',ext';
		}
		$run_sim->nm_output($old);
	}else{
		$run_sim->nm_output('phi,ext');
	}

	ui -> print( category => 'ebe_npde',
				 message  => "Running simulations and reestimations" );

	$run_sim -> run;
	$self->first_callback(0);

	unless (defined $run_sim->raw_results){
		croak("Running simulations failed. Check output in ".$run_sim->directory());
	}

	while (1){
		my @extra_headers=('ID','MDV');
		my @headers = ('IWRES');
		my $id_mdv_matrix = [];
		my $dummy_matrix = [];
		my $est_matrix = [];
		my $mean_matrix = [];
		my $decorr = [];
		my $stdev = [];
		my $npde = [];
		my $pde = [];

		my $npd = [];
		my $pd = [];

		my @found_files = ();
		unless (-e $all_iwres_files[0]) {
			print "\nError iwres: original iwres file not found, iwres results cannot be computed\n";
			last;
		}
		my $ret = npde_util::read_table_files([$all_iwres_files[0]],\@extra_headers,$id_mdv_matrix,$dummy_matrix,0);
		unless ($ret ==0){
			print "\nError in read_table_files for iwres: $ret. iwres results cannot be computed\n";
			last;
		}
		foreach my $file (@all_iwres_files) {
			push(@found_files,$file);
		}
		$ret = npde_util::read_table_files(\@found_files,\@headers,$est_matrix,$mean_matrix,1);
		unless ($ret == 0) {
			print "\nError in read_table_files for iwres: $ret. iwres results cannot be computed\n";
			last;
		}
		$ret = npde_util::decorrelation($est_matrix,$mean_matrix,$decorr,$stdev);
		unless ($ret ==0){
			print "\nError in decorrelation for iwres: $ret. iwres results cannot be computed\n";
			last;
		}


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
			#index 0 is TABLE NO
			#index 1 is header, start reading numbers at 2. $stdev start at 0
			for (my $i=2; $i<scalar(@tmp); $i++){
				chomp $tmp[$i];
				my $shr;
				if ($stdev->[$i-2] > 0){
					$shr =sprintf("%.8f",(1-($stdev->[$i-2])));
				}else{
					$shr ='-99';
				}
				print EBE_NPDE $tmp[$i]." ".$shr."\n";
				print DAT $shr."\n";
			}
			close (EBE_NPDE);
			close (DAT);
		}else{
			print "\nError: $fname does not exist\n";
		}




		if ($self->have_CDF()){
			$ret = npde_util::npde_comp($decorr,$pde,$npde);
			unless ($ret ==0){
				print "\nError in npde_comp for iwres: $ret. iwres results cannot be computed\n";
				last;
			}
			open(DAT, ">iwres_npde.csv") || 
				die("Couldn't open iwres_npde.csv : $!");
			print DAT "ID,MDV,NPDE\n";
			for (my $i=0; $i<scalar(@{$npde->[0]});$i++){
				print DAT $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0].',';
				printf DAT ("%.8f\n",$npde->[0]->[$i]);
			}

			close (DAT);

			$ret = npde_util::npde_comp($est_matrix,$pd,$npd);
			unless ($ret ==0){
				print "\nError in npde_comp for iwres: $ret. iwres results cannot be computed\n";
				last;
			}

			open(DAT, ">iwres_npd.csv") || 
				die("Couldn't open iwres_npd.csv : $!");
			print DAT "ID,MDV,NPD\n";
			for (my $i=0; $i<scalar(@{$npd->[0]});$i++){
				print DAT $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0].',';
				printf DAT ("%.8f\n",$npd->[0]->[$i]);
			}
			close (DAT);

		}

		open(ORI, ">decorrelated_original_iwres.csv") || 
			die("Couldn't open decorrelated_original_iwres.csv : $!");
		print ORI "ID,MDV,IWRES_STAR\n";
		open(ORI2, ">raw_original_iwres.csv") || 
			die("Couldn't open raw_original_iwres.csv : $!");
		print ORI2 "ID,MDV,IWRES\n";
		for (my $i=0; $i<scalar(@{$decorr->[0]});$i++){
			print ORI $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0].',';
			printf ORI ("%.8f\n",$decorr->[0]->[$i]->[0]);
			print ORI2 $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0].',';
			printf ORI2 ("%.8f\n",$est_matrix->[0]->[$i]->[0]);
		}

		close ORI;
		close ORI2;


		last; #must have last here, we do not want to loop
	}




	while (1){
		my @extra_headers=('ID');
		my $id_matrix=[];
		my $dummy_matrix=[];
		my $est_matrix=[];
		my $mean_matrix=[];
		my $decorr = [];
		my $dummy =[];
		my $npde = [];
		my $pde=[];

		my $npd = [];
		my $pd=[];

		my @found_files=();
		unless (-e $all_eta_files[0]){
			print "\nError ebe: original eta file not found, ebe results cannot be computed\n";
			last;
		}
		my $ret = npde_util::read_table_files([$all_eta_files[0]],\@extra_headers,$id_matrix,$dummy_matrix,0);
		unless ($ret ==0){
			print "\nError in read_table_files for eta: $ret. ebe results cannot be computed\n";
			last;
		}
		foreach my $file (@all_eta_files){
			push(@found_files,$file);
		}
		$ret = npde_util::read_table_files(\@found_files,\@eta_headers,$est_matrix,$mean_matrix,1);
		unless ($ret ==0){
			print "\nError in read_table_files for eta: $ret. ebe results cannot be computed\n";
			last;
		}
		open(ORI, ">raw_original_eta.csv") || 
			die("Couldn't open raw_original_eta.csv : $!");
		print ORI "ID,".join(',',@eta_headers)."\n";
		for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
			print ORI $id_matrix->[0]->[$i]->[0];
			for (my $j=0; $j<scalar(@{$est_matrix});$j++){
				printf ORI (",%.8f\n",$est_matrix->[$j]->[$i]->[0]);
			}
			print ORI "\n";
		}
		close ORI;

		$ret = npde_util::decorrelation($est_matrix,$mean_matrix,$decorr,$dummy);
		unless ($ret ==0){
			print "\nError in decorrelation for eta: $ret. ebe results cannot be computed\n";
			last;
		}

		open(ORI, ">decorrelated_original_eta.csv") || 
			die("Couldn't open decorrelated_original_eta.csv : $!");
		print ORI "ID,".join(',',@eta_headers)."\n";
		for (my $i=0; $i<scalar(@{$decorr->[0]});$i++){
			print ORI $id_matrix->[0]->[$i]->[0];
			for (my $j=0; $j<scalar(@{$decorr});$j++){
				printf ORI (",%.8f\n",$decorr->[$j]->[$i]->[0]);
			}
			print ORI "\n";
		}
		close ORI;

		$ret = npde_util::npde_comp($decorr,$pde,$npde);
		unless ($ret ==0){
			print "\nError in npde_comp for eta: $ret. ebe results cannot be computed\n";
			last;
		}
		open(DAT, ">eta_pde.csv") || 
			die("Couldn't open eta_pde.csv : $!");
		print DAT "ID,".join(',',@eta_headers)."\n";
		for (my $i=0; $i<scalar(@{$pde->[0]});$i++){
			print DAT $id_matrix->[0]->[$i]->[0];
			for (my $j=0; $j<scalar(@{$pde});$j++){
				printf DAT (",%.8f\n",$pde->[$j]->[$i]);
			}
			print DAT "\n";
		}
		close (DAT);

		if ($self->have_CDF()){
			open(DAT, ">eta_npde.csv") || 
				die("Couldn't open eta_npde.csv : $!");
			print DAT "ID,".join(',',@eta_headers)."\n";
			for (my $i=0; $i<scalar(@{$npde->[0]});$i++){
				print DAT $id_matrix->[0]->[$i]->[0];
				for (my $j=0; $j<scalar(@{$npde});$j++){
					printf DAT (",%.8f\n",$npde->[$j]->[$i]);
				}
				print DAT "\n";
			}
			close (DAT);
			$ret = npde_util::npde_comp($est_matrix,$pd,$npd);
			unless ($ret ==0){
				print "\nError in npde_comp for eta: $ret. ebe results cannot be computed\n";
				last;
			}
			open(DAT, ">eta_npd.csv") || 
				die("Couldn't open eta_npd.csv : $!");
			print DAT "ID,".join(',',@eta_headers)."\n";
			for (my $i=0; $i<scalar(@{$npd->[0]});$i++){
				print DAT $id_matrix->[0]->[$i]->[0];
				for (my $j=0; $j<scalar(@{$npd});$j++){
					printf DAT (",%.8f\n",$npd->[$j]->[$i]);
				}
				print DAT "\n";
			}
			close (DAT);

		}


		last; #must break while here
	}

	while (1){
		my @extra_headers=('ID');
		my @headers = ('OBJ');
		my $id_matrix=[];
		my $dummy_matrix=[];
		my $est_matrix=[];
		my $mean_matrix=[];
		my $decorr = [];
		my $stdev =[];
		my $npde = [];
		my $pde=[];

		my $npd = [];
		my $pd=[];

		my @found_files=();
		unless (-e $all_eta_files[0]){
			print "\nError ebe: original phi file not found, iofv results cannot be computed\n";
			last;
		}
		my $ret = npde_util::read_table_files([$all_eta_files[0]],\@extra_headers,$id_matrix,$dummy_matrix,0);
		unless ($ret ==0){
			print "\nError in read_table_files for iofv: $ret. iofv results cannot be computed\n";
			last;
		}
		foreach my $file (@all_eta_files){
			push(@found_files,$file);
		}
		$ret = npde_util::read_table_files(\@found_files,\@headers,$est_matrix,$mean_matrix,1);
		unless ($ret ==0){
			print "\nError in read_table_files for iofv: $ret. iofv results cannot be computed\n";
			last;
		}

		$ret = npde_util::decorrelation($est_matrix,$mean_matrix,$decorr,$stdev);
		unless ($ret ==0){
			print "\nError in decorrelation for iofv: $ret. iofv results cannot be computed\n";
			last;
		}
		open(ORI, ">raw_original_iofv.csv") || 
			die("Couldn't open raw_original_iofv.csv : $!");
		print ORI "ID,OFV,MEAN_SIM,SD_SIM\n";
		for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
			print ORI $id_matrix->[0]->[$i]->[0].',';
			printf ORI ("%.8f,%.8f,%.8f\n",$est_matrix->[0]->[$i]->[0],$mean_matrix->[0]->[$i],$stdev->[$i]);
		}
		close ORI;

		open(ORI, ">decorrelated_original_iofv.csv") || 
			die("Couldn't open decorrelated_original_iofv.csv : $!");
		print ORI "ID,OFV\n";
		for (my $i=0; $i<scalar(@{$decorr->[0]});$i++){
			print ORI $id_matrix->[0]->[$i]->[0].',';
			printf ORI ("%.8f\n",$decorr->[0]->[$i]->[0]);
		}
		close ORI;

		$ret = npde_util::npde_comp($decorr,$pde,$npde);
		unless ($ret ==0){
			print "\nError in npde_comp for iofv: $ret. iofv results cannot be computed\n";
			last;
		}
		open(DAT, ">iofv_pde.csv") || 
			die("Couldn't open iofv_pde.csv : $!");
		print DAT "ID,OFV_PDE\n";
		for (my $i=0; $i<scalar(@{$pde->[0]});$i++){
			print DAT $id_matrix->[0]->[$i]->[0].',';
			printf DAT ("%.8f\n",$pde->[0]->[$i]);
		}
		close (DAT);

		if ($self->have_CDF()){
			open(DAT, ">iofv_npde.csv") || 
				die("Couldn't open iofv_npde.csv : $!");
			print DAT "ID,OFV_NPDE\n";
			for (my $i=0; $i<scalar(@{$npde->[0]});$i++){
				print DAT $id_matrix->[0]->[$i]->[0].',';
				printf DAT ("%.8f\n",$npde->[0]->[$i]);
			}
			close (DAT);
			$ret = npde_util::npde_comp($est_matrix,$pd,$npd);
			unless ($ret ==0){
				print "\nError in npde_comp for iofv: $ret. iofv results cannot be computed\n";
				last;
			}
			open(DAT, ">iofv_npd.csv") || 
				die("Couldn't open iofv_npd.csv : $!");
			print DAT "ID,OFV_NPD\n";
			for (my $i=0; $i<scalar(@{$npd->[0]});$i++){
				print DAT $id_matrix->[0]->[$i]->[0].',';
				printf DAT ("%.8f\n",$npd->[0]->[$i]);
			}
			close (DAT);

		}

		last; #must break while here
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
}

sub prepare_results
{
	my $self = shift;
	$self->cleanup;
}

sub max_and_min
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  use_runs => { isa => 'ArrayRef[Bool]', optional => 0 },
							  column_index => { isa => 'Int', optional => 0 },
							  start_row_index => { isa => 'Int', default => 0, optional => 1 },
							  end_row_index => { isa => 'Int', optional => 1 }
		);
	my @use_runs = defined $parm{'use_runs'} ? @{$parm{'use_runs'}} : ();
	my $column_index = $parm{'column_index'};
	my $start_row_index = $parm{'start_row_index'};
	my $end_row_index = $parm{'end_row_index'};
	my $maximum;
	my $minimum;

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

	return $maximum, $minimum;
}

sub cleanup
{
	my $self = shift;

	#remove tablefiles in simulation NM_runs, they are 
	#copied to m1 by modelfit and read from there anyway.
	for (my $samp=1;$samp<=$self->samples(); $samp++){
		unlink $self->directory . "/simulation_dir1/NM_run" . $samp . "/mc-sim-" . $samp . ".dat";
		unlink $self->directory . "/simulation_dir1/NM_run" . $samp . "/mc-sim-" . $samp . "-1.dat"; #retry
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
