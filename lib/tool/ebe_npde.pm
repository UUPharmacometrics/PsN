package tool::ebe_npde;

use include_modules;
use tool::modelfit;
use log;
use Math::Random;
use Config;
use linear_algebra;
use npde_util;
use Moose;
use MooseX::Params::Validate;
use utils::file;

extends 'tool';

has 'samples' => ( is => 'rw', isa => 'Int' );
has 'successful_samples' => ( is => 'rw', isa => 'Int' );
has 'subjects' => ( is => 'rw', isa => 'Int' );
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'estimate_input' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'have_CDF' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'reminimize' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'gls_data_file' => ( is => 'rw', isa => 'Str', default => 'gls_data.dta' );
has 'have_iwres' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_nwpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_tnpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'probnum' => ( is => 'rw', isa => 'Int', default => 1 );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['ebe_npde.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'ebe_npde_results.csv' );
has 'iiv_eta' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'iov_eta' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'occasions' => ( is => 'rw', isa => 'Int',default => 0 );
has 'all_eta_files' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'all_iwres_files' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'missing' => ( is => 'rw', isa => 'Int',default => -99 );
has 'n_simulation_models' => ( is => 'rw', isa => 'Int');

our $iofv_file = 'summary_iofv.csv';
our $iwres_file = 'summary_iwres.csv';
our $ebe_npde_file = 'ebe_npde.csv';
our $all_iofv_file = 'raw_all_iofv.csv';
our $all_iwres_file = 'raw_all_iwres.csv';

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

	#input_checking.pm ensures that either single $PROB or two $PROB where first is tnpri
	#so we assume that here
	if  (scalar (@{$self->models->[0]->problems}) == 2 ){
		$self->have_tnpri(1);
	}

	my $problem_index = (0+$self->have_tnpri());
	foreach my $coderec ('error','des','pred','pk','mix'){ 
		my $acc = $coderec.'s';
		if (defined $self->models->[0]->problems->[$problem_index]->$acc 
			and scalar(@{$self->models->[0]->problems->[$problem_index]->$acc})>0 ) {
			my @extra_code = @{$self->models->[0]->problems->[$problem_index]->$acc->[0]->code};
			foreach my $line (@extra_code){
				if ($line =~ /^\s*IWRES\s*=/){
					$self->have_iwres(1);
					last;
				}
			}
			last if ($self->have_iwres);
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

	$oprob -> add_records( type           => 'table',
						   record_strings => [ join( ' ', @table_header ).
											   ' IPRED PRED NOPRINT NOAPPEND ONEHEADER FILE=orig_pred.dta']);
	if ($self->have_iwres){
		$oprob -> add_records( type           => 'table',
							   record_strings => ['IWRES ID MDV NOPRINT NOAPPEND ONEHEADER FILE=original_iwres.dta']);
		
		push( @all_iwres_files, $self->directory . 'm' . $model_number . '/original_iwres.dta' );
	}
	my ($iivref,$iovref) = get_eta_headers(problem => $oprob);
	$self->iiv_eta($iivref);
	$self->iov_eta($iovref);
	$self->occasions(scalar(@{$iovref}));
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

		tool::add_to_nmoutput(run => $run_orig, extensions => ['phi','ext']);		

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

	my $sims_per_file = int($samples/($self->n_simulation_models));
	my $remainder = $samples % ($self->n_simulation_models);
	
	for( my $sim_no = 1; $sim_no <= $self->n_simulation_models ; $sim_no++ ) {
		my $sims_this_file = $sims_per_file;
		if ($remainder > 0){
			$sims_this_file++;
			$remainder--;
		}
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
			if ($self->have_iwres){
				$sim_model -> add_records( type           => 'table',
										   problem_numbers => [($self->probnum())],
										   record_strings => ['IWRES ID NOPRINT NOAPPEND ONEHEADER FILE=dummy']);
			}
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
        $sim_model -> set_outputfile();
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

		$new_record[0] .= ' SUBPROB='.$sims_this_file;
		$prob -> set_records( type => 'simulation',
							  record_strings => \@new_record );

		if ($self->have_iwres){
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
		}

		$sim_model -> _write();
		push( @orig_and_sim_models, $sim_model );

	} #end loop over number of simulations

	$self->all_eta_files(\@all_eta_files);
	$self->all_iwres_files(\@all_iwres_files);

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

	tool::add_to_nmoutput(run => $run_sim, extensions => ['phi','ext']);		

	my $typerun = 'evaluations';
	$typerun = 'reestimations' if $self->reminimize;
	ui -> print( category => 'ebe_npde',
				 message  => "Running simulations and $typerun" );
	$self->tools([]) unless defined $self->tools;
	push( @{$self->tools}, $run_sim);
}

sub formatfloat
{
	my $val = shift;
	my $string;
	if ($val == -99){
		$string = '';
	}else{
		$string = sprintf("%.8f",$val);
	}
	return $string;
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Num', optional => 1 }
		);
	my $model_number = $parm{'model_number'};


	unless (defined $self->tools->[0]->raw_results){
		croak("Running simulations failed. Check output in ".$self->tools->[0]->directory);
	}

	for (my $loop=0; $loop<1; $loop++){
		last unless ($self->have_iwres);
		unless (-e $self->all_iwres_files->[0]) {
			ui->print(category=> 'all',
					  message => "\nError iwres: original iwres file not found, iwres results cannot be computed\n");
			last;
		}
		my @found_files = ();
		foreach my $file (@{$self->all_iwres_files}) {
			push(@found_files,$file) if (-e $file);
		}
		
		my $headers_array = [['IWRES'],['ID','MDV']];
		my $mean_matrix_array = [[],undef];
		my $values_matrix_array = [[],[]];
		my $filter_all_zero_array = [0,0];
		my $init_only_array = [0,1];

		my $ret = npde_util::get_nmtabledata(filenames => \@found_files,
											 header_strings_array => $headers_array,
											 values_matrix_array => $values_matrix_array,
											 mean_matrix_array => $mean_matrix_array,
											 filter_all_zero_array => $filter_all_zero_array,
											 init_only_array => $init_only_array);

		unless ($ret ==0){
			ui->print(category=> 'all',
					  message =>"\nError in get_nmtabledata for iwres: $ret. iwres results cannot be computed\n");
			last;
		}

		my @extra_headers=('ID','MDV');
		my @headers = ('IWRES');
		my $id_mdv_matrix = $values_matrix_array->[1];
		my $est_matrix = $values_matrix_array->[0];
		my $mean_matrix = $mean_matrix_array->[0];
		my $decorr = [];
		my $stdev = [];
		my $npde = [];
		my $pde = [];

		my $npd = [];
		my $pd = [];

		$ret = npde_util::decorrelation($est_matrix,$mean_matrix,$decorr,$stdev);
		unless ($ret ==0){
			ui->print(category=> 'all',
					  message =>"\nError in decorrelation for iwres: $ret. iwres results cannot be computed\n");
			last;
		}

		open(ORI, ">$all_iwres_file") || die("Couldn't open $all_iwres_file : $!");
		my @head = ('ID','MDV','ORIGINAL');
		for (my $j=1; $j<scalar(@{$est_matrix->[0]->[0]});$j++){
			push(@head,'sample.'.$j);
		}
		print ORI join(',',@head)."\n";
		for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
			print ORI $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0];
			if ($id_mdv_matrix->[1]->[$i]->[0] == 0){
					#not missing DV
				for (my $j=0; $j<scalar(@{$est_matrix->[0]->[0]});$j++){
					print ORI ','.formatfloat($est_matrix->[0]->[$i]->[$j]);
				}
			}else{
				for (my $j=0; $j<scalar(@{$est_matrix->[0]->[0]});$j++){
					print ORI ',';
				}
			}
			print ORI "\n";
		}
		close ORI;


		#append to datafile, also print to own file
		my $fname = 'm'.$model_number.'/orig_pred.dta'; 
		if (-e $fname){
			my @tmp = utils::file::slurp_file($fname);
			my $first=1;
			open(EBE_NPDE, '>'.$self->gls_data_file()) || die("Couldn't open ".$self->gls_data_file()." : $!");
			open(DAT, ">ind_iwres_shrinkage.dta") || die("Couldn't open ind_iwres_shrinkage.dta : $!");
			chomp $tmp[1];
			print EBE_NPDE $tmp[1]."       ISHR\n";
			print DAT "ISHR\n";
			#index 0 is TABLE NO
			#index 1 is header, start reading numbers at 2. $stdev start at 0
			for (my $i=2; $i<scalar(@tmp); $i++){
				chomp $tmp[$i];
				my $shr;
				if ($stdev->[$i-2] > 0){
					$shr = formatfloat(1-($stdev->[$i-2]));
				}else{
					$shr = "'".$self->missing."'";
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
				ui->print(category=> 'all',
						  message => "\nError in npde_comp for iwres: $ret. iwres results cannot be computed\n");
				last;
			}
			open(DAT, ">$iwres_file") || die("Couldn't open $iwres_file : $!");
			print DAT "ID,MDV,ORIGINAL,NPDE\n";
			for (my $i=0; $i<scalar(@{$npde->[0]});$i++){
				print DAT $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0];
				if ($id_mdv_matrix->[1]->[$i]->[0] == 0){
					#not missing DV
					print DAT ','.formatfloat($est_matrix->[0]->[$i]->[0]).','.formatfloat($npde->[0]->[$i])."\n";
				}else{
					print DAT ',,'."\n";
				}
			}
			close (DAT);

			$ret = npde_util::npde_comp($est_matrix,$pd,$npd);
			unless ($ret ==0){
				ui->print(category=> 'all',
						  message => "\nError in npde_comp for iwres: $ret. iwres results cannot be computed\n");
				last;
			}

			open(DAT, ">iwres_npd.csv") || die("Couldn't open iwres_npd.csv : $!");
			print DAT "ID,MDV,NPD\n";
			for (my $i=0; $i<scalar(@{$npd->[0]});$i++){
				print DAT $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0].','.formatfloat($npd->[0]->[$i])."\n";
			}
			close (DAT);

		}

		open(ORI, ">decorrelated_original_iwres.csv") || 
			die("Couldn't open decorrelated_original_iwres.csv : $!");
		print ORI "ID,MDV,IWRES_STAR\n";
#		open(ORI2, ">raw_original_iwres.csv") || 	die("Couldn't open raw_original_iwres.csv : $!");
#		print ORI2 "ID,MDV,IWRES\n";
		for (my $i=0; $i<scalar(@{$decorr->[0]});$i++){
			print ORI $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0].','.
				formatfloat($decorr->[0]->[$i]->[0])."\n";
#			print ORI2 $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0].','.
#				formatfloat($est_matrix->[0]->[$i]->[0])."\n";
		}
		close ORI;
#		close ORI2;
		last; #must have last here, we do not want to loop
	}

	for (my $loop=0; $loop<1; $loop++){
		unless (-e $self->all_eta_files->[0]){
			ui->print(category=> 'all',
					  message => "\nError ebe: original eta file not found, ebe results cannot be computed\n");
			last;
		}
		my @found_files=();
		foreach my $file (@{$self->all_eta_files}){
			push(@found_files,$file) if (-e $file);
		}
		my $headers_array = [['ID'],['OBJ']];
		my $mean_matrix_array = [undef,[]];
		my $values_matrix_array = [[],[]];
		my $filter_all_zero_array = [0,0];
		my $init_only_array = [1,0];

		my @extra_headers=('ID');
		my @etatypes=();
		my $have_iiv=0;
		if (scalar(@{$self->iiv_eta})>0){
			push(@etatypes,'iiv');
			$have_iiv=1;
		}
		for (my $i=1; $i<=$self->occasions; $i++){
			push(@etatypes,'occasion_'.$i);
		}


		my @all_eta_headers=();
		for (my $ti=0; $ti< scalar(@etatypes); $ti++){
			#my ($iivref,$iovref)
			my @eta_headers;
			if ($ti==0 and $have_iiv){
				@eta_headers = @{$self->iiv_eta};
			}else{
				@eta_headers = @{$self->iov_eta()->[($ti-$have_iiv)]};
			}
			push(@all_eta_headers,@eta_headers);
			push(@{$headers_array},\@eta_headers);
			push(@{$mean_matrix_array},[]);
			push(@{$values_matrix_array},[]);
			push(@{$filter_all_zero_array},1);
			push(@{$init_only_array},0);
		}

		my $ret = npde_util::get_nmtabledata(filenames => \@found_files,
											 header_strings_array => $headers_array,
											 values_matrix_array => $values_matrix_array,
											 mean_matrix_array => $mean_matrix_array,
											 filter_all_zero_array => $filter_all_zero_array,
											 init_only_array => $init_only_array);

		unless ($ret ==0){
			ui->print(category=> 'all',
					  message =>"\nError in get_nmtabledata for eta: $ret. ebe npde results cannot be computed\n");
			last;
		}

		my $id_matrix=$values_matrix_array->[0];

		#OBJ
		my $dummy_matrix=[];
		my $est_matrix= $values_matrix_array->[1];
		my $mean_matrix= $mean_matrix_array->[1];
		my $mean_matrix=[];
		my $decorr = [];
		my $stdev =[];
		my $npde = [];
		my $pde=[];

		my $npd = [];
		my $pd=[];

		#number of samples for which have OBJ
		$self->successful_samples(scalar(@{$est_matrix->[0]->[0]})-1); # -1 for original 

		open(ORI, ">$all_iofv_file") || die("Couldn't open $all_iofv_file : $!");
		my @head = ('ID','ORIGINAL');
		for (my $j=1; $j<scalar(@{$est_matrix->[0]->[0]});$j++){
			push(@head,'sample.'.$j);
		}
		print ORI join(',',@head)."\n";
		for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
			print ORI $id_matrix->[0]->[$i]->[0];
			for (my $j=0; $j<scalar(@{$est_matrix->[0]->[0]});$j++){
				print ORI ','.formatfloat($est_matrix->[0]->[$i]->[$j]);
			}
			print ORI "\n";
		}
		close ORI;

#							$estimate_matrix->[column][$id_index][sample];
		#id num is header
#		for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
#			print ORI ',' if ($i>0);
#			print ORI $id_matrix->[0]->[$i]->[0];
#		}
#		print ORI "\n";
#		for (my $j=0; $j<scalar(@{$est_matrix->[0]->[0]});$j++){
#			for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
#				print ORI ',' if ($i>0);
#				print ORI formatfloat($est_matrix->[0]->[$i]->[$j]);
#			}
#			print ORI "\n";
#		}


		$ret = npde_util::decorrelation($est_matrix,$mean_matrix,$decorr,$stdev);
		unless ($ret ==0){
			print "\nError in decorrelation for iofv: $ret. iofv results cannot be computed\n";
			last;
		}
		$ret = npde_util::npde_comp($decorr,$pde,$npde);
#		unless ($ret ==0){
#			print "\nError in npde_comp for iofv: $ret. iofv results cannot be computed\n";
#			last;
#		}
		open(ORI, ">$iofv_file") || die("Couldn't open $iofv_file : $!");
		print ORI "ID,OBSERVED,MEAN_SIM,SD_SIM,STAND_OBS,NPDE\n";
		for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
			print ORI $id_matrix->[0]->[$i]->[0].','.formatfloat($est_matrix->[0]->[$i]->[0]).','.
				formatfloat($mean_matrix->[0]->[$i]).','.formatfloat($stdev->[$i]).','.
				formatfloat(($decorr->[0]->[$i]->[0])**2).',';
			if ($ret == 0){
				print ORI formatfloat($npde->[0]->[$i]);
			}
			print ORI "\n";
		}
		close ORI;


		if (0){
			#have square of decorr in summary iofv
			open(ORI, ">decorrelated_original_iofv.csv") || 
				die("Couldn't open decorrelated_original_iofv.csv : $!");
			print ORI "ID,OFV\n";
			for (my $i=0; $i<scalar(@{$decorr->[0]});$i++){
				print ORI $id_matrix->[0]->[$i]->[0].','.formatfloat($decorr->[0]->[$i]->[0])."\n";
			}
			close ORI;
		}
		if (0){
			open(DAT, ">iofv_pde.csv") || 
				die("Couldn't open iofv_pde.csv : $!");
			print DAT "ID,OFV_PDE\n";
			for (my $i=0; $i<scalar(@{$pde->[0]});$i++){
				print DAT $id_matrix->[0]->[$i]->[0].','.formatfloat($pde->[0]->[$i])."\n";
			}
			close (DAT);
		}

		if ($self->have_CDF()){
			if (0){
				open(DAT, ">iofv_npde.csv") || 	die("Couldn't open iofv_npde.csv : $!");
				print DAT "ID,IOFV_NPDE\n";
				for (my $i=0; $i<scalar(@{$npde->[0]});$i++){
					print DAT $id_matrix->[0]->[$i]->[0].','.formatfloat($npde->[0]->[$i])."\n";
				}
				close (DAT);
			}
			if (0){
				$ret = npde_util::npde_comp($est_matrix,$pd,$npd);
				unless ($ret ==0){
					print "\nError in npde_comp for iofv: $ret. iofv results cannot be computed\n";
					last;
				}
				open(DAT, ">iofv_npd.csv") || die("Couldn't open iofv_npd.csv : $!");
				print DAT "ID,OFV_NPD\n";
				for (my $i=0; $i<scalar(@{$npd->[0]});$i++){
					print DAT $id_matrix->[0]->[$i]->[0].','.formatfloat($npd->[0]->[$i])."\n";
				}
				close (DAT);
			}
		}

		$self->subjects(scalar(@{$id_matrix->[0]}));
		my @all_npde=(); #[ind]->[eta]
		my @standardized=();
		for (my $ti=0; $ti< scalar(@etatypes); $ti++){
			#my ($iivref,$iovref)
			my $offset = 2; #ID and OBJ
			my $type = $etatypes[$ti];
			my @eta_headers = @{$headers_array->[$offset+$ti]};
			my $est_matrix= $values_matrix_array->[$offset+$ti];
			my $mean_matrix= $mean_matrix_array->[$offset+$ti];
			my $decorr = [];
			my $dummy =[];
			my $npde = [];
			my $pde=[];
			my $npd = [];
			my $pd=[];

			open(ORI, ">raw_original_$type"."_ebe.csv") || die("Couldn't open raw_original_$type"."_ebe.csv : $!");
			print ORI "ID,".join(',',@eta_headers)."\n";
			for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
				print ORI $id_matrix->[0]->[$i]->[0];
				if ($ti==0){
					push(@all_npde,[]);
					push(@standardized,0);
				}
				for (my $j=0; $j<scalar(@{$est_matrix});$j++){
					print ORI ','.formatfloat($est_matrix->[$j]->[$i]->[0]);
				}
				print ORI "\n";
			}
			close ORI;
			
			$ret = npde_util::decorrelation($est_matrix,$mean_matrix,$decorr,$dummy);
			unless ($ret ==0){
				print "\nError in decorrelation for ebe: $ret. ebe results cannot be computed\n";
				last;
			}

			open(ORI, ">decorrelated_original_$type"."_ebe.csv") || die("Couldn't open decorrelated_original_$type"."_ebe.csv : $!");
			print ORI "ID,".join(',',@eta_headers)."\n";
			for (my $i=0; $i<scalar(@{$decorr->[0]});$i++){
				print ORI $id_matrix->[0]->[$i]->[0];
				my $sd=0;
				for (my $j=0; $j<scalar(@{$decorr});$j++){
					print ORI ','.formatfloat($decorr->[$j]->[$i]->[0]);
					$sd += ($decorr->[$j]->[$i]->[0])**2 unless ($decorr->[$j]->[$i]->[0] == $self->missing)  ;
				}
				$standardized[$i] += $sd;
				print ORI "\n";
			}
			close ORI;
			
			$ret = npde_util::npde_comp($decorr,$pde,$npde);
			unless ($ret ==0){
				print "\nError in npde_comp for eta: $ret. ebe results cannot be computed\n";
				last;
			}
			open(DAT, ">ebe_pde_$type".".csv") || die("Couldn't open ebe_pde_$type".".csv : $!");
			print DAT "ID,".join(',',@eta_headers)."\n";
			for (my $i=0; $i<scalar(@{$pde->[0]});$i++){
				print DAT $id_matrix->[0]->[$i]->[0];
				for (my $j=0; $j<scalar(@{$pde});$j++){
					print DAT ','.formatfloat($pde->[$j]->[$i]);
				}
				print DAT "\n";
			}
			close (DAT);
			
			if ($self->have_CDF()){
				for (my $i=0; $i<scalar(@all_npde);$i++){ #loop id
					for (my $j=0; $j<scalar(@{$npde});$j++){ #loop eta
						push(@{$all_npde[$i]},$npde->[$j]->[$i]);
					}
				}



				$ret = npde_util::npde_comp($est_matrix,$pd,$npd);
				unless ($ret ==0){
					print "\nError in npde_comp for ebe: $ret. ebe results cannot be computed\n";
					last;
				}
				open(DAT, ">ebe_npd_$type".".csv") || die("Couldn't open ebe_npd_$type".".csv : $!");
				print DAT "ID,".join(',',@eta_headers)."\n";
				for (my $i=0; $i<scalar(@{$npd->[0]});$i++){
					print DAT $id_matrix->[0]->[$i]->[0];
					for (my $j=0; $j<scalar(@{$npd});$j++){
						print DAT ','.formatfloat($npd->[$j]->[$i]);
					}
					print DAT "\n";
				}
				close (DAT);

			}
		}#end loop etatypes
		if ($self->have_CDF()){
			open(DAT, ">$ebe_npde_file") || die("Couldn't open $ebe_npde_file : $!");
			print DAT "ID,STAND_EBE,".join(',',@all_eta_headers)."\n";
			for (my $i=0; $i<scalar(@all_npde);$i++){
				print DAT $id_matrix->[0]->[$i]->[0].','.formatfloat($standardized[$i]);
				for (my $j=0; $j<scalar(@{$all_npde[$i]});$j++){
					print DAT ','.formatfloat($all_npde[$i]->[$j]);
				}
				print DAT "\n";
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
			trace(tool => 'ebe_npde', message => "Preparing to rearrange raw_results in memory, adding ".
									"model name information", level => 1);

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

sub get_eta_headers
{
	my %parm = validated_hash(\@_,
							  problem => { isa => 'model::problem', optional => 0 },
		);
	my $problem = $parm{'problem'};
	
	my @use_etas=();
	my @these=();
	my @prev=();
	my @eta_headers=();
	my @iov_eta_headers=(); #array over base eta over occasions
	my $iov_eta_counter=0;
	for (my $j=0; $j<scalar(@{$problem->omegas}); $j++){
		my $record= $problem->omegas->[$j];
		@these=();
		my $all_fix=0;
		my $is_iov=0;
		last if ($record->prior());
		if  ($record->same() ){
			#this is iov for sure
			push(@these,@prev);
		}else{
			if (($j+1) < scalar(@{$problem->omegas})){
				#not last omega
				$is_iov=1 if ($problem->omegas->[$j+1]->same()); #next is same, assume this is new eta first occasion
			}
			if  ($record->fix()){
				$all_fix = 1; 
			}
			foreach my $option (@{$record -> options()}) {
				if ($option->on_diagonal()){
					if (($option->fix() or $all_fix )and ($option->init() == 0)){
						push(@these,0);
					}else{
						my $val = 1;
						if ($is_iov){
							$iov_eta_counter++; #1 or larger
							$val = 1+$iov_eta_counter; #2 or larger
						}
						push(@these,$val); 
					}
				}
			}
		}
		push(@use_etas,@these);
		@prev = @these;
	}

	my %occasion=();
	for (my $i=1; $i<=scalar(@use_etas); $i++){
		if ($use_etas[$i-1] == 1){
			push(@eta_headers,'ETA('.$i.')') ; #only iiv
		}elsif ($use_etas[$i-1] > 1){
			my $num = $use_etas[$i-1] -1; #1 or larger
			if (defined $occasion{$num}){
				$occasion{$num} = $occasion{$num}+1;
			}else{
				$occasion{$num}=1;
			}

			if (scalar(@iov_eta_headers)< $occasion{$num}){
				#first eta at this occasion
				push(@iov_eta_headers,['ETA('.$i.')']) ; 
			}else{
				#new eta old occasion
				push(@{$iov_eta_headers[$occasion{$num}-1]},'ETA('.$i.')'); 
			}
		}
	} 

	return (\@eta_headers,\@iov_eta_headers);
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

sub create_R_plots_code{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  rplot => { isa => 'rplots', optional => 0 }
		);
	my $rplot = $parm{'rplot'};

	my @all_eta =();
	my $iiv_eta = 'iiv.eta.names <- c()';
	if (scalar(@{$self->iiv_eta})>0){
		$iiv_eta = "iiv.eta.names <-  c('".join("','",@{$self->iiv_eta})."')";
		push (@all_eta,@{$self->iiv_eta});
	}
	my $iov_eta = 'iov.eta.names <- list()';
	if ($self->occasions > 0){
		$iov_eta = 'iov.eta.occasion.names <- list('."o1=c('".join("','",@{$self->iov_eta->[0]})."')";
		push (@all_eta,@{$self->iov_eta->[0]});
		for (my $occ=2; $occ<= $self->occasions; $occ++){
			$iov_eta .= ",o$occ=c('".join("','",@{$self->iov_eta->[$occ-1]})."')";
			push (@all_eta,@{$self->iov_eta->[$occ-1]});
		}
		$iov_eta .= ')';
	}

	my @all_eta_numbers=();
	foreach my $eta (@all_eta){
		$eta =~ /^ETA\((\d+)\)/;
		push(@all_eta_numbers,$1);
	}

	my $succ = $self->successful_samples;
	$succ = 2 if ($succ == 0);
	my $outlying = 'outlying_criteria <- '.-(Statistics::Distributions::udistr(1/($succ))).
		'  # for successful samples='.$succ;

	$rplot->add_preamble(code => [
							 '#ebe_npde-specific preamble',
							 'samples   <-'.$self->samples,
							 'successful.samples  <- '.$self->successful_samples,
							 $outlying,
							 'n.subjects   <-'.$self->subjects,
							 "ebe.npde.file <- '".$ebe_npde_file."'",
							 "iofv.file <- '".$iofv_file."'",
							 "iwres.file <- '".$iwres_file."'",
							 "all.iofv.file <- '".$all_iofv_file."'",
							 "all.iwres.file <- '".$all_iwres_file."'",
							 'occasions   <-'.$self->occasions,
							 "all.eta.names <-  c('".join("','",@all_eta)."')",
							 'all.eta.numbers <-  c('.join(',',@all_eta_numbers).')',
							 $iiv_eta,
							 $iov_eta
						 ]);

}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
