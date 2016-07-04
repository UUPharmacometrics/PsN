package tool::simeval;

use include_modules;
use tool::modelfit;
use log;
use Math::Random;
use Config;
use linear_algebra;
use simeval_util;
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
has 'have_ipred' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_nwpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_tnpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'probnum' => ( is => 'rw', isa => 'Int', default => 1 );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['simeval.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'simeval_results.csv' );
has 'iiv_eta' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'iov_eta' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'occasions' => ( is => 'rw', isa => 'Int',default => 0 );
has 'all_eta_files' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'all_table_files' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'missing' => ( is => 'rw', isa => 'Int',default => -99 );
has 'n_simulation_models' => ( is => 'rw', isa => 'Int');
has 'extra_variables' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'idv' => ( is => 'rw', isa => 'Str', default => 'TIME' );
has 'vpctab_filenames' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'vpc_result_files' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'vpc_names' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'mdv' => ( is => 'rw', isa => 'Str', default => '' );


our $iofv_file = 'summary_iofv.csv';
our $iwres_file = 'summary_iwres.csv';
our $cwres_file = 'summary_cwres.csv';
our $ebe_npde_file = 'ebe_npde.csv';
our $all_iofv_file = 'raw_all_iofv.csv';
our $all_iwres_file = 'raw_all_iwres.csv';
our $all_cwres_file = 'raw_all_cwres.csv';

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
					last if ($self->have_ipred);
				}
				if ($line =~ /^\s*IPRED\s*=/){
					$self->have_ipred(1);
					last if ($self->have_iwres);
				}
			}
			last if ($self->have_iwres and $self->have_ipred);
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
	my @all_table_files;
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

	#logic for use of MDV column as basis for finding which rows are observations
	#request MDV in $TABLE if no $PRED record or if there is a $PRED record and 
	#MDV is in the input
	#if there is a $PRED but no MDV in input then all rows will be observations

	if (scalar @{$orig_model ->record( record_name => 'pred' )} < 1) {
		$self->mdv('MDV');
	} else {
		if ($orig_model ->is_option_set(record => 'input', name => 'MDV')) {
			$self->mdv('MDV'); #MDV in input
		}
	}

	my $ipred = '';
	$ipred = ' IPRED' if $self->have_ipred;
	$oprob -> add_records( type           => 'table',
						   record_strings => [ join( ' ', @table_header ).$ipred.
											   ' PRED NOPRINT NOAPPEND ONEHEADER FILE=orig_pred.dta']);
	my $tablestring = 'ID DV '.$self->mdv.' '.$self->idv.' CWRES'.$ipred.' PRED';
	if ($self->have_iwres){
		$tablestring .= ' IWRES';
	}
	$tablestring .= ' '.join(' ',@{$self->extra_variables});
	$tablestring .= ' NOPRINT NOAPPEND ONEHEADER FILE=original_res_table.dta';
	$oprob -> add_records( type           => 'table',
						   record_strings => [$tablestring]);
		
	push( @all_table_files, $self->directory . 'm' . $model_number . '/original_res_table.dta' );

	my $ref = $oprob->get_eta_sets(header_strings => 1);
	$self->iiv_eta($ref->{'iiv'});
	$self->iov_eta($ref->{'iov'});
	$self->occasions(scalar(@{$ref->{'iov'}}));
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

		ui -> print( category => 'simeval',
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
			my $tablestring = 'ID DV '.$self->mdv.' CWRES'.$ipred;
			if ($self->have_iwres){
				$tablestring .= ' IWRES';
			}
			$tablestring .= ' '.join(' ',@{$self->extra_variables});
			$tablestring .= ' NOPRINT NOAPPEND ONEHEADER FILE=dummy';

			$sim_model -> add_records( type           => 'table',
									   problem_numbers => [($self->probnum())],
									   record_strings => [$tablestring]);

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

		my $tab_file = "sim_res_table-$sim_no.dta";
		$prob -> remove_option( record_name  => 'table',
								option_name  => 'FILE',
								fuzzy_match => 1,
								record_number => 1);

		$prob -> add_option(record_name  => 'table',
							record_number  => 1,
							option_name  => 'FILE',
							option_value => $tab_file );   

		push( @all_table_files, $self->directory . 'm' . $model_number . '/' . $tab_file );

		$sim_model -> _write();
		push( @orig_and_sim_models, $sim_model );

	} #end loop over number of simulations

	$self->all_eta_files(\@all_eta_files);
	$self->all_table_files(\@all_table_files);

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
	ui -> print( category => 'simeval',
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
sub formatnpde
{
	my $val = shift;
	my $string;
	if ($val == -99){
		$string = '';
	}else{
		#have 5 sigdig
		if (($val >= 1) or ($val <= -1)){
			$string = sprintf("%.4f",$val);
		}else{
			$string = sprintf("%.5f",$val);
		}
	}
	return $string;
}
sub formatinteger
{
	my $val = shift;
	my $string;
	if ($val == -99){
		$string = '';
	}else{
		$string = sprintf("%i",$val);
	}
	return $string;
}
sub formattime
{
	my $val = shift;
	my $string;
	$string = sprintf("%7.1f",$val);

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


	
	my $have_mdv = 0;
	if (length($self->mdv)>0){
		$have_mdv = 1 ;
	}

	my ($succ_samp,$subjects) = simeval_analyze(have_mdv => $have_mdv,
												have_iwres => $self->have_iwres,
												gls_data_file => $self->gls_data_file,
												simeval_all_table_files => $self->all_table_files,
												simeval_all_eta_files => $self->all_eta_files,
												missing_value => $self->missing,
												have_CDF => $self->have_CDF,
												iiv_eta => $self->iiv_eta,
												iov_eta => $self->iov_eta,
												occasions => $self->occasions,
												m1dir => 'm'.$model_number.'/',
		);
	$self->successful_samples($succ_samp);
	$self->subjects($subjects);
}

sub simeval_analyze
{ #input filenames are relative working directory, or absolute
	my %parm = validated_hash(\@_,
							  have_mdv => { isa => 'Bool', optional => 0 },
							  have_iwres => { isa => 'Bool', optional => 0 },
							  gls_data_file => { isa => 'Str', optional => 0 },
							  simeval_all_table_files => { isa => 'ArrayRef', optional => 0 },
							  simeval_all_eta_files => { isa => 'ArrayRef', optional => 0 },
							  missing_value => { isa => 'Int', optional => 0 },
							  have_CDF => { isa => 'Bool', optional => 0 },
							  iiv_eta => { isa => 'ArrayRef', optional => 0 },
							  iov_eta => { isa => 'ArrayRef', optional => 0 },
							  occasions => { isa => 'Int', optional => 0 },
							  m1dir => { isa => 'Str', optional => 0},
							  testing => { isa => 'Bool', optional => 1, default => 0 },
							  write_auto => { isa => 'Bool', optional => 1, default => 0 },
		);
	my $have_mdv = $parm{'have_mdv'};
	my $have_iwres = $parm{'have_iwres'};
	my $gls_data_file = $parm{'gls_data_file'};
	my $simeval_all_table_files = $parm{'simeval_all_table_files'};
	my $simeval_all_eta_files = $parm{'simeval_all_eta_files'};
	my $missing_value = $parm{'missing_value'};
	my $have_CDF = $parm{'have_CDF'};
	my $iiv_eta = $parm{'iiv_eta'};
	my $iov_eta = $parm{'iov_eta'};
	my $occasions = $parm{'occasions'};
	my $m1dir = $parm{'m1dir'};
	my $testing = $parm{'testing'};
	my $write_auto = $parm{'write_auto'}; #only turn on for debugging
	
	my $errmess;
	my $ret_subjects;
	my $ret_successful_samples;

	my $commaMDV = '';
	if ($have_mdv){
		$commaMDV= ',MDV';
	}

	my $time_array = [];
	if ($write_auto and (-e $simeval_all_table_files->[0])) {
		my $tablef = nmtablefile->new(filename => $simeval_all_table_files->[0]);
		my $index = $tablef->tables->[0]->header->{'TIME'};
		$time_array = $tablef->tables->[0]->columns->[$index];
	}
	for (my $loop=0; $loop<1; $loop++){
		my $original_nmtable;
		my @iwres_outlier_array=();
		my @cwres_outlier_array=();
		if (-e $simeval_all_table_files->[0]) {
			$original_nmtable = nmtablefile->new(filename => $simeval_all_table_files->[0]);
		}else{
			ui->print(category=> 'all',
					  message => "\nError residuals: original residuals table file not found, residual results cannot be computed\n");
			last;
		}
		my @found_files = ();
		foreach my $file (@{$simeval_all_table_files}) {
			push(@found_files,$file) if (-e $file);
		}
		
		my $headers_array = [['ID'],['CWRES']];
		if ($have_mdv){
			push(@{$headers_array->[0]},'MDV');
		}
		my @table_headers = ('CWRES');
		my @all_file_names = ($all_cwres_file);
		my @summary_file_names = ($cwres_file);
		my $mean_matrix_array = [undef,[]];
		my $values_matrix_array = [[],[]];
		my $filter_all_zero_array = [0,0];
		my $init_only_array = [1,0];

		if ($have_iwres){
			push(@table_headers,'IWRES');
			push(@all_file_names,$all_iwres_file);
			push(@summary_file_names,$iwres_file);
			push(@{$headers_array},['IWRES']);
			push(@{$mean_matrix_array},[]);
			push(@{$values_matrix_array},[]);
			push(@{$filter_all_zero_array},0);
			push(@{$init_only_array},0);
		}


		my $ret = simeval_util::get_nmtabledata(filenames => \@found_files,
											 header_strings_array => $headers_array,
											 values_matrix_array => $values_matrix_array,
											 mean_matrix_array => $mean_matrix_array,
											 filter_all_zero_array => $filter_all_zero_array,
											 init_only_array => $init_only_array);

		unless ($ret ==0){
			ui->print(category=> 'all',
					  message =>"\nError in get_nmtabledata for residuals: $ret. residuals results cannot be computed\n");
			last;
		}

		for (my $k=0; $k< scalar(@table_headers); $k++){
			#we will never get to IWRES unless have IWRES
		
			my @extra_headers=('ID');
			if ($have_mdv){
				push(@extra_headers,'MDV');
			}

			my @headers = ($table_headers[$k]);
			my $all_file_name = $all_file_names[$k];
			my $summary_file_name = $summary_file_names[$k];
			my $id_mdv_matrix = $values_matrix_array->[0];
			my $est_matrix = $values_matrix_array->[($k+1)]; #+1 for ID MDV
			my $mean_matrix = $mean_matrix_array->[($k+1)]; #+1 for ID MDV
			my $decorr = [];
			my $stdev = [];
			my $npde = [];
			my $pde = [];
			my $npd = [];
			my $pd = [];


			if ($write_auto){
				my $origname = 'auto_orig_'.$table_headers[$k].'.tab';
				my $simname = 'auto_sim_'.$table_headers[$k].'.tab';
				open(ORI, ">$origname") || die("Couldn't open $origname : $!");
				print ORI "ID TIME ".$table_headers[$k]."\n";
				for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
					if ((not $have_mdv) or ($id_mdv_matrix->[1]->[$i]->[0] == 0)){
						#not missing DV
						#ID TIME DV
						print ORI formatinteger($id_mdv_matrix->[0]->[$i]->[0]).' '.
							formattime($time_array->[$i]).' '.
							$est_matrix->[0]->[$i]->[0]."\n"; 
					}
				}
				close ORI;
				open(SIM, ">$simname") || die("Couldn't open $simname : $!");
				print SIM "ID TIME ".$table_headers[$k]."\n";
				for (my $j=1; $j<scalar(@{$est_matrix->[0]->[0]});$j++){ #loop sim
					for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){ #loop records
						if ((not $have_mdv) or ($id_mdv_matrix->[1]->[$i]->[0] == 0)){
							#not missing DV
							#ID TIME DV
							print SIM formatinteger($id_mdv_matrix->[0]->[$i]->[0]).' '.
								formattime($time_array->[$i]).' '.
								$est_matrix->[0]->[$i]->[$j]."\n";
						} 
					}
				}
				close SIM;
			}
			
			open(ORI, ">$all_file_name") || die("Couldn't open $all_file_name : $!");
			my @head = ('ID');
			if ($have_mdv){
				push(@head,'MDV');
			}
			push(@head,'ORIGINAL');

			for (my $j=1; $j<scalar(@{$est_matrix->[0]->[0]});$j++){
				push(@head,'sample.'.$j);
			}
			print ORI join(',',@head)."\n";
			for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
				print ORI $id_mdv_matrix->[0]->[$i]->[0];
				if ($have_mdv){
					print ORI ','.$id_mdv_matrix->[1]->[$i]->[0];
				}
				
				if ((not $have_mdv) or ($id_mdv_matrix->[1]->[$i]->[0] == 0)){
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

			my $original_outlier=[];
			($ret,$errmess) = simeval_util::decorrelation_and_npde_records_by_id($est_matrix,$mean_matrix,$id_mdv_matrix,$have_mdv,$npde,$original_outlier);

			unless ($ret ==0){
				ui->print(category=> 'all',
						  message =>"\nError in decorrelation and npde for ".$table_headers[$k].": $ret. ".
						  $table_headers[$k]." results cannot be computed\n".$errmess);
				last;
			}
			if ($table_headers[$k] eq 'IWRES'){
				@iwres_outlier_array= @{$original_outlier};
			}elsif ($table_headers[$k] eq 'CWRES'){
				@cwres_outlier_array= @{$original_outlier};
			}
			if (0 and ($table_headers[$k] eq 'IWRES')){ #FIXME, need to compute stdev
				#append to datafile, also print to own file
				my $fname = $m1dir.'orig_pred.dta'; 
				if (-e $fname){
					my @tmp = utils::file::slurp_file($fname);
					my $first=1;
					open(EBE_NPDE, '>'.$gls_data_file) || die("Couldn't open ".$gls_data_file." : $!");
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
							$shr = "'".$missing_value."'";
						}
						print EBE_NPDE $tmp[$i]." ".$shr."\n";
						print DAT $shr."\n";
					}
					close (EBE_NPDE);
					close (DAT);
				}else{
					print "\nError: $fname does not exist\n";
				}
			}

			if ($have_CDF){
				open(DAT, ">$summary_file_name") || die("Couldn't open $summary_file_name : $!");
				print DAT "ID$commaMDV".",ORIGINAL,NPDE,OUTLIER\n";

				my $obs_index;
				for (my $record_index=0; $record_index<scalar(@{$id_mdv_matrix->[0]});$record_index++){
					print DAT $id_mdv_matrix->[0]->[$record_index]->[0];
					if ($have_mdv){
						print DAT ','.$id_mdv_matrix->[1]->[$record_index]->[0];
					}
				
					if ((not $have_mdv) or ($id_mdv_matrix->[1]->[$record_index]->[0] == 0)){
						print DAT ','.formatfloat($est_matrix->[0]->[$record_index]->[0]).
							','.formatnpde($npde->[$record_index]).','.$original_outlier->[$record_index]."\n";
					}else{
						print DAT ',,,'."\n";
					}
				}
				close (DAT);


				if (0){
					$ret = simeval_util::npde_comp($est_matrix,$pd,$npd);
					unless ($ret ==0){
						ui->print(category=> 'all',
								  message => "\nError in npde_comp for ".$table_headers[$k].": $ret. ".$table_headers[$k].
								  " results pd and npd cannot be computed\n");
						last;
					}
					my $npdname = lc($table_headers[$k]).'_npd.csv';
					open(DAT, ">$npdname") || die("Couldn't open $npdname : $!");
					print DAT "ID$commaMDV".",NPD\n";
					
					for (my $i=0; $i<scalar(@{$npd->[0]});$i++){
						print DAT $id_mdv_matrix->[0]->[$i]->[0];
						if ($have_mdv){
							print DAT ','.$id_mdv_matrix->[1]->[$i]->[0];
						}
						print DAT ','.formatfloat($npd->[0]->[$i])."\n";
					}
					close (DAT);
				}
			}

			if (0){
				#no not have decorrelated
				my $decorrname = 'decorrelated_original_'.lc($table_headers[$k]).'.csv';

				open(ORI, ">$decorrname") || die("Couldn't open $decorrname : $!");
				print ORI "ID$commaMDV".','.$table_headers[$k]."_STAR\n";
				#		open(ORI2, ">raw_original_res_table.csv") || 	die("Couldn't open raw_original_res_table.csv : $!");
				#		print ORI2 "ID,MDV,".$table_headers[$k]."\n";
				for (my $i=0; $i<scalar(@{$decorr->[0]});$i++){
					print ORI $id_mdv_matrix->[0]->[$i]->[0];
					if ($have_mdv){
						print ORI ','.$id_mdv_matrix->[1]->[$i]->[0];
					}
					print ORI ','.formatfloat($decorr->[0]->[$i]->[0])."\n";
					#			print ORI2 $id_mdv_matrix->[0]->[$i]->[0].','.$id_mdv_matrix->[1]->[$i]->[0].','.
					#				formatfloat($est_matrix->[0]->[$i]->[0])."\n";
				}
				close ORI;
			}
			#		close ORI2;
		} #end loop over table headers CWRES, IWRES ...

		my $fname = 'residual_outliers.csv';
		open(ORI, ">$fname") || die("Couldn't open $fname : $!");
		my $iwr='';
		my $outiwr= '';
		$iwr = ',IWRES' if ($have_iwres);
		$outiwr = ',OUTLIER.IWRES' if ($have_iwres);
		print ORI "ID,TIME,DV,PRED$iwr".",CWRES$outiwr".",OUTLIER.CWRES\n";

		my $id_index = $original_nmtable->tables->[0]->header->{'ID'};
		my $time_index = $original_nmtable->tables->[0]->header->{'TIME'};
		my $dv_index = $original_nmtable->tables->[0]->header->{'DV'};
		my $pred_index = $original_nmtable->tables->[0]->header->{'PRED'};
		my $cwres_index = $original_nmtable->tables->[0]->header->{'CWRES'};
		my $iwres_index = $original_nmtable->tables->[0]->header->{'IWRES'} if ($have_iwres);
#		$time_array = $original_nmtable->tables->[0]->columns->[$index];
#				@cwres_outlier_array= @{$original_outlier};
#				@iwres_outlier_array= @{$original_outlier};

		#formatinteger($id_mdv_m
		my $origiwres='';
		my $outline='';
		for (my $record=0; $record < scalar(@{$original_nmtable->tables->[0]->columns->[$id_index]}); $record++){
			next unless (($cwres_outlier_array[$record]==1) or ($have_iwres and $iwres_outlier_array[$record]==1));
			if ($have_iwres){
				$origiwres = ','.$original_nmtable->tables->[0]->columns->[$iwres_index]->[$record];
				$outline = ','.$iwres_outlier_array[$record];
			}
			print ORI formatinteger($original_nmtable->tables->[0]->columns->[$id_index]->[$record]).
				','.$original_nmtable->tables->[0]->columns->[$time_index]->[$record].
				','.$original_nmtable->tables->[0]->columns->[$dv_index]->[$record].
				','.$original_nmtable->tables->[0]->columns->[$pred_index]->[$record].
				$origiwres.
				','.$original_nmtable->tables->[0]->columns->[$cwres_index]->[$record].
				$outline.
				','.$cwres_outlier_array[$record]."\n";
		}
		close ORI;
		last; #must have last here, we do not want to loop
	}

	for (my $loop=0; $loop<1; $loop++){
		unless (-e $simeval_all_eta_files->[0]){
			ui->print(category=> 'all',
					  message => "\nError ebe: original eta file not found, ebe results cannot be computed\n");
			last;
		}
		my @found_files=();
		foreach my $file (@{$simeval_all_eta_files}){
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
		if (scalar(@{$iiv_eta})>0){
			push(@etatypes,'iiv');
			$have_iiv=1;
		}
		for (my $i=1; $i<=$occasions; $i++){
			push(@etatypes,'occasion_'.$i);
		}


		my @all_eta_headers=();
		for (my $ti=0; $ti< scalar(@etatypes); $ti++){
			#my ($iivref,$iovref)
			my @eta_headers;
			if ($ti==0 and $have_iiv){
				@eta_headers = @{$iiv_eta};
			}else{
				@eta_headers = @{$iov_eta->[($ti-$have_iiv)]};
			}
			push(@all_eta_headers,@eta_headers);
			push(@{$headers_array},\@eta_headers);
			push(@{$mean_matrix_array},[]);
			push(@{$values_matrix_array},[]);
			push(@{$filter_all_zero_array},1);
			push(@{$init_only_array},0);
		}

		my $ret = simeval_util::get_nmtabledata(filenames => \@found_files,
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
		my $decorr = [];
		my $stdev =[];
		my $npde = [];
		my $pde=[];

		my $npd = [];
		my $pd=[];

		#number of samples for which have OBJ
		$ret_successful_samples = scalar(@{$est_matrix->[0]->[0]})-1; # -1 for original 

		if ($write_auto){
			my $origname = 'auto_orig_IOFV.tab';
			my $simname = 'auto_sim_IOFV.tab';

			open(ORI, ">$origname") || die("Couldn't open $origname : $!");
			print ORI "ID TIME IOFV\n";
			for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
				print ORI formatinteger($id_matrix->[0]->[$i]->[0]).
					' 1.0 '.$est_matrix->[0]->[$i]->[0]."\n";
			}
			close ORI;

			open(SIM, ">$simname") || die("Couldn't open $simname : $!");
			print SIM "ID TIME IOFV\n";

			for (my $j=1; $j<scalar(@{$est_matrix->[0]->[0]});$j++){
				for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
					print SIM formatinteger($id_matrix->[0]->[$i]->[0]).
						' 1.0 '.$est_matrix->[0]->[$i]->[$j]."\n";
				} 
			}
			close SIM;

		}
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


		($ret,$errmess) = simeval_util::decorrelation($est_matrix,$mean_matrix,$decorr,$stdev);
		unless ($ret ==0){
			ui->print(category=> 'all',
					  message => "\nError in decorrelation for iofv: $ret. iofv results cannot be computed\n".$errmess);
			last;
		}
		$ret = simeval_util::npde_comp($decorr,$pde,$npde);
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
				print ORI formatnpde($npde->[0]->[$i]);
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
				print DAT $id_matrix->[0]->[$i]->[0].','.formatnpde($pde->[0]->[$i])."\n";
			}
			close (DAT);
		}

		if ($have_CDF){
			if (0){
				open(DAT, ">iofv_npde.csv") || 	die("Couldn't open iofv_npde.csv : $!");
				print DAT "ID,IOFV_NPDE\n";
				for (my $i=0; $i<scalar(@{$npde->[0]});$i++){
					print DAT $id_matrix->[0]->[$i]->[0].','.formatnpde($npde->[0]->[$i])."\n";
				}
				close (DAT);
			}
			if (0){
				$ret = simeval_util::npde_comp($est_matrix,$pd,$npd);
				unless ($ret ==0){
					print "\nError in npde_comp for iofv: $ret. iofv results cannot be computed\n";
					last;
				}
				open(DAT, ">iofv_npd.csv") || die("Couldn't open iofv_npd.csv : $!");
				print DAT "ID,OFV_NPD\n";
				for (my $i=0; $i<scalar(@{$npd->[0]});$i++){
					print DAT $id_matrix->[0]->[$i]->[0].','.formatnpde($npd->[0]->[$i])."\n";
				}
				close (DAT);
			}
		}

		$ret_subjects = scalar(@{$id_matrix->[0]});
		
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

			if ($write_auto and ($ti == 0)){
				my $origname = 'auto_orig_EBE.tab';
				my $simname = 'auto_sim_EBE.tab';
				my $maxebe = scalar(@{$est_matrix});
				open(ORI, ">$origname") || die("Couldn't open $origname : $!");
				print ORI "ID TIME EBE\n";
				for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
					for (my $k=0; $k<$maxebe; $k++){
						print ORI formatinteger($id_matrix->[0]->[$i]->[0]).
							' '.($k+1).'.0 '.($est_matrix->[$k]->[$i]->[0])."\n";
					}
				}
				close ORI;
				open(SIM, ">$simname") || die("Couldn't open $simname : $!");
				print SIM "ID TIME EBE\n";

				for (my $j=1; $j<scalar(@{$est_matrix->[0]->[0]});$j++){
					for (my $i=0; $i<scalar(@{$est_matrix->[0]});$i++){
						for (my $k=0; $k<$maxebe; $k++){
							print SIM formatinteger($id_matrix->[0]->[$i]->[0]).
								' '.($k+1).'.0 '.($est_matrix->[$k]->[$i]->[$j])."\n";
						}
					} 
				}
				close SIM;

			}

			
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
			
			($ret,$errmess) = simeval_util::decorrelation($est_matrix,$mean_matrix,$decorr,$dummy);
			unless ($ret ==0){
				ui->print(category=> 'all',
						  message => "\nError in decorrelation for ebe: $ret. ebe results cannot be computed\n".$errmess);
				last;
			}

			if (0){
				open(ORI, ">decorrelated_original_$type"."_ebe.csv") || die("Couldn't open decorrelated_original_$type"."_ebe.csv : $!");
				print ORI "ID,".join(',',@eta_headers)."\n";
				for (my $i=0; $i<scalar(@{$decorr->[0]});$i++){
					print ORI $id_matrix->[0]->[$i]->[0];
					my $sd=0;
					for (my $j=0; $j<scalar(@{$decorr});$j++){
						print ORI ','.formatfloat($decorr->[$j]->[$i]->[0]);
						$sd += ($decorr->[$j]->[$i]->[0])**2 unless ($decorr->[$j]->[$i]->[0] == $missing_value)  ;
					}
					$standardized[$i] += $sd;
					print ORI "\n";
				}
				close ORI;
			}
			$ret = simeval_util::npde_comp($decorr,$pde,$npde);
			unless ($ret ==0){
				print "\nError in npde_comp for eta: $ret. ebe results cannot be computed\n";
				last;
			}
			if (0){
				open(DAT, ">ebe_pde_$type".".csv") || die("Couldn't open ebe_pde_$type".".csv : $!");
				print DAT "ID,".join(',',@eta_headers)."\n";
				for (my $i=0; $i<scalar(@{$pde->[0]});$i++){
					print DAT $id_matrix->[0]->[$i]->[0];
					for (my $j=0; $j<scalar(@{$pde});$j++){
						print DAT ','.formatnpde($pde->[$j]->[$i]);
					}
					print DAT "\n";
				}
				close (DAT);
			}
			if ($have_CDF){
				for (my $i=0; $i<scalar(@all_npde);$i++){ #loop id
					for (my $j=0; $j<scalar(@{$npde});$j++){ #loop eta
						push(@{$all_npde[$i]},$npde->[$j]->[$i]);
					}
				}


				if (0){
					$ret = simeval_util::npde_comp($est_matrix,$pd,$npd);
					unless ($ret ==0){
						print "\nError in npde_comp for ebe: $ret. ebe results cannot be computed\n";
						last;
					}
					open(DAT, ">ebe_npd_$type".".csv") || die("Couldn't open ebe_npd_$type".".csv : $!");
					print DAT "ID,".join(',',@eta_headers)."\n";
					for (my $i=0; $i<scalar(@{$npd->[0]});$i++){
						print DAT $id_matrix->[0]->[$i]->[0];
						for (my $j=0; $j<scalar(@{$npd});$j++){
							print DAT ','.formatnpde($npd->[$j]->[$i]);
						}
						print DAT "\n";
					}
					close (DAT);
				}
			}
		}#end loop etatypes
		if ($have_CDF){
			open(DAT, ">$ebe_npde_file") || die("Couldn't open $ebe_npde_file : $!");
			print DAT "ID,STAND_EBE,".join(',',@all_eta_headers)."\n";
			for (my $i=0; $i<scalar(@all_npde);$i++){
				print DAT $id_matrix->[0]->[$i]->[0].','.formatfloat($standardized[$i]);
				for (my $j=0; $j<scalar(@{$all_npde[$i]});$j++){
					print DAT ','.formatnpde($all_npde[$i]->[$j]);
				}
				print DAT "\n";
			}
			close (DAT);

		}
		last; #must break while here
	}
	return ($ret_successful_samples,$ret_subjects);
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
			trace(tool => 'simeval', message => "Preparing to rearrange raw_results in memory, adding ".
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
					ui -> print( category => 'simeval',
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
					ui -> print( category => 'simeval',
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

	my @residual_files = ($cwres_file);
	my @residual_names = ('CWRES');
	if ($self->have_iwres){
		push(@residual_files,$iwres_file);
		push(@residual_names,'IWRES');
	}

	my @vpctabs=();
	foreach my $filename (@{$self->vpctab_filenames}){
		push(@vpctabs,rplots::double_backslashes(string => $filename));
	}
	my @vpcresults=();
		
	foreach my $filename (@{$self->vpc_result_files}){
		push(@vpcresults,rplots::double_backslashes(string => $filename));
	}

	
	$rplot->add_preamble(code => [
							 '#simeval-specific preamble',
							 'samples   <-'.$self->samples,
							 'successful.samples  <- '.$self->successful_samples,
							 $outlying,
							 'n.subjects   <-'.$self->subjects,
							 "ebe.npde.file <- '".$ebe_npde_file."'",
							 "iofv.file <- '".$iofv_file."'",
							 "residual.files <- c('".join("','",@residual_files)."')",
							 "residual.names <- c('".join("','",@residual_names)."')",
							 "all.iofv.file <- '".$all_iofv_file."'",
#							 "all.iwres.file <- '".$all_iwres_file."'",
#							 "all.cwres.file <- '".$all_cwres_file."'",
							 'occasions   <-'.$self->occasions,
							 "all.eta.names <-  c('".join("','",@all_eta)."')",
							 'all.eta.numbers <-  c('.join(',',@all_eta_numbers).')',
							 $iiv_eta,
							 $iov_eta,
							 "vpctab.filenames <-  c('".join("','",@vpctabs)."')",
							 "vpc.result.files <-  c('".join("','",@vpcresults)."')",
							 "vpc.names <-  c('".join("','",@{$self->vpc_names})."')",
						 ]);

}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
