package tool::sse;

use include_modules;
use tool::cdd;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Config;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'samples' => ( is => 'rw', isa => 'Int', default => 100 );
has 'alternative_models' => ( is => 'rw', isa => 'ArrayRef[model]', default => sub { [] } );
has 'random_estimation_inits' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'mc_models' => ( is => 'rw', isa => 'ArrayRef[model]', default => sub { [] } );
has 'initial_values' => ( is => 'rw', isa => 'Any', clearer => 'clear_initial_values' );
has 'first_alternative' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'bayes' => ( is => 'rw', isa => 'Bool', isa => 0 );
has 'simulation_rawres' => ( is => 'rw', isa => 'Str' );
has 'in_filter' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'out_filter' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'covariance_file' => ( is => 'rw', isa => 'Str' );
has 'rawres_input' => ( is => 'rw', isa => 'Str' );
has 'offset_rawres' => ( is => 'rw', isa => 'Int', default => 1 );
has 'add_models' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'recompute' => ( is => 'rw', isa => 'Str' );
has 'ref_ofv' => ( is => 'rw', isa => 'Num' );
has 'parallel_simulations' => ( is => 'rw', isa => 'Int' );
has 'estimate_simulation' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'keep_tables' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_nwpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_tnpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'probnum' => ( is => 'rw', isa => 'Int', default => 1 );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['sse.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'sse_results.csv' );

sub BUILD
{
	my $self = shift;

	if ($self->random_estimation_inits and not defined $self->rawres_input) {
		croak('Need rawres_input when using random_estimation_inits');
	}
	
	for my $accessor ('logfile', 'raw_results_file', 'raw_nonp_file'){
		my @new_files = ();
		my @old_files = @{$self->$accessor};
		for (my $i=0; $i < scalar(@old_files); $i++){
			my $name;
			my $ldir;
			( $ldir, $name ) = OSspecific::absolute_path( $self->directory(), $old_files[$i] );
			push(@new_files, $ldir . $name) ;
		}
		$self->$accessor(\@new_files);
	}	

	if ( scalar (@{$self -> models->[0]-> problems}) > 2 ) {
		croak('Cannot have more than two $PROB in the simulation model.');
	} elsif (scalar (@{$self -> models->[0]-> problems}) == 2 ) {
		if ((defined $self -> models->[0]-> problems->[0]->priors()) and 
			scalar(@{$self -> models->[0]-> problems->[0] -> priors()})>0 ){
			my $tnpri=0;
			foreach my $rec (@{$self -> models->[0]-> problems->[0] -> priors()}){
				unless ((defined $rec) &&( defined $rec -> options )){
					carp("No options for rec \$PRIOR");
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
			if (defined $self->rawres_input){
				croak('Cannot use option rawres_input if the simulation model has $PRIOR.');
			}
			unless( defined $self -> models->[0]-> extra_files ){
				croak('When using $PRIOR TNPRI you must set option -extra_files to '.
					  'the msf-file, otherwise the msf-file will not be copied to the NONMEM '.
					  'run directory.');
			}

		}else{
			croak('The simulation model must contain exactly one problem, unless'.
				  ' first $PROB has $PRIOR TNPRI');
		}
	}
	if ((not $self->have_tnpri()) and
		(defined $self->models->[0]->problems->[0]->priors()) and 
		scalar(@{$self->models->[0]->problems->[0]->priors()})>0 ) {
		$self->have_nwpri(1);
		if (defined $self->rawres_input) {
			croak('Cannot use option rawres_input if the simulation model has $PRIOR.');
		}
	}
	if ($self->estimate_simulation && (defined $self->ref_ofv)) {
		croak("Not allowed to use a reference ofv-value when estimating the simulation model.");
	}

	unless (defined $self->recompute) {
		foreach my $alt (@{$self -> alternative_models}){
			if ( (scalar (@{$alt -> problems}) < 1 )|| ( scalar (@{$alt -> problems}) > 2 )) {
				croak('The alternative models must each contain either one or two problems.');
			}
		}

		if ((scalar(@{$self -> alternative_models}) < 1) && (not $self->estimate_simulation)) {
			print "\nWarning: No model to estimate. Will only simulate.\n";
		}

		if ($self->samples < 2) {
			croak("Must set -samples to at least 2.");
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

{ 
	return if (defined $self->recompute);
	my $model = $self -> models -> [$model_number-1];
	my @alternatives = @{$self -> alternative_models}; #may be empty
	unless ($self->keep_tables){
		$model -> remove_records( type => 'table' );
		for (my $i=0; $i< scalar(@alternatives); $i++){
			$alternatives[$i]-> remove_records( type => 'table' );
		}
	}
	my @seed;
	my ( @orig_est_models, @alt_est_models );
	my ( $sim_model, $est_alternative );
	my $alternative;
	my $old_alternatives=0;
	my $time_in_input=0;
	my $datx_in_input=0;
	my $addstring='';
	my $done = ( -e $self -> directory."/m$model_number/done" ) ? 1 : 0;
	$self->first_alternative(1);
	my @thetalabels;
	my @omegalabels;
	my @sigmalabels;
	my @thetaoriginals;
	my @omegaoriginals;
	my @sigmaoriginals;
	my @initscode;
	$self->probnum(2) if ($self->have_tnpri);

	if ($self->add_models){
		unless ($done) {
			croak("To use the option -add_models, the old sse run must be complete.");
		}
		while ((-e $self -> directory.'m'.$model_number.'/mc-alternative_'.
				($old_alternatives+1).'-1.mod') or 
			   (-e $self -> directory.'m'.$model_number.'/mc-alt_'.
				($old_alternatives+1).'-1.mod'))
		{
			$old_alternatives++;
		} 
		$self->first_alternative($old_alternatives+1);
		#change results_file so that old is not overwritten
		my $fname = $self->results_file();
		$fname =~ s/\.csv$/_add/ ;
		my $addnum=1;
		while (-e $self->directory."/$fname$addnum".'.csv'){
			$addnum++;
		}

		my $rawfname; 
		$rawfname = $self->raw_results_file->[$model_number-1];


		if ($self->estimate_simulation){
			#check if have estimated originals in original raw results
			if (-e $rawfname){
				open(RAW, $rawfname) or die "Could not open file $rawfname for reading.\n";
				my $estcounter=0;
				while(<RAW>) {
					$estcounter++ if (/^simulation\,/);
				}
				close(RAW);
				if ($estcounter == $self->samples()){
					$self->simulation_rawres($rawfname);
				}else{
					1;
				}
				
			}else{
				print "$rawfname does not exist\n";
			}
		}

		$rawfname =~ s/\.csv$/_add/ ;
		while (-e $self -> directory."/$rawfname$addnum".'.csv'){
			$addnum++;
		}
		$addstring='_add'.$addnum;
		my $nonpname = $self->raw_nonp_file->[$model_number-1];
		$nonpname =~ s/\.csv$/_add/ ;

		$self->results_file("$fname$addnum" . '.csv');
		$self -> raw_results_file -> [$model_number-1] = "$rawfname$addnum".'.csv';
		$self -> raw_nonp_file->[$model_number-1]=("$nonpname$addnum".'.csv');
	}

	if ( (not $done)  or $self->add_models) {
		
		# this is code for the either first run , i.e. not a restart.
		# or a restart with added alternative models

		my @sim_models;
		my @table_header;
		my @all_simulated_files;
		my ( @orig_table_names, @alt_table_names );
		my @msfo_stems_original = (); #one element for each $EST
		my $sampled_params_arr;
		
		for ( my $sim_no = 1; $sim_no <= $self->samples; $sim_no++ ) {
			
			# Copy the model to new simulation models

			my $est_original;
			my $sim_name = "mc-$sim_no.sim";
			my $orig_name = "mc-orig-$sim_no.mod";
			my $out_name = "mc-$sim_no.lst";
			my $orig_out = "mc-orig-$sim_no.lst";
			if ( $sim_no == 1 ) {
				if ($self->add_models) {
					#get table file names from original model. Keep in simulation if keep_tables is set
					my $tbl_nm_ref = $model -> get_option_value(
						record_name  => 'table',
						option_name  => 'FILE',
						record_index => 'all',
						problem_index => ($self->probnum-1));
					
					if( defined $tbl_nm_ref ) {
						for (my $k = 0; $k < scalar(@{$tbl_nm_ref}); $k++) {
							if (defined $tbl_nm_ref->[$k]) {
								my $name = $tbl_nm_ref->[$k];
								$name =~ s/[0-9]*$//;
								push(@orig_table_names, $name);
							} else {
								push(@orig_table_names, undef);
							}
						}
					}

				} else {
					$sim_model = $model->copy(
						filename    => $self->directory . 'm' . $model_number . '/' . $sim_name,
						copy_datafile   => 1,    #copy original datafile once to m1, TODO change this if commandline -no-copy_data
						write_copy => 0,
						copy_output => 0);

					if ($sim_model-> is_option_set(record=>'input',name=>'TIME')) {
						#this assumes no synonym, and TIME is always option, not value.
						$time_in_input = 1;
					}
					foreach my $col ('DATE','DAT1','DAT2','DAT3') {
						if ($sim_model->is_option_set(record => 'input', name => $col)) {
							#this assumes no synonym, and name always options, not value.
							$datx_in_input = 1;
							last;
						}
					}
					
					#set IGNORE=@ since datafile will
					#get a header during copying. Keep IGNORE=LIST
					for (my $in=0; $in< $self->probnum; $in++){
						$sim_model -> problems->[$in]->datas->[0]->ignoresign('@');
					}
					$self->initial_values(ext::Config::Tiny -> new());

					#get table file names from original model. Keep in simulation
					my $tbl_nm_ref = 
						$sim_model -> get_option_value( record_name  => 'table',
														option_name  => 'FILE',
														record_index => 'all',
														problem_index => ($self->probnum()-1));
					
					if( defined $tbl_nm_ref ) {
						for (my $k = 0; $k < scalar(@{$tbl_nm_ref}); $k++) {
							if (defined $tbl_nm_ref->[$k]) {
								my $name = $tbl_nm_ref->[$k];
								$name =~ s/[0-9]*$//;
								push(@orig_table_names,$name);
							} else {
								push(@orig_table_names,undef);
							}
						}
					}

					my $model_index = 0;

					if (defined $self->rawres_input) {
						#cannot have tnpri if in here, need not use $self->probnum
						@thetalabels = @{$sim_model -> labels( parameter_type => 'theta', generic => 0)};
						@omegalabels = @{$sim_model -> labels( parameter_type => 'omega', generic => 0)};
						@sigmalabels = @{$sim_model -> labels( parameter_type => 'sigma', generic => 0)};
						unless (defined $thetalabels[0] and defined $omegalabels[0] and defined $sigmalabels[0]){
							croak("all labels references are not defined in setup sse");
						}
						$sampled_params_arr = 
							$sim_model->get_rawres_params(filename => $self->rawres_input,
														  filter => $self->in_filter,
														  offset => $self->offset_rawres);
						if (defined $sampled_params_arr) {
							unless (scalar(@{$sampled_params_arr}) >= ($self->samples)) {
								if (defined $self->in_filter) {
									croak("Too few sets (lines) of parameter values in\n".
										  $self->rawres_input."\nafter filtering. Have ".scalar(@{$sampled_params_arr}).
										  " but need at least ".$self->samples.".\n");
								} else {
									croak("Too few sets (lines) of parameter values in\n".
										  $self->rawres_input.". Have ".scalar(@{$sampled_params_arr}).
										  " but need at least ".
										  ($self->samples + $self->offset_rawres).".\n");
								}
							}
						} else {
							croak("get_rawres_params returned undef");
						}

						if ( $self->random_estimation_inits){
							#do not simulate with uncertainty but estimate with random inits instead.
							#must still store simulation initial values even if do not simulate with uncertainty

							my $tmp = $sim_model->nsigmas(problem_numbers => [1],
														  with_correlations => 1);
							my $n_initials = $tmp->[0];
							$tmp = $sim_model->initial_values(problem_numbers => [1],	
															  parameter_type => 'sigma',
															  get_same => 1);
							my @sigmavalues=@{$tmp->[0]};
							croak('Error number of initial sigmas.') 
								unless (scalar(@sigmavalues) == $n_initials);

							$tmp = $sim_model->nomegas(problem_numbers => [1],
													   with_correlations => 1);
							$n_initials = $tmp->[0];
							$tmp = $sim_model->initial_values(problem_numbers => [1],	
															  parameter_type => 'omega',
															  get_same => 1);
							my @omegavalues=@{$tmp->[0]};
							croak('Error number of initial omegas.') 
								unless (scalar(@omegavalues) == $n_initials);
							$tmp = $sim_model->nthetas(problem_number => 1);

							$n_initials = $tmp;
							$tmp = $sim_model->initial_values(problem_numbers => [1],	
															  parameter_type => 'theta');
							my @thetavalues=@{$tmp->[0]};
							croak('Error number of initial thetas.') 
								unless (scalar(@thetavalues) == $n_initials);

							$self -> initial_values -> {$model_index} -> {'theta'} = join(',',@thetavalues);
							$self -> initial_values -> {$model_index} -> {'omega'} = join(',',@omegavalues);
							$self -> initial_values -> {$model_index} -> {'sigma'} = join(',',@sigmavalues);


						}else{
							$sim_model -> update_inits(from_hash => $sampled_params_arr->[0]); 
							my @paramarr = ();
							
							foreach my $label (@{$thetalabels[0]}){
								push(@paramarr,$sampled_params_arr->[0]->{'theta'}->{$label});
							}
							$self -> initial_values -> {$model_index} -> {'theta'} = join(',',@paramarr);
							@paramarr = ();
							foreach my $label (@{$omegalabels[0]}){
								push(@paramarr,$sampled_params_arr->[0]->{'omega'}->{$label});
							}
							$self -> initial_values -> {$model_index} -> {'omega'} = join(',',@paramarr);
							@paramarr = ();
							foreach my $label (@{$sigmalabels[0]}){
								push(@paramarr,$sampled_params_arr->[0]->{'sigma'}->{$label});
							}
							$self -> initial_values -> {$model_index} -> {'sigma'} = join(',',@paramarr);
						}

					}elsif (defined $self->covariance_file){
						#another way of simualting with uncertainty
						#cannot have tnpri if in here, need not use $self->probnum()
						@thetalabels = @{$sim_model -> labels( parameter_type => 'theta', generic => 0)};
						@omegalabels = @{$sim_model -> labels( parameter_type => 'omega', generic => 0)};
						@sigmalabels = @{$sim_model -> labels( parameter_type => 'sigma', generic => 0)};
						unless (defined $thetalabels[0] and defined $omegalabels[0] and defined $sigmalabels[0]){
							croak("all labels references are not defined in setup sse");
						}
						$sampled_params_arr = 
							$sim_model -> get_covariance_params(filename => $self->covariance_file(),
																samples => $self->samples());
						if (defined $sampled_params_arr){
							unless (scalar(@{$sampled_params_arr}) >= ($self->samples())){
								croak("Too few sets of parameter values generated ".
									  "by get_covariance_params. This is a bug, please report it. ");
							}
						}else{
							croak("get_covariance_params returned undef");
						}

						$sim_model -> update_inits(from_hash => $sampled_params_arr->[0]); 
						my @paramarr = ();
						
						foreach my $label (@{$thetalabels[0]}){
							push(@paramarr,$sampled_params_arr->[0]->{'theta'}->{$label});
						}
						$self -> initial_values -> {$model_index} -> {'theta'} = join(',',@paramarr);
						@paramarr=();
						foreach my $label (@{$omegalabels[0]}){
							push(@paramarr,$sampled_params_arr->[0]->{'omega'}->{$label});
						}
						$self -> initial_values -> {$model_index} -> {'omega'} = join(',',@paramarr);
						@paramarr=();
						foreach my $label (@{$sigmalabels[0]}){
							push(@paramarr,$sampled_params_arr->[0]->{'sigma'}->{$label});
						}
						$self -> initial_values -> {$model_index} -> {'sigma'} = join(',',@paramarr);
					}elsif ($self->have_nwpri() or $self->have_tnpri()){
						#another way of simualting with uncertainty
						#only store sigmas. thetas and omegas come after sim
						#must use $self->probnum()
						@thetalabels = @{$sim_model -> labels( parameter_type => 'theta', generic => 0)};
						@omegalabels = @{$sim_model -> labels( parameter_type => 'omega', generic => 0)};
						@sigmalabels = @{$sim_model -> labels( parameter_type => 'sigma', generic => 0)};
						my $tmp = $sim_model->nsigmas(problem_numbers => [$self->probnum],
													  with_correlations => 1);
						my $n_initials = $tmp->[0];#still first element here since only asked for 1
						$tmp = $sim_model->initial_values(problem_numbers => [$self->probnum],	
														  parameter_type => 'sigma');
						my @sigmavalues=@{$tmp->[0]}; #still first element here since only asked for 1
						croak('Error number of initial sigmas.') 
							unless (scalar(@sigmavalues) == $n_initials);
						$self -> initial_values -> {$model_index} -> {'sigma'} = join(',',@sigmavalues);

						$tmp = $sim_model->initial_values(problem_numbers => [$self->probnum],	
														  parameter_type => 'omega');
						my @omegavalues=@{$tmp->[0]};

						$tmp = $sim_model->initial_values(problem_numbers => [$self->probnum],	
														  parameter_type => 'theta');
						my @thetavalues=@{$tmp->[0]};
						@thetaoriginals=@thetavalues;
						@omegaoriginals=@omegavalues;
						@sigmaoriginals=@sigmavalues;


					} else {
						#we do not simulate with uncertainty
						#need not use probnum

						my $tmp = $sim_model->nsigmas(problem_numbers => [1],
													  with_correlations => 1);
						my $n_initials = $tmp->[0];
						$tmp = $sim_model->initial_values(problem_numbers => [1],	
														  parameter_type => 'sigma',
														  get_same => 1);
						my @sigmavalues=@{$tmp->[0]};
						croak('Error number of initial sigmas.') 
							unless (scalar(@sigmavalues) == $n_initials);

						$tmp = $sim_model->nomegas(problem_numbers => [1],
												   with_correlations => 1);
						$n_initials = $tmp->[0];
						$tmp = $sim_model->initial_values(problem_numbers => [1],	
														  parameter_type => 'omega',
														  get_same => 1);
						my @omegavalues=@{$tmp->[0]};
						croak('Error number of initial omegas.') 
							unless (scalar(@omegavalues) == $n_initials);
						$tmp = $sim_model->nthetas(problem_number => 1);

						$n_initials = $tmp;
						$tmp = $sim_model->initial_values(problem_numbers => [1],	
														  parameter_type => 'theta');
						my @thetavalues=@{$tmp->[0]};
						croak('Error number of initial thetas.') 
							unless (scalar(@thetavalues) == $n_initials);

						$self -> initial_values -> {$model_index} -> {'theta'} = join(',',@thetavalues);
						$self -> initial_values -> {$model_index} -> {'omega'} = join(',',@omegavalues);
						$self -> initial_values -> {$model_index} -> {'sigma'} = join(',',@sigmavalues);

					}
				} #end if else add_models

				if ( $self->estimate_simulation and (not defined $self->simulation_rawres)) {
					
					$est_original = $model->copy(
						filename    => $self -> directory.'m'.$model_number.'/'.$orig_name,
						copy_datafile   => 0,
						write_copy => 0,
						copy_output => 0);

					foreach my $modprob (@{$est_original->problems()}){
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
							}
						}
					}



					$est_original -> remove_records( type => 'simulation' );

					#handle msfo numbering. first get msfo option from all estimation records.
					#save stems, i.e. file name without trailing numbers, if any
					#last remove option completely, will be set with new value frther down
					my $msfolist_original = $est_original -> get_option_value( record_name  => 'estimation',
																			   option_name  => 'MSFO',
																			   record_index => 'all');
					if (defined $msfolist_original){
						for (my $k=0; $k<scalar(@{$msfolist_original}); $k++){
							if (defined $msfolist_original->[$k]){
								my $name = $msfolist_original->[$k];
								$name =~ s/[0-9]*$//;
								push(@msfo_stems_original,$name);
							}else{
								push(@msfo_stems_original,undef);
							}
						}
					}
					if ($self-have_tnpri() or $self->have_nwpri()){
						$est_original -> remove_option( record_name  => 'prior',
														problem_numbers => [(1)],
														option_name  => 'PLEV',
														fuzzy_match => 1);
					}

					#ignore @ since simdata contains header rows. can skip old ignores since filtered
					#set for all $PROB
					$est_original -> remove_option( record_name  => 'data',
													option_name  => 'IGNORE',
													fuzzy_match => 1);
					for (my $in=0; $in< scalar(@{$est_original -> problems}); $in++){
						$est_original -> problems->[$in]->datas->[0]->ignoresign('@');
					}

					#remove any DATX from est_original $INPUT
					if ($datx_in_input){
						foreach my $col ('DATE','DAT1','DAT2','DAT3'){
							#all $PROB
							$est_original -> remove_option(record_name => 'input',
														   option_name => $col);
						}
						unless ($time_in_input){
							#all $PROB
							$est_original -> set_option(record_name => 'input',
														option_name => 'TIME');
						}
					}
					
					$est_original -> remove_option( record_name  => 'table',
													option_name  => 'FILE',
													fuzzy_match => 1,
													record_number => 0); #0 means all
					if (defined $sampled_params_arr){
						$est_original -> update_inits(from_hash => $sampled_params_arr->[0],
													  problem_number => $self->probnum()); 
					}
				} # end 	if( $self -> estimate_simulation  and not $self->simulation_rawres) {
				
			} else {
				#not first sim model
				unless ($self->add_models) {
					$sim_model = $sim_models[0]->copy(
						filename    => $self -> directory.'m'.$model_number.'/'.$sim_name,
						copy_datafile   => 0,
						write_copy => 0,
						copy_output => 0);
					if (defined $sampled_params_arr and (not $self->random_estimation_inits)) {
						$sim_model->update_inits(from_hash => $sampled_params_arr->[($sim_no-1)]); 
						my @paramarr = ();
						foreach my $label (@{$thetalabels[0]}) {
							push(@paramarr,$sampled_params_arr->[($sim_no-1)]{'theta'}{$label});
						}
						$self -> initial_values -> {($sim_no-1)} -> {'theta'} = join(',',@paramarr);
						@paramarr=();
						foreach my $label (@{$omegalabels[0]}){
							push(@paramarr,$sampled_params_arr->[($sim_no-1)]{'omega'}{$label});
						}
						$self -> initial_values -> {($sim_no-1)} -> {'omega'} = join(',',@paramarr);
						@paramarr=();
						foreach my $label (@{$sigmalabels[0]}){
							push(@paramarr,$sampled_params_arr->[($sim_no-1)]{'sigma'}{$label});
						}
						$self -> initial_values -> {($sim_no-1)} -> {'sigma'} = join(',',@paramarr);
					} elsif($self->have_nwpri) {
						$self -> initial_values -> {($sim_no-1)} -> {'sigma'} = 
							$self -> initial_values -> {0} -> {'sigma'};
					} else {
						#no simulation with uncertainty, just copy inits from first sample, because they are all the same
						$self -> initial_values -> {($sim_no-1)} -> {'theta'} = 
							$self -> initial_values -> {0} -> {'theta'};
						$self -> initial_values -> {($sim_no-1)} -> {'omega'} = 
							$self -> initial_values -> {0} -> {'omega'};
						$self -> initial_values -> {($sim_no-1)} -> {'sigma'} = 
							$self -> initial_values -> {0} -> {'sigma'};
					}
				}

				if( $self->estimate_simulation and (not defined $self->simulation_rawres)) {
					$est_original = $orig_est_models[0]->copy(
						filename    => $self -> directory . 'm' . $model_number . '/' . $orig_name,
						write_copy => 0,
						copy_datafile   => 0,
						copy_output => 0);
					
					if (defined $sampled_params_arr){
						$est_original -> update_inits(from_hash => $sampled_params_arr->[($sim_no-1)]); 
					}
				}
			}

			unless ($self->add_models()){
				$sim_model -> ignore_missing_files( 1 );
				$sim_model -> outputfile( $self -> directory.'m'.$model_number.'/'.$out_name );
				$sim_model -> ignore_missing_files( 0 );
			}
			if( $self -> estimate_simulation and (not defined $self->simulation_rawres)){
				$est_original -> ignore_missing_files( 1 );
				$est_original -> outputfile( $self -> directory.'m'.$model_number.'/'.$orig_out );
				$est_original -> ignore_missing_files( 0 );
			}

			if( $self -> shrinkage ) {
				
				if( $self -> estimate_simulation and (not defined $self->simulation_rawres)){
					$est_original -> shrinkage_stats( enabled => 1 );
				}
			}
			
			# The simulation model must contain exactly one problem      

			unless ($self->add_models()){

				my $prob = $sim_model -> problems -> [$self->probnum()-1];
				my $codeprob = $sim_model -> problems -> [0];
				
				# set $SIMULATION record
				
				my $sim_record = $sim_model -> record( problem_number => $self->probnum(),
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
								$new_line .= random_uniform_integer( 1, 1, 1000000 ); # Upper limit is from nmhelp 
								$old_seed = $3;
							}
							
							$new_line .= $old_seed;
							
						}
						
						push( @new_record, $new_line.$sim_line );
					}
					
					$prob -> set_records( type => 'simulation',
										  record_strings => \@new_record );
					foreach my $altopt ('SUBPROBLEMS','SUBPROBS','NSUBPROBLEMS','NSUBPROBS','NSUBS'){
						#many synonyms for same thing
						$prob -> remove_option( record_name  => 'simulation',
												option_name  => $altopt,
												fuzzy_match => 1); #default is remove for all records
					}
					
				} else {
					
					my $seed = random_uniform_integer( 1, 1, 1000000 ); # Upper limit is from nmhelp
					$prob -> set_records( type           => 'simulation',
										  record_strings => [ '(' . $seed .
															  ') ONLYSIMULATION' ] );
				}
				
				if( $sim_no == 1 ) {	  
					if ($self->have_nwpri() or $self->have_tnpri()){
						unless ($sim_model -> is_option_set( problem_number => $self->probnum(),
															 record => 'simulation',
															 name => 'TRUE',
															 fuzzy_match => 1 )){
							$sim_model -> set_option(record_name => 'simulation',
													 option_name => 'TRUE',
													 option_value=> 'PRIOR',
													 problem_numbers => [($self->probnum())]);
						}

						my @thetalab = @{$sim_model -> labels( parameter_type => 'theta', 
															   problem_numbers   => [$self->probnum()],
															   generic => 0)->[0]};
						my @thetacoords;
						for (my $i=1; $i<= scalar(@thetalab);$i++){
							push(@thetacoords,'THETA('.$i.')');
						}

						my @omegalab = @{$sim_model -> labels( parameter_type => 'omega', 
															   problem_numbers   => [($self->probnum())],
															   generic => 0)->[0]};
						my @omegacoords = @{$sim_model -> labels( parameter_type => 'omega', 
																  problem_numbers   => [($self->probnum())],
																  generic => 1)->[0]};
						
						unless (scalar(@omegalab)>0 and (scalar(@omegalab) eq scalar(@omegacoords))){
							croak("number of labels and coords for omega unequal: ".
								  scalar(@omegalab)." ".scalar(@omegacoords));
						}

						@initscode=('IF (ICALL.EQ.4.AND.NEWIND.EQ.0) THEN',
									"  OPEN(50,FILE='../../m1/mc-1.inits')",
									"  WRITE (50,*) '".join(' ',@thetalab)."'",
									'  WRITE (50,2) '.join(',',@thetacoords),
									"  WRITE (50,*) '".join(' ',@omegalab)."'",
									'  WRITE (50,2) '.join(',',@omegacoords),
									'  CLOSE (50)',
									'ENDIF');
						my @code; 
						@code = @{$sim_model -> pk( problem_number => 1 )};
						unless ( $#code > 0 ) {
							@code = @{$sim_model -> pred( problem_number => 1 )};
						}
						if ( $#code <= 0 ) {
							croak("Neither PK or PRED defined in " .
								  $sim_model -> filename . "\n" );
						}
						my $coderef;
						$coderef = $sim_model->problems->[0]-> pks -> [0] -> code if (defined $sim_model->problems->[0]-> pks);
						unless ( defined $coderef and scalar(@{$coderef}) > 0 ) {
							$coderef = $sim_model->problems->[0]-> preds -> [0] -> code;
						}
						push(@{$coderef},@initscode);

					}
					if( $sim_model -> is_option_set( problem_number => $self->probnum(),record => 'estimation',
													 name => 'LIKELIHOOD',fuzzy_match => 1 )
						or
						$sim_model -> is_option_set( problem_number => $self->probnum(),record => 'estimation',
													 name => '-2LOGLIKELIHOOD',fuzzy_match => 1 )
						or
						$sim_model -> is_option_set( problem_number => $self->probnum(), record => 'estimation',
													 name => '-2LLIKELIHOOD',fuzzy_match => 1 )
						or
						$sim_model -> is_option_set( problem_number => $self->probnum(), record => 'estimation',
													 name => 'LAPLACIAN',fuzzy_match => 1 )
						){
						#set_nopred_onlysim
						unless ($sim_model -> is_option_set( problem_number => $self->probnum(),
															 record => 'simulation',
															 name => 'NOPREDICTION',fuzzy_match => 1 )){
							$sim_model -> set_option(record_name => 'simulation',
													 option_name => 'NOPRED',
													 problem_numbers => [($self->probnum())]);
						}
						unless ($sim_model -> is_option_set( problem_number => $self->probnum(),
															 record => 'simulation',
															 name => 'ONLYSIMULATION',fuzzy_match => 1 )){
							$sim_model -> set_option(record_name => 'simulation',
													 option_name => 'ONLYSIM',
													 problem_numbers => [($self->probnum())]);
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
					$prob -> add_records( type           => 'table',
										  record_strings => [ join( ' ', @table_header ).
															  ' NOPRINT NOAPPEND ONEHEADER FILE=dummy']);


				} #end if sim_no==1

				for (my $k=0; $k<scalar(@orig_table_names); $k++){ 
					if (defined $orig_table_names[$k]){
						$prob -> remove_option( record_name  => 'table',
												option_name  => 'FILE',
												fuzzy_match => 1,
												record_number => ($k+1));

						$prob -> add_option(record_name  => 'table',
											record_number  => ($k+1),
											option_name  => 'FILE',
											option_value => $orig_table_names[$k].'-sim-'.$sim_no );   
					}
				}

				my $simulated_file = "mc-sim-$sim_no.dat";
				$prob -> remove_option( record_name  => 'table',
										option_name  => 'FILE',
										fuzzy_match => 1,
										record_number => (scalar(@orig_table_names)+1));
				
				$prob -> add_option(record_name  => 'table',
									record_number  => (scalar(@orig_table_names)+1),
									option_name  => 'FILE',
									option_value => $simulated_file );   


				push( @all_simulated_files, $self -> directory.'m'.$model_number.'/'.
					  $simulated_file );

				if ($self->have_nwpri or $self->have_tnpri()){
					#path to m1 to avoid extra output files

					#already have the code in place. Now need to substitute output file name
					my $coderef;
					$coderef = $codeprob-> pks -> [0] -> code if (defined $codeprob-> pks);
					unless ( defined $coderef and scalar(@{$coderef}) > 0 ) {
						$coderef = $codeprob-> preds -> [0] -> code;
					}
					my $string = 'mc-'.$sim_no.'.inits';
					foreach my $line (@{$coderef}){
						if (($line =~ /^  OPEN\(50,FILE=/) and ($line =~ /mc-1.inits/)){
							$line =~ s/mc-1.inits/$string/;
							last;
						}
					}
				}
				$sim_model -> _write(relative_data_path => 1); #we have copied original dataset to m1, so make sure use rel path here
				push( @sim_models, $sim_model );
			} #end if add_models
			if( defined $est_original ){
				push( @orig_est_models, $est_original );
			}
			
		} #end loop over number of simulations

		$self->stop_motion_call(tool=>'sse',message => "created simulation models in directory ".
								$self -> directory.'m'.$model_number)
			if ($self->stop_motion());

		if( $self -> estimate_simulation and (not defined $self->simulation_rawres)){
			#take care of msfo numbering
			#take care of table FILE numbering
			for( my $sim_no = 1; $sim_no <= $self -> samples; $sim_no++ ) {
				for (my $k=0; $k<scalar(@msfo_stems_original); $k++){ 
					if (defined $msfo_stems_original[$k]){
						$orig_est_models[($sim_no-1)] -> remove_option( record_name  => 'estimation',
																		option_name  => 'MSFO',
																		fuzzy_match => 1,
																		problem_numbers => [($self->probnum())],
																		record_number => 0); #0 means all
						$orig_est_models[($sim_no-1)] -> 
							add_option(record_name  => 'estimation',
									   record_number  => ($k+1),
									   option_name  => 'MSFO',
									   problem_numbers => [($self->probnum())],
									   option_value => $msfo_stems_original[$k].$sim_no );   
					}
				}
				for (my $k=0; $k<scalar(@orig_table_names); $k++){ 
					if (defined $orig_table_names[$k]){
						$orig_est_models[($sim_no-1)] -> 
							add_option(record_name  => 'table',
									   record_number  => ($k+1),
									   option_name  => 'FILE',
									   problem_numbers => [($self->probnum())],
									   option_value => $orig_table_names[$k].'-'.$sim_no );   
					}
				}
			}
		}

		unless ($self->add_models()){
			unless ($self->have_nwpri() or $self->have_tnpri()){
				$self -> initial_values -> write( $self -> directory.'simulation_initial_values' );
				$self->clear_initial_values;
			}
			my $threads = $self -> parallel_simulations ? $self -> parallel_simulations : $self->threads;

			$self->stop_motion_call(tool=>'sse',message => "Getting ready to run all the simulation models in ".
									$self -> directory.'m'.$model_number)
				if ($self->stop_motion());

			my $mod_sim = 
				tool::modelfit -> new(  %{common_options::restore_options(@common_options::tool_options)},
										top_tool         => 0,
										models           => \@sim_models,
										nmtran_skip_model => 2,
										base_directory   => $self -> directory,
										directory        => $self -> directory.'simulation_dir'.$model_number, 
										parent_tool_id   => $self -> tool_id,
										retries          => 1,
										logfile	         => undef,
										raw_results      => undef,
										prepared_models       => undef,
										threads          => $threads,
										copy_data        => 0,
										abort_on_fail => $self->abort_on_fail);
			
			$mod_sim -> run;
			if ($self->have_nwpri() or $self->have_tnpri()){
				#parse inits_sims
				for (my $i=1;$i<=$self->samples();$i++){
					my $file = $self->directory.'m'.$model_number.'/mc-'.$i.'.inits';
					open(INITS, $file) or die "Could not open file $file for reading.\n";
					my @lines = <INITS>;
					close( INITS);
					#4 lines, thetalabels, thetavalues, omegalabels, omegavalues
					$lines[1] =~ s/^\s*//;
					$lines[1] =~ s/\s*$//;
					$lines[3] =~ s/^\s*//;
					$lines[3] =~ s/\s*$//;
					my @vals = split(/\s+/,$lines[1]) if (defined $lines[1]); 
					$self -> initial_values -> {$i-1} -> {'theta'} = join(',',@vals);
					@vals = split(/\s+/,$lines[3]) if (defined $lines[3]); 
					$self -> initial_values -> {$i-1} -> {'omega'} = join(',',@vals);
				}
				$self -> initial_values -> write( $self -> directory.'simulation_initial_values' );
				#output to raw_results like file for possible reuse.
				my $file = $self -> directory.'initial_estimates.csv';
				open(INITS, '>'.$file) or die "Could not open file $file for writing.\n";
				#labels stored earlier
				my @header = ('model',@{$thetalabels[$self->probnum()-1]},@{$omegalabels[$self->probnum()-1]},@{$sigmalabels[$self->probnum()-1]});
				#To avoid problems if cells contain commas
				print INITS join(",",map {s/\"/\"\"/g; '"'.$_.'"'} @header ),"\n";

				my $original = '0,'.join(',',@thetaoriginals,).','.join(',',@omegaoriginals).','.
					join(',',@sigmaoriginals);
				print INITS $original."\n";
				for (my $i=1;$i<=$self->samples();$i++){
					print INITS "$i,".$self -> initial_values -> {$i-1} -> {'theta'}.
						','.$self -> initial_values -> {$i-1} -> {'omega'}.','.
						$self -> initial_values -> {$i-1} -> {'sigma'}."\n";
				}
				close (INITS);
				$self->clear_initial_values;
			}

		}


		for( my $j = 0; $j <= $#orig_est_models; $j++ ) {
			#can be 0 est models
			my $sim_file = $self -> directory.'m'.$model_number.'/'."mc-sim-".($j+1).".dat";

			#it is an error if the data file does not exist here, but we ignore that for now
			# and let the NM run fail instead, letting the other runs continur
			$orig_est_models[$j] -> ignore_missing_files(1);
			my @new_names = ($sim_file) x scalar(@{$orig_est_models[$j] ->problems});
			$orig_est_models[$j] -> datafiles(new_names => \@new_names);
			$orig_est_models[$j] -> _write(relative_data_path => 1); #should be default, but just to make sure
		}
		
		#could reload originals here, but difficult

		$self -> mc_models(\@orig_est_models ); #can be 0 if no-est

		my $alternative_counter = $self->first_alternative()-1;
		foreach $alternative (@alternatives){
			$alternative_counter++;
			# {{{ create copies of the alternative models
			@alt_table_names = ();
			@alt_est_models =();
			my $filestem = 'mc-alt_' . $alternative_counter;
			my @msfo_stems= (); #one element for each $EST
			for( my $sim_no = 1; $sim_no <= $self -> samples; $sim_no++ ) {
				my $alt_name = "$filestem-$sim_no.mod";
				my $alt_out = "$filestem-$sim_no.lst"; 
				
				if( $sim_no == 1 ) {
					$est_alternative = $alternative ->
						copy( filename    => $self -> directory.'m'.$model_number.'/'.$alt_name,
							  write_copy      => 0,
							  copy_datafile   => 0,
							  copy_output => 0);

					my $found_dropped=0;
					foreach my $modprob (@{$est_alternative->problems()}){
						my $inp_ref =  $modprob -> inputs();
						if ( defined $inp_ref and defined $inp_ref -> [0] ) {
							my $input = $inp_ref -> [0];
							my $opt_ref = $input -> options;
							if ( defined $opt_ref ) {
								my @options = @{$opt_ref};
								my @keep;
								foreach my $option ( @options ) {
									if ( not ($option -> value eq 'DROP' or $option -> value eq 'SKIP'
											  or $option -> name eq 'DROP' or $option -> name eq 'SKIP')){
										push ( @keep, $option ) ;
									}else{
										ui -> print( category => 'sse',
													 message  => "\nWarning: Removing DROP/SKIP items completely ".
													 "from \$INPUT of ".$alternative->filename().", assuming that ".
													 "those columns had DROP/SKIP also in simulation model ".
													 "and are not present in the simulated data.") 
											unless $found_dropped;
										$found_dropped =1;
									}
								}
								$input -> options( \@keep );
							}
						}
					}
					


					#handle msfo numbering. first get msfo option from all estimation records.
					#save stems, i.e. file name without trailing numbers, if any
					#last remove option completely, will be set with new value frther down
					my $msfolist = $est_alternative -> get_option_value( record_name  => 'estimation',
																		 problem_index  => 0,
																		 option_name  => 'MSFO',
																		 record_index => 'all');
					if (defined $msfolist){
						for (my $k=0; $k<scalar(@{$msfolist}); $k++){
							if (defined $msfolist->[$k]){
								my $name = $msfolist->[$k];
								$name =~ s/[0-9]*$//;
								push(@msfo_stems,$name);
							}else{
								push(@msfo_stems,undef);
							}
						}
					}
					$est_alternative -> remove_option( record_name  => 'estimation',
													   option_name  => 'MSFO',
													   fuzzy_match => 1,
													   problem_numbers => [(1)],
													   record_number => 0); #0 means all
					


					#remove any DATX in $INPUT
					foreach my $col ('DATE','DAT1','DAT2','DAT3'){
						$est_alternative -> remove_option(record_name => 'input',
														  problem_numbers => [(1)],
														  option_name => $col);
					}
					#if added time then remove TIME (if present) and then add TIME (must be last in list)
					if ((not $time_in_input) && ($datx_in_input)){
						$est_alternative -> remove_option(record_name => 'input',
														  problem_numbers => [(1)],
														  option_name => 'TIME');
						$est_alternative -> set_option(record_name => 'input',
													   problem_numbers => [(1)],
													   option_name => 'TIME');
					}

					$est_alternative -> remove_records(problem_numbers => [(1)],
													   type => 'simulation' );
					
					#ignore @ since simdata contains header rows. 
					#keep old ignores. It is up to the user to make sure datasets are comparable
					for (my $in=0; $in< scalar(@{$est_alternative -> problems}); $in++){
						$est_alternative -> problems->[$in]->datas->[0]->ignoresign('@');
					}

					my $ignorelist = $est_alternative -> get_option_value( record_name  => 'data',
																		   problem_index => 0,
																		   option_name  => 'IGNORE',
																		   option_index => 'all');

					if (scalar (@{$ignorelist})>0){
						foreach my $val (@{$ignorelist}){
							unless ($val =~ /^.$/){
								if ($val =~ /\.NE\.|\.EQ\.|=[^(]/){
									ui -> print( category => 'sse',
												 message  => "\nWarning: Found IGNORE using .EQ. or .NE. or = in alternative model.\n".
												 "NONMEM considers 1 not equal to 1.000 in \$DATA IGNORE statements,\n".
												 "and simulated data will be formatted differently than your original\n".
												 "data (via \$TABLE).\n".
												 "Check output carefully to make sure IGNORE has the intended effect.\n");
									print "\n";
								}
							}
						}
					}

					my $acceptlist = $est_alternative -> get_option_value( record_name  => 'data',
																		   problem_index => 0,
																		   option_name  => 'ACCEPT',
																		   option_index => 'all');
					if (scalar (@{$acceptlist})>0){
						foreach my $val (@{$acceptlist}){
							unless ($val =~ /^.$/){
								if ($val =~ /\.NE\.|\.EQ\.|=[^(]/){
									ui -> print( category => 'sse',
												 message  => "\nWarning: Found ACCEPT using .EQ. or .NE. or =\n".
												 "NONMEM considers 1 not equal to 1.000 in \$DATA ACCEPT statements.\n".
												 "Check output carefully to make sure ACCEPT has the intended effect.\n");
									print "\n";
								}
							}
						}
					}

					##done fixing ignore

					#this is for first problem
					my $tbl_nm_ref = $est_alternative -> get_option_value( record_name  => 'table',
																		   option_name  => 'FILE',
																		   record_index => 'all',
																		   problem_index => 0);
					if( defined $tbl_nm_ref ){
						for (my $k=0; $k<scalar(@{$tbl_nm_ref}); $k++){
							if (defined $tbl_nm_ref->[$k]){
								my $name = $tbl_nm_ref->[$k];
								$name =~ s/[0-9]*$//;
								push(@alt_table_names,$name);
							}else{
								push(@alt_table_names,undef);
							}
						}
					}
					$est_alternative -> remove_option( record_name  => 'table',
													   option_name  => 'FILE',
													   fuzzy_match => 1,
													   problem_numbers => [(1)],
													   record_number => 0); #0 means all
					
				} else {
					#not sim_no==1
					$est_alternative = $alt_est_models[0] ->
						copy( filename    => $self -> directory.'m'.$model_number.'/'.$alt_name,
							  write_copy      => 0,
							  copy_datafile   => 0,
							  copy_output => 0);
				}

				$est_alternative -> ignore_missing_files( 1 );
				$est_alternative -> outputfile( $self -> directory.'m'.$model_number.'/'.$alt_out );
				$est_alternative -> ignore_missing_files( 0 );

				if( $self -> shrinkage() ) {
					$est_alternative -> shrinkage_stats( enabled => 1 );

				}
				if (defined $sampled_params_arr and $self->random_estimation_inits) {
					$est_alternative -> update_inits(from_hash => $sampled_params_arr->[$sim_no-1]);

				}

				push( @alt_est_models, $est_alternative );

			} #end loop over sim_no

			#take care of msfo numbering
			my $second_prob = 0;
			if (scalar (@{$alt_est_models[0] -> problems()}) > 1){
				$second_prob=1;
			}
			for( my $sim_no = 1; $sim_no <= $self -> samples; $sim_no++ ) {
				my $last_name=undef;
				for (my $k=0; $k<scalar(@msfo_stems); $k++){ 
					if (defined $msfo_stems[$k]){
						$last_name = $msfo_stems[$k].$alternative_counter.'-'.$sim_no;
						$alt_est_models[($sim_no-1)] -> add_option(record_name  => 'estimation',
																   problem_numbers => [(1)],
																   record_number  => ($k+1),
																   option_name  => 'MSFO',
																   option_value => $last_name );
					}
				}
				for (my $k=0; $k<scalar(@alt_table_names); $k++){ 
					if (defined $alt_table_names[$k]){
						$alt_est_models[($sim_no-1)] -> 
							add_option(record_name  => 'table',
									   record_number  => ($k+1),
									   option_name  => 'FILE',
									   problem_numbers => [(1)],
									   option_value => $alt_table_names[$k].'-'.$alternative_counter.'-'.$sim_no );   
					}
				}

				# if more than one problem, set MSFI to same as last $k
				#assume exists $MSFI
				if ($second_prob){
					if ( defined $alt_est_models[($sim_no-1)] -> problems->[1]){
						$alt_est_models[($sim_no-1)] -> set_file( record => 'msfi',new_name => $last_name,
																  problem_number => 2);
					} else {
						"Error, could not change filename in MSFI record\n";
					}
					$alt_est_models[($sim_no-1)] 
						-> set_option( record_name  => 'table',
									   record_number => 1,
									   problem_numbers => [(2)],
									   option_name  => 'FILE',
									   option_value => 'simtab'.$alternative_counter.'-'.$sim_no.'.dat');
				}
			}
			

			for( my $j = 0; $j <= $#alt_est_models; $j++ ) {
				my $sim_file = $self -> directory.'m'.$model_number.'/'."mc-sim-".($j+1).".dat";
				#it is an error if the data file does not exist here, but we ignore that for now
				# and let the NM run fail instead, letting the other runs continue
				$alt_est_models[$j] -> ignore_missing_files(1);
				my @new_names = ($sim_file) x scalar(@{$alt_est_models[$j] ->problems});
				$alt_est_models[$j] -> datafiles(new_names => \@new_names);
				$alt_est_models[$j] -> _write;

			}
			push (@{$self -> mc_models}, @alt_est_models);
			
			# }}}
			
		} #end loop over alternatives
		$self->stop_motion_call(tool=>'sse',message => "Created estimation models in ".
								$self -> directory.'m'.$model_number.
								"\nfor all alternative ".
								"models and simulated datasets ")
			if ($self->stop_motion());
		
		# Create a checkpoint.
		unless ($done){
			open( DONE, ">".$self -> directory."/m$model_number/done" ) ;
			print DONE "Simulation from ",$sim_models[0] -> filename," through ",
			$sim_models[$#sim_models] -> filename," performed\n";
			print DONE $self -> samples." samples\n";
			@seed = random_get_seed();
			print DONE "seed: @seed\n";
			close( DONE );
		}

		#end if not done 
	} else {
		
		#$done=true and not add_model
		# Recreate the datasets and models from a checkpoint
		ui -> print( category => 'sse',
					 message  => "Recreating models from a previous run" );
		open( DONE, $self -> directory."/m$model_number/done" );
		my @rows = <DONE>;
		close( DONE );
		my ( $junk, $junk, $stored_filename1, $junk,
			 $stored_filename2, $junk ) = split(' ',$rows[0],6);
		my ( $stored_samples, $junk ) = split(' ',$rows[1],2);
		@seed = split(' ',$rows[2]);
		shift( @seed ); # get rid of 'seed'-word
		# Reinitiate the model objects
		if( $self -> estimate_simulation) {
			for ( my $j = 1; $j <= $stored_samples; $j++ ) {
				my ($model_dir, $orig_name) = OSspecific::absolute_path( $self -> directory.'/m'.
																		 $model_number,
																		 "mc-orig-$j.mod" );
				my ($out_dir, $orig_out) = OSspecific::absolute_path( $self -> directory.'/m'.
																	  $model_number,
																	  "mc-orig-$j.lst" );
				my $est_original = model ->
					new(%{common_options::restore_options(@common_options::model_options)},
						directory   => $model_dir,
						 filename    => $orig_name,
						 outputfile  => $orig_out,
						 extra_files => $model -> extra_files,
						 ignore_missing_files => 1,
						 extra_output => $model -> extra_output(),
						 cwres       => $model -> cwres());

				push( @orig_est_models, $est_original );
				my $nl = $j == $stored_samples ? "" : "\r"; 
				ui -> print( category => 'sse',
							 message  => ui -> status_bar( sofar => $j+1,
														   goal  => $stored_samples+1 ).$nl,
							 wrap     => 0,
							 newline  => 0 );
			} #end loop over samples
		}
		$self -> mc_models(\@orig_est_models);
		
		#start alternatives section
		my $alternative_counter=0;
		foreach $alternative (@alternatives){
			$alternative_counter++;
			my $filestem = 'mc-alt_' . $alternative_counter;
			@alt_est_models =();
			print "reload alternatives\n";
			for ( my $j = 1; $j <= $stored_samples; $j++ ) {
				my ($model_dir, $alt_name) = OSspecific::absolute_path( $self -> directory.'/m'.
																		$model_number,
																		"$filestem-$j.mod" );
				my ($out_dir, $alt_out) = OSspecific::absolute_path( $self -> directory.'/m'.
																	 $model_number,
																	 "$filestem-$j.lst" );
				my $est_alternative = model ->
					new( %{common_options::restore_options(@common_options::model_options)},
						 directory   => $model_dir,
						 filename    => $alt_name,
						 outputfile  => $alt_out,
						 extra_files => $alternative -> extra_files,
						 ignore_missing_files => 1,
						 extra_output => $model -> extra_output(),
						 cwres       => $model -> cwres());
				push( @alt_est_models, $est_alternative );
				my $nl = $j == $stored_samples ? "" : "\r"; 
				ui -> print( category => 'sse',
							 message  => ui -> status_bar( sofar => $j+1,
														   goal  => $stored_samples+1 ).$nl,
							 wrap     => 0,
							 newline  => 0 );
			} #end loop over samples
			push (@{$self -> mc_models}, @alt_est_models );
			
		} #end loop over alternatives
		
		ui -> print( category => 'sse', message  => " ... done." );
		random_set_seed( @seed );
		ui -> print( category => 'sse',
					 message  => "Using $stored_samples data sets, previously simulated ".
					 "from $stored_filename1 through $stored_filename2\n" )
			unless $self -> parent_threads > 1;
		
	} #end $done


	return if ((scalar(@{$self -> alternative_models}) < 1) && 
			   (not $self->estimate_simulation));
	
	my $subdir = 'modelfit';
	my @subtools = ();
	@subtools = @{$self -> subtools} if (defined $self->subtools);
	shift( @subtools );
	my %subargs = ();
	if ( defined $self -> subtool_arguments ) {
		%subargs = %{$self -> subtool_arguments};
	}

	for (my $i=0;$i< scalar(@{$self -> mc_models->[0]->problems()}); $i++){
		#get ref of array of methods
		my $methref = $self -> mc_models->[0]-> get_option_value(record_name => 'estimation', 
																 option_name => 'METHOD',
																 problem_index => $i, 
																 record_index => 'all'); 
		if (defined $methref){
			my $j= scalar(@{$methref})-1;
			if (defined $methref->[$j]){
				$self->bayes(1) if ($methref->[$j] eq 'BAYES' or 
									(index('BAYES',$methref->[$j])==0));
			}
		}
	}

	my $rerun=1;
	$self->stop_motion_call(tool=>'sse',message => "Preparing to run all estimation models ")
		if ($self->stop_motion());
	$self->tools([]) unless (defined $self->tools());
	push( @{$self -> tools},
		  tool::modelfit -> new(
			  %{common_options::restore_options(@common_options::tool_options)},
			  top_tool         => 0,
			  logfile	         => undef,
			  raw_results      => undef,
			  rerun    => $rerun,
			  prepared_models  => undef,
			  nmtran_skip_model => 2,
			  models         => $self -> mc_models,
			  nm_version     => $self -> nm_version,
			  parent_tool_id   => $self -> tool_id,
			  base_directory => $self -> directory,
			  directory      => $self -> directory.'/'.$subdir.'_dir'.$model_number.$addstring,
			  subtools       => $#subtools >= 0 ? \@subtools : undef,
			  shrinkage      => $self -> shrinkage,
			  _raw_results_callback => $self ->
			  _modelfit_raw_results_callback( model_number => $model_number ),
			  copy_data       => 0,
			  %subargs ) );
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
		OSspecific::absolute_path( $self -> directory,
								   $self -> raw_results_file ->[$model_number-1] );
	my ($npdir,$npfile) = 
		OSspecific::absolute_path( $self -> directory,
								   $self -> raw_nonp_file -> [$model_number-1]);

	$subroutine = sub {
		#can have 2 $PROB if tnpri and est_sim, interesting with 2nd $PROB only
		my $modelfit = shift;
		my $mh_ref   = shift;
		my %max_hash = %{$mh_ref};
		$modelfit -> raw_results_file( [$dir.$file] ); #file to print to
		$modelfit -> raw_nonp_file( [$npdir.$npfile] );
		
		#if prior tnpri nothing will be in raw_results for first $PROB, can
		#take first row for model as final estimates as usual, even if
		#it happens to be from second $PROB

		# {{{ New leading text on each row
		#we will rearrange rows so that all first $PROBS come before all second $PROBS. Then second
		#$PROBS will be ignored in analysis
		#exception if estimate_simulation and tnpri, then in the first model it is the second $PROB
		#that should be used

		if ( defined $modelfit -> raw_results() ) {
			$self->stop_motion_call(tool=>'sse',message => "Preparing to rearrange raw_results in memory, adding ".
									"model name information")
				if ($self->stop_motion());
			
			my @rows = @{$modelfit -> raw_results()};
			my $n_rows = scalar(@rows);
			my @firsts;
			my @seconds;
			my $last_model= 0;
			my $alternative_counter= $self->first_alternative()-1;
			
			my $sample = 0; 
			my $altname;
			
			my @simestrows;
			if (defined $self->simulation_rawres and -e $self->simulation_rawres){
				open(RAW, $self->simulation_rawres);
				while(<RAW>) {
					chomp;
					my @line = split(',',$_);
					push(@firsts,\@line) if ($line[0] eq 'simulation');
				}
				close(RAW);
				#already checked that right count of lines
				$alternative_counter++;
				$altname = 'mc-alternative_'.$alternative_counter;
			}elsif( $self -> estimate_simulation ){
				$altname = 'simulation';
			}else{
				$alternative_counter++;
				$altname = 'mc-alternative_'.$alternative_counter;
			}

			for (my $i=0; $i< $n_rows; $i++){
				my $this_model = $rows[$i]->[0]; 
				my $step= ($this_model-$last_model);
				if ($step < 0){
					ui -> print( category => 'sse',
								 message  => "Error: It seems the raw_results is not sorted".
								 ". Statistics in sse_results will be wrong");
				}else {
					#if 0 step and problem column is 1 then error
					$sample += $step; #normally +1, sometimes 0,sometimes 2 or more
					if ($step > 1){
						ui -> print( category => 'sse',
									 message  => "Warning: It seems the estimation of $altname".
									 "did not produce any results with dataset ".($sample-1).
									 ". Statistics in sse_results might be wrong, it is recommended".
									 "to check manually using raw_results.");
					}
					if ($step == 0 and ($rows[$i]->[1] == 1)){
						ui -> print( category => 'sse',
									 message  => "Warning: There seems to be missing lines in raw_results ".
									 "(step 0 and PROB is 1, model $this_model), or the lines are not sorted by PROBLEM. ".
									 "Statistics in sse_results might be wrong, it is recommended".
									 "to check manually using raw_results.");
					}
				}

				if ($sample > $self -> samples){
					#next alternative
					$alternative_counter++;
					$altname = 'mc-alternative_'.$alternative_counter;
					$sample=1;
				}
				if ($step > 0){
					#new model
					unshift( @{$rows[$i]}, $sample );
					unshift( @{$rows[$i]}, $altname );
					push(@firsts,$rows[$i]);
				}else{
					#step==0, second problem same model
					unshift( @{$rows[$i]}, $sample );
					unshift( @{$rows[$i]}, $altname );
					push(@seconds,$rows[$i]);
				}
				$last_model=$this_model;
			}

			push(@firsts,@seconds); #assume analysis ignores the last rows
			$modelfit -> raw_results(\@firsts); #replace with set of resorted $PROBS

			unshift( @{$modelfit -> raw_results_header}, 'sample' );
			unshift( @{$modelfit -> raw_results_header}, 'hypothesis' );

			$self->raw_line_structure($modelfit -> raw_line_structure);

			foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure})){
				foreach my $category (keys %{$self->raw_line_structure -> {$mod}}){
					next if ($category eq 'line_numbers');
					my ($start,$len) = split(',',$self->raw_line_structure -> {$mod}->{$category});
					$self->raw_line_structure -> {$mod}->{$category} = ($start+2).','.$len; #add 2 for hypothesis, simdatanum
				}
				$self->raw_line_structure -> {$mod}->{'hypothesis'} = '0,1';
				$self->raw_line_structure -> {$mod}->{'sample'} = '1,1';
			}
			if (not $self->add_models and $self -> estimate_simulation ){
				$self->raw_line_structure -> write( $dir.'raw_results_structure_simest' );
			}elsif (defined $self->simulation_rawres and -e $self->simulation_rawres
					and -e $dir.'raw_results_structure_simest'){
				#read and prepend to raw_line_structure
				my $structure = ext::Config::Tiny -> read($dir.'raw_results_structure_simest');
				#first move all old models 'samples' steps down. make sure to start from end
				#so that do not overwrite, sort descending
				foreach my $mod (sort({$b <=> $a} keys %{$self->raw_line_structure})){
					$self->raw_line_structure -> {($mod+$self->samples())} = 
						$self->raw_line_structure -> {$mod};
				}
				for (my $mod =1;$mod <= $self->samples(); $mod++){
					$self->raw_line_structure -> {$mod} = $structure -> {$mod};
				}

			}
			$self->raw_line_structure -> write( $dir.'raw_results_structure' );
		} #end if defined modelfit->raw_results
		if ( defined $modelfit -> raw_nonp_results() ) {
			my @rows = @{$modelfit -> raw_nonp_results()};
			my $n_rows = scalar(@rows);
			my @firsts;
			my @seconds;
			my $last_model= 0;
			my $alternative_counter= $self->first_alternative()-1;
			my $sample = 0; 
			my $altname;

			if( $self -> estimate_simulation ){
				$altname = 'simulation';
			}else{
				$alternative_counter++;
				$altname = 'mc-alternative_'.$alternative_counter;
			}

			for (my $i=0; $i< $n_rows; $i++){
				my $this_model = $rows[$i]->[0]; 
				my $step= ($this_model-$last_model);
				if ($step < 0){
					ui -> print( category => 'sse',
								 message  => "Warning: It seems the file nonparametric_raw_results is not sorted");
				}else {
					#if 0 step and problem column is 1 then error
					$sample += $step; #normally +1, sometimes 0,sometimes 2 or more
				}

				if ($sample > $self -> samples){
					#next alternative
					$alternative_counter++;
					$altname = 'mc-alternative_'.$alternative_counter;
					$sample=1;
				}
				if ($step > 0){
					#new model
					unshift( @{$rows[$i]}, $sample );
					unshift( @{$rows[$i]}, $altname );
					push(@firsts,$rows[$i]);
				}else{
					#step==0, second problem same model
					unshift( @{$rows[$i]}, $sample );
					unshift( @{$rows[$i]}, $altname );
					push(@seconds,$rows[$i]);
				}
				$last_model=$this_model;
			}

			push(@firsts,@seconds); #assume analysis ignores the last rows
			$modelfit -> raw_nonp_results(\@firsts); #replace with set of resorted $PROBS
			unshift( @{$modelfit -> raw_nonp_results_header}, 'sample' );
			unshift( @{$modelfit -> raw_nonp_results_header}, 'hypothesis' );

		} #end if defined modelfit->raw_nonp_results


		$self -> raw_results_header(\@{$modelfit -> raw_results_header});
		$self -> raw_nonp_results_header(\@{$modelfit -> raw_nonp_results_header});
		#  New header
		
	};
	return $subroutine;
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 }
		);
	my $model_number = $parm{'model_number'};

	return if (defined $self->recompute);
	return if ((scalar(@{$self -> alternative_models}) < 1) && 
			   (not $self->estimate_simulation));

	$self -> tools->[0] -> print_results if (defined $self->tools); 
}

sub prepare_results
{
	my $self = shift;

	$self->cleanup();
	my $n_alternatives=0;
	my $samples = $self -> samples;
	if (defined $self->recompute()){
		#change results_file so that old is not overwritten
		my $fname = $self->results_file();
		$fname =~ s/\.csv$/_recompute/ ;
		my $addnum=1;
		while (-e $self -> directory."/$fname$addnum".'.csv'){
			$addnum++;
		}
		$self->results_file("$fname$addnum".'.csv');
		$self -> raw_results_file->[0] = $self->directory().$self->recompute();
		$self->read_raw_results(); #only one model here, need to split on models?
		$self -> raw_results($self -> raw_results -> [0]); #each line is one model
		my $prevname = 'dummy';
		my $firstalt;
		$self->estimate_simulation(0);
		my $samp=0;
		foreach my $line (@{$self -> raw_results}){
			if ($prevname eq $line->[0]){
				$samp++;
			}else{
				$samp=1;
				$n_alternatives++ if ($line->[0]=~ /^mc\-alternative\_(\d+)/);
				$firstalt = $1 unless (defined $firstalt);
				$prevname=$line->[0];
				$self->estimate_simulation(1) if ($line->[0]=~ /^simula/);
			}
		}
		$samples = $samp;
		$self->raw_line_structure(ext::Config::Tiny -> read($self -> directory.'raw_results_structure'));

		$self->first_alternative($firstalt);
	}else{
		$self -> raw_results($self -> tools->[0] -> raw_results) if (defined $self->tools);
		$n_alternatives = scalar(@{$self -> alternative_models});
	}
	$self->stop_motion_call(tool=>'sse',message => "Computing statistics based on raw_results ")
		if ($self->stop_motion());
	
	my $model = $self -> models -> [0];
	
	my %orig_results_section;
	my @alt_results_sections;
	
	### Retrieve initial parameter estimates from original model.
	
	my %n_initials;

	my $initials = ext::Config::Tiny -> read($self -> directory.'simulation_initial_values');
	my $form_initials = $self->format_initials(initials_object =>$initials);

	foreach my $measure ('theta','omega','sigma'){
		my @arr = split(/,/,$initials -> {0} -> {$measure});
		$n_initials{$measure}=scalar(@arr);
	}
	$n_initials{'ofv'}=0;


	#parse filter
	my ($ref1,$ref2,$ref3);
	($ref1,$ref2,$ref3) = $model->setup_filter(filter => $self->out_filter())
		if (defined $self->out_filter());
	my @filter_column = @{$ref1} if (defined $ref1);
	my @filter_relation = @{$ref2} if (defined $ref2);
	my @filter_value = @{$ref3} if (defined $ref3);
	

	## Prepare general run info for output file
	my %return_section;
	$return_section{'name'} = 'SSE run info';
	my $modelname=$model ->filename();
	$return_section{'labels'} = [[],['Date','samples run','simulation model',
									 'PsN version','NONMEM version','Sim. with uncertainty']];
	my $uncertainty = 'no';
	if (defined $self->rawres_input() and (not $self->random_estimation_inits)){
		$uncertainty = 'raw results file';
	}elsif ($self->have_nwpri()){
		$uncertainty = 'NWPRI';
	}elsif ($self->have_tnpri()){
		$uncertainty = 'TNPRI';
	}
	my @datearr=localtime;
	my $the_date=($datearr[5]+1900).'-'.($datearr[4]+1).'-'.($datearr[3]);
	$return_section{'values'} = [[$the_date,$samples,$modelname,
								  'v'.$PsN::version,$self->nm_version,$uncertainty]];
	#results is initialized in tool.dia
	push( @{$self -> results->[0]{'own'}},\%return_section );


	### Prepare result section with ofv reference value , if used, and true parameter values for sim
	my $ofvname='ofv';
	$ofvname = 'DIC' if $self->bayes;

	my %reference_section;
	$reference_section{'name'} = 'Reference/True values';
	my @reference_headers=($ofvname);
	my @reference_values;
	if (defined $self->ref_ofv){
		@reference_values=($self->ref_ofv);
	}elsif ($self->estimate_simulation){
		@reference_values=('Sim. model result');
	} else{
		@reference_values=('Alt. model '.$self->first_alternative.' result');
	}

	unless ((defined $self->rawres_input() and $self->random_estimation_inits) or $self->have_nwpri() or $self->have_tnpri()){
		foreach my $measure ( 'theta','omega','sigma' ){
			my $tmp = $model->indexes(problem_numbers => [$self->probnum()],
									  parameter_type => $measure);
			my @list=@{$tmp->[0]};
			croak("Error number of $measure indices. model->indexes returns ".
				  scalar(@list)." but n_initials from file simulation_initial_values is ".$n_initials{$measure}) 
				unless (scalar(@list) == $n_initials{$measure});
			my @vals = split(/,/,$initials->{0}->{$measure});
			for (my $i=1; $i<=$n_initials{$measure};$i++){
				push(@reference_headers, $list[$i-1] );
				push(@reference_values,$vals[$i-1]);
			}
		}
	}
	

	$reference_section{'labels'}=[[],\@reference_headers];
	$reference_section{'values'}=[\@reference_values];
	push( @{$self -> results->[0]{'own'}},\%reference_section );
	
	### Loop over the original model (the zero) and over all alternative
	### models.

	my $end_index = ( $self -> estimate_simulation)? $n_alternatives: $n_alternatives - 1;
	
	my %label_indices;
	my $skipnum;
	for my $model_index ( 0..$end_index ){

		### Foreach such model, create a %results_section
		my @runs_kept= (1) x $samples;
		
		my %results_section;
		
		### Each results_section contains statistics of ofv and
		### parameters.

		my @values;
		my @labels;
		
		@{$labels[0]} = ('mean','median','sd','max','min','skewness',
						 'kurtosis','rmse','relative_rmse','bias','relative_bias','relative_absolute_bias','rse');
		$skipnum=scalar(@{$labels[0]});
		for (my $i=0; $i< $skipnum; $i++){
			$label_indices{$labels[0]->[$i]}=$i;
		}

		my @ci_list=(0.5,2.5,5,95,97.5,99.5);
		my @z_list=(-2.58,-1.96,-1.645,1.645,1.96,2.58);
		push(@{$labels[0]},('standard error CI'));
		foreach my $ci (@ci_list){
			push(@{$labels[0]},"$ci\%");
		}

		if( $model_index == 0 and $self -> estimate_simulation){
			$results_section{'name'} = "\nSimulation model";
		} else {
			my $alt_index;
			if( $self -> estimate_simulation ){
				$alt_index = $model_index + ($self->first_alternative-1); 
			} else {
				$alt_index = $model_index + 1 +($self->first_alternative-1);
			}
			$results_section{'name'} = "\nAlternative model ".$alt_index;
		}
		
		$results_section{'labels'} = \@labels;
		$results_section{'values'} = \@values;
		
		push( @{$self -> results->[0]{'own'}},\%results_section );
		
	  MEASURE: foreach my $measure ( 'ofv','theta','omega','sigma' ){

		  # Create a header, we might use max_hash from modelfit for this
		  # later on.
		  
		  my $start=0;
		  my $length=0;
		  
		  for( my $sample_index=0; $sample_index<$samples; $sample_index++ ){
			  my ($s, $l) = split(/,/, $self->raw_line_structure 
								  -> {1+$model_index*$samples + $sample_index} -> {$measure});
			  
			  my $skip = 0;
			  if( defined $s and $start != 0 and $s != $start ){
				  croak("Internal structure corrupted at $model_index, ".
						"$samples, $_, $measure, this is a bug" );
			  } elsif (not defined $s){
#				  print "error undefined $measure pos in raw_line_structure\n";
				  $skip=1;
			  } else {
				  $start = $s;
			  }
			  
			  if( defined $l and $l > $length ){
				  $length = $l;
			  }
			  if ($measure eq 'ofv'){
				  #do the filtering here
				  #skip can already be 1 if run crashed
				  for (my $i=0; $i< scalar(@filter_column);$i++){
					  last if $skip;
					  #find the index
					  my ($filter_index, $dirt) = split(/,/, $self->raw_line_structure 
														-> {1+$model_index*$samples + $sample_index} 
														-> {$filter_column[$i]});
					  my $val = undef;
					  if (defined $self -> raw_results 
						  -> [$model_index*$samples + $sample_index][$filter_index]){
						  $val = $self -> raw_results-> [$model_index*$samples + $sample_index][$filter_index];
					  }else{
						  $val='NA';
					  }

					  if ($val eq 'NA'){
						  $skip=1;
						  last;
					  }else{
						  my $string=$val.$filter_relation[$i].$filter_value[$i];
						  unless (eval($string)){
							  $skip=1;
							  last;
						  }else{
						  }
					  }
				  }
				  if ($skip){
					  $runs_kept[$sample_index]=0;
				  }elsif ($start eq '' or $start == 0) {
					  # Either ofv does not exist (unlikely) or the model has not terminated.
					  $runs_kept[$sample_index]=0;
				  }else{
					  if ((not defined $self -> raw_results
						   -> [$model_index*$samples + $sample_index][$start])
						  or $self -> raw_results-> [$model_index*$samples + $sample_index][$start] == 0) {
						  $runs_kept[$sample_index]=0;
					  }
				  }
			  }
		  }
		  
		  if( $measure eq 'ofv' ){
			  push( @{$labels[1]}, $ofvname );
			  my $finished=0;
			  foreach my $r (@runs_kept){
				  $finished += $r;
			  }
			  $results_section{'name'} = $results_section{'name'}."\nSamples completed: $finished";
			  if ($finished == 0){
				  #no values will be printed, add an undef to get the section
				  push( @{$values[0]}, undef );
			  }
		  } else {
			  for( 1..$length ){
				  push( @{$labels[1]}, uc(substr($measure, 0,2))."_$_" );
			  }
		  }

		  if (defined $form_initials -> {$measure}){
			  if (scalar(@{$form_initials -> {$measure}}) != $length){
				  print "In estimated model ".($model_index+1).
					  ", for $measure the number of final estimates is $length but the number of simulation initial values is ".
					  scalar(@{$form_initials -> {$measure}})."\n";
			  }
		  }
		  foreach my $col ( $start..($start + $length-1) ){
			  my ($skew, $kurt, $mean, $stdev,$warn) 
				  = $self -> skewness_and_kurtosis( use_runs => \@runs_kept,
													column_index => $col,
													start_row_index => $model_index*$samples, 
													end_row_index => $model_index*$samples+$samples-1 );
			  if ($warn){
				  print "Based on the ofv value, sample $warn for ".$results_section{'name'}.
					  ", is completed, but an undefined value was found for $measure for sample $warn\n";
			  }

			  my ($maximum,$minimum) 
				  = $self -> max_and_min( use_runs => \@runs_kept,
										  column_index => $col,
										  start_row_index => $model_index*$samples, 
										  end_row_index => $model_index*$samples+$samples-1 );
			  
			  push( @{$values[0]}, $mean );
			  push( @{$values[2]}, $stdev );
			  push( @{$values[3]}, $maximum );
			  push( @{$values[4]}, $minimum );
			  push( @{$values[5]}, $skew );
			  push( @{$values[6]}, $kurt );

			  my $median = $self -> median( use_runs => \@runs_kept,
											column_index => $col,
											start_row_index => $model_index*$samples, 
											end_row_index => $model_index*$samples+$samples-1 );

			  push( @{$values[1]}, $median );

			  if( 1){
				  
				  my $abs_rmse = ' ';
				  my $relative_rmse_percent = ' ';
				  my $relative_bias_percent = ' ';
				  my $absolute_bias = ' ';
				  my $relative_absolute_bias_percent = ' ';
				  my $rse = ' ';
				  
				  if( defined $form_initials -> {$measure} ){
					  #not for ofv, only for params
					  my $init =  $form_initials -> {$measure}->[$col-$start]; #ref of array	  
					  ($abs_rmse,$relative_rmse_percent) = $self -> compute_rmse( use_runs => \@runs_kept,
																				  column_index => $col,
																				  start_row_index => $model_index*$samples, 
																				  end_row_index => $model_index*$samples+$samples-1,
																				  initial_values => $init );
					  
					  ($absolute_bias,$relative_absolute_bias_percent,$relative_bias_percent) = 
						  $self -> compute_bias( use_runs => \@runs_kept,
												 column_index => $col,
												 start_row_index => $model_index*$samples, 
												 end_row_index => $model_index*$samples+$samples-1,
												 initial_values => $init );

					  unless ((defined $self->rawres_input() and $self->random_estimation_inits)or $self->have_nwpri() or $self->have_tnpri()){
						  #not sim with uncertainty, all inits are the same
						  $rse = ($init->[0] != 0)? 100*$stdev/(($init->[0])*sqrt($samples)) : 'NA';
					  }
				  }

				  push( @{$values[$label_indices{'rmse'}]}, $abs_rmse );	  
				  push( @{$values[$label_indices{'relative_rmse'}]}, $relative_rmse_percent );	  
				  push( @{$values[$label_indices{'bias'}]}, $absolute_bias );
				  push( @{$values[$label_indices{'relative_bias'}]}, $relative_bias_percent );
				  push( @{$values[$label_indices{'relative_absolute_bias'}]}, $relative_absolute_bias_percent );
				  push( @{$values[$label_indices{'rse'}]}, $rse );

				  my $label_index=$skipnum+1;
				  foreach my $zval (@z_list){
					  if( $measure ne 'ofv' ){
						  push( @{$values[$label_index]},($relative_bias_percent+$zval*$rse));
					  } else {
						  push( @{$values[$label_index]}, ' ' );
					  }
					  $label_index++;
				  }
				  
			  } else {
				  #push placeholders so that values will stay in correct column
				  push( @{$values[$label_indices{'rmse'}]}, ' ' );	  
				  push( @{$values[$label_indices{'relative_rmse'}]}, ' ' );	  
				  push( @{$values[$label_indices{'bias'}]}, ' ' );
				  push( @{$values[$label_indices{'relative_bias'}]}, ' ' );
				  push( @{$values[$label_indices{'relative_absolute_bias'}]}, ' ' );
				  push( @{$values[$label_indices{'rse'}]}, ' ' );

				  my $label_index=$skipnum+1; 
				  foreach my $zval (@z_list){
					  push( @{$values[$label_index]}, ' ' );
					  $label_index++;
				  }
			  }
		  }
	  }
	} #end loop model_index 
	
	## Calculate OFV mean calculations
	#if at least two estimated models or ref_ofv given
	if( $end_index > 0 or defined $self -> ref_ofv ){
		
		my @ofv_limits = (0,3.84,5.99,7.81,9.49);
		
		my (%results_section_below, %results_section_above);
		my (@labels_below, @labels_above, @values_below, @values_above);
		
		@{$labels_above[1]} = ('mean delta_'.$ofvname);
		@{$labels_below[1]} = ('mean delta_'.$ofvname);
		
		foreach my $limit( @ofv_limits ){
			push( @{$labels_above[1]}, "$limit" );
			push( @{$labels_below[1]}, "$limit" );
		}
		
		$results_section_above{'name'} = "\n$ofvname Statistics\nType I error rate\n,,Percent delta_$ofvname > N";
		$results_section_above{'labels'} = \@labels_above;
		$results_section_above{'values'} = \@values_above;
		
		$results_section_below{'name'} = "\n1 - type II error rate (power)\n,,Percent delta_$ofvname < N";
		$results_section_below{'labels'} = \@labels_below;
		$results_section_below{'values'} = \@values_below;
		
		push( @{$self -> results->[0]{'own'}},\%results_section_above );
		push( @{$self -> results->[0]{'own'}},\%results_section_below );
		
		my $reference_string;
		my $alt_index;
		#if add_models then start at later alt in
		if ( defined $self -> ref_ofv ){
			$reference_string="reference $ofvname -";
			$alt_index = $self->first_alternative;
		} elsif( $self -> estimate_simulation ){
			$reference_string= 'simulation -';
			$alt_index=$self->first_alternative;
		} else {
			$reference_string= 'alternative'.$self->first_alternative.' -';
			$alt_index = $self->first_alternative+1;
		}

		# $end_index is already set.
		my $start_index = (defined $self -> ref_ofv)? 0 : 1;

	  ALTMODEL: foreach my $model_index( $start_index..$end_index ){

		  push( @{$labels_above[0]}, "$reference_string alternative".($alt_index));
		  push( @{$labels_below[0]}, "$reference_string alternative".($alt_index));
		  $alt_index++;

		  my $start=0;
		  my $delta_sum=0;      
		  my %nr_below;
		  my %nr_above;
		  my $nr_terminated=0;
		  my $org_ofv;
		  
		  foreach my $sample( 0..($samples-1) ){
			  
			  my ($s, $l) = split(/,/, $self->raw_line_structure -> {1+$model_index*$samples + $sample} -> {'ofv'});
			  if( $s ne '' and $start != 0 and $s != $start ){
				  croak("$ofvname: Internal structure corrupted at ".
						"$model_index, $samples, $sample, this is a bug" );
			  } else {
				  $start = $s;
			  }
			  if($start eq '' or $start==0 ){
				  # This model probably did not terminate. 
				  next; #sample
			  }
			  
			  if ( defined $self -> ref_ofv ){
				  $org_ofv = $self -> ref_ofv;
			  } else {
				  #check reference model terminated
				  my ($s, $l) = split(/,/, $self->raw_line_structure -> {1+ $sample} -> {'ofv'});
				  if( $s ne '' and ($s != 0)){
					  $org_ofv = $self -> raw_results -> [$sample][$start];
					  next if ($org_ofv == 0);
				  }else {
					  next; #sample
				  }
			  }

			  my $alt_ofv = $self -> raw_results -> [$model_index*$samples + $sample][$start];
			  next if ($alt_ofv == 0);
			  $nr_terminated++;
			  my $delta = $org_ofv - $alt_ofv;
			  $delta_sum += $delta;
			  
			  foreach my $limit ( @ofv_limits ){
				  
				  if( $delta < -$limit ){
					  $nr_below{$limit}++;
				  }
				  
				  if( $delta > $limit ) {
					  $nr_above{$limit}++;
				  }
				  
			  }
			  
		  } #end loop over samples

		  if ($nr_terminated > 0){
			  push( @{$values_below[$model_index-$start_index]}, ($delta_sum / $nr_terminated) );
			  foreach my $limit( @ofv_limits ){
				  push( @{$values_below[$model_index-$start_index]}, ($nr_below{$limit} / $nr_terminated)*100 . "%" );
			  }
			  
			  push( @{$values_above[$model_index-$start_index]}, ($delta_sum / $nr_terminated) );
			  foreach my $limit( @ofv_limits ){
				  push( @{$values_above[$model_index-$start_index]}, ($nr_above{$limit} / $nr_terminated)*100 . "%" );
			  }
		  } else {
			  push( @{$values_below[$model_index-$start_index]},"For no sample are both models completed" );
			  push( @{$values_above[$model_index-$start_index]},"For no sample are both models completed");
		  }
	  } #end second loop model_index 
	}
}

sub format_initials
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  initials_object => { isa => 'Ref', optional => 1 },
							  initial_value => { isa => 'Num', optional => 1 }
		);
	my $initials_object = $parm{'initials_object'};
	my $initial_value = $parm{'initial_value'};
	my %initials_hash;

	#in is initials_object reference of config-tiny object
	#out is initials_hash (reference of) hash  with keys theta omega sigma, of arrays over parameter indices of arrays over samples

	my %n_initials;

	my $n_samples=scalar(keys %{$initials_object});
	$initials_hash{'theta'} = [];
	$initials_hash{'omega'} = [];
	$initials_hash{'sigma'} = [];

	foreach my $measure ('theta','omega','sigma'){
		my @arr = split(/,/,$initials_object -> {0} -> {$measure});
		$n_initials{$measure}=scalar(@arr);
		for (my $i=0; $i< $n_initials{$measure}; $i++){
			push(@{$initials_hash{$measure}},[]);
		}
	}

	for (my $sample=0; $sample< $n_samples; $sample++){
		foreach my $measure ('theta','omega','sigma'){
			my @arr = split(/,/,$initials_object -> {$sample} -> {$measure});
			for (my $i=0; $i< $n_initials{$measure}; $i++){
				push(@{$initials_hash{$measure}->[$i]},$arr[$i]);
			}
		}
	}

	return \%initials_hash;
}

sub compute_rmse
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  use_runs => { isa => 'ArrayRef[Bool]', optional => 0 },
							  column_index => { isa => 'Int', optional => 0 },
							  start_row_index => { isa => 'Int', default => 0, optional => 1 },
							  end_row_index => { isa => 'Int', optional => 1 },
							  initial_values => { isa => 'ArrayRef', optional => 0 }
		);
	my @use_runs = defined $parm{'use_runs'} ? @{$parm{'use_runs'}} : ();
	my $column_index = $parm{'column_index'};
	my $start_row_index = $parm{'start_row_index'};
	my $end_row_index = $parm{'end_row_index'};
	my @initial_values = defined $parm{'initial_values'} ? @{$parm{'initial_values'}} : ();
	my $rmse;
	my $relative_rmse_percent;

	#input is integers $column_index, $start_row_index, $end_row_index and ref of array floats @initial_values
	#output is scalar $rmse_percent

	unless( $end_row_index ){
		$end_row_index = $#{$self -> raw_results};
	}

	croak("Bad row index input") if ($start_row_index > $end_row_index);
	my $nrows = $end_row_index - $start_row_index +1;
	if ($nrows < scalar(@initial_values)){
		print "Warning: more reference values than samples\n";
	}
	if ($nrows > scalar(@initial_values)){
		croak("the number of samples is larger than the number of set of simulation initial values\n");
	}
	my $row_count_relative = 0;
	my $row_count_abs = 0;
	my $sum_squared_errors=0;
	my $sum_squared_relative_errors=0;
	for (my $i=$start_row_index; $i<=$end_row_index; $i++){
		if ($use_runs[$i-$start_row_index]){
			if (defined $self->raw_results->[$i][$column_index]){
				$sum_squared_errors += ($self->raw_results->[$i][$column_index] - $initial_values[$i-$start_row_index])**2;
				$row_count_abs++;
				if ($initial_values[$i-$start_row_index] != 0){
					$row_count_relative++;
					$sum_squared_relative_errors += 
						(($self->raw_results->[$i][$column_index] - $initial_values[$i-$start_row_index])/$initial_values[$i-$start_row_index])**2;
				}
			}
		}
	}

	#$rmse abs
	if ($row_count_abs == 0){
		$rmse='NA';
	}else{
		$rmse = sqrt($sum_squared_errors/$row_count_abs);
	}
	#$relative_rmse
	if ($row_count_relative == 0){
		$relative_rmse_percent='NA';
	}else{
		$relative_rmse_percent= 100*sqrt($sum_squared_relative_errors/$row_count_relative);
	}

	return $rmse ,$relative_rmse_percent;
}

sub compute_bias
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  use_runs => { isa => 'ArrayRef[Bool]', optional => 0 },
							  column_index => { isa => 'Int', optional => 0 },
							  start_row_index => { isa => 'Int', default => 0, optional => 1 },
							  end_row_index => { isa => 'Int', optional => 1 },
							  initial_values => { isa => 'ArrayRef', optional => 0 }
		);
	my @use_runs = defined $parm{'use_runs'} ? @{$parm{'use_runs'}} : ();
	my $column_index = $parm{'column_index'};
	my $start_row_index = $parm{'start_row_index'};
	my $end_row_index = $parm{'end_row_index'};
	my @initial_values = defined $parm{'initial_values'} ? @{$parm{'initial_values'}} : ();
	my $absolute_bias;
	my $relative_absolute_bias_percent;
	my $relative_bias_percent;

	#input is integers $column_index, $start_row_index, $end_row_index and ref of array floata $initial_values
	#output is scalar $relative_bias_percent

	unless( $end_row_index ){
		$end_row_index = $#{$self -> raw_results};
	}
	
	croak("Bad row index input") if ($start_row_index > $end_row_index);
	my $nrows = $end_row_index - $start_row_index +1;
	if ($nrows < scalar(@initial_values)){
		print "Warning: more reference values than samples\n";
	}
	if ($nrows > scalar(@initial_values)){
		croak("the number of samples is larger than the number of set of simulation initial values\n");
	}
	
	my $row_count_abs = 0;
	my $row_count_relative = 0;
	my $sum_errors=0;
	my $sum_relative_errors=0;
	my $sum_relative_absolute_errors=0;
	for (my $i=$start_row_index; $i<=$end_row_index; $i++){
		if ($use_runs[$i-$start_row_index]){
			if (defined $self->raw_results->[$i][$column_index]){
				$sum_errors += ($self->raw_results->[$i][$column_index] - $initial_values[$i-$start_row_index]);
				$row_count_abs++;
				if ($initial_values[$i-$start_row_index] != 0){
					$row_count_relative++;
					$sum_relative_errors += 
						($self->raw_results->[$i][$column_index] - $initial_values[$i-$start_row_index])/($initial_values[$i-$start_row_index]);
					$sum_relative_absolute_errors += 
						($self->raw_results->[$i][$column_index] - $initial_values[$i-$start_row_index])/abs($initial_values[$i-$start_row_index]);
					#possibly change to abs in numerator for relative_absolute
				}
			}
		}
	}

	if ($row_count_abs == 0){
		$absolute_bias='NA';
	}else{
		$absolute_bias=$sum_errors/$row_count_abs;
	}
	if ($row_count_relative == 0){
		$relative_bias_percent ='NA';
		$relative_absolute_bias_percent='NA';
	}else{
		$relative_bias_percent= ($sum_relative_errors/$row_count_relative)*100;
		$relative_absolute_bias_percent= ($sum_relative_absolute_errors/$row_count_relative)*100;
	}

	return $absolute_bias ,$relative_absolute_bias_percent ,$relative_bias_percent;
}

sub median
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
	my $median;

	#input is integers $column_index, $start_row_index, $end_row_index 

	unless( $end_row_index ){
		$end_row_index = $#{$self -> raw_results};
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

	return $median;
}

sub skewness_and_kurtosis
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
	my $skewness;
	my $kurtosis;
	my $mean;
	my $stdev;
	my $warn = 0;

	#input is integers $column_index, $start_row_index, $end_row_index 
	
	unless( $end_row_index ){
		$end_row_index = $#{$self -> raw_results};
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
		if ($row_count > 2){
			$skewness = ($sum_errors_pow3*$row_count)/(($row_count-1)*($row_count-2)*($stdev**3));
		}
		if ($row_count > 3){
			$kurtosis = -3*(($row_count-1)**2)/(($row_count-2)*($row_count-3)) + 
				($sum_errors_pow4*$row_count*($row_count+1))/(($row_count-1)*($row_count-2)*($row_count-3)*($stdev**4));
		}
	}

	return $skewness ,$kurtosis ,$mean ,$stdev ,$warn;
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
		$end_row_index = $#{$self -> raw_results};
	}

	croak("Bad row index input") if ($start_row_index >= $end_row_index);

	$maximum = -1000000000;
	$minimum =  1000000000;
	for (my $i=$start_row_index; $i<=$end_row_index; $i++){
		if ($use_runs[$i-$start_row_index]){
			if (defined $self->raw_results->[$i][$column_index]){
				$maximum = $self->raw_results->[$i][$column_index] if
					($self->raw_results->[$i][$column_index] > $maximum); 
				$minimum = $self->raw_results->[$i][$column_index] if
					($self->raw_results->[$i][$column_index] < $minimum); 
			}else{
			}
		}
	}

	return $maximum ,$minimum;
}

sub cleanup
{
	my $self = shift;

	#remove tablefiles in simulation NM_runs, they are 
	#copied to m1 by modelfit and read from there anyway.
	for (my $samp=1;$samp<=$self->samples(); $samp++) {
		unlink $self -> directory."/simulation_dir1/NM_run".$samp."/mc-sim-".$samp.".dat";
		unlink $self -> directory."/simulation_dir1/NM_run".$samp."/mc-sim-".$samp."-1.dat"; #retry
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
