package tool::llp;

use include_modules;
use File::Copy 'cp';
use ext::Math::SigFigs;
use tool::modelfit;
use Data::Dumper;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'max_iterations' => ( is => 'rw', isa => 'Int', default => 10 );
has 'significant_digits' => ( is => 'rw', isa => 'Int', default => 3 );
has 'normq' => ( is => 'rw', isa => 'Num', default => 1.96 );
has 'ofv_increase' => ( is => 'rw', isa => 'Num', default => 3.84 );
has 'theta_log' => ( is => 'rw', isa => 'HashRef' );
has 'run_thetas' => ( is => 'rw', isa => 'ArrayRef' );
has 'run_omegas' => ( is => 'rw', isa => 'ArrayRef' );
has 'run_sigmas' => ( is => 'rw', isa => 'ArrayRef' );
has 'omega_log' => ( is => 'rw', isa => 'HashRef' );
has 'sigma_log' => ( is => 'rw', isa => 'HashRef' );
has 'orig_ofvs' => ( is => 'rw', isa => 'ArrayRef' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['llplog.csv'] } );
has 'no_header_logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['llplog_no_header.csv'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'llp_results.csv' );
has 'rse_thetas' => ( is => 'rw', isa => 'ArrayRef' );
has 'rse_omegas' => ( is => 'rw', isa => 'ArrayRef' );
has 'rse_sigmas' => ( is => 'rw', isa => 'ArrayRef' );
has 'iteration' => ( is => 'rw', isa => 'Int', default => 1 );
has 'theta_interval_ratio_check' => ( is => 'rw', isa => 'Num', default => 1.3 );
has 'omega_interval_ratio_check' => ( is => 'rw', isa => 'Num', default => 1.6 );
has 'sigma_interval_ratio_check' => ( is => 'rw', isa => 'Num', default => 1.6 );
has 'within_interval_check' => ( is => 'rw', isa => 'Int', default => 0 );
has 'unstacked_logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['unstacked_llplog.csv'] } );


sub BUILD
{
	my $self  = shift;

	my @various_input_formats = ( 'run_thetas', 'run_omegas', 'run_sigmas',
								  'rse_thetas', 'rse_omegas', 'rse_sigmas' );
	foreach my $var ( @various_input_formats ) {
		if ( defined $self -> $var ) {
			if ( ref( $self -> $var ) eq 'ARRAY' and ref( $self -> $var -> [0] ) eq 'ARRAY' ) {
				if ( scalar @{$self -> $var} != scalar @{$self -> models} ) {
					croak('If you define the thetas per model, the first '.
						  'dimension of $var must match the number of models' );
				}
			}
		}
	}

	#skipped numbering when more than 1 model
	for my $accessor ( 'logfile', 'unstacked_logfile', 'raw_results_file','no_header_logfile',
					   'raw_nonp_file'){
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

}

sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 }
		);
	my $model_number = $parm{'model_number'};

	my $model = $self -> models -> [$model_number-1];

	my $mfit_threads = ref( $self -> threads ) eq 'ARRAY' ? 
		$self -> threads -> [1]:$self -> threads;
	my $own_threads = ref( $self -> threads ) eq 'ARRAY' ?
		$self -> threads -> [0]:$self -> threads;

	# Check which models that hasn't been run and run them
	# This will be performed each step but will only result in running
	# models at the first step, if at all.

	# If more than one process is used, there is a VERY high risk of interaction
	# between the processes when creating directories for model fits. Therefore
	# thedirectory attribute is given explicitly below.

	unless ( $model -> is_run ) {

		# -----------------------  Run original run  ------------------------------

		# {{{ orig run

		my %subargs = ();
		if ( defined $self -> subtool_arguments ) {
			%subargs = %{$self -> subtool_arguments};
		}
		my $orig_fit = tool::modelfit ->
			new( %{common_options::restore_options(@common_options::tool_options)},
				 models                => [$model],
				 threads               => $mfit_threads,
				 base_directory        => $self ->directory,
				 directory             => $self ->directory.'/orig_modelfit_dir'.$model_number,
				 subtools              => [],
				 top_tool              => 0,
				 parent_threads        => $own_threads,
				 parent_tool_id        => $self -> tool_id,
				 logfile	         => undef,
				 raw_results           => undef,
				 prepared_models       => undef,
				 %subargs );
		ui -> print( category => 'llp',
					 message  => "Evaluating basic model" ) unless $self -> parent_threads > 1;
		$orig_fit -> run;

		# }}} orig run

	}

	my $first = 0;
	# Prepare the step
	unless ( defined $self -> theta_log or
			 defined $self -> omega_log or
			 defined $self -> sigma_log ) {
		# First step, register the estimates from the original runs
		carp("First llp step, register estimates from original run" );
		$self -> _register_estimates( first        => 1,
									  model_number => $model_number);
		$first = 1;
	}

	# If we are resuming, there should exist a 'done' file
	my $done = 0;
	if ( -e $self ->directory.'/m'.$model_number."/done" ) {
		# Recreate the datasets and models from a checkpoint
		$done = 1;
	}

	# Make new guesses for the parameter values
	$self -> _make_new_guess( first        => $first,
							  model_number => $model_number,
							  done         => $done );

	# Construct new models for this step from the observed OFV's from the
	# previous step
	# They should be placed in m1, m2 and so on for each model.
	$self -> prepared_models( $self -> _create_models( model_number => $model_number,
													   done         => $done ));

	# ---------------------  Create the modelfit  -------------------------------

	# {{{ modelfit

	if( defined $self -> prepared_models and scalar @{$self -> prepared_models} > 0 ) {
		# Create a modelfit tool for all the models of this step.
		# This is the last setup part before running the step.
		my %subargs = ();
		if ( defined $self -> subtool_arguments ) {
			%subargs = %{$self -> subtool_arguments};
		}
		$self->tools([]) unless (defined $self->tools);
		push( @{$self -> tools},
			  tool::modelfit ->
			  new( %{common_options::restore_options(@common_options::tool_options)},
				   models                => $self -> prepared_models,
				   threads               => $mfit_threads,
				   base_directory        => $self ->directory,
				   directory             => $self ->directory.'/modelfit_dir'.$model_number,
				   _raw_results_callback => $self -> _modelfit_raw_results_callback,
				   subtools              => [],
				   parent_threads        => $own_threads,
				   parent_tool_id        => $self -> tool_id,
				   raw_results_file      => [$self -> raw_results_file->[$model_number-1]],
				   logfile	         => undef,
				   raw_results           => undef,
				   prepared_models       => undef,
				   raw_results_header    => undef,
				   tools                 => undef,
				   top_tool							 => 0,
				   %subargs ) );
	}

	# }}} modelfit
}

sub confidence_limits
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  width => { isa => 'Num', optional => 1 },
							  parameter => { isa => 'Str', optional => 1 }
		);
	my $width = $parm{'width'};
	my $parameter = $parm{'parameter'};
	my @limits;

	if ( defined $self -> results ){
		for ( my $i = 0; $i < scalar @{$self -> results -> {'own'}}; $i++ ) {
			my %num_lim;
			if ( defined $self->results -> {'own'}->[$i]->{$parameter.'_log'} ) {
				my @nums = sort {$a <=> $b} keys %{$self->results -> {'own'} ->
													   [$i]->{$parameter.'_log'}};
				foreach my $num ( @nums ) {
					my @prob_lim = ();
					for ( my $j = 0; $j < scalar @{$self->results -> {'own'}->
													   [$i]->{$parameter.'_log'}->{$num}}; $j++ ) {
						my @last_lvl = @{$self->results -> {'own'}->
											 [$i]->{$parameter.'_log'}->{$num}->[$j]};
						push( @prob_lim, [$last_lvl[0][0],$last_lvl[0][scalar @{$last_lvl[0]}-1]] );
					}
					$num_lim{$num} = \@prob_lim;
				}
			}
			push( @limits, \%num_lim );
		}
	}

	return \@limits;
}

sub _register_estimates
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 },
							  first => { isa => 'Bool', default => 0, optional => 1 }
		);
	my $model_number = $parm{'model_number'};
	my $first = $parm{'first'};
	my $active = 0;

	# Förenkla loggen: param_log{parameternr}[prob][[estimates...][ofv...]]
	# Antag att man spec. paramnr som gäller ? alla modeller och problem,
	# dvs att run_param = [2,4,3] tex.
	my %run;
	$run{'thetas'} = (ref( $self -> run_thetas -> [0] ) eq 'ARRAY') ? 
		$self -> run_thetas -> [$model_number-1]:$self -> run_thetas;
	$run{'omegas'} = (ref( $self -> run_omegas -> [0] ) eq 'ARRAY') ? 
		$self -> run_omegas -> [$model_number-1]:$self -> run_omegas;
	$run{'sigmas'} = (ref( $self -> run_sigmas -> [0] ) eq 'ARRAY') ? 
		$self -> run_sigmas -> [$model_number-1]:$self -> run_sigmas;
	
	my %models;
	my @mod_variants;

	my @prep_models;
	my ( $orig_output, $orig_ofvs );
	my $ui_text;

	# Global activity flag for this model:
	$active = 0;

	my $orig_model = $self -> models -> [$model_number-1];	

	my $mode = $first ? '>' : '>>';
	open( LOG, $mode.$self -> logfile->[$model_number-1] );
	open( LOG2, $mode.$self -> unstacked_logfile->[$model_number-1] );
	print LOG2 sprintf("%10s",'Parameter'),',',sprintf("%10s",'Side'),
	',',sprintf("%10s",'Value'),',',sprintf("%10s",'OFV.diff'),"\n" if( $first );

	if ( $first ) {
		croak("No output object defined through model" )
			unless ( defined $orig_model -> outputs -> [0] );
		$orig_output = $orig_model -> outputs -> [0];
		$orig_ofvs   = $orig_output -> ofv;
		# Save the ofvs in orig_ofvs
		$self -> orig_ofvs($orig_ofvs);

		# Print a log-header
		foreach my $param ( 'theta', 'omega', 'sigma' ) {
			next unless ( defined $run{$param.'s'} and
						  scalar @{$run{$param.'s'}} > 0 and
						  $run{$param.'s'}->[0] ne '' );
			my @par_nums    = @{$run{$param.'s'}};
			my $orig_ests    = $orig_model -> get_values_to_labels ( category => $param);

			# Loop the parameter numbers
			foreach my $num ( @par_nums ) {
				# Loop the problems
				for ( my $j = 0; $j < scalar @{$orig_ests}; $j++ ) {
					my $label = uc($param).$num."_".($j+1);
					$ui_text = $ui_text.sprintf("%10s",$label).','.sprintf("%10s",'OFV_DIFF').',';
					print LOG sprintf("%10s",$label),',',sprintf("%10s",'OFV_DIFF'),',';
				}
			}
		}
		ui -> print( category => 'llp',
					 message  => $ui_text,
					 wrap     => 0 );
		print LOG "\n";

		# It is much more time efficient to store all bounds, estimates etc in
		# an array and access that when doing the registration than calling the
		# tools within the loop:
		# Loop over the parameter names
		$ui_text = '';
		foreach my $param ( 'theta', 'omega', 'sigma' ) {
			# jump to next parameter if no parameter of this type should be run
			next unless ( defined $run{$param.'s'} and
						  scalar @{$run{$param.'s'}} > 0 and
						  $run{$param.'s'}->[0] ne '' );
			my @par_nums    = @{$run{$param.'s'}};
			my $orig_ests    = $orig_model -> get_values_to_labels ( category => $param);
			my $orig_se_ests    = $orig_model -> get_values_to_labels ( category => 'se'.$param);
			# Loop over the parameter numbers of interest
			my %all_num_est = ();
			foreach my $num ( @par_nums ) {
				my @all_prob_est = ();
				# Loop over the problems:
				for ( my $j = 0; $j < scalar @{$orig_ests}; $j++ ) {
					croak("Subproblems are not allowed for the log-likelihood profiling tool" )
						if ( scalar @{$orig_ests->[$j]} > 1 );
					# For the original estimates, we need to use the estimates from the original output file.
					my $orig  = $orig_ests->[$j][0][$num-1];
					# Store original estimates. The [0] is there for print_results.
					#push(@{$self -> original_estimates->[$model_number-1][$j][0]}, $orig);
					# all_prob_est: [fixed estimates][ofv_diffs][finished_flag(lower,upper)]
					my %finished = ( 'lower' => 0, 'upper' => 0 );
					push( @all_prob_est, [[$orig],[0],\%finished]);
					# If we end up here, the the llp of this model is still active
					$active = 1; 
					$ui_text = $ui_text.sprintf("%10f",$orig).','.sprintf("%10f",0).',';
					print LOG sprintf("%10f",$orig),',',sprintf("%10f",0),',';
					print LOG2 sprintf("%10s",$param.$num),',',sprintf("%10s",'orig'),
					',',sprintf("%10f",$orig),',',sprintf("%10f",0),"\n";
				}
				$all_num_est{$num} = \@all_prob_est;
			}
			my $tempfunc = $param.'_log';
			$self -> $tempfunc(\%all_num_est);
		}
		ui -> print( category => 'llp',
					 message  => $ui_text,
					 wrap     => 0 );
		print LOG "\n";
	} else {
		@prep_models = @{$self -> prepared_models};

		# It is much more time efficient to store all bounds, estimates etc in
		# an array and access that when doing the registration than calling the
		# tools within the loop:
		# Loop over the parameter names
		foreach my $side ( 'lower', 'upper' ) {
			# reset the user interface text
			$ui_text = '';
			foreach my $param ( 'theta', 'omega', 'sigma' ) {
				# jump to next parameter if no parameter of this type should be run
				next unless ( defined $run{$param.'s'} and
							  scalar @{$run{$param.'s'}} > 0 and
							  $run{$param.'s'}->[0] ne '' );
				my %bounds;
				$bounds{'lower'} =
					$orig_model -> lower_bounds( parameter_type => $param );
				$bounds{'upper'} =
					$orig_model -> upper_bounds( parameter_type => $param );
				
				my $accessor    = $param.'s';
				my $logfunc   = $param.'_log';
				my @par_nums    = @{$run{$param.'s'}};
				# get the stored ofvs
				$orig_ofvs = $self -> orig_ofvs;
				# Loop over the parameter numbers of interest
				foreach my $num ( @par_nums ) {
					my ( $model, $output, $ofvs, $ests );
					# Check if there exists any model for this side (if not, it is
					# probably (hopefully) finished
					if (defined $self->{$param.'_models'} -> {$num} -> {$side}) {
						$model    = $self->{$param.'_models'} -> {$num} -> {$side};
						$output   = $model  -> outputs -> [0];
						$ofvs     = $output -> ofv;
						my $accessor = $param.'s';
						#$ests     = $output -> $accessor;
						$ests    = $model -> get_values_to_labels ( category => $param);
					}
					# Loop over the problems:
					for ( my $j = 0; $j < scalar @{$orig_ofvs}; $j++ ) {
						# Is this side of the problem finished?
						if ( $self->$logfunc->{$num}->[$j]->[2]->{$side} ) {
							$ui_text = $ui_text.sprintf("%10s",$PsN::out_miss_data).','.sprintf("%10s",$PsN::out_miss_data).',';
							print LOG sprintf("%10s",$PsN::out_miss_data),',',sprintf("%10s",$PsN::out_miss_data),',';
							next;
						}
						# Collect the model outputs
						my $diff = $ofvs->[$j][0]-$orig_ofvs->[$j][0];
						my $est = $model -> initial_values( parameter_type => $param ) -> [0][$num - 1];
						$ui_text = $ui_text.sprintf("%10f",$est).','.sprintf("%10f",$diff).',';
						print LOG sprintf("%10f",$est),',',sprintf("%10f",$diff),',';
						print LOG2 sprintf("%10s",$param.$num),',',sprintf("%10s",$side),',',
						sprintf("%10f",$est),',',sprintf("%10f",$diff),"\n";
						my $bound = $bounds{$side}->[$j][$num-1];
						carp("Registering diff $diff" );
						if ( $side eq 'lower' ) {
							unshift( @{$self->$logfunc->{$num}->[$j]->[1]}, $diff );
							$est = $self->$logfunc->{$num}->[$j]->[0]->[0];
						} else {
							push( @{$self->$logfunc->{$num}->[$j]->[1]}, $diff );
							my $sofar = scalar @{$self->$logfunc->{$num}->[$j]->[0]};
							$est = $self->$logfunc->{$num}->[$j]->[0]->[$sofar-1];
						}
						my $finished = $self -> _test_sigdig( number => $diff,
															  goal   => $self -> ofv_increase,
															  sigdig => $self -> significant_digits );
						my $print_diff = &FormatSigFigs($diff, $self -> significant_digits );
						carp("New OFV diff: $print_diff" );
						carp(" equals goal ".$self->ofv_increase.
							 ". This side is finished" ) if $finished;
						if ( $finished ) {
							$self->$logfunc->{$num}->[$j]->[2]->{$side} = 1;
						} elsif ( defined $bound and
								  $self -> _test_sigdig( number => $est,
														 goal   => $bound,
														 sigdig => 2 ) ) {
							carp("Estimate $est too close to $side boundary $bound".
								 " terminating search" );
							$self->$logfunc->{$num}->[$j]->[2]->{$side} = 2;
						} else {
							$active = 1;
						}
						$model -> outputs -> [0] -> flush;
					}
				}
			}
			# Print to screen if we are in the right context
			ui -> print( category => 'llp',
						 message  => $ui_text,
						 wrap     => 0 );
			print LOG "\n";
		}
	}
	close ( LOG );
	close ( LOG2 );

	return $active;
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 }
		);
	my $model_number = $parm{'model_number'};

	my $own_threads = ref( $self -> threads ) eq 'ARRAY' ?
		$self -> threads -> [0]:$self -> threads;

	# 1. Register fixed values and ofvs
	#    and check if ofvs are close enough to target
	my $active = $self -> _register_estimates( model_number => $model_number );

	my ( $returns, $prep_models );
	# 3. Return logged values-ofv pairs to calling tool
	if ( $active ) {
		if ( $self -> max_iterations > 1 ) {
			$self -> update_raw_results( model_number => $model_number );
			# !!! The results_file attribute should not be set when calling
			# llp recursively. If set, a result file will be written for each
			# recurrence level which is redundant.
			my %run;
			$run{'thetas'} = (ref( $self -> run_thetas -> [0] ) eq 'ARRAY') ? 
				$self -> run_thetas -> [$model_number-1]:$self -> run_thetas;
			$run{'omegas'} = (ref( $self -> run_omegas -> [0] ) eq 'ARRAY') ? 
				$self -> run_omegas -> [$model_number-1]:$self -> run_omegas;
			$run{'sigmas'} = (ref( $self -> run_sigmas -> [0] ) eq 'ARRAY') ? 
				$self -> run_sigmas -> [$model_number-1]:$self -> run_sigmas;
			my %subargs = ();
			if ( defined $self -> subtool_arguments ) {
				%subargs = %{$self -> subtool_arguments};
			}
			#never to check nmtran after first iteration
			my $internal_llp =
				tool::llp ->
				new( %{common_options::restore_options(@common_options::tool_options)},
					 normq              => $self->normq,
					 ofv_increase       => $self->ofv_increase,
					 check_nmtran => 0,
					 significant_digits => $self->significant_digits,
					 theta_interval_ratio_check => $self->theta_interval_ratio_check,
					 omega_interval_ratio_check => $self->omega_interval_ratio_check,
					 sigma_interval_ratio_check => $self->sigma_interval_ratio_check,
					 theta_log              => $self->theta_log,
					 omega_log              => $self->omega_log,
					 sigma_log              => $self->sigma_log,
					 orig_ofvs              => $self->orig_ofvs,
					 no_header_logfile => $self->no_header_logfile,
					 results_file => $self->results_file,
					 within_interval_check => $self->within_interval_check,
					 raw_results_file   => [$self -> raw_results_file->[$model_number-1]],
					 base_directory     =>  $self ->directory,
					 directory	      =>  $self ->directory.'/llp_dir'.$model_number,
					 logfile	      => [$self -> logfile->[$model_number-1]],
					 unstacked_logfile  => [$self -> unstacked_logfile->[$model_number-1]],
					 max_iterations     => ($self -> max_iterations - 1),
					 iteration	      => ($self -> iteration + 1),
					 models	      => [$self -> models -> [$model_number-1]],
					 run_thetas	      => [$self -> run_thetas -> [$model_number-1]],
					 run_omegas	      => [$self -> run_omegas -> [$model_number-1]],
					 run_sigmas	      => [$self -> run_sigmas -> [$model_number-1]],
					 parent_tool_id     =>  $self -> tool_id,
					 parent_threads     =>  $own_threads,
					 raw_results        => undef,
					 prepared_models    => undef,
					 rse_thetas         => undef,
					 rse_omegas         => undef,
					 rse_sigmas         => undef,
					 raw_results_header => undef,
					 tools              => undef,
					 prepared_models    => undef,
					 results            => undef,
					 %subargs );

			( $returns, $prep_models ) = $internal_llp -> run;

			if ( defined $prep_models ) {
				carp("Inside ".ref($self).
					 " have called internal llp ".
					 scalar @{$prep_models} );
				#FIXME for Moose
				push ( @{$self -> prepared_models->[$model_number-1]{'subtools'}},
					   $prep_models );
			} else {
				carp("Inside analyze".ref($self).
					 " but no prep_models defined from internal llp" );
			}

		} else {
			$self -> raw_results->[$model_number-1] =
				$self -> tools -> [0] -> raw_results;
			$self -> update_raw_results( model_number => $model_number );
		}
	} else {
		$self -> raw_results->[$model_number-1] =
			$self -> tools -> [0] -> raw_results if( defined $self -> tools -> [0] );
	}
	if( $self -> iteration() < 2 and
	    defined $PsN::config -> {'_'} -> {'R'} and
	    -e $PsN::lib_dir . '/R-scripts/llp.R' ) {
		# copy the llp R-script
		cp ( $PsN::lib_dir . '/R-scripts/llp.R', $self ->directory );
		# Execute the script
		system( $PsN::config -> {'_'} -> {'R'}." CMD BATCH llp.R" );
	}
}

sub _create_models
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 },
							  done => { isa => 'Bool', default => 0, optional => 1 }
		);
	my $model_number = $parm{'model_number'};
	my $done = $parm{'done'};
	my @new_models;

	# --------------------  Initiate the run parameters -------------------------

	# {{{ initiate params

	my %run;
	$run{'thetas'} = (ref( $self -> run_thetas -> [0] ) eq 'ARRAY') ? 
		$self -> run_thetas -> [$model_number-1]:$self -> run_thetas;
	$run{'omegas'} = (ref( $self -> run_omegas -> [0] ) eq 'ARRAY') ? 
		$self -> run_omegas -> [$model_number-1]:$self -> run_omegas;
	$run{'sigmas'} = (ref( $self -> run_sigmas -> [0] ) eq 'ARRAY') ? 
		$self -> run_sigmas -> [$model_number-1]:$self -> run_sigmas;

	my $model = $self -> models -> [$model_number-1];

	# }}} initiate params

	# ------------------  Create the fixed parameter models ---------------------

	# {{{ create models

	# Loop over the parameters
	foreach my $param ( 'theta', 'omega', 'sigma' ) {
	    my $logfunc = $param.'_log';
		# jump to next parameter if no parameter of this type should be run
		next unless ( defined $run{$param.'s'} and
					  scalar @{$run{$param.'s'}} > 0 and
					  $run{$param.'s'}->[0] ne '' );
		my @par_nums = @{$run{$param.'s'}};
		my %par_log  = %{$self -> $logfunc};
		# Loop over the parameter numbers of interest
		foreach my $num ( @par_nums ) {
			my @active = @{$model -> active_problems};
			my $skip_model = 1;
			foreach my $val ( @active ) {
				$skip_model = 0 if ( $val );
			}
			foreach my $side ( 'lower', 'upper' ) {
				my $filename = substr($param,0,2).$num.$side.'.mod';
				my $model_dir = $self ->directory.'/m'.$model_number.'/';
				my ($output_dir, $outputfilename) =
					OSspecific::absolute_path( $self ->directory.'/m'.$model_number.'/',
											   substr($param,0,2).$num.
											   $side.'.lst');
				if ( not $done ) {

					# ----------------------  Create model  -----------------------------

					# {{{ create

					my $new_mod = $model -> copy( filename    => $filename,
												  directory   => $model_dir,
												  copy_datafile   => 0,
												  write_copy => 0,
												  copy_output => 0 );
					$new_mod -> extra_files( $model -> extra_files ),
					$new_mod -> ignore_missing_files( 1 );
					$new_mod -> outputfile( $output_dir . $outputfilename );
					$new_mod -> ignore_missing_files( 0 );
					
					$new_mod -> update_inits( from_output => $model -> outputs -> [0] );
					my $active_flag = 0;
					# Loop over the problems:
					for ( my $j = 1; $j <= scalar @{$par_log{$num}}; $j++ ) {
						# Is this side of the problem finished?
						if ( $self->$logfunc->{$num}->[$j-1]->[2]->{$side} ){
							carp("This side is finished!" );
							next;
						}
						my $sofar = scalar @{$par_log{$num}->[$j-1]->[0]};
						my $guess;
						if ( $side eq 'lower' ) {
							$guess = $par_log{$num}->[$j-1]->[0]->[0];
						} else {
							$guess = $par_log{$num}->[$j-1]->[0]->[$sofar-1];
						}
						my @diagnostics =
							@{$new_mod -> initial_values( parameter_type    => $param,
														  parameter_numbers => [[$num]],
														  problem_numbers   => [$j],
														  new_values        => [[$guess]] )};
						if ( $side eq 'lower' ) {
							$par_log{$num}->[$j-1]->[0]->[0] = $diagnostics[0][2];
						} else {
							$par_log{$num}->[$j-1]->[0]->[$sofar-1] = $diagnostics[0][2];
						}
						carp("Registering used value ".
							 $diagnostics[0][2]." for the $side side" );
						$new_mod -> fixed( parameter_type    => $param,
										   parameter_numbers => [[$num]],
										   problem_numbers   => [$j],
										   new_values        => [[1]] );
						$active_flag = 1;
					}
					if ( $active_flag ) {
						$new_mod -> _write(); #use absolute data path for original dataset when writing to m1
						push( @new_models, $new_mod );
						$self->{$param.'_models'}->{$num}->{$side} = $new_mod;
					}

					# }}} create

				} else {

					# -------------------------  Resume  --------------------------------

					# {{{ resume

					my $active_flag = 0;
					# Loop over the problems:
					for ( my $j = 1; $j <= scalar @{$par_log{$num}}; $j++ ) {
						# Is this side of the problem finished?
						if ( $self->$logfunc->{$num}->[$j-1]->[2]->{$side} ){
							carp("This side is finished!" );
							next;
						} 
						$active_flag = 1;
					}
					if ( $active_flag ) {
						my $new_mod = model -> new( directory   => $model_dir,
													filename    => $filename,
													outputfile  => $outputfilename,
													extra_files => $model -> extra_files,
													ignore_missing_files => 1 );
						push( @new_models, $new_mod );
						$self->{$param.'_models'}->{$num}->{$side} = $new_mod;
					}
					
					# }}} resume

				}
			}
		}
	}

	# }}} create models

	return \@new_models;
}

sub _modelfit_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Num', optional => 1 }
		);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	# This functions creates a subrouting which will be called by
	# the modelfit subtool. The subroutine will add columns to the
	# result rows from the modelfit. The result rows will then be
	# printed to the "raw_results" file by the modelfit.

	# First we create some variables that the subroutine needs. To
	# avoid confuision we avoid using the $self variable in the
	# subroutine, instead we create these variables which become
	# available to the subroutine.

	my $iteration = $self -> iteration;
	my %run;
	my @models = @{$self -> models};
	my $model_number = $self -> model_number;
	my ($dir,$file) = 
		OSspecific::absolute_path( $self ->directory,
								   $self -> raw_results_file->[$model_number-1] );

	$run{'thetas'} = (ref( $self -> run_thetas -> [0] ) eq 'ARRAY') ? 
	    $self -> run_thetas -> [$model_number-1]:$self -> run_thetas;
	$run{'omegas'} = (ref( $self -> run_omegas -> [0] ) eq 'ARRAY') ? 
	    $self -> run_omegas -> [$model_number-1]:$self -> run_omegas;
	$run{'sigmas'} = (ref( $self -> run_sigmas -> [0] ) eq 'ARRAY') ? 
	    $self -> run_sigmas -> [$model_number-1]:$self -> run_sigmas;
	my %log;
	foreach my $param ( 'theta', 'omega', 'sigma' ) {
	    my $logfunc = $param.'_log';
	    $log{$param} = $self -> $logfunc;
	}

	# The raw results of the original model should be put as the first row
	my $orig_mod = $self -> models->[$model_number-1];

	# And here is the subroutine which is given to the modelfit.
	my $callback = sub {
		# The modelfit object is given to us. It could have been
		# retrieved from $self but this was easier.

		my $modelfit = shift;
		my $mh_ref   = shift;
		my $raw_results_header = $modelfit -> raw_results_header;
		my $raw_results = $modelfit -> raw_results;

		$modelfit -> raw_results_append( 1 ) if( $iteration > 1 );

		if ( $iteration == 1 ) {

			my %dummy;

			my ($raw_results_rows, $nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
																				   model => $orig_mod,
																				   raw_line_structure => \%dummy );

			$orig_mod -> outputs -> [0] -> flush;

			unshift( @{$raw_results_rows -> [0]}, ( 0, undef, undef, undef, undef ));
			
			unshift( @{$raw_results}, @{$raw_results_rows} );

			# Set the header once.
			
			unshift( @{$raw_results_header} , ('iteration', 'parameter.type',
											   'parameter.number', 'side', 'finish.message' ) );

			$self->raw_line_structure($modelfit -> raw_line_structure);
			foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure})){
				foreach my $category (keys %{$self->raw_line_structure -> {$mod}}){
					next if ($category eq 'line_numbers');
					my ($start,$len) = split(',',$self->raw_line_structure -> {$mod}->{$category});
					$self->raw_line_structure -> {$mod}->{$category} = ($start+5).','.$len; 
				}
				$self->raw_line_structure -> {$mod}->{'iteration'} = '0,1';
				$self->raw_line_structure -> {$mod}->{'parameter.type'} = '1,1';
				$self->raw_line_structure -> {$mod}->{'parameter.number'} = '2,1';
				$self->raw_line_structure -> {$mod}->{'side'} = '3,1';
				$self->raw_line_structure -> {$mod}->{'finish.message'} = '4,1';
			}
			$self->raw_line_structure -> {'0'} = $self->raw_line_structure -> {'1'};
			$self->raw_line_structure -> write( $dir.'raw_results_structure' );


		}

		# {{{ New header

		# First prepend llp specific stuff.

		# It is implicitly true that the inner loop will execute once
		# for each row in the raw_results array. We keep track of its
		# the row index with $result_row.

		my $result_row = $iteration == 1 ? 1 : 0; # skip the original results row
		
		foreach my $param ( 'theta', 'omega', 'sigma' ) {
			foreach my $num ( @{$run{$param.'s'}} ) {
				foreach my $side ( 'lower', 'upper' ) {
					next unless ( defined $run{$param.'s'} and
								  scalar @{$run{$param.'s'}} > 0 );
					next if $log{$param}{$num}[0][2]{$side};
					if ( defined $raw_results -> [$result_row] ) {
						unshift( @{$raw_results -> [$result_row]}, ($iteration, $param, $num, $side, undef ) );
					}
					$result_row++;
				}
			}
		}

		# }}} New header

	};
	return $callback;
}

sub _make_new_guess
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 },
							  first => { isa => 'Bool', default => 0, optional => 1 },
							  done => { isa => 'Bool', default => 0, optional => 1 }
		);
	my $model_number = $parm{'model_number'};
	my $first = $parm{'first'};
	my $done = $parm{'done'};

	my %run;
	$run{'thetas'} = (ref( $self -> run_thetas -> [0] ) eq 'ARRAY') ? 
		$self -> run_thetas -> [$model_number-1]:$self -> run_thetas;
	$run{'omegas'} = (ref( $self -> run_omegas -> [0] ) eq 'ARRAY') ? 
		$self -> run_omegas -> [$model_number-1]:$self -> run_omegas;
	$run{'sigmas'} = (ref( $self -> run_sigmas -> [0] ) eq 'ARRAY') ? 
		$self -> run_sigmas -> [$model_number-1]:$self -> run_sigmas;

	my $orig_output;
	my $orig_model = $self -> models -> [$model_number-1];
	if ( $first ) {
		croak("No output object defined through model" )
			unless ( defined $orig_model -> outputs -> [0] );
		$orig_output = $orig_model -> outputs -> [0];
	}
	# Loop over the parameter names
	foreach my $param ( 'theta', 'omega', 'sigma' ) {
		# jump to next parameter if no parameter of this type should be run
		next unless ( defined $run{$param.'s'} and
					  scalar @{$run{$param.'s'}} > 0 and
					  $run{$param.'s'}->[0] ne '' );
		my $accessor    = $param.'s';
		my $logfunc = $param.'_log';
		my $rsefunc = 'rse_'.$param.'s';
		my @par_nums    = @{$run{$param.'s'}};
		my $diagonals = $orig_model -> on_diagonal( parameter_type => $param );
		my %bounds;
		$bounds{'lower'} =
			$orig_model -> lower_bounds( parameter_type => $param );
		if ($param eq 'omega' or $param eq 'sigma' and defined $diagonals){
			for ( my $j = 0; $j < scalar @{$bounds{'lower'}}; $j++ ) {
				next unless (defined $diagonals->[$j]);
				foreach my $num ( @par_nums ) {
					if (defined $diagonals->[$j][$num-1] and $diagonals->[$j][$num-1]== 1){
						$bounds{'lower'}->[$j][$num-1] = 0 
							unless (defined $bounds{'lower'}->[$j][$num-1]);
					}
				}
			}
		}
		$bounds{'upper'} =
			$orig_model -> upper_bounds( parameter_type => $param );
		if ( $first ) {
			my $orig_ests    = $orig_model -> get_values_to_labels ( category => $param);
			my $orig_se_ests = $orig_model -> get_values_to_labels ( category => 'se'.$param);
			# Loop over the parameter numbers of interest
			foreach my $num ( @par_nums ) {
				# Loop over the problems:
				for ( my $j = 0; $j < scalar @{$orig_ests}; $j++ ) {
					die "Subproblems are not allowed for the log-likelihood profiling tool\n"
						if ( scalar @{$orig_ests->[$j]} > 1 );
					my $orig  = $orig_ests->[$j][0][$num-1];
					my $upbnd = $bounds{'upper'}->[$j][$num-1];
					my $lobnd = $bounds{'lower'}->[$j][$num-1];
					my $width;
					if ( defined $orig_se_ests->[$j][0][$num-1] ) {
						$width = abs( $orig_se_ests->[$j][0][$num-1] *
									  $self -> normq );
					} elsif ( defined $self -> $rsefunc->[$model_number-1]{$num} ) {
						$width = abs( $self -> $rsefunc->[$model_number-1]{$num}/100*abs($orig) *
									  $self -> normq );
					} else {
						die "No estimate of the standard error of $param $num is available from the output file nor from the command line\n";
					}
					my $upper = $orig + $width;
					my $lower = $orig - $width;

					$lower = ( defined $lobnd and $lower < $lobnd  ) ?
						($lobnd-$orig)*0.9+$orig : $lower;
					$upper = ( defined $upbnd and $upper > $upbnd ) ?
						($upbnd-$orig)*0.9+$orig : $upper;
					unshift( @{$self->$logfunc->{$num}->[$j]->[0]}, $lower );
					push( @{$self->$logfunc->{$num}->[$j]->[0]}, $upper );
				}
			}
		} else {
			# Loop over the parameter numbers of interest
			foreach my $num ( @par_nums ) {
				# Loop over the problems:
				for ( my $j = 0; $j < scalar @{$bounds{'lower'}}; $j++ ) {
					my %guesses;
					foreach my $side ( 'lower', 'upper' ) {
						# Is this side of the problem finished?
						next if $self->$logfunc->{$num}->[$j]->[2]->{$side};
						# Collect the model outputs
						carp("Making new guess for $param number $num on the $side side" );
						my $bound = $bounds{$side}->[$j][$num-1];
						my $guess =
							$self -> _guess( param_log => $self->$logfunc->{$num}->[$j],
											 side => $side );
						if ( defined $bounds{$side}->[$j][$num-1] ) {
							$guess =
								$self -> _try_bounds( guess     => $guess,
													  side      => $side,
													  bound     => $bounds{$side}->[$j][$num-1],
													  param_log => $self->$logfunc->
													  {
														  $num}->[$j]->[0]);
						}
						$guesses{$side} = $guess;
						if ( not defined $guess ) {
							print "Warning: The search for the $side CI-limit for $param $num ".
								"could not continue due to numerical difficulties\n";
							$self->$logfunc->{$num}->[$j]->[2]->{$side} = 1;
						}
					}
					unshift( @{$self->$logfunc->{$num}->[$j]->[0]}, $guesses{'lower'} )
						if ( defined $guesses{'lower'} );
					push( @{$self->$logfunc->{$num}->[$j]->[0]}, $guesses{'upper'} )
						if ( defined $guesses{'upper'} );
				}
			}
		}
	}
	# Logging must be done fairly quick, therefore this loop by itself
	open( DONE, '>'.$self ->directory."/m$model_number/done" );
	foreach my $param ( 'theta', 'omega', 'sigma' ) {
		my $logfunc = $param.'_log';
		next unless ( defined $self->$logfunc );
		while ( my ( $num, $probs ) = each %{$self->$logfunc} ) {
			# Loop over the problems:
			for ( my $prob = 0; $prob < scalar @{$probs}; $prob++ ) {
				foreach my $side ( 'lower', 'upper' ) {
					next if $self->$logfunc->{$num}->[$prob]->[2]->{$side};
					my $log_size = scalar @{$probs -> [$prob] -> [0]};
					if ( $side eq 'lower' ) {
						print DONE "$param $num $prob $side ",
						$probs -> [$prob] -> [0] -> [0],"\n";
					} elsif ( $side eq 'upper' ) {
						print DONE "$param $num $prob $side ",
						$probs -> [$prob] -> [0] -> [$log_size-1],"\n";
					}
				}
			}
		}
	}
	close( DONE );
}

sub update_raw_results
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 }
		);
	my $model_number = $parm{'model_number'};

	my ($dir,$file) = OSspecific::absolute_path( $self ->directory,
												 $self -> raw_results_file->[$model_number-1] );
	open( RRES, $dir.$file );
	my @rres = <RRES>;
	close( RRES );
	open( RRES, '>',$dir.$file );
	my @new_rres;
	foreach my $row_str ( @rres ) {
		chomp( $row_str );
		my @row = split( ',', $row_str );
		if ( $row[0] eq $self -> iteration ) {
			# The [0] is the problem level, should be removed
			my $logfunc = $row[1].'_log';
			if ( $self -> $logfunc->{$row[2]}[0][2]{$row[3]} == 1 ) {
				$row[4] = 'limit.found';
			} elsif ( $self -> $logfunc->{$row[2]}[0][2]{$row[3]} == 2 ) {
				$row[4] = 'near.boundary';
			} elsif ( $self -> max_iterations <= 1 ) {
				$row[4] = 'max.iterations';
			}
		}
		push( @new_rres, \@row );
		print RRES join(',',@row ),"\n";
	}
	close( RRES );
	$self -> raw_results->[$model_number-1] = \@new_rres;
}

sub create_matlab_scripts
{
	my $self = shift;

	if( defined $PsN::lib_dir ){
		unless( -e $PsN::lib_dir . '/matlab/profiles.m') {
			croak('LLP matlab template scripts are not installed, no matlab scripts will be generated.' );
			return;
		}

		open( PROF, $PsN::lib_dir . '/matlab/profiles.m' );
		my @file = <PROF>;
		close( PROF );	
		my $found_code;
		my $code_area_start=0;
		my $code_area_end=0;

		for(my $i = 0;$i < scalar(@file); $i++) {
			if( $file[$i] =~ /% ---------- Autogenerated code below ----------/ ){
				$found_code = 1;
				$code_area_start = $i;
			}
			if( $file[$i] =~ /% ---------- End autogenerated code ----------/ ){
				unless( $found_code ){
					croak('LLP matlab template script is malformated, no matlab scripts will be generated' );
					return;
				}
				$code_area_end = $i;
			}
		}

		my @auto_code;
		push( @auto_code, "str_format = '%30s';\n\n" );

		my %param_names;

		push( @auto_code, "col_names = [ " );

		foreach my $param ( 'theta','omega','sigma' ) {
			my $labels = $self -> models -> [0] -> labels( parameter_type => $param );
			if ( defined $labels ){
				foreach my $label ( @{$labels -> [0]} ){
					push( @auto_code, "	      sprintf(str_format,'",$label,"');\n" );
				}
			}
		}
		push( @auto_code, "	      ];\n\n" );
		push( @auto_code, "goal = 3.84;\n\n" );

		push( @auto_code, "filename = '".$self -> no_header_logfile->[0]."';\n" );

		splice( @file, $code_area_start, ($code_area_end - $code_area_start), @auto_code );	
		open( OUTFILE, ">", $self ->directory . "/profiles.m" );
		print OUTFILE "addpath " . $PsN::lib_dir . ";\n";
		print OUTFILE @file ;
		close OUTFILE;

		open( LOGFILE, "<", $self -> logfile->[0] );
		my @log = <LOGFILE>;
		close LOGFILE;

		open( OUTFILE, ">", $self -> no_header_logfile->[0] );
		for( my $i = 1; $i <= $#log; $i ++ ){ #Skip header
			$log[$i]=~s/NA\,/NaN\,/g;
			print OUTFILE $log[$i];
		}
		close OUTFILE;

	} else {
		croak('matlab_dir not configured, no matlab scripts will be generated.');
		return;
	}
}

sub create_R_scripts
{
	my $self = shift;

	unless( -e $PsN::lib_dir . '/R-scripts/llp.R' ){
		croak('LLP R-script are not installed, no R scripts will be generated.' );
		return;
	}
	cp ( $PsN::lib_dir . '/R-scripts/llp.R', $self ->directory );
}

sub _try_bounds
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  param_log => { isa => 'ArrayRef[Num]', optional => 1 },
							  guess => { isa => 'Num', optional => 1 },
							  side => { isa => 'Str', optional => 1 },
							  bound => { isa => 'Num', optional => 1 }
		);
	my @param_log = defined $parm{'param_log'} ? @{$parm{'param_log'}} : ();
	my $guess = $parm{'guess'};
	my $side = $parm{'side'};
	my $bound = $parm{'bound'};
	my $new_value;

	if ( ( $side eq 'lower' and $guess < $bound ) or
	     ( $side eq 'upper' and $guess > $bound ) ) {
		my @s_log = sort { $a <=> $b } @param_log;
		if ( $side eq 'lower' and $guess < $bound ) {
			$guess = ( $bound - $s_log[0])*0.9+$s_log[0];
			carp("Corrected lower guess to $guess" );
		}
		if ( $side eq 'upper' and $guess > $bound ) {
			$guess = ( $bound - $s_log[$#s_log])*0.9+$s_log[$#s_log];
			carp("Corrected upper guess to $guess" );
		}
	}

	$new_value = $guess;

	return $new_value;
}

sub _aag
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  x => { isa => 'ArrayRef[Num]', optional => 1 },
							  y => { isa => 'ArrayRef[Num]', optional => 1 },
);
my @x = defined $parm{'x'} ? @{$parm{'x'}} : ();
my @y = defined $parm{'y'} ? @{$parm{'y'}} : ();
	my @polynomial;

	my $total = scalar(@x);
	croak("No data supplied to the polynomial approximation".
		      " algorithm in the log-likelihood profiling tool" ) if ( $total < 1 );

	my $y=0;    my $y2=0;    my $x1=0;    my $x2=0;
	my $x3=0;   my $x4=0;    my $x1y=0;   my $x2y=0;

	my $count=0;
	while ($count<$total){
	  $y+=$y[$count];
	  $y2+=($y[$count])**2;
	  $x1+=$x[$count];
	  $x2+=($x[$count])**2;
	  $x3+=($x[$count])**3;
	  $x4+=($x[$count])**4;
	  $x1y+=$x[$count]*$y[$count];
	  $x2y+=(($x[$count])**2)*$y[$count];
	  ++$count;
	}
	my $a = $x1y - $x1*$y/$total;
	my $b = $x3 - $x1*$x2/$total;
	my $c = $x2 - $x1**2/$total;
	my $d = $x2y - $x2*$y/$total;
	my $e = $x4 - $x2**2/$total;

	# Try to avoid division by zero
	$c = $c == 0 ? 0.00001 : $c;
	my $tmp = ($b**2/$c - $e);
	$tmp = $tmp == 0 ? 0.00001 : $tmp;

	my $apr1 = ($a*$b/$c - $d) / $tmp;
	my $apr2 = $a/$c - $b*$apr1/$c;
	my $apr3 = ($y - $apr2*$x1 - $apr1*$x2)/$total;

	@polynomial = ($apr1,$apr2,$apr3);

	return \@polynomial;
}

sub _guess
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 param_log => { isa => 'ArrayRef', optional => 1 },
		 side => { isa => 'Str', optional => 1 },
		 param => { isa => 'Str', optional => 1 },
		 num => { isa => 'Int', optional => 1 },
		 probnum => { isa => 'Int', optional => 1 }
	);
	my @param_log = defined $parm{'param_log'} ? @{$parm{'param_log'}} : ();
	my $guess;
	my $side = $parm{'side'};
	my $param = $parm{'param'};
	my $num = $parm{'num'};
	my $probnum = $parm{'probnum'};
	
	my @x = @{$param_log[0]};
	my @y = @{$param_log[1]};
	
	croak('The number of logged parameter values ('.
		scalar @{$param_log[0]}.
		') does not match the number of logged ofv-diffs ('.  scalar @{$param_log[1]}.')' )
	if ( scalar @{$param_log[0]} != scalar @{$param_log[1]} );

	my ( @x1, @y1 );
	
	my $points = scalar(@x);
	
	my $zero = 0;

	while ($y[$zero] > 0){
	  $zero++;
	}

	if ( $side eq 'lower' ) {
	  @x1 = @x[0..2];
	  @y1 = @y[0..2];
	  
	} else {
	  @x1 = @x[$points-3..$points-1];
	  @y1 = @y[$points-3..$points-1];
	}

	my $goal = $y[$zero]+ $self -> ofv_increase;
	
	my @pol = @{$self -> _aag( x => \@x1,
				   y => \@y1 ) };

	if( $pol[0] == 0 ) {
	  print "The log-likelihood profile could not be approximated by a second order polynomial\n".
	      "The output may not be correct\n";
	  $pol[0] = 0.00001;
	}

	if ( $side eq 'lower' ){
	  if ($pol[0] > 0){
	    $guess = -$pol[1]/2/$pol[0] -
	      (($pol[1]/2/$pol[0])**2 - ($pol[2]-$goal)/$pol[0])**(0.5);
	  } else {
	    $guess = -$pol[1]/2/$pol[0] +
	      (($pol[1]/2/$pol[0])**2 - ($pol[2]-$goal)/$pol[0])**(0.5);
	  }
	  
	} else {
	  if ($pol[0] > 0){
	    $guess = -$pol[1]/2/$pol[0] +
	      (($pol[1]/2/$pol[0])**2 - ($pol[2]-$goal)/$pol[0])**(0.5);
	  } else {
	    $guess = -$pol[1]/2/$pol[0] -
	      (($pol[1]/2/$pol[0])**2 - ($pol[2]-$goal)/$pol[0])**(0.5);
	  }
	}
	
	if ($guess eq '-nan' or $guess eq 'nan' or $guess eq '-1.#IND' ){ #'-1.#IND' - is that a compiler spec. signal?
	  if ( ($y[0] - $y[1]) == 0 or ($x[0] - $x[1]) == 0 or
	       ($y[$points-1] - $y[$points-2]) == 0 or ($x[$points-1] - $x[$points-2]) == 0 ) {
	    $guess = undef;
	  } else {
	    if ( $side eq 'lower' ){
	      $guess = $x[0] - ($goal - $y[0]) / ( ($y[0] - $y[1])/($x[0] - $x[1]));
	    } else {
	      $guess = $x[$points-1] + ($goal - $y[$points-1]) / ( ($y[$points-1] - $y[$points-2])/($x[$points-1] - $x[$points-2]));
	    }
	  }
	}

	return $guess;
}

sub prepare_results
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	$self -> read_raw_results();

	unless( defined $self->raw_line_structure){
	  print "reading raw line structure in ".$self->directory().'raw_results_structure'."\n";
	  $self->raw_line_structure( ext::Config::Tiny -> read( $self->directory().'raw_results_structure'));
	}
	for ( my $i = 0; $i < scalar @{$self->raw_results}; $i++ ) { # All models
	  my $orig_mod = $self -> models->[$i];
	  my @params = ( 'theta', 'omega', 'sigma' );
	  my ( %param_nums, %labels, %orig_estimates );
	  foreach my $param ( @params ) {
	    my $modlabels = $orig_mod -> labels( parameter_type => $param );
	    $labels{$param} = $modlabels -> [0]; # Only one problem
	    $param_nums{$param} = scalar @{$modlabels -> [0]} if ( defined $modlabels );
	    my $orig_ests    = $orig_mod -> get_values_to_labels ( category => $param);
	    $orig_estimates{$param} = $orig_ests->[0][0];
	  }
	  my ( %ci, %near_bound, %max_iterations, %interval_ratio,
	       %skewed, %within_interval );

	  # The 9 on the row below is offset for iteration,
	  # parameter.type, parameter.number, side, finish.message,
	  # model, problem, subproblem, ofv

	  # Skip original run:
	  for ( my $j = 1; $j < scalar @{$self -> raw_results->[$i]}; $j++ ) {
	    my $row = $self -> raw_results->[$i][$j];

	    my ($start,$len) = split(/,/,$self->raw_line_structure-> {'1'} -> {$row -> [1]});
	    #$row->[1] is parameter.type in rawres
	    #$row->[2] is parameter.number in rawres
	    #$row->[3] is side in rawres
	    #$num is index in rawres of this param  
	    my $num = $start+($row -> [2]  - 1);

	    $ci{$row -> [1]}{$row -> [2]}{$row -> [3]} = $row -> [$num];
	    if ( $row -> [4] eq 'near.boundary' ) {
	      $near_bound{$row -> [1]}{$row -> [2]}{$row -> [3]} = 1;
	    } elsif ( $row -> [4] eq 'max.iterations' ) {
	      $max_iterations{$row -> [1]}{$row -> [2]}{$row -> [3]} = 1;
	    }
	  }
	  my ( @ci_labels, @ci_values, @li_values );
	  $ci_labels[1] = [ 'lower', 'maximum.likelihood.estimate',
			    'upper', 'interval.ratio', 'near.bound','max.iterations' ];
	  foreach my $param ( @params ) {
	    next if ( not defined $ci{$param} );
	    my @nums = sort { $a <=> $b } keys %{$ci{$param}};
	    foreach my $num ( @nums ) {
	      push( @{$ci_labels[0]}, $labels{$param}[$num-1] );
	      if ( defined $ci{$param}{$num}{'lower'} and 
		   defined $ci{$param}{$num}{'upper'} ) {
		if( abs( $ci{$param}{$num}{'lower'} - $orig_estimates{$param}[$num-1] ) == 0 ){
		  $interval_ratio{$param}{$num} = 'INF';
		} else {
		  $interval_ratio{$param}{$num} =
		      abs( $ci{$param}{$num}{'upper'} - $orig_estimates{$param}[$num-1] ) /
		      abs( $ci{$param}{$num}{'lower'} - $orig_estimates{$param}[$num-1] );
		  if ( $interval_ratio{$param}{$num} > $self -> {$param.'_interval_ratio_check'} or
		       $interval_ratio{$param}{$num} < 1/$self -> {$param.'_interval_ratio_check'} ) {
		    $skewed{$param}{$num} = 1;
		  } else {
		    $skewed{$param}{$num} = 0;
		  }
		  if ( $self -> within_interval_check < $ci{$param}{$num}{'upper'} and
		       $self -> within_interval_check > $ci{$param}{$num}{'lower'} ) {
		    $within_interval{$param}{$num} = 1;
		  } else {
		    $within_interval{$param}{$num} = 0;
		  }
		}
	      }
	      my @row;
	      push( @row, $ci{$param}{$num}{'lower'} );
	      push( @row, $orig_estimates{$param}[$num-1] );
	      push( @row, $ci{$param}{$num}{'upper'} );
	      push( @row, $interval_ratio{$param}{$num} );
	      push( @row, $near_bound{$param}{$num}{'upper'} ? 1 : $near_bound{$param}{$num}{'lower'} ? 1 : 0 );
	      push( @row, $max_iterations{$param}{$num}{'upper'} ? 1 : $max_iterations{$param}{$num}{'lower'} ? 1 : 0 );
	      push( @ci_values, \@row );
	    }
	  }
 	  my %return_section;
	  $return_section{'name'} = 'confidence.intervals';
	  $return_section{'labels'} = \@ci_labels;
	  $return_section{'values'} = \@ci_values;
 	  unshift( @{$self -> results->[$i]{'own'}},\%return_section );
	}
}

sub _test_sigdig
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number => { isa => 'Num', optional => 1 },
		 goal => { isa => 'Num', optional => 1 },
		 sigdig => { isa => 'Int', optional => 1 }
	);
	my $number = $parm{'number'};
	my $goal = $parm{'goal'};
	my $test;
	my $sigdig = $parm{'sigdig'};

	$number = &FormatSigFigs($number, $sigdig );
	if ( $goal == 0 ) {
	  $number = sprintf( "%.4f", $number );
	  $goal = sprintf( "%.4f", $goal );
	} else {
	  $goal = &FormatSigFigs($goal, $sigdig );
	}
	$test = $number eq $goal ? 1 : 0;

	return $test;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
