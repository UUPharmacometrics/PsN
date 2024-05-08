package scmplus;
require file;
require tool;
require tool::scm;
require tool::scm::config_file;
require model_plus;
use MouseX::Params::Validate;
require ui;
use PsN;
use model;
use scmlogfile;
use scm_util;
use warnings;
use Mouse;
use File::Copy qw(move mv copy);
use OSspecific;
use File::Copy::Recursive;

extends 'tool';

has 'iteration' => ( is => 'rw', isa => 'Int', default => 0 );
has 'stashed_count' => ( is => 'rw', isa => 'Int', default => 0 );
has 'dropped' => ( is => 'rw', isa => 'Str');  
has 'scm_top_directory' => ( is => 'rw', isa => 'Str');  
has 'logfilename' => ( is => 'rw', isa => 'Str');  
has 'scm' => ( is => 'rw', isa => 'tool::scm');  
has 'done' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'setup_only' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'readded_stashed' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'retest_stashed_relations' => ( is => 'rw', isa => 'Bool', default => 1);  
has 'started_backward' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'next_reduction_step' => ( is => 'rw', isa => 'Int');  
has 'previous_reduction_step' => ( is => 'rw', isa => 'Maybe[Int]');  
has 'max_scm_depth' => ( is => 'rw', isa => 'Maybe[Int]');  
has 'both_directions' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'keep_local_min' => ( is => 'rw', isa => 'Bool', default => 1);  
has 'keep_failed' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'p_cutoff' => ( is => 'rw', isa => 'Num');  
has 'p_backward' => ( is => 'rw', isa => 'Num');  
has 'p_forward' => ( is => 'rw', isa => 'Num');  
has 'ofv_backward' => ( is => 'rw', isa => 'Maybe[Num]');  
has 'original_relations' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'original_test_relations' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'dropped_relations' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'scope_reduction_steps' => ( is => 'rw', isa => 'ArrayRef', default => sub { [1] } );
has 'keep_covariance' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'keep_tables' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'maxevals' => ( is => 'rw', isa => 'Str', default => '3.1');  
has 'ctype4' => ( is => 'rw', isa => 'Bool', default => 1);  
has 'etas' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'ignore_no_sigl' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'tune_model' => ( is => 'rw', isa => 'Bool', default => 1);  
has 'original_logit' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );

our $perform_scope_reduction='perform_scope_reduction';
our $switch_to_backward='switch_to_backward';
our $add_back_stashed='add_back_stashed';
our $finalize='finalize';
our $plain_step_in_same_direction='plain_step_in_same_direction';

sub set_common_defaults
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef'},
		);
	my $options = $parm{'options'};

	unless ((defined $options->{'crash_restarts'} and (length($options->{'crash_restarts'})>0)) or
			(defined $options->{'handle_crashes'} and (length($options->{'handle_crashes'})>0)) ){
		$options->{'crash_restarts'} = 1;
		$options->{'handle_crashes'} = 0;
	}
    if (not defined $options->{'clean'} or $options->{'clean'} > 2) {
        $options->{'clean'} = 2;
    }
}

sub set_next_reduction_step
{
    my $self = shift;
	$self->previous_reduction_step($self->next_reduction_step());
	if (scalar(@{$self->scope_reduction_steps()}) > 0){
		if ($self->scope_reduction_steps()->[0] eq 'all'){
			if (defined $self->previous_reduction_step()){
				$self->next_reduction_step($self->previous_reduction_step() + 1);
			}else{ 
				$self->next_reduction_step(1);
			}			
		}else{
			$self->next_reduction_step(shift @{$self->scope_reduction_steps()});
		}
	}else{
		$self->next_reduction_step(0);
	}
}

sub set_max_scm_depth
{
    my $self = shift;

	my $next_scm_step_number=1;
	$next_scm_step_number= 2 if ($self->iteration() >0);
	if ($self->etas){
		$self->max_scm_depth($next_scm_step_number);
	}else{
		my $delta = $self->next_reduction_step();
		$delta -= $self->previous_reduction_step() if (defined $self->previous_reduction_step());
		if ($delta > 0){
			$self->max_scm_depth($delta + $next_scm_step_number -1);
		}else{
			$self->max_scm_depth(undef);
		}
	}
}

sub final_directory
{
    my $self = shift;
	return $self->directory.'final_model';
}

sub current_option_string
{
    my $self = shift;
	my %parm = validated_hash(\@_,
							  local_options => {isa => 'ArrayRef', optional => 0},
		);
	my $local_options = $parm{'local_options'};

	my $string = '';
	
	
	$string .= "Actual values optional scmplus options (undefined values not listed):\n";
	foreach my $opt (sort(@{$local_options})){
		$opt =~ s/[!:|].*//g; #get rid of :s |? :i etcetera
		if (defined $self->{$opt}){
			if (not ref($self->{$opt})){
				$string .= "-$opt=".$self->{$opt}."\n";
			} elsif ( ref($self->{$opt}) eq "ARRAY") {
				if (not ref($self->{$opt}->[0])){
					$string .= "-$opt=".(join ',',@{$self->{$opt}})."\n";
				}
			}
		}
	}
	return $string;
}

sub print_options
{
    my $self = shift;
	my %parm = validated_hash(\@_,
							  tool_options => {isa => 'ArrayRef', optional => 0},
							  scmplus_options => {isa => 'ArrayRef', optional => 0},
							  cmd_line => {isa => 'Str', optional => 0},
		);
	my $tool_options = $parm{'tool_options'};
	my $scmplus_options = $parm{'scmplus_options'};
	my $cmd_line = $parm{'cmd_line'};

	# scm 4.7.0
	my %optional_scm_options= ("search_direction:s" => 'forward|backward|both',
							   "gof:s" => '',
							   "base_ofv:f" => '',
							   "global_init:s" => '',
							   "logfile:s" => '',
							   "model:s" => '',
							   "noabort!" => '',
							   "max_steps:i" => '',
							   "p_value:s" => '',
							   "p_forward:s" => '',
							   "p_backward:s" => '',
							   "do_not_drop:s" => '',
							   "linearize!" => '',
							   "epsilon!" => '',
							   "foce!" => '',
							   "lst_file:s" => '',
							   "update_derivatives!" => '',
							   "only_successful!" => '',
							   "parallel_states!" => '',
							   "error:s"=> '');

	$self->scm()-> print_options(cmd_line => $cmd_line, 
								 toolname => 'scm',
								 directory => $self->directory(),
								 local_options => [keys %optional_scm_options],
								 common_options => $tool_options);
	

	#append new option string
	my $scmplus_options_string = $self->current_option_string(local_options=>$scmplus_options);
	open(CMD, ">>", $self->directory . "/version_and_option_info.txt");
	print CMD $scmplus_options_string."\n";
	close(CMD);
}

sub p_cutoff_default
{
	my $config_file = shift;
	my %opt=('p_cutoff'=> 0.05); #hard-coded scm.pm line 182
	if( defined $config_file -> p_forward ){
		$opt{'p_cutoff'} = $config_file -> p_forward;
	}elsif (defined $config_file -> p_value){
		$opt{'p_cutoff'} = $config_file -> p_value;
	}
	return \%opt;
}

sub fast_defaults
{
	my $fast = shift;
	my %fast_defaults=();
	if (defined $fast){
		if ($fast){
			$fast_defaults{'maxevals'}='3.1';
			$fast_defaults{'ctype4'}=1;
			$fast_defaults{'retest_stashed_relations'}=0;
			$fast_defaults{'scope_reduction_steps'}=['all'];
		}
	}
	return \%fast_defaults;
}

sub parsed_scope_reduction_steps
{
	my $option = shift;
	my %hash=();
	if (defined $option){
		if ($option eq 'all'){
			$hash{'scope_reduction_steps'}=['all'];
		}elsif ($option eq 'none'){
			$hash{'scope_reduction_steps'}=[];
		}else{
			my @fields = split(',',$option);
			my @array=();
			foreach my $step (@fields){
				if (($step =~ /^\d+$/) and ($step > 0) and 
					((scalar(@array)==0) or ($step > $array[-1]))){
					push(@array,$step);
				}else{
					die("$option is not a comma-separated list of increasing and positive integers");
				}
			}
			$hash{'scope_reduction_steps'}=[@array];
		}
	}
	return \%hash;
}

sub setup
{
    my $self = shift;
	my %parm = validated_hash(\@_,
							  config_file => {isa => 'tool::scm::config_file', optional => 0},
							  options => {isa => 'HashRef', optional => 0},
							  common_options_hashref => {isa => 'HashRef', optional => 0},
		);
	my $config_file = $parm{'config_file'};
	my $options = $parm{'options'};
	my $common_options_hashref = $parm{'common_options_hashref'};
	
	$self->both_directions(($config_file->search_direction() eq 'both')? 1 : 0);
	$self -> _prepare_model( model_number => 1 ); #this just creates m1
	
	if ($self->tune_model()){
		my ($errors,$warnings,$information) = 
			model_plus::tune(model => $self->models->[0],
							 keep_covariance => $self->keep_covariance,
							 keep_tables => $self->keep_tables,
							 ctype4 => $self->ctype4,
							 etas => $self->etas,
							 ignore_no_sigl => $self->ignore_no_sigl,
							 maxevals => $self->maxevals,
							 reference_evaluations => model_plus::get_number_of_evaluations(model =>$self->models->[0])
			);
		model_plus::report(errors => $errors,
						   warnings => $warnings,
						   information => $information,
						   category => 'scm',
						   scriptname => 'scmplus');
	}
		
	$self->models->[0] -> _write(filename => $self->directory.'/m1/base_model.mod');
	if (defined $self->models->[0] ->extra_files()){
		foreach my $file (@{$self->models->[0] ->extra_files()}){
			copy($file,$self->directory.'/m1/');
		}
	}
	
	$self->scm_top_directory($self->directory().'rundir');
	
	$self->scm(scm_util::setup( common_options_hashref => $common_options_hashref,
								config_file => $config_file,
								options => $options,
								model => $self->models->[0],
								scm_options_hashref => {both_directions => 0,
														directory => $self->scm_top_directory,
														logfile => [$self->directory().'scmlog.txt']},
			   )
		);
	
    if (defined $self->scm()->logit and scalar(@{$self->scm()->logit()})>0){
		push(@{$self->original_logit()},@{$self->scm()->logit()});
	}
	
	$self->ofv_backward($self->scm()->ofv_backward);
	if (defined $self->scm()->p_backward){
		$self->p_backward($self->scm()->p_backward);
	}else{
		$self->p_backward($self->scm()->p_value());
	}
	$self->p_forward($self->scm()->p_value());

}

sub get_internal_logit
{
	my %parm = validated_hash(\@_,
							  original_logit => {isa => 'ArrayRef', optional => 0},
							  test_relations => {isa => 'HashRef', optional => 0}
		);
	my $original_logit = $parm{'original_logit'};
	my $test_relations = $parm{'test_relations'};

	my @internal_logit = ();
	my $relation_hash = {};
	
    if (scalar(@{$original_logit})>0){
		foreach my $par ( sort keys %{$test_relations} ){
			$relation_hash->{$par}=1;
		}
        foreach my $par (@{$original_logit}){
            if (defined $relation_hash->{$par}){
				push(@internal_logit,$par);
			}
        }
    }
	return \@internal_logit;
}

sub get_internal_scm
{
	my %parm = validated_hash(\@_,
							  oldscm => {isa => 'tool::scm', optional => 0},
							  test_relations => {isa => 'HashRef', optional => 0},
							  relations => {isa => 'HashRef', optional => 0},
							  finaldir => {isa => 'Str',optional => 0},
							  dir => {isa => 'Str',optional => 0},
							  p_value => {isa => 'Num',optional => 0},
							  search_direction => {isa => 'Str',optional => 0},
							  both_directions => {isa => 'Bool',optional => 0},
							  max_steps => {isa => 'Any'},
							  step_number => {isa => 'Int', optional=>0},
							  initial_estimates_model => {isa => 'model'},
							  internal_logit => {isa => 'ArrayRef', optional => 0}
		);
	my $oldscm = $parm{'oldscm'};
	my $test_relations = $parm{'test_relations'};
	my $relations = $parm{'relations'};
	my $finaldir = $parm{'finaldir'};
	my $dir = $parm{'dir'};
	my $p_value = $parm{'p_value'};
	my $search_direction = $parm{'search_direction'};
	my $both_directions = $parm{'both_directions'};
	my $max_steps = $parm{'max_steps'};
	my $step_number = $parm{'step_number'};
	my $initial_estimates_model = $parm{'initial_estimates_model'};
	my $internal_logit = $parm{'internal_logit'};
	
	my $model_number=1;

	unless (-d $finaldir) {
		mkdir ($finaldir);
	}
	
	return tool::scm ->new( %{common_options::restore_options(@common_options::tool_options)},
								 gof                    => $oldscm -> gof(),
								 test_relations         => $test_relations,
								 parameters             => $oldscm -> parameters,
								 check_nmtran            => 0,
								 main_data_file            => $oldscm->main_data_file,
								 categorical_covariates => $oldscm -> categorical_covariates(),
								 continuous_covariates  => $oldscm -> continuous_covariates(),
								 do_not_drop            => $oldscm -> do_not_drop,
								 ofv_change             => $oldscm -> ofv_change,
								 p_value                => $p_value,
								 search_direction       => $search_direction,
							     both_directions => $both_directions,
								 valid_states           => $oldscm -> valid_states,
								 covariate_statistics_file => $oldscm -> covariate_statistics_file,
								 relations_file         => $oldscm -> relations_file,
								 short_logfile          => [$oldscm -> short_logfile->[$model_number-1]],
								 bounds                 => $oldscm -> bounds,
								 cpu_time             => undef,
								 xv_pred_data          => $oldscm -> xv_pred_data,
								 max_steps             => $max_steps,
								 xv_results              => $oldscm -> xv_results,
								 global_init          => $oldscm -> global_init,
								 covariate_statistics => $oldscm -> covariate_statistics,
								 directory            => $dir,
								 models               => [$oldscm -> models->[$model_number-1]],
								 relations            => $relations,
								 initial_estimates_model => $initial_estimates_model, 
								 included_relations   => $oldscm -> included_relations,
								 step_number          => $step_number,
								 raw_results_file     => [$oldscm -> raw_results_file ->[$model_number-1]],
								 logfile              => [$oldscm -> logfile->[$model_number-1]],
								 base_criteria_values => $oldscm -> base_criteria_values, #when regular scm returns then this is updated $new_base_crit_val_ref 
								 parent_tool_id       => $oldscm -> tool_id,
								 top_tool             => 0,
								 logit                => $internal_logit,
								 linearize                 => $oldscm->linearize,
								 foce                 => $oldscm->foce,
								 second_order         => $oldscm->second_order,
								 only_successful        => $oldscm->only_successful(),
								 parameter_eta        => $oldscm->parameter_eta,
								 parameter_relation   => $oldscm->parameter_relation,
								 derivatives_base_model => $oldscm->derivatives_base_model,
								 derivatives_output    => $oldscm->derivatives_output(),
								 data_items    => $oldscm->data_items(),
								 sizes_pd    => $oldscm->sizes_pd(),
								 update_derivatives    => $oldscm->update_derivatives(),
								 error                 => $oldscm->error(),
								 error_code           => $oldscm->error_code(),
								 epsilon           => $oldscm->epsilon(),
								 parallel_states     => $oldscm->parallel_states(),
								 config_file          => undef,
								 resulting_model      => undef,
								 xv_results_file => $oldscm->xv_results_file(),
								 final_model_directory => $finaldir
				);

}

sub get_readded_relations
{
	my $self = shift;

	#merge included and stashed, drop the rest
	my %readded_relations =();
	foreach my $par ( sort keys %{$self->original_relations()} ) {
		foreach my $cov ( sort keys %{$self->original_relations()->{$par}} ){
			if (((defined $self->dropped_relations()->{$par}) and (defined $self->dropped_relations()->{$par}{$cov}) and 
				 ($self->dropped_relations()->{$par}{$cov} == 1)) or
				((defined $self->scm()->included_relations->{$par} ) and 
				 (defined $self->scm->included_relations->{$par}{$cov}) and ((defined $self->scm->included_relations->{$par}{$cov}{'state'})))){
				$readded_relations{$par}{$cov}=$self->original_relations()->{$par}{$cov};
			}
		}
	}

	return \%readded_relations;
}

sub get_reduced_relations
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  original_relations => {isa => 'HashRef', optional => 0},
							  drop_relations => {isa => 'HashRef', optional => 0},
		);
	my $original_relations = $parm{'original_relations'};
	my $drop_relations = $parm{'drop_relations'};

	my %reduced_relations = ();
	foreach my $par ( sort keys %{$original_relations} ) {
		foreach my $cov ( sort keys %{$original_relations->{$par}} ){
			if ((defined $drop_relations->{$par}) and (defined $drop_relations->{$par}{$cov}) and
				$drop_relations->{$par}{$cov} == 1){
				next;
			}else{
				$reduced_relations{$par}{$cov}=$original_relations->{$par}{$cov};
			}
		}
	}
	return \%reduced_relations;
}

sub drop_new_relations
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  drop => {isa => 'HashRef', optional => 0},
		);
	my $drop = $parm{'drop'};

	my %all_dropped_relations = %{$self->dropped_relations};
	foreach my $par (sort keys %{$drop}){
		foreach my $cov (sort keys %{$drop->{$par}}){
			$all_dropped_relations{$par}{$cov}=$drop->{$par}{$cov};
		}
	}
	$self->dropped_relations(\%all_dropped_relations);
	
}

sub get_included_text
{
	my %parm = validated_hash(\@_,
							  included_relations => {isa => 'HashRef', optional => 0},
		);
	my $included_relations = $parm{'included_relations'};
	my $text = '';
	my $first=1;
	foreach my $par (sort keys %{$included_relations}){
		foreach my $cov (sort keys %{$included_relations->{$par}}){
			next unless (defined $included_relations->{$par}{$cov}{'state'});
			if ($first){
				$first=0;
				$text .= ' ';
			}else{
				$text .= ',';
			}
			$text .= $par.'-'.$cov.'-'.$included_relations->{$par}{$cov}{'state'};
		}
	}
	if ($first){
		$text = 'none';
	}
	return $text;

}

sub get_drop_text
{
	my %parm = validated_hash(\@_,
							  drop_relations => {isa => 'HashRef', optional => 0},
		);
	my $drop_relations = $parm{'drop_relations'};
	my $text = '';
	my $first=1;
	foreach my $par (sort keys %{$drop_relations}){
		foreach my $cov (sort keys %{$drop_relations->{$par}}){
			if ($first){
				$first=0;
			}else{
				$text .= ',';
			}
			$text .= $par.'-'.$cov;
		}
	}
	return $text;
}

sub get_readded_test_relations
{
	my $self = shift;

	#merge included and stashed, drop the rest
	my %readded_test_relations =();
	foreach my $par ( sort keys %{$self->original_test_relations()} ) {
		foreach my $cov ( @{$self->original_test_relations()->{$par}} ){
			if (((defined $self->dropped_relations()->{$par}) and (defined $self->dropped_relations()->{$par}{$cov})
				 and ($self->dropped_relations()->{$par}{$cov} == 1)) or
				((defined $self->scm()->included_relations->{$par} ) and 
				 (defined $self->scm->included_relations->{$par}{$cov}) and ((defined $self->scm->included_relations->{$par}{$cov}{'state'})))){
				$readded_test_relations{$par} = [] unless (defined $readded_test_relations{$par});
				push(@{$readded_test_relations{$par}},$cov);
			}
		}
	}

	return \%readded_test_relations;
}

sub get_reduced_test_relations
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  original_test_relations => {isa => 'HashRef', optional => 0},
							  drop_relations => {isa => 'HashRef', optional => 0},
		);
	my $original_test_relations = $parm{'original_test_relations'};
	my $drop_relations = $parm{'drop_relations'};

	my %reduced_test_relations = ();
	foreach my $par ( sort keys %{$original_test_relations} ) {
		foreach my $cov ( @{$original_test_relations->{$par}} ){
			if ((defined $drop_relations->{$par}) and (defined $drop_relations->{$par}{$cov}) and
				$drop_relations->{$par}{$cov} == 1){
				next;
			}else{
				$reduced_test_relations{$par} = [] unless (defined $reduced_test_relations{$par});
				push(@{$reduced_test_relations{$par}},$cov);
			}
		}
	}
	return \%reduced_test_relations;
}

sub get_scmdir_and_finaldir
{
	my %parm = validated_hash(\@_,
							  scm_topdir => {isa => 'Str', optional => 0},
							  tag => {isa => 'Str',optional=> 0},
							  stepnum => {isa => 'Int',optional => 0}
		);
	my $scm_topdir = $parm{'scm_topdir'};
	my $tag = $parm{'tag'};
	my $stepnum = $parm{'stepnum'};
	
	my $dir = $scm_topdir.'/'.$tag.'_scm_dir'.$stepnum;
	my ($finaldir, $dummy) = 
		OSspecific::absolute_path($scm_topdir.'/'.$tag.'_final_models'.$stepnum, '');

	return ($dir,$finaldir);
}

sub have_included_relations
{
	my $self = shift;

	foreach my $par (keys %{$self->scm() ->included_relations()}){
		foreach my $cov (keys %{$self->scm() ->included_relations()->{$par}}){
			if ( defined $self->scm() ->included_relations()->{$par}{$cov}{'state'}){
				return 1;
			}
		}
	}
	return 0;

}

sub get_initial_estimates_model
{
	my %parm = validated_hash(\@_,
							  oldscm => {isa => 'tool::scm', optional => 0},
		);
	my $oldscm = $parm{'oldscm'};

	my $direction = $oldscm ->search_direction();
	
	if (defined $oldscm -> resulting_model){
		$oldscm -> resulting_model->set_outputfile();
		$oldscm -> resulting_model->load_output();
		return $oldscm -> resulting_model; 
	}else{
		if (-e $oldscm->final_model_directory.'final_'.$direction.'.mod'){
			return model->new(filename => $oldscm->final_model_directory.'final_'.$direction.'.mod',
							  parse_output => 1);
		}else{
			return $oldscm -> initial_estimates_model();
		}
	}

}

sub log_scm_basedir
{
	my $self = shift;

	my $logdir = $self->scm()->directory;
	$logdir = "done" if ($self->done);
	open( my $fh, ">".$self->scm_top_directory.'/'.$scmlogfile::nextbasefilename ); #overwrite
	print $fh $logdir."\n";
	close $fh;

}

sub choose_action
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  any_relation_chosen => {isa => 'Bool'},
							  step_count => {isa => 'Int'}
		);
	my $any_relation_chosen = $parm{'any_relation_chosen'};
	my $step_count = $parm{'step_count'};

	my $asrscmtag = $scmlogfile::asrscmtag;
	my $action=undef;
	my $message='';

	if ($any_relation_chosen){
		if (($self->next_reduction_step() > 0) and
			($self->next_reduction_step() == $step_count)){
			$action = $perform_scope_reduction;
			$self->set_next_reduction_step();
		}else{
			$action = $plain_step_in_same_direction;
			$message .= "Taking a step ".$self->scm()->search_direction()." in scmplus.\n"; 
		}
	}else{
		if ($self->next_reduction_step() > 0){
			if ($self->next_reduction_step() == $step_count){
				$message .= "Scope reduction requested in $asrscmtag after forward step $step_count but\n".
					"no relation was chosen for inclusion by scm in this step:\n";
			}
			$self->scope_reduction_steps([]); #no more scope reduction
			$self->set_next_reduction_step();
		}
		if ((not $self->readded_stashed()) and ($self->stashed_count() > 0)
			and $self->retest_stashed_relations()){
			$message .=  "$asrscmtag forward search with reduced scope is done.\n";
			$action = $add_back_stashed;
		}elsif ($self->both_directions() and 
				(not $self->started_backward()) and $self->have_included_relations()){
			$message .=  "scmplus forward search is done.\n";
			$action = $switch_to_backward;
		}else{
			$message .= "scmplus ".$self->scm()->search_direction()." search is done.\n";
			$action = $finalize;
		}
	}
	return ($action,$message);
}

sub setup_next_scm
{
	my $self = shift;

	my $model_number=1;
	my $asrscmtag = $scmlogfile::asrscmtag;

	if ($self->iteration()==0){
		$self->set_next_reduction_step();
		$self->set_max_scm_depth();
		$self->scm()->max_steps($self->max_scm_depth()) if (defined $self->max_scm_depth());
	}else{
		my $oldscm = $self->scm();
		unless (defined $self->logfilename()){
			$self->logfilename($oldscm-> logfile->[$model_number-1]);
		}
		my $message = '';
		my %current_test_relations;
		my %current_relations;
		my $logfile;

		my $have_failed = 0;
		my ($stashed_count,$drop_relations);
		
		my $any_relation_chosen=0;
		
		%current_test_relations = %{$oldscm -> test_relations};
		%current_relations = %{$oldscm -> relations};
		unless (scalar(keys %{$self->original_test_relations()}) > 0){
			# storing original relations
			$self->original_test_relations(\%current_test_relations);
			$self->original_relations(\%current_relations);
		}
			
		$logfile = scmlogfile->new(filename => $self->logfilename,
								   test_relations => $self->original_test_relations());

		my ($search_direction_forward,$next_stepnumber) = $logfile->is_forward_and_stepnumber();
		my $step_count=$next_stepnumber-1;

		if (($self->next_reduction_step() > 0) and (not $search_direction_forward) ){
			die("next reduction step is positive ".$self->next_reduction_step()." but logfile indicates backward direction");
		}
		
		if (defined $logfile->steps->[-1]->chosen_index()){
			$any_relation_chosen=1;
		}

		my ($action,$tempmessage) = $self->choose_action(any_relation_chosen => $any_relation_chosen,
														 step_count => $step_count);
		$message .= $tempmessage;
		$self->set_max_scm_depth();

		my $phierror = [];
		my $phiwarning =[];
		my $phiinfo = [];
		
		if ($self->etas() and (not $action eq $finalize)){
			my $phifile = $logfile->current_base_phi_file();
			($phierror,$phiwarning,$phiinfo) = model_plus::update_etas_filename(model => $oldscm -> models->[$model_number-1],
																				initiate_extra_files => 0,
																				etas_fullname => $phifile);
		}


		if ($action eq $finalize){
			$self->ensure_final_model_written(); 
			$self->done(1);
		}elsif ($action eq $perform_scope_reduction){
			($have_failed,$stashed_count,$drop_relations) = 
				$logfile->get_dropped_relations(p_cutoff => $self->p_cutoff(),
												keep_local_min => $self->keep_local_min(),
												keep_failed => $self->keep_failed(),
												step => -1);

			$self->drop_new_relations(drop => $drop_relations);
			$self->stashed_count($self->stashed_count()+ $stashed_count);

			my $reduced_relations = $self->get_reduced_relations(original_relations =>  \%current_relations,
																 drop_relations => $drop_relations);
			my $reduced_test_relations = $self->get_reduced_test_relations(original_test_relations =>  \%current_test_relations,
																		   drop_relations => $drop_relations);

			my ($dir,$finaldir) = get_scmdir_and_finaldir(stepnum => $next_stepnumber,
														  tag => 'reduced_forward',
														  scm_topdir => $self->scm_top_directory);

			my $internal_scm = get_internal_scm(oldscm => $oldscm,
												test_relations => $reduced_test_relations,
												relations => $reduced_relations,
												finaldir => $finaldir,
												both_directions => 0,
												dir => $dir,
												p_value =>$oldscm -> p_value, 
												search_direction => 'forward',
												max_steps => $self->max_scm_depth,
												step_number => 2, 
												initial_estimates_model => get_initial_estimates_model(oldscm => $oldscm),
												internal_logit => get_internal_logit ('original_logit' => $self->original_logit(),
																					  'test_relations' => $reduced_test_relations)
				);
			my $text = get_drop_text(drop_relations => $drop_relations);
			if (defined $self->dropped()){
				$self->dropped($self->dropped().','.$text) if (length($text) > 0);
			}else{
				$self->dropped($text) if ($stashed_count > 0);
			}
			#this text is parsed by scmlogfile 
			$message .= "Taking a step forward in $asrscmtag after reducing scope with $stashed_count relations : ".$text;
			$self->scm($internal_scm);
		}elsif ($action eq $add_back_stashed){
			$self->readded_stashed(1);

			my ($dir,$finaldir) = get_scmdir_and_finaldir(stepnum => $next_stepnumber,
														  tag => 'readded_forward',
														  scm_topdir => $self->scm_top_directory);

			my $readded_test_relations = $self->get_readded_test_relations();
			my $internal_scm = get_internal_scm(oldscm => $oldscm,
												test_relations => $readded_test_relations,
												relations => $self->get_readded_relations(),
												finaldir => $finaldir,
												dir => $dir,
												both_directions => 0,
												step_number => 2, # ($oldscm -> step_number() + 1),
												p_value =>$self->p_forward, 
												search_direction => 'forward',
												initial_estimates_model => get_initial_estimates_model(oldscm => $oldscm),
												max_steps => $self->max_scm_depth(),
												internal_logit => get_internal_logit ('original_logit' => $self->original_logit(),
																					  'test_relations' => $readded_test_relations)
				);

			my $text = get_included_text(included_relations => $self->scm->included_relations);
			$message .= "Included relations so far: ".$text."\n";

			#this is parsed by asrscm
			$message .= "Re-testing ".$self->stashed_count()." relations after $asrscmtag reduced forward search : ".$self->dropped();
			$self->scm($internal_scm);

		}elsif ($action eq $switch_to_backward){

			$self->started_backward(1);

			my ($dir,$finaldir) = get_scmdir_and_finaldir(stepnum => $next_stepnumber,
														  tag => 'backward',
														  scm_topdir => $self->scm_top_directory);

			my $backward_test_relations = ($self->readded_stashed() ? $self->original_test_relations() : $oldscm -> test_relations);
			my $internal_scm = get_internal_scm(oldscm => $oldscm,
												test_relations => $backward_test_relations,
												relations => ($self->readded_stashed() ? $self->original_relations() : $oldscm -> relations),
												finaldir => $finaldir,
												dir => $dir,
												both_directions => 0,
												step_number => 2,
												p_value => $self->p_backward(), 
												search_direction => 'backward',
												initial_estimates_model => get_initial_estimates_model(oldscm => $oldscm),
												max_steps => $self->max_scm_depth(),
												internal_logit => get_internal_logit ('original_logit' => $self->original_logit(),
																					  'test_relations' => $backward_test_relations)
				);
			
			$message .= "Starting scmplus backward search";
			$self->scm($internal_scm);

		}elsif($action eq $plain_step_in_same_direction){
			my $tag;
			if ($search_direction_forward){
				if ($self->readded_stashed()){
					$tag = 'readded_forward';
				}elsif($self->stashed_count() > 0){
					$tag = 'reduced_forward';
				}else{
					$tag = 'forward';
				}
			}else{
				$tag = 'backward';
			}

			my ($dir,$finaldir) = get_scmdir_and_finaldir(stepnum => $next_stepnumber,
														  tag => $tag,
														  scm_topdir => $self->scm_top_directory);

			my $internal_scm = get_internal_scm(oldscm => $oldscm,
												test_relations => $oldscm -> test_relations,
												relations => $oldscm -> relations,
												finaldir => $finaldir,
												dir => $dir,
												both_directions => 0,
												step_number => 2,
												p_value => $oldscm->p_value(), 
												search_direction => $oldscm->search_direction(),
												initial_estimates_model => get_initial_estimates_model(oldscm => $oldscm),
												max_steps => $self->max_scm_depth(),
												internal_logit => get_internal_logit ('original_logit' => $self->original_logit(),
																					  'test_relations' => $oldscm -> test_relations)
				);
			
			$self->scm($internal_scm);
			
		}else{
			die("bug in decision tree");
		}
		ui -> print( category => 'scm',
					 message  =>  $message)
			unless $oldscm -> parent_threads > 1;
		model_plus::report(errors => $phierror,
						   warnings => $phiwarning,
						   information => $phiinfo,
						   category => 'scm',
						   scriptname => 'scmplus');

		open( LOG, ">>".$self->logfilename() );
		print LOG "\n".$message."\n";
		close LOG;
	}

	unless ($self->done()){
		$self->scm()->add_to_nmoutput(extensions => ['phi']) if ($self->etas());
	}
	
	$self->log_scm_basedir();
	$self->iteration($self->iteration+1);	
}

sub run
{
	my $self = shift;

	while (1) {
		$self->setup_next_scm();
		last if ($self->done() or $self->setup_only());
		my $message;
		if ($self->scm() ->search_direction eq 'forward'){
			if (defined $self->scm() ->max_steps()){
				my $maxstep = $self->scm() ->max_steps() - ($self->scm() ->step_number()-1);
				$message  = "Running internal scm with at most $maxstep forward step(s) before next update or scope reduction.";
			}else{
				$message  =  "Running internal forward scm until no more signficant relations or no more relations to add.";
			}
		}else{
			if (defined $self->scm() ->max_steps()){
				$message  = "Running internal scm with a single backward step before next update.";
			}else{
				$message = "Running internal backward scm until no more insignificant relations to remove.";
			}
		}
		ui -> print( category => 'scm',
					 message  =>  $message)
			unless $self->scm() -> parent_threads > 1;
		$self->scm() -> run;
	}		

}

sub ensure_final_model_written
{
	my $self = shift;
		
	#if forward and no relation added, no internal scm created
	#if backward and no relation removed, no internal scm created

	unless ( defined $self->scm() -> resulting_model ) {
		#verify no final model
		my $fname = 'final_'.$self->scm()->search_direction().'.mod';
		if ($self->scm()->linearize()){
			$fname = 'final_'.$self->scm()->search_direction().'_linear.mod';
		}
		my $fdir = $self->scm()->final_model_directory();
		if (-e "$fdir$fname"){
			ui -> print( category => 'scm',
						 message  =>  "Error: $fdir$fname exists");
		}else{
			ui -> print( category => 'scm',message => "Finalizing scmplus" );
			if (defined $self->scm() -> initial_estimates_model){
				$self->scm()->write_final_models(final_model => $self->scm() -> initial_estimates_model,
												 model_number => 1);
			}
		}
	}
	unless (-e $self->final_directory){
		unless(mkdir( $self->final_directory )){
			ui -> print( category => 'scm',
						 message  =>  "failed to create final directory : $!");
		}
	}
	unless (File::Copy::Recursive::dircopy($self->scm()->final_model_directory(),$self->final_directory)) {
		ui -> print( category => 'scm',
					 message  =>  "failed to copy final directory : $!");
	}
}

1;
