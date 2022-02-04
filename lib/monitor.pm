package monitor;

use strict;
use MouseX::Params::Validate;
use scmlogfile;

sub get_options
{
	my %parm = validated_hash(\@_,
							  directory => { isa => 'Str' },
							  options => { isa => 'HashRef' },
		);

	my $directory=$parm{'directory'};
	my $options =$parm{'options'}; 

	my $requested = scalar(keys %{$options});
	my $found = 0;
	my $filename = undef;
	if (-d $directory){
		my $versionfile = $directory.'/version_and_option_info.txt';
		if (-e $versionfile){
			open(my $fh, $versionfile) or return $found;
			while ($found < $requested) {
				my $line = readline($fh);
				unless (defined $line){
					last; #reached EOF
				}
				if ($line =~ /^-(\w+)=(\S+)/){
					foreach my $opt (keys %{$options}){
						next if defined $options->{$opt};
						if ($1 eq $opt){
							$options->{$opt}=$2;
							$found++;
							last;
						}
					}
				}
			}
		}
	}
	return $found;
}

sub get_scriptname
{
    # Figure out which tool was run in this directory
    #for now will return undef for most tools
    my $directory = shift;

    my $tool;

    if (-e $directory . '/version_and_option_info.txt') {
		open(my $fh, $directory . '/version_and_option_info.txt') or return $tool;
		while (1) {
			my $line = readline($fh);
			unless (defined $line){
				last; #reached EOF
			}
            if ($line =~ /^Actual values optional (\w+) options/) {
                $tool = lc($1);
                last unless ($tool eq 'scm'); #keep checking if actually scmplus
            }
        }
        close($fh);
	}

    return $tool; #can be undef
}

sub get_scmdir1_below
{
	my $directory =shift;
	$directory = strip_m1($directory);
	return $directory.'/scm_dir1';
}

sub strip_m1
{
	my $directory =shift;
	$directory =~ s/\/?(m1)?\/?$//;
	return $directory;
}

sub get_scm_progress
{
	my %parm = validated_hash(\@_,
							  tooldir => { isa => 'Str' },
							  tries => { isa => 'Int' , default => 2},
		);

    my $tooldir = $parm{'tooldir'};
	my $tries = $parm{'tries'};
	
	my $modelfit=$tooldir.'/modelfit_dir1';
	my $m1 = $tooldir.'/m1';
	my $candidates = {};
	my $translation = {};
	
	for (my $i=0; $i< $tries; $i++){
		if (scalar (keys %{$candidates}) < 1){
			$candidates = get_candidate_models($m1);
		}
		if (scalar (keys %{$translation}) < 1){
			$translation = get_model_NMrun_translation($modelfit);
		}
		last if ((scalar (keys %{$candidates}) > 0) and (scalar (keys %{$translation}) > 0));
	}

	my %results = ();
	if (scalar (keys %{$candidates}) > 0){
		%results = %{initiate_results(candidates => $candidates,
									  extra_attributes =>['parameter','covariate','oldstate','state'])};
		
		update_finished_results(results => \%results,
								directory => $m1,
								maxeval_exceeded => 1);
		update_running_results(results => \%results,
							   modelfit => $modelfit,
							   translation => $translation);
	}										
	return \%results;
}

sub update_running_results
{
	my %parm = validated_hash(\@_,
							  results => { isa => 'HashRef' },
							  translation => { isa => 'HashRef'},
							  estimates => { isa => 'Bool', default => 0 },
							  modelfit => { isa => 'Str' },
		);

    my $modelfit = $parm{'modelfit'};
	my $estimates = $parm{'estimates'}; 
	my $translation = $parm{'translation'};
	my $results = $parm{'results'};
	#FIXME support estimates
	foreach my $name (keys %{$results}){
		next if ($results->{$name}->{'finished'});
		if (defined $translation->{$name.'.mod'}){
			#sub does existence check on file, do not have to repeat here
			my ($feval,$ofv) = get_current_feval_ofv($modelfit.'/'.$translation->{$name.'.mod'}.'/psn.ext');
			$results->{$name}->{'ofv'} = $ofv;
			$results->{$name}->{'feval'} = $feval;
			if (not defined $results->{$name}->{'jobid'}){
				$results->{$name}->{'jobid'} = get_jobid($modelfit.'/'.$translation->{$name.'.mod'});
			}
		}
	}

}

sub initiate_results
{
	my %parm = validated_hash(\@_,
							  candidates => { isa => 'HashRef' },
							  extra_attributes => {isa => 'ArrayRef'},
		);

	my $candidates = $parm{'candidates'};
	my $extra_attributes = $parm{'extra_attributes'};

	my %results = ();
	foreach my $name (keys %{$candidates}){
		$results{$name} = {'finished' => 0,
						   'ofv' => undef,
						   'jobid' => undef,
						   'rounding_errors' => undef,
						   'maxeval_exceeded' => undef};
		foreach my $attr (@{$extra_attributes}){
			$results{$name}->{$attr} = $candidates->{$name}->{$attr};
		}
	}
	return \%results;

}

sub update_finished_results
{
	my %parm = validated_hash(\@_,
							  results => { isa => 'HashRef' },
							  maxeval_exceeded => { isa => 'Bool', default => 0 },
							  rounding_errors => { isa => 'Bool', default => 0 },
							  estimates => { isa => 'Bool', default => 0 },
							  directory => { isa => 'Str' },
		);

    my $directory = $parm{'directory'};
	my $estimates = $parm{'estimates'}; 
	my $maxeval_exceeded = $parm{'maxeval_exceeded'};
	my $rounding_errors = $parm{'rounding_errors'};
	my $results = $parm{'results'};

	my $found_finished=1;
	while ($found_finished){
		$found_finished=0;
		foreach my $name (keys %{$results}){
			next if $results->{$name}->{'finished'};
			if (-e $directory.'/'.$name.'.lst'){
				$results->{$name}->{'finished'}=1;
				$found_finished=1;
				$results->{$name}->{'ofv'}=get_final_ofv($directory.'/'.$name.'.ext');
				if ($maxeval_exceeded){
					$results->{$name}->{'maxeval_exceeded'}=maxevals_exceeded($directory.'/'.$name.'.lst');
				}
				if ($rounding_errors){
					$results->{$name}->{'rounding_errors'}=rounding_errors($directory.'/'.$name.'.lst');
				}
			}
		}
	}

}

sub monitor_scm
{
	my %parm = validated_hash(\@_,
							  directory => { isa => 'Str' },
							  is_asrscm => {isa => 'Bool'},
							  run_on_slurm => {isa => 'Bool'},
							  logfilename => { isa => 'Str' },
							  search_direction => { isa => 'Str' },
							  interval => { isa => 'Maybe[Int]' },
		);

    my $directory = $parm{'directory'};
	my $is_asrscm = $parm{'is_asrscm'};
	my $run_on_slurm = $parm{'run_on_slurm'};
	my $logfilename = $parm{'logfilename'}; 
	my $search_direction = $parm{'search_direction'}; 
	my $interval = $parm{'interval'};
	
	my ($logfileobject,$active_dir,$last_logged_modeldir,$table);
	my ($base_ofv,$search_direction_forward,$use_old_state);
	my ($status,$active_relations,$finished_relations);

	my $maxinactive = 15;
	
	if ($search_direction eq 'backward'){
		$search_direction_forward = 0;
	}else{
		$search_direction_forward = 1;
	}

	my $time_no_active = 0;
	my $stepnumber = 1;
	my $active_is_forward;
	my @active_attributes =('maxeval_exceeded');
	if ($run_on_slurm){
		push(@active_attributes,'jobid');
	}

	while (1){
		$active_dir = undef;
		$base_ofv = undef;
		$last_logged_modeldir = undef;
		
		if (-e $logfilename){
			$logfileobject = scmlogfile->new(filename => $logfilename); #no directory, logfile has full path
			if (scalar(@{$logfileobject->steps()})>0){
				($search_direction_forward,$stepnumber) = $logfileobject->is_forward_and_stepnumber();
				$last_logged_modeldir = $logfileobject->steps->[-1]->directory();
				$base_ofv = $logfileobject->current_base_ofv();
			}
		}

		($active_dir,$active_is_forward) = get_active_scmdir(basedir => $directory,
															 is_asrscm => $is_asrscm,
															 last_logged_modeldir => $last_logged_modeldir,
															 last_direction_forward => $search_direction_forward);

		unless ($search_direction_forward == $active_is_forward){
			$stepnumber = 1; 
			$search_direction_forward = $active_is_forward;
		}
		
		$status={};
		$active_relations = {};
		$finished_relations = {};
		my @output_attributes=();
		my $ofvtype;
		if (defined $active_dir){
			$time_no_active = 0;
			$status = get_scm_progress(tooldir => $active_dir);
			$use_old_state = 0;
			if (($search_direction eq 'forward') and (not $search_direction_forward)){
				$use_old_state = 1; #rename model after old state
			}
			($active_relations,$ofvtype) = process_scm_status(status => $status,
															  is_forward => $search_direction_forward,
															  base_ofv => $base_ofv,
															  use_old_state => $use_old_state,
															  attributes =>\@active_attributes);

			push(@output_attributes,('status',$ofvtype),@active_attributes);
		}else{
			$time_no_active += $interval if (defined $interval);
		}
		if (defined $logfileobject){
			$finished_relations = $logfileobject->get_statistics();
			push(@output_attributes,('tested','ok','local_min','failed','chosen_step'));
			if ($is_asrscm){
				push(@output_attributes,('stash_step','retest_step'));
			}
			if ($search_direction eq 'forward' and (not $search_direction_forward)){
				#both directions and we have switched to backward
				push(@output_attributes,'removed_backward_step');
			}
		}
		
		my $stepname = 'Forward_'.$stepnumber;
		unless ($search_direction_forward){
			$stepname = 'Backward_'.$stepnumber;
		}
		
		my $merged_statistics = scmlogfile::merge_statistics(finished_relations =>$finished_relations,
															 active_relations  =>$active_relations);
		
		my $stats_matrix = scmlogfile::get_statistics_matrix(relations => $merged_statistics,
															 attributes => \@output_attributes,
															 sort_by => $ofvtype);

		my ($header,$formatstring) = scmlogfile::get_scm_header_and_format(attributes => \@output_attributes,
																		   stepname => $stepname);

		my $text_matrix = scmlogfile::get_text_formatted_matrix(as_R => 0,
																matrix => $stats_matrix,
																header => $header);
		
		my $table =scmlogfile::text_matrix_to_string(as_R => 0,
														text_matrix => $text_matrix,
														header => $header,
														formatstring => $formatstring);
		
		print "\n\n\n\n\n".$table;
		
		if ($time_no_active > $maxinactive){
			last;
		}elsif (defined $interval){
			sleep($interval);
		}else{
			last;
		}
	}

}

sub process_scm_status
{
	my %parm = validated_hash(\@_,
							  status => { isa => 'HashRef' },
							  base_ofv => { isa => 'Maybe[Num]' },
							  is_forward => {isa => 'Bool'},
							  use_old_state => {isa => 'Bool'},
							  attributes => {isa => 'ArrayRef'},
		);
	my $status = $parm{'status'};
	my $base_ofv = $parm{'base_ofv'};
	my $is_forward = $parm{'is_forward'};
	my $use_old_state = $parm{'use_old_state'};
	my $attributes = $parm{'attributes'};

	my %active_relations = ();
	my ($parcov,$state);
	my $ofvtype = 'ofv';
	if (defined $base_ofv){
		$ofvtype='ofvdrop';
	}
	foreach my $candidate (keys %{$status}){
		$parcov = $status->{$candidate}->{'parameter'}.$status->{$candidate}->{'covariate'};
		if ($use_old_state){
			$state = $status->{$candidate}->{'oldstate'};
		}else{
			$state = $status->{$candidate}->{'state'};
		}
		$active_relations{$parcov} = {} unless (defined $active_relations{$parcov});
		$active_relations{$parcov}->{$state} = {};

		if (defined $base_ofv){
			if (defined $status->{$candidate}->{'ofv'}){
				$active_relations{$parcov}->{$state}->{$ofvtype} = $base_ofv -$status->{$candidate}->{'ofv'};
			}else{
				$active_relations{$parcov}->{$state}->{$ofvtype} = undef;
			}
		}else{
			$active_relations{$parcov}->{$state}->{$ofvtype} = $status->{$candidate}->{'ofv'};
		}

		if ($status->{$candidate}->{'finished'}){
			if (defined $active_relations{$parcov}->{$state}->{$ofvtype}){
				if (defined $base_ofv and $is_forward){
					if ($active_relations{$parcov}->{$state}->{$ofvtype} > 0) {
						$active_relations{$parcov}->{$state}->{'status'}='ok';
					}else{
						$active_relations{$parcov}->{$state}->{'status'}='localmin';
					}
				}else{
					$active_relations{$parcov}->{$state}->{'status'} = 'finished';
				}
			}else{
				$active_relations{$parcov}->{$state}->{'status'} = 'failed';
			}
		}else{
			if (defined $active_relations{$parcov}->{$state}->{$ofvtype}){
				$active_relations{$parcov}->{$state}->{'status'} = 'started';
			}else{
				$active_relations{$parcov}->{$state}->{'status'} = 'notStarted';
			}
		}

		foreach my $attr (@{$attributes}){
			unless (defined $active_relations{$parcov}->{$state}->{$attr}){
				$active_relations{$parcov}->{$state}->{$attr} = $status->{$candidate}->{$attr};
			}
		}
		
	}
	return (\%active_relations,$ofvtype);
}

sub get_active_scmdir
{
	my %parm = validated_hash(\@_,
							  basedir => { isa => 'Str' },
							  is_asrscm => {isa => 'Bool'},
							  last_logged_modeldir => { isa => 'Maybe[Str]' },
							  last_direction_forward => { isa => 'Bool' },
		);

    my $basedir = $parm{'basedir'};
	my $is_asrscm = $parm{'is_asrscm'};
    my $last_logged_modeldir = $parm{'last_logged_modeldir'};
    my $last_direction_forward = $parm{'last_direction_forward'};

	my $search_direction_forward = $last_direction_forward; 
	
	my $returndir = undef;
	if (defined $last_logged_modeldir){
		my $scmdir_below = get_scmdir1_below($last_logged_modeldir);
		if (-d $scmdir_below){
			$returndir = $scmdir_below;
		}else{
			if ($is_asrscm){
				my $newbasedir = scmlogfile::get_next_scmplus_basedir(scm_top_directory => $basedir);
				if (-d $newbasedir){
					$returndir=$newbasedir;
					if ($newbasedir =~ /backward_scm_dir1/){
						$search_direction_forward = 0;
					}
				}
			}else{
				if ($last_direction_forward){
					my $backdir = $basedir.'/backward_scm_dir1';
					my $forwarddir = $basedir.'/forward_scm_dir1';
					if (-d $backdir){
						$returndir=$backdir;
						$search_direction_forward = 0;
					}elsif(-d $forwarddir){
						$returndir=$forwarddir;
					}
				}
			}
		}
	}else{
		#first step
		$returndir = $basedir;
	}
	return ($returndir,$search_direction_forward); #can be undef if search is done
}

sub get_jobid
{
	my $dir = shift;
	my $id = undef;

	my $filename = $dir . '/jobId';
	if (-e $filename) {
		my $id;
		open(my $fh, '<', $filename) or die "cannot open file $filename";
		{
			local $/;
			$id = <$fh>;
		}
		close($fh);
		chomp $id;
	}
	return $id;
}

sub get_current_feval_ofv
{
	my $extfile = shift;
	my $feval=undef;
	my $ofv =undef;
	if (-e $extfile){
        open(my $fh, '<', $extfile); 
        while (my $line = <$fh>) {
            my @a = split ' ', $line; 
            if ($a[0] =~ /^\s*[0-9]/) {
                $feval = int($a[0]);
                $ofv = $a[-1]; 
            }
        }
		close $fh;
	}
	return $feval,$ofv;
}

sub get_line
{
    my $re = shift;
    my $path = shift;

	open(my $fh, '<', $path) or die "cannot open file $path";
    while (my $line = <$fh>) {
        if ($line =~ /$re/) {
            close($fh);
            return $line;
        }
    }
    close($fh);
    return "";
}

sub get_final_ofv
{
	my $extfile = shift;
	my $ofv =undef;
	if (-e $extfile) {
		my $line = get_line('^  -1000000000', $extfile);
		if ($line =~ / (\S+)\s*$/){
			$ofv = $1;
		}
	}
	return $ofv;
}

sub rounding_errors
{
	my $lstfile = shift;
	my $rounding = 0;
	if (-e $lstfile) {
		my $line = get_line('^ DUE TO ROUNDING ERRORS', $lstfile);
		$rounding = 1 if ($line =~ /^ DUE/);
	}
	return $rounding;
}

sub maxevals_exceeded
{
	my $lstfile = shift;
	my $exceeded = 0;
	if (-e $lstfile) {
		my $line = get_line('^ DUE TO MAX\. NO\. OF FUNCTION EVALUATIONS EXCEEDED', $lstfile);
		$exceeded = 1 if ($line =~ /^ DUE/);
	}
	return $exceeded;
}

sub get_candidate_models
{
    my $m1dir = shift;
	my $filename = $m1dir.'/done.log';

	my %mapping =();
	if (-d $m1dir and (-e $filename)){
		open(my $fh, "<", "$filename") or return \%mapping;
		while (1) {
			my $line = readline($fh);
			unless (defined $line){
				last; #reached EOF
			}
			if ($line =~ /^(\S+) (\S+) (0|1) (\d+) (\d+)/){
				$mapping{$1.$2.$5}={'parameter' => $1, 'covariate' =>$2, 
									'state' => $5, 'oldstate' => $4, 'continuous' => $3};
			}
		}
		close $fh;
	}
	return \%mapping;
}

sub get_model_NMrun_translation
{
    my $modelfitdir = shift;
	my $filename = $modelfitdir.'/model_NMrun_translation.txt';

	my %mapping =();
	if (-d $modelfitdir and (-e $filename)){
		open(my $fh, "<", "$filename") or return \%mapping;
		while (1) {
			my $line = readline($fh);
			unless (defined $line){
				last; #reached EOF
			}
			if ($line =~ /^(\S+)\s+(NM_run\d+)/){
				$mapping{$1}=$2;
			}
		}
		close $fh;
	}
	return \%mapping;
}

sub ext_file
{
	my $filename = shift;
	$filename =~ s/(mod|ctl)$/ext/;
	return $filename;
}

sub lst_file
{
	my $filename = shift;
	$filename =~ s/(mod|ctl)$/lst/;
	return $filename;
}

1;
