package scmlogfile;

# A class representing a scm log file

use Moose;
use MooseX::Params::Validate;
use scmlogstep;

has 'filename' => ( is => 'rw', isa => 'Str' );
has 'directory' => ( is => 'rw', isa => 'Str' );
has 'steps' => ( is => 'rw', isa => 'ArrayRef[scmlogstep]', default => sub { [] } );
has 'parcov_lookup' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'test_relations' => ( is => 'rw', isa => 'HashRef' );
has 'stashed_relations' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'warnings' => ( is => 'rw', isa => 'Str', default => '' );
has 'is_asrscm' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'have_forward' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'have_backward' => ( is => 'rw', isa => 'Bool', default => 0);  
has 'gof_is_pvalue' => ( is => 'rw', isa => 'Bool', default => 1);  

our $asrscmtag="adaptive scope reduction scm";
our $nextbasefilename="scmplus_current_basedir.txt";

sub BUILD
{
    my $self = shift;
	if (defined $self->test_relations){
		$self->create_parcov_lookup(test_relations => $self->test_relations);
	}

	if ((defined $self->directory) and (defined $self->filename)){
		my $filename = $self->directory().$self->filename();
		unless (-e $filename){
			my $alt = $self->directory.'/'.$self->filename; 
			if (-e $alt){
				$filename = $alt; 
			}else{
				my $help = '';
				if (length($self->directory)<1){
					$help = ' in current working directory. Did you forget to specify option -directory?'."\n";
				}else{
					$help = ' in directory '.$self->directory."\n";
				}
				die("File ".$self->filename." does not exist$help");
			}
		}
		$self->filename($filename);
	}
	
    if (defined $self->filename) {
        $self->read_scmlogfile(filename => $self->filename);
    }
	
}

sub get_scm_header_and_format
{
	my %parm = validated_hash(\@_,
							  attributes => { isa => 'ArrayRef'},
							  stepname => {isa => 'Str',optional=>1},
							  firsthead => {isa => 'Str',default=>'Model'},
		);
	my $attributes = $parm{'attributes'};
	my $stepname = $parm{'stepname'};
	my $firsthead = $parm{'firsthead'};

	my @header = ($firsthead);
	my $formatstring = "%-15s";
	
	my %lookup = ('status' => [$stepname," %12s"],
				  'tested'  => ['N_test'," %7s"],
				  'ok'  => ['N_ok'," %7s"],
				  'ofv'  => ['ofv'," %12s"],
				  'ofvdrop'  => ['ofvdrop'," %12s"],
				  'local_min'  => ['N_localmin'," %10s"],
				  'failed'  => ['N_failed'," %8s"],
				  'chosen_step'  => ['StepSelected'," %7s"],
				  'removed_backward_step' => ['BackstepRemoved'," %15s"],
				  'stash_step' =>['StepStashed'," %11s"],
				  'retest_step' =>['StepReadded'," %11s"],
				  'feval'=> ['evaluations'," %11s"],
				  'maxeval_exceeded'=> ['exceed_eval'," %11s"],
				  'jobid'=> ['jobid'," %7s"],
				  'covariate' => ['covariate'," %10s"],
				  'parameter' => ['parameter'," %10s"],
				  'state' => ['state'," %6s"],
				  'pvalue' => ['pvalue', " %7s"],
				  'BASE OFV' => ['BASEOFV'," %15s"],
				  'NEW OFV' => ['NEWOFV'," %15s"],
				  'TEST OFV (DROP)' => ['OFVDROP'," %15s"],
				  'dDF'=> ['dDF'," %5s"],
				  'PVAL' => ['PVALrun'," %10s"]);
	
	foreach my $attr (@{$attributes}){
		if (defined $lookup{$attr}){
			push(@header,$lookup{$attr}->[0]);
			$formatstring .= $lookup{$attr}->[1];
		}else{
			push(@header,$attr);
			$formatstring .= " %12s";
		}
	}
	$formatstring .= "\n";
	return (\@header,$formatstring);
	
	
}

sub text_matrix_to_string
{
	my %parm = validated_hash(\@_,
							  as_R => { isa => 'Bool'},
							  text_matrix => { isa => 'ArrayRef'},
							  header => { isa => 'ArrayRef'},
							  formatstring => {isa => 'Str'},
		);


	my $as_R = $parm{'as_R'};
	my $text_matrix = $parm{'text_matrix'};
	my $header = $parm{'header'};
	my $formatstring = $parm{'formatstring'};

	my $string = '';

	my %Rcolumns = ();

	if ($as_R){
		foreach my $head (@{$header}){
			$Rcolumns{$head} = [];
		}
		for (my $j=0; $j< scalar(@{$text_matrix}); $j++){
			for (my $i=0; $i< scalar(@{$header}); $i++){
				push(@{$Rcolumns{$header->[$i]}},$text_matrix->[$j]->[$i]);
			}
		}
		$string = 'data.frame(stringsAsFactors = FALSE';
		for (my $i=0; $i< scalar(@{$header}); $i++){
			$string .= ','."\n";
			if ($header->[$i] =~ /^(Model|Step|parameter|covariate)$/){
				my $line = $header->[$i]." = c(";
				for (my $j=0; $j< scalar(@{$Rcolumns{$header->[$i]}}); $j++){
					$line .= ',' unless ($j==0);
					if ($Rcolumns{$header->[$i]}->[$j] eq 'NA'){
						$line .= $Rcolumns{$header->[$i]}->[$j];
					}else{
						$line .= '"'.$Rcolumns{$header->[$i]}->[$j].'"';
					}
				}
				$string .= $line.')';
			}else{
				$string .= $header->[$i]." = c(".join(",",@{$Rcolumns{$header->[$i]}}).")";
			}
		}
		$string .= ')'."\n";


	}else{
		$string = sprintf($formatstring,@{$header});
		for (my $j=0; $j< scalar(@{$text_matrix}); $j++){
			$string .= sprintf($formatstring,@{$text_matrix->[$j]});

		}
	}

	return $string;
}

sub get_text_formatted_matrix
{
	my %parm = validated_hash(\@_,
							  as_R => { isa => 'Bool'},
							  matrix => { isa => 'ArrayRef'},
							  header => { isa => 'ArrayRef'},
							  decimals => {isa => 'Int', default => 2}
		);

	my $as_R = $parm{'as_R'};
	my $matrix = $parm{'matrix'};
	my $header = $parm{'header'};
	my $decimals = $parm{'decimals'};
	
	
	my @formatted=();
	foreach my $line (@{$matrix}){
		push(@formatted,[]);
		for (my $i=0; $i< scalar(@{$line}); $i++){
			if (($i>0) and ($header->[$i] =~ /(step|Step)/ )){
				push(@{$formatted[-1]},format_step('as_R' => $as_R, 'stepnum' => $line->[$i]));
			}elsif ($header->[$i] =~ /(ofv|OFV)/ ){
				push(@{$formatted[-1]},format_ofv(as_R => $as_R, ofv =>$line->[$i], decimals => $decimals));
			}elsif ($header->[$i] =~ /\b(PVALrun|PVAL)\b/ ){
				push(@{$formatted[-1]},format_pval(as_R => $as_R, pval =>$line->[$i]));
			}elsif (defined $line->[$i]){
				push(@{$formatted[-1]},$line->[$i]);
			}elsif ($as_R) {
				push(@{$formatted[-1]},'NA');
			}else{
				push(@{$formatted[-1]},'');
			}
		}
	}
	return \@formatted;	
}

sub get_next_scmplus_basedir
{
	my %parm = validated_hash(\@_,
							  scm_top_directory => { isa => 'Str'},
		);
	my $scm_top_directory = $parm{'scm_top_directory'};

	my $nextdir=undef;

	my $nextasrbasefile = $scm_top_directory.'/'.$nextbasefilename;
	if (-e $nextasrbasefile){ #this is scmplus
		open( my $fh, "<".$nextasrbasefile);
		my $line = readline($fh);
		close $fh;
		chomp $line;
		my $basedir = $line; #can be scm_dir below
	}


}

sub get_report_string
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  ofv_table => { isa => 'Bool', default => 0 },
							  as_R => { isa => 'Bool', default => 0},
							  decimals => { isa => 'Int', default => 5},
		);
    my $ofv_table = $parm{'ofv_table'};
    my $decimals = $parm{'decimals'};
    my $as_R = $parm{'as_R'};

	if ($ofv_table){
		return $self->get_summary_string(decimals => $decimals, as_R => $as_R);
	}else{
		return $self->get_statistics_string(as_R => $as_R);
	}
}

sub get_attributes_array{
	my $self = shift;

	my $attributes = ['covariate','parameter','state','BASE OFV','NEW OFV','TEST OFV (DROP)'];

	if ($self->gof_is_pvalue()){
		push(@{$attributes},'dDF','PVAL','pvalue');
	}
	return $attributes;

}

sub get_summary_string
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  include_posterior => { isa => 'Bool', default => 1 },
							  as_R => { isa => 'Bool', default => 1},
							  decimals => { isa => 'Int'}
		);
    my $include_posterior = $parm{'include_posterior'};
    my $as_R = $parm{'as_R'};
	my $decimals = $parm{'decimals'};

	my $include_direction = 1;

	my $attributes = $self->get_attributes_array();
	
	my $matrix = $self->get_summary_matrix(include_direction => $include_direction,
										   include_posterior => $include_posterior,
										   attributes => $attributes);

	my ($header,$formatstring) = get_scm_header_and_format(attributes => $attributes,
														   firsthead => 'Step');

	my $text_matrix = get_text_formatted_matrix(as_R => $as_R,
												matrix => $matrix,
												header => $header,
												decimals => $decimals);
		
	my $table =text_matrix_to_string(as_R => $as_R,
									 text_matrix => $text_matrix,
									 header => $header,
									 formatstring => $formatstring);
	
	return $table;
	
}

sub get_statistics_string
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  as_R => { isa => 'Bool', default => 1},
		);
	my $as_R = $parm{'as_R'};

	my $relations = $self->get_statistics();
	my $have_forward = 0;
	my $have_backward = 0;
	if (scalar(@{$self->steps()})>0){
		$have_forward = $self->steps->[0]->is_forward();
		$have_backward = (not $self->steps->[-1]->is_forward());
	}
	my @output_attributes = ('tested','ok','local_min','failed','chosen_step');
	push(@output_attributes,'stash_step','retest_step') if ($self->is_asrscm());
	push(@output_attributes,'removed_backward_step') if ($have_forward and $have_backward);

	my $merged_statistics = merge_statistics(finished_relations =>$relations,
											 active_relations  =>{});
		
	my $stats_matrix = get_statistics_matrix(relations => $merged_statistics,
											 attributes => \@output_attributes);

	my ($header,$formatstring) = get_scm_header_and_format(attributes => \@output_attributes,
														   firsthead => 'Model');

	my $text_matrix = get_text_formatted_matrix(as_R => $as_R,
												 matrix => $stats_matrix,
												 header => $header);
		
	my $table =text_matrix_to_string(as_R => $as_R,
									 text_matrix => $text_matrix,
									 header => $header,
									 formatstring => $formatstring);
	
	return $table;
}

sub format_ofv
{
	my %parm = validated_hash(\@_,
							  as_R => { isa => 'Bool'},
							  ofv => { isa => 'Maybe[Num]'},
							  decimals => {isa => 'Int'}
		);
	my $as_R = $parm{'as_R'};
	my $ofv = $parm{'ofv'};
	my $decimals = $parm{'decimals'};
	if (not defined $ofv){
		if ($as_R){
			return 'NA';
		}else{
			return '-';
		}
	}else{
		return sprintf("%.".$decimals."f",$ofv);
	}
}

sub format_pval
{
	my %parm = validated_hash(\@_,
							  as_R => { isa => 'Bool'},
							  pval => { isa => 'Maybe[Str]'}
		);
	my $as_R = $parm{'as_R'};
	my $pval = $parm{'pval'};
	if (not defined $pval){
		if ($as_R){
			return 'NA';
		}else{
			return '-';
		}
#	}elsif (($pval == '999') or ($pval == '9999')){
#		if ($as_R){
#			return 'NA';
#		}else{
#			return '-';
#		}
	}else{
		return $pval;
	}
}

sub is_forward_and_stepnumber
{
	my $self = shift;

	my $is_forward = $self->steps()->[-1]->is_forward(); #is_forward, stepnumber
	my $stepnum = 1;
	for (my $i=1; $i <= scalar(@{$self->steps}); $i++){
		if ($self->steps()->[-$i]->is_forward == $is_forward){
			$stepnum++;
		}else{
			last;
		}
	}
	return ($is_forward,$stepnum);
}

sub current_base_ofv
{
	my $self = shift;
	my $base_ofv =undef;
	for (my $i=1; $i <= scalar(@{$self->steps}); $i++){
		if (defined $self->steps()->[-$i]->chosen_model_ofv()){
			$base_ofv = $self->steps()->[-$i]->chosen_model_ofv();
			last;
		}
	}
	return $base_ofv;

}

sub current_base_phi_file
{
	my $self = shift;
	my $base_phi =undef;
	for (my $i=1; $i <= scalar(@{$self->steps}); $i++){
		if (defined $self->steps()->[-$i]->chosen_index()){
			my $index=$self->steps()->[-$i]->chosen_index();
			my $phi = $self->steps()->[-$i]->directory().'/'.
				$self->steps()->[-$i]->candidates()->[$index]->{'parcov'}.
				$self->steps()->[-$i]->candidates()->[$index]->{'state'}.'.phi';
			if (-e $phi){
				$base_phi = $phi;
			}else{
				print "\n cannot find $phi\n";
			}
			last;
		}
	}
	return $base_phi;
}

sub format_step
{
	my %parm = validated_hash(\@_,
							  as_R => { isa => 'Bool'},
							  stepnum => { isa => 'Maybe[Int]'},
		);
	my $as_R = $parm{'as_R'};
	my $stepnum = $parm{'stepnum'};
	if ((not defined $stepnum) or ($stepnum == 0)){
		if ($as_R){
			return 'NA';
		}else{
			return '-';
		}
	}else{
		return $stepnum;
	}
}

sub get_initial_statistics
{
	return {'tested' => 0, 'ok' => 0, 'failed' => 0, 'local_min' => 0, 
			'chosen_step' => 0,'stash_step' => 0, 'retest_step' => 0,'removed_backward_step' => 0 };
}

sub get_ordered_parcov_state
{
	#static
	my %parm = validated_hash(\@_,
							  relations =>{isa => 'HashRef'},
							  attributes => {isa => 'ArrayRef'},
							  sort_by => {isa => 'Maybe[Str]', optional => 1} #$ofvtype
		);
    my $relations = $parm{'relations'};
    my $attributes = $parm{'attributes'};
	my $sort_by = $parm{'sort_by'};

	my $bignum=1000000000;
	my $decreasing = 0;
	if (defined $sort_by){
		if (lc($sort_by) =~ /drop/){
			$decreasing = 1;
			$bignum= -1000000000;
		}
	}
	my @parcov_state=();
	
	foreach my $parcov (sort { lc($a) cmp lc($b) } (keys %{$relations}) ){
		foreach my $state (sort { $a <=> $b } keys %{$relations->{$parcov}}) {
			push(@parcov_state,[$parcov,$state]);
			if (defined $sort_by){
				if (defined $relations->{$parcov}->{$state}->{$sort_by}){
					push(@{$parcov_state[-1]},$relations->{$parcov}->{$state}->{$sort_by});
				}else{
					push(@{$parcov_state[-1]},$bignum);
				}
			}
		}
	}
	if (defined $sort_by){
		return sort_on_last_column(\@parcov_state,$decreasing);
	}else{
		return \@parcov_state;
	}
}

sub sort_on_last_column
{
	my $matrix = shift;
	my $decreasing = shift;
	my @sorted;
	if ($decreasing){
		@sorted = sort {$b->[-1] <=> $a->[-1]} (@{$matrix});
	}else{
		@sorted = sort {$a->[-1] <=> $b->[-1]} (@{$matrix});
	}
	return \@sorted;
}

sub get_statistics_matrix
{
	#static
	my %parm = validated_hash(\@_,
							  relations =>{isa => 'HashRef'},
							  attributes => {isa => 'ArrayRef'},
							  sort_by => {isa => 'Maybe[Str]', optional => 1} #$ofvtype
		);
    my $relations = $parm{'relations'};
    my $attributes = $parm{'attributes'};
	my $sort_by = $parm{'sort_by'};

	my $ordered_parcov_state = get_ordered_parcov_state(relations => $relations,
														attributes => $attributes,
														sort_by => $sort_by);
	
	my @statistics =();
	my $parcov;
	my $state;
	foreach my $item (@{$ordered_parcov_state}){
		$parcov = $item->[0];
		$state = $item->[1];
		push(@statistics,[$parcov.'-'.$state]);
		foreach my $attr (@{$attributes}){
			if (defined $relations->{$parcov}->{$state}->{$attr}){
				push(@{$statistics[-1]},$relations->{$parcov}->{$state}->{$attr});
			}else{
				push(@{$statistics[-1]},undef);
			}
		}
	}
	return \@statistics;
}
	
sub merge_statistics
{
	#static
	my %parm = validated_hash(\@_,
							  finished_relations =>{isa => 'HashRef'},
							  active_relations  =>{isa => 'HashRef'},
		);
    my $finished_relations = $parm{'finished_relations'};
    my $active_relations = $parm{'active_relations'};

	my %merged_statistics = ();

	foreach my $parcov (sort { lc($a) cmp lc($b) } (keys %{$finished_relations})){
		$merged_statistics{$parcov} = {} unless (defined $merged_statistics{$parcov}); 
		foreach my $state (sort { $a <=> $b } keys %{$finished_relations->{$parcov}}) {
			$merged_statistics{$parcov}->{$state} = {} unless (defined $merged_statistics{$parcov}->{$state});
			foreach my $attr (keys %{$finished_relations->{$parcov}->{$state}}){
				$merged_statistics{$parcov}->{$state}->{$attr} =
					$finished_relations->{$parcov}->{$state}->{$attr};
			}
		}
	}
	foreach my $parcov (sort { lc($a) cmp lc($b) } (keys %{$active_relations})){
		$merged_statistics{$parcov} = {} unless (defined $merged_statistics{$parcov}); 
		foreach my $state (sort { $a <=> $b } keys %{$active_relations->{$parcov}}) {
			$merged_statistics{$parcov}->{$state} = {} unless (defined $merged_statistics{$parcov}->{$state});
			foreach my $attr (keys %{$active_relations->{$parcov}->{$state}}){
				$merged_statistics{$parcov}->{$state}->{$attr} =
					$active_relations->{$parcov}->{$state}->{$attr};
			}
		}
	}

	return \%merged_statistics;
}

sub get_statistics
{
	my $self = shift;
	
	my %relations=();

	my $final_index=-1; #previous step with any defined chosen_index

	my $stepnum=0;
	for (my $index=0; $index < scalar(@{$self->steps()}); $index++){
		next unless ($self->steps()->[$index]->is_forward() == $self->have_forward());
		$stepnum++;

		if (defined $self->steps()->[$index]->chosen_index){
			$final_index = $index;
		}
		foreach my $candidate (@{$self->steps()->[$index]->candidates}){
			my $parcov = $candidate->{'parcov'}; 
			my $state = $candidate->{'state'};
			$relations{$parcov} = {} unless (defined $relations{$parcov});
			unless (defined $relations{$parcov}->{$state}) {
				$relations{$parcov}->{$state} = get_initial_statistics();
			}
			if ($candidate->{'local_min'}){
				$relations{$parcov}->{$state}->{'local_min'}++;
			}elsif ($candidate->{'failed'}){
				$relations{$parcov}->{$state}->{'failed'}++;
			}else{
				$relations{$parcov}->{$state}->{'ok'}++;
			}
			if ($candidate->{'chosen'}){
				$relations{$parcov}->{$state}->{'chosen_step'} = $stepnum;
			}
			$relations{$parcov}->{$state}->{'tested'}++;
		}
	}
	if ($self->have_forward() and $self->have_backward() and ($final_index >= 0)){
		$stepnum=0;
		for (my $index=0; $index < scalar(@{$self->steps()}); $index++){
			next unless ($self->steps()->[$index]->is_forward() == 0);
			$stepnum++;

			if (defined $self->steps()->[$index]->chosen_index){
				set_removed_backward(steps => $self->steps,
									 stepnum => $stepnum,
									 relations => \%relations,
									 old_chosen_index => $final_index,
									 new_chosen_index => $index);
				$final_index = $index;
			}
		}
	}
	if($self->is_asrscm and $self->have_forward()){
		foreach my $param (keys %{$self->stashed_relations}){
			foreach my $cov (keys %{$self->stashed_relations->{$param}}){
				unless (defined $relations{$param.$cov}){
					die("have stashed relation $param-$cov that is not in tested relations");
				}
				foreach my $state (keys %{$relations{$param.$cov}}){
					$relations{$param.$cov}->{$state}->{'stash_step'} = $self->stashed_relations->{$param}->{$cov}->{'step'};
					$relations{$param.$cov}->{$state}->{'retest_step'} = $self->stashed_relations->{$param}->{$cov}->{'retest'};
				}
			}
		}
	}
	return \%relations;
}

sub set_removed_backward
{
	my %parm = validated_hash(\@_,
							  relations =>{isa => 'HashRef'},
							  old_chosen_index => { isa => 'Int'},
							  new_chosen_index => { isa => 'Int'},
							  stepnum => { isa => 'Int'},
							  steps => { isa => 'ArrayRef'},
		);
    my $relations = $parm{'relations'};
    my $old_chosen_index = $parm{'old_chosen_index'};
    my $new_chosen_index = $parm{'new_chosen_index'};
    my $stepnum = $parm{'stepnum'};
    my $steps = $parm{'steps'};

	for (my $i=0; $i< scalar(@{$steps->[$old_chosen_index]->posterior_included_relations}); $i++){
		my $found = 0;
		for (my $j=0; $j< scalar(@{$steps->[$new_chosen_index]->posterior_included_relations}); $j++){
			if (($steps->[$old_chosen_index]->posterior_included_relations()->[$i]->{'parameter'} eq
				$steps->[$new_chosen_index]->posterior_included_relations()->[$j]->{'parameter'}) and
				($steps->[$old_chosen_index]->posterior_included_relations()->[$i]->{'covariate'} eq
				 $steps->[$new_chosen_index]->posterior_included_relations()->[$j]->{'covariate'}) and
				($steps->[$old_chosen_index]->posterior_included_relations()->[$i]->{'state'} eq
				 $steps->[$new_chosen_index]->posterior_included_relations()->[$j]->{'state'})){
				$found = 1;
				last;
			}
		}
		unless ($found){
			my $parcov = $steps->[$old_chosen_index]->posterior_included_relations()->[$i]->{'parameter'}.
				$steps->[$old_chosen_index]->posterior_included_relations()->[$i]->{'covariate'};
			my $state = $steps->[$old_chosen_index]->posterior_included_relations()->[$i]->{'state'};
	
			unless (defined $relations->{$parcov}->{$state}) {
				$relations->{$parcov}->{$state} = {'tested' => 0, 'ok' => 0, 'failed' => 0, 'local_min' => 0, 
												   'chosen_step' => 0,'stash_step' => 0, 'retest_step' => 0 };
			}
			$relations->{$parcov}->{$state}->{'removed_backward_step'} = $stepnum;
		}
	}
}

sub get_summary_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  include_direction => { isa => 'Bool', default => 1 },
							  include_posterior => { isa => 'Bool', default => 1 },
							  attributes => { isa => 'ArrayRef'},
		);
    my $include_direction = $parm{'include_direction'};
    my $include_posterior = $parm{'include_posterior'};
    my $attributes = $parm{'attributes'};

	my $counter = 0;
	my $old_direction_is_forward = 1;
	my $final_index=0; #last step with any defined chosen_index

	my @lines=();
	my $last_step_index = scalar(@{$self->steps()}) -1;
	
	for (my $index=0; $index <= $last_step_index; $index++){
		if ($self->steps()->[$index]->is_forward() == $old_direction_is_forward){
			$counter++;
		}else{
			$counter=1;
			$old_direction_is_forward = $self->steps()->[$index]->is_forward();
		}
		if (defined $self->steps()->[$index]->chosen_index){
			$final_index = $index;
		}
		push(@lines,@{$self->steps->[$index]->get_summary(counter=> $counter,
														  include_direction => $include_direction,
														  summarize_posterior => 0,
														  attributes => $attributes)});
	}
	if ($include_posterior){
		if ($self->steps()->[$last_step_index]->is_forward()){
			push(@lines,@{$self->steps->[$final_index]->get_summary(include_direction => $include_direction,
																	attributes => $attributes,
																	summarize_posterior => 1)});
		}else{
			#Transfer information from next to last step to last step, if necessary
			$self->prepare_summarize_notchosen(final_index => $final_index,
											   last_step_index => $last_step_index);
			push(@lines,@{$self->steps->[$last_step_index]->get_summary(include_direction => $include_direction,
																	attributes => $attributes,
																	summarize_posterior => 0,
																	summarize_notchosen => 1)});
		}

	}
	return \@lines;
}

sub prepare_summarize_notchosen
{
    my $self = shift;
	my %parm = validated_hash(\@_,
							  final_index => { isa => 'Int', optional => 0 },
							  last_step_index => { isa => 'Int', optional => 0 },
		);

	my $final_index = $parm{'final_index'};
	my $last_step_index = $parm{'last_step_index'};
	return if ($self->steps()->[$last_step_index]->is_forward());
	

	my $is_prior = ($last_step_index == ($final_index+1));
					
	my $lookup = $self->steps->[$final_index]->get_posterior_parcov_lookup();
	$self->steps->[$last_step_index]->update_parameter_covariate_from_prior(parcov_lookup => $lookup,
																			is_prior => $is_prior);


	if ($is_prior){
		if((not defined $self->steps->[$last_step_index]->base_model_ofv) and
		   (defined $self->steps->[$final_index]->chosen_model_ofv)){
			#handle any merged ofv
			$self->steps->[$last_step_index]->base_model_ofv($self->steps->[$final_index]->chosen_model_ofv());
			$self->steps->[$last_step_index]->split_merged_ofv();
		}
	}
	
}

sub get_dropped_relations
{
    my $self = shift;
	my %parm = validated_hash(\@_,
							  keep_local_min => { isa => 'Bool', optional => 0 },
							  keep_failed => { isa => 'Bool', optional => 0 },
							  p_cutoff => { isa => 'Num', default => 0 },
							  step => { isa => 'Int', default => -1 },
		);
    my $keep_failed = $parm{'keep_failed'};
    my $keep_local_min = $parm{'keep_local_min'};
    my $p_cutoff = $parm{'p_cutoff'};
    my $step = $parm{'step'};

	return $self->steps->[$step]->get_dropped_relations(p_cutoff => $p_cutoff, 
														keep_local_min => $keep_local_min,
														keep_failed => $keep_failed);
}

sub create_parcov_lookup
{
    my $self = shift;
	my %parm = validated_hash(\@_,
		test_relations => { isa => 'HashRef' },
	);
    my $test_relations = $parm{'test_relations'};

	$self->test_relations($test_relations);

	my %hash=();
	foreach my $parameter (keys %{$test_relations}){
		foreach my $covariate (@{$test_relations->{$parameter}}){
			$hash{$parameter.$covariate} = [$parameter,$covariate];
		}
	}
	$self->parcov_lookup(\%hash);
}

sub add_scmlogstep
{
    my $self = shift;
	push(@{$self->steps},shift);
	$self->steps->[-1]->split_merged_ofv();
	if ($self->steps->[-1]->is_forward()){
		$self->have_forward(1);
	}else{
		$self->have_backward(1);
	}
}

sub update_stashed_relations
{
    my $self = shift;
	my %parm = validated_hash(\@_,
							  text => { isa => 'Str' },
							  add => { isa => 'Bool' },
		);
	my $text = $parm{'text'};
	my $add = $parm{'add'};

	my $list = parse_relations_list(text => $text);
	if ($add){
		my $stepnum = scalar(@{$self->steps}); #retest line comes after actual step is added
		foreach my $relation (@{$list}){
			$self->stashed_relations()->{$relation->{'parameter'}} = {} unless (defined $self->stashed_relations()->{$relation->{'parameter'}}); 
			if (defined $self->stashed_relations()->{$relation->{'parameter'}}->{$relation->{'covariate'}}){
				my $message = " Stashed relation ".$relation->{'parameter'}."-".$relation->{'covariate'}." more than once";
				$self->warnings($self->warnings().$message);
				print $message."\n";
			}else{
				$self->stashed_relations()->{$relation->{'parameter'}}->{$relation->{'covariate'}}={'step' => $stepnum, 'retest'=> 0};
			}
		}
	}else{
		my $stepnum = scalar(@{$self->steps})+1; #retest line comes before actual step is added
		foreach my $relation (@{$list}){
			if (defined $self->stashed_relations()->{$relation->{'parameter'}}->{$relation->{'covariate'}}){
				$self->stashed_relations()->{$relation->{'parameter'}}->{$relation->{'covariate'}}->{'retest'} = $stepnum;
			}else{
				my $message = " Cannot find retested relation ".$relation->{'parameter'}."-".$relation->{'covariate'}." among stashed";
				$self->warnings($self->warnings().$message);
				print $message."\n";
			}
			
		}
	}
}

sub parse_relations_list
{
	my %parm = validated_hash(\@_,
							  text => { isa => 'Str' }
		);
	my $text = $parm{'text'};

	my @relations=();
	foreach my $pair (split(',',$text)){
		$pair =~ s/\s+//g;
		next unless ($pair =~ /\w-\w/);
		my ($parameter,$covariate) = split('-',$pair,2);
		push(@relations,{'parameter' => $parameter,'covariate' => $covariate,'parcov' => $parameter.$covariate});
	}
	return \@relations;
		
}

sub read_scmlogfile
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		filename => { isa => 'Str' },
	);
    my $filename = $parm{'filename'};

    $self->filename($filename);

	open(my $fh, "<", "$filename") or die("Could not open logfile $filename\n");
	
  LOGFILE: while (1) {
	  my $line = readline($fh);
	  unless (defined $line){
		  last; #reached EOF
	  }
	  
	  if ($line =~ /^Taking a step forward in $asrscmtag after reducing scope with (\d+) relations :\s+(\S*)$/ ){
		  $self->is_asrscm(1);
		  if ($1 > 0){
			  $self->update_stashed_relations(text => $2,add => 1);
		  }
	  }elsif($line =~ /^Using user-defined ofv change criteria/){
		  $self->gof_is_pvalue(0)
	  }elsif($line =~ /^Re-testing (\d+) relations after $asrscmtag reduced forward search :\s+(\S*)$/){
		  $self->is_asrscm(1);
		  if ($1 > 0){
			  $self->update_stashed_relations(text => $2, add => 0);
		  }
	  }elsif($line =~ /^Starting $asrscmtag backward search/){
		  $self->is_asrscm(1);
	  }elsif ($line =~ /^Model directory\s+([^\s]+)/){
		  my $logstep = scmlogstep->new(directory => $1);
		  my $row;
		  
        STEP: while (1) {
            $row = readline($fh);
            if (not defined $row) {
				$self->add_scmlogstep($logstep);
                last LOGFILE;
            }elsif ($row =~ /^Parameter-covariate relation chosen in this (backward|forward) step:/){
				$logstep->set_chosen(line=> $row);
            }elsif ($row =~ /^CRITERION\s+PVAL\s+[><]\s+([0-9\.]+)/){
				$logstep->p_value($1);
            }elsif ($row =~ /^BASE_MODEL_OFV\s*([\-0-9\.]+)/){
				$logstep->base_model_ofv($1);
            }elsif ($row =~ /^CHOSEN_MODEL_OFV\s*([\-0-9\.]+)/){
				$logstep->chosen_model_ofv($1);
			}elsif($row =~ /^Relations included after this step/){
				my $parline;
			  INCLUDED: while (1) {
				  $parline = readline($fh);
				  if(not defined $parline){
					  $self->add_scmlogstep($logstep);
					  last LOGFILE;
				  }elsif ($parline =~ /^----------------/){
					  $self->add_scmlogstep($logstep);
					  last STEP;
				  }else{
					  $logstep->add_posterior(line => $parline);
				  }
			  }
			}elsif($row =~ /^MODEL\s+TEST\s+/){
				$logstep->parse_header(header => $row);
				my $candidate;
			  CANDIDATE: while (1) {
				  $candidate = readline($fh);
				  if (not defined $candidate) {
					  $self->add_scmlogstep($logstep);
					  last LOGFILE;
				  }elsif ($candidate =~ /^\s*$/){
					  last CANDIDATE;
				  }else{
					  $logstep->add_candidate(line => $candidate,
											  parcov_lookup => $self->parcov_lookup());
				  }
			  }
			}elsif($row =~ /^----------------/){
				$self->add_scmlogstep($logstep);
				last STEP;
			}


		}

	  }
  }
	close $fh;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
