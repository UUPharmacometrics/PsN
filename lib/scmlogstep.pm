package scmlogstep;

# A class representing a single step
use Moose;
use MooseX::Params::Validate;

has 'candidates' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'directory' => ( is => 'rw', isa => 'Str');  
has 'is_forward' => ( is => 'rw', isa => 'Bool', default => 1);  
has 'gof_is_pvalue' => ( is => 'rw', isa => 'Bool', default => 1);  
has 'p_value' => ( is => 'rw', isa => 'Num');  # threshold p_value
has 'base_model_ofv' => ( is => 'rw', isa => 'Str'); #we use it in pattern matching  
has 'chosen_model_ofv' => ( is => 'rw', isa => 'Num'); 
has 'chosen_index' => ( is => 'rw', isa => 'Int');  
has 'posterior_included_relations' => ( is => 'rw', isa => 'ArrayRef', default => sub{[]});  
has 'header' => ( is => 'rw',isa => 'ArrayRef',default => sub {['MODEL','TEST','BASE OFV','NEW OFV','TEST OFV (DROP)','GOAL','dDF','SIGNIFICANT','PVAL']});
has 'column_index' => ( is => 'rw',isa => 'ArrayRef',default => sub {[0,1,2,3,4,6,7,-2,-1]});

sub split_merged_ofv
{
	my $self = shift;

	return unless (defined $self->base_model_ofv);

	my $base= $self->base_model_ofv;
	for (my $i=0; $i< scalar(@{$self->candidates}); $i++){
		next unless (defined $self->candidates->[$i]->{'merged_ofv'} and (not defined $self->candidates->[$i]->{'BASE OFV'}));
		if ($self->candidates->[$i]->{'merged_ofv'} =~ s/^$base//){
			$self->candidates->[$i]->{'NEW OFV'} = $self->candidates->[$i]->{'merged_ofv'};
			$self->candidates->[$i]->{'BASE OFV'} = $base;
			$self->candidates->[$i]->{'merged_ofv'} = undef;
		}
	}
}

sub add_posterior
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		line => { isa => 'Str' },
	);
    my $line = $parm{'line'};
	chomp $line;
	my @fields = split('\s+',$line); #FIXME will not work if parameter itself has 8 or more characters
	if (scalar(@fields)>0){
		my $parameter=$fields[0];
		for (my $i=1; $i<scalar(@fields); $i++){
			while ($fields[$i] =~ s/^([A-Za-z_0-9]+)-(\d+)//){
				push(@{$self->posterior_included_relations},{'parameter' => $parameter,'covariate' => $1,'state' => $2});
			}
		}
	}

}

sub get_posterior_parcov_lookup
{
	my $self = shift;

	my %hash=();
	foreach my $posterior (@{$self->posterior_included_relations}){
		my $parameter = $posterior->{'parameter'};
		my $covariate = $posterior->{'covariate'};
		if (defined $hash{$parameter.$covariate}){
			#two instances of same parameter covariate, different state
			#ambiguous state due to parallel states. Set state to undef
			$hash{$parameter.$covariate} = [$parameter,$covariate,undef];
		}else{
			$hash{$parameter.$covariate} = [$parameter,$covariate,$posterior->{'state'}];
		}
	}	

	return \%hash;
}

sub update_parameter_covariate_from_prior
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  parcov_lookup => { isa => 'HashRef' },
							  is_prior => { isa => 'Bool' }
	);
	my $parcov_lookup = $parm{'parcov_lookup'};
	my $is_prior = $parm{'is_prior'};
	
	for (my $i=0; $i < scalar(@{$self->candidates()}); $i++) {
		next if (defined $self->candidates()->[$i]->{'parameter'});
		next if (defined $self->candidates()->[$i]->{'covariate'});
		my $parcov = $self->candidates()->[$i]->{'parcov'};
		if ((defined $parcov) and (defined $parcov_lookup->{$parcov})){
			$self->candidates()->[$i]->{'parameter'} = $parcov_lookup->{$parcov}->[0];
			$self->candidates()->[$i]->{'covariate'} = $parcov_lookup->{$parcov}->[1];
			if ($is_prior){
				$self->candidates()->[$i]->{'prior_state'} = $parcov_lookup->{$parcov}->[2];
			}
		}
	}

	
}

sub summarize_candidate
{
	#static
	my %parm = validated_hash(\@_,
							  candidate => { isa => 'HashRef', optional => 0 },
							  attributes => { isa => 'ArrayRef'},
		);
    my $candidate = $parm{'candidate'};
    my $attributes = $parm{'attributes'};

	my @array = ();
	foreach my $attribute (@{$attributes}){
		if (defined $candidate->{$attribute}){
			push(@array,$candidate->{$attribute});
		}else{
			push(@array,undef);
		}
	}
	return \@array;
}

sub get_summary
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  include_direction => { isa => 'Bool', default => 1 },
							  summarize_posterior => { isa => 'Bool', default => 0 },
							  summarize_notchosen => { isa => 'Bool', default => 0 },
							  counter => { isa => 'Int', default => 1},
							  attributes => { isa => 'ArrayRef'}
	);
    my $include_direction = $parm{'include_direction'};
    my $summarize_posterior = $parm{'summarize_posterior'};
    my $summarize_notchosen = $parm{'summarize_notchosen'};
    my $counter = $parm{'counter'};
    my $attributes = $parm{'attributes'};

	my @local_attributes = @{$attributes};
	
	my $set_pvalue_index = -1;
	if (not $summarize_posterior){
		for (my $i=0; $i < scalar(@{$attributes}); $i++) {
			if ($attributes->[$i] eq 'pvalue'){
				$set_pvalue_index = $i;
				last unless ($summarize_notchosen);
			}
			if ($summarize_notchosen and ($attributes->[$i] eq 'state')){
				$local_attributes[$i]= 'prior_state';
			}
		}
	}
	
	my @firstcolumns = ();		
	if ($include_direction){
		if ($summarize_posterior or $summarize_notchosen){
			push(@firstcolumns,'Final included');
		}else{
			if ($self->is_forward){
				push(@firstcolumns, 'Forward '.$counter);
			}else{
				push(@firstcolumns, 'Backward '.$counter);
			}
		}
	}
	
	my @candidates = ();
	if ($summarize_posterior){
		push(@candidates,@{$self->posterior_included_relations});
	}elsif ($summarize_notchosen){
		for (my $i=0; $i < scalar(@{$self->candidates()}); $i++) {
			next if ((defined $self->chosen_index()) and  ($i == $self->chosen_index()));
			push(@candidates,$self->candidates()->[$i]);
		}
	}else{
		if (defined $self->chosen_index){
			push(@candidates,$self->candidates()->[$self->chosen_index()]);
		}else{
			push(@candidates,{});
		}
	}
	
	my @array=();
	foreach my $candidate (@candidates){
		push(@array,[]);
		push(@{$array[-1]},@firstcolumns);
		my @candidate_row = @{summarize_candidate(candidate => $candidate,attributes => \@local_attributes )};
		if ($set_pvalue_index >= 0){
			$candidate_row[$set_pvalue_index] = $self->p_value();
		}
		push(@{$array[-1]},@candidate_row);
	}
		
	return \@array;
}

sub set_chosen
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		line => { isa => 'Str' },
	);
    my $line = $parm{'line'};
    
	if ($line =~ /^Parameter-covariate relation chosen in this (backward|forward) step: ([^-]+)-([^-]+)-(\d+)/){
		if (($1 eq 'backward') && $self->is_forward){
			die "header indicated forward step but chosen line indicates backward";
		}elsif (($1 eq 'forward') && (not $self->is_forward)){
			die "header indicated backward step but chosen line indicates forward";
		}
		my $parameter = $2;
		my $covariate = $3;
		my $state = $4;
		for (my $i=0; $i < scalar(@{$self->candidates}); $i++) {
			if ((defined $self->candidates()->[$i]->{'parameter'}) and
				($self->candidates()->[$i]->{'parameter'} eq $parameter) and 
				($self->candidates()->[$i]->{'covariate'} eq $covariate) and
				($self->candidates()->[$i]->{'state'} == $state)){
				$self->candidates()->[$i]->{'chosen'} = 1;
				$self->chosen_index($i);
				last;
			}
		}
		unless (defined $self->chosen_index()){
			#assume no parcov lookup
			for (my $i=0; $i < scalar(@{$self->candidates}); $i++) {
				if ((defined $self->candidates()->[$i]->{'parcov'}) and
					($self->candidates()->[$i]->{'parcov'} eq $parameter.$covariate) and 
					($self->candidates()->[$i]->{'state'} == $state)){
					$self->candidates()->[$i]->{'chosen'} = 1;
					$self->candidates()->[$i]->{'parameter'} = $parameter; #now we know parameter and covariate
					$self->candidates()->[$i]->{'covariate'} = $covariate;
					$self->chosen_index($i);
					last;
				}
			}
		}
		unless (defined $self->chosen_index()){
			die("could not find $parameter-$covariate-$state");
		}
	}
}

sub get_dropped_relations
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  p_cutoff => {isa => 'Num'},
							  keep_local_min => {isa => 'Bool'},
							  keep_failed => {isa => 'Bool'},
	);
	my $p_cutoff = $parm{'p_cutoff'};
	my $keep_local_min = $parm{'keep_local_min'};
	my $keep_failed = $parm{'keep_failed'};

	my %dropped_relations =();
	my $have_failed = 0;
	my $have_bad = 0;
	foreach my $candidate (@{$self->candidates}){
		$have_failed = 1 if ($candidate->{'failed'});
		if (($candidate->{'PVAL'} < $p_cutoff) or
			$candidate->{'chosen'} or
			($keep_local_min and $candidate->{'local_min'}) or
			($keep_failed and $candidate->{'failed'})
			){
			next;
		}else{
			$dropped_relations{$candidate->{'parameter'}}->{$candidate->{'covariate'}}=1;
			$have_bad++;
		}
	}
	return ($have_failed,$have_bad,\%dropped_relations);
}

sub add_candidate
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  line => { isa => 'Str' },
							  parcov_lookup => {isa => 'HashRef'},
	);
    my $line = $parm{'line'};
	my $parcov_lookup = $parm{'parcov_lookup'};

	my $candidate = $self->_parse_candidate(line => $line);
	if (defined $parcov_lookup->{$candidate->{'parcov'}}){
		$candidate->{'parameter'} = $parcov_lookup->{$candidate->{'parcov'}}->[0];
		$candidate->{'covariate'} = $parcov_lookup->{$candidate->{'parcov'}}->[1];
	}
	$candidate->{'chosen'} = 0;
	push(@{$self->candidates},$candidate);
}

sub parse_header
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		header => { isa => 'Str' },
	);
    my $header = $parm{'header'};

	if ($header =~ /^MODEL\s+TEST\s+BASE\s+OFV\s+NEW\s+OFV\s+TEST\s+OFV/){
		$self->gof_is_pvalue(1);
		$self->header(['MODEL','TEST','BASE OFV','NEW OFV','TEST OFV (DROP)','GOAL','dDF','SIGNIFICANT','PVAL']);
		$self->column_index([0,1,2,3,4,6,7,-2,-1]);
		if ($header =~ / INSIGNIFICANT /){
			$self->is_forward(0);
		}elsif($header =~ / SIGNIFICANT /){
			$self->is_forward(1);
		}else{
			die("unrecognized pval scmlog header $header");
		}
	}elsif($header =~ /^MODEL\s+TEST\s+NAME\s+BASE\s+VAL\s+NEW\s+VAL\s+/){
		$self->gof_is_pvalue(0);
		$self->header(['MODEL','TEST NAME','BASE OFV','NEW OFV','TEST OFV (DROP)','GOAL','SIGNIFICANT']);
		$self->column_index([0,1,2,3,4,6,7]);

		if ($header =~ / \(IN\)SIGNIFICANT/){
			$self->is_forward(0);
		}elsif($header =~ / SIGNIFICANT/){
			$self->is_forward(1);
		}else{
			die("unrecognized ofv scmlog header $header");
		}
	}else{
		die("unrecognized scmlog header $header");
	}
}

sub _parse_candidate
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		line => { isa => 'Str' },
	);
    my $line = $parm{'line'};
	chomp($line);

	my @fields = split('\s+',$line);
	my %res=();
	for (my $i=0; $i < scalar(@{$self->header}); $i++){
		if (($self->header->[$i] eq 'BASE OFV') and ($fields[$self->column_index->[$i]] =~ /\.\d+\-?\d+\./)){
			$res{'merged_ofv'} = $fields[$self->column_index->[$i]];
		}elsif (defined $res{'merged_ofv'} and ($self->column_index->[$i]>0)){
			next if ($self->header->[$i] eq 'NEW OFV');
			$res{$self->header->[$i]} = $fields[($self->column_index->[$i])-1];
		}else{
			$res{$self->header->[$i]} = $fields[($self->column_index->[$i])];
		}
	}
	$res{'SIGNIFICANT'} = ((defined $res{'SIGNIFICANT'}) and $res{'SIGNIFICANT'} eq 'YES!') ? 1:0;
	if ((defined $res{'NEW OFV'}) and $res{'NEW OFV'} eq 'FAILED'){
		$res{'failed'} = 1;
		$res{'local_min'} = 0;
	}else{
		$res{'failed'} = 0;
		if (($self->gof_is_pvalue and ($res{'PVAL'} == 9999 or $res{'PVAL'} == 999)) or
			((not $self->gof_is_pvalue) and (($self->is_forward and ($res{'TEST OFV (DROP)'}< 0))
									   or ((not $self->is_forward) and ($res{'TEST OFV (DROP)'} > 0))
			 ))
			)  {
			$res{'local_min'} = 1;
		}else{
			$res{'local_min'} = 0;
		}
	}
	
	if ($res{'MODEL'} =~ /^(.+)-(\d+)$/ ){
		$res{'parcov'} = $1;
		$res{'state'} = $2;
	}else{
		die("unrecognized MODEL ".$res{'MODEL'});
	}
	return \%res;

}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
