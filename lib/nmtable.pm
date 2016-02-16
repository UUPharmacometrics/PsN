package nmtable;

# A class representing a table output from NONMEM

use include_modules;
use Moose;
use MooseX::Params::Validate;
use table;
use math qw(usable_number);

extends 'table'; 

has 'table_number' => ( is => 'rw', isa => 'Int' );
has 'method' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'goal_function' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'problem' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'subproblem' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'superproblem1' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'iteration1' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'superproblem2' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'iteration2' => ( is => 'rw', isa => 'Maybe[Int]' );

sub read_table_row
{
    # Parses the TABLE NO. row
    my $self = shift;
	my %parm = validated_hash(\@_,
		row => { isa => 'Str' },
	);
    my $row = $parm{'row'};

    if ($row =~ /TABLE NO.\s+(\d+):\s*(.*):\s*Problem=(\d+)\s+Subproblem=(\d+)\s+Superproblem1=(\d+)\s+Iteration1=(\d+)\s+Superproblem2=(\d+)\s+Iteration2=(\d+)/) {
        $self->table_number($1);
        $self->problem($3);
        $self->subproblem($4);
        $self->superproblem1($5);
        $self->iteration1($6);
        $self->superproblem2($7);
        $self->iteration2($8);
	}elsif ($row =~ /TABLE NO.\s+(\d+):\s*(.*)\s*$/) {
        $self->table_number($1);
    }elsif ($row =~ /TABLE NO.\s+(\d+)/) {
        $self->table_number($1);
    }

    $self->_parse_method_string($2); 
}

sub _parse_method_string
{
    my $self = shift;
    my $string = shift;

    if ($string =~ /\s*(.*):\s*Goal Function=(.*)/) {
        $self->method($1);
        $self->goal_function($2);
    } elsif ($string !~ /:/) {
        $self->method($string);
    }
}

sub get_iteration_lookup
{
	my $self = shift;
    my $header = $self->get_header;

	my %index =();
	if ($header->[0] eq 'ITERATION'){
		my $result_types = $self->get_column(index => 0);
		for (my $i=1; $i<= scalar(@{$result_types}); $i++){
			#search backwards for efficiency and not confound with burn-in iterations
			if     ($result_types->[-$i] == -1000000000 ) {
				$index{'est'} = scalar(@{$result_types})-$i;
				last;
			}elsif ($result_types->[-$i] == -1000000001 ) {
				$index{'se'} = scalar(@{$result_types})-$i;
			}elsif ($result_types->[-$i] == -1000000002 ) {
				$index{'eigen'} = scalar(@{$result_types})-$i;
			}elsif ($result_types->[-$i] == -1000000003 ) {
				$index{'matrix'} = scalar(@{$result_types})-$i;
			}elsif ($result_types->[-$i] == -1000000004 ) {
				$index{'sd'} = scalar(@{$result_types})-$i;
			}elsif ($result_types->[-$i] == -1000000005 ) {
				$index{'sdse'} = scalar(@{$result_types})-$i;
			}elsif ($result_types->[-$i] == -1000000006 ) {
				$index{'extra'} = scalar(@{$result_types})-$i;
			}
		}
	}
	return \%index;

}
sub parse_ext_table
{
    my $self = shift;
	my %results=();

	my $lookup = $self->get_iteration_lookup();
	return {} unless (scalar(keys %{$lookup}) > 0);
    my $header = $self->get_header;
	my @eigenvalues = ();
	my $any_se = 0;
	my $any_est = 0;
	for (my $i=1; $i<scalar(@{$header}); $i++){
		my $values = $self->get_column(index => $i);
		if ($i==1 and defined $lookup->{'matrix'}){
			$results{'condition_number'} = eval($values->[$lookup->{'matrix'}]);
		}elsif ($i==2 and defined $lookup->{'matrix'}){
			$results{'lowest_eigenvalue'} = eval($values->[$lookup->{'matrix'}]);
		}elsif ($i==3 and defined $lookup->{'matrix'}){
			$results{'highest_eigenvalue'} = eval($values->[$lookup->{'matrix'}]);
		}
		if (defined $lookup->{'eigen'} and (not $values->[$lookup->{'eigen'}] == 0)){
			push (@eigenvalues, _get_value($values->[$lookup->{'eigen'}])); #push also undefs
		}
		if ($header->[$i] =~ /THETA/){
			if (defined $lookup->{'est'}){
				my $val = _get_value($values->[$lookup->{'est'}]);
				if (defined $val){
					$results{'thetacoordval'}->{$header->[$i]} = $val ;
					$any_est = 1;
				}
			}
			if (defined $lookup->{'se'}){
				my $val = _get_value($values->[$lookup->{'se'}]);
				if (defined $val){
					$results{'sethetacoordval'}->{$header->[$i]} = $val ;
					$any_se = 1;
				}
			}
		}elsif($header->[$i] =~ /OMEGA/){
			if (defined $lookup->{'est'}){
				my $val = _get_value($values->[$lookup->{'est'}]);
				if (defined $val){
					$results{'omegacoordval'}->{$header->[$i]} = $val;
					$any_est = 1;
				}
			}
			if (defined $lookup->{'se'}){
				my $val = _get_value($values->[$lookup->{'se'}]);
				if (defined $val){
					$results{'seomegacoordval'}->{$header->[$i]} = $val;
					$any_se = 1;
				}
			}
			if (defined $lookup->{'sd'}){
				my $val = _get_value($values->[$lookup->{'sd'}]);
				$results{'sdcorrform_omegacoordval'}->{$header->[$i]} = $val if (defined $val);
			}
			if (defined $lookup->{'sdse'}){
				my $val = _get_value($values->[$lookup->{'sdse'}]);
				$results{'sdcorrform_seomegacoordval'}->{$header->[$i]} = $val if (defined $val);
			}
		}elsif($header->[$i] =~ /SIGMA/){
			if (defined $lookup->{'est'}){
				my $val = _get_value($values->[$lookup->{'est'}]);
				if (defined $val){
					$results{'sigmacoordval'}->{$header->[$i]} = $val;
					$any_est = 1;
				}
			}
			if (defined $lookup->{'se'}){
				my $val = _get_value($values->[$lookup->{'se'}]);
				if (defined $val){
					$results{'sesigmacoordval'}->{$header->[$i]} = $val;
					$any_se = 1;
				}
			}
			if (defined $lookup->{'sd'}){
				my $val = _get_value($values->[$lookup->{'sd'}]);
				$results{'sdcorrform_sigmacoordval'}->{$header->[$i]} = $val if (defined $val);
			}
			if (defined $lookup->{'sdse'}){
				my $val = _get_value($values->[$lookup->{'sdse'}]);
				$results{'sdcorrform_sesigmacoordval'}->{$header->[$i]} = $val if (defined $val);
			}
		}elsif($header->[$i] =~ /OBJ/ and defined $lookup->{'est'}){
			my $val = _get_value($values->[$lookup->{'est'}]);
			$results{'ofv'} = $val if (defined $val);
		}
	}
	$results{'any_se'} = $any_se;
	$results{'any_est'} = $any_est;
	if (scalar(@eigenvalues)>0){
		$results{'eigenvalues'} = \@eigenvalues;
	}
	my $have_omegas = 0;
	$have_omegas = 1 if (defined $self->header->{'OMEGA(2,2)'});#never more than 1 dummy col
	my $have_sigmas = 0;
	$have_sigmas = 1 if (defined $self->header->{'SIGMA(2,2)'});#never more than 1 dummy col
	$results{'have_omegas'} = $have_omegas;
	$results{'have_sigmas'} = $have_sigmas;
	
	return \%results;
}

sub _get_value{
	#static method, translate text in NM7 ext,cov, coi etc to either number or undef
	my $val = shift;
	my $no_value = 10000000000;
	my $answer= eval($val);
	if(($answer == $no_value) or (not math::usable_number($val))){
		$answer = undef;
	}
	return $answer;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
