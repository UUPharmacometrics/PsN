#!/usr/bin/perl

# Unit tests for output.pm

#use strict;
use FindBin qw($Bin);
use lib "$Bin/../../lib"; 	# PsN packages
use output;
no warnings 'all';

$SIG{__WARN__} = sub {
	my $message = shift;
};

#1.copy new outfile to relevant location with informative name
#2. set $outfile to new outfile and run this script, generates file testcase.txt
#3. manually check that output text in file testcase.txt matches what is in .lst and .ext etc
#4. manually copy output text in testcase.txt to answerhash in file ???
#5. run prove output.t to count tests, and update number of tests in plan
#6. run prove output.t to check

#our $test_files = '/home/kajsa/dev_PsN_tools/new_lst_parser/';
our $test_files = '/home/kajsa/kod-psn/PsN4/test/test_files/output/';
my $outfile = 'onePROB/multEST/firstEstTerm.lst';
#my $outfile = 'nm73/example6b_V7_30_beta.lst';
#my $outfile = 'nm74/icon_examples/control3boot.res';


open (OUT,">testcase.txt");
my $outobj = output -> new ('filename'=> $test_files.$outfile);
print OUT '$index++'.";\n";
print OUT '$answer_hashes[$index]={};'."\n";
print OUT '$answer_hashes[$index]->{file}='."'".$outfile."'".";\n";
print OUT '$answer_hashes[$index]->{answers}={};'."\n";
if( $outobj -> parsed_successfully ){
	print OUT '$answer_hashes[$index]->{parsed_successfully}=1'.";\n";
	my $minim = $outobj->access_any(attribute => 'estimation_step_run');
	print "problems ".scalar(@{$minim})."\n";
	for (my $prob=0; $prob < scalar(@{$minim}); $prob++){
		print "subprobs ".scalar(@{$minim->[$prob]})."\n";
		for (my $subprob=0; $subprob < scalar(@{$minim->[$prob]}); $subprob++){
			my $val = $outobj->get_single_value(problem_index => $prob, 
												subproblem_index=> $subprob, 
												attribute=>'ofv');
			print OUT '$answer_hashes[$index]->{answers}->{'.$prob.'}->{'.$subprob.'}->{'."'ofv'".'}='.sprintf("%.14f",$val).";\n";
			foreach my $attr ('covariance_step_successful','covariance_step_run',
							  'covariance_step_warnings',
							  'rounding_errors','minimization_successful',
							  'estimate_near_boundary','condition_number'){
				my $val = $outobj->get_single_value(problem_index => $prob, 
													subproblem_index=> $subprob, 
													attribute=>$attr);
				print OUT '$answer_hashes[$index]->{answers}->{'.$prob.'}->{'.$subprob.'}->{'."'".$attr."'".'}='.$val.";\n";
			}
			foreach my $attr ('thetas','omegas','sigmas','sethetas','seomegas','sesigmas'){
				my $val = $outobj->get_single_value(problem_index => $prob, 
													subproblem_index=> $subprob, 
													attribute=>$attr);
				for (my $j=0; $j<scalar(@{$val}); $j++){
					$val->[$j] = sprintf("%.5E",$val->[$j]);
				}
				print OUT '$answer_hashes[$index]->{answers}->{'.$prob.'}->{'.$subprob.'}->{'."'".$attr."'".'}=['.
					join(',',@{$val})."];\n";
			}
		}
	}
}else{
	print OUT '$answer_hashes[$index]->{parsed_successfully}=0'.";\n";
}
close OUT;


