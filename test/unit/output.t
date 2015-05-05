#!/usr/bin/perl

# Unit tests for output.pm

use strict;
use warnings;
use Test::More;# tests=>1716;
use Test::Exception;
use Math::Random;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

unshift @INC, $includes::testfiledir . '/output';
require answers;
use output;

our $test_files = $includes::testfiledir . '/output/';

$SIG{__WARN__} = sub {
	my $message = shift;
};

sub cmp_array
{
    my $func=shift;
    my $facit=shift;
    my $label=shift;

    is (scalar(@{$func}),scalar(@{$facit}),"$label, equal length");

    my $min = scalar(@{$func});
    $min = scalar(@{$facit}) if (scalar(@{$facit})< $min);
    for (my $i=0; $i<$min; $i++){
    	cmp_ok($func->[$i],'==',$facit->[$i],"$label, index $i");
    }		
	
}

my $ref = answers::read_answers();
my @answer_hashes = @{$ref};


for (my $i=0; $i< scalar(@answer_hashes); $i++){
	my $fname = $answer_hashes[$i]->{file};
	my $outfile = $test_files.$answer_hashes[$i]->{file};
	ok(-e $outfile, "output file $outfile exists");
	unless(-e $outfile) {
		print "file $outfile does not exist\n";
	    next;
	}

	my $outobj = output -> new ('filename' => $outfile);
	cmp_ok($outobj->parsed_successfully,'==',$answer_hashes[$i]->{parsed_successfully}, "output file $outfile parsed successfully");

	unless( $outobj -> parsed_successfully ){
		cmp_ok(length($outobj->parsing_error_message),'>',5,'error message exists');
	    next;
	}

	if(defined $answer_hashes[$i]->{estimation_evaluation_problem_number}){
		cmp_ok($outobj->get_estimation_evaluation_problem_number(),'==',
			   $answer_hashes[$i]->{estimation_evaluation_problem_number},"$fname estimation_evaluation_problem_number");
	}

	if(defined $answer_hashes[$i]->{problem_count}){
		cmp_ok($outobj->get_problem_count(),'==',$answer_hashes[$i]->{problem_count},"$fname problem_count");
	}
	if(defined $answer_hashes[$i]->{subproblem_count}){
		for (my $prob=0; $prob<scalar(@{$answer_hashes[$i]->{subproblem_count}}); $prob++){
			cmp_ok($outobj->problems->[$prob]->get_subproblem_count,
				   '==',$answer_hashes[$i]->{subproblem_count}->[$prob],"$fname subproblem_count prob $prob ");
		}
	}

	if(defined $answer_hashes[$i]->{high_correlations_names}){
		#assume only for 1 prob 1 subprob
		my ($high_names,$high_values) = $outobj->high_correlations(problems=>[1],subproblems=>[1],limit=>0.95);
		is_deeply($high_names->[0][0],$answer_hashes[$i]->{high_correlations_names},"$fname high_correlations names");
		is_deeply($high_values->[0][0],$answer_hashes[$i]->{high_correlations_values},"$fname high_correlations values");
#		print join(';',@{$high_names->[0][0]})."\n";
#		print join(';',@{$answer_hashes[$i]->{high_correlations_names}})."\n";
	}

	if(defined $answer_hashes[$i]->{large_standard_errors_names}){
		#assume only for 1 prob 1 subprob
		my ($high_names,$high_values) = $outobj->large_standard_errors(problems=>[1],subproblems=>[1],
																	   theta_cv_limit => 0.95,
																	   omega_cv_limit => 0.95,
																	   sigma_cv_limit => 0.95,);
		is_deeply($high_names->[0][0],$answer_hashes[$i]->{large_standard_errors_names},"$fname large_standard_errors names");
		is_deeply($high_values->[0][0],$answer_hashes[$i]->{large_standard_errors_values},"$fname large_standard_errors values");
	}

	foreach my $prob (keys %{$answer_hashes[$i]->{answers}}){
		foreach my $subprob (keys %{$answer_hashes[$i]->{answers}->{$prob}}){
			foreach my $attr (keys %{$answer_hashes[$i]->{answers}->{$prob}->{$subprob}}){
				if ($attr =~ /^(sethetas|seomegas|sesigmas|thetas|omegas|sigmas|sdcorrform_)/){
					cmp_array($outobj->get_single_value(problem_index => $prob, 
														subproblem_index=> $subprob, 
														attribute=>$attr),
							 $answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr},"$fname $attr prob $prob subprob $subprob");
				}elsif ($attr =~ /^condition_number/){
					my $tval = sprintf ("%.7f",$outobj->get_single_value(problem_index => $prob, 
																		 subproblem_index=> $subprob,
																		 attribute=>$attr));
					my $ansval = sprintf ("%.7f",$answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr} );
					cmp_ok($tval,'eq',$ansval,"$fname $attr prob $prob subprob $subprob");
				}else{
					cmp_ok($outobj->get_single_value(problem_index => $prob, 
													 subproblem_index=> $subprob,
													 attribute=>$attr),'==',
						   $answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr},"$fname $attr prob $prob subprob $subprob");
				}
			}
		}
	}
}

done_testing;
