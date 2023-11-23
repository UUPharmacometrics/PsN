#!/usr/bin/perl

# Unit tests for output.pm

use strict;
use warnings;
use Test::More;# tests=>1716;
use Test::Exception;
use File::Spec;
use File::Basename;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

unshift @INC, File::Spec->catfile($includes::testfiledir, 'output');
require answers;
use output;
use ui;

ui->silent(1);

our $test_files = File::Spec->catfile($includes::testfiledir, 'output');



my $ref = answers::read_answers();
my @answer_hashes = @{$ref};


for (my $i=0; $i< scalar(@answer_hashes); $i++){
	my $fname = $answer_hashes[$i]->{file};
	my $outfile = File::Spec->catfile($test_files, $answer_hashes[$i]->{file});
	ok(-e $outfile, "output file $outfile exists");
	unless(-e $outfile) {
		print "file $outfile does not exist\n";
	    next;
	}

    (my $filename, my $dirs) = fileparse($outfile);
	my $outobj = output->new(filename => $filename, directory => $dirs);
	cmp_ok($outobj->parsed_successfully,'==',$answer_hashes[$i]->{parsed_successfully}, "output file $outfile parsed successfully");
	unless( $outobj -> parsed_successfully ){
		cmp_ok(length($outobj->parsing_error_message),'>',5,'error message exists');
		my ($fail,$reason) = $outobj->nonmem_run_failed();
		is($fail,1,'not parsed successfully means nonmem run failed'); 
		cmp_ok($reason,'eq','lst-file not parsed successfully',"$fname nonmem fail reason");
		if(defined $answer_hashes[$i]->{iterations_interrupted}){
			cmp_ok($outobj->iterations_interrupted,'==',$answer_hashes[$i]->{iterations_interrupted}, 
				   "output file $outfile iterations interrupted");
		}
	    next;
	}

	my ($fail,$reason) = $outobj->nonmem_run_failed();
	if(defined $answer_hashes[$i]->{nonmem_run_failed}){
		cmp_ok($fail,'==',$answer_hashes[$i]->{nonmem_run_failed},"$fname nonmem_run_failed ok");
		cmp_ok($reason,'eq',$answer_hashes[$i]->{nonmem_run_fail_reason},"$fname nonmem fail reason") if ($fail);
	}else{
		cmp_ok($fail,'==',0,"$fname nonmem_run_failed is false");
	}
	if(defined $answer_hashes[$i]->{estimation_evaluation_problem_number}){
		cmp_ok($outobj->get_estimation_evaluation_problem_number(),'==',
			   $answer_hashes[$i]->{estimation_evaluation_problem_number},"$fname estimation_evaluation_problem_number");
	}

	if(defined $answer_hashes[$i]->{runtime}){
		cmp_ok($outobj->runtime(),'eq',$answer_hashes[$i]->{runtime},"$fname runtime");
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

	if(defined $answer_hashes[$i]->{near_bounds_names}){
		#assume only for 1 prob 1 subprob
		my ($bounds,$names,$values) = $outobj->near_bounds(zero_limit=>0.01,
														   off_diagonal_sign_digits => 2,
														   significant_digits=>2);
		is_deeply($bounds->[0][0],$answer_hashes[$i]->{near_bounds_bounds},"$fname near_bounds bounds");
		is_deeply($names->[0][0],$answer_hashes[$i]->{near_bounds_names},"$fname near_bounds names");
		is_deeply($values->[0][0],$answer_hashes[$i]->{near_bounds_values},"$fname near_bounds values");
	}

	if(defined $answer_hashes[$i]->{high_correlations_names}){
		#assume only for 1 prob 1 subprob
		my ($high_names,$high_values) = $outobj->high_correlations(problems=>[1],subproblems=>[1],limit=>0.95);
		is_deeply($high_names->[0][0],$answer_hashes[$i]->{high_correlations_names},"$fname high_correlations names");
		is_deeply($high_values->[0][0],$answer_hashes[$i]->{high_correlations_values},"$fname high_correlations values");
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

    if (defined $answer_hashes[$i]->{tables_step_error}) {
        is (defined $outobj->problems->[0]->tables_step_error ? 1 : 0, $answer_hashes[$i]->{tables_step_error}, "$fname tables_step_error");
    }

	foreach my $prob (keys %{$answer_hashes[$i]->{answers}}){
		foreach my $subprob (keys %{$answer_hashes[$i]->{answers}->{$prob}}){
			foreach my $attr (keys %{$answer_hashes[$i]->{answers}->{$prob}->{$subprob}}){
				if ($attr =~ /^(sethetas|seomegas|sesigmas|thetas|omegas|sigmas|sdcorrform_|est_thetanames|est_sigmanames|est_omeganames|eigens|npetabars|npomegas|npcorr)/){
					cmp_float_array($outobj->get_single_value(problem_index => $prob, 
														subproblem_index=> $subprob, 
														attribute=>$attr),
							 $answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr},"$fname $attr prob $prob subprob $subprob");
				}elsif ($attr =~ /^(inverse_covariance_matrix|covariance_matrix|correlation_matrix|t_matrix)/){
					my $ref = $outobj->get_single_value(problem_index => $prob,subproblem_index=> $subprob, attribute=>$attr);
					cmp_float_array($ref, $answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr},"$fname $attr prob $prob subprob $subprob");
				}elsif ($attr =~ /^condition_number/){
					my $tval = sprintf ("%.7f",$outobj->get_single_value(problem_index => $prob, 
																		 subproblem_index=> $subprob,
																		 attribute=>$attr));
					my $ansval = sprintf ("%.7f",$answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr} );
					cmp_ok($tval,'eq',$ansval,"$fname $attr prob $prob subprob $subprob");
				}else{
					my $have = $outobj->get_single_value(problem_index => $prob, 
														 subproblem_index=> $subprob,
														 attribute=>$attr);
					if (defined $have){
						cmp_ok($have,'==',
							   $answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr},"$fname $attr prob $prob subprob $subprob");
					}else{
						is($have,$answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr},"$fname $attr prob $prob subprob $subprob");
					}
				}
			}
		}
	}
}

done_testing;
