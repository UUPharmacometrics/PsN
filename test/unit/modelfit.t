#!/usr/bin/perl

# Unit tests for llp.pm
# Note that only a small portion of the module is tested

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use tool::modelfit;
use math;
use output;

our $output_files = $includes::testfiledir . '/output/';
my $modeldir = $includes::testfiledir;


my $outobj = output -> new ('filename' => $output_files.'special_mod/near_bounds.lst');

#testing static passed_picky

my $passed = tool::modelfit::passed_picky(minimization_successful => $outobj->minimization_successful,
										  minimization_message => $outobj->minimization_message,
										  probnum => 1,
										  picky => 1);

is($passed,0,'passed picky near bounds');

$outobj = output -> new ('filename' => $modeldir.'/phenoMAXEV0.lst');
$passed = tool::modelfit::passed_picky(minimization_successful => $outobj->minimization_successful,
									   minimization_message => $outobj->minimization_message,
									   probnum => 1,
									   picky => 1);

is($passed,0,'passed picky pheno MAXEV=0');

$outobj = output -> new ('filename' => $modeldir.'/phenofull.lst');
$passed = tool::modelfit::passed_picky(minimization_successful => $outobj->minimization_successful,
									   minimization_message => $outobj->minimization_message,
									   probnum => 1,
									   picky => 1);

is($passed,1,'passed picky phenofull');

$passed = tool::modelfit::passed_picky(minimization_successful => $outobj->minimization_successful,
									   minimization_message => $outobj->minimization_message,
									   probnum => 1,
									   picky => 0);

is($passed,0,'passed picky phenofull picky not set');

$passed = tool::modelfit::passed_picky(minimization_successful => $outobj->minimization_successful,
									   minimization_message => $outobj->minimization_message,
									   probnum => 0,
									   picky => 1);

is($passed,0,'passed picky phenofull 0 probnum');


#testing static select_best_retry
my @run_results=();

#1
push(@run_results,{'ofv' => undef, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => undef, 'pass_picky'=> 0, 'minimization_successful' => 0});

my $selected = tool::modelfit::select_best_retry(run_results => \@run_results,
												 accepted_ofv_difference => 0.5);

cmp_ok($selected,'==',1,'if no ofv defined then select number 1 ');

#2 picky set, choose any
@run_results=();
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 12, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 13, 'pass_picky'=> 1, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 14, 'pass_picky'=> 1, 'minimization_successful' => 1});

$selected = tool::modelfit::select_best_retry(run_results => \@run_results,
											  accepted_ofv_difference => 0.5);

cmp_ok($selected,'==',2,' select nonsuccess if very good ofv');

#3 picky set, choose success when only little 
@run_results=();
push(@run_results,{'ofv' => 10, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10.8, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 10.4, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 12, 'pass_picky'=> 1, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 11, 'pass_picky'=> 1, 'minimization_successful' => 1});

$selected = tool::modelfit::select_best_retry(run_results => \@run_results,
											  accepted_ofv_difference => 0.5);

cmp_ok($selected,'==',4,'select success when only little worse than best global');

#4 picky set, choose picky
@run_results=();
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 10.7, 'pass_picky'=> 1, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 10.4, 'pass_picky'=> 1, 'minimization_successful' => 1});

$selected = tool::modelfit::select_best_retry(run_results => \@run_results,
											  accepted_ofv_difference => 0.5);

cmp_ok($selected,'==',6,' select picky when only little worse than best global ');

#5 picky not set
@run_results=();
push(@run_results,{'ofv' => 8.6, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 9, 'pass_picky'=> 0, 'minimization_successful' => 1});

$selected = tool::modelfit::select_best_retry(run_results => \@run_results,
											  accepted_ofv_difference => 0.5);

cmp_ok($selected,'==',4,' pick success when not much worse than global ');

#6 picky not set
@run_results=();
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 12, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 14, 'pass_picky'=> 0, 'minimization_successful' => 1});

$selected = tool::modelfit::select_best_retry(run_results => \@run_results,
											  accepted_ofv_difference => 0.5);

cmp_ok($selected,'==',2,' pick global when success not good enough ');

#7 picky not set
@run_results=();
push(@run_results,{'ofv' => 12, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 14, 'pass_picky'=> 0, 'minimization_successful' => 1});

$selected = tool::modelfit::select_best_retry(run_results => \@run_results,
											  accepted_ofv_difference => 0);

cmp_ok($selected,'==',3,'pick success when equal to global ');

#8 picky not set
@run_results=();
push(@run_results,{'ofv' => 13, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 10.5, 'pass_picky'=> 0, 'minimization_successful' => 1});

$selected = tool::modelfit::select_best_retry(run_results => \@run_results,
											  accepted_ofv_difference => 0.5);

cmp_ok($selected,'==',4,' pick success when diff withing gloval');

#9 picky not set
@run_results=();
push(@run_results,{'ofv' => 13, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10, 'pass_picky'=> 0, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 10.5, 'pass_picky'=> 0, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 11, 'pass_picky'=> 0, 'minimization_successful' => 1});

$selected = tool::modelfit::select_best_retry(run_results => \@run_results,
											  accepted_ofv_difference => 0.2);

cmp_ok($selected,'==',2,' pick global when success not good enough ');


done_testing;
