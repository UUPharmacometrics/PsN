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


our $tempdir = create_test_dir('unit_modelfit');
our $dir = "$tempdir/fail_test";
mkdir($dir);
#put pheno.mod in testdir so that .ext etc in testfiledir are not modified

copy_test_files($dir,["modelfit/diagnose_lst_errors/FMSG", "modelfit/diagnose_lst_errors/locfile.set", "modelfit/diagnose_lst_errors/psn.lst"]);
chdir($dir);
my $ref = tool::modelfit::diagnose_lst_errors(missing => 0, 
							  run_no => 0,
							  have_stats_runs => 0,
							  modext  => 'mod',
							  run_local => 1,
							  nmtran_error_file => 'nmtran_error.txt',
							  nmqual => 0);
cmp_ok($ref->[0],'eq','There was an error when running nmfe, NMtran could not be initiated (the NMtran output file FDATA is missing)',
'failure nmtran init ');
cmp_ok($ref->[2],'==',0,'restart possible nmtran init ');
cmp_ok($ref->[3],'==',1,'store error nmtran init ');

copy_test_files($dir,["modelfit/diagnose_lst_errors/FDATA"]);

$ref = tool::modelfit::diagnose_lst_errors(missing => 0, 
							  run_no => 0,
							  have_stats_runs => 0,
							  modext  => 'mod',
							  run_local => 1,
							  nmtran_error_file => 'nmtran_error.txt',
							  nmqual => 0);
cmp_ok($ref->[0],'eq','NMtran failed','failure nmtran failed ');
cmp_ok($ref->[2],'==',0,'restart possible nmtran fail ');
cmp_ok($ref->[3],'==',1,'store error nmtran fail ');

copy_test_files($dir,["modelfit/diagnose_lst_errors/FREPORT"]);

$ref = tool::modelfit::diagnose_lst_errors(missing => 0, 
							  run_no => 0,
							  have_stats_runs => 0,
							  modext  => 'mod',
							  run_local => 1,
							  nmtran_error_file => 'nmtran_error.txt',
							  nmqual => 0);
cmp_ok($ref->[0],'eq','It seems like the compilation failed','failure compilation failed ');
cmp_ok($ref->[2],'==',0,'restart possible compilation fail ');
cmp_ok($ref->[3],'==',1,'store error compilation fail ');

copy_test_files($dir,["modelfit/diagnose_lst_errors/nonmem"]);

$ref = tool::modelfit::diagnose_lst_errors(missing => 0, 
										   run_no => 0,
										   interrupted => 0,
										   have_stats_runs => 0,
										   modext  => 'mod',
										   run_local => 1,
										   nmtran_error_file => 'nmtran_error.txt',
										   nmqual => 0);
cmp_ok($ref->[0],'eq','NONMEM run failed','failure NONMEM failed ');
cmp_ok($ref->[2],'==',0,'restart possible NONMEM fail ');
cmp_ok($ref->[3],'==',1,'store error NONMEM fail ');

$ref = tool::modelfit::diagnose_lst_errors(missing => 1, 
										   run_no => 0,
										   have_stats_runs => 0,
										   modext  => 'mod',
										   run_local => 1,
										   nmtran_error_file => 'nmtran_error.txt',
										   nmqual => 0);
cmp_ok($ref->[0],'eq','the lst-file does not exist in NM_run1','failure lst missing ');
cmp_ok($ref->[2],'==',0,'restart possible NONMEM fail ');
cmp_ok($ref->[3],'==',1,'store error NONMEM fail ');

remove_test_dir($tempdir);

our $tempdir = create_test_dir('unit_modelfit');
our $dir = "$tempdir/move_retry";
mkdir($dir);
my @files =("pheno5.mod","pheno5.lst","pheno5.ext","pheno5.phi");
copy_test_files($dir,\@files);
chdir($dir);

my ($mess,$file1,$file2) = tool::modelfit::move_retry_files(retry => 0,
															filenames => \@files,
															nm_major_version => 7);

ok(-e 'pheno5-1.mod', "move retry mod");
ok(-e 'pheno5-1.lst', "move retry lst");
ok(-e 'pheno5-1.ext', "move retry ext");
ok(-e 'pheno5-1.phi', "move retry phi");

my ($mess,$file1,$file2) = tool::modelfit::move_retry_files(retry => 0,
															crash => 2,
															filenames => \@files,
															nm_major_version => 7);

ok(-e 'pheno5-1-step2.mod', "move retry crash mod");
ok(-e 'pheno5-1-step2.lst', "move retry crash lst");
ok(-e 'pheno5-1-step2.ext', "move retry crash ext");
ok(-e 'pheno5-1-step2.phi', "move retry crash phi");

remove_test_dir($tempdir);

my $str = tool::modelfit::get_retry_name(filename => 'psn.lst',
										 retry => 0,
										 nm_major_version => 7,
										 nm_minor_version => undef);
cmp_ok($str,'eq','psn-1.lst','get retry name 1');

$str = tool::modelfit::get_retry_name(filename => 'psn.ctl',
									  retry => 1,
									  crash => 1,
									  nm_major_version => 7,
									  nm_minor_version => undef);
cmp_ok($str,'eq','psn-2-step1.ctl','get retry name 2');

$str = tool::modelfit::get_retry_name(filename => 'output',
										 retry => 0,
										 nm_major_version => 7,
										 nm_minor_version => undef);
cmp_ok($str,'eq','output-1','get retry name 3');

$str = tool::modelfit::get_retry_name(filename => 'msf_ETAS',
										 retry => 0,
										 nm_major_version => 7,
										 nm_minor_version => 3);
cmp_ok($str,'eq','msf-1_ETAS','get retry name 4');
$str = tool::modelfit::get_retry_name(filename => 'run1.msf_ETAS',
										 retry => 1,
										 nm_major_version => 7,
										 nm_minor_version => 3);
cmp_ok($str,'eq','run1-2.msf_ETAS','get retry name 5');


$outobj = output -> new ('filename' => $output_files.'special_mod/maxeval_exceeded.lst');
cmp_ok(tool::modelfit::maxeval_exceeded(output => $outobj, retries_probnum => 1),'==',15,'modelfit maxeval_exceeded 1');

cmp_ok(tool::modelfit::hessian_error(output => $outobj, retries_probnum => 1),'==',0,'modelfit hessian_error 1');

$outobj = output -> new ('filename' => $output_files.'onePROB/oneEST/noSIM/warfarin_noext.lst');
cmp_ok(tool::modelfit::maxeval_exceeded(output => $outobj, retries_probnum => 1),'==',0,'modelfit maxeval_exceeded 2');

$outobj = output -> new ('filename' => $output_files.'onePROB/oneEST/noSIM/hessian_error.lst');
cmp_ok(tool::modelfit::hessian_error(output => $outobj, retries_probnum => 1),'==',1,'modelfit hessian_error 2');

#significant_digits_accept
@run_results=();
push(@run_results,{'ofv' => 13, 'significant_digits'=> 3.2, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 13, 'significant_digits'=> 3.2, 'minimization_successful' => 1});
push(@run_results,{'ofv' => 13, 'significant_digits'=> undef, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 13, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 13, 'significant_digits'=> 3, 'minimization_successful' => 0});


cmp_ok(tool::modelfit::significant_digits_accepted(run_results => \@run_results,significant_digits_accept => 3, try => 0),'==',1,'signficant_digits_accepted enough') ;
cmp_ok(tool::modelfit::significant_digits_accepted(run_results => \@run_results,significant_digits_accept => 4, try => 0),'==',0,'signficant_digits_accepted not enough') ;
cmp_ok(tool::modelfit::significant_digits_accepted(run_results => \@run_results,significant_digits_accept => 3, try => 1),'==',0,'signficant_digits_accepted min succ') ; #min succ

cmp_ok(tool::modelfit::significant_digits_accepted(run_results => \@run_results,significant_digits_accept => 1, try => 2),'==',0,'signficant_digits_accepted undef') ;

cmp_ok(tool::modelfit::significant_digits_accepted(run_results => \@run_results,significant_digits_accept => 1, try => 3),'==',0,'signficant_digits_accepted not exist') ;
cmp_ok(tool::modelfit::significant_digits_accepted(run_results => \@run_results,significant_digits_accept => 0, try => 4),'==',0,'signficant_digits_accepted not set') ;
cmp_ok(tool::modelfit::significant_digits_accepted(run_results => \@run_results,significant_digits_accept => undef, try => 4),'==',0,'signficant_digits_accepted not set') ;

#local_minimum
@run_results=();
push(@run_results,{'ofv' => 13, 'significant_digits'=> 3.2, 'minimization_successful' => 0});
push(@run_results,{'ofv' => 14, 'significant_digits'=> 3.2, 'minimization_successful' => 1});

my $local_min = tool::modelfit::local_minimum(run_results => \@run_results,
											  accepted_ofv_difference => 0.5, 
											  reduced_model_ofv => 12.6,
											  have_accepted_run => 0,
											  try => 0);

cmp_ok($local_min,'==',0,'local min accepted_ofv_diff ok');

$local_min = tool::modelfit::local_minimum(run_results => \@run_results,
										   accepted_ofv_difference => 0, 
										   reduced_model_ofv => 12.6,
										   have_accepted_run => 0,
										   try => 0);

cmp_ok($local_min,'==',1,'local min accepted_ofv_diff not ok ');

$local_min = tool::modelfit::local_minimum(run_results => \@run_results,
										   accepted_ofv_difference => 0, 
										   reduced_model_ofv => 15,
										   have_accepted_run => 0,
										   try => 1);

cmp_ok($local_min,'==',2,'local min worse than prev run '); #local type 2

$local_min = tool::modelfit::local_minimum(run_results => \@run_results,
										   accepted_ofv_difference => 1, 
										   reduced_model_ofv => 15,
										   have_accepted_run => 0,
										   try => 1);

cmp_ok($local_min,'==',0,'local min worse than prev run but accepted ok');


#too low probnum
my $ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 0,
										   minimization_successful => 0,
										   local_minimum => 0,
										   hessian_error => 1,
										   handle_hessian_npd => 1, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 0,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 0,
										   sigdigs_accepted => 0,
										   pass_picky => 0,
										   picky => 0,
										   handle_msfo => 1,
										   tweak_inits => 1,
										   min_retries => 0,
										   max_retries => 5,
										   try => 0,
										   have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 1');
cmp_ok($ref->{'tweak_inits'},'==',0,'retries_decide_what_to_do tweak_inits 1');
cmp_ok($ref->{'retry'} ,'==',0,'retries_decide_what_to_do retry 1');
cmp_ok($ref->{'reset_msfo'},'==',0,'retries_decide_what_to_do reset msfo 1');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 1');
cmp_ok($ref->{'message'},'eq','Not doing retry because no estimation to evaluate','retries_decide message');

#max retries and min retries reached
$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 2,
										   minimization_successful => 0,
										   local_minimum => 0,
										   hessian_error => 1,
										   handle_hessian_npd => 1, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 0,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 0,
										   sigdigs_accepted => 0,
										   pass_picky => 0,
										   picky => 0,
										   handle_msfo => 1,
										   tweak_inits => 1,
										   min_retries => 0,
										   max_retries => 1,
										   try => 1,
										   have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 2');
cmp_ok($ref->{'tweak_inits'},'==',0,'retries_decide_what_to_do tweak_inits 2');
cmp_ok($ref->{'retry'} ,'==',0,'retries_decide_what_to_do retry 2');
cmp_ok($ref->{'reset_msfo'},'==',0,'retries_decide_what_to_do reset msfo 2');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 2');
cmp_ok($ref->{'message'},'eq','Not doing retry because reached max_retries','retries_decide message');

#not successful but min_retries reached and have old accepted run
$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
										   minimization_successful => 0,
										   local_minimum => 0,
										   hessian_error => 0,
										   handle_hessian_npd => 0, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 0,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 0,
										   sigdigs_accepted => 0,
										   pass_picky => 0,
										   picky => 0,
										   handle_msfo => 0,
										   tweak_inits => 1,
										   min_retries => 1,
										   max_retries => 2,
										   try => 1,
										   have_accepted_run => 1);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 3');
cmp_ok($ref->{'tweak_inits'},'==',0,'retries_decide_what_to_do tweak_inits 3');
cmp_ok($ref->{'retry'} ,'==',0,'retries_decide_what_to_do retry 3');
cmp_ok($ref->{'reset_msfo'},'==',0,'retries_decide_what_to_do reset msfo 3');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 3');
cmp_ok($ref->{'message'},'eq','Not doing retry because have accepted run from before and have reached min_retries','retries_decide message');



# hessian error and handle

$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													 minimization_successful => 0,
													 local_minimum => 0,
													 hessian_error => 1,
													 handle_hessian_npd => 1, 
													 round_error => 0,
													 cut_thetas_rounding_errors => 0,
													 maxevals_exceeded => 0,
													 cut_thetas_maxevals => 0,
													 sigdigs_accepted => 0,
													 pass_picky => 0,
													 picky => 0,
													 handle_msfo => 1,
													 tweak_inits => 0,
													 min_retries => 0,
													 max_retries => 1,
													 try => 0,
													 have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',1,'retries_decide_what_to_do cut_thetas 4');
cmp_ok($ref->{'tweak_inits'},'==',0,'retries_decide_what_to_do tweak_inits 4');
cmp_ok($ref->{'retry'} ,'==',1,'retries_decide_what_to_do retry 4');
cmp_ok($ref->{'reset_msfo'},'==',1,'retries_decide_what_to_do reset msfo 4');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 4');
cmp_ok($ref->{'message'},'eq','Doing retry because of hessian/round/maxevals termination and handling','retries_decide message');


# round_error and handle

$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													 minimization_successful => 0,
													 local_minimum => 0,
													 hessian_error => 0,
													 handle_hessian_npd => 0, 
													 round_error => 1,
													 cut_thetas_rounding_errors => 1,
													 maxevals_exceeded => 0,
													 cut_thetas_maxevals => 0,
													 sigdigs_accepted => 0,
													 pass_picky => 0,
													 picky => 0,
													 handle_msfo => 1,
													 tweak_inits => 1,
													 min_retries => 0,
													 max_retries => 2,
													 try => 1,
													 have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',1,'retries_decide_what_to_do cut_thetas 5');
cmp_ok($ref->{'tweak_inits'},'==',1,'retries_decide_what_to_do tweak_inits 5');
cmp_ok($ref->{'retry'} ,'==',1,'retries_decide_what_to_do retry 5');
cmp_ok($ref->{'reset_msfo'},'==',1,'retries_decide_what_to_do reset msfo 5');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 5');
cmp_ok($ref->{'message'},'eq','Doing retry because of hessian/round/maxevals termination and handling','retries_decide message');

# maxevals and handle

$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													 minimization_successful => 0,
													 local_minimum => 0,
													 hessian_error => 0,
													 handle_hessian_npd => 0, 
													 round_error => 0,
													 cut_thetas_rounding_errors => 0,
													 maxevals_exceeded => 1,
													 cut_thetas_maxevals => 1,
													 sigdigs_accepted => 0,
													 pass_picky => 0,
													 picky => 0,
													 handle_msfo => 1,
													 tweak_inits => 0,
													 min_retries => 0,
													 max_retries => 2,
													 try => 1,
													 have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',1,'retries_decide_what_to_do cut_thetas 6');
cmp_ok($ref->{'tweak_inits'},'==',0,'retries_decide_what_to_do tweak_inits 6');
cmp_ok($ref->{'retry'} ,'==',1,'retries_decide_what_to_do retry 6');
cmp_ok($ref->{'reset_msfo'},'==',1,'retries_decide_what_to_do reset msfo 6');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 6');
cmp_ok($ref->{'message'},'eq','Doing retry because of hessian/round/maxevals termination and handling','retries_decide message');


#tweak_inits turned off
$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
										   minimization_successful => 0,
										   local_minimum => 0,
										   hessian_error => 0,
										   handle_hessian_npd => 0, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 0,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 0,
										   sigdigs_accepted => 0,
										   pass_picky => 0,
										   picky => 0,
										   handle_msfo => 1,
										   tweak_inits => 0,
										   min_retries => 0,
										   max_retries => 3,
										   try => 1,
										   have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 7');
cmp_ok($ref->{'tweak_inits'},'==',0,'retries_decide_what_to_do tweak_inits 7');
cmp_ok($ref->{'retry'} ,'==',0,'retries_decide_what_to_do retry 7');
cmp_ok($ref->{'reset_msfo'},'==',0,'retries_decide_what_to_do reset msfo 7');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 7');
cmp_ok($ref->{'message'},'eq','Not doing retry because tweak_inits turned off','retries_decide message');

#not pass picky
$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
										   minimization_successful => 1,
										   local_minimum => 0,
										   hessian_error => 0,
										   handle_hessian_npd => 0, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 0,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 0,
										   sigdigs_accepted => 0,
										   pass_picky => 0,
										   picky => 1,
										   handle_msfo => 0,
										   tweak_inits => 1,
										   min_retries => 0,
										   max_retries => 3,
										   try => 1,
										   have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 8');
cmp_ok($ref->{'tweak_inits'},'==',1,'retries_decide_what_to_do tweak_inits 8');
cmp_ok($ref->{'retry'} ,'==',1,'retries_decide_what_to_do retry 8');
cmp_ok($ref->{'reset_msfo'},'==',0,'retries_decide_what_to_do reset msfo 8');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 8');
cmp_ok($ref->{'message'},'eq','Doing retry because of not pass picky','retries_decide message');


#minim not successful
$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
										   minimization_successful => 0,
										   local_minimum => 0,
										   hessian_error => 0,
										   handle_hessian_npd => 0, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 0,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 0,
										   sigdigs_accepted => 0,
										   pass_picky => 0,
										   picky => 0,
										   handle_msfo => 1,
										   tweak_inits => 1,
										   min_retries => 0,
										   max_retries => 1,
										   try => 0,
										   have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 9');
cmp_ok($ref->{'tweak_inits'},'==',1,'retries_decide_what_to_do tweak_inits 9');
cmp_ok($ref->{'retry'} ,'==',1,'retries_decide_what_to_do retry 9');
cmp_ok($ref->{'reset_msfo'},'==',1,'retries_decide_what_to_do reset msfo 9');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 9');
cmp_ok($ref->{'message'},'eq','Doing retry because of minimization not successful and not sigdigs accepted','retries_decide message');

#minim not successful but sigdigs accepted
$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
										   minimization_successful => 0,
										   local_minimum => 0,
										   hessian_error => 0,
										   handle_hessian_npd => 0, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 0,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 0,
										   sigdigs_accepted => 1,
										   pass_picky => 0,
										   picky => 0,
										   handle_msfo => 1,
										   tweak_inits => 1,
										   min_retries => 0,
										   max_retries => 2,
										   try => 1,
										   have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 10');
cmp_ok($ref->{'tweak_inits'},'==',0,'retries_decide_what_to_do tweak_inits 10');
cmp_ok($ref->{'retry'} ,'==',0,'retries_decide_what_to_do retry 10');
cmp_ok($ref->{'reset_msfo'},'==',0,'retries_decide_what_to_do reset msfo 10');
cmp_ok($ref->{'run_is_accepted'},'==',1,'retries_decide_what_to_do is accepted 10');
cmp_ok($ref->{'message'},'eq','Not doing retry because run is accepted','retries_decide message');

#minim successful but local min
$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
										   minimization_successful => 1,
										   local_minimum => 1,
										   hessian_error => 0,
										   handle_hessian_npd => 0, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 0,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 0,
										   sigdigs_accepted => 0,
										   pass_picky => 0,
										   picky => 0,
										   handle_msfo => 0,
										   tweak_inits => 1,
										   min_retries => 0,
										   max_retries => 2,
										   try => 1,
										   have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 11');
cmp_ok($ref->{'tweak_inits'},'==',1,'retries_decide_what_to_do tweak_inits 11');
cmp_ok($ref->{'retry'} ,'==',1,'retries_decide_what_to_do retry 11');
cmp_ok($ref->{'reset_msfo'},'==',0,'retries_decide_what_to_do reset msfo 11');
cmp_ok($ref->{'run_is_accepted'},'==',0,'retries_decide_what_to_do is accepted 11');
cmp_ok($ref->{'message'},'eq','Doing retry because of local minimum','retries_decide message');

#accepted run but min_retries not reached
$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
										   minimization_successful => 1,
										   local_minimum => 0,
										   hessian_error => 0,
										   handle_hessian_npd => 0, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 0,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 0,
										   sigdigs_accepted => 0,
										   pass_picky => 1,
										   picky => 1,
										   handle_msfo => 1,
										   tweak_inits => 1,
										   min_retries => 2,
										   max_retries => 0,
										   try => 1,
										   have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 12');
cmp_ok($ref->{'tweak_inits'},'==',1,'retries_decide_what_to_do tweak_inits 12');
cmp_ok($ref->{'retry'} ,'==',1,'retries_decide_what_to_do retry 12');
cmp_ok($ref->{'reset_msfo'},'==',1,'retries_decide_what_to_do reset msfo 12');
cmp_ok($ref->{'run_is_accepted'},'==',1,'retries_decide_what_to_do is accepted 12');
cmp_ok($ref->{'message'},'eq','Doing retry because of min_retries not reached','retries_decide message');

#accepted run and min_retries reached
$ref = tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
										   minimization_successful => 0,
										   local_minimum => 0,
										   hessian_error => 0,
										   handle_hessian_npd => 1, 
										   round_error => 0,
										   cut_thetas_rounding_errors => 1,
										   maxevals_exceeded => 0,
										   cut_thetas_maxevals => 1,
										   sigdigs_accepted => 1,
										   pass_picky => 0,
										   picky => 0,
										   handle_msfo => 1,
										   tweak_inits => 1,
										   min_retries => 1,
										   max_retries => 3,
										   try => 1,
										   have_accepted_run => 0);
cmp_ok($ref->{'cut_thetas'},'==',0,'retries_decide_what_to_do cut_thetas 13');
cmp_ok($ref->{'tweak_inits'},'==',0,'retries_decide_what_to_do tweak_inits 13');
cmp_ok($ref->{'retry'} ,'==',0,'retries_decide_what_to_do retry 13');
cmp_ok($ref->{'reset_msfo'},'==',0,'retries_decide_what_to_do reset msfo 13');
cmp_ok($ref->{'run_is_accepted'},'==',1,'retries_decide_what_to_do is accepted 13');
cmp_ok($ref->{'message'},'eq','Not doing retry because run is accepted','retries_decide message');


dies_ok {tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													minimization_successful => 0,
													local_minimum => 0,
													hessian_error => 0,
													handle_hessian_npd => 1, 
													round_error => 0,
													cut_thetas_rounding_errors => 1,
													maxevals_exceeded => 0,
													cut_thetas_maxevals => 1,
													sigdigs_accepted => 1,
													pass_picky => 1,
													picky => 1,
													handle_msfo => 1,
													tweak_inits => 1,
													min_retries => 1,
													max_retries => 3,
													try => 1,
													have_accepted_run => 0)  } "pass_picky and not minimization_successful";

dies_ok {tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													minimization_successful => 1,
													local_minimum => 0,
													hessian_error => 0,
													handle_hessian_npd => 1, 
													round_error => 0,
													cut_thetas_rounding_errors => 1,
													maxevals_exceeded => 0,
													cut_thetas_maxevals => 1,
													sigdigs_accepted => 1,
													pass_picky => 1,
													picky => 1,
													handle_msfo => 1,
													tweak_inits => 1,
													min_retries => 1,
													max_retries => 3,
													try => 1,
													have_accepted_run => 0)  } "retries_decide_what_to_do sigdigs_accepted and minimization_successful";

dies_ok {tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													minimization_successful => 1,
													local_minimum => 0,
													hessian_error => 1,
													handle_hessian_npd => 1, 
													round_error => 0,
													cut_thetas_rounding_errors => 1,
													maxevals_exceeded => 0,
													cut_thetas_maxevals => 1,
													sigdigs_accepted => 0,
													pass_picky => 1,
													picky => 1,
													handle_msfo => 1,
													tweak_inits => 1,
													min_retries => 1,
													max_retries => 3,
													try => 1,
													have_accepted_run => 0)  } "retries_decide_what_to_do hessian_error and minimization_successful";

dies_ok {tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													minimization_successful => 1,
													local_minimum => 0,
													hessian_error => 0,
													handle_hessian_npd => 1, 
													round_error => 0,
													cut_thetas_rounding_errors => 1,
													maxevals_exceeded => 1,
													cut_thetas_maxevals => 1,
													sigdigs_accepted => 0,
													pass_picky => 0,
													picky => 0,
													handle_msfo => 1,
													tweak_inits => 1,
													min_retries => 1,
													max_retries => 3,
													try => 1,
													have_accepted_run => 0)  } "retries_decide_what_to_do maxevals_exceeded and minimization_successful";
dies_ok {tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													minimization_successful => 0,
													local_minimum => 0,
													hessian_error => 0,
													handle_hessian_npd => 1, 
													round_error => 0,
													cut_thetas_rounding_errors => 1,
													maxevals_exceeded => 0,
													cut_thetas_maxevals => 1,
													sigdigs_accepted => 1,
													pass_picky => 0,
													picky => 0,
													handle_msfo => 1,
													tweak_inits => 1,
													min_retries => -1,
													max_retries => 3,
													try => 1,
													have_accepted_run => 0)  } "retries_decide_what_to_do min_retries < 0";
dies_ok {tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													minimization_successful => 0,
													local_minimum => 0,
													hessian_error => 0,
													handle_hessian_npd => 1, 
													round_error => 0,
													cut_thetas_rounding_errors => 1,
													maxevals_exceeded => 0,
													cut_thetas_maxevals => 1,
													sigdigs_accepted => 0,
													pass_picky => 0,
													picky => 0,
													handle_msfo => 1,
													tweak_inits => 1,
													min_retries => 1,
													max_retries => -2,
													try => 1,
													have_accepted_run => 0)  } "retries_decide_what_to_do max_retries < 0";
dies_ok {tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													minimization_successful => 0,
													local_minimum => 0,
													hessian_error => 0,
													handle_hessian_npd => 1, 
													round_error => 0,
													cut_thetas_rounding_errors => 1,
													maxevals_exceeded => 0,
													cut_thetas_maxevals => 1,
													sigdigs_accepted => 0,
													pass_picky => 0,
													picky => 0,
													handle_msfo => 1,
													tweak_inits => 1,
													min_retries => 1,
													max_retries => 3,
													try => 2,
													have_accepted_run => 1)  } "retries_decide_what_to_do have_accepted_run and try > min_retries";

dies_ok {tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													minimization_successful => 1,
													local_minimum => 0,
													hessian_error => 0,
													handle_hessian_npd => 1, 
													round_error => 1,
													cut_thetas_rounding_errors => 1,
													maxevals_exceeded => 0,
													cut_thetas_maxevals => 1,
													sigdigs_accepted => 0,
													pass_picky => 0,
													picky => 0,
													handle_msfo => 1,
													tweak_inits => 1,
													min_retries => 1,
													max_retries => 3,
													try => 1,
													have_accepted_run => 0)  } "retries_decide_what_to_do round_error minimization_successful";

dies_ok {tool::modelfit::retries_decide_what_to_do( retries_probnum => 1,
													minimization_successful => 1,
													local_minimum => 0,
													hessian_error => 0,
													handle_hessian_npd => 1, 
													round_error => 0,
													cut_thetas_rounding_errors => 1,
													maxevals_exceeded => 0,
													cut_thetas_maxevals => 1,
													sigdigs_accepted => 0,
													pass_picky => 1,
													picky => 0,
													handle_msfo => 1,
													tweak_inits => 1,
													min_retries => 1,
													max_retries => 3,
													try => 1,
													have_accepted_run => 0)  } "retries_decide_what_to_do pass_picky and not picky";
 




done_testing;
