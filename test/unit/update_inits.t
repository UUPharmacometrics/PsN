#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model::problem::estimation;
use model::problem::table;
use model::problem::problem;
use model;

open STDERR, '>', File::Spec->devnull();	# Silence STDERR

our $tempdir = create_test_dir('unit_updateinits');
my $modeldir = $includes::testfiledir;


is (model::get_run_number_string(filename => 'run1.mod'),'1',"get_run_number_string run1.mod");
is (model::get_run_number_string(filename => 'Run54a.ctl'),'54a',"get_run_number_string Run54a.ctl");
is (model::get_run_number_string(filename => 'pheno.mod'),undef,"get_run_number_string pheno.mod");
is (model::get_run_number_string(filename => 'RUN55.mod'),'55',"get_run_number_string RUN55.mod");
is (model::get_run_number_string(filename => 'Run54abc.ctl'),'54abc',"get_run_number_string Run54abc.ctl");
is (model::get_run_number_string(filename => 'run55'),undef,"get_run_number_string run55");
is (model::get_run_number_string(filename => 'run1.2.mod'),'1',"get_run_number_string run1.2.mod");

my $msfi = model::problem::msfi->new(record_arr => ['msf8']);
is ($msfi->options->[0]->name, 'msf8', "msfi before");

$msfi->renumber_msfi (numberstring => '9');
is ($msfi->options->[0]->name, 'msf9', "msfi after 1");

my $est = model::problem::estimation->new(record_arr => ['MAXEV=99 MSFO=msf8', 'ANY=SOUP']);
is ($est->options->[1]->value, 'msf8', "est msf before");

$est->renumber_msfo (numberstring => '9');
is ($est->options->[1]->value, 'msf9', "est msf after 1");

$est->renumber_msfo (numberstring => '11abc');
is ($est->options->[1]->value, 'msf11abc', "est msf after 2");

$est->renumber_msfo (numberstring => '9');
is ($est->options->[1]->value, 'msf9', "est msf after 3");

$est = model::problem::estimation->new(record_arr => ['MAXEV=99 MSFO=run50.msf', 'ANY=SOUP']);
is ($est->options->[1]->value, 'run50.msf', "est msf before 2");

$est->renumber_msfo (numberstring => '9');
is ($est->options->[1]->value, 'run9.msf', "est msf after 2");

$est->renumber_msfo (numberstring => '33abc');
is ($est->options->[1]->value, 'run33abc.msf', "est msf after 3");

$est->renumber_msfo (numberstring => '12');
is ($est->options->[1]->value, 'run12.msf', "est msf after 4");


is_deeply(model::get_xpose_runno_and_suffix(filename => 'patab01'),['01',''],'get_xpose_runno_and_suffix 1');
is_deeply(model::get_xpose_runno_and_suffix(filename => 'patab5a'),['5a',''],'get_xpose_runno_and_suffix 2');
is_deeply(model::get_xpose_runno_and_suffix(filename => 'cotab4.dat'),['4','.dat'],'get_xpose_runno_and_suffix 3');
is_deeply(model::get_xpose_runno_and_suffix(filename => 'catab87a.csv'),['87a','.csv'],'get_xpose_runno_and_suffix 4');
is_deeply(model::get_xpose_runno_and_suffix(filename => 'hejtab01'),[undef,''],'get_xpose_runno_and_suffix 5');


my $tab = model::problem::table->new(record_arr => ['ID DV MDV NOPRINT ONHEADER NOAPPEND FILE=patab01']);
is ($tab->options->[6]->value, 'patab01', "tab before");


$tab->renumber_file (numberstring => '9');
is ($tab->options->[6]->value, 'patab9', "tab after");

$tab = model::problem::table->new(record_arr => ['ID DV MDV NOPRINT ONHEADER FILE=mytab88.csv NOAPPEND']);
is ($tab->options->[5]->value, 'mytab88.csv', "tab csv before");

$tab->renumber_file (numberstring => '9');
is ($tab->options->[5]->value, 'mytab9.csv', "tab csv after");

$tab = model::problem::table->new(record_arr => ['ID DV MDV NOPRINT','FILE=tab55a.csv']);
is ($tab->options->[4]->value, 'tab55a.csv', "tab num-letter before");

$tab->renumber_file (numberstring => '09');
is ($tab->options->[4]->value, 'tab09.csv', "tab num-letter after");

$tab = model::problem::table->new(record_arr => ['ID DV MDV NOPRINT','FILE=tab55.csv']);
is ($tab->options->[4]->value, 'tab55.csv', "tab num-letter before 2");

$tab->renumber_file (numberstring => '09a');
is ($tab->options->[4]->value, 'tab09a.csv', "tab num-letter after 2");

$tab = model::problem::table->new(record_arr => ['ID DV MDV NOPRINT','FILE=output55abc.csv']);
is ($tab->options->[4]->value, 'output55abc.csv', "other num-letter before 3");

$tab->renumber_file (numberstring => '9');
is ($tab->options->[4]->value, 'output9.csv', "tab num-letter after 3");

my $prob = model::problem::problem->new(record_arr => ['$PROBLEM']);

$prob->add_comment(new_comment => 'added comment');
my $ref = $prob -> _format_record;
my @arr = split("\n",$ref->[0],2);
is ($arr[1],';added comment'."\n",'problem add comment');

$prob = model::problem::problem->new(record_arr => ['$PROBLEM DESCRIPTION']);

$prob->add_comment(new_comment => 'added comment');
my $ref = $prob -> _format_record;
my @arr = split("\n",$ref->[0],2);
is ($arr[1],';added comment'."\n",'problem add comment');


my $model = model->new(filename => "$modeldir/mox1.mod");

my $ref = $model-> get_hash_values_to_labels;

#arr over problems, hash omega sigma theta
is ($ref->[0]->{'theta'}->{'TVCL'},26.1,'TVCL');
is ($ref->[0]->{'theta'}->{'TVV'},100,'TVV');
is ($ref->[0]->{'theta'}->{'TVKA'},4.5,'TVKA');
is ($ref->[0]->{'theta'}->{'LAG'},0.2149,'LAG');
is ($ref->[0]->{'sigma'}->{'SIGMA(1,1)'},0.109,'SIGMA(1,1)');
is (eval($ref->[0]->{'omega'}->{'OMEGA(1,1)'}),eval(0.0750),'OMEGA(1,1)');
is ($ref->[0]->{'omega'}->{'OMEGA(2,1)'},0.0467,'OMEGA(2,1)');
is ($ref->[0]->{'omega'}->{'IIV (CL-V)'},0.0564,'OMEGA(2,2)');
is ($ref->[0]->{'omega'}->{'IIV KA'},2.82,'OMEGA(3,3)');
is ($ref->[0]->{'omega'}->{'IOV CL'},0.0147,'OMEGA(4,4)');
is ($ref->[0]->{'omega'}->{'IOV KA'},0.506,'OMEGA(6,6)');

#modify hash, then use in update_inits and check that got what is expected
$ref->[0]->{'theta'}->{'TVCL'} = 20;
$ref->[0]->{'theta'}->{'TVV'} = 90;
$ref->[0]->{'theta'}->{'TVKA'} = 5;
$ref->[0]->{'theta'}->{'LAG'} = 0.4;
$ref->[0]->{'sigma'}->{'SIGMA(1,1)'}= 0.2;
$ref->[0]->{'omega'}->{'OMEGA(1,1)'} = 0.4;
$ref->[0]->{'omega'}->{'OMEGA(2,1)'}= 0.01;
$ref->[0]->{'omega'}->{'IIV (CL-V)'} = 0.05;
$ref->[0]->{'omega'}->{'IIV KA'}= 3;
$ref->[0]->{'omega'}->{'IOV CL'}= 0.01;
$ref->[0]->{'omega'}->{'IOV KA'}= 0.5;


$model -> update_inits( from_hash => $ref->[0],
						problem_number => 1,
						ensure_posdef => 0,
						ignore_missing_parameters => 0 );

my $updated = $model-> get_hash_values_to_labels;

is(eval($updated->[0]->{'theta'}->{'TVCL'}),eval(20),'updated hash TVCL');
is(eval($updated->[0]->{'theta'}->{'TVV'}),eval(90),'updated hash TVV');
is(eval($updated->[0]->{'theta'}->{'TVKA'}),eval(5),'updated hash TVKA');
is(eval($updated->[0]->{'theta'}->{'LAG'}),eval(0.4),'updated hash LAG');
is(eval($updated->[0]->{'sigma'}->{'SIGMA(1,1)'}),eval(0.2),'updated hash SIGMA');
is(eval($updated->[0]->{'omega'}->{'OMEGA(1,1)'}),eval(0.4),'updated hash OM 1,1');
is(eval($updated->[0]->{'omega'}->{'OMEGA(2,1)'}),eval(0.01),'updated hash Om 2,1');
is(eval($updated->[0]->{'omega'}->{'IIV (CL-V)'}),eval(0.05),'updated hash IIV CL-V');
is(eval($updated->[0]->{'omega'}->{'IIV KA'}),eval(3),'updated hash IIV KA');
is(eval($updated->[0]->{'omega'}->{'IOV CL'}),eval(0.01),'updated hash IOV CL');
is(eval($updated->[0]->{'omega'}->{'IOV KA'}),eval(0.5),'updated hash IOV KA');

copy_test_files($tempdir,["pheno.mod", "pheno.lst",'mox1.lst','mox1.mod']);


my %hash;
$hash{'theta'}->{'THETA1'}=1;
$hash{'theta'}->{'THETA2'}=2;
$hash{'theta'}->{'THETA3'}=3;
$hash{'theta'}->{'THETA4'}=4;
$hash{'omega'}={};
$hash{'sigma'}={};

$model -> update_inits( from_hash => \%hash,
						problem_number => 1,
						match_labels => 0,
						ensure_posdef => 0,
						ignore_missing_parameters => 1 );

my $updated = $model-> get_hash_values_to_labels;

is(eval($updated->[0]->{'theta'}->{'TVCL'}),eval(1),'updated hash 2 TVCL');
is(eval($updated->[0]->{'theta'}->{'TVV'}),eval(2),'updated hash 2 TVV');
is(eval($updated->[0]->{'theta'}->{'TVKA'}),eval(3),'updated hash 2 TVKA');
is(eval($updated->[0]->{'theta'}->{'LAG'}),eval(4),'updated hash 2 LAG');


chdir($tempdir);
my @command_line = (
	get_command("update_inits") . " pheno.mod -out=run1.mod",
	get_command("update_inits") . " pheno.mod -out=run2.mod -comment=\"new comment\"",
	get_command("update_inits") . " mox1.mod -out=run3.mod -sigdig=3",
	get_command("update") . " mox1.mod -out=run4.mod -add_tags",
	get_command("update") . " pheno.mod -out=run5.mod -add_prior=1,1",
	get_command("update") . " run3.mod -out=run6.mod -cholesky=omega",
	get_command("update") . " run6.mod -out=run7.mod -cholesky=inverse -sigdig=3",
);
foreach my $i (0..$#command_line) {
	my $command= $command_line[$i];
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

#check that cholesky gave identity FIX blocks
my $cholmodel = model->new(filename => "run6.mod",
							ignore_missing_data => 1,
							ignore_missing_files => 1);
my $hash = $cholmodel-> get_hash_values_to_labels;

is(eval($hash->[0]->{'omega'}->{'OMEGA(1,1)'}),eval(1),'om 1,1');
is(eval($hash->[0]->{'omega'}->{'OMEGA(2,1)'}),eval(0),'om 2,1');
is(eval($hash->[0]->{'omega'}->{'IIV KA'}),eval(1),'om 3,3');
is(eval($hash->[0]->{'omega'}->{'IOV CL'}),eval(1),'om 4,4');
is(eval($hash->[0]->{'omega'}->{'IOV KA'}),eval(1),'om 6,6');

#check that start was not simple identity matrices
my $startmodel = model->new(filename => "run3.mod",
							ignore_missing_data => 1,
							ignore_missing_files => 1);
$hash = $startmodel-> get_hash_values_to_labels;
cmp_relative(eval($hash->[0]->{'omega'}->{'OMEGA(1,1)'}),0.416,3,'om start 1,1');
cmp_relative(eval($hash->[0]->{'omega'}->{'OMEGA(2,1)'}),0.390,3,'om start 2,1');
cmp_relative(eval($hash->[0]->{'omega'}->{'IIV KA'}),0.258,3,'om start 3,3');



#check that cholesky was invertible
my $endmodel = model->new(filename => "run7.mod",
						  ignore_missing_data => 1,
						  ignore_missing_files => 1);
my $hashend = $endmodel-> get_hash_values_to_labels;

is(eval($hash->[0]->{'omega'}->{'OMEGA(1,1)'}),eval($hashend->[0]->{'omega'}->{'OMEGA(1,1)'}),'inverse om 1,1');
is(eval($hash->[0]->{'omega'}->{'OMEGA(2,1)'}),eval($hashend->[0]->{'omega'}->{'OMEGA(2,1)'}),'inverse om 2,1');
is(eval($hash->[0]->{'omega'}->{'IIV KA'}),eval($hashend->[0]->{'omega'}->{'IIV KA'}),'inverse om 3,3');
is(eval($hash->[0]->{'omega'}->{'IOV CL'}),eval($hashend->[0]->{'omega'}->{'IOV CL'}),'inverse om 4,4');
is(eval($hash->[0]->{'omega'}->{'IOV KA'}),eval($hashend->[0]->{'omega'}->{'IOV KA'}),'inverse om 6,6');


chdir('..');

remove_test_dir($tempdir);




done_testing();
