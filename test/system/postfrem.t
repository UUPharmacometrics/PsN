#!/etc/bin/perl

# Hash based testing of postfrem

use strict;
use warnings;
use File::Path 'rmtree';
use File::Copy::Recursive qw(dircopy);
#use Test::More tests=>1;
use Test::More;

use Data::Dumper;
use Digest::file qw(digest_file_hex);

use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use PsN;

my $interactive=0;
our $temp_dir = create_test_dir('system_postfrem');
chdir($temp_dir);
my $model_dir = $includes::testfiledir."/postfrem";
dircopy("$model_dir/frem_covstep", "frem_covstep");
dircopy("$model_dir/frem_sir", "frem_sir");

my $seed=588549; # important for hash tests

my @frem_dirs = (
    "frem_covstep",
    "frem_sir",
);

my @test_dirs = (
	"postfrem_covstep",
	# "postfrem_covstep_removed",
	"postfrem_sir",
);
my @commands = (
	# frem executed with: frem -covar=WT,DGRP -categorical=DGRP -no-check mox_frem.mod -no-run_sir -mceta=50
	get_command('postfrem') . " -frem_dir=frem_covstep -dir=$test_dirs[0] -seed=$seed",

	# same frem, but model_4.mod re-executed with $COV step commented out (test point estimate mode)
    # FIXME: model_1.mod expected for uncertainty propagation, fix and activate when postfrem has better such functionality
	# get_command('postfrem') . " -frem_dir=$model_dir/frem_covstep_removed -dir=$test_dirs[1] -seed=$seed",

	# same frem, but sir executed with: sir -covmat_input=proposal_density.cov -dir=sir (sir input test)
	get_command('postfrem') . " -frem_dir=frem_sir -sir_dir=frem_sir/sir/ -dir=$test_dirs[1] -seed=$seed",
);

my %hashes = (
	$test_dirs[0] => {
		"covdata.csv"                  =>  "8103ecd8f582e04d67d8b2930fe33cae8b114c7f",
        #"frem_condvar.csv"             =>  "3d6ef6f5d3f2053ba3f8f532a9d8dc3bd985b8f5",
		"id_covdata.csv"               =>  "64708b1ef82a785ad71e328fd428e12d3c45218c",
		"pardata.csv"                  =>  "8075ff01b0b886ba9a2484a4b9b335880dad380a",
		"sd_coefficients_summary.csv"  =>  "6a2a773720f3f6a84585a838d1c5f7a325231384",
	},
	$test_dirs[1] => {
		"covdata.csv"                  =>  "8103ecd8f582e04d67d8b2930fe33cae8b114c7f",
        #"frem_condvar.csv"             =>  "98b4268c1c33a67d52e5e7edd11591c1b24dd683",
		"frem_id_ratios.csv"           =>  "8ac2780b5a45b59dbb5f4b7773cf9ac3a56e4bb7",
		"frem_ratio.csv"               =>  "1a9936646f753c9f3fa55cc1de4340acced66709",
		"id_covdata.csv"               =>  "64708b1ef82a785ad71e328fd428e12d3c45218c",
		"pardata.csv"                  =>  "8075ff01b0b886ba9a2484a4b9b335880dad380a",
		"sd_coefficients_summary.csv"  =>  "d050af5fe21b6f460b7765bb42e138765f26aa5f",
	},
);

for my $i (0..$#commands) {
	my $command = $commands[$i];
	my $test_dir = $test_dirs[$i];
    my $frem_dir = $frem_dirs[$i];

	my  $rc = system($command);
	$rc = $rc >> 8;

	my $postfrem_passed = ok ($rc == 0, "$command");

	# execute deterministic file hashing tests if postfrem succeeded
    if ($PsN::dev) {
	    subtest "$test_dir hash tests" => sub {
		    plan 'skip_all' unless $postfrem_passed;
		    foreach my $file (keys %{$hashes{$test_dir}}) {
			    my $file_to_hash = "$frem_dir/$test_dir/$file";
			    is (digest_file_hex($file_to_hash, "SHA-1"), $hashes{$test_dir}->{$file}, "hash eq test : ".$file_to_hash);
		    }
	    }
    }
}

remove_test_dir($temp_dir);

done_testing();
