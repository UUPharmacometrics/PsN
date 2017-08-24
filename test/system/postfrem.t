#!/etc/bin/perl

# Hash based testing of postfrem

use strict;
use warnings;
use File::Path 'rmtree';
#use Test::More tests=>1;
use Test::More;

use Data::Dumper;
use Digest::file qw(digest_file_hex);

use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

my $interactive=0;
our $temp_dir = create_test_dir('system_postfrem');
chdir($temp_dir);
my $model_dir = $includes::testfiledir."/postfrem";

my $seed=588549; # important for hash tests

# my ($major,$minor,$dirt) = get_major_minor_nm_version();
# if ($major < 7 or ($major == 7 and $minor < 3)){
# }
my @test_dirs = (
	"postfrem_covstep",
	# "postfrem_covstep_removed",
	"postfrem_sir",
);
my @commands = (
	# frem executed with: frem -covar=WT,DGRP -categorical=DGRP -no-check mox_frem.mod -no-run_sir -mceta=50
	get_command('postfrem') . " -frem_dir=$model_dir/frem_covstep -dir=$test_dirs[0] -seed=$seed",

	# same frem, but model_4.mod re-executed with $COV step commented out (test point estimate mode)
    # FIXME: model_1.mod expected for uncertainty propagation, fix and activate when postfrem has better such functionality
	# get_command('postfrem') . " -frem_dir=$model_dir/frem_covstep_removed -dir=$test_dirs[1] -seed=$seed",

	# same frem, but sir executed with: sir -covmat_input=proposal_density.cov -dir=sir (sir input test)
	get_command('postfrem') . " -frem_dir=$model_dir/frem_sir -sir_dir=$model_dir/frem_sir/sir/ -dir=$test_dirs[1] -seed=$seed",
);

my %hashes = (
	$test_dirs[0] => {
		"covdata.csv"                  =>  "0f49a89179883ce380f1d6fbde2cb5089092fc38",
		"frem_condvar.csv"             =>  "80da908280c1cf19ce7a950a478797c8ca82b0f5",
		"frem_id_ratios.csv"           =>  "91e08b904639701d5a3a0c491a25fe9fd0ac7c5e",
		"frem_ratio.csv"               =>  "45754b1ec8656aea383e7e3384a34f31f73a4bf3",
		"id_covdata.csv"               =>  "64708b1ef82a785ad71e328fd428e12d3c45218c",
		"pardata.csv"                  =>  "31040d07f6f37c0cd886744bb23a416b859c49d1",
		"sd_coefficients_summary.csv"  =>  "ea8e70db4286e1b95187a75a4ae4e576e0ba4bbd",
	},
	# $test_dirs[1] => {
    #     "covdata.csv"                  =>  "0f49a89179883ce380f1d6fbde2cb5089092fc38",
    #     "frem_condvar.csv"             =>  "4c49d12328eca280d5dfcce1aba00ff7fd6144e5",
    #     "frem_id_ratios.csv"           =>  "1f60d048baff6b6ac82a4e340a988d2bb54c373e",
    #     "frem_ratio.csv"               =>  "0dbca5f593ac5cbc166a45d794e533c37af7d091",
    #     "id_covdata.csv"               =>  "64708b1ef82a785ad71e328fd428e12d3c45218c",
    #     "pardata.csv"                  =>  "31040d07f6f37c0cd886744bb23a416b859c49d1",
    #     "sd_coefficients_summary.csv"  =>  "a347cdb8227c860ceaa7107de9f956f576d43849",
	# },
	$test_dirs[1] => {
		"covdata.csv"                  =>  "0f49a89179883ce380f1d6fbde2cb5089092fc38",
		"frem_condvar.csv"             =>  "b57cc0b0651a2886d4097355eaea4d59df730332",
		"frem_id_ratios.csv"           =>  "52efebf8f48f7fcae133f2ccea6b93df0e369448",
		"frem_ratio.csv"               =>  "18c92027fe883e151dfbd12e9a2a08657457574e",
		"id_covdata.csv"               =>  "64708b1ef82a785ad71e328fd428e12d3c45218c",
		"pardata.csv"                  =>  "31040d07f6f37c0cd886744bb23a416b859c49d1",
		"sd_coefficients_summary.csv"  =>  "d4d23c20626ef905f0039b00ac0ebb9b77e124fe",
	},
);

for my $i (0..$#commands) {
	my $command = $commands[$i];
	my $test_dir = $test_dirs[$i];

	my  $rc = system($command);
	$rc = $rc >> 8;

	my $postfrem_passed = ok ($rc == 0, "$command");

	# execute deterministic file hashing tests if postfrem succeeded
	subtest "$test_dir hash tests" => sub {
		plan 'skip_all' unless $postfrem_passed;
		foreach my $file (keys %{$hashes{$test_dir}}) {
			my $file_to_hash = "$test_dir/$file";
			ok ($hashes{$test_dir}->{$file} eq digest_file_hex($file_to_hash, "SHA-1"), "hash eq test : ".$file_to_hash);
		}
	}
}

print "\n";
if (Test::More->builder->is_passing)
{
    print "all tests succeded!\n";
	remove_test_dir($temp_dir);
} else {
    print "tests failed, execution directory '$temp_dir' not cleaned\n";
}

done_testing();
