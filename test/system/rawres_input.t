#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>6;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

#black box testing of rawres_input functionality

our $tempdir = create_test_dir('system_rawresinput');
our $dir = "$tempdir"."rawres_test";
our $bootdir = "$tempdir"."boot_test";
our $ssedir = "$tempdir"."sse_test";
my $model_dir = $includes::testfiledir;

my @commands = 
	($includes::bootstrap." -samples=5 $model_dir/pheno.mod -dir=$bootdir",
	 $includes::parallel_retries." $model_dir/pheno.mod -dir=$dir -samples=2 -rawres_input=$bootdir/raw_results_pheno.csv -no-display",
	 $includes::sir." $model_dir/pheno.mod -rawres_input=$bootdir/raw_results_pheno.csv -samples=3 -offset_rawres=1 -in_filter=minimization_successful.eq.1 -resamples=20 -with_replacement -dir=$dir",
	 $includes::sse." $model_dir/pheno.mod -rawres_input=$bootdir/raw_results_pheno.csv -samples=5 -no-est -dir=$dir",
	 $includes::sse." $model_dir/pheno.mod  -samples=20 -dir=$ssedir",
	 $includes::vpc." $model_dir/pheno.mod -rawres_input=$ssedir/raw_results_pheno.csv -offset=0 -samples=20 -auto_bin=unique -dir=$dir");

foreach my $command (@commands) {
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([$dir]);
}

remove_test_dir($tempdir);

done_testing();
