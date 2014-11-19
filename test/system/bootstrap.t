#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>4;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_bootstrap');
our $dir = "$tempdir/bootstrap_test";
my $model_dir = $includes::testfiledir;

copy_test_files($tempdir,["pheno5.mod", "pheno5.dta", "mox1.mod", "mox_simulated.csv"]);

my $command = get_command('execute') . " $tempdir/pheno5.mod -dir=$dir";
my $rc = system($command);
rmtree([$dir]);
$command = get_command('bootstrap') . " $tempdir/pheno5.mod -samples=10 -bca -seed=12345 -dir=$dir -no-skip_minim ";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 1 that should run ok");

$command = get_command('bootstrap') . " $tempdir/pheno5.mod -samples=10 -seed=12345 -dir=$dir -dofv ";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 1b that should run ok");

rmtree([$dir]);

$command = get_command('bootstrap') . " $tempdir/mox1.mod -samples=10 -stratify_on=DGRP -dir=$dir -no-skip_minim -no-skip_est -clean=3";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 2 that should run ok");
unlink($dir.'/m1/bs_pr1_1.lst');
unlink($dir.'/m1/bs_pr1_1.ext');
$command = get_command('bootstrap') . " $tempdir/mox1.mod -samples=10 -stratify_on=DGRP -dir=$dir -no-skip_minim  -no-skip_est";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 3 that should run ok");


remove_test_dir($tempdir);

done_testing();
