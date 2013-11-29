#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>2;
use FindBin qw($Bin);
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $dir = 'bootstrap_test';
my $model_dir = "$Bin/../test_files";

my @a;
rmtree([ "./$dir" ]);

my $command = $includes::execute." $model_dir/pheno5.mod -dir=$dir";
my $rc = system($command);
rmtree([ "./$dir" ]);
$command = $includes::bootstrap." $model_dir/pheno5.mod -samples=10 -bca -seed=12345 -dir=$dir -no-skip_minim ";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 1 that should run ok");

rmtree([ "./$dir" ]);

$command = $includes::bootstrap." $model_dir/mox1.mod -samples=10 -stratify_on=DGRP -dir=$dir -no-skip_minim -no-skip_est";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 2 that should run ok");

rmtree([ "./$dir" ]);


done_testing();
