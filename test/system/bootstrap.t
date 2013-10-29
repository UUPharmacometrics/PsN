#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);

our $dir = 'bootstrap_test';
my $path = "$Bin/../../bin/";
my $model_dir = "$Bin/../test_files";
our $private_test_files = $ENV{HOME}.'/.test_files';

my @a;
rmtree([ "./$dir" ]);

my $command = $path."execute $model_dir/pheno5.mod -dir=$dir";
my $rc = system($command);
rmtree([ "./$dir" ]);
$command = $path."bootstrap $model_dir/pheno5.mod -samples=10 -bca -seed=12345 -dir=$dir -no-skip_minim";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 1 that should run ok");

rmtree([ "./$dir" ]);

$command = $path."bootstrap $private_test_files/moxonidine.mod -samples=4 -stratify_on=DGRP -dir=$dir";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 2 that should run ok");

rmtree([ "./$dir" ]);


done_testing();
