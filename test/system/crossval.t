#!/etc/bin/perl

# Blackbox testing of crossval, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);

my $path = "$Bin/../../bin/";
our $dir = 'crossval_test';
my $model_dir = "$Bin/../test_files";
our $private_test_files = $ENV{HOME}.'/.test_files';

rmtree([ "./$dir" ]);

my $command = $path."crossval $private_test_files/run_mx.mod -groups=3 -dir=$dir -clean=1";

my  $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "crossval that should run ok");

rmtree([ "./$dir" ]);

done_testing();
