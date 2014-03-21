#!/etc/bin/perl

# Blackbox testing of crossval, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $dir = 'crossval_test';
my $model_dir = $includes::testfiledir;

rmtree([ "./$dir" ]);

my $command = $includes::crossval." $model_dir/mox1.mod -groups=3 -dir=$dir -clean=1";

my  $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "crossval that should run ok");

rmtree([ "./$dir" ]);

done_testing();
