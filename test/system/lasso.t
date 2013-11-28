#!/etc/bin/perl

# Blackbox testing of lasso, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition


our $dir = 'lasso_test';
my $model_dir = "$Bin/../test_files";

my @a;
rmtree([ "./$dir" ]);

my $command = $includes::path."lasso $model_dir/mox1.mod -relations=CL:AGE-3,DIU-1,SEX-1,CRCL-2,,V:AGE-2,ACE-1,DIG-1,DIU-1,WT-2 -stratify_on=SEX -seed=1 -retries=0 -start_t=0.1 -step_t=0.05 -stop_t=0.15 -dir=$dir";

my  $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "lasso that should run ok");

rmtree([ "./$dir" ]);

done_testing();
