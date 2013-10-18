#!/etc/bin/perl

# Blackbox testing of lasso, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);

our $dir = 'lasso_test';
my $model_dir = "$Bin/../test_files";
our $private_test_files = $ENV{HOME}.'/.test_files';

my @a;
rmtree([ "./$dir" ]);

my $command = "lasso $private_test_files/run_mx.mod -relations=CL:AGE-3,NYHA-1,SEX-1,CRCL-2,,V:AGE-2,ACE-1,DIG-1,NYHA-1,WT-2 -stratify_on=SEX -seed=1 -retries=0 -start_t=0.1 -step_t=0.05 -stop_t=0.2 -dir=$dir";

my  $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "lasso that should run ok");

rmtree([ "./$dir" ]);

done_testing();
