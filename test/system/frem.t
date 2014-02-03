#!/etc/bin/perl

# Blackbox testing of frem, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use FindBin qw($Bin);
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $dir = 'frem_test';
my $model_dir = "../test_files";

rmtree([ "./$dir" ]);

	 
my $command = $includes::frem." -time_var=WT,NYHA -occ=VISI -param=PHI -invar=SEX -vpc $model_dir/mox_no_bov.mod -est=3  -dir=$dir";

my  $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "frem that should run ok");

#rmtree([ "./$dir" ]);


done_testing();
