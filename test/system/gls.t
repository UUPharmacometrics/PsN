#!/etc/bin/perl

# Blackbox testing of crossval, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);

our $dir = 'gls_test';
my $model_dir = "$Bin/../test_files";
our $private_test_files = $ENV{HOME}.'/.test_files';

rmtree([ "./$dir" ]);

my $command = "gls $model_dir/glstags.mod -samples=3 -set_simest -ind_shrink -dir=$dir";

my  $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "gls that should run ok");

rmtree([ "./$dir" ]);

done_testing();
