#!/etc/bin/perl

# Blackbox testing of crossval, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_crossval');
chdir($tempdir);
my $model_dir = $includes::testfiledir;

my $command = get_command('crossval') . " $model_dir/mox1.mod -groups=3 -clean=1 -seed=54321";

my  $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "crossval that should run ok");

remove_test_dir($tempdir);

done_testing();
