#!/etc/bin/perl

# Blackbox testing of update and update_inits, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests => 2;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition


our $tempdir = create_test_dir('system_update_inits');
our $dir = "update_inits_test";
my $model_dir = $includes::testfiledir;
copy_test_files($tempdir, ["pheno.mod", "pheno.lst"]);

chdir($tempdir);

my $command2 = get_command('update') . " pheno.mod";
my $command1 = get_command('update_inits') . " pheno.mod";

my  $rc = system($command1);
$rc = $rc >> 8;

ok ($rc == 0, "update that should run ok");
my  $rc = system($command2);
$rc = $rc >> 8;

ok ($rc == 0, "update_inits that should run ok");

chdir('..');
remove_test_dir($tempdir);

done_testing();
