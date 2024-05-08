#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use List::Util qw(first);
use Config;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use ui;

our $tempdir = create_test_dir('system_record_order');
my $model_dir = $includes::testfiledir;
#put mod in testdir so that .ext etc in testfiledir are not modified
copy_test_files($tempdir,["pheno.dta","illegal_order.mod"]);


# Note: do not use same -dir name, errors on slurm (file sync?)

#test illegal record order crashes
chdir($tempdir);
my $command = get_command('execute') . " -no-abort_on_fail illegal_order.mod -dir=test1 -silent";
print "Running $command\n";
my $rc = system($command);
$rc = $rc >> 8;
ok ($rc == 0, "$command, illegal order");
ok ((not -e 'illegal_order.lst'), "lst not exist illegal order"); #should not exist because of nmtranerr
unlink('illegal_order.lst');

$command = get_command('execute') . " illegal_order.mod -psn_record_order -dir=test2";
print "Running $command\n";
$rc = system($command);
$rc = $rc >> 8;
ok ($rc == 0, "$command, illegal order fix with psn_record_order");
ok ((-e 'illegal_order.lst'), "lst exists illegal order fix with psn_record_order"); #should exist after setting psn_record_order
unlink('illegal_order.lst');

$command = get_command('execute') . " illegal_order.mod -omega_before_pk -dir=test3";
print "Running $command\n";
$rc = system($command);
$rc = $rc >> 8;
ok ($rc == 0, "$command, illegal order fix with omega_before_pk");
ok ((-e 'illegal_order.lst'), "lst exists illegal order fix with omega_before_pk");
unlink('illegal_order.lst');


remove_test_dir($tempdir);

done_testing();
