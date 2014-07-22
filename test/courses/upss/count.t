#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

our $tempdir = create_test_dir('courses_upss_count');
my $model_dir = "$Bin/count_files";
my @needed = <$model_dir/*>;
foreach my $file (@needed) {
	cp($file, $tempdir . '/.');
}
chdir($tempdir);

my @command_list = (
	[$includes::execute." run65.mod -model_dir_name","HO Count task 1"],
	[$includes::execute." run66.mod -model_dir_name","HO Count task 3a"],
	[$includes::execute." run66sim.mod -model_dir_name","HO Count task 3b"],
	[$includes::vpc." run66vpc.mod -lst=run66.lst -samples=100 -dir=vpc66a -seed=123 -levels=0,1,2,3,4 -nopred -dv=HC","HO Count task 3c"],
	[$includes::vpc." run66vpc.mod -lst=run66.lst -samples=100 -dir=vpc66b -seed=123 -levels=0,1,2,3,4 -idv=DOSE -nopred -dv=HC","HO Count task 3d"],
	[$includes::execute." run67.mod -model_dir_name","HO Count task 4a"],
	[$includes::execute." run67sim.mod -model_dir_name","HO Count task 4b"],
	[$includes::vpc." run67vpc.mod -lst=run67.lst -samples=100 -dir=vpc67a -seed=123 -levels=0,1,2,3,4 -nopred -dv=HC","HO Count task 4c"],
	[$includes::vpc." run67vpc.mod -lst=run67.lst -samples=100 -dir=vpc67b -seed=123 -levels=0,1,2,3,4 -idv=DOSE -nopred -dv=HC","HO Count task 4d"],
	[$includes::execute." run68.mod -model_dir_name","HO Count task 5"],
	[$includes::execute." run69.mod -model_dir_name","HO Count task extra credit a"],
	[$includes::execute." run70.mod -model_dir_name","HO Count task extra credit b"]
	);
plan tests => scalar(@command_list);

foreach my $ref (@command_list) {
	my $command = $ref->[0];
	my $comment = $ref->[1];
	print "Running $comment:\n$command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$comment ");
}

chdir($Bin);
remove_test_dir($tempdir);

done_testing();
