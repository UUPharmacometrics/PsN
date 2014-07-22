#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

our $tempdir = create_test_dir('courses_upss_diseaseprogression');
my $model_dir = "$Bin/disease_progression_files";
my @needed = <$model_dir/*>;
foreach my $file (@needed) {
	cp($file, $tempdir . '/.');
}
chdir($tempdir);
#note to Siti update_inits by default overwrites .mod, so do not need to set -out if is input model
my @command_list = (
	[$includes::execute." run1.mod -dir=run1","HO Disease Progression task 1a 1"],
	[$includes::sumo." run1.lst","HO Disease Progression task 1a 2"],
#	[$includes::vpc." run1.mod -lst=run1.lst -samples=200 -dir=vpc1 -bin_by_count=0 -idv=TVIS -bin_array=.5,1.5,3,6,10,12.5,13.5,15,18,23,24.5,25.5,27,30 -stratify_on=STRT -seed=123","HO Disease Progression task 1b"],
	[$includes::vpc." run1.mod -samples=200 -dir=vpc1 -idv=TVIS -auto_bin=15 -stratify_on=STRT -seed=123","HO Disease Progression task 1b"],
	[$includes::execute." run2.mod -dir=run2","HO Disease Progression task 1c 1"],
	[$includes::execute." run3.mod -dir=run3","HO Disease Progression task 1c 2"],
	[$includes::update_inits." run4.mod run2.lst","HO Disease Progression task 1c 3"],
	[$includes::execute." run4.mod -dir=run4","HO Disease Progression task 1c 4"],
	[$includes::execute." run5.mod -dir=run5","HO Disease Progression task 2a"],
	[$includes::execute." run6.mod -dir=run6","HO Disease Progression task 2b"],
	[$includes::execute." run7.mod -dir=run7","HO Disease Progression task 2c"],
	[$includes::execute." run8.mod -dir=run8","HO Disease Progression task 2d"],
	[$includes::execute." run9.mod -dir=run9","HO Disease Progression task 3a"],
	[$includes::execute." run10.mod -dir=run10","HO Disease Progression task 3b"],
#	[$includes::vpc." run9.mod -lst=run9.lst -samples=200 -dir=vpc9 -bin_by_count=0 -idv=TVIS -bin_array=.5,1.5,3,6,10,12.5,13.5,15,18,23,24.5,25.5,27,30 -stratify_on=STRT -seed=123 ","HO Disease Progression task 3c"]
	[$includes::vpc." run9.mod -samples=200 -dir=vpc9 -idv=TVIS -auto_bin=15 -stratify_on=STRT -seed=123","HO Disease Progression task 3c"]
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
