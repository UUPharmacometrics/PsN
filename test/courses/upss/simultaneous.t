#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

our $tempdir = create_test_dir('courses_upss_simultaneous');
my $model_dir = "$Bin/simultaneous_files";
my @needed = <$model_dir/*>;
foreach my $file (@needed) {
	cp($file, $tempdir . '/.');
}
chdir($tempdir);

my @command_list = (
	[$includes::execute." run81.mod -model_dir_name","HO Simultaneous task 1 a"],
	[$includes::execute." run83.mod -model_dir_name","HO Simultaneous task 1 b"],
	[$includes::execute." run84.mod -model_dir_name","HO Simultaneous task 2 a"],
	[$includes::execute." run85.mod -model_dir_name","HO Simultaneous task 2 b"],
	[$includes::execute." run86.mod -model_dir_name","HO Simultaneous task 2 c"],
	[$includes::execute." run87.mod -model_dir_name","HO Simultaneous task 2 d"],
	[$includes::execute." run88.mod -model_dir_name","HO Simultaneous task 2 e"],
	[$includes::execute." run89.mod -model_dir_name","HO Simultaneous task 2 f"],
	[$includes::execute." run810.mod -model_dir_name","HO Simultaneous task 2 g"],
	[$includes::execute." run811.mod -model_dir_name","HO Simultaneous task 2 h"],
	[$includes::vpc." run811.mod -tte=RTTE -flip_comments -samples=30 -compress -clean=3 -stratify_on=FLG,DV1,TRET,HR,CONC,CE,AGE,SEX,WT,CRCL","HO Simultaneous task 4"]
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
