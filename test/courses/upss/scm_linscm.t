#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

our $tempdir = create_test_dir('courses_upss_scm_linscm');
my $model_dir = "$Bin/scm_linearizedscm_files";
my @needed = <$model_dir/*>;
foreach my $file (@needed) {
	cp($file, $tempdir . '/.');
}
chdir($tempdir);

my @command_list = (
	[$includes::scm." run100.scm -dir=run100_nl","HO SCM linearized SCM task A i nonlin"],
	[$includes::scm." run101.scm -dir=run101_lin","HO SCM linearized SCM task A i linear"],
	[$includes::scm." run1.scm -dir=run1_nl","HO SCM linearized SCM task B i nonlin"],
	[$includes::scm." run1_lin.scm -dir=run1_lin","HO SCM linearized SCM task B i linear"],
	[$includes::scm." run2_1.scm -dir=run2_1_nl","HO SCM lin SCM extra credit task 1 i nonlin"],
	[$includes::scm." run2_1_lin.scm -dir=run2_1_lin","HO SCM lin SCM extra credit task 1 i linear"],
	[$includes::scm." run2_2.scm -dir=run2_2_nl","HO SCM lin SCM extra credit task 2 i nonlin"],
	[$includes::scm." run2_2_lin.scm -dir=run2_2_lin","HO SCM lin SCM extra credit task 2 i linear"],
	[$includes::scm." run2_3.scm -dir=run2_3_nl","HO SCM lin SCM extra credit task 3 i nonlin"],
	[$includes::scm." run2_3_lin.scm -dir=run2_3_lin","HO SCM lin SCM extra credit task 3 i linear"],
	[$includes::scm." run2_4.scm -dir=run2_4_nl","HO SCM lin SCM extra credit task 4 i nonlin"],
	[$includes::scm." run2_4_lin.scm -dir=run2_4_lin","HO SCM lin SCM extra credit task 4 i linear"]
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
