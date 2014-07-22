#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

our $tempdir = create_test_dir('courses_upss_xvscm');
my $model_dir = "$Bin/xvscm_files";
my @needed = <$model_dir/*>;
foreach my $file (@needed) {
	cp($file, $tempdir . '/.');
}
chdir($tempdir);

my @command_list = (
	[$includes::xv_scm." -config_file=run_lin.scm -dir=run_lin -groups=5 -splits=3 -max_steps=3","HO XV_SCM task 1"]
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
