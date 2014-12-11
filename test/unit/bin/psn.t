#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

#our $tempdir = create_test_dir('unit_updateinits');
#my $modeldir = $includes::testfiledir;

#copy_test_files($tempdir,["pheno.mod", "pheno.lst",'mox1.lst','mox1.mod']);

#chdir($tempdir);
my @command_line = (
	get_command("psn") . " -nm_versions",
	get_command("psn_options") . " -nm_versions",
);
foreach my $i (0..$#command_line) {
	my $command= $command_line[$i];
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}
#chdir('..');

#remove_test_dir($tempdir);




done_testing();
