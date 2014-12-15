#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

my @command_line = (
	get_command("psn") . " -nm_versions",
	get_command("psn_options") . " -nm_version",
    get_command("psn_clean") . " -h",
);
foreach my $i (0..$#command_line) {
	my $command= $command_line[$i];
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

done_testing();
