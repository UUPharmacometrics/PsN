#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
require File::Copy::Recursive;

our $toolname = 'bootstrap';
our $tempdir = create_test_dir('rplots_'.$toolname);

my $input_dir = $includes::testfiledir.'/rplots/'.$toolname.'/run1';

unless (File::Copy::Recursive::dircopy($input_dir, $tempdir)) {
	die " Could not copy contents of $input_dir to $tempdir : $!\n";
}

chdir($tempdir);

my $command = get_command('bootstrap') . " run1.mod -dir=rundir -dofv -rplots=2 -summarize";

my $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 1 that should run ok");
ok (-e 'rundir/PsN_bootstrap_plots.pdf','pdf exists. Check that 5 plots in '.$tempdir.'rundir/PsN_bootstrap_plots.pdf'); 



done_testing();
