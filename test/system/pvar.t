#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>2;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_pvar');
my $dir = "$tempdir/pvar_test";
our $scm_file_dir = $includes::testfiledir . '/scm';

my $scm_command = get_command('scm') . " -no-zip config_normal.scm -directory=$dir -clean=1";
my $pvar_command = get_command('pvar') . " scmlog1.txt -parameters=CL,V ";

chdir $scm_file_dir;

my $rc = system $scm_command;
$rc = $rc >> 8;

ok ($rc == 0, "scm config normal crash test");

chdir $dir;
$rc = system $pvar_command;
$rc = $rc >> 8;

ok ($rc == 0, "Pvar crash test");

remove_test_dir($tempdir);

done_testing();
