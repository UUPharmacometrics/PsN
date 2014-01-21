#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use FindBin qw($Bin);

my $dir = 'pvar_test';
our $scm_file_dir="$Bin/../test_files/scm";

my $scm_command = "scm config_normal.scm -directory=$dir";
my $pvar_command = "pvar scmlog1.txt -parameters=CL,V -directory=$dir";



chdir $scm_file_dir;
rmtree([ "./$dir" ]);

system $scm_command;

chdir $dir;
my $rc = system $pvar_command;
$rc = $rc >> 8;

ok ($rc == 0, "Pvar crash test");

chdir "..";
rmtree([ "./$dir" ]);
