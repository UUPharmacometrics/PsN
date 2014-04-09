#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>2;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_cdd');
our $dir = "$tempdir/cdd_test";
my $model_dir = $includes::testfiledir;

copy_test_files($tempdir,["pheno5.mod", "pheno5.dta", "mox1.mod", "mox_simulated.csv"]);

my @commands = 
	($includes::cdd." -case_column=ID $tempdir/pheno5.mod -xv  -dir=$dir",
	 $includes::cdd." $tempdir/mox1.mod -case_column=DGRP  -dir=$dir",
	);

foreach my $command (@commands) {
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree(["$dir"]);
}
rmtree(["$dir"]);
remove_test_dir($tempdir);

done_testing();
