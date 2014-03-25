#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir;
our $dir = "boot_scm_test";
my $model_dir = $includes::testfiledir;

my @needed=("$model_dir/scm/pheno_with_cov.mod",
			"$model_dir/scm/pheno_ch.csv",
			"$model_dir/config_xv_scm.scm",
			"$model_dir/mox1.mod",
			"$model_dir/scm_config.scm",
			"$model_dir/mox_simulated.csv"			
	);
my $bootdir = "$tempdir/boot_xv_scm_test";

mkdir($bootdir);
foreach my $file (@needed) {
	cp($file, "$bootdir/.");
}
chdir($bootdir);
my @scmcommands = 
	(
	 $includes::boot_scm." -samples=2 scm_config.scm -dummy_cov=WGT -dir=$dir",
	);
foreach my $command (@scmcommands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	print "L\n"; #Something is fishy when test harness runs this test. To print something extra here fixes it.	
	ok ($rc == 0, "$command, should run ok");
	rmtree(["./$dir"]);
}

remove_test_dir;

done_testing();
