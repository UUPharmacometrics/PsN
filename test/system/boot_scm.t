#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>3;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_bootscm');
our $dir = "boot_scm_test";
my $model_dir = $includes::testfiledir;

my @needed=("$model_dir/scm/pheno_with_cov.mod",
			"$model_dir/scm/pheno_ch.csv",
			"$model_dir/scm/config_foce.scm",
			"$model_dir/scm_config.scm",
	);
my $bootdir = "$tempdir/boot_scm_test";

mkdir($bootdir);
foreach my $file (@needed) {
	cp($file, "$bootdir/.");
}
chdir($bootdir);
my @scmcommands = 
	(
	 $includes::boot_scm." -samples=2 scm_config.scm -dummy_cov=WGT -stratify_on=CVD1 -dir=$dir",
	 $includes::boot_scm." -samples=2 config_foce.scm -stratify_on=CVD2 -dir=$dir ",
	 $includes::boot_scm." -samples=2 config_foce.scm -stratify_on=CVD2 -dir=$dir -methodA",
	);
foreach my $command (@scmcommands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	print "\n"; #Something is fishy when test harness runs this test. To print something extra here fixes it.	
	ok ($rc == 0, "$command, should run ok");
	rmtree(["./$dir"]);
}

remove_test_dir($tempdir);

done_testing();
