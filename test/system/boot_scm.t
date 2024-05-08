#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_bootscm');
our $dir = "boot_scm_test";
my $model_dir = $includes::testfiledir;

copy_test_files($tempdir,["scm/pheno_with_cov.mod","scm/pheno_ignore.mod","scm/config_ignore.scm","scm/pheno_ch.csv","scm/config_foce.scm","scm/config_time_varying.scm",
				"scm_config.scm"]);

chdir($tempdir);
my @scmcommands = 
	(
	 get_command('boot_scm') . " -samples=2 config_time_varying.scm -dummy_cov=WGT -stratify_on=APGR ",
	 get_command('boot_scm') . " -samples=2 scm_config.scm -dummy_cov=WGT ",
	 get_command('boot_scm') . " -samples=2 config_ignore.scm -stratify_on=CVD2 ",
	 get_command('boot_scm') . " -samples=2 config_foce.scm -stratify_on=CVD2 -nm_out=ext,cov -methodA",
	);
foreach my $command (@scmcommands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	print "\n"; #Something is fishy when test harness runs this test. To print something extra here fixes it.	
	ok ($rc == 0, "$command, should run ok");
}

remove_test_dir($tempdir);

done_testing();
