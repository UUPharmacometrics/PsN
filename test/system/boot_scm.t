#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use FindBin qw($Bin);
use File::Copy 'cp';
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $dir = 'boot_scm_test';
my $model_dir = "$Bin/../test_files";

my @needed=("$model_dir/scm/pheno_with_cov.mod",
			"$model_dir/scm/pheno_ch.csv",
			"$model_dir/config_xv_scm.scm",
			"$model_dir/mox1.mod",
			"$model_dir/scm_config.scm",
			"$model_dir/mox_simulated.csv"			
	);
my $bootdir='boot_xv_scm_test';
rmtree([ "./$bootdir" ]);

mkdir($bootdir);
foreach my $file (@needed){
	cp($file,"$bootdir/.");
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
	rmtree([ "./$dir" ]);
}
chdir('..');
rmtree([ "./$bootdir" ]);

done_testing();
