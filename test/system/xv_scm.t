#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition


our $dir = 'xv_scm_test';
my $model_dir = $includes::testfiledir;

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
	( $includes::xv_scm." -groups=3 -splits=2 -seed=12345 -max_step=2 -config=config_xv_scm.scm -dir=$dir");
foreach my $command (@scmcommands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([ "./$dir" ]);
}
chdir('..');
rmtree([ "./$bootdir" ]);

done_testing();
