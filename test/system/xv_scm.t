#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use File::Copy 'copy';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_xvscm');
our $dir = 'xv_scm_test';
my $model_dir = $includes::testfiledir;

my @needed=("$model_dir/config_xv_scm.scm",
			"$model_dir/mox1.mod",
			"$model_dir/mox_simulated.csv"			
	);
my $bootdir = "$tempdir/boot_xv_scm_test";

mkdir($bootdir);
foreach my $file (@needed){
	copy($file,"$bootdir/.");
}
chdir($bootdir);
my @scmcommands = 
	( get_command('xv_scm') . " -groups=3 -splits=2 -seed=12345 -max_step=2 -config=config_xv_scm.scm -dir=$dir");
foreach my $command (@scmcommands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([$dir]);
}

remove_test_dir($tempdir);

done_testing();
