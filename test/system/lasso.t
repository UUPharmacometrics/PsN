#!/etc/bin/perl

# Blackbox testing of lasso, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition


our $tempdir = create_test_dir('system_lasso');
chdir($tempdir);
my $model_dir = $includes::testfiledir;

my @commands = 
	(
	 get_command('lasso') . " $model_dir/pheno.mod -relations=CL:WGT-2,APGR-3,,V:APGR-1 -adaptive -start_t=0 -stop_t=1.5 -seed=790  ",
	 get_command('lasso') . " $model_dir/pheno_cond.mod -relations=CL:WGT-2,,V:WGT-2 -adaptive -adjusted -start_t=0.5 -stop_t=1.5 -seed=790743 ",
	 get_command('lasso') . " $model_dir/mox1.mod -relations=CL:SEX-1,,V:AGE-2,WT-2 -stratify_on=SEX -seed=790743 -retries=2 -start_t=0.1 -step_t=0.05 -stop_t=0.15  ",
	);

for (my $i=0; $i<scalar(@commands); $i++){
	my $command = $commands[$i];
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

remove_test_dir($tempdir);

done_testing();
