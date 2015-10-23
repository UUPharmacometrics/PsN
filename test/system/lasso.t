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
our $dir = "$tempdir/lasso_test";
my $model_dir = $includes::testfiledir;

my @commands = 
	(
	 get_command('lasso') . " $model_dir/pheno.mod -relations=CL:WGT-2,APGR-3,,V:APGR-1 -adaptive -seed=1 -start_t=0 -stop_t=1.5 -seed=790 -dir=$dir ",
	 get_command('lasso') . " $model_dir/pheno_cond.mod -relations=CL:WGT-2,,V:WGT-2 -adaptive -adjusted -seed=1 -start_t=0.5 -stop_t=1.5 -seed=790743 -dir=$dir ",
	 get_command('lasso') . " $model_dir/mox1.mod -relations=CL:AGE-3,SEX-1,CRCL-2,,V:AGE-2,ACE-1,DIG-1,WT-2 -stratify_on=SEX -seed=1 -retries=0 -start_t=0.1 -step_t=0.05 -stop_t=0.15 -dir=$dir ",
	);

for (my $i=0; $i<scalar(@commands); $i++){
	my $command = $commands[$i];
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([$dir]);# if ($i>1);
}

remove_test_dir($tempdir);

done_testing();
