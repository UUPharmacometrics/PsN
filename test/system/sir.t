#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_sir');



my $dir = "$tempdir/sir_test";
my $model_dir = $includes::testfiledir;

my @commands = 
	(
	 get_command('sir') . " $model_dir/pheno.mod -samples=50,100 -resamples=25,50 -no-boxcox -dir=$dir", 
	 get_command('sir') . " $model_dir/pheno.mod -samples=50 -resamples=25 -auto_rawres=0.9 -seed=50032 -dir=$dir",
	 get_command('sir') . " $model_dir/sir/localmin.mod -samples=50,100 -resamples=25,50 -dir=$dir",
	 get_command('sir') . " $model_dir/pheno.mod -samples=50,100,100 -resamples=25,50,50 -covmat_input=$model_dir/pheno_fake.cov -dir=$dir",
	 get_command('sir') . " $model_dir/pheno.mod -samples=100 -resamples=50 -covmat_input=$model_dir/pheno_fake.cov -dir=$dir",
	 get_command('sir') . " $model_dir/pheno.mod -samples=10 -resamples=5 -covmat_input=$model_dir/pheno_fake_2.cov -no-copy_data -dir=$dir",
	 get_command('sir') . " $model_dir/mox_sir_block2.mod -samples=50 -resamples=25 -dir=$dir -problems_per_file=10",
	 get_command('sir') . " $model_dir/mox_sir_block2.mod -samples=50,50 -resamples=25,25 -dir=$dir",
	 get_command('sir') . " $model_dir/mox_sir_block2.mod -samples=50,50 -resamples=25,25 -dir=$dir -seed=877914", #trigger -1.#IND on 7.1.2 xp  
	 get_command('sir') . " $model_dir/mox_sir.mod -samples=50 -resamples=100 -with_replacement -dir=$dir"
	);

foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([$dir]);
}

remove_test_dir($tempdir);

done_testing();
