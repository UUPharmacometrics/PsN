#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_sir');

my $model_dir = $includes::testfiledir;
chdir($tempdir);
my @commands = 
	(
	 get_command('sir') . " $model_dir/pheno.mod -samples=50,100 -resamples=25,50 -no-boxcox -dir=sirtest -omega_inflation=2,2", 
	 get_command('sir') . "  -dir=sirtest", 
	 get_command('sir') . " -samples=50 -resamples=25 -add_iterations -dir=sirtest", 
	 get_command('sir') . " $model_dir/pheno.mod -samples=50 -resamples=25 -auto_rawres=1.1 -seed=50032 ",
	 get_command('sir') . " $model_dir/sir/localmin.mod -samples=50,100 -resamples=25,50 -dir=sirtest_localmin",
	 get_command('sir') . " $model_dir/pheno.mod -samples=50,100,100 -resamples=25,50,50 -covmat_input=$model_dir/pheno_fake.cov ",
	 get_command('sir') . " $model_dir/pheno.mod -samples=100 -resamples=50 -covmat_input=identity -theta_infl=0.0000001 -omega_inf=0.002 -sigma_inf=0.0001 ",
	 get_command('sir') . " $model_dir/pheno.mod -samples=10 -resamples=5 -covmat_input=$model_dir/pheno_fake_2.cov -copy_data ",
	 get_command('sir') . " $model_dir/mox_sir_block2.mod -samples=50 -resamples=25  -problems_per_file=10 -copy_data",
	 get_command('sir') . " $model_dir/mox_sir_block2.mod -samples=50,50 -resamples=25,25  -copy_data",
	 get_command('sir') . " $model_dir/mox_sir_block2.mod -samples=50,50 -resamples=25,25 -seed=877914 -copy_data", #trigger -1.#IND on 7.1.2 xp  
	 get_command('sir') . " $model_dir/mox_sir.mod -samples=50 -resamples=100 -cap_resampling=3 -copy_data",

	 get_command('sir') . " $model_dir/pheno.mod -samples=50,100 -resamples=25,50 -no-boxcox -dir=sirtest_fast -omega_inflation=2,2 -fast_posdef_checks", 
	 get_command('sir') . " $model_dir/sir/localmin.mod -samples=50,100 -resamples=25,50 -dir=sirtest_localmin_fast -fast_posdef_checks"
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
