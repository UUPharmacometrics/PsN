#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>3;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_sir');
our $dir = "$tempdir/sir_test";
my $model_dir = $includes::testfiledir;

my @commands = 
	(
	 $includes::sir." $model_dir/pheno.mod -samples=100 -resamples=50 -covmat_input=$model_dir/pheno_fake.cov -dir=$dir",
	 $includes::sir." $model_dir/mox_sir_block2.mod -samples=100 -resamples=50 -dir=$dir",
	 $includes::sir." $model_dir/mox_sir.mod -samples=50 -resamples=100 -with_replacement -dir=$dir"
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
