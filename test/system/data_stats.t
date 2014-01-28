#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>2;
use FindBin qw($Bin);
use File::Copy 'cp';
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

#black box testing of data class and progs that are not covered by other test files

my $model_dir = "$Bin/../test_files";

my @commands = 
	(
	 $includes::data_stats." $model_dir/mox_simulated.csv",
	 $includes::data_stats." $model_dir/pheno.dta",
	 );

foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

done_testing();
