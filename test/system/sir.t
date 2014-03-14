#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>2;
use FindBin qw($Bin);
use File::Copy 'cp';
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $dir = 'sir_test';
my $model_dir = "$Bin/../test_files";

my @commands = 
	($includes::sir." $model_dir/mox_sir_block2.mod -samples=100 -resamples=50 -dir=$dir",
	$includes::sir." $model_dir/mox_sir.mod -samples=50 -resamples=100 -with_replacement -dir=$dir"
	);

rmtree([ "./$dir" ]);
foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([ "./$dir" ]);
}
rmtree([ "./$dir" ]);

done_testing();
