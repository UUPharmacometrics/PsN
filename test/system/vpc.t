#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir;
our $dir = "$tempdir/vpc_test";
my $model_dir = $includes::testfiledir;

my @commands = 
	($includes::vpc." -samples=20 $model_dir/mox1.mod -stratif=AGE -no_of_strata=3 -auto_bin=12 -dir=$dir",
	 );

foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([$dir]);
}

remove_test_dir;

done_testing();
