#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_pind');
our $dir = "pind_test";
my $model_dir = $includes::testfiledir;

my @commands = 
	($includes::pind." $model_dir/pheno5.mod  -dir=$dir",
	);
chdir($tempdir);
foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree(["$dir"]);
}

remove_test_dir($tempdir);

done_testing();
