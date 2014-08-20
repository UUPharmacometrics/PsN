#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>2;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

#black box testing of data class and progs that are not covered by other test files

our $tempdir = create_test_dir('system_npc');
our $dir = "$tempdir/npc_test";
my $model_dir = $includes::testfiledir;

my @commands = 
	(
	 get_command('npc') . " -samples=20 $model_dir/mox2.mod -dir=$dir",
	 get_command('npc') . " -samples=20 $model_dir/pheno5.mod -dir=$dir -no-copy_data",
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
