#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>2;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

#black box testing of data class and progs that are not covered by other test files

our $tempdir = create_test_dir('system_npc');
my $model_dir = $includes::testfiledir;
chdir($tempdir);
my @commands = 
	(
	 get_command('npc') . " -samples=20 $model_dir/mox2.mod ",
	 get_command('npc') . " -samples=20 $model_dir/pheno5.mod -copy_data",
	 );

foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

remove_test_dir($tempdir);

done_testing();
