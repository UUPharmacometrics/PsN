#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use File::Copy 'copy';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_randtest');
my $model_dir = $includes::testfiledir;

chdir($tempdir);
my @commands = 
	(get_command('randtest') . " -no-update_inits $model_dir/mox1.mod -samples=5 -randomization_column=DOSE ",
	 get_command('randtest') . " $model_dir/phenofull.mod -base=$model_dir/pheno.mod -samples=5 -randomization_column=WGT ",
	 get_command('randtest') . " $model_dir/phenofull.mod -base=$model_dir/pheno.mod -full_model_inits -samples=5 -randomization_column=WGT ",
	 );

foreach my $command (@commands) {
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

remove_test_dir($tempdir);

done_testing();
