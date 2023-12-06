#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use File::Copy 'copy';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_parallel_retries');

my $model_dir = $includes::testfiledir;
copy_test_files($tempdir,["pheno.mod", "pheno.dta"]);

my @commands = 
	(get_command('parallel_retries') . " pheno.mod -min_retries=2 -no-display -no-check",
	);
chdir($tempdir);
foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

remove_test_dir($tempdir);

done_testing();
