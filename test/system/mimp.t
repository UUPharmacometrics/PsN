#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use File::Copy 'copy';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_mimp');
chdir($tempdir);
my $model_dir = $includes::testfiledir;

my @commands = 
	(get_command('mimp') . " -reg=$model_dir/pheno.mod -base=$model_dir/pheno.mod -mi_model=$model_dir/pheno.mod -imputations=2 ",
	);

foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

remove_test_dir($tempdir);

done_testing();
