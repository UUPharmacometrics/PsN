#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_rawresults');

my $model_dir = $includes::testfiledir;

copy_test_files($tempdir, [ "pheno.mod", "pheno.lst" ]);
chdir($tempdir);

my @commands = 
	(get_command('rawresults') . " -path=$tempdir -outfile=test.csv ",
	);

foreach my $command (@commands) {
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}
remove_test_dir($tempdir);

done_testing();
