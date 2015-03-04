#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_rawresults');
our $dir = "$tempdir/rawresults_test";
my $model_dir = $includes::testfiledir;

copy_test_files($tempdir, [ "pheno.mod", "pheno.lst" ]);

my @commands = 
	(get_command('rawresults') . " -path=$tempdir -outfile=$tempdir/test.csv",
	);

foreach my $command (@commands) {
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree(["$dir"]);
}
rmtree(["$dir"]);
remove_test_dir($tempdir);

done_testing();
