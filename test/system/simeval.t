#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_simeval');
my $model_dir = $includes::testfiledir;

chdir($tempdir);
my @commands = 
	(
	 get_command('simeval') . " -samples=20 $model_dir/pheno_cond.mod -rplots=2 -html "
  );

foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

if ($^O eq 'MSWin32') {
    ok(-e ($tempdir . '/simeval_dir1/PsN_simeval_plots.html'), 'generate simeval plots');
}

remove_test_dir($tempdir);

done_testing();
