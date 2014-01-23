#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use FindBin qw($Bin);
use File::Copy 'cp';
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $dir = 'randtest_test';
my $model_dir = "$Bin/../test_files";

my @commands = 
	($includes::randtest . " $model_dir/mox1.mod -samples=5 -randomization_column=DOSE -dir=$dir",
	 );

rmtree([ "./$dir" ]);
foreach my $command (@commands) {
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([ "./$dir" ]);
}
rmtree([ "./$dir" ]);

done_testing();
