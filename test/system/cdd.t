#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>2;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $dir = 'misc_test';
my $model_dir = $includes::testfiledir;

my @commands = 
	($includes::cdd." -case_column=ID $model_dir/pheno5.mod -xv  -dir=$dir",
	 $includes::cdd." $model_dir/mox1.mod -case_column=DGRP  -dir=$dir",
	);

rmtree([ "./$dir" ]);
foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([ "./$dir" ]);
}
rmtree([ "./$dir" ]);

done_testing();
