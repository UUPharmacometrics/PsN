#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use File::Copy 'cp';
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

#black box testing of rawres_input functionality

our $dir = 'rawres_test';
our $bootdir = 'boot_test';
our $ssedir = 'sse_test';
my $model_dir = "$Bin/../test_files";

my @commands = 
	($includes::path."bootstrap -samples=5 $model_dir/pheno.mod -dir=$bootdir",
	 $includes::path."sse $model_dir/pheno.mod -rawres_input=$bootdir/raw_results_pheno.csv -samples=5 -no-est -dir=$dir",
	 $includes::path."sse $model_dir/pheno.mod  -samples=20 -dir=$ssedir",
	 $includes::path."vpc $model_dir/pheno.mod -rawres_input=$ssedir/raw_results_pheno.csv -offset=0 -samples=20 -auto_bin=unique -dir=$dir");



rmtree([ "./$dir" ]);
rmtree([ "./$ssedir" ]);
rmtree([ "./$bootdir" ]);
foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	rmtree([ "./$dir" ]);
}
rmtree([ "./$dir" ]);
rmtree([ "./$ssedir" ]);
rmtree([ "./$bootdir" ]);


done_testing();
