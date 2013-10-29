#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use File::Copy 'cp';

#black box testing of rawres_input functionality

my $path = "$Bin/../../bin/";
our $dir = 'rawres_test';
our $bootdir = 'boot_test';
our $ssedir = 'sse_test';
my $model_dir = "$Bin/../test_files";
our $private_test_files = $ENV{HOME}.'/.test_files';

my @commands = 
	($path."bootstrap -samples=5 $model_dir/pheno.mod -dir=$bootdir",
	 $path."sse $model_dir/pheno.mod -rawres_input=$bootdir/raw_results_pheno.csv -samples=5 -no-est -dir=$dir",
	 $path."sse $model_dir/pheno.mod  -samples=20 -dir=$ssedir",
	 $path."vpc $model_dir/pheno.mod -rawres_input=$ssedir/raw_results_pheno.csv -offset=0 -samples=20 -auto_bin=unique -dir=$dir");



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
