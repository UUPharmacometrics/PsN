#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

#black box testing of data class and progs that are not covered by other test files

our $tempdir = create_test_dir('system_mcmp');
chdir($tempdir);
my $model_dir = $includes::testfiledir;

my @commands = (
	get_command('mcmp') . " -full=$model_dir/pheno.mod -reduced=$model_dir/pheno.mod -rplots=1 -dir=mcmp_withplot",
    get_command('mcmp') . " -n_bootstrap=5 -full=$model_dir/pheno.mod -reduced=$model_dir/pheno.mod  ",
	get_command('mcmp') . " -simdata=$model_dir/pheno.dta -significance_level=20 -n_bootstrap=5 -full=$model_dir/pheno.mod -reduced=$model_dir/pheno.mod  ",
	get_command('mcmp') . " -simdata=$model_dir/pheno.dta -significance_level=0.1 -n_bootstrap=5 -full=$model_dir/pheno.mod -reduced=$model_dir/pheno.mod  ",
	get_command('mcmp') . " -simdata=$model_dir/pheno.dta -significance_level=10 -n_bootstrap=5 -full=$model_dir/pheno.mod -reduced=$model_dir/pheno.mod  ",
	get_command('mcmp') . " -simdata=$model_dir/pheno.dta -significance_level=5 -df=2 -n_bootstrap=5 -full=$model_dir/pheno.mod -reduced=$model_dir/pheno.mod  ",
	get_command('mcmp') . " -simdata=$model_dir/pheno.dta -critical_ofv=4 -n_bootstrap=5 -full=$model_dir/pheno.mod -reduced=$model_dir/pheno.mod ",
);

foreach my $command (@commands) {
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
    if (index($command, "-rplots") != -1) {
        ok (-e "$tempdir/mcmp_withplot/PsN_mcmp_plots.pdf", "mcmp plots")
    }
}

remove_test_dir($tempdir);

done_testing();
