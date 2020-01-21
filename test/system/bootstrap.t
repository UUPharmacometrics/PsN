#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_bootstrap');

my $model_dir = $includes::testfiledir;

copy_test_files($tempdir,["pheno5.mod", "pheno5.dta", "mox1.mod","mox1.lst", "mox_simulated.csv"]);
chdir($tempdir);
my $command = get_command('execute') . " pheno5.mod ";
my $rc = system($command);

$command = get_command('bootstrap') . " pheno5.mod -samples=10 -bca -seed=12345 -dir=boot1 -no-skip_minim -clean=1 -always_datafile_in_nmrun";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 1 that should run ok");
ok (-e 'boot1/modelfit_dir1/NM_run1/bs_pr1_1.dta','always datafile in nmrun yes');
ok (-e 'boot1/PsN_bootstrap_plots.R' or -e 'boot1/PsN_bootstrap_plots.Rmd','Creates Rmd or R default plot script');

$command = get_command('bootstrap') . " pheno5.mod -samples=10 -seed=12345 -dir=boot1a -dofv -no-rmarkdown";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 1a that should run ok");
ok (-e 'boot1a/PsN_bootstrap_plots.R','Creates R default file, not Rmd');

$command = get_command('bootstrap') . " pheno5.mod -samples=10 -seed=12345 -dir=boot1 -dofv -rplots=2 -html ";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 1b that should run ok");
ok (-e 'boot1/PsN_bootstrap_plots.html', 'html generation for plots');

$command = get_command('bootstrap') . " mox1.mod -samples=10 -stratify_on=DGRP -dir=boot2 -no-skip_minim -no-skip_est -clean=3";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 2 that should run ok");
unlink('boot2/m1/bs_pr1_1.lst');
unlink('boot2/m1/bs_pr1_1.ext');
$command = get_command('bootstrap') . " mox1.mod -samples=10 -stratify_on=DGRP  -no-skip_minim  -no-skip_est -dir=boot2";

$rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "bootstrap 3 that should run ok");


remove_test_dir($tempdir);

done_testing();
