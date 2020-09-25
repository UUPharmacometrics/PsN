#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use PsN;

if (not have_pharmpy()) {
    plan skip_all => 'Cannot test frem without python and pharmpy';
}

my $interactive=0;
our $tempdir = create_test_dir('system_frem');
chdir($tempdir);
my $model_dir = $includes::testfiledir;

my $cholesky = '-cholesky';
my ($major,$minor,$dirt) = get_major_minor_nm_version();
if ($major < 7 or ($major == 7 and $minor < 3)){
	$cholesky = ''; #NM7.1 cannot handle & as line continuation
}
my @commands = (
	get_command('frem') . " -covar=WT,DGRP,SEX -skip_omegas=3  -log=WT -categorical=DGRP -check $model_dir/mox_frem.mod -no-run_sir",
	get_command('frem') . " -covar=WT,DGRP -categorical=DGRP -no-check $model_dir/mox_frem.mod -no-run_sir -mceta=50",
	get_command('frem') . " -covar=DIG,WT -no-check $model_dir/mox_frem.mod -no-run_sir -estimate_means",
	get_command('frem') . " -covar=SEX,DGRP -skip_om=2 -categorical=SEX,DGRP $cholesky -no-run_sir -no-check $model_dir/mox1.mod ",
	get_command('frem') . " -covar=AGE,SEX -categorical=SEX -no-check $model_dir/mox1.mod -no-run_sir",
	);

foreach my $command (@commands) {
	my  $rc = system($command);
    $rc = $rc >> 8;

	ok ($rc == 0, "$command");
	
}


my $plot_command = get_command('frem') . " -cov=APGR,WGT $model_dir/pheno_real.mod -rplots=1 -dir=plot_dir";
my  $rc = system($plot_command);
$rc = $rc >> 8;
ok($rc == 0, "$plot_command");
ok(-e 'plot_dir/results.json', "results.json");
ok(-e 'plot_dir/results.csv', "results.csv");
ok(-e 'plot_dir/results.html', "results.html");

remove_test_dir($tempdir);

done_testing();
