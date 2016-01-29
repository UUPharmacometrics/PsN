#!/etc/bin/perl

# Blackbox testing of frem, not crash

use strict;
use warnings;
use File::Path 'rmtree';
#use Test::More tests=>1;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

my $interactive=0;
our $tempdir = create_test_dir('system_frem');
our $dir = "$tempdir/frem_test";
my $model_dir = $includes::testfiledir;

my @commands = (
#	get_command('frem') . " -time_var=WT -occ=VISI -param=PHI,LAG -invar=SEX,DGRP -no-check $model_dir/mox_no_bov.mod -dir=$dir",
	get_command('frem') . " -covar=WT,DGRP,SEX -skip_omegas=2  -log=WT -categorical=DGRP -check $model_dir/mox_frem.mod -dir=$dir",
	get_command('frem') . " -covar=WT,DGRP -categorical=DGRP -no-check $model_dir/mox_frem.mod -dir=$dir",
	get_command('frem') . " -covar=DIG,WT -no-check $model_dir/mox_frem.mod -dir=$dir",
	get_command('frem') . " -covar=SEX,DGRP -skip_om=2 -categorical=SEX,DGRP -estimate_chole -estimate_reg -no-check $model_dir/mox1.mod -dir=$dir",
	get_command('frem') . " -covar=AGE,SEX -categorical=SEX -no-check $model_dir/mox1.mod -dir=$dir",
#	get_command('frem') . " -time_var=WT,NYHA -occ=VISI -param=PHI -invar=SEX -start_eta=3 $model_dir/mox_no_bov.mod  -dir=$dir",
#	get_command('frem') . " -time_var=WT,NYHA -occ=VISI -param=CL -invar=SEX -no-check $model_dir/mox_no_bov.mod -dir=$dir",
#	get_command('frem') . " -time_var=WT,NYHA -occ=VISI -param=V -invar=SEX -no-check $model_dir/mox_no_bov.mod -est=0  -dir=$dir",
#	get_command('frem') . " -time_var=WT,NYHA -occ=VISI -param=KA,LAG -no-check $model_dir/mox_no_bov.mod -dir=$dir",
	);

plan tests => scalar(@commands);

foreach my $command (@commands) {
	my  $rc = system($command);
	$rc = $rc >> 8;

	ok ($rc == 0, "$command");
	
	if ($rc == 0) {
		if ($interactive) {
			print "hit return to remove $dir and continue: ";
			my $input = <STDIN>;
		}
		rmtree([$dir]);
	} else {
		exit;
	}
}

remove_test_dir($tempdir);

done_testing();
