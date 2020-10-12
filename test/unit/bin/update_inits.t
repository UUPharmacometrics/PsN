#!/etc/bin/perl

# Blackbox testing of update and update_inits, not crash

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition


our $tempdir = create_test_dir('system_update_inits');
my $model_dir = $includes::testfiledir;
copy_test_files($tempdir, ["pheno.mod", "pheno.lst","run45.mod","run45.lst","raw_pheno_for_rawres_input.csv"]);

chdir($tempdir);

my @command_line = (
	get_command('update') . " pheno.mod -out=p1.mod",
	get_command('update_inits') . " pheno.mod -out=p1.mod",
	get_command('update') . " run45.mod -out=run46.mod",
	get_command('update_inits') . " run45.mod -flip_comments -out=run45sim.mod",
	get_command('update') . " pheno.mod -rawres_input=raw_pheno_for_rawres_input.csv -offset_rawres=3 -out=raw1.mod",
	get_command('update_inits') . " pheno.mod -rawres_input=raw_pheno_for_rawres_input.csv -in_filter=model.eq.4 -out=raw2.mod",
	);

foreach my $i (0..$#command_line) {

	my  $rc = system($command_line[$i]);
	$rc = $rc >> 8;

	ok ($rc == 0, $command_line[$i]." that should run ok");
}


chdir('..');
remove_test_dir($tempdir);

done_testing();
