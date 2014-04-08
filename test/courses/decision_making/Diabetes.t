#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

#making sure commands in HO HIV run ok, missing extra credit sse:s

our $tempdir = create_test_dir;
our $dir = "$tempdir/Diabetes_test";
my $model_dir = "$Bin/HO_Diabetes_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed) {
	cp($file, $dir . '/.');
}
chdir($dir);

my @command_list=(
	[$includes::execute." run51.mod -dir=run51 ","Task 3a: run51"],
	[$includes::execute." run52.mod -dir=run52 ","Task 3b: run52"],
	[$includes::execute." run53.mod -dir=run53 ","Task 3c: run53"],
	[$includes::execute." run54.mod -dir=run54 ","Task 3d: run54"],
	[$includes::execute." run61.mod -dir=run61 ","Task 3e: run61"],
	[$includes::execute." run62.mod -dir=run62 ","Task 3f: run62"],
	[$includes::execute." run63.mod -dir=run63 ","Task 3g: run63"],
	[$includes::execute." run64.mod -dir=run64 ","Task 3h: run64"],
	[$includes::execute." run71.mod -dir=run71 ","Task 3i: run71"],
	[$includes::execute." run72.mod -dir=run72 ","Task 3j: run72"],
	[$includes::execute." run73.mod -dir=run73 ","Task 3k: run73"],
	[$includes::execute." run74.mod -dir=run74 ","Task 3l: run74"],
	[$includes::vpc." run51.mod -dir=run51vpc -dv=CFB -stratify=TRT -samples=20 -rawres_inp=raw_results_run51.csv","Task 6a: run51 vpc"], #samp=500
	[$includes::vpc." run52.mod -dir=run52vpc -dv=CFB -stratify=TRT -samples=20 -rawres_inp=raw_results_run52.csv","Task 6b: run52 vpc"]
	);

plan tests => scalar(@command_list);

foreach my $ref (@command_list){
	my $command=$ref->[0];
	my $comment=$ref->[1];
	print "Running $comment:\n$command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$comment ");
}

remove_test_dir($tempdir);

done_testing();
