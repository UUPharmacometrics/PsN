#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use File::Copy 'cp';

#making sure commands in HO HIV run ok, missing extra credit sse:s

my $path = "$Bin/../../../bin/";
our $dir = 'Diabetes_test';
my $model_dir = "HO_Diabetes_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);

my @command_list=(
	[$path."execute run51.mod -dir=run51 ","Task 3a: run51"],
	[$path."execute run52.mod -dir=run52 ","Task 3b: run52"],
	[$path."execute run53.mod -dir=run53 ","Task 3c: run53"],
	[$path."execute run54.mod -dir=run54 ","Task 3d: run54"],
	[$path."execute run61.mod -dir=run61 ","Task 3e: run61"],
	[$path."execute run62.mod -dir=run62 ","Task 3f: run62"],
	[$path."execute run63.mod -dir=run63 ","Task 3g: run63"],
	[$path."execute run64.mod -dir=run64 ","Task 3h: run64"],
	[$path."execute run71.mod -dir=run71 ","Task 3i: run71"],
	[$path."execute run72.mod -dir=run72 ","Task 3j: run72"],
	[$path."execute run73.mod -dir=run73 ","Task 3k: run73"],
	[$path."execute run74.mod -dir=run74 ","Task 3l: run74"],
	[$path."vpc run51.mod -dir=run51vpc -dv=CFB -stratify=TRT -samples=20 -rawres_inp=raw_results_run51.csv","Task 6a: run51 vpc"], #samp=500
	[$path."vpc run52.mod -dir=run52vpc -dv=CFB -stratify=TRT -samples=20 -rawres_inp=raw_results_run52.csv","Task 6b: run52 vpc"]
	);
foreach my $ref (@command_list){
	my $command=$ref->[0];
	my $comment=$ref->[1];
	print "Running $comment:\n$command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$comment ");
}

chdir('..');
rmtree([ "./$dir" ]); #with all sub run dirs



done_testing();
