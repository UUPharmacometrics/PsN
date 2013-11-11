#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use File::Copy 'cp';

#making sure commands in HO HIV run ok, missing extra credit sse:s

my $path = "$Bin/../../../bin/";
our $dir = 'RTTE_test';
my $model_dir = "RepeatedTTE";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);
#change back samp to 50 if running for real
my @command_list=(
	[$path."execute run51.mod","task 1 of 4"],
	[$path."vpc run51.mod -tte=RTTE -flip_comments -samples=20 -compress -clean=3 -stratify_on=DOSE,CON,SMAX,SMXH,THR,CAV,CAVH,CONC",
	 "task 2 of 4"],
	[$path."execute run57.mod","task 3 of 4"],
	[$path."vpc run57.mod -tte=RTTE -flip_comments -samples=20 -compress -clean=3 -stratify_on=DOSE,CON,SMAX,SMXH,THR,CAV,CAVH,CONC",
	 "task 4 of 4"]
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
