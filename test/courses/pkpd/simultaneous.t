#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use File::Copy 'cp';

#making sure commands in HO HIV run ok, missing extra credit sse:s

my $path = "$Bin/../../../bin/";
our $dir = 'Simultaneous_test';
my $model_dir = "Simultaneous";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);
#change back samp to 50 if running for real
my @command_list=(
[$path."execute run81.mod  -model_dir_name","task 1 of 2"],
[$path."vpc run81.mod -tte=RTTE -flip_comments -samples=20 -compress -clean=3 -stratify_on=FLG,DV1,TRET,HR,CONC,CE,AGE,SEX,WT,CRCL"
 ,"task 2 of 2"]
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
