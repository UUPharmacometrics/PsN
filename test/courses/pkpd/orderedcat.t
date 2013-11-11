#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use File::Copy 'cp';

#making sure commands in HO HIV run ok, missing extra credit sse:s

my $path = "$Bin/../../../bin/";
our $dir = 'OrderedCat_test';
my $model_dir = "OrderedCat";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);
#change back samp to 50 if running for real
my @command_list=(
	[$path."execute run45.mod","task 1 od 2"],
	[$path."vpc run45vpc.mod -samples=50 -seed=1234 -dir=vpc_45 -lst=run45.lst -nopred -dv=SMXH -levels=0.5,1.5,2.5","task 2 of 2"]
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
