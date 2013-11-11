#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use File::Copy 'cp';

#making sure commands in HO HCV (Hepatitis C) run ok, no extra credit runs

my $path = "$Bin/../../../bin/";
our $dir = 'Bootstrap_dOFV_test';
my $model_dir = "HO_Bootstrap_dOFV_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);
my @command_list=(
	[$path."execute run1.mod -model_dir_name","Task 1a"],
	[$path."sumo run1.lst","Task 1b"],
	[$path."bootstrap run1.mod -samples=100 -dofv -dir=boot1a -seed=1234567","Task 2"],
	[$path."bootstrap run1.mod -samples=100 -dofv -dir=boot1b -seed=7654321","Task 4"],
	[$path."execute run2.mod -model_dir_name","Task 5a"],
	[$path."bootstrap run2.mod -samples=100 -dofv -dir=boot2 -seed=1234567","Task 5b"]
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
