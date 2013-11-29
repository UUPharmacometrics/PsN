#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use lib "../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

#making sure commands in HO HCV (Hepatitis C) run ok, no extra credit runs


our $dir = 'Bootstrap_dOFV_test';
my $model_dir = "HO_Bootstrap_dOFV_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);
my @command_list=(
	[$includes::execute." run1.mod -model_dir_name","Task 1a"],
	[$includes::sumo." run1.lst","Task 1b"],
	[$includes::bootstrap." run1.mod -samples=10 -dofv -dir=boot1a -seed=1234567","Task 2"],
	[$includes::bootstrap." run1.mod -samples=10 -dofv -dir=boot1b -seed=7654321","Task 4"],
	[$includes::execute." run2.mod -model_dir_name","Task 5a"],
	[$includes::bootstrap." run2.mod -samples=10 -dofv -dir=boot2 -seed=1234567","Task 5b"]
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
chdir('..');
rmtree([ "./$dir" ]); #with all sub run dirs



done_testing();
