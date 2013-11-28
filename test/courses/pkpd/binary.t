#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use lib "../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';


our $dir = 'Binary_test';
my $model_dir = "HO_Binary_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);
#change back samp to 50 if running for real
my @command_list=(
	[$includes::path."execute run36.mod -model_dir_name","task 1 of 2"],
	[$includes::path."vpc run36vpc.mod -samples=20 -seed=1234 -levels=1.5 -dir=vpc_36 -lst=run36.lst -stratify_on=DOSE -nopred -dv=SMXH","task 2 of 2"]
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
