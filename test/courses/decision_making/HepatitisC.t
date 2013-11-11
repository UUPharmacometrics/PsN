#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use File::Copy 'cp';

#making sure commands in HO HCV (Hepatitis C) run ok, no extra credit runs

my $path = "$Bin/../../../bin/";
our $dir = 'HepatitisC_test';
my $model_dir = "HO_HepatitisC_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);
my @command_list=([$path."execute run53.mod -model_dir_name","task 1a"],
				  [$path."sse run53.mod -no-estimate -samples=10 -rawres_input=sim_raw_res53_3.csv -dir=sse53","task 2b"] #reduced samples from 1000
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
