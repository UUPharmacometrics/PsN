#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use lib "../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

#making sure commands in HO HIV run ok, missing extra credit sse:s

our $dir = 'HIV_test';
my $model_dir = "HO_HIV_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);
#change back samp to 50 if running for real
#get a lot orf warning for task1 because of trailing empty lines in uncert_sse_1.csv, ok
my @command_list=([$includes::path."sse sse_1u.mod -samples=5 -seed=12345 -rawres_input=uncertainty_sse_1.csv -offset=1".
				   " -no-estimate_simulation -dir=sim1u ","task 1c:1"],
				  [$includes::path."sse sse_2u.mod -samples=5 -seed=12345 -rawres_input=uncertainty_sse_2.csv -offset=1".
				   " -no-estimate_simulation -dir=sim2u ","task 1c:2"],
				  [$includes::path."sse sse_3u.mod -samples=5 -seed=12345 -rawres_input=uncertainty_sse_3.csv -offset=1".
				   " -no-estimate_simulation -dir=sim3u ","task 1c:3"],
				  [$includes::path."sse sse_CI_1u.mod -samples=5 -seed=12345 -rawres_input=uncertainty_sse_CI_1.csv -offset=1".
				   " -no-estimate_simulation -dir=sim_CI_1u ","task 1e:1"],
				  [$includes::path."sse sse_CI_2u.mod -samples=5 -seed=12345 -rawres_input=uncertainty_sse_CI_2.csv -offset=1".
				   " -no-estimate_simulation -dir=sim_CI_2u ","task 1e:2"],
				  [$includes::path."sse sse_CI_3u.mod -samples=5 -seed=12345 -rawres_input=uncertainty_sse_CI_3.csv -offset=1".
				   " -no-estimate_simulation -dir=sim_CI_3u ","task 1e:3"],
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
