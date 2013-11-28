#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use File::Copy 'cp';
use lib "../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

#making sure commands in HO POC_and_dose_finding/Study_design_dose_finding run ok

our $dir = 'POC_dosefinding_test';
my $model_dir = "HO_POC_dose_finding_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);

my @command_list=(
	[$includes::path."execute run10.mod -model_dir_name","task 1a 1"],
	[$includes::path."sumo run10.lst","task 1a 2"],
	[$includes::path."llp run10.mod -thetas=5 -dir=llp10","task 1b"],
	[$includes::path."bootstrap -samples=10 run10.mod -dir=boot10","task 1c"],
	[$includes::path."sse run11.mod -samples=10 -dir=sseMEM -seed=1234 -alt=run12.mod,run13.mod -rawres=boot10/raw_results_run10.csv","task 2a"],
	[$includes::path."sse run11.mod -samples=3 -dir=sseMEM_lowED50 -seed=1234 -alt=run12.mod -rawres=boot10/raw_results_run10.csv -in_filter=5_ED50.lt.60","task 2c"],
	[$includes::path."sse run11.mod -samples=10 -dir=sseCFB -seed=1234 -alt=run14.mod,run15.mod,run16.mod -rawres=boot10/raw_results_run10.csv -no-est","task 2e"],
	[$includes::path."update_inits run17.template run10.lst -out=run17.mod","task 3a"], #change $DATA to data17.csv also
	[$includes::path."mcmp -full_model=run17.mod -reduced_model=run13.mod -stratify_on=ARM -dir=mcmpMEM","task 3b"],
	[$includes::path."mcmp -simulation_model=run17.mod -full_model=run14.mod -reduced_model=run16.mod -stratify_on=ARM -dir=mcmpCFB","task 3c"],
	[$includes::path."update_inits run18.template run10.lst -out=run18.mod","task 3a"],
	[$includes::path."sse run18.mod -samples=20 -dir=sse18","task 4a"],
	[$includes::path."vpc run19.mod -samples=20 -idv=DOSE -flip_comments -rawres_input=sse18/raw_results_run18.csv -offset_rawres=0 -dir=vpcMEM300 -auto_bin=unique","task 4b"]
	);
foreach my $ref (@command_list){
	my $command=$ref->[0];
	my $comment=$ref->[1];
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$comment ");
}


chdir('..');
rmtree([ "./$dir" ]); #with all sub run dirs


done_testing();
