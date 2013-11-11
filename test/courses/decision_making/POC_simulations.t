#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use File::Copy 'cp';

#making sure commands in HO POC_and_dose_finding/Simulations run ok

my $path = "$Bin/../../../bin/";
our $dir = 'POC_sim_test';
my $model_dir = "HO_POC_simulation_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);

my @command_list=(
	[$path."execute run0.mod -model_dir_name","task 1a 1"],
	[$path."execute run1.mod -model_dir_name","task 1a 2"],
	[$path."sumo run0.lst","task 1b 1"],
	[$path."sumo run1.lst","task 1b 2"],
	[$path."randtest run1.mod -samples=100 -base_model=run0.mod -random=DOSE -dir=randtest1 ","task 1d"],
	[$path."vpc run1.mod -stratify_on=ARM -auto_bin=unique -samples=100 -dir=vpc1","task 2a"],
	[$path."vpc run1.mod -stratify_on=ARM  -auto_bin=unique -samples=100 -dir=vpc1CFB -dv=CFB","task 2b"],
	[$path."bootstrap run1.mod -samples=100 -dir=boot1 -stratify_on=ARM","task 2c"],
	[$path."vpc run1.mod  -auto_bin=unique -stratify_on=ARM -samples=100 -dir=vpc1boot -rawres_in=boot1/raw_results_run1.csv","task 2e"],
	[$path."vpc run5.mod  -auto_bin=unique -stratify_on=ARM -samples=100 -dir=vpc5 -rawres_in=boot1/raw_results_run1.csv","task 3a"],
	[$path."vpc run5.mod -samples=100 -auto_bin=unique -stratify=ARM -dir=vpc5_NO_PU","task 3b"],
	[$path."vpc run5.mod -samples=100 -auto_bin=unique -stratify=ARM -dir=vpc5DD -refstrat=0 -dv=CFB -rawres=boot1/raw_results_run1.csv","task 3d"],
	[$path."execute run9.mod -extra_output=fort.50,fort.60 -model_dir_name","task 4b"],
	[$path."vpc run9.mod -samples=100 -stratify=ARM -dir=vpc9 -auto_bin=unique","task 4a"]
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
rmtree([ "./$dir" ]);



done_testing();
