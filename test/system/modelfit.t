#!/etc/bin/perl

# Testing the following features of execute:
#		* smoke test
#		* shrinkage
#		* tbs

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use List::Util qw(first);
use Config;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';


our $tempdir = create_test_dir('system_modelfit');
my $model_dir = $includes::testfiledir;
#put pheno.mod in testdir so that .ext etc in testfiledir are not modified
copy_test_files($tempdir,["phenomaxeval10.mod", "pheno.dta","pheno.mod"]);
my $dir='mfit';

chdir($tempdir);

my @command_line = (
	get_command('execute') . " phenomaxeval10.mod -nmfe -clean=1 -no-tweak_inits -min_retries=0 -retries=0 -maxevals=9999 -handle_msfo -directory=$dir",
	get_command('execute') . " phenomaxeval10.mod -nmfe -clean=1 -tweak_inits  -min_retries=0 -retries=0 -maxevals=0 -directory=$dir",
	get_command('execute') . " phenomaxeval10.mod -nmfe -clean=1 -tweak_inits  -min_retries=0 -retries=1 -maxevals=0 -directory=$dir",
	get_command('execute') . " pheno.mod -clean=1 -nmfe -tweak_inits  -min_retries=0 -retries=1 -reduced_model_ofv=70 -directory=$dir",
);

# If we are running on Windows remove ' in command line
if ($Config{osname} eq 'MSWin32') {
	foreach (@command_line) {
		tr/'//d;
	}
}


foreach my $i (0..$#command_line) {

	my $command= $command_line[$i];
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	if ($i == 0){
		ok (-e $dir.'/NM_run1/psn-1-step1.lst'," step files exist for maxevals>0");
		ok (not (-e $dir.'/NM_run1/psn-2.mod')," not retry files exist for maxevals>0 without tweak");
	}elsif($i == 1){
		ok (not (-e $dir.'/NM_run1/psn-2.mod')," retry files not exist for no retry");
	}elsif($i == 2){
		ok ((-e $dir.'/NM_run1/psn-2.mod')," retry files exist for retry maxeval exceeded");
	}elsif($i == 3){
		ok ((-e $dir.'/NM_run1/psn-2.mod')," retry files exist for retry local min");
	}

	rmtree([$dir]);
}

remove_test_dir($tempdir);

done_testing();
