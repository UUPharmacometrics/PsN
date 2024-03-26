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
use File::Copy 'copy';
use tool::modelfit;
use common_options;
sub check_diff
{
	my $file1 = shift;
	my $file2 = shift;
	my $expected_diff = shift; #0 means files equal, 1 means different

	my $ok=0;
	if ((-e $file1) and (-e $file2)){
		if ($Config{osname} eq 'MSWin32') {
			$ok = 1; #cannot use diff to test
		}else{
			system("diff $file1 $file2 > /dev/null");
			my $result = $?;
			if ($expected_diff == 0){
				$ok = 1 if ($result == 0);
			}else{
				$ok = 1 unless ($result == 0);
			}
		}
	}
	return $ok;
}

our $tempdir = create_test_dir('system_modelfit');
my $model_dir = $includes::testfiledir;
#put pheno.mod in testdir so that .ext etc in testfiledir are not modified
copy_test_files($tempdir,["phenomaxeval10.mod","phenomaxeval0.mod", "pheno.dta","pheno.mod",'tnpri.mod','create_tnpri_msf.mod','data_tnpri.csv']);

chdir($tempdir);

my %options = %{get_psn_options()};
common_options::setup( \%options, 'execute' ); #
our $modext = 'mod';
our $lstext = 'lst'; 
my @command_line = (
	get_command('execute') . " -seed=1 phenomaxeval10.mod -no-disp -clean=1 -no-tweak_inits -min_retries=0 -retries=0 -maxevals=9999 -handle_msfo -directory=dir0",
	get_command('execute') . " -seed=1  phenomaxeval10.mod -no-disp -clean=1 -tweak_inits  -min_retries=0 -retries=0 -maxevals=0 -directory=dir1",
	get_command('execute') . " -seed=1  phenomaxeval10.mod -no-disp -clean=1 -tweak_inits  -min_retries=0 -retries=1 -maxevals=0 -directory=dir2",
	get_command('execute') . " -seed=52  pheno.mod -clean=1 -no-disp -tweak_inits  -min_retries=0 -retries=1 -reduced_model_ofv=70 -directory=dir3",
	get_command('execute') . " -seed=1  phenomaxeval0.mod -no-disp -clean=1 -tweak_inits  -min_retries=0 -retries=1 -maxevals=0 -directory=dir4",
	get_command('execute') . " -seed=154  phenomaxeval0.mod -no-disp -clean=1 -tweak_inits  -min_retries=1 -retries=0 -maxevals=0 -directory=dir5",
	get_command('execute') . " -seed=1  create_tnpri_msf.mod  -no-disp",
	get_command('execute') . " -seed=1  tnpri.mod -clean=1 -tweak_inits  -no-disp -min_retries=0 -retries=1 -extra_files=msf_tnpri -directory=dir6",
	get_command('execute') . " -seed=1  tnpri.mod -clean=1 -tweak_inits  -no-disp -min_retries=1 -retries=0 -extra_files=msf_tnpri -maxevals=0 -directory=dir7",
);

# If we are running on Windows remove ' in command line
if ($Config{osname} eq 'MSWin32') {
	foreach (@command_line) {
		tr/'//d;
	}
}


foreach my $i (0..$#command_line) {
	my $dir='dir'.$i;
	my $command= $command_line[$i];
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	if ($i == 0){
		ok (-e $dir.'/NM_run1/psn-1-step1.'.$lstext," step files exist for maxevals>0");
		ok (not (-e $dir.'/NM_run1/psn-1.'.$modext)," not retry files exist for maxevals>0 without tweak");
	}elsif($i == 1){
		ok (not (-e $dir.'/NM_run1/psn-1.'.$modext)," retry files not exist for no retry");
	}elsif($i == 2){
		ok ((-e $dir.'/NM_run1/psn-1.'.$modext)," retry files exist for retry maxeval exceeded");
		ok(check_diff($dir.'/NM_run1/psn-1.'.$modext,$dir.'/NM_run1/psn.'.$modext,1),'retry file is tweaked 1');
	}elsif($i == 3){
		ok ((-e $dir.'/NM_run1/psn-1.'.$modext)," retry files exist for retry local min");
		ok(check_diff($dir.'/NM_run1/psn-1.'.$modext,$dir.'/NM_run1/psn.'.$modext,1),'retry file is tweaked 2');
	}elsif($i == 4){
		ok (not (-e $dir.'/NM_run1/psn-1.'.$modext)," not retry files exist for no minimization step run");
	}elsif($i == 5){
		ok ((-e $dir.'/NM_run1/psn-2.'.$modext)," retry files exist for no minimization step run but min_retries");
		ok(check_diff($dir.'/NM_run1/psn-2.'.$modext,$dir.'/NM_run1/psn.'.$modext,1),'retry file is tweaked 3');
	}elsif($i == 6){
		ok ((-e 'msf_tnpri')," msf file");
	}elsif($i == 7){
		ok (not (-e $dir.'/NM_run1/psn-2.'.$modext)," not retry files exist for no minimization step run (onlysim)");
	}elsif($i == 8){
		ok (not (-e $dir.'/NM_run1/psn-2.'.$modext)," not retry files exist for onlysim but min_retries but no params to tweak");
	}

}

remove_test_dir($tempdir);

done_testing();
