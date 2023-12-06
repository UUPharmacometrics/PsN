#!/etc/bin/perl


use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

use File::Copy 'copy';

#in psn.conf must set output_style = SPLUS, otherwise tests will fail. fix by setting here.

my $model_dir = $includes::testfiledir;


our $tempdir = create_test_dir('system_sse');
my ($major,$minor,$dirt) = get_major_minor_nm_version();

my $tndir = "$tempdir/tndir";
mkdir($tndir);
my $mod = 'tnpri.mod';
foreach my $file ("$model_dir/tnpri.mod","$model_dir/data_tnpri.csv","$model_dir/create_tnpri_msf.mod"){
	copy($file, "$tndir/.");
}
chdir($tndir);

my $command = get_command('execute') . " create_tnpri_msf.mod -seed=630992 ";
print "Running $command\n";
my $rc = system($command);

SKIP: {
    skip $major.".".$minor." is a too old NONMEM version for sse prior",1 if ($major < 7 or ($major == 7 and $minor < 2));

	$command = get_command('sse') . " -samples=3 $mod -seed=630992 ";
	print "Running $command\n";
	$rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, sse with prior tnpri should run ok");
}
chdir('..');

rmtree([$tndir]);

SKIP: {
    skip $major.".".$minor." is a too old NONMEM version for sse prior",1 if ($major < 7 or ($major == 7 and $minor < 2));
	#the template file is rubbish, but we just want to see that rscript generation does not crash, even if R does
	$command = get_command('sse') . " -samples=3 $model_dir/nwpri.mod -seed=630992 -rplots=1 -template_file=$model_dir/nwpri.mod";
	print "Running $command\n";
	$rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, sse with rplots should run ok");
}

$command = get_command('sse') . " -samples=3 $model_dir/pheno.mod -alt=$model_dir/sse_append_TYVV_DV.mod -samples=3 -no-est -append_col=TVV,DV";
print "Running $command\n";
$rc = system($command);
$rc = $rc >> 8;
ok ($rc == 0, "$command, sse with append_col should run ok");

remove_test_dir($tempdir);

done_testing();
