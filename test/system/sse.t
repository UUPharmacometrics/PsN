#!/etc/bin/perl


use strict;
use warnings;
use Test::More tests=> 2;
#use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

use File::Copy 'cp';

#in psn.conf must set output_style = SPLUS, otherwise tests will fail. fix by setting here.

#tnpri is NM version dependent due to msfi file, 730 or 72.
PsN::set_nonmem_info('default');
our $nm_version = $PsN::nm_major_version * 100 + $PsN::nm_minor_version * 10;
my $model_dir = $includes::testfiledir;


our $tempdir = create_test_dir;
our $dir = "$tempdir/sse_test";

my $tndir = "$tempdir/tndir";
mkdir($tndir);
my $mod;
if ($nm_version >= 730) {
	foreach my $file ("$model_dir/tnpri_nm730.mod","$model_dir/msf_tnpri_nm730","$model_dir/data_tnpri.csv"){
		cp($file, "$tndir/.");
	}
	$mod = 'tnpri_nm730.mod';
} else {
	foreach my $file ("$model_dir/tnpri.mod","$model_dir/msf_tnpri","$model_dir/data_tnpri.csv"){
		cp($file, "$tndir/.");
	}
	$mod = 'tnpri.mod';
}
chdir($tndir);

my $command = $includes::sse." -samples=3 $mod -seed=630992 -directory=$dir";
print "Running $command\n";
my $rc = system($command);
$rc = $rc >> 8;
ok ($rc == 0, "$command, sse with prior tnpri should run ok");
chdir('..');
rmtree([$tndir]);

$command= $includes::sse." -samples=3 $model_dir/nwpri.mod -seed=630992 -directory=$dir";
print "Running $command\n";
$rc = system($command);
$rc = $rc >> 8;
ok ($rc == 0, "$command, sse with prior nwpri should run ok");

remove_test_dir($tempdir);

done_testing();
