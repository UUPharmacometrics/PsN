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
my $model_dir = $includes::testfiledir;


our $tempdir = create_test_dir('system_sse');
our $dir = "$tempdir/sse_test";

my $tndir = "$tempdir/tndir";
mkdir($tndir);
my $mod = 'tnpri.mod';
foreach my $file ("$model_dir/tnpri.mod","$model_dir/data_tnpri.csv","$model_dir/create_tnpri_msf.mod"){
	cp($file, "$tndir/.");
}
chdir($tndir);

my $command = $includes::execute." create_tnpri_msf.mod -seed=630992 ";
print "Running $command\n";
my $rc = system($command);
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
