#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use PsN;
use Config;
use Env qw(PATH);
use Cwd;

#R version

 SKIP: {
	 skip "R not in path",1 unless ($PATH =~ /:\/usr\/bin:/ and -x '/usr/bin/R');
	 $PsN::config->{'_'}->{'R'} = undef;
	 is(PsN::get_R_exec,'R','get_R_exec in path');
}


$PsN::config->{'_'}->{'R'} = "/path/to/R";
my $Rexec = PsN::get_R_exec();
if ($Config{osname} eq 'MSWin32') {
    # Remove drive letter since it is different in CI
    is(substr($Rexec, 1), ":\\path\\to\\R", 'get_R_exec config');
} else {
    is($Rexec, "/path/to/R", 'get_R_exec config');
}

# get_nmversion_info
$PsN::config->{'nm_versions'}->{nm73} = "/opt/NONMEM/nm73,7.3";
my @result = PsN::get_nmversion_info("nm73");
is ($result[0], "/opt/NONMEM/nm73", "get_nmversion_info path");
is ($result[1], 7, "get_nmversion_info major");
is ($result[2], 3, "get_nmversion_info minor");

$PsN::config->{'nm_versions'}->{nm74} = "/opt/NONMEM/nm74";
dies_ok { PsN::get_nmversion_info("nm74") } "get_nmversion_info missing version";

$PsN::config->{'nm_versions'}->{nm74} = undef;
dies_ok { PsN::get_nmversion_info("nm74") } "get_nmversion_info missing info";

$PsN::config->{'nm_versions'}->{nm74} = "/path,4.3";
dies_ok { PsN::get_nmversion_info("nm74") } "get_nmversion_info non supported major version";

dies_ok { PsN::get_nmversion_info() } "get_nmversion_info no input";

is(PsN::find_nmfe_from_system_path('nmfe54'),0,'nmfe54 not in path');
is(PsN::find_nmfe_from_system_path('opt/nmfe54'),0,'nmfe54 not in path');
is(PsN::find_nmfe_from_system_path('C:\nmfe54'),0,'nmfe54 not in path');

my $redirectstderr = redirect_stderr;

my @outp=readpipe("nmfe73 -h".$redirectstderr); 
my $found73 = 0;
$found73 = ($outp[0] =~ /Usage/) if (defined $outp[0]);
my $found72=0;
unless ($found73){
	@outp=readpipe("nmfe72 -h".$redirectstderr);
	$found72 = ($outp[0] =~ /Starting/) if (defined $outp[0]);
}

if ($Config{osname} eq 'MSWin32'){

SKIP: {

    skip "nmfe not in path",3 unless (($found72 or $found73) 
									  and (($PATH =~ /C:\\nonmem\\nm730\\run/) or ($PATH =~ /C:\\nm72_g\\run/)));
	
	if ($found73){
		is(PsN::find_nmfe_from_system_path('nmfe73'),'C:\nonmem\nm730\run\nmfe73.bat','nmfe73 in path');
		is(PsN::find_nmfe_from_system_path('nmfe73.bat'),'C:\nonmem\nm730\run\nmfe73.bat','nmfe73.bat in path');
		is(PsN::find_nmfe_from_system_path('C:\nonmem\nm730\run\nmfe73.bat'),0,'nmfe73 full path');
	}else{
		is(PsN::find_nmfe_from_system_path('nmfe72'),'C:\nm72_g\run\nmfe72.bat','nmfe72 in path');
		is(PsN::find_nmfe_from_system_path('nmfe72.bat'),'C:\nm72_g\run\nmfe72.bat','nmfe72.bat in path');
		is(PsN::find_nmfe_from_system_path('C:\nm72_g\run\nmfe72.bat'),0,'nmfe72 full path');
	}

	}
}else{
	#unix
  SKIP: {
	  
	  
	  skip "nmfe not in path",1 unless ($found73 and ($PATH =~ /\/opt\/nm730\/run/));

	  is(PsN::find_nmfe_from_system_path('nmfe73'),'/opt/nm730/run/nmfe73','nmfe73 in path');
	}

}


our $tempdir = create_test_dir('unit_psn');
chdir($tempdir);
$tempdir = Cwd::getcwd;
mkdir('lib');
chdir('lib');
my $absdir = Cwd::getcwd;

is(PsN::get_Rscripts_dir($absdir),undef,'no rscripts dir');
mkdir($tempdir.'/R-scripts');
is(PsN::get_Rscripts_dir($absdir),Cwd::abs_path($tempdir.'/R-scripts'),' rscripts dir beside lib');

mkdir('R-scripts');
is(PsN::get_Rscripts_dir($absdir),Cwd::abs_path($absdir.'/R-scripts'),' rscripts dir in lib');


remove_test_dir($tempdir);

done_testing();
