#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::Random;
use PsN;
use Config;
use Env qw(PATH);

SKIP: {

	my @outp=readpipe("nmfe73 -h");
	my $found73 = ($outp[0] =~ /Usage/);
	my $found72=0;
	unless ($found73){
		@outp=readpipe("nmfe72 -h");
		$found72 = ($outp[0] =~ /Starting/);
	}

    skip "nmfe not in path" unless (($found72 or $found73) 
									and (($PATH =~ /C:\\nonmem\\nm730\\run/) or ($PATH =~ /\/opt\/nm730\/run/)
									or ($PATH =~ /C:\\nm72_g\\run/)));

	if ($Config{osname} eq 'MSWin32'){
		if ($found73){
			is(PsN::find_nmfe_from_system_path('nmfe73'),'C:\nonmem\nm730\run\nmfe73.bat','nmfe73 in path');
			is(PsN::find_nmfe_from_system_path('nmfe73.bat'),'C:\nonmem\nm730\run\nmfe73.bat','nmfe73.bat in path');
			is(PsN::find_nmfe_from_system_path('C:\nonmem\nm730\run\nmfe73.bat'),0,'nmfe73 full path');
		}else{
			is(PsN::find_nmfe_from_system_path('nmfe72'),'C:\nm72_g\run\nmfe72.bat','nmfe72 in path');
			is(PsN::find_nmfe_from_system_path('nmfe72.bat'),'C:\nm72_g\run\nmfe72.bat','nmfe72.bat in path');
			is(PsN::find_nmfe_from_system_path('C:\nm72_g\run\nmfe72.bat'),0,'nmfe72 full path');
		}
	}else{
		is(PsN::find_nmfe_from_system_path('nmfe73'),'/opt/nm730/run/nmfe73','nmfe73 in path');
	}
	is(PsN::find_nmfe_from_system_path('nmfe54'),0,'nmfe54 not in path');
	is(PsN::find_nmfe_from_system_path('opt/nmfe54'),0,'nmfe54 not in path');
	is(PsN::find_nmfe_from_system_path('C:\nmfe54'),0,'nmfe54 not in path');

}
done_testing();
