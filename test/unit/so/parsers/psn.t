#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;

use so::parsers::psn;

SKIP: {
    eval { require File::Copy::Recursive };
    skip "File::Copy::Recursive not installed" if $@;


	our $tempdir = create_test_dir('so_parser_psn');
	chdir($tempdir);
	my $filedir = $includes::testfiledir.'/SO/parser_psn';

	unless (File::Copy::Recursive::dircopy($filedir,'.')) {
		abort(" Could not copy contents of lib directory to testdirs : $!\n");
	}

	#bootrun  executerun  failrun  sserun  vpcrun
	chdir('bootrun');
	is(so::parsers::psn::_get_toolname(directory => 'rundir'),'bootstrap','_get_toolname bootstrap');
	my ($ref) = so::parsers::psn::_connector_get_files(directory => 'rundir',
															pharmml => 'pheno5.mod');
	is_deeply($ref,['pheno5.lst'],' get files');
	is((-e 'bootstrap_results.csv'),1,'rseultsfile exist boot 1');
	is((-e 'raw_results_pheno5.csv'),1,'rseultsfile exist boot 2');

	chdir($tempdir);
	chdir('executerun');
	is(so::parsers::psn::_get_toolname(directory => 'rundir'),'execute','_get_toolname execute');
	($ref) = so::parsers::psn::_connector_get_files(directory => 'rundir',
															pharmml => 'pheno5.mod');
	is_deeply($ref,['pheno5.lst'],' get files');
	is((-e 'raw_results_pheno5.csv'),1,'rseultsfile exist');


	chdir($tempdir);
	chdir('failrun');
	is(so::parsers::psn::_get_toolname(directory => 'rundir'),undef,'_get_toolname fail');
	($ref) = so::parsers::psn::_connector_get_files(directory => 'rundir',
															pharmml => 'pheno5.mod');
	is_deeply($ref,['pheno5.psn.log'],' get files failrun');
	is((-e 'pheno5.psn.log'),1,'rseultsfile exist');


	chdir($tempdir);
	chdir('sserun');
	is(so::parsers::psn::_get_toolname(directory => 'rundir'),'sse','_get_toolname sse');
	($ref) = so::parsers::psn::_connector_get_files(directory => 'rundir',
															pharmml => 'pheno5.mod');
	is_deeply($ref,['mc-1.lst','mc-2.lst','mc-3.lst'],' get files');
	is((-e 'sse_results.csv'),1,'rseultsfile exist sse 1');
	is((-e 'mc-1.lst'),1,'rseultsfile exist sse 2');
	is((-e 'mc-2.lst'),1,'rseultsfile exist sse 3');
	is((-e 'mc-sim-1.dat'),1,'rseultsfile exist sse 4');
	is((-e 'mc-sim-2.dat'),1,'rseultsfile exist sse 5');


	chdir($tempdir);
	chdir('vpcrun');
	is(so::parsers::psn::_get_toolname(directory => 'rundir'),'vpc','_get_toolname vpc');
	($ref) = so::parsers::psn::_connector_get_files(directory => 'rundir',
															pharmml => 'pheno5.mod');
	is_deeply($ref,['vpc_simulation.1.lst'],' get files');
	is((-e 'vpc_simulation.1.lst'),1,'rseultsfile exist vpc1');
	is((-e 'vpctab'),1,'rseultsfile exist vpc2');
	is((-e 'vpc_results.csv'),1,'rseultsfile exist vpc3');
	is((-e 'npctab.dta'),1,'rseultsfile exist vpc4');

	chdir($tempdir);

	chdir('windows');
	is(so::parsers::psn::_get_toolname(directory => 'boot'),'bootstrap','_get_toolname windows bootstrap');
	is(so::parsers::psn::_get_toolname(directory => 'exec'),'execute','_get_toolname windows execute');
	is(so::parsers::psn::_get_toolname(directory => 'vpcdir'),'vpc','_get_toolname windows vpc');


	remove_test_dir($tempdir);

}
is(so::parsers::psn::_get_stem(pharmml => 'UseCase1.xml'),'UseCase1','_get_stem');
is(so::parsers::psn::_get_logfile(pharmml => 'UseCase1.xml'),'UseCase1.psn.log','_get_logfile');
is(so::parsers::psn::_get_lstfile(pharmml => 'UseCase1.xml'),'UseCase1.lst','_get_lstfile');
is(so::parsers::psn::_get_sofile(pharmml => 'UseCase1.xml'),'UseCase1.SO.xml','_get_sofile');

is(so::parsers::psn::_get_stem(pharmml => 'run1.mod'),'run1','_get_stem');
is(so::parsers::psn::_get_logfile(pharmml => 'run1.mod'),'run1.psn.log','_get_logfile');
is(so::parsers::psn::_get_lstfile(pharmml => 'run1.mod'),'run1.lst','_get_lstfile');
is(so::parsers::psn::_get_sofile(pharmml => 'run1.mod'),'run1.SO.xml','_get_sofile');

is(so::parsers::psn::_get_stem(pharmml => 'run1.6.ctl'),'run1.6','_get_stem');
is(so::parsers::psn::_get_logfile(pharmml => 'run1.6.ctl'),'run1.6.psn.log','_get_logfile');
is(so::parsers::psn::_get_lstfile(pharmml => 'run1.6.ctl'),'run1.6.lst','_get_lstfile');



done_testing();
