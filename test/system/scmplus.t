#!/etc/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/..";
use includes; #file with paths to PsN packages and $path variable definition


our $tempdir = create_test_dir('system_scmplus');

our $dir = "$tempdir/scmplus_test";

my @configs = ('scmplus/localminboth.scm', 'scmplus/phenoboth.scm');
my @runs = (
	'phenoboth.scm -fast -no-retest ',
	'phenoboth.scm -scope=none -etas ',
	'phenoboth.scm -p_cutoff=0.06 -scope=2,3 -no-retest -no-keep_local',
	'phenoboth.scm -p_cutoff=0.05 -scope=all',
	'localminboth.scm  -p_cutoff=0.07 -no-keep_local ',
	'localminboth.scm -p_cutoff=0.07  ',
);

my @extra_files = qw (
scmplus/pheno_with_cov.mod
scmplus/pheno_with_cov.lst
scmplus/pheno_with_cov.phi
scmplus/pheno_localmin.mod
scmplus/pheno_localmin.lst
scmplus/pheno_ch.csv
);


copy_test_files($tempdir,\@configs);
copy_test_files($tempdir,\@extra_files);

chdir($tempdir);

foreach my $cmd (@runs) {
	my $command = get_command('scmplus').' '.$cmd.' ';
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "scmplus $cmd crash test");

}

my %options = ('etas'=> undef,
			   'maxevals' => undef,
			   'ctype4' => undef,
	);
	

remove_test_dir($tempdir);

done_testing();
