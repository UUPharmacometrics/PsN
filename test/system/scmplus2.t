#!/etc/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes;


our $tempdir = create_test_dir('system_scmplus2');

our $dir = "$tempdir/scmplus_test";

my @configs=('scmplus/run1.scm');
my @runs = (
	'run1.scm -no-abort_on_fail',
	'run1.scm -scope=none -etas -no-abort_on_fail',
);


my @extra_files = qw (
scmplus/run1.mod
scmplus/pheno.dta
scmplus/run1.lst
scmplus/run1.phi
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
