#!/etc/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/..";
use includes;


our $tempdir = create_test_dir('system_scmplus2');

our $dir = "$tempdir/scmplus_test";

my @configs=('scmplus/run1.scm', 'scmplus/run2.scm');

my @dieruns = ('scmplus/run2.scm');

my @extra_files = qw (
scmplus/run1.mod
scmplus/pheno.dta
scmplus/run1.lst
scmplus/run1.phi
);

copy_test_files($tempdir,\@configs);
copy_test_files($tempdir,\@extra_files);

chdir($tempdir);

foreach my $cmd (@dieruns) {
	my $command = get_command('scmplus').' '.$cmd.' ';
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc != 0, "scmplus $cmd should crash");

}

remove_test_dir($tempdir);

done_testing();
