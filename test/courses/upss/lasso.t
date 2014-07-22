#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

our $tempdir = create_test_dir('courses_upss_lasso');
my $model_dir = "$Bin/lasso_files";
my @needed = <$model_dir/*>;
foreach my $file (@needed) {
	cp($file, $tempdir . '/.');
}
chdir($tempdir);

my @command_list = (
	[$includes::lasso." run1.mod -relations=CL:AGE-2,ACE-1,DIG-1,NYHA-1,SEX-1,,V:AGE-2,ACE-1,DIG-1,SEX-1,NYHA-1 -stratify_on=SEX -seed=1 -retries=0","HO LASSO task i. Note: this run will not find any significant covariates"],
	[$includes::lasso." run2.mod -relations=CL:AGE-3,ACE-1,DIG-1,NYHA-1,SEX-1,CRCL-2,,V:AGE-2,ACE-1,DIG-1,SEX-1,NYHA-1,WT-2 -stratify_on=SEX -seed=1 -retries=0","HO LASSO task iii"],
	[$includes::lasso." run2.mod -relations=CL:CRCL-2,WT-3,,V:WT-2 -seed=1 -retries=0","HO LASSO task v"]
	);
plan tests => scalar(@command_list);

foreach my $ref (@command_list) {
	my $command = $ref->[0];
	my $comment = $ref->[1];
	print "Running $comment:\n$command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$comment ");
}

chdir($Bin);
remove_test_dir($tempdir);

done_testing();
