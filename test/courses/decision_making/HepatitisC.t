#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

#making sure commands in HO HCV (Hepatitis C) run ok, no extra credit runs

our $tempdir = create_test_dir;
our $dir = "$tempdir/HepatitisC_test";
my $model_dir = "$Bin/HO_HepatitisC_files";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed) {
	cp($file, $dir . '/.');
}
chdir($dir);
my @command_list=([$includes::execute." run53.mod -model_dir_name","task 1a"],
				  [$includes::sse." run53.mod -no-estimate -samples=5 -rawres_input=sim_raw_res53_3.csv -dir=sse53","task 2b"] #reduced samples from 1000
	);
plan tests => scalar(@command_list);

foreach my $ref (@command_list){
	my $command=$ref->[0];
	my $comment=$ref->[1];
	print "Running $comment:\n$command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$comment ");
}

remove_test_dir($tempdir);

done_testing();
