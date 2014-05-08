#!/etc/bin/perl
use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('courses_pkpd_orderedcat');
our $dir = "$tempdir/OrderedCat_test";
my $model_dir = "$Bin/OrderedCat";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed){
	cp($file,$dir.'/.');
}
chdir($dir);
#change back samp to 50 if running for real
my @command_list=(
	[$includes::execute." run45.mod","task 1 od 2"],
	[$includes::vpc." run45vpc.mod -samples=20 -seed=1234 -dir=vpc_45 -lst=run45.lst -nopred -dv=SMXH -levels=0.5,1.5,2.5","task 2 of 2"]
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

remove_test_dir($tempdir);

done_testing();
