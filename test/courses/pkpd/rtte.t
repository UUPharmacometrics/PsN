#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'cp';

our $tempdir = create_test_dir('courses_pkpd_rtte');
our $dir = "$tempdir/RTTE_test";
my $model_dir = "$Bin/RepeatedTTE";
my @needed = <$model_dir/*>;
mkdir($dir);
foreach my $file (@needed) {
	cp($file, $dir . '/.');
}
chdir($dir);
#change back samp to 50 if running for real
my @command_list=(
	[get_command('execute')." run51.mod","task 1 of 4"],
	[get_command('vpc')." run51.mod -tte=RTTE -flip_comments -samples=20 -compress -clean=3 -stratify_on=DOSE,CON,SMAX,SMXH,THR,CAV,CAVH,CONC",
	 "task 2 of 4"],
	[get_command('execute')." run57.mod","task 3 of 4"],
	[get_command('vpc')." run57.mod -tte=RTTE -flip_comments -samples=20 -compress -clean=3 -stratify_on=DOSE,CON,SMAX,SMXH,THR,CAV,CAVH,CONC",
	 "task 4 of 4"]
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
