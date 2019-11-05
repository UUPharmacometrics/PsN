#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
require File::Copy::Recursive;

our $toolname = 'frem';
our $tempdir = create_test_dir('rplots_'.$toolname);

my $input_dir = $includes::rplots_testfiledir.'/'.$toolname.'/moxonidine';

unless (File::Copy::Recursive::dircopy($input_dir, $tempdir)) {
	die " Could not copy contents of $input_dir to $tempdir : $!\n";
}

chdir($tempdir);

my $command = get_command('postfrem') . " -frem_directory=frem_dir -directory=rundir ";

my $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "postfrem 1 that should run ok");
my %pdf_files_pages=($tempdir.'rundir/ID.IIV_CL.pdf' => 1,
					 $tempdir.'rundir/ID.IIV_KA.pdf' => 1,
					 $tempdir.'rundir/ID.IIV_V.pdf' => 1,
					 $tempdir.'rundir/IIV_CL.pdf' => 1,
					 $tempdir.'rundir/IIV_KA.pdf' => 1,
					 $tempdir.'rundir/IIV_V.pdf' => 1);

includes::test_pdf_pages(\%pdf_files_pages,[$tempdir.'rundir/Rplots.pdf']);
	

done_testing();
