#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_npfit');

my $model_dir = $includes::testfiledir;
chdir($tempdir);

my @commands = 
	(get_command('npfit') . " $model_dir/pheno.mod -npsupp=100,200,300",
	 get_command('npfit') . " $model_dir/pheno.mod -npsupp=59",
	 get_command('npfit') . " $model_dir/pheno.mod -npsupp=0,100 -silent",
	 get_command('npfit') . " $model_dir/mox1.mod -npsupp=80,100",
	 get_command('npfit') . " $model_dir/mox1.mod -npsupp=54,100 -silent",
	 get_command('npfit') . " $model_dir/run1.mod -npsupp=100",
	 get_command('npfit') . " $model_dir/run1.mod -npsupp=2,10 -silent",
	);
foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

my @crash_commands = 
	(get_command('npfit') . " $model_dir/pheno.mod -npsupp='' -silent",
	 get_command('npfit') . " $model_dir/pheno.mod -silent",
	 get_command('npfit') . " -npsupp=100 -silent ",
	);
open STDERR, '>', File::Spec->devnull();       # Silence STDERR croak messages from below
foreach my $crash_commands (@crash_commands){
	print "Running $crash_commands\n";
	my $rc = system($crash_commands);
	$rc = $rc >> 8;
	ok (not($rc == 0), "$crash_commands, should crach");
}

remove_test_dir($tempdir);

done_testing();
