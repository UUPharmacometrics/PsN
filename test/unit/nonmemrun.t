#!/etc/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Config;
my @readpipe_list;

use nonmemrun;
use model;

my $nonmemrun = nonmemrun->new(nm_version => 'default');

my $cmd = $nonmemrun->create_command;

like($cmd, qr/nmfe/, "nmfe name");
like($cmd, qr/\s+psn.mod\s+psn.lst\s+/, "psn files");
like($cmd, qr/\s+-background/, "background");

my $nonmemrun = nonmemrun->new(
	nm_version => 'default',
	nmfe_options => 'my opts',
);

my $cmd = $nonmemrun->create_command;

like($cmd, qr/\s+my opts/, "nmfe_options");

$nonmemrun->job_id(10002265);
my $id = $nonmemrun->job_id;
is ($id,10002265,"big jobid");

our $tempdir = create_test_dir('unit_nonmemrun');

#create nmfe dummy installdir to search in
chdir($tempdir);
my $suffix = '';
my $path=$tempdir.'/';
if ($Config{osname} eq 'MSWin32') {
	$suffix = '.bat';
	$path = $tempdir."\\";
}
my @dirs=('run','util');
my @nmfe=('nmfe6','nmfe72');

for (my $i=0;$i<scalar(@dirs); $i++){
	mkdir($dirs[$i]);
	chdir($dirs[$i]);
	open my $fh,'>',$nmfe[$i].$suffix;
	print $fh "hej\n";
	close $fh;
	chmod 0777,$nmfe[$i].$suffix; #make executable
	chdir('..');
}

#print "tempdir $tempdir\n";
my $found_nmfe = nonmemrun::find_nmfe_script(nmdir =>$tempdir,major=>'6', minor=>undef);
is($found_nmfe,$tempdir.'/run/nmfe6'.$suffix,'found nmfe6 in run');

$found_nmfe = nonmemrun::find_nmfe_script(nmdir =>$path.'run',major=>'6', minor=>'1');
is($found_nmfe,$path.'run/nmfe6'.$suffix,'found nmfe6 in . ');

$found_nmfe = nonmemrun::find_nmfe_script(nmdir =>$tempdir,major=>'7', minor=>undef);
is($found_nmfe,$tempdir.'/util/nmfe72'.$suffix,'found nmfe72 in util');

$found_nmfe = nonmemrun::find_nmfe_script(nmdir =>$path.'util',major=>'7', minor=>'2');
is($found_nmfe,$path.'util/nmfe72'.$suffix,'found nmfe72 in . ');

remove_test_dir($tempdir);

done_testing();
