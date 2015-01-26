#!/etc/bin/perl

use strict;
use warnings;
use Test::More tests => 5;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

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

done_testing();
