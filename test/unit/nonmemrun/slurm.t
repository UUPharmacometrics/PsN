#!/etc/bin/perl

use strict;
use warnings;
use Test::More tests => 19;
use lib "../.."; #location of includes.pm
use includes; #file with paths to PsN packages

my @readpipe_list;

BEGIN
{
	*CORE::GLOBAL::readpipe = sub { push @readpipe_list, $_[0] };
	*CORE::GLOBAL::system = sub { };		# Override to avoid extra files
}

use nonmemrun::slurm;
use model;

my $model = model->new(filename => "../../test_files/pheno5.mod");
my $nonmemrun = nonmemrun::slurm->new(nm_version => 'default', model => $model);

$nonmemrun->submit;

foreach my $line (@readpipe_list) {
	print "$line\n";
}

my $re_account = qr/\s+-A\s+myaccount\s+/;
my $re_maxruntime = qr/\s+-t\s+12:00:00\s+/;
my $re_partition = qr/\s+-p\s+mypartition\s+/;
my $re_mailuser = qr/\s+--mail-user=psn\@psn.org\s+/;
my $re_mailtype = qr/\s+--mail-type=ALL\s+/;
my $re_prependflags = qr/\s+myflags flag2\s+/;

is(@readpipe_list, 1, "Number of readpipe calls");

my $cmd = $readpipe_list[0];

like($cmd, qr/\Asbatch\s+/, "command name");
like($cmd, qr/\s+-background\s+/, "background in call string");
like($cmd, qr/2>&1/, "redirection");
like($cmd, qr/\s+-o\s+nmfe\.output\s+/, "stdout redirection");
like($cmd, qr/\s+-e\s+nmfe\.output\s+/, "stderr redirection");
like($cmd, qr/\s+-J\s+pheno5.mod\s+/, "jobname");
unlike($cmd, $re_account, "no account");
unlike($cmd, $re_maxruntime, "no maxruntime");
unlike($cmd, $re_partition, "no partition");
unlike($cmd, $re_mailuser, "no mailuser");
unlike($cmd, $re_mailtype, "no mailtype");
unlike($cmd, $re_prependflags, "no prepended flags");

@readpipe_list = ();
my $nonmemrun = nonmemrun::slurm->new(
	nm_version => 'default',
	model => $model,
	account => 'myaccount',
	max_runtime => '12:00:00',
	partition => 'mypartition',
	send_email => 'ALL',
	email_address => 'psn@psn.org',
	prepend_flags => 'myflags flag2',
);
$nonmemrun->submit;

my $cmd = $readpipe_list[0];

like($cmd, $re_account, "account");
like($cmd, $re_maxruntime, "maxruntime");
like($cmd, $re_partition, "partition");
like($cmd, $re_mailuser, "mailuser");
like($cmd, $re_mailtype, "mailtype");
like($cmd, $re_prependflags, "prependflags");


done_testing();
