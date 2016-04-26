#!/etc/bin/perl

use strict;
use warnings;
use Test::More tests => 19;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

my @readpipe_list;


BEGIN
{
	*CORE::GLOBAL::readpipe = sub { push @readpipe_list, $_[0] };
	*CORE::GLOBAL::system = sub { };		# Override to avoid extra files
}

use nonmemrun::slurm;
use model;

my ($dirt1,$dirt2,$nmvers) = get_major_minor_nm_version;
my $model = model->new(filename => $includes::testfiledir . "/pheno5.mod");

#temporarily unset uppmax if set
my $old_uppmax = $PsN::config -> {'default_options'} -> {'uppmax'};
$PsN::config -> {'default_options'} -> {'uppmax'} = 0;

my $nonmemrun = nonmemrun::slurm->new(nm_version => $nmvers, model => $model);
$nonmemrun->submit;

#reset uppmax
$PsN::config -> {'default_options'} -> {'uppmax'} = $old_uppmax;

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
like($cmd, qr/\s+-o\s+nmfe_output\.txt\s+/, "stdout redirection");
like($cmd, qr/\s+-e\s+nmfe_output\.txt\s+/, "stderr redirection");
like($cmd, qr/\s+-J\s+pheno5.mod\s+/, "jobname");
unlike($cmd, qr/\s+-A\s+/, "no account");
unlike($cmd, qr/\s+-t\s+/, "no maxruntime");
unlike($cmd, qr/\s+-p\s+/, "no partition");
unlike($cmd, qr/\s+--mail-user\s+/, "no mailuser");
unlike($cmd, qr/\s+--mail-type\s+/, "no mailtype");
unlike($cmd, $re_prependflags, "no prepended flags");

@readpipe_list = ();
$nonmemrun = nonmemrun::slurm->new(
	nm_version => $nmvers,
	model => $model,
	account => 'myaccount',
	max_runtime => '12:00:00',
	partition => 'mypartition',
	send_email => 'ALL',
	email_address => 'psn@psn.org',
	prepend_flags => 'myflags flag2',
);
$nonmemrun->submit;

$cmd = $readpipe_list[0];

like($cmd, $re_account, "account");
like($cmd, $re_maxruntime, "maxruntime");
like($cmd, $re_partition, "partition");
like($cmd, $re_mailuser, "mailuser");
like($cmd, $re_mailtype, "mailtype");
like($cmd, $re_prependflags, "prependflags");


done_testing();
