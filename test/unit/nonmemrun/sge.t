#!/etc/bin/perl

use strict;
use warnings;
use Test::More tests => 14;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

my @readpipe_list;

BEGIN
{
	*CORE::GLOBAL::readpipe = sub { push @readpipe_list, $_[0] };
	*CORE::GLOBAL::system = sub { };		# Override to avoid extra files
}

use nonmemrun::sge;
use model;

my ($dirt1,$dirt2,$nmvers) = get_major_minor_nm_version;
my $model = model->new(filename => $includes::testfiledir . "/pheno5.mod");
my $nonmemrun = nonmemrun::sge->new(nm_version => $nmvers, model => $model);

$nonmemrun->submit;

foreach my $line (@readpipe_list) {
	print "$line\n";
}

my $re_queue = qr/\s+-q\s+myqueue\s+/;
my $re_resource = qr/\s+-l\s+myresource\s+/;
my $re_prependflags = qr/\s+myflags flag2\s+/;

is(@readpipe_list, 1, "Number of readpipe calls");

my $cmd = $readpipe_list[0];

like($cmd, qr/\Aqsub\s+/, "command name");
like($cmd, qr/\s+-background\s+/, "background in call string");
like($cmd, qr/2>&1/, "redirection");
like($cmd, qr/\s+-cwd\s+/, "execute from current working directory flag");
like($cmd, qr/\s+-j\s+y\s+/, "stderr redirection");
like($cmd, qr/\s+-N\s+pheno5.mod\s+/, "jobname");
like($cmd, qr/\s+-b\s+y\s+/, "binary flag");
unlike($cmd, qr/\s+-q\s+/, "no queue");
unlike($cmd, qr/\s+-l\s+/, "no resource");
unlike($cmd, $re_prependflags, "no prepended flags");

@readpipe_list = ();
$nonmemrun = nonmemrun::sge->new(
	nm_version => $nmvers,
  model => $model,
	resource => 'myresource',
	queue => 'myqueue',
	prepend_flags => 'myflags flag2',
);
$nonmemrun->submit;

$cmd = $readpipe_list[0];

like($cmd, $re_resource, "resource");
like($cmd, $re_queue, "queue");
like($cmd, $re_prependflags, "prependflags");


done_testing();
