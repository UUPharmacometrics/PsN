#!/etc/bin/perl

use strict;
use warnings;
use Test::More tests => 14;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use ui;

ui->silent(1);

my @readpipe_list;

BEGIN
{
	*CORE::GLOBAL::readpipe = sub { push @readpipe_list, $_[0] };
	*CORE::GLOBAL::system = sub { };		# Override to avoid extra files
}

use File::Spec;
use nonmemrun::lsf;
use model;


my ($dirt1,$dirt2,$nmvers) = get_major_minor_nm_version;

my $temp_dir = create_test_dir('unit_nonmemrun_lsf');
chdir $temp_dir;

my $model = model->new(filename => $includes::testfiledir . "/pheno5.mod");
my $nonmemrun = nonmemrun::lsf->new(nm_version => $nmvers, model => $model, lsf_sleep => 0);

$nonmemrun->submit;

foreach my $line (@readpipe_list) {
	print "$line\n";
}

is(@readpipe_list, 1, "Number of readpipe calls");

my $cmd = $readpipe_list[0];

my $jobscript = 'lsf_jobscript';

like_file_row($jobscript, qr/\A#BSUB\s+-J\s+pheno5\.mod\Z/, "jobname");
like_file_row($jobscript, qr/\A#BSUB\s+-e\s+\w+\Z/, "stderr redirection");
like_file_row($jobscript, qr/\A#BSUB\s+-o\s+\w+\Z/, "stdout redirection");
unlike_file_row($jobscript, qr/\s+-q\s+/, "no queue");
unlike_file_row($jobscript, qr/\s+-P\s+/, "no project");
unlike_file_row($jobscript, qr/\s+-c\s+/, "no ttl");
unlike_file_row($jobscript, qr/\s+-R\s+/, "no resources");

like($cmd, qr/\Absub\s+/, "command name");

@readpipe_list = ();
$nonmemrun = nonmemrun::lsf->new(
	nm_version => $nmvers,
  model => $model,
	lsf_sleep => 0,
	lsf_options => 'myopt optA',
	lsf_queue => 'myqueue',
	lsf_project_name => 'myproject',
	lsf_ttl => 'myttl',
	lsf_resources => 'myresource',
);
$nonmemrun->submit;

$cmd = $readpipe_list[0];

like($cmd, qr/myopt optA/, "lsf_options");
like_file_row($jobscript, qr/\A#BSUB\s+-q\s+myqueue\Z/, "queue");
like_file_row($jobscript, qr/\A#BSUB\s+-P\s+myproject\Z/, "project name");
like_file_row($jobscript, qr/\A#BSUB\s+-c\s+myttl\Z/, "ttl");
like_file_row($jobscript, qr/\A#BSUB\s+-R\s+myresource\Z/, "resource");

remove_test_dir($temp_dir);

done_testing();
