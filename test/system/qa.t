#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

my $interactive=0;
our $tempdir = create_test_dir('system_qa');
chdir($tempdir);
my $model_dir = $includes::testfiledir;

my @commands = (
	get_command('qa') . " -cont=APGR,WGT $model_dir/pheno_real.mod -dir=qa_pheno",
	);

foreach my $command (@commands) {
	my  $rc = system($command);
    $rc = $rc >> 8;

	ok($rc == 0, "$command");
    if ($^O eq 'MSWin32') {
        ok(-e 'qa_pheno/PsN_qa_plots.html');
    }
}

remove_test_dir($tempdir);

done_testing();
