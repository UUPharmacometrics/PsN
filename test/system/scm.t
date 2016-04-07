#!/etc/bin/perl

use strict;
use warnings;
use Test::More tests=>11;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

use data;
use file;
use tool::scm::config_file;
use tool::scm;
use common_options;

our $tempdir = create_test_dir('system_scm');

our $dir = "$tempdir/scm_test";

my @config_files = qw (
config_nohead.scm
config_ignore.scm
config_included.scm
config_logit.scm
config_normal_sum.scm
config_only_categorical.scm
config_state5.scm
config_time_varying.scm
config_tv.scm
config_usererror.scm
config_all_default_codes_explicitly.scm
);

my @extra_files = qw (
pheno_ignore.mod
pheno_logit.mod
pheno_logit_V.mod
pheno_missing_9999.mod
pheno_missing.mod
pheno_nohead.mod
pheno_with_cov_foce_prop.mod
pheno_with_cov.mod
pheno_with_tv.mod
filter_data.lst
pheno_ignore.lst
pheno_logit.lst
pheno_nohead.lst
pheno_with_cov_foce_prop.lst
pheno_with_cov.lst
pheno_with_tv.lst
pheno_nohead.dta
pheno_ch.csv
pheno_missing_9999.csv
pheno_missing.csv
);

foreach my $file (@config_files){
	copy_test_files($tempdir,['scm/'.$file]);
}
foreach my $file (@extra_files){
	copy_test_files($tempdir,['scm/'.$file]);
}
chdir($tempdir);
foreach my $cfile (@config_files) {
	my $command = get_command('scm').' '.$cfile.' ';
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "scm $cfile crash test");

}

remove_test_dir($tempdir);

done_testing();
