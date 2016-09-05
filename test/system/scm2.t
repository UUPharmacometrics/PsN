#!/etc/bin/perl

use strict;
use warnings;
use Test::More;
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

our $tempdir = create_test_dir('system_scm2');
our $dir = "$tempdir/scm_test2";
our $scm_file_dir = $includes::testfiledir . '/scm';
our $file_dir = $includes::testfiledir;

my @config_files = qw (
config_different_parameterizations_different_covariates.scm
config_foce_prop.scm
config_foce_prop_upd.scm
config_foce_upd.scm
config_alternative_parameterizations.scm
config_centering_bivariate.scm
config_emax.scm
config_foce_backward.scm
config_foce.scm
config_grouping_categorical.scm
config_hockey.scm
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
