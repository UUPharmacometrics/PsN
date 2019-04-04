#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_cdd');

copy_test_files($tempdir, ["pheno15.mod", "pheno.dta", "pheno5.mod", "pheno5_ignore.mod", "pheno5.dta","pheno5.lst", "mox1.mod","mox1.lst", "mox_simulated.csv"]);
chdir($tempdir);

my @commands = 
	(get_command('cdd') . " -case_column=ID $tempdir/pheno15.mod -xv -directory=cdd_rplots -rplots=2 ",
	 get_command('cdd') . " $tempdir/mox1.mod -case_column=DGRP ",
	);

foreach my $command (@commands) {
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

# Test rplots
my %pdf_files_pages=('cdd_rplots/PsN_cdd_plots.pdf' => 5);

includes::test_pdf_pages(\%pdf_files_pages);

# Test dofv calculation when one line was ignored

my $rc = system(get_command('cdd') . " $tempdir/pheno5_ignore.mod -dir=cdd_ignore -clean=0");
$rc = $rc >> 8;
is ($rc, 0, "cdd with ignored individuals run without crash");
chdir("cdd_ignore");

open my $fh, "<", "raw_results_pheno5_ignore.csv";
<$fh>;
my $orig_line = <$fh>;
my @a = split ',', $orig_line;

my $orig_ofv = $a[19];

my @ofv;
my @dofv;
while (my $line = <$fh>) {
    @a = split ',', $line;
    push @ofv, $a[19]; 
    push @dofv, $a[42]; 
}
close $fh;

chdir("orig_modelfit_dir1/NM_run1");
open my $phih, "<", "psn.phi";
<$phih>;
<$phih>;

my @iofv;
while (my $line = <$phih>) {
    $line =~ s/^\s*//;
    @a = split /\s+/, $line;
    push @iofv, $a[7];
}
close $phih;

for (my $i = 0; $i < scalar(@ofv); $i++) {
    cmp_relative ($dofv[$i], $orig_ofv - $ofv[$i] - $iofv[$i], 4, "dofv $i");
}


remove_test_dir($tempdir);

done_testing();
