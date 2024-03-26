#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use File::Copy 'copy';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed" if $@;

    our $tempdir = create_test_dir('system_nmoutput2so');
    my $model_dir = $includes::testfiledir;
    copy_test_files($tempdir, [ "pheno.lst", "pheno.mod", "pheno.dta" ]);
    chdir($tempdir);

    my @commands = (
        get_command('nmoutput2so') . " pheno.lst",
        get_command('execute') ." pheno.mod -so ",
    );
    foreach my $command (@commands){
        print "Running $command\n";
        my $rc = system($command);
        $rc = $rc >> 8;
        ok ($rc == 0, "$command, should run ok");
    }

    remove_test_dir($tempdir);
}

done_testing();
