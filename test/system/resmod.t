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
	my ($major,$minor,$dirt) = get_major_minor_nm_version();
    skip $major.".".$minor." is a too old NONMEM version for resmod",1 if ($major < 7 or ($major == 7 and $minor < 3));

    our $tempdir = create_test_dir('system_resmod');
    chdir($tempdir);
    my $model_dir = $includes::testfiledir;
    copy("$model_dir/resmod/sdtab", $tempdir);
    copy("$model_dir/resmod/pheno.mod", $tempdir);


    my @commands = ( 
        get_command('resmod') . " pheno.mod -dir=run",
    );

    foreach my $command (@commands){
        print "Running $command\n";
        my $rc = system($command);
        $rc = $rc >> 8;
        ok ($rc == 0, "$command, should run ok");
    }

    chdir("run");

    ok (-e "resmod_results.csv", "Created resmod_results.csv");

    remove_test_dir($tempdir);
}

done_testing();
