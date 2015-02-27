#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use PsN;

SKIP: {
    PsN::set_nonmem_info("default");
    print $PsN::nm_major_version, ".", $PsN::nm_minor_version . "\n";
    skip "too old NONMEM version for precond" if ($PsN::nm_major_version < 7 or ($PsN::nm_major_version == 7 and $PsN::nm_minor_version < 2));

    our $tempdir = create_test_dir('system_precond');
    our $dir = "$tempdir/precond_test";
    my $model_dir = $includes::testfiledir;

    copy_test_files($tempdir, ["run1.mod", "data.csv"]);

    chdir $tempdir;

    my @commands =  (
        get_command('execute') . " run1.mod -dir=modelfit_dir1 -clean=2",
        get_command('precond') . " run1.mod -clean=2",
    );

    foreach my $command (@commands) {
        print "Running $command\n";
        my $rc = system($command);
        $rc = $rc >> 8;
        ok ($rc == 0, "$command, should run ok");
        rmtree(["$dir"]);
    }
    rmtree(["$dir"]);
    remove_test_dir($tempdir);

}

done_testing();
