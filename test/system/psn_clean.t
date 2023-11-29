#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

sub create_file
{
    my $name = shift;
    open my $fh, ">", $name;
    print $fh "file created by test";
    close $fh;
}

our $tempdir = create_test_dir('system_psn_clean');

copy_test_files($tempdir, ["pheno.mod", "pheno.dta"]);
chdir($tempdir);
system(get_command('execute')." pheno.mod -dir=mydir -seed=52 -min_ret=1 -tweak -no-disp -clean=0");
ok (-e "mydir/NM_run1", "NM_run1 ok");
ok (-e "mydir/NM_run1/patab1", "patab1 ok");
ok (((-e "mydir/NM_run1/psn-1.mod") or (-e "mydir/NM_run1/psn-1.ctl")) , "psn-1.mod ok");

# remove tables
system(get_command('psn_clean')." mydir -no-interactive -tab");
ok (not (-e "mydir/NM_run1/patab1"), "psn_clean removed sdtab");

# remove restart files
create_file("mydir/NM_run1/psn-1.mod");
ok ((-e "mydir/NM_run1/psn-1.mod"), "psn-1.mod ok");
system(get_command('psn_clean')." mydir -no-interactive");
ok (not ((-e "mydir/NM_run1/psn-1.mod") or (-e "mydir/NM_run1/psn-1.ctl")), "psn_clean removed psn-1.mod");

# lst
create_file("mydir/NM_run1/testing.lst");
ok ((-e "mydir/NM_run1/testing.lst"), "testing ok");
system(get_command('psn_clean')." mydir -no-interactive -lst");
ok (not (-e "mydir/NM_run1/testing.lst"), "psn_clean removed lst file");

# level 3
system(get_command('psn_clean')." mydir -no-interactive -level=3");
ok (not (-e "mydir/NM_run1"), "psn_clean NM_run1 removed");

remove_test_dir($tempdir);

done_testing();
