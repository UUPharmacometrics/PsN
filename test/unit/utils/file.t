#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Config;

use utils::file qw(:all);

# get_file_stem
if ($Config{osname} ne 'MSWin32') {
    utils::file::_set_unix();
    is (get_file_stem('/usr/bin/thisfile.rexx'), 'thisfile', "get_file_stem 1");
    is (get_file_stem('pheno.SO.xml'), 'pheno.SO', "get_file_stem 2");
} else {
    utils::file::_set_windows();
    is (get_file_stem('C:\mydir\dir2\myfile.ctl'), 'myfile', "get_file_stem 3");
    is (get_file_stem('mod.2.ctl'), 'mod.2', "get_file_stem 4");
}

# replace_extension
is (replace_extension("pheno.lst", "SO.xml"), "pheno.SO.xml", "replace_extension lst");
is (replace_extension("noext", "lst"), "noext.lst", "replace_extension noext");

# slurp_file
my $testdir = create_test_dir("utils_file_test");
chdir $testdir;

open my $fh, ">", "ksrfile.tmp";
print $fh "Row1\xARow2\xA";
close $fh;
my @a = utils::file::slurp_file("ksrfile.tmp");
is_deeply(\@a, [ "Row1\xA", "Row2\xA" ], "slurp_file unix");

open my $fh, ">", "ksrfile.tmp";
print $fh "Row1\xARow2\xD\xA";
close $fh;
my @a = utils::file::slurp_file("ksrfile.tmp");
is_deeply(\@a, [ "Row1\xA", "Row2\xA" ], "slurp_file unix with CRs");

open my $fh, ">", "ksrfile.tmp";
print $fh "Row1\xD\xARow2\xD\xA";
close $fh;
my @a = utils::file::slurp_file("ksrfile.tmp");
is_deeply(\@a, [ "Row1\xA", "Row2\xA" ], "slurp_file windows");

open my $fh, ">", "ksrfile.tmp";
print $fh "R\xDow1\xD\xARo\rw2\xD\xA";
close $fh;
my @a = utils::file::slurp_file("ksrfile.tmp");
is_deeply(\@a, [ "Row1\xA", "Row2\xA" ], "slurp_file windows extra CRs");

unlink "ksrfile.tmp";

remove_test_dir($testdir);

done_testing();
