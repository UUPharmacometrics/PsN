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

# remove_path
utils::file::_set_unix();      # Force unix
is (remove_path('/usr/bin/myfile'), 'myfile', "remove_path 1");
is (remove_path('/tmp/host/model.SO.xml'), 'model.SO.xml', "remove_path 2");
is (remove_path('justfile.pm'), 'justfile.pm', "remove_path only file in unix");
utils::file::_set_windows();    # Force windows
is (remove_path('C:\mydir\dir2\myfile'), 'myfile', "remove_path 3");
is (remove_path('this\that\myfile.SO.xml'), 'myfile.SO.xml', "remove_path 4");
is (remove_path('myfile.ctl'), 'myfile.ctl', "remove_path only file in Windows");

# get_file_stem
utils::file::_set_unix();
is (get_file_stem('/usr/bin/thisfile.rexx'), 'thisfile', "get_file_stem 1");
is (get_file_stem('pheno.SO.xml'), 'pheno.SO', "get_file_stem 2");
utils::file::_set_windows();
is (get_file_stem('C:\mydir\dir2\myfile.ctl'), 'myfile', "get_file_stem 3");
is (get_file_stem('mod.2.ctl'), 'mod.2', "get_file_stem 4");

# replace_extension
is (replace_extension("pheno.lst", "SO.xml"), "pheno.SO.xml", "replace_extension lst");
is (replace_extension("noext", "lst"), "noext.lst", "replace_extension noext");

# directory
utils::file::_set_unix();
is (directory('/etc/share/tjo.c'), '/etc/share/', "directory with dir on unix");
is (directory('nodir'), './', "directory nodir in unix");
utils::file::_set_windows();
is (directory('C:\dir\dir2\file.h'), "C:\\dir\\dir2\\", "directory with dir on windows");
is (directory('nodir'), ".\\", "directory nodir in windows");

# slurp_file
utils::file::_set_unix();

open my $fh, ">", "ksrfile.tmp";
print $fh "Row1\xARow2\xA";
close $fh;
my @a = utils::file::slurp_file("ksrfile.tmp");
is_deeply(\@a, [ "Row1\xA", "Row2\xA" ], "slurp_file unix");
open my $fh, ">", "ksrfile.tmp";
print $fh "R\xDow1\xARow2\xD\xA";
close $fh;
my @a = utils::file::slurp_file("ksrfile.tmp");
is_deeply(\@a, [ "Row1\xA", "Row2\xA" ], "slurp_file unix with CRs");

utils::file::_set_windows();
open my $fh, ">", "ksrfile.tmp";
print $fh "Row1\xD\xARow2\xD\xA";
close $fh;
my @a = utils::file::slurp_file("ksrfile.tmp");
is_deeply(\@a, [ "Row1\xD\xA", "Row2\xD\xA" ], "slurp_file windows");
open my $fh, ">", "ksrfile.tmp";
print $fh "R\xDow1\xD\xARo\rw2\xD\xA";
close $fh;
my @a = utils::file::slurp_file("ksrfile.tmp");
is_deeply(\@a, [ "Row1\xD\xA", "Row2\xD\xA" ], "slurp_file windows");

unlink "ksrfile.tmp";

done_testing();
