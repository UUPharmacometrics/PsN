#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use citations;

my $references = citations::_scan_bib_file(filename => '../test_files/simple.bib', keyword => 'keyw');
is ($references->[1], "    item1,\n", "citations itemname");
is (scalar @$references, 5, "citations: number of rows captured");

my $references = citations::_scan_bib_file(filename => '../test_files/simple.bib', keyword => 'none');
is (scalar @$references, 10, "citations: none, number of rows captured");

done_testing();
