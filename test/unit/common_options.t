#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use common_options;

# get_option_array
is_deeply (common_options::get_option_array('4,5,7.2'), [ 4, 5, 7.2 ], "get_option_array numeric");
is_deeply (common_options::get_option_array('five,six'), [ 'five', 'six' ], "get_option_array string");
is_deeply (common_options::get_option_array('23'), [ 23 ], "get_option_array single value");
is (common_options::get_option_array(',,'), undef, "get_option_array error");

# get_option_matrix
my $matrix = common_options::get_option_matrix('1,2,3:4,5,6:7,8,9');
is (scalar(@$matrix), 3, "get_option_matrix numeric size");
is_deeply ($matrix->[0], [1, 2, 3], "get_option_matrix numeric row 1");
is_deeply ($matrix->[1], [4, 5, 6], "get_option_matrix numeric row 2");
is_deeply ($matrix->[2], [7, 8, 9], "get_option_matrix numeric row 3");

$matrix = common_options::get_option_matrix('nine,ten:apple,orange');
is (scalar(@$matrix), 2, "get_option_matrix string size");
is_deeply ($matrix->[0], ['nine', 'ten'], "get_option_matrix string row 1");
is_deeply ($matrix->[1], ['apple', 'orange'], "get_option_matrix string row 2");

$matrix = common_options::get_option_matrix('2:3:5');
is (scalar(@$matrix), 3, "get_option_matrix single value per row");
is_deeply ($matrix->[0], [2], "get_option_matrix single value per row 1");
is_deeply ($matrix->[1], [3], "get_option_matrix single value per row 1");
is_deeply ($matrix->[2], [5], "get_option_matrix single value per row 1");

$matrix = common_options::get_option_matrix('7,11,13');
is (scalar(@$matrix), 1, "get_option_matrix single row");
is_deeply ($matrix->[0], [7, 11, 13], "get_option_matrix single row 1");

$matrix = common_options::get_option_matrix('23.3');
is (scalar(@$matrix), 1, "get_option_matrix single value");
is_deeply ($matrix->[0], [ 23.3 ], "get_option_matrix signle value 1");

done_testing();
