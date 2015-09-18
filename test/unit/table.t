#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use table;

# set_header
my $t = table->new();
$t->set_header(header => "  CL  V  IVV");
is_deeply($t->header, { CL => 0, V => 1, IVV => 2 }, "set_header");

# add_row
$t->add_row(row => "   50 28   34");
is_deeply($t->columns, [ ['50'], ['28'], ['34'] ], "add_row");
$t->skip_leading_whitespace(0);
$t->add_row(row => "100 3.14 cov");
is_deeply($t->columns, [ [ '50', '100' ], [ '28', '3.14' ], [ '34', 'cov' ] ], "add_row with no skip_leading_whitespace");
$t->add_row(row => "   2 3");
is_deeply($t->columns, [ [ '50', '100', '' ], [ '28', '3.14', '2' ], [ '34', 'cov', '3' ] ], "add_row with empty first column");

# get_column
is_deeply($t->get_column(name => "CL"), [ '50', '100', '' ], "get_column using name");
is_deeply($t->get_column(index => 2), [ '34', 'cov', '3' ], "get_column using index");

done_testing();
