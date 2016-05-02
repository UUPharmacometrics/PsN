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
is_deeply($t->get_header, ['CL','V','IVV'], "get_header");

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

$t = table->new(delimiter => ',', skip_leading_whitespace => 0);
my @arr = $t->_split_row(' A,B,C');
is_deeply(\@arr,[' A','B','C'],'split with delimiter comma');
$t = table->new();
@arr = $t->_split_row(' A,B,C');
is_deeply(\@arr,['A,B,C'],'split with default delimiter ');
$t = table->new(delimiter => '\t');
@arr = $t->_split_row(" A B\t C");
is_deeply(\@arr,['A B',' C'],'split with delimiter tab');
@arr = $t->_split_row(" A B\t\t C");
is_deeply(\@arr,['A B','',' C'],'split with delimiter tab 2');
$t = table->new(delimiter => '\t+');
@arr = $t->_split_row(" A B\t\t C");
is_deeply(\@arr,['A B',' C'],'split with delimiter tab 3');
$t = table->new(delimiter => 'B');
@arr = $t->_split_row(" A B\t\t C");
is_deeply(\@arr,['A ',"\t\t C"],'split with delimiter B');
done_testing();
