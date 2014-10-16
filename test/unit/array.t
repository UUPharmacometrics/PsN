#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests => 64;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use array qw(:all);

my (@a, @b, $a, $b, $c);

# not_empty
ok (array::not_empty([1, 2, 3]), "not_empty for filled array");
ok (!array::not_empty([]), "not not_empty for empty array");
ok (!array::not_empty(undef), "not not_empty for undef array reference");

# is_empty
ok (!array::is_empty([1, 2, 3]), "not is_empty for filled array");
ok (array::is_empty([]), "is_empty for empty array");
ok (array::is_empty(undef), "is_empty for undef array reference");

# diff
@a = qw(2 5 9 1);
$b = array::diff(\@a);

is ($$b[0], 3, "Simple diff 0");
is ($$b[1], 4, "Simple diff 1");
is ($$b[2], -8, "Simple diff 2");

# Test of cumsum
@a = qw(1 2 3 4);
$b = array::cumsum(\@a);

is ($$b[0], 1, "Cumsum of array 0");
is ($$b[1], 3, "Cumsum of array 1");
is ($$b[2], 6, "Cumsum of array 2");
is ($$b[3], 10, "Cumsum of array 3");

# Test of max
@a = qw (-1 -2 0 5 2);

is (array::max(\@a), 5, "Maximum of a referenced array");
($a, $b) = array::max(\@a);
is ($a, 5, "Maximum of referenced array in array context");
is ($b, 3, "Index of maximum of referenced array in array context");
is (array::max(-1, -2, 0, 5, 2), 5, "Maximum of list");
($a, $b) = array::max(-1, -2, 0, 5, 2);
is ($a, 5, "Maximum of array in array context");
is ($b, 3, "Index of maximum of array in array context");

dies_ok { array::max(undef) } "Array maximum for undef array";

# Test of min
@a = qw (-1 -2 0 1 2);

is (array::min(\@a), -2, "Minimum of a referenced array");
($a, $b) = array::min(\@a);
is ($a, -2, "Minimum of referenced array in array context");
is ($b, 1, "Index of minimum of referenced array in array context");
is (array::min(-1, -2, 0, 1, 2), -2, "Minimum of a list");
($a, $b) = array::min(-1, -2, 0, 1, 2);
is ($a, -2, "Minimum of array in array context");
is ($b, 1, "Index of minimum of array in array context");

dies_ok { array::min(undef) } "Array minimum for undef array";

# Test of linspace
$b = array::linspace(-1, 3, 6);

is ($$b[0], -1, "Linspace 0");
is ($$b[1], -0.2, "Linspace 1");
is ($$b[2], 0.6, "Linspace 2");
is ($$b[3], 1.4, "Linspace 3");
is ($$b[4], 2.2, "Linspace 4");
is ($$b[5], 3, "Linspace 5");

# Test of unique
@a = qw(2 1 -1 5 6 5 1 2);
($a, $b) = array::unique(\@a);

my @unique_res = qw(-1 1 2 5 6);
foreach my $i (0..@$a - 1) {
	is ($$a[$i], $unique_res[$i], "unique $i");
}

my @unique_pos = qw(2 1 0 3 4);
foreach my $i (0..@$b - 1) {
	is ($$b[$i], $unique_pos[$i], "unique $i");
}

$a = array::unique(\@a);
foreach my $i (0..@$a - 1) {
	is ($$a[$i], $unique_res[$i], "unique in scalar context $i");
}

# Test of add
@a = qw(1 2 3 4);
@b = qw(-2 3 7 0);
array::add(\@a, \@b);

is ($a[0], -1, "Sum of two arrays 0");
is ($a[1], 5, "Sum of two arrays 1");
is ($a[2], 10, "Sum of two arrays 2");
is ($a[3], 4, "Sum of two arrays 3");

shift @b;

# Test of sum
@a = qw(1 2 3 4);
is (array::sum(\@a), 10, "A simple sum");
is (array::sum([]), 0, "Sum of an empty array");
dies_ok ( sub { sum(undef) }, "Sum of an undef array");

# Test of mean
is (array::mean(\@a), 2.5, "A simple mean");
dies_ok ( sub { array::mean([]) }, "Mean of an empty array");
dies_ok ( sub { array::mean(undef) }, "Mean of undef array");

# Median
@a = qw(5 2 3);
is (array::median(\@a), 3, "Median 1");
@a = qw(5 4 3 2);
is (array::median(\@a), 3.5, "Median 2");
@a = ();
is (array::median(\@a), 0, "Median 3");
@a = qw(1 2 3 4 5);
is (array::median(\@a), 3, "Median 4");

# Test of variance
@a = qw(1 2 4 4);
is (array::variance(\@a), 2.25, "A simple variance"); 

# Test of stdev
@a = qw(3 5 9 8);
cmp_float (array::stdev(\@a), 2.753785273643051, "A simple stdev");

done_testing();
