#!/usr/bin/perl

# Unit tests for binning.pm
# Note that only a small portion of the module is tested

use strict;
use warnings;
use Test::More tests => 50;
use Test::Deep;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use binning;

my (@a, @b, $a, $b);

# Test of variability
is (binning::variability([8, 8, 8, 8]), 0, "No variability");
is (binning::variability([1, 2, 3, 4]), 5, "Simple variability");
is (binning::variability([1, 2, 3, 4], 2.5), 5, "Simple variability with provided mean"); 
dies_ok { binning::variability(undef) } "Variability of an undef array";
dies_ok { binning::variability([]) } "Variability of an empty array";

# Test of any
ok (!(binning::any { $_ == 0 } 1..4), "Any 0");
ok ((binning::any { $_ == 2 } (9, 9, 1, 2)), "Any 1");

# Test of gaussFilter
@a = qw(1 2 3 5);
@b = qw(2 1 2 3);
$b = binning::linspace(1, 2, 5);
$a = binning::gaussFilter(\@a, \@b, $b, 0.2);

#truncate 0 and 4 to 12 and 11 decimals
cmp_float ($$a[0], 0.755854279357, "gaussFilter 0");
cmp_float ($$a[1], 0.125588126607087, "gaussFilter 1");
cmp_float ($$a[2], 0.000864113627775956, "gaussFilter 2");
cmp_float ($$a[3], 0.06279411801682412, "gaussFilter 3");
cmp_float ($$a[4], 0.37792713967868, "gaussFilter 4");

# Test of equalSize
@a = qw(93 1 1 60 1 7 1 5 5 1 1 3 2 5 1 1 1 62 7 1 5 5 1 4 1 5 1 1 1 1 1 60 8 1 5 7 1 1 2 1 4 2 1 1 1 1 1 61 6 1 6 5 1 1 1 2 4 2 1 1 92 62 1 4 6 4 3 1 3 1 2 1 3 59 2 1 5 6 5 2 2 3 2 2 1 1 1 57
		   1 6 1 6 7 1 2 2 1 5 1 1 1 57 1 7 6 5 3 1 1 1 3 1 1 2 1 80 1 3 1 1 57 10 1 7 1 3 1 1 2 2 52 11 1 7 6 2 1 2 1 2 2 1 2 53 1 7 1 9 3 1 2 2 2 1 1 1 2 1 53 11 1 8 2 1 2 1 2 1 1 2);

($a, $b) = binning::equalSize(170, \@a, 1364, 16, 10);

my @edgeIndices = qw(0 1 8 19 32 48 60 61 73 87 101 115 120 130 143 158 170);

foreach my $i (0..@$a - 1) {
	is ($$a[$i], $edgeIndices[$i], "edgeIndices $i");
}

my @binN = qw(93 76 89 87 98 31 92 91 92 92 90 86 85 90 87 85);

foreach my $i (0..@$b - 1) {
	is ($$b[$i], $binN[$i], "binN $i");
}

# Test of negB
my @N = qw(32 21 20 12 10);
my @meanIdv = qw(3.236037499999986e+01 3.250566666666644e+01 3.252509999999984e+01 3.276383333333312e+01 3.286659999999974e+01);
my $totMeanIdv = 1.473203812536656e+01;
my @negB_result = qw(-9944.2643508978 -6633.93930200631 -6331.86101749057 -3901.74766106455 -3288.62334384899);		# Differs in the final decimals from matlab

my $negB = binning::calcNegB(\@N, \@meanIdv, $totMeanIdv);

foreach my $i (0..@$negB - 1) {
	cmp_deeply ($$negB[$i], fnum($negB_result[$i]), "negB $i");
}

done_testing();
