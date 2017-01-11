#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use array qw(:all);

my (@a, @b, $a, $b, $c);

# find_zeros
@a = qw(-1 0 23 -14.5 0);
is_deeply(array::find_zeros(\@a), [1,4], "find_zeros 2");
is_deeply(array::find_zeros([1,2,3]), [], "find_zeros none");

#is_zero
is_deeply(array::is_zero([1,2,3]), [0,0,0], "is_zero none");
is_deeply(array::is_zero([1,2,0]), [0,0,1], "is_zero one");
is_deeply(array::is_zero([0,2,0]), [1,0,1], "is_zero two");

# get_intersection
is_deeply(array::get_intersection(arr1 => [1,2,3,4], arr2 =>[3,5,6]),[3],'get_intersection 1');
is_deeply(array::get_intersection(arr1 => [1,2,3,4], arr2 =>[1,5,6]),[1],'get_intersection 2');
is_deeply(array::get_intersection(arr1 => [1,2,3,4], arr2 =>[1,2,3,4]),[1,2,3,4],'get_intersection 3');
is_deeply(array::get_intersection(arr1 => [1,2,3,4], arr2 =>[5,6]),[],'get_intersection 4');
is_deeply(array::get_intersection(arr1 => [1], arr2 =>[5,6,1]),[1],'get_intersection 5');

		  
#any_nonzero
ok (!array::any_nonzero([0, 0, 0]), "any_nonzero all zeros");
ok (!array::any_nonzero([]), "any_nonzero empty array");
ok (array::any_nonzero([0,-2,0]), "any_nonzero neg");

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

# absolute
@a = qw(-1 1 23 -14.5 14);
is_deeply(array::absolute(\@a), [ 1, 1, 23, 14.5, 14 ], "absolute");

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

# Test of remove_adjacent_duplicates
is_deeply (array::remove_adjacent_duplicates([ 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 2]), [ 1, 2, 1, 2 ], "remove_adjacent_duplicates 1");
is_deeply (array::remove_adjacent_duplicates([ 23 ]), [ 23 ], "remove_adjacent_duplicates 2");
is_deeply (array::remove_adjacent_duplicates([ 1, 2, 3, 4, 4, 4, 5, 1, 2, 3, 5, 5 ]), [ 1, 2, 3, 4, 5, 1, 2, 3, 5 ], "remove_adjacent_duplicates 3");
is_deeply (array::remove_adjacent_duplicates([ ]), [ ], "remove_adjacent_duplicates 4");


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

# median_and_ci
@a = ();
my @ans = array::median_and_ci(\@a,95);
is_deeply(\@ans, [undef,undef,undef], "median_and_ci 1");
@a = qw(0.4 0.5 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.6 0.7 0.7 0.7 0.7 0.7 0.7 0.7 0.7 0.7 0.7 0.7 0.7 0.7 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.8 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1);
@ans = array::median_and_ci(\@a,95);
is_deeply (\@ans, [1,0.6,1], "median_and_ci 2");

@a = qw(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20);
@ans = array::median_and_ci(\@a,95);
is_deeply (\@ans, [10.5,1,20], "median_and_ci 3");

@a = qw(1 2 3 4 5 6 7);
@ans = array::median_and_ci(\@a,95);
is_deeply (\@ans, [4,1,7], "median_and_ci 4");

# Test of quartiles
@a = qw(1 2 4 4);
my @res = array::quartiles(\@a);
is_deeply (\@res, [ 1.5, 3, 4 ], "quartiles 1");
@a = qw(4 3 2 5 1);
@res = array::quartiles(\@a);
is_deeply (\@res, [ 1.5, 3, 4.5 ], "quartiles 2");
@a = qw(9 10 11 12 13 14 15 16);
@res = array::quartiles(\@a);
is_deeply (\@res, [ 10.5, 12.5, 14.5 ], "quartiles 3");
@a = qw(4 6 8 10 12 24);
@res = array::quartiles(\@a);
is_deeply (\@res, [ 6, 9, 12 ], "quartiles 4");

# Test of variance
@a = qw(1 2 4 4);
is (array::variance(\@a), 2.25, "A simple variance"); 

# Test of stdev
@a = qw(3 5 9 8);
cmp_float (array::stdev(\@a), 2.753785273643051, "A simple stdev");
#test of rse
cmp_float(array::rse(\@a,10),0.2753785273643051,"relative standard error of expectation");

@a = qw(1);
is (array::stdev(\@a), undef, "stdev of only one value");


#test of sem
@a = qw(3 -5 0 -100 3);
cmp_float(array::sem(\@a),20.103233570746770,"standard error of the mean");


# Test of is_int
ok (array::is_int([1, 2, 3, 4]), "is_int integer array");
ok (!array::is_int([1, 2, 3, 3.3]), "is_int array with float");
ok (array::is_int([[1,2,3],[4,5]]), "is_int two dimensional int array");
ok (!array::is_int([[1,2,3], ['opel', 34]]), "is_int two dim nonint array");

#test of pvalues
my @sorted = (1,2,3,4,5,6,7,8,9,10);
my @values = (0,1,1.5,5,5.5,6.5,10,11);

my $pvals = percentile(sorted_numbers => \@sorted,
					test_values => \@values);

is ($pvals->[0],1/11, "pvals ".$values[0]); #1/11
is ($pvals->[1],1/10, "pvals ".$values[1]); #1/10
is ($pvals->[2],1/10, "pvals ".$values[2]);
is ($pvals->[3],5/10, "pvals ".$values[3]);
is ($pvals->[4],5/10, "pvals ".$values[4]);
is ($pvals->[5],6/10, "pvals ".$values[5]);
is ($pvals->[6],10/10, "pvals ".$values[6]);
is ($pvals->[7],1, "pvals ".$values[7]);

# is_equal
ok (array::is_equal([1, 2, 3, 4], [1, 2, 3, 4]), "equal arrays");
ok (!array::is_equal([1, 2, 3, 4], [1, 2, 3, 5]), "not equal arrays");
ok (!array::is_equal([1, 2, 3, 4], [1, 2, 3, 4, 5]), "not equal arrays 2");

my $target = ['sample.id','model','problem','subproblem','covariance_step_run','minimization_successful',
	'covariance_step_successful','covariance_step_warnings','estimate_near_boundary','rounding_errors',
	'zero_gradients','final_zero_gradients','hessian_reset','s_matrix_singular','significant_digits','condition_number',
	'est_methods','model_run_time','subprob_est_time','subprob_cov_time','ofv','deltaofv','likelihood_ratio','relPDF',
	'importance_ratio','probability_resample','resamples','CL','V','IVCL','IVV','SIGMA(1,1)','seCL','seV','seIVCL',
	'seIVV','seSIGMA(1,1)','shrinkage_eta1(%)','shrinkage_eta2(%)','shrinkage_iwres(%)','EI1','EI2','EI3','EI4','EI5'];

my $keys =['CL','V','IVCL','IVV','SIGMA(1,1)'];
is_deeply(get_array_positions(target => $target,keys=> $keys),['28:32'],'get_array_positions 1 ');

$keys =['CL','IVCL','SIGMA(1,1)'];
is_deeply(get_array_positions(target => $target,keys=> $keys),[28,30,32],'get_array_positions 2 ');

$keys =['CL','V','IVV','SIGMA(1,1)'];
is_deeply(get_array_positions(target => $target,keys=> $keys),['28:29','31:32'],'get_array_positions 3 ');

$keys =['CL','V','IVV'];
is_deeply(get_array_positions(target => $target,keys=> $keys),['28:29',31],'get_array_positions 4 ');

$keys =['CL','V','IVCL','IVV','SIGMA(1,1)'];
is_deeply(get_array_positions(target => $target,keys=> $keys, R_indexing => 0),[27,28,29,30,31],'get_array_positions 5 ');

$keys =['CL','IVCL','SIGMA(1,1)'];
is_deeply(get_array_positions(target => $target,keys=> $keys, R_indexing => 0),[27,29,31],'get_array_positions 6 ');

$keys =['CL','V','IVV','SIGMA(1,1)'];
is_deeply(get_array_positions(target => $target,keys=> $keys, R_indexing => 0),[27,28,30,31],'get_array_positions 7 ');

$keys =['CL','V','IVV'];
is_deeply(get_array_positions(target => $target,keys=> $keys, R_indexing => 0),[27,28,30],'get_array_positions 8 ');

$keys =['IVCL','CL','SIGMA(1,1)','V','IVV'];
is_deeply(get_positions(target => $target,keys=> $keys),[29,27,31,28,30],'get_positions 1 ');

is(count_lower(array => [1,2,3],limit => 1.0),0,'count_lower 1');
is(count_lower(array => [],limit => 1.0),0,'count_lower 2');
is(count_lower(array => [0,1E-16,3],limit => 0.0000000001),2,'count_lower 3');
is(count_lower(array => [0.0000000001,0.000000000012,0.00000000001],limit => 0.0000000001),2,'count_lower 4');

done_testing();
