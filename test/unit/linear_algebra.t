#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use linear_algebra;


plan tests => 85;

#pad_matrix
my @A = ([1, 2, 4], [3, 5 ,7], [8, 4, 1]);
linear_algebra::pad_matrix(\@A, 5);

is(scalar(@A), 5, "pad_matrix num rows");
is_array(@A[0], [1, 2, 4, 0, 0], "pad_matrix row 1");
is_array(@A[1], [3, 5, 7, 0, 0], "pad_matrix row 2");
is_array(@A[2], [8, 4, 1, 0, 0], "pad_matrix row 3");
is_array(@A[3], [0, 0, 0, 1, 0], "pad_matrix row 4");
is_array(@A[4], [0, 0, 0, 0, 1], "pad_matrix row 5");

#reduce_matrix
my @A = ([1, 3, 5, 7], [8, 6, 4, 2], [1, 2, 3, 4], [6, 7, 9, 1]);
linear_algebra::reduce_matrix(\@A, 2);

is(scalar(@A), 2, "reduce_matrix num rows");
is_array(@A[0], [1, 3], "reduce_matrix row 1");
is_array(@A[1], [8, 6], "reduce_matrix row 1");

#put_ones_on_diagonal_of_zero_lines
my @A = ([1, 2, 3, 4], [0, 0, 0, 0], [9, 8, 7, 6], [0, 0, 0, 0]);
linear_algebra::put_ones_on_diagonal_of_zero_lines(\@A);

is_array(@A[0], [1, 2, 3, 4], "put_ones_on... row 1");
is_array(@A[1], [0, 1, 0, 0], "put_ones_on... row 2");
is_array(@A[2], [9, 8, 7, 6], "put_ones_on... row 3");
is_array(@A[3], [0, 0, 0, 1], "put_ones_on... row 4");

#LU_factorization
my @B = ([8, 4, 1], [5, 5, 2], [4, 2, 2]);
linear_algebra::LU_factorization(\@B);
is_array(@B[0], [8, 4, 1], "lu matrix row 1");
is_array(@B[1], [0.625, 2.5, 1.375], "lu matrix row 2");
is_array(@B[2], [0.5, 0, 1.5], "lu matrix row 3");

#invert_symmetric
my @matrix = ([3,1,0], [1,5,2], [0,2,4]);
my $result = [];
my $err = linear_algebra::invert_symmetric(\@matrix,$result);

is ($err,0,'invert_symmetric success');
cmp_float($result->[0]->[0], 0.363636363636364, 'invert_symmetric (1,1)');
cmp_float($result->[0]->[1], -0.090909090909091, 'invert_symmetric (1,2)');
cmp_float($result->[0]->[2], 0.045454545454545, 'invert_symmetric (1,3)');
cmp_float($result->[1]->[0], -0.090909090909091, 'invert_symmetric (2,1)');
cmp_float($result->[1]->[1], 0.272727272727273, 'invert_symmetric (2,2)');
cmp_float($result->[1]->[2], -0.136363636363636, 'invert_symmetric (2,3)');
cmp_float($result->[2]->[0], 0.045454545454545, 'invert_symmetric (3,1)');
cmp_float($result->[2]->[1], -0.136363636363636, 'invert_symmetric (3,2)');
cmp_float($result->[2]->[2], 0.318181818181818, 'invert_symmetric (3,3)');

my @matrix = ([3,1,0], [1,5,0], [0,0,0]);
my $result = [];
my $err = linear_algebra::invert_symmetric(\@matrix,$result);

is ($err,1,'invert_symmetric numerr');

#get_identity_matrix
my $identity = linear_algebra::get_identity_matrix(2);
is ($identity->[0]->[0],1,'identity_matrix 1,1');
is ($identity->[0]->[1],0,'identity_matrix 1,2');
is ($identity->[1]->[0],0,'identity_matrix 2,1');
is ($identity->[1]->[1],1,'identity_matrix 2,2');


done_testing();
