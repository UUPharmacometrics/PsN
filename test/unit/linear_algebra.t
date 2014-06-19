#!/usr/bin/perl

use strict;
use warnings;
use Test::More;# tests=>8;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use linear_algebra;


plan tests => 15;

my @matrix = ([3,1,0],[1,5,2],[0,2,4]);
my $result = [];
my $err = linear_algebra::invert_symmetric(\@matrix,$result);

is ($err,0,'invert_symmetric success');
cmp_float($result->[0]->[0],0.363636363636364,'invert_symmetric (1,1)');
cmp_float($result->[0]->[1],-0.090909090909091,'invert_symmetric (1,2)');
cmp_float($result->[0]->[2],0.045454545454545,'invert_symmetric (1,3)');
cmp_float($result->[1]->[0],-0.090909090909091,'invert_symmetric (2,1)');
cmp_float($result->[1]->[1],0.272727272727273,'invert_symmetric (2,2)');
cmp_float($result->[1]->[2],-0.136363636363636,'invert_symmetric (2,3)');
cmp_float($result->[2]->[0],0.045454545454545,'invert_symmetric (3,1)');
cmp_float($result->[2]->[1],-0.136363636363636,'invert_symmetric (3,2)');
cmp_float($result->[2]->[2],0.318181818181818,'invert_symmetric (3,3)');

my @matrix = ([3,1,0],[1,5,0],[0,0,0]);
my $result = [];
my $err = linear_algebra::invert_symmetric(\@matrix,$result);

is ($err,1,'invert_symmetric numerr');

my $identity = linear_algebra::get_identity_matrix(2);
is ($identity->[0]->[0],1,'identity_matrix 1,1');
is ($identity->[0]->[1],0,'identity_matrix 1,2');
is ($identity->[1]->[0],0,'identity_matrix 2,1');
is ($identity->[1]->[1],1,'identity_matrix 2,2');

done_testing();
