#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use random qw(random_multivariate_normal);


my @mu = (0, 0);
my @cov = ([2, 1], [1, 2]);
my @random = random_multivariate_normal(1, @mu, @cov); 

is(scalar(@random), 1, "Size");

done_testing();
