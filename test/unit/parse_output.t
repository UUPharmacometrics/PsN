#!/usr/bin/perl

use strict;
use warnings;
use Test::More;# tests=>8;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Scalar::Util qw(looks_like_number);
use output::problem::subproblem qw(_get_value);

my %hash;
$hash{'+Infinity'}=undef;
$hash{'-Infinity'}=undef;
$hash{'+Inf'}=undef;
$hash{'-Inf'}=undef;
$hash{'+INF'}=undef;
$hash{'-INF'}=undef;
$hash{'INF'}=undef;
$hash{'inf'}=undef;
$hash{'-inf'}=undef;
$hash{'NA'}=undef;
$hash{'NaN'}=undef;
$hash{''}=undef;
$hash{'10000000000'}=undef;
$hash{'1E+00'}=1;
$hash{'1.01E+01'}=10.1;
$hash{'12345'}=12345;

plan tests => scalar(keys %hash);

foreach my $key (keys %hash){
	is (output::problem::subproblem::_get_value(val => $key),$hash{$key},"_get_value $key");
}



done_testing();
