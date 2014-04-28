#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests=>3;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data::individual;


my $ind = data::individual->new(subject_data => ['1,0.0000e0,1.0000e0', '1,1.0000e0,0.0000e0', '1,1.0000e0,0.0000e0']);

#factors
my %factors = %{$ind->factors(column => 2)};
is_array([sort keys %factors], ['0.0000e0', '1.0000e0'], "data::individual->factors");


done_testing;
