#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests=>4;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages

use model;
use model::problem;

my $modeldir = "../test_files";
my $model = model->new(filename => "$modeldir/pheno.mod");
my $problem = $model->problems->[0];

# Test get_full_omega method
my $full_omega = $problem->get_full_omega;

ok ($full_omega->[0]->[0] == 0.4000, "get_full_omega 0,0");
ok ($full_omega->[0]->[1] == 0.0000, "get_full_omega 0,1");
ok ($full_omega->[1]->[0] == 0.0099, "get_full_omega 1,0");
ok ($full_omega->[1]->[1] == 0.2500, "get_full_omega 1,1");


done_testing();
