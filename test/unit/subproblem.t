#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use output;

my $modeldir = $includes::testfiledir;
my $output = output->new(filename => "$modeldir/output/nm73/theo.lst");
my $subproblem = $output->problems->[0]->subproblems->[0];

is_deeply ($subproblem->est_thetanames, [ 'THETA1', 'THETA2', 'THETA3' ], "est_thetanames for theophylline");
is_deeply ($subproblem->est_omeganames, [ 'OMEGA(1,1)', 'OMEGA(2,1)', 'OMEGA(2,2)', 'OMEGA(3,1)', 'OMEGA(3,2)', 'OMEGA(3,3)' ], "est_omeganames for theophylline");
is_deeply ($subproblem->est_sigmanames, [ 'SIGMA(1,1)' ], "est_sigmanames for theophylline"); 

done_testing();
