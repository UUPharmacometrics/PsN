#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use nmtable;

# set_header
my $t = nmtable->new();
$t->read_table_row(row => "TABLE NO.     1: First Order: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=2 Subproblem=3 Superproblem1=5 Iteration1=7 Superproblem2=11 Iteration2=107");
is($t->table_number, 1, "read_table_row table_number");
is($t->problem, 2, "read_table_row problem");
is($t->subproblem, 3, "read_table_row subproblem");
is($t->superproblem1, 5, "read_table_row superproblem");
is($t->iteration1, 7, "read_table_row iteration1");
is($t->superproblem2, 11, "read_table_row superproblem2");
is($t->iteration2, 107, "read_table_row iteration2");
is($t->method, "First Order", "read_table_row method");
is($t->goal_function, "MINIMUM VALUE OF OBJECTIVE FUNCTION", "read_table_row goal_function");

$t->read_table_row(row => "TABLE NO.     3: MCMC Bayesian Analysis: Goal Function=AVERAGE VALUE OF LIKELIHOOD FUNCTION: Problem=3 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0");
is($t->table_number, 3, "read_table_row 2 table_number");
is($t->problem, 3, "read_table_row 2 problem");
is($t->subproblem, 0, "read_table_row 2 subproblem");
is($t->superproblem1, 0, "read_table_row 2 superproblem");
is($t->iteration1, 0, "read_table_row 2 iteration1");
is($t->superproblem2, 0, "read_table_row 2 superproblem2");
is($t->iteration2, 0, "read_table_row 2 iteration2");
is($t->method, "MCMC Bayesian Analysis", "read_table_row 2 method");
is($t->goal_function, "AVERAGE VALUE OF LIKELIHOOD FUNCTION", "read_table_row 2 goal_function");

done_testing();
