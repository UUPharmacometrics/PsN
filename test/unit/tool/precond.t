#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

use tool::precond;
use model;

our $test_files = $includes::testfiledir;

# Mock mkpath to avoid directory creation
*tool::mkpath = sub { 1 };

my $dummy_model = model::create_dummy_model();
my $model = model->new(
    filename => "$test_files/pheno.mod",
    ignore_missing_files => 1,
    skip_data_parsing => 1,
    ignore_missing_data => 1,
);


# preprocess_precond_matrix
my $matrix = [[1, 2], [2, 1]];
tool::precond::preprocess_precond_matrix(precond_matrix => $matrix, nthetas => 2), 
is_deeply ($matrix, [[1, 2], [2, 1]], "preprocess_precond_matrix 2x2 nochange");

my $matrix = [[1, 2], [2, 1]];
tool::precond::preprocess_precond_matrix(precond_matrix => $matrix, nthetas => 3);
is_deeply($matrix, [[1, 2, 0], [2, 1, 0], [0, 0, 1]], "preprocess_precond_matrix 2x2 augment");

my $matrix = [[1, 2, 3], [2, 5, 4], [3, 4, 17]];
tool::precond::preprocess_precond_matrix(precond_matrix => $matrix, nthetas => 2);
is_deeply($matrix, [[1, 2], [2, 5]], "preprocess_precond_matrix 3x3 reduce");

my $matrix = [[0, 0, 0], [0, 2, 3], [0, 3, 5]];
tool::precond::preprocess_precond_matrix(precond_matrix => $matrix, nthetas => 3);
is_deeply($matrix, [[1, 0, 0], [0, 2, 3], [0, 3, 5]], "preprocess_precond_matrix 3x3 zero row");

my $matrix = [[0, 0, 0], [0, 2, 3], [0, 3, 5]];
tool::precond::preprocess_precond_matrix(precond_matrix => $matrix, nthetas => 2);
is_deeply($matrix, [[1, 0], [0, 2]], "preprocess_precond_matrix 3x3 zero row and reduce");


#my $precond = tool::precond->new(
#    models => [ $dummy_model ],
#    precond_model => $model,
#    precond_matrix => $matrix, 
#);



done_testing();
