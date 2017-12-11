#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model_approximations;

my $modeldir = $includes::testfiledir;

my $model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);

my $derivatives_model = model_approximations::second_order_derivatives_model(model => $model);

is (scalar(@{$derivatives_model->problems->[0]->pks->[0]->code}), 15, "2nd order Derivatives model reset");
is (scalar(@{$derivatives_model->problems->[0]->errors->[0]->code}), 19, "2nd order Derivatives model error");

my $approximation_model = model_approximations::second_order_approximation_model(model => $model);



done_testing();
