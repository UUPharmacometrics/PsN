#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model_transformations;

my $modeldir = $includes::testfiledir;

my $model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);

is (model_transformations::_number_of_etas(model => $model), 2, "_number_of_etas");

my $omega1 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.5 0.1 0.5']);
my $omega2 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.25 0.1 0.25'], n_previous_rows => 2);
my $omega3 = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(2) 0.1 0.2'], n_previous_rows => 4);
my $omega4 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(3) SAME'], n_previous_rows => 6);

$model->problems->[0]->omegas([$omega1, $omega2]);
is (model_transformations::_number_of_etas(model => $model), 4, "_number_of_etas 2");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega1]), [1, 2], "_etas_from_omega_records");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega2]), [3, 4], "_etas_from_omega_records 2");
model_transformations::_remove_omega_records(model => $model, omegas => [$omega1]);
is_deeply ($model->problems->[0]->omegas, [$omega2], "_remove_omega_records 1");

$model->problems->[0]->omegas([$omega1, $omega2, $omega3]);
is (model_transformations::_number_of_etas(model => $model), 6, "_number_of_etas 3");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega2, $omega3]), [3, 4, 5, 6], "_etas_from_omega_records 3");
model_transformations::_remove_omega_records(model => $model, omegas => [$omega2]);
is_deeply ($model->problems->[0]->omegas, [$omega1, $omega3], "_remove_omega_records 2");

$model->problems->[0]->omegas([$omega1, $omega2, $omega3, $omega4]);
is (model_transformations::_number_of_etas(model => $model), 9, "_number_of_etas 4");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega4]), [7, 8, 9], "_etas_from_omega_records 4");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega2, $omega4]), [3, 4, 7, 8, 9], "_etas_from_omega_records 4");
model_transformations::_remove_omega_records(model => $model, omegas => [$omega1, $omega3]);
is_deeply ($model->problems->[0]->omegas, [$omega2, $omega4], "_remove_omega_records 3");

$model->set_code(record => 'pk', code => [ 'ETA_CL = ETA(1)' ]);
model_transformations::_remove_etas(model => $model, etas => [ 1 ]);
is_deeply ($model->get_code(record => 'pk'), ['ETA_CL = 0'], "_remove_etas 1");

$model->set_code(record => 'pk', code => [ 'ETA_COMB = ETA(1) * ETA(2)' ]);
model_transformations::_remove_etas(model => $model, etas => [ 1 ]);
is_deeply ($model->get_code(record => 'pk'), ['ETA_COMB = 0 * ETA(1)'], "_remove_etas 2");

$model->set_code(record => 'pk', code => [ 'ETA_COMB = ETA(1) * ETA(2) * ETA(3) * ETA(4)' ]);
model_transformations::_remove_etas(model => $model, etas => [ 1, 3 ]);
is_deeply ($model->get_code(record => 'pk'), ['ETA_COMB = 0 * ETA(1) * 0 * ETA(2)'], "_remove_etas 3");

$model->problems->[0]->omegas([$omega1, $omega2, $omega3, $omega4]);
$model->set_code(record => 'pk', code => [ 'ETA_COMB = ETA(1) * ETA(2) * ETA(3) * ETA(4)' ]);
model_transformations::_remove_omegas(model => $model, omegas => [$omega1]);
is_deeply ($model->get_code(record => 'pk'), ['ETA_COMB = 0 * 0 * ETA(1) * ETA(2)'], "_remove_omegas 1 check etas");
is_deeply ($model->problems->[0]->omegas, [$omega2, $omega3, $omega4], "_remove_omegas 1 check records");

$model->problems->[0]->omegas([$omega1, $omega2, $omega3, $omega4]);
$model->set_code(record => 'pk', code => [ 'ETA_COMB = ETA(1) * ETA(2) * ETA(5) * ETA(8)' ]);
model_transformations::_remove_omegas(model => $model, omegas => [$omega2, $omega4]);
is_deeply ($model->get_code(record => 'pk'), ['ETA_COMB = ETA(1) * ETA(2) * ETA(3) * 0'], "_remove_omegas 2 check etas");
is_deeply ($model->problems->[0]->omegas, [$omega1, $omega3], "_remove_omegas 2 check records");


$model->problems->[0]->omegas([$omega1, $omega2, $omega3, $omega4]);
model_transformations::_fix_omegas(model => $model, omegas => [$omega2]);
is ($model->problems->[0]->omegas->[0]->fix, 0, "_fix_omegas 1 other");
is ($model->problems->[0]->omegas->[1]->fix, 1, "_fix_omegas 1 fixed");


# omega_block
my $omega_ob = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(2) 0.1 0.2']);
$model->problems->[0]->omegas([ $omega_ob ]);
my $omega_block = model_transformations::omega_block(model => $model, start_eta => 1, end_eta => 2);
is ($omega_block->size, 2, "omega_block size");
is ($omega_block->type, "BLOCK", "omega_block size");
is (scalar(@{$omega_block->options}), 3, "omega_block options size");

my $omega_ob2 = model::problem::omega->new(record_arr => ['$OMEGA 0.1']);
my $omega_ob3 = model::problem::omega->new(record_arr => ['$OMEGA 0.2'], n_previous_rows => 1);
$model->problems->[0]->omegas([ $omega_ob2, $omega_ob3 ]);
$omega_block = model_transformations::omega_block(model => $model, start_eta => 1, end_eta => 2);
is ($omega_block->size, 2, "omega_block size");
is ($omega_block->type, "BLOCK", "omega_block size");
is (scalar(@{$omega_block->options}), 3, "omega_block options size");

# full_omega_block
my $was_full = model_transformations::full_omega_block(model => $model);
ok (!$was_full, "full_omega_block was_full 1");
$omega_block = $model->problems->[0]->omegas->[0];
is (scalar(@{$model->problems->[0]->omegas}), 1, "full_omega_block nomega blocks");
is ($omega_block->size, 2, "full_omega_block size");
is ($omega_block->type, "BLOCK", "full_omega_block size");
is (scalar(@{$omega_block->options}), 3, "full_omega_block options size");

$omega_ob = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(2) 0.1 0.2']);
$model->problems->[0]->omegas([ $omega_ob ]);
$was_full = model_transformations::full_omega_block(model => $model);
ok (!$was_full, "full_omega_block was_full 2");
$omega_block = $model->problems->[0]->omegas->[0];
is (scalar(@{$model->problems->[0]->omegas}), 1, "full_omega_block nomega blocks 2");
is ($omega_block->size, 2, "full_omega_block size 2");
is ($omega_block->type, "BLOCK", "full_omega_block size 2");
is (scalar(@{$omega_block->options}), 3, "full_omega_block options size 2");

$omega_ob = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2) 0.1 0.02 0.1']);
$omega_ob2 = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(1) 0.2'], n_previous_rows => 2);
$model->problems->[0]->omegas([ $omega_ob, $omega_ob2 ]);
$was_full = model_transformations::full_omega_block(model => $model);
ok (!$was_full, "full_omega_block was_full 3");
$omega_block = $model->problems->[0]->omegas->[0];
is (scalar(@{$model->problems->[0]->omegas}), 1, "full_omega_block nomega blocks 3");
is ($omega_block->size, 3, "full_omega_block size 3");
is ($omega_block->type, "BLOCK", "full_omega_block size 3");
is (scalar(@{$omega_block->options}), 6, "full_omega_block options size 3");

$omega_ob = model::problem::omega->new(record_arr => ['$OMEGA 0.1 FIX']);
$omega_ob2 = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(1) 0.2'], n_previous_rows => 1);
$model->problems->[0]->omegas([ $omega_ob, $omega_ob2 ]);
$was_full = model_transformations::full_omega_block(model => $model);
ok ($was_full, "full_omega_block was_full 4");
$omega_block = $model->problems->[0]->omegas->[0];
is (scalar(@{$model->problems->[0]->omegas}), 2, "full_omega_block nomega blocks 4");

$omega_ob = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2) 0.1 0.01 0.1']);
$omega_ob2 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2) SAME'], n_previous_rows => 2);
$model->problems->[0]->omegas([ $omega_ob, $omega_ob2 ]);
$was_full = model_transformations::full_omega_block(model => $model);
ok ($was_full, "full_omega_block was_full 5");
$omega_block = $model->problems->[0]->omegas->[0];
is (scalar(@{$model->problems->[0]->omegas}), 2, "full_omega_block nomega blocks 5");

$omega_ob = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(2) 0.1 0.01']);
$omega_ob2 = model::problem::omega->new(record_arr => ['$OMEGA 0.1 FIX'], n_previous_rows => 2);
$model->problems->[0]->omegas([ $omega_ob, $omega_ob2 ]);
$was_full = model_transformations::full_omega_block(model => $model);
ok (!$was_full, "full_omega_block was_full 6");
$omega_block = $model->problems->[0]->omegas->[0];
is (scalar(@{$model->problems->[0]->omegas}), 2, "full_omega_block nomega blocks 6");
is ($omega_block->size, 2, "full_omega_block size 6");
is ($omega_block->type, "BLOCK", "full_omega_block size 6");
is (scalar(@{$omega_block->options}), 3, "full_omega_block options size 6");

$omega_ob = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(2) 0.1 0.01']);
$omega_ob2 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2) 0.1 0.1 0.1 '], n_previous_rows => 2);
$omega_ob3 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2) SAME'], n_previous_rows => 4);
$model->problems->[0]->omegas([ $omega_ob, $omega_ob2, $omega_ob3 ]);
$was_full = model_transformations::full_omega_block(model => $model);
ok (!$was_full, "full_omega_block was_full 7");
$omega_block = $model->problems->[0]->omegas->[0];
is (scalar(@{$model->problems->[0]->omegas}), 3, "full_omega_block nomega blocks 7");
is ($omega_block->size, 2, "full_omega_block size 7");
is ($omega_block->type, "BLOCK", "full_omega_block size 7");
is (scalar(@{$omega_block->options}), 3, "full_omega_block options size 7");


done_testing();
