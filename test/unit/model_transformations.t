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

$model->problems->[0]->omegas([$omega1, $omega2]);
is (model_transformations::_number_of_etas(model => $model), 4, "_number_of_etas 2");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega1]), [1, 2], "_etas_from_omega_records");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega2]), [3, 4], "_etas_from_omega_records 2");
model_transformations::_remove_omega_records(model => $model, omegas => model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega1]));
is_deeply ($model->problems->[0]->omegas, [$omega2], "_remove_omega_records 1");

$omega1 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.5 0.1 0.5']);
$omega2 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.25 0.1 0.25'], n_previous_rows => 2);
my $omega3 = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(2) 0.1 0.2'], n_previous_rows => 4);

$model->problems->[0]->omegas([$omega1, $omega2, $omega3]);
is (model_transformations::_number_of_etas(model => $model), 6, "_number_of_etas 3");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega2, $omega3]), [3, 4, 5, 6], "_etas_from_omega_records 3");
model_transformations::_remove_omega_records(model => $model, omegas => model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega2]));
is_deeply ($model->problems->[0]->omegas, [$omega1, $omega3], "_remove_omega_records 2");

$omega1 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.5 0.1 0.5']);
$omega2 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.25 0.1 0.25'], n_previous_rows => 2);
$omega3 = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(2) 0.1 0.2'], n_previous_rows => 4);
my $omega4 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(3) SAME'], n_previous_rows => 6);

$model->problems->[0]->omegas([$omega1, $omega2, $omega3, $omega4]);
is (model_transformations::_number_of_etas(model => $model), 9, "_number_of_etas 4");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega4]), [7, 8, 9], "_etas_from_omega_records 4");
is_deeply (model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega2, $omega4]), [3, 4, 7, 8, 9], "_etas_from_omega_records 4");
model_transformations::_remove_omega_records(model => $model, omegas => model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega1, $omega3]));
is_deeply ($model->problems->[0]->omegas, [$omega2, $omega4], "_remove_omega_records 3");


$omega1 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.5 0.1 0.5']);
$omega2 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.25 0.1 0.25'], n_previous_rows => 2);
$omega3 = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(2) 0.1 0.2'], n_previous_rows => 4);
$omega4 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(3) SAME'], n_previous_rows => 6);
$model->problems->[0]->omegas([$omega2, $omega4]);

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
model_transformations::_remove_omegas(model => $model, omegas => model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega1]));
is_deeply ($model->get_code(record => 'pk'), ['ETA_COMB = 0 * 0 * ETA(1) * ETA(2)'], "_remove_omegas 1 check etas");
is_deeply ($model->problems->[0]->omegas, [$omega2, $omega3, $omega4], "_remove_omegas 1 check records");

$omega1 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.5 0.1 0.5']);
$omega2 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.25 0.1 0.25'], n_previous_rows => 2);
$omega3 = model::problem::omega->new(record_arr => ['$OMEGA DIAGONAL(2) 0.1 0.2'], n_previous_rows => 4);
$omega4 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(3) SAME'], n_previous_rows => 6);
$model->problems->[0]->omegas([$omega1, $omega2, $omega3, $omega4]);
$model->set_code(record => 'pk', code => [ 'ETA_COMB = ETA(1) * ETA(2) * ETA(5) * ETA(8)' ]);
model_transformations::_remove_omegas(model => $model, omegas => model_transformations::_etas_from_omega_records(model => $model, omegas => [$omega2, $omega4]));
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



#_remove_omega_records
$model = model->new(filename => "$modeldir/tbs1.mod", ignore_missing_data => 1);
model_transformations::_remove_omega_records(model => $model, omegas => [ 1 ]);
is (scalar(@{$model->problems->[0]->omegas}), 6, "_remove_omega_record nrecords");
is ($model->problems->[0]->omegas->[0]->size, 1, "_remove_omega_record size");
is (scalar(@{$model->problems->[0]->omegas->[0]->options}), 1, "_remove_omega_record first record");
is ($model->problems->[0]->omegas->[0]->options->[0]->coordinate_string, "OMEGA(1,1)", "_remove_omega_record first coord string");
is ($model->problems->[0]->omegas->[1]->options->[0]->coordinate_string, "OMEGA(2,2)", "_remove_omega_record first coord string 2");

$model = model->new(filename => "$modeldir/tbs1.mod", ignore_missing_data => 1);
model_transformations::_remove_omega_records(model => $model, omegas => [ 1, 2 ]);
is (scalar(@{$model->problems->[0]->omegas}), 5, "_remove_omega_record nrecords");
is ($model->problems->[0]->omegas->[0]->size, 1, "_remove_omega_record size");
is (scalar(@{$model->problems->[0]->omegas->[0]->options}), 1, "_remove_omega_record first record");
is ($model->problems->[0]->omegas->[0]->options->[0]->coordinate_string, "OMEGA(1,1)", "_remove_omega_record first coord string");
is ($model->problems->[0]->omegas->[1]->options->[0]->coordinate_string, "OMEGA(2,2)", "_remove_omega_record first coord string 2");

$omega_ob = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(3) 0.1 0.02, 0.3, 0.04, 0.05, 0.6']);
$model->problems->[0]->omegas([ $omega_ob ]);
model_transformations::_remove_omega_records(model => $model, omegas => [ 2 ]);
is (scalar(@{$model->problems->[0]->omegas}), 1, "_remove_omega_record nrecords");
is ($model->problems->[0]->omegas->[0]->size, 2, "_remove_omega_record size");
is (scalar(@{$model->problems->[0]->omegas->[0]->options}), 3, "_remove_omega_record first record");
is ($model->problems->[0]->omegas->[0]->options->[0]->coordinate_string, "OMEGA(1,1)", "_remove_omega_record first coord string");
is ($model->problems->[0]->omegas->[0]->options->[1]->coordinate_string, "OMEGA(2,1)", "_remove_omega_record first coord string");
is ($model->problems->[0]->omegas->[0]->options->[2]->coordinate_string, "OMEGA(2,2)", "_remove_omega_record first coord string");
is ($model->problems->[0]->omegas->[0]->options->[0]->init, 0.1, "_remove_omega_record init value 1");
is ($model->problems->[0]->omegas->[0]->options->[1]->init, 0.04, "_remove_omega_record init value 1");
is ($model->problems->[0]->omegas->[0]->options->[2]->init, 0.6, "_remove_omega_record init value 1");


#find_zero_fix_omegas
$omega1 = model::problem::omega->new(record_arr => ['$OMEGA', '0.1 0 FIX']);
$omega2 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2)', '0.25 0.1 0.25'], n_previous_rows => 2);
$omega3 = model::problem::omega->new(record_arr => ['$OMEGA BLOCK(2) 0 0 0 FIX'], n_previous_rows => 2);
$model->problems->[0]->omegas([$omega1, $omega2]);
is_deeply (model_transformations::find_zero_fix_omegas(model => $model), [ 2 ], "find_zero_fix_omegas 1");
$model->problems->[0]->omegas([$omega1, $omega3]);
is_deeply (model_transformations::find_zero_fix_omegas(model => $model), [ 2, 3, 4 ], "find_zero_fix_omegas 2");

#remaining_omegas
is_deeply (model_transformations::remaining_omegas(model => $model, omegas => [ 1, 2 ]), [ 3, 4 ], "remaining_omegas 1");
is_deeply (model_transformations::remaining_omegas(model => $model, omegas => [ 3 ]), [ 1, 2, 4 ], "remaining_omegas 2");
is_deeply (model_transformations::remaining_omegas(model => $model, omegas => [ 2,3,4 ]), [ 1 ], "remaining_omegas 3");

# omega_options_from_etas
$model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
my $options = model_transformations::omega_options_from_etas(model => $model, etas => [ 2 ]);
is (scalar(@$options), 1, "omega_options_from_etas size");
is ($options->[0]->coordinate_string, "OMEGA(2,2)", "omega_options_from_etas 1");

# list_etas_used_in_code
$model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
is_deeply (model_transformations::list_etas_used_in_code(model => $model), { '1' => 1, '2' => 1 }, "list_etas_used_in_code 1");

# get_omega_element
my $record = model::problem::omega->new(record_arr => ['BLOCK(1) 0.02']);
my $option = model_transformations::get_omega_element(record => $record, row => 1, col => 1);
is_deeply ($option, $record->options->[0], "get_omega_elements block 1");

$record = model::problem::omega->new(record_arr => ['BLOCK(2) 0.02','0 0.01 FIX']);
$option = model_transformations::get_omega_element(record => $record, row => 1, col => 1);
is_deeply ($option, $record->options->[0], "get_omega_elements block 2 1");
$option = model_transformations::get_omega_element(record => $record, row => 2, col => 1);
is_deeply ($option, $record->options->[1], "get_omega_elements block 2 2");
$option = model_transformations::get_omega_element(record => $record, row => 1, col => 2);
is_deeply ($option, $record->options->[1], "get_omega_elements block 2 2 reversed");
$option = model_transformations::get_omega_element(record => $record, row => 2, col => 2);
is_deeply ($option, $record->options->[2], "get_omega_elements block 2 3");

$record = model::problem::omega->new(record_arr => ['BLOCK(3) 0.02','0 0.01 2 5 1 0.023']);
$option = model_transformations::get_omega_element(record => $record, row => 1, col => 1);
is_deeply ($option, $record->options->[0], "get_omega_elements block 3 1");
$option = model_transformations::get_omega_element(record => $record, row => 3, col => 2);
is_deeply ($option, $record->options->[4], "get_omega_elements block 3 2");
$option = model_transformations::get_omega_element(record => $record, row => 3, col => 3);
is_deeply ($option, $record->options->[5], "get_omega_elements block 3 3");

# update_coordstring
$record = model::problem::omega->new(record_arr => ['BLOCK(2) 0.02','0 0.01 FIX']);
$option = model_transformations::get_omega_element(record => $record, row => 2, col => 1);
model_transformations::update_coordstring(option => $option, row => 4, col => 28);
is ($option->coordinate_string, "OMEGA(4,28)", "update_coordstring");

# reorder_omega_record
$record = model::problem::omega->new(record_arr => ['BLOCK(2) 0.02','0 0.01 FIX']);
my %order = ( 1 => 2, 2 => 1 );
model_transformations::reorder_omega_record(record => $record, order => \%order);
is ($record->size, 2, "reorder_omega_record full block 2 size");
is ($record->n_previous_rows, 0, "reorder_omega_record full block 2 n_prev");
is ($record->options->[0]->init, 0.01, "reorder_omega_record full block 2 init 0");
is ($record->options->[0]->coordinate_string, 'OMEGA(1,1)', "reorder_omega_record full block 2 coord 0");
is ($record->options->[1]->init, 0, "reorder_omega_record full block 2 init 1");
is ($record->options->[1]->coordinate_string, 'OMEGA(2,1)', "reorder_omega_record full block 2 coord 1");
is ($record->options->[2]->init, 0.02, "reorder_omega_record full block 2 init 2");
is ($record->options->[2]->coordinate_string, 'OMEGA(2,2)', "reorder_omega_record full block 2 coord 2");

$record = model::problem::omega->new(record_arr => ['BLOCK(3) 0.02','0 0.01 4 9 3']);
%order = ( 1 => 22, 3 => 21 );
model_transformations::reorder_omega_record(record => $record, order => \%order);
is ($record->size, 2, "reorder_omega_record partial block 3 size");
is ($record->n_previous_rows, 20, "reorder_omega_record partial block 3 n_prev");
is ($record->options->[0]->init, 3, "reorder_omega_record partial block 3 init 0");
is ($record->options->[0]->coordinate_string, 'OMEGA(21,21)', "reorder_omega_record partial block 3 coord 0");
is ($record->options->[1]->init, 4, "reorder_omega_record partial block 3 init 1");
is ($record->options->[1]->coordinate_string, 'OMEGA(22,21)', "reorder_omega_record partial block 3 coord 1");
is ($record->options->[2]->init, 0.02, "reorder_omega_record partial block 3 init 2");
is ($record->options->[2]->coordinate_string, 'OMEGA(22,22)', "reorder_omega_record partial block 3 coord 2");

$record = model::problem::omega->new(record_arr => ['BLOCK(5) 1 ;CL', '3 4', '8 9 10', '14 15 16 17', '23 ;CL-V', '24 25 26', '27 ;V']);
%order = ( 1 => 3, 3 => 2, 5 => 4 );
model_transformations::reorder_omega_record(record => $record, order => \%order);
is ($record->size, 3, "reorder_omega_record block 5 size");
is ($record->n_previous_rows, 1, "reorder_omega_record block 5 n_prev");
is ($record->options->[0]->init, 10, "reorder_omega_record block 5 init 0");
is ($record->options->[0]->coordinate_string, 'OMEGA(2,2)', "reorder_omega_record block 5 coords 0");
is ($record->options->[1]->init, 8, "reorder_omega_record block 5 init 1");
is ($record->options->[1]->coordinate_string, 'OMEGA(3,2)', "reorder_omega_record block 5 coords 1");
is ($record->options->[2]->init, 1, "reorder_omega_record block 5 init 2");
is ($record->options->[2]->coordinate_string, 'OMEGA(3,3)', "reorder_omega_record block 5 coords 2");
is ($record->options->[2]->label, 'CL', "reorder_omega_record block 5 label 2");
is ($record->options->[3]->init, 25, "reorder_omega_record block 5 init 3");
is ($record->options->[3]->coordinate_string, 'OMEGA(4,2)', "reorder_omega_record block 5 coords 3");
is ($record->options->[4]->init, 23, "reorder_omega_record block 5 init 4");
is ($record->options->[4]->coordinate_string, 'OMEGA(4,3)', "reorder_omega_record block 5 coords 4");
is ($record->options->[4]->label, 'CL-V', "reorder_omega_record block 5 label 4");
is ($record->options->[5]->init, 27, "reorder_omega_record block 5 init 5");
is ($record->options->[5]->coordinate_string, 'OMEGA(4,4)', "reorder_omega_record block 5 coords 5");
is ($record->options->[5]->label, 'V', "reorder_omega_record block 5 label 5");

$record = model::problem::omega->new(record_arr => ['BLOCK(5) 1 ;CL', '3 4', '8 9 10', '14 15 16 17', '23 ;CL-V', '24 25 26', '27 ;V']);
%order = ( 5 => 1 );
model_transformations::reorder_omega_record(record => $record, order => \%order);
is ($record->size, 1, "reorder_omega_record block 5to1 size");
is ($record->n_previous_rows, 0, "reorder_omega_record block 5to1 n_prev");
is ($record->options->[0]->init, 27, "reorder_omega_record block 5to1 init 0");
is ($record->options->[0]->coordinate_string, 'OMEGA(1,1)', "reorder_omega_record block 5to1 coords 0");


$record = model::problem::omega->new(record_arr => ['0.02 0 0.01 4 9 3']);
%order = ( 5 => 14, 6 => 13, 2 => 15 );
model_transformations::reorder_omega_record(record => $record, order => \%order);
is ($record->size, 3, "reorder_omega_record diag size");
is ($record->n_previous_rows, 12, "reorder_omega_record diag n_prev");
is ($record->options->[0]->init, 3, "reorder_omega_record diag init 0");
is ($record->options->[0]->coordinate_string, 'OMEGA(13,13)', "reorder_omega_record diag coord 0");
is ($record->options->[1]->init, 9, "reorder_omega_record diag init 1");
is ($record->options->[1]->coordinate_string, 'OMEGA(14,14)', "reorder_omega_record diag coord 1");
is ($record->options->[2]->init, 0, "reorder_omega_record diag init 2");
is ($record->options->[2]->coordinate_string, 'OMEGA(15,15)', "reorder_omega_record diag coord 2");

$record = model::problem::omega->new(record_arr => ['BLOCK(10) SAME']);
%order = ( 5 => 14, 6 => 13, 2 => 15 );
model_transformations::reorder_omega_record(record => $record, order => \%order);
is ($record->size, 3, "reorder_omega_record block same size");
is ($record->n_previous_rows, 12, "reorder_omega_record block same n_prev");

# find_omega_record_for_eta
$model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
$record = model_transformations::find_omega_record_for_eta(model => $model, eta => 2);
is_deeply($record, $model->problems->[0]->omegas->[0], "find_omega_record_for_eta");
$record = model_transformations::find_omega_record_for_eta(model => $model, eta => 1);
is_deeply($record, $model->problems->[0]->omegas->[0], "find_omega_record_for_eta");

$model = model->new(filename => "$modeldir/mixture.mod", ignore_missing_data => 1);
$record = model_transformations::find_omega_record_for_eta(model => $model, eta => 2);
is_deeply($record, $model->problems->[0]->omegas->[0], "find_omega_record_for_eta 2");
$record = model_transformations::find_omega_record_for_eta(model => $model, eta => 4);
is_deeply($record, $model->problems->[0]->omegas->[1], "find_omega_record_for_eta 2");

$model = model->new(filename => "$modeldir/glstags.mod", ignore_missing_data => 1);
$record = model_transformations::find_omega_record_for_eta(model => $model, eta => 2);
is_deeply($record, $model->problems->[0]->omegas->[1], "find_omega_record_for_eta 3");
$record = model_transformations::find_omega_record_for_eta(model => $model, eta => 3);
is_deeply($record, $model->problems->[0]->omegas->[2], "find_omega_record_for_eta 3");

# hash_slice
my $hash = { 1 => 2, 3 => 5, 6 => 7 };
my $key_array = [ 1, 6 ];
my $result_hash = model_transformations::hash_slice($hash, $key_array);
is_deeply($result_hash, { 1 => 2, 6 => 7 }, "hash_slice 1");

# reorder_etas
$model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
my $model_orig = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
%order = (1 => 2, 2 => 1);
model_transformations::reorder_etas(model => $model, order => \%order);
is_deeply($model->problems->[0]->omegas->[0]->options->[0]->init, $model_orig->problems->[0]->omegas->[0]->options->[1]->init, "reorder_etas pheno 1");
is_deeply($model->problems->[0]->omegas->[0]->options->[1]->init, $model_orig->problems->[0]->omegas->[0]->options->[0]->init, "reorder_etas pheno 2");
is_deeply($model->problems->[0]->omegas->[0]->options->[0]->label, $model_orig->problems->[0]->omegas->[0]->options->[1]->label, "reorder_etas pheno 3");
is_deeply($model->problems->[0]->omegas->[0]->options->[1]->label, $model_orig->problems->[0]->omegas->[0]->options->[0]->label, "reorder_etas pheno 4");
is_deeply($model->problems->[0]->omegas->[0]->options->[0]->coordinate_string, $model_orig->problems->[0]->omegas->[0]->options->[0]->coordinate_string, "reorder_etas pheno 5");
is_deeply($model->problems->[0]->omegas->[0]->options->[1]->coordinate_string, $model_orig->problems->[0]->omegas->[0]->options->[1]->coordinate_string, "reorder_etas pheno 6");

$record = model::problem::omega->new(record_arr => ['BLOCK(3) 2 4 6 8 10 12']);
my $record2 = model::problem::omega->new(record_arr => ['19 20'], n_previous_rows => 3);
$model->problems->[0]->omegas([$record, $record2]);
%order = (1 => 4, 2 => 5, 3 => 3, 4 => 2, 5 => 1);      # Note that this order does not require splitting
model_transformations::reorder_etas(model => $model, order => \%order);
is (scalar(@{$model->problems->[0]->omegas}), 2, "reorder_etas example 1 no records");
is ($model->problems->[0]->omegas->[0]->is_block(), 0, "reorder_etas example 1 rec 0 type");
is ($model->problems->[0]->omegas->[0]->get_size(), 2, "reorder_etas example 1 rec 0 size");
is ($model->problems->[0]->omegas->[0]->n_previous_rows, 0, "reorder_etas example 0 rec 0 nprev");
is ($model->problems->[0]->omegas->[1]->is_block(), 1, "reorder_etas example 1 rec 1 type");
is ($model->problems->[0]->omegas->[1]->get_size(), 3, "reorder_etas example 1 rec 1 size");
is ($model->problems->[0]->omegas->[1]->n_previous_rows, 2, "reorder_etas example 1 rec 0 nprev");
is ($model->problems->[0]->omegas->[0]->options->[0]->init, 20, "reorder_etas example 1 rec 0 opt 0 init");
is ($model->problems->[0]->omegas->[0]->options->[0]->coordinate_string, 'OMEGA(1,1)', "reorder_etas example 1 rec 0 opt 0 coord");
is ($model->problems->[0]->omegas->[0]->options->[1]->init, 19, "reorder_etas example 1 rec 0 opt 1 init");
is ($model->problems->[0]->omegas->[0]->options->[1]->coordinate_string, 'OMEGA(2,2)', "reorder_etas example 1 rec 0 opt 1 coord");
is ($model->problems->[0]->omegas->[1]->options->[0]->init, 12, "reorder_etas example 1 rec 1 opt 0 init");
is ($model->problems->[0]->omegas->[1]->options->[0]->coordinate_string, 'OMEGA(3,3)', "reorder_etas example 1 rec 1 opt 0 coord");
is ($model->problems->[0]->omegas->[1]->options->[1]->init, 8, "reorder_etas example 1 rec 1 opt 1 init");
is ($model->problems->[0]->omegas->[1]->options->[1]->coordinate_string, 'OMEGA(4,3)', "reorder_etas example 1 rec 1 opt 1 coord");
is ($model->problems->[0]->omegas->[1]->options->[2]->init, 2, "reorder_etas example 1 rec 1 opt 2 init");
is ($model->problems->[0]->omegas->[1]->options->[2]->coordinate_string, 'OMEGA(4,4)', "reorder_etas example 1 rec 1 opt 2 coord");
is ($model->problems->[0]->omegas->[1]->options->[3]->init, 10, "reorder_etas example 1 rec 1 opt 3 init");
is ($model->problems->[0]->omegas->[1]->options->[3]->coordinate_string, 'OMEGA(5,3)', "reorder_etas example 1 rec 1 opt 3 coord");
is ($model->problems->[0]->omegas->[1]->options->[4]->init, 4, "reorder_etas example 1 rec 1 opt 4 init");
is ($model->problems->[0]->omegas->[1]->options->[4]->coordinate_string, 'OMEGA(5,4)', "reorder_etas example 1 rec 1 opt 4 coord");
is ($model->problems->[0]->omegas->[1]->options->[5]->init, 6, "reorder_etas example 1 rec 1 opt 5 init");
is ($model->problems->[0]->omegas->[1]->options->[5]->coordinate_string, 'OMEGA(5,5)', "reorder_etas example 1 rec 1 opt 5 coord");

$record = model::problem::omega->new(record_arr => ['BLOCK(3) 2 4 6 8 10 12']);
$record2 = model::problem::omega->new(record_arr => ['19 20'], n_previous_rows => 3);
$model->problems->[0]->omegas([$record, $record2]);
%order = (4 => 1, 1 => 2, 2 => 3, 5 => 4, 3 => 5);      # Note that this order does require splitting
model_transformations::reorder_etas(model => $model, order => \%order);
is (scalar(@{$model->problems->[0]->omegas}), 4, "reorder_etas example 2 no records");
is ($model->problems->[0]->omegas->[0]->is_block(), 0, "reorder_etas example 2 rec 0 type");
is ($model->problems->[0]->omegas->[0]->get_size(), 1, "reorder_etas example 2 rec 0 size");
is ($model->problems->[0]->omegas->[0]->n_previous_rows, 0, "reorder_etas example 2 rec 0 nprev");
is ($model->problems->[0]->omegas->[1]->is_block(), 1, "reorder_etas example 2 rec 1 type");
is ($model->problems->[0]->omegas->[1]->get_size(), 2, "reorder_etas example 2 rec 1 size");
is ($model->problems->[0]->omegas->[1]->n_previous_rows, 1, "reorder_etas example 2 rec 1 nprev");
is ($model->problems->[0]->omegas->[2]->is_block(), 0, "reorder_etas example 2 rec 2 type");
is ($model->problems->[0]->omegas->[2]->get_size(), 1, "reorder_etas example 2 rec 2 size");
is ($model->problems->[0]->omegas->[2]->n_previous_rows, 3, "reorder_etas example 2 rec 2 nprev");
is ($model->problems->[0]->omegas->[3]->is_block(), 1, "reorder_etas example 2 rec 3 type");
is ($model->problems->[0]->omegas->[3]->get_size(), 1, "reorder_etas example 2 rec 3 size");
is ($model->problems->[0]->omegas->[3]->n_previous_rows, 4, "reorder_etas example 2 rec 3 nprev");
is ($model->problems->[0]->omegas->[0]->options->[0]->init, 19, "reorder_etas example 2 rec 0 opt 0 init");
is ($model->problems->[0]->omegas->[0]->options->[0]->coordinate_string, 'OMEGA(1,1)', "reorder_etas example 2 rec 0 opt 0 coord");
is ($model->problems->[0]->omegas->[1]->options->[0]->init, 2, "reorder_etas example 2 rec 0 opt 0 init");
is ($model->problems->[0]->omegas->[1]->options->[0]->coordinate_string, 'OMEGA(2,2)', "reorder_etas example 2 rec 1 opt 0 coord");
is ($model->problems->[0]->omegas->[1]->options->[1]->init, 4, "reorder_etas example 2 rec 0 opt 1 init");
is ($model->problems->[0]->omegas->[1]->options->[1]->coordinate_string, 'OMEGA(3,2)', "reorder_etas example 2 rec 1 opt 1 coord");
is ($model->problems->[0]->omegas->[1]->options->[2]->init, 6, "reorder_etas example 2 rec 0 opt 2 init");
is ($model->problems->[0]->omegas->[1]->options->[2]->coordinate_string, 'OMEGA(3,3)', "reorder_etas example 2 rec 1 opt 2 coord");
is ($model->problems->[0]->omegas->[2]->options->[0]->init, 20, "reorder_etas example 2 rec 2 opt 0 init");
is ($model->problems->[0]->omegas->[2]->options->[0]->coordinate_string, 'OMEGA(4,4)', "reorder_etas example 2 rec 2 opt 0 coord");
is ($model->problems->[0]->omegas->[3]->options->[0]->init, 12, "reorder_etas example 2 rec 3 opt 0 init");
is ($model->problems->[0]->omegas->[3]->options->[0]->coordinate_string, 'OMEGA(5,5)', "reorder_etas example 2 rec 3 opt 0 coord");

$record = model::problem::omega->new(record_arr => ['DIAGONAL(2) 5 7']);
$record2 = model::problem::omega->new(record_arr => ['BLOCK(2) 11 13 15'], n_previous_rows => 2);
my $record3 = model::problem::omega->new(record_arr => ['BLOCK(2) SAME'], n_previous_rows => 4);
$model->problems->[0]->omegas([$record, $record2, $record3]);
%order = (3 => 1, 5 => 2, 1 => 3, 2 => 4, 4 => 5, 6 => 6);      # Note that this order does require splitting of SAME block
model_transformations::reorder_etas(model => $model, order => \%order);
is (scalar(@{$model->problems->[0]->omegas}), 5, "reorder_etas example 3 no records");
is ($model->problems->[0]->omegas->[0]->is_block(), 1, "reorder_etas example 3 rec 0 type");
is ($model->problems->[0]->omegas->[0]->get_size(), 1, "reorder_etas example 3 rec 0 size");
is ($model->problems->[0]->omegas->[0]->n_previous_rows, 0, "reorder_etas example 3 rec 0 nprev");
is ($model->problems->[0]->omegas->[0]->same, 0, "reorder_etas example 3 rec 0 same");
is ($model->problems->[0]->omegas->[1]->is_block(), 1, "reorder_etas example 3 rec 1 type");
is ($model->problems->[0]->omegas->[1]->get_size(), 1, "reorder_etas example 3 rec 1 size");
is ($model->problems->[0]->omegas->[1]->n_previous_rows, 1, "reorder_etas example 3 rec 1 nprev");
is ($model->problems->[0]->omegas->[1]->same, 1, "reorder_etas example 3 rec 1 same");
is ($model->problems->[0]->omegas->[2]->is_block(), 0, "reorder_etas example 3 rec 2 type");
is ($model->problems->[0]->omegas->[2]->get_size(), 2, "reorder_etas example 3 rec 2 size");
is ($model->problems->[0]->omegas->[2]->n_previous_rows, 2, "reorder_etas example 3 rec 2 nprev");
is ($model->problems->[0]->omegas->[2]->same, 0, "reorder_etas example 3 rec 2 same");
is ($model->problems->[0]->omegas->[3]->is_block(), 1, "reorder_etas example 3 rec 3 type");
is ($model->problems->[0]->omegas->[3]->get_size(), 1, "reorder_etas example 3 rec 3 size");
is ($model->problems->[0]->omegas->[3]->n_previous_rows, 4, "reorder_etas example 3 rec 3 nprev");
is ($model->problems->[0]->omegas->[3]->same, 0, "reorder_etas example 3 rec 3 same");
is ($model->problems->[0]->omegas->[4]->is_block(), 1, "reorder_etas example 3 rec 4 type");
is ($model->problems->[0]->omegas->[4]->get_size(), 1, "reorder_etas example 3 rec 4 size");
is ($model->problems->[0]->omegas->[4]->n_previous_rows, 5, "reorder_etas example 3 rec 4 nprev");
is ($model->problems->[0]->omegas->[4]->same, 1, "reorder_etas example 3 rec 4 same");
is ($model->problems->[0]->omegas->[0]->options->[0]->init, 11, "reorder_etas example 3 rec 0 opt 0 init");
is ($model->problems->[0]->omegas->[0]->options->[0]->coordinate_string, 'OMEGA(1,1)', "reorder_etas example 3 rec 0 opt 0 coord");
is ($model->problems->[0]->omegas->[2]->options->[0]->init, 5, "reorder_etas example 3 rec 2 opt 0 init");
is ($model->problems->[0]->omegas->[2]->options->[0]->coordinate_string, 'OMEGA(3,3)', "reorder_etas example 3 rec 2 opt 0 coord");
is ($model->problems->[0]->omegas->[2]->options->[1]->init, 7, "reorder_etas example 3 rec 2 opt 1 init");
is ($model->problems->[0]->omegas->[2]->options->[1]->coordinate_string, 'OMEGA(4,4)', "reorder_etas example 3 rec 2 opt 1 coord");
is ($model->problems->[0]->omegas->[3]->options->[0]->init, 15, "reorder_etas example 3 rec 3 opt 0 init");
is ($model->problems->[0]->omegas->[3]->options->[0]->coordinate_string, 'OMEGA(5,5)', "reorder_etas example 3 rec 3 opt 0 coord");


done_testing();
