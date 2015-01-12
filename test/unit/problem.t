#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use linear_algebra;
use model;
use model::problem;

my $modeldir = $includes::testfiledir;
my $model = model->new(filename => "$modeldir/pheno.mod");
my $problem = $model->problems->[0];


#
my $omega_mat = $problem->get_matrix(type => 'omega',
									 start_row => 1);
cmp_ok($omega_mat->[0]->[0],'==',0.4000, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[1],'==',0.2500, "get_matrix 1,1");


# Test get_filled_omega method
my $full_omega = $problem->get_filled_omega_matrix(start_eta => 1);

cmp_ok($full_omega->[0]->[0],'==',0.4000, "get_filled_omega_matrix 0,0");
cmp_ok($full_omega->[0]->[1],'==',0.0000, "get_filled_omega_matrix 0,1");
cmp_ok($full_omega->[1]->[0],'==',0.0001, "get_filled_omega_matrix 1,0");
cmp_ok($full_omega->[1]->[1],'==',0.2500, "get_filled_omega_matrix 1,1");

#
$model = model->new(filename => "$modeldir/mox1.mod");
$problem = $model->problems->[0];

cmp_ok(($problem->check_start_eta(start_eta => 1)),'==',1, "check start_eta 1");
cmp_ok(($problem->check_start_eta(start_eta => 3)),'==',2, "check start_eta 3");
cmp_ok(($problem->check_start_eta(start_eta => 4)),'==',3, "check start_eta 4");
cmp_ok(($problem->check_start_eta(start_eta => 5)),'==',4, "check start_eta 5");
cmp_ok(($problem->check_start_eta(start_eta => 6)),'==',5, "check start_eta 6");
cmp_ok(($problem->check_start_eta(start_eta => 7)),'==',6, "check start_eta 7");

#
$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 1);
cmp_ok($omega_mat->[0]->[0],'==',0.0750, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[0],'==',0.0467, "get_matrix 1,0");
cmp_ok($omega_mat->[1]->[1],'==',0.0564, "get_matrix 1,1");
cmp_ok($omega_mat->[2]->[2],'==',2.82, "get_matrix 2,2");
cmp_ok($omega_mat->[3]->[3],'==',0.0147, "get_matrix 3,3");
cmp_ok($omega_mat->[4]->[4],'==',0.0147, "get_matrix 4,4");
cmp_ok($omega_mat->[5]->[5],'==',0.506, "get_matrix 5,5");
cmp_ok($omega_mat->[6]->[6],'==',0.506, "get_matrix 6,6");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 1,
								  end_row => 5);
cmp_ok($omega_mat->[0]->[0],'==',0.0750, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[0],'==',0.0467, "get_matrix 1,0");
cmp_ok($omega_mat->[1]->[1],'==',0.0564, "get_matrix 1,1");
cmp_ok($omega_mat->[2]->[2],'==',2.82, "get_matrix 2,2");
cmp_ok($omega_mat->[3]->[3],'==',0.0147, "get_matrix 3,3");
cmp_ok($omega_mat->[4]->[4],'==',0.0147, "get_matrix 4,4");
cmp_ok(scalar(@{$omega_mat}),'==',5, "get_matrix size");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 3);
cmp_ok($omega_mat->[0]->[0],'==',2.82, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[1],'==',0.0147, "get_matrix 1,1");
cmp_ok($omega_mat->[2]->[2],'==',0.0147, "get_matrix 2,2");
cmp_ok($omega_mat->[3]->[3],'==',0.506, "get_matrix 3,3");
cmp_ok($omega_mat->[4]->[4],'==',0.506, "get_matrix 4,4");
cmp_ok(scalar(@{$omega_mat}),'==',5, "get_matrix size");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 3,
								  end_row => 6);
cmp_ok($omega_mat->[0]->[0],'==',2.82, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[1],'==',0.0147, "get_matrix 1,1");
cmp_ok($omega_mat->[2]->[2],'==',0.0147, "get_matrix 2,2");
cmp_ok($omega_mat->[3]->[3],'==',0.506, "get_matrix 3,3");
cmp_ok(scalar(@{$omega_mat}),'==',4, "get_matrix size");



$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 4,
								  end_row => 7);
cmp_ok($omega_mat->[0]->[0],'==',0.0147, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[1],'==',0.0147, "get_matrix 1,1");
cmp_ok($omega_mat->[2]->[2],'==',0.506, "get_matrix 2,2");
cmp_ok($omega_mat->[3]->[3],'==',0.506, "get_matrix 3,3");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 5,
								  end_row => 7);
cmp_ok($omega_mat->[0]->[0],'==',0.0147, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[1],'==',0.506, "get_matrix 1,1");
cmp_ok($omega_mat->[2]->[2],'==',0.506, "get_matrix 2,2");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 6);
cmp_ok($omega_mat->[0]->[0],'==',0.506, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[1],'==',0.506, "get_matrix 1,1");
$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 7);
cmp_ok($omega_mat->[0]->[0],'==',0.506, "get_matrix 0,0");

my $cov_mat = [[0.02,0.003,0.004],
			   [0.003,0.5,0.00000000000000002],
			   [0.004,0.00000000000000002,0.5]	];

$full_omega = $problem->get_filled_omega_matrix(start_eta => 5,
												covmatrix => $cov_mat);
cmp_ok($full_omega->[0]->[0],'==',0.0147, "get_filled_omega_matrix 0,0");
cmp_ok($full_omega->[1]->[0],'==',0.003, "get_filled_omega_matrix 1,0");
cmp_ok($full_omega->[1]->[1],'==',0.506, "get_filled_omega_matrix 1,1");
cmp_ok($full_omega->[2]->[0],'==',0.004, "get_filled_omega matrix 0,2");
cmp_ok($full_omega->[2]->[1],'==',0.0001, "get_filled_omega matrix 1,2");
cmp_ok($full_omega->[2]->[2],'==',0.506, "get_filled_omega matrix 2,2");

$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 1,
										 row_format => 1);
cmp_ok($omega_mat->[0]->[0],'==',0.0750, "get_record_matrix row 0,0");
cmp_ok($omega_mat->[1]->[0],'==',0.0467, "get_record_matrix row 1,0");
cmp_ok($omega_mat->[1]->[1],'==',0.0564, "get_record_matrix row 1,1");
cmp_ok($omega_mat->[0]->[1],'==',0, "get_record_matrix row 0,1");
cmp_ok(scalar(@{$omega_mat}),'==',2, "get_record_matrix size");

$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 1,
										 row_format => 0);
cmp_ok($omega_mat->[0]->[0],'==',0.0750, "get_record_matrix col 0,0");
cmp_ok($omega_mat->[1]->[0],'==',0, "get_record_matrix col 1,0");
cmp_ok($omega_mat->[1]->[1],'==',0.0564, "get_record_matrix col 1,1");
cmp_ok($omega_mat->[0]->[1],'==',0.0467, "get_record_matrix col 0,1");
   


$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 2,
										 row_format => 1);
cmp_ok($omega_mat->[0]->[0],'==',2.82, "get_record_matrix row 0,0");

$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 2,
										 row_format => 0);
cmp_ok($omega_mat->[0]->[0],'==',2.82, "get_record_matrix col 0,0");



$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 3,
										 row_format => 1);

cmp_ok($omega_mat->[0]->[0],'==',0.0147, "get_record_matrix row 0,0");

$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 3,
										 row_format => 0);

cmp_ok($omega_mat->[0]->[0],'==',0.0147, "get_record_matrix col 0,0");


$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 5,
										 row_format => 1);

cmp_ok($omega_mat->[0]->[0],'==',0.506, "get_record_matrix row 0,0");
$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 5,
										 row_format => 0);

cmp_ok($omega_mat->[0]->[0],'==',0.506, "get_record_matrix col 0,0");

# find_table
my $model = model->new(filename => "$modeldir/pheno.mod");
my $problem = $model->problems->[0];

ok ($problem->find_table(columns => [ 'ID', 'TVCL' ]), "find_table 1");
ok (!$problem->find_table(columns => [ 'ID', 'RED' ]), "find_table 1");

done_testing();
