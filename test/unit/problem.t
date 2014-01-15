#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests=>54;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages
use linear_algebra;
use model;
use model::problem;

sub is_array{
    my $func=shift;
    my $facit=shift;
    my $label=shift;

    is (scalar(@{$func}),scalar(@{$facit}),"$label, equal length");

    my $min = scalar(@{$func});
    $min = scalar(@{$facit}) if (scalar(@{$facit})< $min);
    for (my $i=0; $i<$min; $i++){
    	is ($func->[$i],$facit->[$i],"$label, index $i");
    }		
	
}

my $modeldir = "../test_files";
my $model = model->new(filename => "$modeldir/pheno.mod");
my $problem = $model->problems->[0];


#
my $omega_mat = $problem->get_matrix(type => 'omega',
									 start_row => 1);
ok ($omega_mat->[0]->[0] == 0.4000, "get_matrix 0,0");
ok ($omega_mat->[1]->[1] == 0.2500, "get_matrix 1,1");


# Test get_filled_omega method
my $full_omega = $problem->get_filled_omega_matrix(start_eta => 1);

ok ($full_omega->[0]->[0] == 0.4000, "get_filled_omega_matrix 0,0");
ok ($full_omega->[0]->[1] == 0.0000, "get_filled_omega_matrix 0,1");
ok ($full_omega->[1]->[0] == 0.0099, "get_filled_omega_matrix 1,0");
ok ($full_omega->[1]->[1] == 0.2500, "get_filled_omega_matrix 1,1");

#
$model = model->new(filename => "$modeldir/mox1.mod");
$problem = $model->problems->[0];

ok (($problem->check_start_eta(start_eta => 1)) == 1, "check start_eta 1");
ok (($problem->check_start_eta(start_eta => 3)) == 2, "check start_eta 3");
ok (($problem->check_start_eta(start_eta => 4)) == 3, "check start_eta 4");
ok (($problem->check_start_eta(start_eta => 5)) == 4, "check start_eta 5");
ok (($problem->check_start_eta(start_eta => 6)) == 5, "check start_eta 6");
ok (($problem->check_start_eta(start_eta => 7)) == 6, "check start_eta 7");

#
$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 1);
ok ($omega_mat->[0]->[0] == 0.0750, "get_matrix 0,0");
ok ($omega_mat->[1]->[0] == 0.0467, "get_matrix 1,0");
ok ($omega_mat->[1]->[1] == 0.0564, "get_matrix 1,1");
ok ($omega_mat->[2]->[2] == 2.82, "get_matrix 2,2");
ok ($omega_mat->[3]->[3] == 0.0147, "get_matrix 3,3");
ok ($omega_mat->[4]->[4] == 0.0147, "get_matrix 4,4");
ok ($omega_mat->[5]->[5] == 0.506, "get_matrix 5,5");
ok ($omega_mat->[6]->[6] == 0.506, "get_matrix 6,6");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 1,
								  end_row => 5);
ok ($omega_mat->[0]->[0] == 0.0750, "get_matrix 0,0");
ok ($omega_mat->[1]->[0] == 0.0467, "get_matrix 1,0");
ok ($omega_mat->[1]->[1] == 0.0564, "get_matrix 1,1");
ok ($omega_mat->[2]->[2] == 2.82, "get_matrix 2,2");
ok ($omega_mat->[3]->[3] == 0.0147, "get_matrix 3,3");
ok ($omega_mat->[4]->[4] == 0.0147, "get_matrix 4,4");
ok (scalar(@{$omega_mat}) == 5, "get_matrix size");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 3);
ok ($omega_mat->[0]->[0] == 2.82, "get_matrix 0,0");
ok ($omega_mat->[1]->[1] == 0.0147, "get_matrix 1,1");
ok ($omega_mat->[2]->[2] == 0.0147, "get_matrix 2,2");
ok ($omega_mat->[3]->[3] == 0.506, "get_matrix 3,3");
ok ($omega_mat->[4]->[4] == 0.506, "get_matrix 4,4");
ok (scalar(@{$omega_mat}) == 5, "get_matrix size");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 3,
								  end_row => 6);
ok ($omega_mat->[0]->[0] == 2.82, "get_matrix 0,0");
ok ($omega_mat->[1]->[1] == 0.0147, "get_matrix 1,1");
ok ($omega_mat->[2]->[2] == 0.0147, "get_matrix 2,2");
ok ($omega_mat->[3]->[3] == 0.506, "get_matrix 3,3");
ok (scalar(@{$omega_mat}) == 4, "get_matrix size");



$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 4,
								  end_row => 7);
ok ($omega_mat->[0]->[0] == 0.0147, "get_matrix 0,0");
ok ($omega_mat->[1]->[1] == 0.0147, "get_matrix 1,1");
ok ($omega_mat->[2]->[2] == 0.506, "get_matrix 2,2");
ok ($omega_mat->[3]->[3] == 0.506, "get_matrix 3,3");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 5,
								  end_row => 7);
ok ($omega_mat->[0]->[0] == 0.0147, "get_matrix 0,0");
ok ($omega_mat->[1]->[1] == 0.506, "get_matrix 1,1");
ok ($omega_mat->[2]->[2] == 0.506, "get_matrix 2,2");

$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 6);
ok ($omega_mat->[0]->[0] == 0.506, "get_matrix 0,0");
ok ($omega_mat->[1]->[1] == 0.506, "get_matrix 1,1");
$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 7);
ok ($omega_mat->[0]->[0] == 0.506, "get_matrix 0,0");

my $cov_mat = [[0.02,0.003,0.004],
			   [0.003,0.5,0.00000000000000002],
			   [0.004,0.00000000000000002,0.5]	];

$full_omega = $problem->get_filled_omega_matrix(start_eta => 5,
												covmatrix => $cov_mat);
is ($full_omega->[0]->[0],0.0147, "get_fileed_omega_matrix 0,0");
is ($full_omega->[1]->[0],0.003, "get_fileed_omega_matrix 1,0");
is ($full_omega->[1]->[1],0.506, "get_filled_omega_matrix 1,1");
is ($full_omega->[2]->[0],0.004, "get_filled_omega matrix 0,2");
is ($full_omega->[2]->[1],0.006515, "get_filled_omega matrix 1,2");
is ($full_omega->[2]->[2],0.506, "get_filled_omega matrix 2,2");

done_testing();
