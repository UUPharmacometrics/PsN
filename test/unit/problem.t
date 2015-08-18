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

# Test get_filled_omega method
my $full_omega = $problem->get_filled_omega_matrix(start_eta => 1);

cmp_ok($full_omega->[0]->[0],'==',0.4000, "get_filled_omega_matrix 0,0");
cmp_ok($full_omega->[0]->[1],'==',0.0001, "get_filled_omega_matrix 0,1");
cmp_ok($full_omega->[1]->[0],'==',0.0001, "get_filled_omega_matrix 1,0");
cmp_ok($full_omega->[1]->[1],'==',0.2500, "get_filled_omega_matrix 1,1");

#
my $omega_mat = $problem->get_matrix(type => 'omega',
									 start_row => 1);
cmp_ok($omega_mat->[0]->[0],'==',0.4000, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[1],'==',0.2500, "get_matrix 1,1");
cmp_ok($omega_mat->[0]->[1],'==',0, "get_matrix 0,1");
cmp_ok($omega_mat->[1]->[0],'==',0, "get_matrix 1,0");

#crash tests repara

foreach my $input ('omega','sigma','omega,diagonal',
				   'diagonal','fix','omega,sigma,diagonal','fix,omega','diagonal,sigma',
				   'o1,s1','o1','s1'){
	$model = model->new(filename => "$modeldir/pheno.mod");
	$model->problems->[0]->cholesky_reparameterize(what => $input);
}
$model = model->new(filename => "$modeldir/pheno.mod");
$model->problems->[0]->cholesky_reparameterize(what => 'all');
my $start_theta_record_index = $model->problems->[0]->find_start_theta_record_index(theta_number => 3);
is($start_theta_record_index,2,'pheno start_theta_record_index'); 
my ($corhash,$sdhash,$esthash) = $model->problems->[0]->get_SD_COR_values(start_theta_record_index => $start_theta_record_index,
																 theta_record_count => 3);
cmp_relative($sdhash->{'A'}->{1},sqrt(0.4),7,'SD A 1');
cmp_relative($sdhash->{'A'}->{2},sqrt(0.25),7,'SD A 2');
cmp_relative($sdhash->{'B'}->{1},sqrt(0.04),7,'SD B 1');
is(($esthash->{'A'} == 1),1,'pheno any est');
is(($esthash->{'B'} == 1),1,'pheno any est');
#
#$OMEGA  BLOCK(2) 
#0.0750   
#0.0467  0.0564  ; IIV (CL-V) 
$model = model->new(filename => "$modeldir/mox1.mod");
$model->problems->[0]->cholesky_reparameterize(what => 'o1');
is($model->problems->[0]->record_count(record_name => 'theta'),7,'mox 1 theta count after cholesky repara 01');
my $start_theta_record_index = $model->problems->[0]->find_start_theta_record_index(theta_number => 5);
is($start_theta_record_index,4,'mox start_theta_record_index'); 
my ($corhash,$sdhash,$esthash) = $model->problems->[0]->get_SD_COR_values(start_theta_record_index => $start_theta_record_index,
																 theta_record_count => 3);
cmp_relative($sdhash->{'A'}->{1},sqrt(0.0750),7,'mox SD A 1');
cmp_relative($sdhash->{'A'}->{2},sqrt(0.0564),7,'mox SD A 2');
cmp_relative($corhash->{'A'}->{'2,1'},0.0467/(sqrt(0.0564)*sqrt(0.075)),7,'mox COR A 2,1');
is(($esthash->{'A'} == 1),1,'mox any est');

my $hashref = model::problem::SD_COR_to_cov_vectors(cor_hash =>{'A' => {'2,1' => 1, '3,1'=>0.2, '3,2'=> 0.5}, 'C' => {'2,1'=> 0.5}},
									sd_hash =>{'A' => {1=>5,2=>3,3=>2}, 'B' => {1=>3,2=>4}, 'C' => {1=>3,2=>2}, 'D' => {1=>2}  });

is_deeply($hashref->{'A'},[25,15,9,2,3,4],'SD_COR_to_cov_vectors block 3');
is_deeply($hashref->{'C'},[9,3,4],'SD_COR_to_cov_vectors block 2');
is_deeply($hashref->{'B'},[9,16],'SD_COR_to_cov_vectors diagonal 2');
is_deeply($hashref->{'D'},[4],'SD_COR_to_cov_vectors diagonal 1');

$model->problems->[0]->remove_theta_records(start_index => $start_theta_record_index,
											count => 3);
is($model->problems->[0]->record_count(record_name => 'theta'),4,'mox 1 theta count after removed thetas index 4 count 3');

$model = model->new(filename => "$modeldir/mox1.mod");
$problem = $model->problems->[0];

cmp_ok(($problem->check_start_eta(start_eta => 1)),'==',1, "check start_eta 1");
cmp_ok(($problem->check_start_eta(start_eta => 3)),'==',2, "check start_eta 3");
cmp_ok(($problem->check_start_eta(start_eta => 4)),'==',3, "check start_eta 4");
cmp_ok(($problem->check_start_eta(start_eta => 5)),'==',4, "check start_eta 5");
cmp_ok(($problem->check_start_eta(start_eta => 6)),'==',5, "check start_eta 6");
cmp_ok(($problem->check_start_eta(start_eta => 7)),'==',6, "check start_eta 7");

is($problem->find_start_theta_record_index(theta_number=> 4),3, 'find_start_theta_record_index');
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
cmp_ok($full_omega->[0]->[1],'==',0.003, "get_filled_omega_matrix 0,1");
cmp_ok($full_omega->[1]->[1],'==',0.506, "get_filled_omega_matrix 1,1");
cmp_ok($full_omega->[2]->[0],'==',0.004, "get_filled_omega matrix 2,0");
cmp_ok($full_omega->[0]->[2],'==',0.004, "get_filled_omega matrix 0,2");
cmp_ok($full_omega->[2]->[1],'==',0.0001, "get_filled_omega matrix 2,1");
cmp_ok($full_omega->[1]->[2],'==',0.0001, "get_filled_omega matrix 1,2");
cmp_ok($full_omega->[2]->[2],'==',0.506, "get_filled_omega matrix 2,2");

$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 1);
cmp_ok($omega_mat->[0]->[0],'==',0.0750, "get_record_matrix row 0,0");
cmp_ok($omega_mat->[1]->[0],'==',0.0467, "get_record_matrix row 1,0");
cmp_ok($omega_mat->[1]->[1],'==',0.0564, "get_record_matrix row 1,1");
cmp_ok($omega_mat->[0]->[1],'==',0.0467, "get_record_matrix row 0,1");
cmp_ok(scalar(@{$omega_mat}),'==',2, "get_record_matrix size");

   


$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 2);
cmp_ok($omega_mat->[0]->[0],'==',2.82, "get_record_matrix row 0,0");

$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 2);
cmp_ok($omega_mat->[0]->[0],'==',2.82, "get_record_matrix col 0,0");



$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 3);

cmp_ok($omega_mat->[0]->[0],'==',0.0147, "get_record_matrix row 0,0");



$omega_mat = $problem->get_record_matrix(type => 'omega',
										 record_number => 5);

cmp_ok($omega_mat->[0]->[0],'==',0.506, "get_record_matrix row 0,0");

# find_table
my $model = model->new(filename => "$modeldir/pheno.mod");
my $problem = $model->problems->[0];

ok ($problem->find_table(columns => [ 'ID', 'TVCL' ]), "find_table 1");
ok (!$problem->find_table(columns => [ 'ID', 'RED' ]), "find_table 1");

my $model = model->new(filename => "$modeldir/nonposdef.mod");
$omega_mat = $model->problems->[0]->get_record_matrix(type => 'omega',
													  record_number => 1);
cmp_ok($omega_mat->[0]->[0],'==',0.3, "get_record_matrix row 0,0");
my $sigma_mat = $model->problems->[0]->get_record_matrix(type => 'sigma',
														 record_number => 1);
cmp_ok($sigma_mat->[0]->[0],'==',1, "get_record_matrix row 0,0");

$omega_mat = $model->problems->[0]->get_record_matrix(type => 'omega',
													  record_number => 2);
cmp_ok($omega_mat->[0]->[0],'==',0.3, "get_record_matrix row 0,0");
cmp_ok($omega_mat->[1]->[0],'==',0.3, "get_record_matrix row 1,0");
cmp_ok($omega_mat->[1]->[1],'==',0.3, "get_record_matrix row 1,1");
cmp_ok($omega_mat->[0]->[1],'==',0.3, "get_record_matrix row 0,1");

$model->problems->[0]->ensure_posdef();

$omega_mat = $model->problems->[0]->get_record_matrix(type => 'omega',
													  record_number => 2);
cmp_ok($omega_mat->[0]->[0],'==',0.315, "get_record_matrix row 0,0 after posdef");
cmp_ok($omega_mat->[1]->[0],'==',0.3, "get_record_matrix row 1,0 after posdef");
cmp_ok($omega_mat->[1]->[1],'==',0.315, "get_record_matrix row 1,1 after posdef");
cmp_ok($omega_mat->[0]->[1],'==',0.3, "get_record_matrix row 0,1 after posdef");

$omega_mat = $model->problems->[0]->get_record_matrix(type => 'omega',
													  record_number => 1);
cmp_ok($omega_mat->[0]->[0],'==',0.3, "get_record_matrix row 0,0 after posdef");
my $sigma_mat = $model->problems->[0]->get_record_matrix(type => 'sigma',
														 record_number => 1);
cmp_ok($sigma_mat->[0]->[0],'==',1, "get_record_matrix row 0,0 after posdef");

my $model = model->new(filename => "$modeldir/nonposdef.mod");
$model->problems->[0]->ensure_posdef(inflate_diagonal => 0);
$omega_mat = $model->problems->[0]->get_record_matrix(type => 'omega',
													  record_number => 2);
cmp_ok($omega_mat->[0]->[0],'==',0.3, "get_record_matrix row 0,0 after posdef deflate");
cmp_ok($omega_mat->[1]->[0],'==',0.285, "get_record_matrix row 1,0 after posdef deflate");
cmp_ok($omega_mat->[1]->[1],'==',0.3, "get_record_matrix row 1,1 after posdef deflate");
cmp_ok($omega_mat->[0]->[1],'==',0.285, "get_record_matrix row 0,1 after posdef deflate");

my @code =qw(CH_C22=SQRT(1-(COR_C21**2)) CH_C32=(COR_C32-COR_C21*COR_C31)/CH_C22
CH_C42=(COR_C42-COR_C21*COR_C41)/CH_C22 CH_C52=(COR_C52-COR_C21*COR_C51)/CH_C22
CH_C62=(COR_C62-COR_C21*COR_C61)/CH_C22 CH_C33=SQRT(1-(COR_C31**2+CH_C32**2))
CH_C43=(COR_C43-(COR_C31*COR_C41+CH_C32*CH_C42))/CH_C33
CH_C53=(COR_C53-(COR_C31*COR_C51+CH_C32*CH_C52))/CH_C33
CH_C63=(COR_C63-(COR_C31*COR_C61+CH_C32*CH_C62))/CH_C33
CH_C44=SQRT(1-(COR_C41**2+CH_C42**2+CH_C43**2))
CH_C54=(COR_C54-(COR_C41*COR_C51+CH_C42*CH_C52+CH_C43*CH_C53))/CH_C44
CH_C64=(COR_C64-(COR_C41*COR_C61+CH_C42*CH_C62+CH_C43*CH_C63))/CH_C44
CH_C55=SQRT(1-(COR_C51**2+CH_C52**2+CH_C53**2+CH_C54**2))
CH_C65=(COR_C65-(COR_C51*COR_C61+CH_C52*CH_C62+CH_C53*CH_C63+CH_C54*CH_C64))/CH_C55
CH_C66=SQRT(1-(COR_C61**2+CH_C62**2+CH_C63**2+CH_C64**2+CH_C65**2))
ETA_6=ETA(6)*SD_C1
ETA_7=ETA(6)*COR_C21*SD_C2+ETA(7)*CH_C22*SD_C2
ETA_8=ETA(6)*COR_C31*SD_C3+ETA(7)*CH_C32*SD_C3+ETA(8)*CH_C33*SD_C3
ETA_9=ETA(6)*COR_C41*SD_C4+ETA(7)*CH_C42*SD_C4+ETA(8)*CH_C43*SD_C4+ETA(9)*CH_C44*SD_C4
ETA_10=ETA(6)*COR_C51*SD_C5+ETA(7)*CH_C52*SD_C5+ETA(8)*CH_C53*SD_C5+ETA(9)*CH_C54*SD_C5+ETA(10)*CH_C55*SD_C5
ETA_11=ETA(6)*COR_C61*SD_C6+ETA(7)*CH_C62*SD_C6+ETA(8)*CH_C63*SD_C6+ETA(9)*CH_C64*SD_C6+ETA(10)*CH_C65*SD_C6+ETA(11)*CH_C66*SD_C6
);

my $newc= model::problem::reformat_code(code => \@code,
							  max_length => 50);

#print join("\n",@{$newc})."\n";
done_testing();
