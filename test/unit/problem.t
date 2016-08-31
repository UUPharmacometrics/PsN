#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use linear_algebra;
use model;
use model::problem;
use ui;

ui->silent(1);

my $modeldir = $includes::testfiledir;
my $model = model->new(filename => "$modeldir/pheno.mod");
my $problem = $model->problems->[0];
is_deeply($problem->get_estimated_attributes(attribute => 'choleskyform'),
		  [0,0,0,0,0],
		  'get estimated attributes choleskyform');
# Test get_filled_omega method
my $full_omega = $problem->get_filled_omega_matrix(start_eta => 1);

cmp_ok($full_omega->[0]->[0],'==',0.4000, "get_filled_omega_matrix 0,0");
cmp_float($full_omega->[0]->[1],sqrt(0.4)*sqrt(0.25)*0.01, "get_filled_omega_matrix 0,1");
cmp_ok($full_omega->[1]->[0],'==',$full_omega->[0]->[1], "get_filled_omega_matrix 1,0");
cmp_ok($full_omega->[1]->[1],'==',0.2500, "get_filled_omega_matrix 1,1");

#
my $omega_mat = $problem->get_matrix(type => 'omega',
									 start_row => 1);
cmp_ok($omega_mat->[0]->[0],'==',0.4000, "get_matrix 0,0");
cmp_ok($omega_mat->[1]->[1],'==',0.2500, "get_matrix 1,1");
cmp_ok($omega_mat->[0]->[1],'==',0, "get_matrix 0,1");
cmp_ok($omega_mat->[1]->[0],'==',0, "get_matrix 1,0");


$problem->add_records(type => 'omega',record_strings =>['BLOCK(2) 0.02 CHOLESK ','0.001 0.5']);
$problem->reset_estimated_parameters_hash();
is_deeply($problem->get_estimated_attributes(attribute => 'choleskyform'),
		  [0,0,0,0,1,1,1,0],
		  'get estimated attributes choleskyform 2');

#crash tests repara

foreach my $input ('omega','sigma','omega,diagonal',
				   'diagonal','fix','omega,sigma,diagonal','fix,omega','diagonal,sigma',
				   'o1,s1','o1','s1'){
	$model = model->new(filename => "$modeldir/pheno.mod");
	$model->problems->[0]->cholesky_reparameterize(what => $input);
}
$model = model->new(filename => "$modeldir/pheno.mod");
$model->problems->[0]->cholesky_reparameterize(what => 'all',
											   bounded_theta => 0);
my $start_theta_record_index = $model->problems->[0]->find_start_theta_record_index(theta_number => 3);
is($start_theta_record_index,2,'pheno start_theta_record_index unbounded'); 
my ($corhash,$sdhash,$esthash) = $model->problems->[0]->get_SD_COR_values(start_theta_record_index => $start_theta_record_index,
																		  bounded_theta => 0,
																		  theta_record_count => 3);
cmp_relative($sdhash->{'A'}->{1},sqrt(0.4),7,'SD A 1 unbounded');
cmp_relative($sdhash->{'A'}->{2},sqrt(0.25),7,'SD A 2 unbounded');
cmp_relative($sdhash->{'B'}->{1},sqrt(0.04),7,'SD B 1 unbounded');
is(($esthash->{'A'} == 1),1,'pheno any est unbounded');
is(($esthash->{'B'} == 1),1,'pheno any est unbounded');

$model = model->new(filename => "$modeldir/pheno.mod");
$model->problems->[0]->cholesky_reparameterize(what => 'all', bounded_theta => 1);
$start_theta_record_index = $model->problems->[0]->find_start_theta_record_index(theta_number => 3);
is($start_theta_record_index,2,'pheno start_theta_record_index'); 
($corhash,$sdhash,$esthash) = $model->problems->[0]->get_SD_COR_values(start_theta_record_index => $start_theta_record_index,
																		  bounded_theta => 1,
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
$model->problems->[0]->cholesky_reparameterize(what => 'o1',
											   bounded_theta => 0);
is($model->problems->[0]->record_count(record_name => 'theta'),7,'mox 1 theta count after cholesky repara 01 unbounded');
$start_theta_record_index = $model->problems->[0]->find_start_theta_record_index(theta_number => 5);
is($start_theta_record_index,4,'mox start_theta_record_index unbounded'); 
($corhash,$sdhash,$esthash) = $model->problems->[0]->get_SD_COR_values(start_theta_record_index => $start_theta_record_index,
																		  bounded_theta => 0,
																		  theta_record_count => 3);
cmp_relative($sdhash->{'A'}->{1},sqrt(0.0750),6,'mox SD A 1 unbounded');
cmp_relative($sdhash->{'A'}->{2},sqrt(0.0564),6,'mox SD A 2 unbounded');
cmp_relative($corhash->{'A'}->{'2,1'},0.0467/(sqrt(0.0564)*sqrt(0.075)),7,'mox COR A 2,1 unbounded');
is(($esthash->{'A'} == 1),1,'mox any est unbounded');


$model = model->new(filename => "$modeldir/mox1.mod");
$model->problems->[0]->cholesky_reparameterize(what => 'o1', bounded_theta=> 1);
is($model->problems->[0]->record_count(record_name => 'theta'),7,'mox 1 theta count after cholesky repara 01');
$start_theta_record_index = $model->problems->[0]->find_start_theta_record_index(theta_number => 5);
is($start_theta_record_index,4,'mox start_theta_record_index'); 
($corhash,$sdhash,$esthash) = $model->problems->[0]->get_SD_COR_values(start_theta_record_index => $start_theta_record_index,
																		  bounded_theta => 1,
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

cmp_ok(($problem->check_skip_etas(skip_etas => 0)),'==',1, "check skip_etas 0");
dies_ok  { $problem->check_skip_etas(skip_etas => 1) } "check skip_etas 1";

cmp_ok(($problem->check_skip_etas(skip_etas => 2)),'==',2, "check skip_etas 2");
cmp_ok(($problem->check_skip_etas(skip_etas => 3)),'==',3, "check skip_etas 3");
cmp_ok(($problem->check_skip_etas(skip_etas => 4)),'==',4, "check skip_etas 4");
cmp_ok(($problem->check_skip_etas(skip_etas => 5)),'==',5, "check skip_etas 5");
cmp_ok(($problem->check_skip_etas(skip_etas => 6)),'==',6, "check skip_etas 6");
cmp_ok(($problem->check_skip_etas(skip_etas => 7)),'==',7, "check skip_etas 7");

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
cmp_float($full_omega->[2]->[1],(0.506*0.01), "get_filled_omega matrix 2,1");
cmp_ok($full_omega->[1]->[2],'==',$full_omega->[2]->[1], "get_filled_omega matrix 1,2");
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
$model = model->new(filename => "$modeldir/pheno.mod");
$problem = $model->problems->[0];

ok ($problem->find_table(columns => [ 'ID', 'TVCL' ]), "find_table 1");
ok (!$problem->find_table(columns => [ 'ID', 'RED' ]), "find_table 1");

$model = model->new(filename => "$modeldir/nonposdef.mod");
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
$sigma_mat = $model->problems->[0]->get_record_matrix(type => 'sigma',
														 record_number => 1);
cmp_ok($sigma_mat->[0]->[0],'==',1, "get_record_matrix row 0,0 after posdef");

$model = model->new(filename => "$modeldir/nonposdef.mod");
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


# ensure_unique_labels
$model = model->new(filename => "$modeldir/model/pheno_not_unique_labels.mod", ignore_missing_data => 1);
is ($model->problems->[0]->thetas->[0]->options->[0]->label, "CL", "ensure_unique_labels THETA1");
is ($model->problems->[0]->thetas->[1]->options->[0]->label, "V", "ensure_unique_labels THETA2");
is ($model->problems->[0]->omegas->[0]->options->[0]->label, "CL_", "ensure_unique_labels OMEGA1");
is ($model->problems->[0]->omegas->[0]->options->[1]->label, "CL__", "ensure_unique_labels OMEGA2");


$model = model->new(filename => $includes::testfiledir."/mox1.mod", ignore_missing_data => 1);
my $ref = $model->problems->[0]->get_eta_sets(header_strings => 1);
is_deeply($ref->{'iiv'},['ETA(1)','ETA(2)','ETA(3)'],"get eta headers iiv");
is(scalar(@{$ref->{'iov'}}),2,"get eta headers two occasions");
is($ref->{'iov'}->[0]->[0],'ETA(4)',"get eta headers iov 1 occasion 1 et1 ");
is($ref->{'iov'}->[0]->[1],'ETA(6)',"get eta headers iov 1 occasion 1 et 2");
is($ref->{'iov'}->[1]->[0],'ETA(5)',"get eta headers iov 1 occasion 2 et 1");
is($ref->{'iov'}->[1]->[1],'ETA(7)',"get eta headers iov 1 occasion 2 et 2");
is_deeply($ref->{'iiv_omega_numbers'},[1,2],"get eta headers iiv_omega_numbers");
is_deeply($ref->{'iov_omega_numbers'},[3,4,5,6],"get eta headers iov_omega_numbers");

$ref = model::problem::etas_per_omega(problem => $model->problems->[0]);
is_deeply($ref,[[1,2],[3],[4],[5],[6],[7]],'eta_per_omega');

$ref = $model->problems->[0]->get_eta_sets(header_strings => 0);
is_deeply($ref->{'iiv'},[1,2,3],"get eta headers iiv");
is(scalar(@{$ref->{'iov'}}),2,"get eta headers two occasions");
is($ref->{'iov'}->[0]->[0],4,"get eta headers iov 1 occasion 1 et1 ");
is($ref->{'iov'}->[0]->[1],6,"get eta headers iov 1 occasion 1 et 2");
is($ref->{'iov'}->[1]->[0],5,"get eta headers iov 1 occasion 2 et 1");
is($ref->{'iov'}->[1]->[1],7,"get eta headers iov 1 occasion 2 et 2");
is_deeply($ref->{'iiv_omega_numbers'},[1,2],"get eta headers iiv_omega_numbers 2");
is_deeply($ref->{'iov_omega_numbers'},[3,4,5,6],"get eta headers iov_omega_numbers 2");

$ref = $model->problems->[0]->get_eta_sets(header_strings => 0,skip_etas => 2);
is_deeply($ref->{'iiv'},[3],"get eta headers iiv skip2");
is(scalar(@{$ref->{'iov'}}),2,"get eta headers two occasions skip2");
is($ref->{'iov'}->[0]->[0],4,"get eta headers iov 1 occasion 1 et1 skip2 ");
is($ref->{'iov'}->[0]->[1],6,"get eta headers iov 1 occasion 1 et 2 skip2");
is($ref->{'iov'}->[1]->[0],5,"get eta headers iov 1 occasion 2 et 1 skip2");
is($ref->{'iov'}->[1]->[1],7,"get eta headers iov 1 occasion 2 et 2 skip2");
is_deeply($ref->{'iiv_omega_numbers'},[2],"get eta headers iiv_omega_numbers skip2");
is_deeply($ref->{'iov_omega_numbers'},[3,4,5,6],"get eta headers iov_omega_numbers skip2");

$ref = $model->problems->[0]->get_eta_sets(header_strings => 0,skip_etas => 3);
is_deeply($ref->{'iiv'},[],"get eta headers iiv skip3");
is(scalar(@{$ref->{'iov'}}),2,"get eta headers two occasions skip3");
is($ref->{'iov'}->[0]->[0],4,"get eta headers iov 1 occasion 1 et1  skip3");
is($ref->{'iov'}->[0]->[1],6,"get eta headers iov 1 occasion 1 et 2 skip3");
is($ref->{'iov'}->[1]->[0],5,"get eta headers iov 1 occasion 2 et 1 skip3");
is($ref->{'iov'}->[1]->[1],7,"get eta headers iov 1 occasion 2 et 2 skip3");
is_deeply($ref->{'iiv_omega_numbers'},[],"get eta headers iiv_omega_numbers skip3");
is_deeply($ref->{'iov_omega_numbers'},[3,4,5,6],"get eta headers iov_omega_numbers skip3");



$model = model->new(filename => $includes::testfiledir."/pheno.mod", ignore_missing_data => 1);
$ref = $model->problems->[0]->get_eta_sets(header_strings => 1);
is_deeply($ref->{'iiv'},['ETA(1)','ETA(2)'],"get eta headers iiv 2");
is(scalar(@{$ref->{'iov'}}),0,"get eta headers no iov");
is_deeply($ref->{'iiv_omega_numbers'},[1],"get eta headers iiv_omega_numbers no iov");
is_deeply($ref->{'iov_omega_numbers'},[],"get eta headers iov_omega_numbers no iov");
$ref = model::problem::etas_per_omega(problem => $model->problems->[0]);
is_deeply($ref,[[1,2]],'eta_per_omega');


$model->problems->[0]->remove_records(type => 'omega');
$model->problems->[0]->add_records(type => 'omega',record_strings =>['BLOCK(2) 0.02','-0.002 0.5']);
$model->problems->[0]->add_records(type => 'omega',record_strings =>['BLOCK(3) 0.02','-0.002 0.5','0.003 -0.005 1']);
$model->problems->[0]->add_records(type => 'omega',record_strings =>['BLOCK(3) SAME']);
$model->problems->[0]->add_records(type => 'omega',record_strings =>['BLOCK(3) SAME']);
$model->problems->[0]->add_records(type => 'omega',record_strings =>['BLOCK(3) 0.02','-0.002 0.5','0.003 -0.005 1']);
$model->problems->[0]->add_records(type => 'omega',record_strings =>['BLOCK(3) SAME']);
$model->problems->[0]->add_records(type => 'omega',record_strings =>['BLOCK(3) SAME']);

$ref = $model->problems->[0]->get_eta_sets(header_strings => 1);
is_deeply($ref->{'iiv'},['ETA(1)','ETA(2)'],"get eta headers iiv 3");
is_deeply($ref->{'iov'}->[0],['ETA(3)','ETA(4)','ETA(5)','ETA(12)','ETA(13)','ETA(14)'],"get eta headers 2 iov occasion 1");
is_deeply($ref->{'iov'}->[1],['ETA(6)','ETA(7)','ETA(8)','ETA(15)','ETA(16)','ETA(17)'],"get eta headers 2 iov occasion 2");
is_deeply($ref->{'iov'}->[2],['ETA(9)','ETA(10)','ETA(11)','ETA(18)','ETA(19)','ETA(20)'],"get eta headers 2 iov occasion 3");
is_deeply($ref->{'iiv_omega_numbers'},[1],"get eta headers iiv_omega_numbers added");
is_deeply($ref->{'iov_omega_numbers'},[2,3,4,5,6,7],"get eta headers iov_omega_numbers added");




$problem->set_records(type=>'omega',
					  record_strings => ['BLOCK(1) FIX',' 0.394415  ;    IOV LAG']); #1
$problem->add_records(type=>'omega',
					  record_strings => ['BLOCK(1) SAME']); #2
$problem->add_records(type=>'omega',
					  record_strings => ['BLOCK(2) FIX',
										 ' 0.415933  ;     IIV CL ',
										 ' 0.392094 0.583808  ;      IIV V']); # 3,4
$problem->add_records(type=>'omega',
					  record_strings => ['BLOCK(2)',
										 ' 0.0420795  ; BSV_LNWT_1',
										 ' 0.00103171 0.648676  ; BSV_DGRP_1']); #5,6
$problem->add_records(type=>'omega',
					  record_strings => ['BLOCK(1) FIX',
										 ' 0.224389  ;     IIV KA']); #7
$problem->add_records(type=>'omega',
					  record_strings => ['BLOCK(2)',
										 ' 0.0420799  ; BSV_LNWT_2',
										 ' 0.00103185 0.648675  ; BSV_DGRP_2']); #8,9
$omega_mat = $problem->get_matrix(type => 'omega',
								  start_row => 3,
								  end_row => 4);
cmp_ok($omega_mat->[0]->[0],'==',0.415933, "get_matrix 0,0");
cmp_ok($omega_mat->[0]->[1],'==',0, "get_matrix 0,1");
cmp_ok($omega_mat->[1]->[0],'==',0.392094, "get_matrix 1,0");
cmp_ok($omega_mat->[1]->[1],'==',0.583808, "get_matrix 1,1");

	
my $code = ['CL=THETA(2)*EXP(ETA(2))',
			'V=TVV+ETA(33)',
			'KA=THETA(4)*EXP(1+ETA(4))',
			'Y=ETA(5)+ETA(6)'];
my $ans = ['CL=THETA(2)*EXP(ETA(7))',
			'V=TVV+ETA(33)',
			'KA=THETA(4)*EXP(1+ETA(8))',
			'Y=ETA(10)+ETA(11)'];

model::problem::renumber_etas(code => $code,
							  eta_from => [[2,4,3,5,6]],
							  eta_to => [[7,8,9,10,11]]);
is($code->[0],$ans->[0],'renumber etas 1');
is($code->[1],$ans->[1],'renumber etas 2');
is($code->[2],$ans->[2],'renumber etas 3');
is($code->[3],$ans->[3],'renumber etas 4');

$code = ['CL=THETA(2)*EXP(ETA(2))',
			'V=TVV+ETA(33)',
			'KA=THETA(4)*EXP(1+ETA(4))',
			'Y=ETA(5)+ETA(6)'];
$ans = ['CL=THETA(2)*EXP((ETA(2)*ASD_ETA_2))',
			'V=TVV+ETA(33)',
			'KA=THETA(4)*EXP(1+(ETA(4)*ASD_ETA_4))',
			'Y=(ETA(5)*ASD_ETA_5)+(ETA(6)*ASD_ETA_6)'];
my $orig = ['CL=THETA(2)*EXP(ETA(2))',
			'V=TVV+ETA(33)',
			'KA=THETA(4)*EXP(1+ETA(4))',
			'Y=ETA(5)+ETA(6)'];

model::problem::substitute_scaled_etas(code => $code,
									   eta_list => [2,4,3,5,6],
									   inverse => 0);

is($code->[0],$ans->[0],'subst etas 1');
is($code->[1],$ans->[1],'subst etas 2');
is($code->[2],$ans->[2],'subst etas 3');
is($code->[3],$ans->[3],'subst etas 4');

model::problem::substitute_scaled_etas(code => $code,
									   eta_list => [2,4,3,5,6],
									   inverse => 1);

is($code->[0],$orig->[0],'subst etas 1 i');
is($code->[1],$orig->[1],'subst etas 2 i');
is($code->[2],$orig->[2],'subst etas 3 i');
is($code->[3],$orig->[3],'subst etas 4 i');

$model = model->new(filename => $includes::testfiledir."/mox1.mod", ignore_missing_data => 1);

model::problem::rescale_etas(problem => $model->problems->[0],
							 use_pred =>0,
							 omega_indices =>[0,1]);

is($model->problems->[0]-> record_count( record_name => 'theta' ),7,'theta count after rescale');

cmp_float($model->problems->[0]->thetas->[4]->options->[0]->init,sqrt(0.0750),'theta init 1');
is($model->problems->[0]->thetas->[4]->options->[0]->fix,1,'theta fix 1');

cmp_float($model->problems->[0]->thetas->[5]->options->[0]->init,sqrt(0.0564),'theta init 2');
is($model->problems->[0]->thetas->[5]->options->[0]->fix,1,'theta fix 2');
cmp_float($model->problems->[0]->thetas->[6]->options->[0]->init,sqrt(2.82),'theta init 3');
is($model->problems->[0]->thetas->[6]->options->[0]->fix,1,'theta fix 3');

#Test find_data_column

$model = model->new(filename => $includes::testfiledir."/pheno.mod", ignore_missing_data => 1);
$problem = $model->problems->[0];
is ($problem->find_data_column(column_name => 'ID'), 0, "find_data_column ID"); 
is ($problem->find_data_column(column_name => 'AMT'), 2, "find_data_column AMT"); 
is ($problem->find_data_column(column_name => 'DV'), 5, "find_data_column DV"); 
is ($problem->find_data_column(column_name => 'NOEX'), -1, "find_data_column non existing column"); 

done_testing();
