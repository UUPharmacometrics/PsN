#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::Random;
use influential_individuals;
use Config;
use Env qw(PATH);
use model;
use ui;
use tool::frem;
#ui->silent(1);


my $datadir = $includes::testfiledir;

#sub get_eta_count

my $array = ['SUBJECT_NO','ID','ETA(1)','ETA(2)','ETC(1,1)','ETC(2,1)','ETC(2,2)','OBJ'];

is(influential_individuals::get_eta_count(header => $array ),2,'get_eta_count test a');
   
$array = ['SUBJECT_NO','ID','ETA(1)','ETA(2)','ETA(3)','ETA(4)','ETA(5)','ETA(6)',
		   'ETA(7)','ETC(1,1)','ETC(2,1)','ETC(2,2)','ETC(3,1)','ETC(3,2)','ETC(3,3)',
		   'ETC(4,1)','ETC(4,2)','ETC(4,3)','ETC(4,4)','ETC(5,1)','ETC(5,2)','ETC(5,3)',
		   'ETC(5,4)','ETC(5,5)','ETC(6,1)','ETC(6,2)','ETC(6,3)','ETC(6,4)','ETC(6,5)',
		   'ETC(6,6)','ETC(7,1)','ETC(7,2)','ETC(7,3)','ETC(7,4)','ETC(7,5)','ETC(7,6)',
		   'ETC(7,7)','OBJ'];

is(influential_individuals::get_eta_count(header => $array ),7,'get_eta_count test b');

my $phi = nmtablefile->new(filename => "$datadir/mox_no_bov.phi");

is(influential_individuals::get_eta_count(header => $phi->tables->[0]->get_header),3,'get_eta_count test c');

$array = ['hej','ingen','riktig','ETA'];

is(influential_individuals::get_eta_count(header => $array ),0,'get_eta_count test d');

$array = ['THETA(1)','THETA(2)','THETA(3)'];

is(influential_individuals::get_eta_count(header => $array ),0,'get_eta_count test e');

$array = ['SUBJECT_NO','ID','ETA(A)','ETA(B)','ETA(C)','OBJ'];

is(influential_individuals::get_eta_count(header => $array ),0,'get_eta_count test f');

$array = ['SUBJECT_NO','ID','ETA(A)','ETA(12)','ETAs(2)','ETA(2)3','ETA(2)a'];

is(influential_individuals::get_eta_count(header => $array ),0,'get_eta_count test g');
   
#sub get_eta_means

$phi = nmtablefile->new(filename => "$datadir/mox1.phi");

my @excel_means =(-1.942467568E-04,-1.673126473E-02,-2.919216946E-02,2.709392482E-07,
					   -1.652155946E-07,-1.926669604E-05,1.354918608E-05);

cmp_float_array(influential_individuals::get_eta_means(table => $phi->tables->[0], eta_count =>7),\@excel_means,'get_eta_means mox1.phi');


#sub get_eta_covmatrix
my @correct_cov =([0.412497085304906,0.388203652647791,-0.0903657802519095,8.46691834696807e-07,5.98247681117187e-07,-1.23091414938343e-05,-5.38970893616148e-06],
[0.388203652647791,0.55329202846575,-0.117076857462827,1.56230741646067e-06,-1.4109232544616e-06,-7.6587861778325e-06,-1.52716470078611e-05],
[-0.0903657802519095,-0.117076857462827,0.137005484217071,9.1020343945293e-07,-1.0207844978907e-06,1.43217143342141e-05,1.2511903402109e-05],
[8.46691834696807e-07,1.56230741646067e-06,9.1020343945293e-07,8.6394504805325e-11,-8.83503529068283e-11,3.03750349919594e-10,-1.25480135081392e-10],
[5.98247681117187e-07,-1.4109232544616e-06,-1.0207844978907e-06,-8.83503529068283e-11,1.03137638999691e-10,-3.72106734134956e-10,1.72178396886762e-10],
[-1.23091414938343e-05,-7.6587861778325e-06,1.43217143342141e-05,3.03750349919594e-10,-3.72106734134956e-10,9.18802177921253e-09,-6.38300374968336e-09],
	[-5.38970893616148e-06,-1.52716470078611e-05,1.2511903402109e-05,-1.25480135081392e-10,1.72178396886762e-10,-6.38300374968336e-09,8.83356395440524e-09]);

my $covmatrix = influential_individuals::get_eta_covmatrix(table => $phi->tables->[0], eta_count =>7);

is_deeply($covmatrix,\@correct_cov,'get_eta_covmatrix mox1.phi');


#sub get_pred_code
my @code = ('IF(EBE.EQ.1) Y=THETA(1)+ETA(1)+EPS(1)*SQRT(VAR11)',
	'IF(EBE.EQ.2) Y=THETA(2)+ETA(2)+EPS(2)*SQRT(VAR22)');

is_deeply(influential_individuals::get_pred_code(eta_count => 2),\@code,' get_pred_code for 2 eta');


@code = ('IF(EBE.EQ.1) Y=THETA(1)+ETA(1)+EPS(1)*SQRT(VAR11)',
	'IF(EBE.EQ.2) Y=THETA(2)+ETA(2)+EPS(2)*SQRT(VAR22)',
	'IF(EBE.EQ.3) Y=THETA(3)+ETA(3)+EPS(3)*SQRT(VAR33)',
	'IF(EBE.EQ.4) Y=THETA(4)+ETA(4)+EPS(4)*SQRT(VAR44)');
is_deeply(influential_individuals::get_pred_code(eta_count => 4),\@code,' get_pred_code for 4 eta');

#sub get_theta_code
my @means =(0,-1.673126473,-2.919216946,-1.942467568);

#note that NONMEM does not accept initial value exactly 0 for a parameter that is not FIX
@code = (
'0.0001 ; mean_EBE1',
'-1.673126473 ; mean_EBE2',
'-2.919216946 ; mean_EBE3',
'-1.942467568 ; mean_EBE4',
	);

is_deeply(influential_individuals::get_theta_code(eta_means => \@means),\@code,' get_theta_code for 4 eta');

done_testing();
