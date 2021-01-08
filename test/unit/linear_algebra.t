#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use linear_algebra;

#subtract, max and absolute
my @A = ([1, 2, 3], [2.1, 4, 5], [3.25, 5.17, 19]);


my @B = ([1, 2.1, 3.25], [2, 4, 5.17], [3, 5, 19]);
my $C = linear_algebra::subtract(\@A, \@B);
is_deeply($C, [[ 0.  , -0.1 , -0.25], [ 0.1 ,  0.  , -0.17], [ 0.25,  0.17,  0.  ]], "matrix subtraction");
is (linear_algebra::max($C), 0.25, "matrix maximum");
linear_algebra::absolute($C);
is_deeply($C, [[ 0.  , 0.1 , 0.25], [ 0.1,  0., 0.17], [ 0.25,  0.17,  0. ]], "matrix absolute");

#pad_matrix
@A = ([1, 2, 4], [3, 5 ,7], [8, 4, 1]);
linear_algebra::pad_matrix(\@A, 5);

is(scalar(@A), 5, "pad_matrix num rows");
is_deeply($A[0], [1, 2, 4, 0, 0], "pad_matrix row 1");
is_deeply($A[1], [3, 5, 7, 0, 0], "pad_matrix row 2");
is_deeply($A[2], [8, 4, 1, 0, 0], "pad_matrix row 3");
is_deeply($A[3], [0, 0, 0, 1, 0], "pad_matrix row 4");
is_deeply($A[4], [0, 0, 0, 0, 1], "pad_matrix row 5");

#reduce_matrix
@A = ([1, 3, 5, 7], [8, 6, 4, 2], [1, 2, 3, 4], [6, 7, 9, 1]);

linear_algebra::reduce_matrix(\@A, 4);
is(scalar(@A), 4, "reduce_matrix don't reduce");
is_deeply($A[0], [1, 3, 5, 7], "reduce_matrix don't reduce row 0");
is_deeply($A[3], [6, 7, 9, 1], "reduce_matrix don't reduce row 3");

linear_algebra::reduce_matrix(\@A, 2);

is(scalar(@A), 2, "reduce_matrix num rows");
is_deeply($A[0], [1, 3], "reduce_matrix row 0");
is_deeply($A[1], [8, 6], "reduce_matrix row 1");

#put_ones_on_diagonal_of_zero_lines
@A = ([1, 2, 3, 4], [0, 0, 0, 0], [9, 8, 7, 6], [0, 0, 0, 0]);
linear_algebra::put_ones_on_diagonal_of_zero_lines(\@A);

is_deeply($A[0], [1, 2, 3, 4], "put_ones_on... row 1");
is_deeply($A[1], [0, 1, 0, 0], "put_ones_on... row 2");
is_deeply($A[2], [9, 8, 7, 6], "put_ones_on... row 3");
is_deeply($A[3], [0, 0, 0, 1], "put_ones_on... row 4");

#triangular_symmetric_to_full
@A = (1, 5, 3, 7, 8, 2);
my $res = linear_algebra::triangular_symmetric_to_full(\@A);

is_deeply($res->[0], [1, 5, 7], "triangular_symmetric_to_full row 1");
is_deeply($res->[1], [5, 3, 8], "triangular_symmetric_to_full row 2");
is_deeply($res->[2], [7, 8, 2], "triangular_symmetric_to_full row 3");

#Transpose
@A = ([1, 2, 3], [4, 5, 6], [7, 8, 9]);
linear_algebra::transpose(\@A);

is_deeply($A[0], [1, 4, 7], "transpose row 1");
is_deeply($A[1], [2, 5, 8], "transpose row 2");
is_deeply($A[2], [3, 6, 9], "transpose row 3");

#LU_factorization
my $A = [[2, 3], [1, 2]];
linear_algebra::LU_factorization($A);
is_deeply($A, [[2, 3], [0.5, 0.5]], "lu matrix A");

@B = ([8, 4, 1], [5, 5, 2], [4, 2, 2]);
linear_algebra::LU_factorization(\@B);
is_deeply(\@B, [[8, 4, 1], [0.625, 2.5, 1.375], [0.5, 0, 1.5]], "lu matrix B");

$C = [[8, 7, 6, 5], [4, 3, 2, 1], [4, 3, 3, 1], [4, 3, 2, 0]];
linear_algebra::LU_factorization($C);
is_deeply($C, [[8, 7, 6, 5], [0.5, -0.5, -1, -1.5], [0.5, 1, 1, 0], [0.5, 1, 0, -1]], "lu matrix C");

#eigenvalue_decomposition
my @eigenValMatrix = ( [ '8.9544282663304415E+01', '-8.2566649009388087E+01', '8.6967174304372286E-02', '-4.3909645699741713E+00', '-1.6476336788019591E+01',
        '1.3784822470949292E+01', '1.2074968416416303E+00', '1.5699623392919333E+00', '-1.3673882615395747E+00', '-5.8476185564888334E+00' ],
      [ '-8.2566649009388087E+01', '5.0538799354932610E+02', '7.8999591192394849E+00', '-1.1155228236154463E+02', '-9.2582185092361200E+01',
        '-2.2659309460626335E+02', '7.2529816314235845E+01', '5.7048131765312196E+00', '-7.7734606378735516E-01', '-9.4482044146047386E+00' ],
      [ '8.6967174304372286E-02', '7.8999591192394849E+00', '2.3183671967132167E+01', '-7.4562261429523469E+00', '-5.9125649890345251E+00',
        '-1.7946256897967089E+01', '-1.7748821194127629E+01', '-6.2240178351010755E-01', '6.1756262261847938E-01', '7.8010427134354521E-01' ],
      [ '-4.3909645699741713E+00', '-1.1155228236154463E+02', '-7.4562261429523469E+00', '5.5315535817910721E+01', '9.0156228965423949E+01',
        '-2.2150166713057892E+01', '3.1349844124200253E+01', '-2.6057687943018619E+00', '-1.3024298077126174E+01', '-1.0438244499744593E+01' ],
      [ '-1.6476336788019591E+01', '-9.2582185092361200E+01', '-5.9125649890345251E+00', '9.0156228965423949E+01', '2.6483381331460754E+02',
        '-2.4031602911954596E+02', '2.4789036253653387E+02', '2.3230899216073259E-01', '-6.4928365881499488E+01', '-9.0630421685273802E+01' ],
      [ '1.3784822470949292E+01', '-2.2659309460626335E+02', '-1.7946256897967089E+01', '-2.2150166713057892E+01', '-2.4031602911954596E+02',
        '4.9321453266930064E+02', '-3.4414470741622665E+02', '-4.3428373858779965E+00', '7.8850627454916236E+01', '1.1527757017894507E+02' ],
      [ '1.2074968416416303E+00', '7.2529816314235845E+01', '-1.7748821194127629E+01', '3.1349844124200253E+01', '2.4789036253653387E+02',
        '-3.4414470741622665E+02', '9.4215994383596786E+04', '3.8342815093138626E+01', '7.7211729326696073E+01', '1.3484651750320691E+02' ],
      [ '1.5699623392919333E+00', '5.7048131765312196E+00', '-6.2240178351010755E-01', '-2.6057687943018619E+00', '2.3230899216073259E-01',
        '-4.3428373858779965E+00', '3.8342815093138626E+01', '1.5964711542724439E+02', '-9.4095451454909025E+01', '5.5635413108142568E+00' ],
      [ '-1.3673882615395747E+00', '-7.7734606378735516E-01', '6.1756262261847938E-01', '-1.3024298077126174E+01', '-6.4928365881499488E+01',
        '7.8850627454916236E+01', '7.7211729326696073E+01', '-9.4095451454909025E+01', '4.8678696776193942E+02', '-2.9983667699545244E+01' ],
      [ '-5.8476185564888334E+00', '-9.4482044146047386E+00', '7.8010427134354521E-01', '-1.0438244499744593E+01', '-9.0630421685273802E+01',
        '1.1527757017894507E+02', '1.3484651750320691E+02', '5.5635413108142568E+00', '-2.9983667699545244E+01', '9.3718803441985904E+02' ]
    );

(my $eigen, my $vecs) = linear_algebra::eigenvalue_decomposition(\@eigenValMatrix);
cmp_float_array($eigen,
[ '103.735518977137', '577.409123620258', '26.4391034036385', '8.31436812089702', '-0.0922465852143416',
  '735.406959776145', '94218.259066972', '132.962268669761', '421.085176924333', '1007.57699130853' ], "eigenvalues");
cmp_float_matrix($vecs,
[ [ '0.858533360145535', '-0.0815272625678409', '-0.276778340107342', '-0.0370674630316422', '0.407929997751623', '0.0941137963799454', '1.10321458448217e-05', '0.0501809815749305', '0.0154494947282136', '0.0165051795403893' ],
          [ '-0.127128277969396', '0.593306835379972', '-0.192703954766765', '-0.0135472742932071', '0.408079183625171', '-0.585521402054757', '0.000779676970620123', '-0.0286033162157461', '-0.253754027916267', '-0.14118519949914' ],
          [ '0.0993153815302009', '0.0126487578069314', '0.905771563340727', '-0.0392429769917785', '0.409272163234004', '-0.0213427696052516', '-0.000187835774071636', '0.00175871852579824', '0.00444455468674422', '-0.00567695916958289' ],
          [ '-0.224380389103329', '-0.218375302337189', '-0.118680379435431', '0.829808874284842', '0.404066784432321', '0.0562716614554156', '0.000335132785146617', '0.00430942902793786', '0.179791297366182', '-0.0227553893526604' ],
          [ '-0.303350253536207', '-0.429591195213723', '-0.175719849038885', '-0.510973612848424', '0.410972196231212', '-0.152306935556326', '0.00264542648885041', '0.0264427130719482', '0.438011164668075', '-0.216339977529082' ],
          [ '-0.302158678647344', '0.123165409979213', '-0.144181113777307', '-0.216508321555358', '0.409135366737881', '0.608711288261839', '-0.00367808146790063', '-0.0558855034169174', '-0.385338814667859', '0.369218725407792' ],
          [ '-0.000112422607409911', '0.000885262766390327', '0.000307388401360586', '0.000309941246724041', '3.52252977801686e-05', '0.00341750857813817', '0.999987907415609', '-0.000890103224710209', '-0.00319067371725204', '0.000738142340499522' ],
          [ '-0.0553746022836126', '-0.130311263073596', '0.00558743004885902', '0.0028712626758958', '0.000552813122758316', '-0.0636376528689959', '0.000407125813673901', '0.957933772312949', '-0.241299741404108', '-0.00051483816067176' ],
          [ '-0.00695086615822947', '0.607570109038514', '-0.00506909671999226', '-0.0115381932216566', '0.000749655862838344', '0.303716331646694', '0.00081789479396663', '0.273792978746659', '0.679992431984221', '0.0324696567072524' ],
          [ '0.0106230539607319', '-0.0874698874648048', '-0.005395398500717', '-0.0144339534684047', '0.000252445100494845', '-0.393259954682628', '0.00143810308725646', '0.0148036793073181', '0.205086094443641', '0.891663468631833' ]
        ], "eigenvecs");


$A = [[9,8,7], [8, 5, 3], [7, 3, 7]];
($eigen, $vecs) = linear_algebra::eigenvalue_decomposition($A);
cmp_float_array($eigen, [ 19.609194939332617,  -1.903857409809233,   3.294662470476631 ], "eigen simple matrix");
cmp_float_matrix($vecs, [[ 0.706547465982442, -0.684965411383339, -0.177800628576624],
       [ 0.491463745176441,  0.655722706583517, -0.573141447853785],
       [ 0.509169977012525,  0.317569074815606,  0.79993488311851 ]], "eigenvectors simple matrix");

$A = [[1, 2], [2, 3]];
($eigen, $vecs) = linear_algebra::eigenvalue_decomposition($A);
cmp_float_array($eigen, [-0.23606797749979,  4.23606797749979], "eigen 2x2 matrix");
cmp_float_matrix($vecs, [[0.85065080835204 , 0.525731112119133], [ -0.525731112119133, 0.85065080835204 ]], "eigenvecs 2x2 matrix");

my $err;
my @matrix;

#get_identity_matrix
my $identity = linear_algebra::get_identity_matrix(2);
is_deeply($identity, [ [ 1, 0 ], [ 0, 1 ] ], "identity matrix");

my $matrix = [ [1,2,3,4],[2,2,3,4],[3,3,3,4],[4,4,4,4] ];

is_deeply(linear_algebra::copy_and_reorder_square_matrix($matrix,[1,3,0,2]),[ [2,4,2,3],[4,4,4,4],[2,4,1,3],[3,4,3,3] ] , "copy and reorder");

my $omega = [[1,0.2,0.1,0.1],
			 [0.2,5,0.05,0.2],
			 [0.1,0.05,3,0.3],
			 [0.1,0.2,0.3,4] ];


my ($strings,$inits,$code,$warnings)=linear_algebra::string_cholesky_block(value_matrix=>$omega,
																		   record_index=>0,
																		   theta_count=>1,
																		   bounded_theta => 0,
																		   testing=>1,
																		   fix=>0);

                                                                       my ($SD_A1,$SD_A2,$SD_A3,$SD_A4,$SD_A5);
my ($COR_A21,$COR_A31,$COR_A41,$COR_A51,$COR_A32,$COR_A42,$COR_A52);
my ($COR_A43,$COR_A53,$COR_A54);
my ($CH_A22,$CH_A32,$CH_A42,$CH_A52,$CH_A33,$CH_A43,$CH_A53,$CH_A44,$CH_A54,$CH_A55);



eval(join(' ',@{$inits}));
eval(join(' ',@{$code}));

@matrix=();
for(my $i=0;$i<scalar(@{$strings});$i++){
	push(@matrix,[ (0) x scalar(@{$strings})]);
}
for(my $i=0;$i<scalar(@{$strings});$i++){
	for (my $j=0; $j<=$i; $j++){
		my $value = eval($strings->[$j]->[$i]);
		$matrix[$i]->[$j]=$value;
	}
}

for(my $i=0;$i<scalar(@{$strings});$i++){
	for (my $j=0; $j<=$i; $j++){
		my $sum=0;
		for (my $k=0;$k<scalar(@{$strings});$k++){
			$sum = $sum+$matrix[$i]->[$k]*$matrix[$j]->[$k];
		}
		cmp_relative($sum,$omega->[$i]->[$j],12,"unbounded cholesky product element $i,$j is ".$omega->[$i]->[$j]);
	}
}


($strings,$inits,$code,$warnings)=linear_algebra::string_cholesky_block(value_matrix=>$omega,
																		   record_index=>0,
																		   theta_count=>1,
																		   testing=>1,
																		   fix=>0);

eval(join(' ',@{$inits}));
eval(join(' ',@{$code}));

@matrix=();
for(my $i=0;$i<scalar(@{$strings});$i++){
	push(@matrix,[ (0) x scalar(@{$strings})]);
}
for(my $i=0;$i<scalar(@{$strings});$i++){
	for (my $j=0; $j<=$i; $j++){
		my $value = eval($strings->[$j]->[$i]);
		$matrix[$i]->[$j]=$value;
	}
}

for(my $i=0;$i<scalar(@{$strings});$i++){
	for (my $j=0; $j<=$i; $j++){
		my $sum=0;
		for (my $k=0;$k<scalar(@{$strings});$k++){
			$sum = $sum+$matrix[$i]->[$k]*$matrix[$j]->[$k];
		}
		cmp_float($sum,$omega->[$i]->[$j],"cholesky product element $i,$j is ".$omega->[$i]->[$j]);
	}
#	print join(' ',@{$matrix[$i]})."\n";
}
#extend to 6x
my $omega6 = [
	[1,0.2,0.1,0.1,0.2,-0.2],
	[0.2,5,0.05,0.2,-0.3,0.2],
	[0.1,0.05,3,0.3,0.2,-0.1],
	[0.1,0.2,0.3,4,0.1,0.3],
	[0.2,-0.3,0.2,0.1,4,0.4],
	[-0.2,0.2,-0.1,0.3,0.4,2]
];

($strings,$inits,$code,$warnings)=linear_algebra::string_cholesky_block(
	value_matrix=>$omega6,
	record_index=>2,
	bounded_theta => 0,
	theta_count=>1,
	testing=>0,
	fix=>0);

is($inits->[0],'0.000001 ; log SD_C1','string cholesky init block 1');
is($inits->[1],'0.80471896 ; log SD_C2','string cholesky init block 2');
is($inits->[2],'0.17936477 ; logit (COR_C21+1)/2','string cholesky init block 3');

($strings,$inits,$code,$warnings)=linear_algebra::string_cholesky_block(
	value_matrix=>$omega6,
	record_index=>2,
	bounded_theta => 1,
	theta_count=>1,
	testing=>0,
	fix=>0);

is($inits->[0],'(0,1) ; SD_C1','string cholesky init block 1');
is($inits->[1],'(0,2.236068) ; SD_C2','string cholesky init block 2');
is($inits->[2],'(-1,0.089442719,1) ; COR_C21','string cholesky init block 3');

my $count;
my $etalist;
($count,$code,$etalist)=linear_algebra::eta_cholesky_code(
	stringmatrix=> $strings,
	eta_count=> 5,
	diagonal => 0);

is_deeply($etalist,[6,7,8,9,10,11],'eta_cholesky_code etalist 1');
is($count,6,'eta_cholesky_code count');
my @anscode =(
'ETA_6=ETA(6)*SD_C1',
'ETA_7=ETA(6)*COR_C21*SD_C2+ETA(7)*CH_C22*SD_C2',
'ETA_8=ETA(6)*COR_C31*SD_C3+ETA(7)*CH_C32*SD_C3+ETA(8)*CH_C33*SD_C3',
'ETA_9=ETA(6)*COR_C41*SD_C4+ETA(7)*CH_C42*SD_C4+ETA(8)*CH_C43*SD_C4+ETA(9)*CH_C44*SD_C4',
'ETA_10=ETA(6)*COR_C51*SD_C5+ETA(7)*CH_C52*SD_C5+ETA(8)*CH_C53*SD_C5+ETA(9)*CH_C54*SD_C5+ETA(10)*CH_C55*SD_C5',
'ETA_11=ETA(6)*COR_C61*SD_C6+ETA(7)*CH_C62*SD_C6+ETA(8)*CH_C63*SD_C6+ETA(9)*CH_C64*SD_C6+ETA(10)*CH_C65*SD_C6+ETA(11)*CH_C66*SD_C6'
	);
is_deeply($code,\@anscode,'eta_cholesky code block');


my ($SD_C1,$SD_C2,$SD_C3,$SD_C4,$SD_C5,$SD_C6);
my ($COR_C21,$COR_C31,$COR_C41,$COR_C51,$COR_C61,$COR_C32,$COR_C42,$COR_C52,$COR_C62);
my ($COR_C43,$COR_C53,$COR_C63,$COR_C54,$COR_C64,$COR_C65);
my ($CH_C22,$CH_C32,$CH_C42,$CH_C52,$CH_C62,$CH_C33,$CH_C43,$CH_C53,$CH_C63,$CH_C44,$CH_C54,$CH_C64,$CH_C55,$CH_C65,$CH_C66);

($strings,$inits,$code,$warnings)=linear_algebra::string_cholesky_block(
	value_matrix=>$omega6,
	record_index=>2,
	theta_count=>1,
	testing=>1,
	fix=>0);
eval(join(' ',@{$inits}));
eval(join(' ',@{$code}));

@matrix=();
for(my $i=0;$i<scalar(@{$strings});$i++){
	push(@matrix,[ (0) x scalar(@{$strings})]);
}
for(my $i=0;$i<scalar(@{$strings});$i++){
	for (my $j=0; $j<=$i; $j++){
		my $value = eval($strings->[$j]->[$i]);
		$matrix[$i]->[$j]=$value;
	}
}

for(my $i=0;$i<scalar(@{$strings});$i++){
	for (my $j=0; $j<=$i; $j++){
		my $sum=0;
		for (my $k=0;$k<scalar(@{$strings});$k++){
			$sum = $sum+$matrix[$i]->[$k]*$matrix[$j]->[$k];
		}
		cmp_float($sum,$omega6->[$i]->[$j],"cholesky product element $i,$j is ".$omega6->[$i]->[$j]);
	}
}



($count,$code,$etalist)=linear_algebra::eta_cholesky_code(
	stringmatrix=> ['SD_C1','SD_C2','SD_C3'],
	eta_count=> 5,
	diagonal => 1);

is($count,3,'eta_cholesky code count');
is($code->[0],'ETA_6=ETA(6)*SD_C1','eta_cholesky code diagonal 1');
is($code->[1],'ETA_7=ETA(7)*SD_C2','eta_cholesky code diagonal 2');
is($code->[2],'ETA_8=ETA(8)*SD_C3','eta_cholesky code diagonal 3');
is_deeply($etalist,[6,7,8],'eta_cholesky_code etalist 2');

($count,$code,$etalist)=linear_algebra::eta_cholesky_code(
	stringmatrix=> [undef,undef,'SD_C3'],
	eta_count=> 5,
	diagonal => 1);

is($count,3,'eta_cholesky code count 2');
is($code->[0],'ETA_8=ETA(8)*SD_C3','eta_cholesky code 2 diagonal 3');
is($etalist->[0],8,'eta_cholesky code etalist');

$code = ['CL=THETA(2)*EXP(ETA(2))','V=THETA(3)*(1+ETA(3))*(1+ETA(5))'];
linear_algebra::substitute_etas(code => $code,eta_list => [2,3,5]);
is($code->[0],'CL=THETA(2)*EXP(ETA_2)','substitute etas 1');
is($code->[1],'V=THETA(3)*(1+ETA_3)*(1+ETA_5)','substitute etas 2');
linear_algebra::substitute_etas(code => $code,eta_list => [2,3,5], inverse => 1);
is($code->[0],'CL=THETA(2)*EXP(ETA(2))','inverse substitute etas 1');
is($code->[1],'V=THETA(3)*(1+ETA(3))*(1+ETA(5))','inverse substitute etas 2');

$code = ['CL=THETA(2)*EXP(ETA(2))','V=THETA(3)*(1+ETA(3))*(1+ETA(5))'];
linear_algebra::substitute_etas(code => $code,eta_list => [1,3]);
is($code->[0],'CL=THETA(2)*EXP(ETA(2))','substitute etas 3');
is($code->[1],'V=THETA(3)*(1+ETA_3)*(1+ETA(5))','substitute etas 4');
linear_algebra::substitute_etas(code => $code,eta_list => [1,3], inverse => 1);
is($code->[0],'CL=THETA(2)*EXP(ETA(2))','inverse substitute etas 3');
is($code->[1],'V=THETA(3)*(1+ETA(3))*(1+ETA(5))','inverse substitute etas 4');

$code = ['CL=THETA(2)*EXP(EPS(2))','V=THETA(3)*(1+EPS(3))*(1+EPS(5))'];
linear_algebra::substitute_etas(code => $code,eta_list => [1,2,3], sigma => 1);
is($code->[0],'CL=THETA(2)*EXP(EPS_2)','substitute etas 5');
is($code->[1],'V=THETA(3)*(1+EPS_3)*(1+EPS(5))','substitute etas 6');
linear_algebra::substitute_etas(code => $code,eta_list => [1,2,3], sigma => 1, inverse => 1);
is($code->[0],'CL=THETA(2)*EXP(EPS(2))','inverse substitute etas 5');
is($code->[1],'V=THETA(3)*(1+EPS(3))*(1+EPS(5))','inverse substitute etas 6');

my $hashref = linear_algebra::get_inverse_parameter_list(code => [
													   'SD_A1=THETA(3)',
													   'SD_A2=THETA(4)',
													   'COR_A21=THETA(5)',
													   'SD_A3=THETA(6)',
													   'COR_A31=THETA(7)',
													   'COR_A32=THETA(8)',
													   ';Comments below show CH variables for 1st column, too simple to need new variables',
													   ';CH_A11=1',
													   ';CH_A21=COR_A21',
													   ';CH_A31=COR_A31',
													   'CH_A22=SQRT(1-(COR_A21**2))',
													   'CH_A32=(COR_A32-COR_A21*COR_A31)/CH_A22',
													   'CH_A33=SQRT(1-(COR_A31**2+CH_A32**2))',
													   'ETA_1=ETA(1)*SD_A1',
													   'ETA_2=ETA(1)*COR_A21*SD_A2+ETA(2)*CH_A22*SD_A2',
													   'ETA_3=ETA(1)*COR_A31*SD_A3+ETA(2)*CH_A32*SD_A3+ETA(3)*CH_A33*SD_A3']);

is_deeply($hashref->{'ETA'},[1,2,3],"get inverse parameter list ETA");
is_deeply($hashref->{'EPS'},[],"get inverse parameter list EPS");
is_deeply($hashref->{'THETA'},[3,4,5,6,7,8],"get inverse parameter list THETA");
is_deeply($hashref->{'RECORD'},[0],"get inverse parameter list RECORD");
is($hashref->{'bounded_theta'},1,'get inverse parameter list bounded theta');

$hashref = linear_algebra::get_inverse_parameter_list(code => [
															 'SD_A1=EXP(THETA(3))',
															 'SD_A2=EXP(THETA(4))',
															 'COR_A21=EXP(THETA(5))*2/(EXP(THETA(5))+1) -1',
															 'SD_A3=EXP(THETA(6))',
															 'COR_A31=EXP(THETA(7))*2/(EXP(THETA(7))+1) -1',
															 'COR_A32=EXP(THETA(8))*2/(EXP(THETA(8))+1) -1',
															 ';Comments below show CH variables for 1st column, too simple to need new variables',
															 ';CH_A11=1',
															 ';CH_A21=COR_A21',
															 ';CH_A31=COR_A31',
															 'CH_A22=SQRT(1-(COR_A21**2))',
															 'CH_A32=(COR_A32-COR_A21*COR_A31)/CH_A22',
															 'CH_A33=SQRT(1-(COR_A31**2+CH_A32**2))',
															 'ETA_1=ETA(1)*SD_A1',
															 'ETA_2=ETA(1)*COR_A21*SD_A2+ETA(2)*CH_A22*SD_A2',
															 'ETA_3=ETA(1)*COR_A31*SD_A3+ETA(2)*CH_A32*SD_A3+ETA(3)*CH_A33*SD_A3']);

is_deeply($hashref->{'ETA'},[1,2,3],"unbounded get inverse parameter list ETA");
is_deeply($hashref->{'EPS'},[],"unbounded get inverse parameter list EPS");
is_deeply($hashref->{'THETA'},[3,4,5,6,7,8],"unbounded get inverse parameter list THETA");
is_deeply($hashref->{'RECORD'},[0],"unbounded get inverse parameter list RECORD");
is($hashref->{'bounded_theta'},0,'unbounded get inverse parameter list bounded theta');

$hashref = linear_algebra::get_inverse_parameter_list(code => [
															 'SD_A1=THETA(3)',
															 'SD_A2=EXP(THETA(4))',
															 'COR_A21=EXP(THETA(5))*2/(EXP(THETA(5))+1) -1',
															 'SD_A3=EXP(THETA(6))',
															 'COR_A31=EXP(THETA(7))*2/(EXP(THETA(7))+1) -1',
															 'COR_A32=EXP(THETA(8))*2/(EXP(THETA(8))+1) -1',
															 ';Comments below show CH variables for 1st column, too simple to need new variables',
															 ';CH_A11=1',
															 ';CH_A21=COR_A21',
															 ';CH_A31=COR_A31',
															 'CH_A22=SQRT(1-(COR_A21**2))',
															 'CH_A32=(COR_A32-COR_A21*COR_A31)/CH_A22',
															 'CH_A33=SQRT(1-(COR_A31**2+CH_A32**2))',
															 'ETA_1=ETA(1)*SD_A1',
															 'ETA_2=ETA(1)*COR_A21*SD_A2+ETA(2)*CH_A22*SD_A2',
															 'ETA_3=ETA(1)*COR_A31*SD_A3+ETA(2)*CH_A32*SD_A3+ETA(3)*CH_A33*SD_A3']);

is($hashref->{'bounded_theta'},-1,'mix unbounded get inverse parameter list bounded theta');

($strings,$inits,$code)=linear_algebra::string_cholesky_diagonal(
	value_matrix=>[4,9,16],
	record_index=>3,
	theta_count=>0,
	bounded_theta => 1,
	testing=>0,
	fix_vector=>[1,0,1]);
is($strings->[0],undef,'string cholesky diag 1');
is($strings->[1],'SD_D2','string cholesky diag 2');
is($strings->[2],undef,'string cholesky diag 3');

is($inits->[0],'(0,3) ; SD_D2','string cholesky init diag 2');


is($code->[0],'SD_D2=THETA(1)','string cholesky code diag 2');

($strings,$inits,$code)=linear_algebra::string_cholesky_diagonal(
	value_matrix=>[4,9,16],
	record_index=>3,
	theta_count=>0,
	bounded_theta => 0,
	testing=>0,
	fix_vector=>[1,0,1]);
is($strings->[0],undef,'unbounded string cholesky diag 1');
is($strings->[1],'SD_D2','unbounded string cholesky diag 2');
is($strings->[2],undef,'unbounded string cholesky diag 3');

is($inits->[0],'1.0986123 ; log SD_D2','unbounded string cholesky init diag 2');


is($code->[0],'SD_D2=EXP(THETA(1))','unbounded string cholesky code diag 2');


my @Amatrix =([1,9,1],[2,3,0],[1,2,3],[0,1,0]);

is(linear_algebra::full_rank(\@Amatrix),1,"full rank nice");

my $cov=[];
$err = linear_algebra::row_cov(\@Amatrix,$cov);

cmp_relative($cov->[0]->[0],0.666666666666667,7,"row_cov  1 ");
cmp_relative($cov->[0]->[1],0.666666666666667,7,"row_cov  2 ");
cmp_relative($cov->[0]->[2],0,7,"row_cov  3 ");
cmp_relative($cov->[1]->[1],12.916666666666666,7,"row_cov  4 ");
cmp_relative($cov->[1]->[2],0,7,"row_cov  5 ");
cmp_relative($cov->[2]->[2],2,7,"row_cov  6 ");
is($cov->[0]->[1],$cov->[1]->[0],"row_cov symm 1");
is($cov->[0]->[2],$cov->[2]->[0],"row_cov symm 2");
is($cov->[1]->[2],$cov->[2]->[1],"row_cov symm 3");

my $corr=[];
$err = linear_algebra::covar2sdcorr($cov,$corr);
cmp_relative($corr->[0]->[0],sqrt(0.666666666666667),7,"sd corr  1 ");
cmp_relative($corr->[1]->[1],sqrt(12.916666666666666),7,"sdcorr  2 ");
cmp_relative($corr->[2]->[2],sqrt(2),7,"sdcorr  3 ");
cmp_relative($corr->[0]->[1],0.227184733698826,7,"corr 2");
is($corr->[0]->[1],$corr->[1]->[0],"corr symm 1");
is($corr->[0]->[2],$corr->[2]->[0],"corr symm 2");
is($corr->[1]->[2],$corr->[2]->[1],"corr symm 3");

@Amatrix =([1,9,1],[2,3,0],[1,2,3],[0,1,0]);

$cov=[];
my $median=[];
my $mean=[];
$err = linear_algebra::row_cov_median_mean(\@Amatrix,$cov,$median,$mean,'-99');

cmp_relative($cov->[0]->[0],0.666666666666667,7,"row_cov_median  1 ");
cmp_relative($cov->[0]->[1],0.666666666666667,7,"row_cov_median  2 ");
cmp_relative($cov->[0]->[2],0,7,"row_cov_median  3 ");
cmp_relative($cov->[1]->[1],12.916666666666666,7,"row_cov_median  4 ");
cmp_relative($cov->[1]->[2],0,7,"row_cov_median  5 ");
cmp_relative($cov->[2]->[2],2,7,"row_cov_meidna  6 ");
is_deeply($median,[1,2.5,0.5],'row_cov_median_mean median');
is_deeply($mean,[1,15/4,1],'row_cov_median_mean mean');
is($cov->[0]->[1],$cov->[1]->[0],"row_cov_median symm 1");
is($cov->[0]->[2],$cov->[2]->[0],"row_cov_median symm 2");
is($cov->[1]->[2],$cov->[2]->[1],"row_cov_median symm 3");



@Amatrix =([1,2,3,4,5],
			  [9,3,4,1,1],
			  [1,0,-3,0,1]);

is(linear_algebra::full_rank(\@Amatrix),0,"full rank wide");
my $Rmat=[];
my $err1 = linear_algebra::QR_factorize(\@Amatrix,$Rmat);

cmp_relative($Rmat->[0]->[0],7.416198487095664,8,"qr element 1");
cmp_relative($Rmat->[1]->[0],4.854239009735343,8,"qr element 2");
cmp_relative($Rmat->[1]->[01],9.188926141631764,8,"qr element 3");
cmp_relative($Rmat->[2]->[0],-0.404519917477945,8,"qr element 4");
cmp_relative($Rmat->[2]->[1],-0.003957332533002,8,"qr element 5");
cmp_relative($Rmat->[2]->[2],3.291860868245021,8,"qr element 6");

is($err1,0,"qr factorize ok");
my @singular = ([1,2,3,4,5],
			  [0,1,-5,7,2],
			  [1,3,-2,11,7]);
$Rmat=[];
$err1 = linear_algebra::QR_factorize(\@singular,$Rmat);
cmp_relative($Rmat->[0]->[0],7.416198487095664,8,"singular qr element 1");

is($err1,1,"qr factorize singular");

@A =([1,0,0],[0,-0.1,0],[0,0,1]);


my ($pos,$diff) = linear_algebra::get_symmetric_posdef(matrix => \@A);
is($diff,1,'get_symmetric_posdef one eignevalue too low');
cmp_float_matrix($pos,[[1,0,0],[0,0.0000000001,0],[0,0,1]],'ensure diagonal posdef');

@A =([1,0.1,0.2],[0.1,1,0.3],[0.2,0.3,1]);
($pos,$diff) = linear_algebra::spdarise(matrix=>\@A);

cmp_float_matrix($pos,\@A,'spdarise posdef matrix');
is(abs($diff)<1E-20,1,'spdarise posdef diff');

@Amatrix =([1,2,1,0],[9,3,2,1],[1,0,3,0]);

$cov=[];
$err = linear_algebra::column_cov(\@Amatrix,$cov);

cmp_relative($cov->[0]->[0],0.666666666666667,7,"col_cov  1 ");
cmp_relative($cov->[0]->[1],0.666666666666667,7,"col_cov  2 ");
cmp_relative($cov->[0]->[2],0,7,"col_cov  3 ");
cmp_relative($cov->[1]->[1],12.916666666666666,7,"col_cov  4 ");
cmp_relative($cov->[1]->[2],0,7,"col_cov  5 ");
cmp_relative($cov->[2]->[2],2,7,"col_cov  6 ");
is($cov->[0]->[1],$cov->[1]->[0],"col_cov symm 1");
is($cov->[0]->[2],$cov->[2]->[0],"col_cov symm 2");
is($cov->[1]->[2],$cov->[2]->[1],"col_cov symm 3");

my $covariance = [[2,1,0.5,0.5,0.8],
			   [1,5,0.9,0.7,1],
			   [0.5,0.9,4,0.2,0.3],
			   [0.5,0.7,0.2,3,0.1],
			   [0.8,1,0.3,0.1,5]];

my $ans1;
my $ans2;

my $invcovmat = [
	1.25982E+02,
-1.07192E+02, 1.28773E+02,
-1.83990E+01, 4.17044E+01, 1.86580E+02,
2.60395E+01,-4.62596E+01,-4.33652E+01, 2.50708E+02,
4.74787E+01,-2.77234E+01, 6.88630E+00,-1.12831E+00,  1.82517E+02,
-9.92714E+01, 6.74708E+01,-2.14940E+01,-1.38941E+01, -2.72373E+02, 4.81376E+02,
 2.69450E+01,-1.89793E+01, 2.01481E+01, 1.11231E+01,  1.16686E+02,-2.26952E+02, 1.30232E+02,
 1.92439E+01,-2.08502E+01,-1.59014E+01, 8.05100E+01,   1.10721E+01,-3.83871E+01, 3.89958E+01, 3.72392E+02  ,
 4.87254E-01,-3.85159E-01, 3.02192E+00,-9.68817E-01,-1.02911E-01, -1.33592E+00,1.96125E+00,1.14959E+01,2.52324E+00
	];

($err,$ans1) = linear_algebra::cholesky_of_vector_matrix($invcovmat);


my $matlabanswer=[
[11.224170347958909,0,0,0,0,0,0,0,0],
[-9.550104522379476, 6.129315101349178,0,0,0,0,0,0,0],
[-1.639230288708672, 4.249998405343509,12.877516748781876,0,0,0,0,0,0],
[2.319948752803385,-3.932551439370128,-1.774327522583869,15.056979737107815,0,0,0,0,0],
[4.230040932035026, 2.067756808927676, 0.390785597554319,-0.140589053392896,12.656052181752875,0,0,0,0],
[-8.844430984429268,-2.772649155944083,-1.879889200456328,-0.505718814677402, -18.059659708007011, 8.094653391293978,0,0,0],
[2.400622866963160, 0.643938063723248, 1.657659499712793, 0.732373198287161, 8.269161061298735,-6.313996906225586, 3.539006430146603,0,0],
[1.714505340120703,-0.730341763092609,-0.775536136558838, 4.800715354144464, 0.498404848193361,-1.887337796362435, 4.826732894782685,17.837122703768546,0],
[0.043411137295204, 0.004800193515600, 0.238608118030645,-0.041660589701606,-0.031255335681780,-0.132882327881091, 0.256671499513844, 0.579461518457291, 1.429115976213738]];

is ($err,0,"no error cholesky vector");
for (my $i=0; $i< scalar(@{$ans1}); $i++){
	cmp_float_array($ans1->[$i],$matlabanswer->[$i],"cholesky of vector matrix line $i");
}

cmp_relative(linear_algebra::diagonal_product($ans1),1.232838893716535e+08,8,'diagonal product of cholesky factor');

my $cook;
($err,$cook) = linear_algebra::cook_score_all($ans1,[0,0,0,0,0,0,0,0,0],[1,2,3,4,5,6,7,8,9]);
is ($err,0,"no error cook score");
cmp_relative($cook,1.939403173504674e+02,8,'cook score');

my $det = linear_algebra::sqrt_determinant_vector_covmatrix($invcovmat);
cmp_float($det,1.232838893716538e+08,'sqrt determinant vector matrix');

my $se_par = [2.01673E-01,  1.80612E-01 , 8.08876E-02,  6.94340E-02,  2.33550E-01 , 2.57089E-01 , 2.93394E-01 , 6.04961E-02 , 6.99733E-01  ];

my $orig_est = [3.49590E+00,  6.27891E+00,  1.05572E+00 , 1.52839E+00 ,  8.07614E-01 , 6.74746E-01 , 9.41826E-01,  2.28296E-01, 2.64841E+00];
($err,$cook) = linear_algebra::cook_score_parameters($se_par,[0,0,0,0,0,0,0,0,0],$orig_est);
is ($err,0,"no error cook score parameters");
cmp_float_array($cook,[3.49590E+00/2.01673E-01,  6.27891E+00/1.80612E-01 ,  1.05572E+00/8.08876E-02 , 1.52839E+00/6.94340E-02 ,  8.07614E-01/2.33550E-01 , 6.74746E-01/2.57089E-01 , 9.41826E-01/2.93394E-01,  2.28296E-01/6.04961E-02, 2.64841E+00/6.99733E-01 ],'parameter cook scores');

$cov=[[4,2,1],[2,9,2],[1,2,1]];

my ($modified,$maxcorr,$indices,$cap_indices) = linear_algebra::cap_correlation($cov,undef);
cmp_float_array($cov->[0],[4,2,1],'capped cov 0 1');
cmp_float_array($cov->[1],[2,9,2],'capped cov 0 2');
cmp_float_array($cov->[2],[1,2,1],'capped cov 0 3');
cmp_float($maxcorr,2/3,'maxcorr after mod 0');
is($modified,0,'modified after cap 0');
is_deeply($indices,[2,1],'indices maxcorr 0');
is_deeply($cap_indices,[],'cap indices maxcorr 0');

($modified,$maxcorr,$indices,$cap_indices) = linear_algebra::cap_correlation($cov,0.9);
cmp_float_array($cov->[0],[4,2,1],'capped cov 1 1');
cmp_float_array($cov->[1],[2,9,2],'capped cov 1 2');
cmp_float_array($cov->[2],[1,2,1],'capped cov 1 3');
cmp_float($maxcorr,2/3,'maxcorr after mod 1');
is($modified,0,'modified after cap 1');
is_deeply($indices,[2,1],'indices maxcorr 1');
is_deeply($cap_indices,[],'cap indices maxcorr 1');

($modified,$maxcorr,$indices,$cap_indices) = linear_algebra::cap_correlation($cov,0.5);
cmp_float_array($cov->[0],[4,2,1],'capped cov 2 1');
cmp_float_array($cov->[1],[2,9,1.5],'capped cov 2 2');
cmp_float_array($cov->[2],[1,1.5,1],'capped cov 2 3');
cmp_float($maxcorr,0.5,'maxcorr after mod 2');
is($modified,1,'modified after cap 2');
is_deeply($indices,[2,0],'indices maxcorr 2');
is_deeply($cap_indices->[0],[2,1,2/3],'cap indices maxcorr 2');
is(scalar(@{$cap_indices}),1,'capped indices count');

done_testing();
