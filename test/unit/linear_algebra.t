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

# read_from_file
my $testdir = create_test_dir("linear_algebra_unit");
chdir $testdir;
open my $fh, ">", "testfile";
print $fh "1 ,2, 3,4,5\n";
print $fh "2,3,1,4,  99\n";
close $fh;
my $A = linear_algebra::read_from_file(filename => "testfile");
is_deeply($A, [[1, 2, 3, 4, 5], [2, 3, 1, 4, 99]], "read_from_file comma separated");

open my $fh, ">", "testfile_space";
print $fh "1.23 2e14    5\n";
print $fh "1 2  1.56789\n";
print $fh "1   1 1";
close $fh;
open my $fh, "<", "testfile_space";
my $A = linear_algebra::read_from_file(filehandle => $fh, separator => '\s+');
is_deeply($A, [[1.23, 2e14, 5], [1, 2, 1.56789], [1, 1, 1]], "read_from_file space separated");
close $fh;

remove_test_dir($testdir);

#pad_matrix
my @A = ([1, 2, 4], [3, 5 ,7], [8, 4, 1]);
linear_algebra::pad_matrix(\@A, 5);

is(scalar(@A), 5, "pad_matrix num rows");
is_deeply(@A[0], [1, 2, 4, 0, 0], "pad_matrix row 1");
is_deeply(@A[1], [3, 5, 7, 0, 0], "pad_matrix row 2");
is_deeply(@A[2], [8, 4, 1, 0, 0], "pad_matrix row 3");
is_deeply(@A[3], [0, 0, 0, 1, 0], "pad_matrix row 4");
is_deeply(@A[4], [0, 0, 0, 0, 1], "pad_matrix row 5");

#reduce_matrix
my @A = ([1, 3, 5, 7], [8, 6, 4, 2], [1, 2, 3, 4], [6, 7, 9, 1]);

linear_algebra::reduce_matrix(\@A, 4);
is(scalar(@A), 4, "reduce_matrix don't reduce");
is_deeply(@A[0], [1, 3, 5, 7], "reduce_matrix don't reduce row 0");
is_deeply(@A[3], [6, 7, 9, 1], "reduce_matrix don't reduce row 3");

linear_algebra::reduce_matrix(\@A, 2);

is(scalar(@A), 2, "reduce_matrix num rows");
is_deeply(@A[0], [1, 3], "reduce_matrix row 0");
is_deeply(@A[1], [8, 6], "reduce_matrix row 1");

#put_ones_on_diagonal_of_zero_lines
my @A = ([1, 2, 3, 4], [0, 0, 0, 0], [9, 8, 7, 6], [0, 0, 0, 0]);
linear_algebra::put_ones_on_diagonal_of_zero_lines(\@A);

is_deeply(@A[0], [1, 2, 3, 4], "put_ones_on... row 1");
is_deeply(@A[1], [0, 1, 0, 0], "put_ones_on... row 2");
is_deeply(@A[2], [9, 8, 7, 6], "put_ones_on... row 3");
is_deeply(@A[3], [0, 0, 0, 1], "put_ones_on... row 4");

#triangular_symmetric_to_full
my @A = (1, 5, 3, 7, 8, 2);
my $res = linear_algebra::triangular_symmetric_to_full(\@A);

is_deeply($res->[0], [1, 5, 7], "triangular_symmetric_to_full row 1");
is_deeply($res->[1], [5, 3, 8], "triangular_symmetric_to_full row 2");
is_deeply($res->[2], [7, 8, 2], "triangular_symmetric_to_full row 3");

#Transpose
my @A = ([1, 2, 3], [4, 5, 6], [7, 8, 9]);
linear_algebra::transpose(\@A);

is_deeply(@A[0], [1, 4, 7], "transpose row 1");
is_deeply(@A[1], [2, 5, 8], "transpose row 2");
is_deeply(@A[2], [3, 6, 9], "transpose row 3");

# is_symmetric
my @A = ([1, 2], [2, 3]);
ok(linear_algebra::is_symmetric(\@A), "is_symmetric matrix 1");
@A = ([1, 2], [3, 2]);
ok(!linear_algebra::is_symmetric(\@A), "is_symmetric matrix 2");

#LU_factorization
my $A = [[2, 3], [1, 2]];
linear_algebra::LU_factorization($A);
is_deeply($A, [[2, 3], [0.5, 0.5]], "lu matrix A");

my @B = ([8, 4, 1], [5, 5, 2], [4, 2, 2]);
linear_algebra::LU_factorization(\@B);
is_deeply(\@B, [[8, 4, 1], [0.625, 2.5, 1.375], [0.5, 0, 1.5]], "lu matrix B");

my $C = [[8, 7, 6, 5], [4, 3, 2, 1], [4, 3, 3, 1], [4, 3, 2, 0]];
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


my $A = [[9,8,7], [8, 5, 3], [7, 3, 7]];
(my $eigen, my $vecs) = linear_algebra::eigenvalue_decomposition($A);
cmp_float_array($eigen, [ 19.609194939332617,  -1.903857409809233,   3.294662470476631 ], "eigen simple matrix");
cmp_float_matrix($vecs, [[ 0.706547465982442, -0.684965411383339, -0.177800628576624],
       [ 0.491463745176441,  0.655722706583517, -0.573141447853785],
       [ 0.509169977012525,  0.317569074815606,  0.79993488311851 ]], "eigenvectors simple matrix");

#invert_symmetric
my @matrix = ([3,1,0], [1,5,2], [0,2,4]);
my $result = [];
my $err = linear_algebra::invert_symmetric(\@matrix,$result);

is ($err,0,'invert_symmetric success');
cmp_float($result->[0]->[0], 0.363636363636364, 'invert_symmetric (1,1)');
cmp_float($result->[0]->[1], -0.090909090909091, 'invert_symmetric (1,2)');
cmp_float($result->[0]->[2], 0.045454545454545, 'invert_symmetric (1,3)');
cmp_float($result->[1]->[0], -0.090909090909091, 'invert_symmetric (2,1)');
cmp_float($result->[1]->[1], 0.272727272727273, 'invert_symmetric (2,2)');
cmp_float($result->[1]->[2], -0.136363636363636, 'invert_symmetric (2,3)');
cmp_float($result->[2]->[0], 0.045454545454545, 'invert_symmetric (3,1)');
cmp_float($result->[2]->[1], -0.136363636363636, 'invert_symmetric (3,2)');
cmp_float($result->[2]->[2], 0.318181818181818, 'invert_symmetric (3,3)');

my @matrix = ([3,1,0], [1,5,0], [0,0,0]);
my $result = [];
my $err = linear_algebra::invert_symmetric(\@matrix,$result);

is ($err,1,'invert_symmetric numerr');

#get_identity_matrix
my $identity = linear_algebra::get_identity_matrix(2);
is_deeply($identity, [ [ 1, 0 ], [ 0, 1 ] ], "identity matrix");

my $matrix = [ [1,2,3,4],[2,2,3,4],[3,3,3,4],[4,4,4,4] ]; 

is_deeply(linear_algebra::copy_and_reorder_square_matrix($matrix,[1,3,0,2]),[ [2,4,2,3],[4,4,4,4],[2,4,1,3],[3,4,3,3] ] , "copy and reorder");


done_testing();
