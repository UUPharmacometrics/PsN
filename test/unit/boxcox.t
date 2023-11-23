#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::Random;
use boxcox;
use math qw(usable_number);

my $stat =boxcox::get_quantile_data(5);
is(usable_number($stat->[2]),1,'middle quant usable number is true');
cmp_ok($stat->[2],'==',0,'middle quantile odd number is 0');
cmp_float_array($stat,[-1.2816,-0.52440,0,0.52440,1.2816],'quantile vector N=5');

my $N=6;

$stat =boxcox::get_quantile_data($N);

cmp_float_array($stat,[-1.3830,-0.67449,-0.21043,0.21043,0.67449,1.3830],'quantile vector N=6');



my ($vector,$delta)=boxcox::sort_and_shift_to_positive([3,1,-2,-4,5],1);

is($delta,(4+0.00001),'delta 1');
cmp_float_array($vector,[(-4+4+0.00001),(-2+4+0.00001),(1+4+0.00001),(3+4+0.00001),(5+4+0.00001)],'sorted and shifted 1');

my $tmp = [3,1,-2,-4,5];
my @ans=();
foreach my $val (@{$tmp}){
	push(@ans,(sqrt($val+4+0.00001) - 1)*2);
}
$vector = boxcox::shift_and_box_cox(vector=>$tmp,lambda=>[0.5],delta=>[4+0.00001],inverse => 0);
cmp_float_array($vector,\@ans,'shift_and_box_cox scalar lambda no undef,');

$vector = boxcox::shift_and_box_cox(vector=>$tmp,lambda=>[undef],delta=>[(4+0.00001)], inverse => 0);
cmp_float_array($vector,$tmp,'shift_and_box_cox scalar lambda with undef');

$vector = boxcox::shift_and_box_cox(vector=>[0,0],lambda=>[1,0],delta=>[0.5,2], inverse =>0);
cmp_float_array($vector,[-0.5,log(2)],'shift_and_box_cox array lambda no undef');

$vector = boxcox::shift_and_box_cox(vector=>[0,0],lambda=>[undef,0],delta=>[0.5,2], inverse => 0);
cmp_float_array($vector,[0,log(2)],'shift_and_box_cox array lambda with undef');

$vector = boxcox::shift_and_box_cox(vector=>[ 1,2],lambda=>[0.5],delta=>[4],inverse => 1);
cmp_float_array($vector,[(0.5+1)**2-4,(1+1)**2-4],'inverse shift_and_box_cox scalar lambda no undef,');

$vector = boxcox::shift_and_box_cox(vector=>$tmp,lambda=>[undef],delta=>[(4+0.00001)], inverse => 1);
cmp_float_array($vector,$tmp,'inverse shift_and_box_cox scalar lambda with undef');

$vector = boxcox::shift_and_box_cox(vector=>[-0.5,log(2)],lambda=>[1,0],delta=>[0.5,2], inverse =>1);
cmp_float_array($vector,[0,0],'inverse shift_and_box_cox array lambda no undef');

$vector = boxcox::shift_and_box_cox(vector=>[0,log(2)],lambda=>[undef,0],delta=>[0.5,2], inverse => 1);
cmp_float_array($vector,[0,0],'inverse shift_and_box_cox array lambda with undef');



($vector,$delta)=boxcox::sort_and_shift_to_positive([1,3,0.5,0.2],1);

is($delta,0,'delta 2');
is_deeply($vector,[0.2,0.5,1,3],'sorted and shifted 2');

($vector,$delta)=boxcox::sort_and_shift_to_positive([1,3,0.5,0.2],0);

is($delta,(0.00001),'delta 3');

my $transformed = boxcox::box_cox([1,2,3],0);
cmp_float_array($transformed,[log(1),log(2),log(3)],'boxcox lambda=0');

$transformed = boxcox::box_cox([1,2,3],1);
cmp_float_array($transformed,[0,1,2],'boxcox lambda=1');

$transformed = boxcox::box_cox([1,2,3],-1);
cmp_float_array($transformed,[-0.00000000e+00,0.5,2/3],'boxcox lambda=-1');

my @X=(1,3,5,5,6);
my @Y=(-5,-3,1,3,4);
my ($r,$dirt)=boxcox::compute_r(\@X,\@Y,undef,undef);
cmp_float($r,30/sqrt(960),'compute r onlinestat');


my @CL=(0.00477584,0.00509069,0.00455956,0.00772444,0.00401326,0.00487732,
0.0041124,0.00816226,0.00570479,0.00415326,0.00594502,0.00594599,0.00338406,
0.0046461,0.00387114,0.00772444,0.00460241,0.00487732,0.00408041,0.00371401,
0.00570721,0.00487732,0.00545891,0.00460241,0.00531667,0.00642602,0.00509069,
0.00816226,0.00518047,0.00760107,0.00375295,0.00772444,0.00394836,0.00652462,
0.00487732,0.0044912,0.00408041,0.00524685,0.0054255,0.00650025,0.00401794,
0.00389643,0.00520604,0.00389643,0.00415326,0.00477584,0.00594502,
0.00570479,0.00344748,0.00477584,0.00488036,0.00454498,0.0148183,
0.00625872,0.00487732,0.00320742,0.00455492,0.00643822,0.00652462,
0.00454498,0.00518047,0.00479637,0.00429594,0.00570479,0.00401794,
0.00389643,0.00537437,0.00434611,0.00520604,0.00401794,0.00460241,
0.00672449,0.00401794,0.00570479,0.00570479,0.00384698,0.00389643,
0.0046461,0.00567039,0.00425725,0.00460241,0.00344748,0.00460241,
0.00400316,0.00442741,0.00400316,0.00645188,0.00518431,0.00518047,
0.00550419,0.00389643,0.00429594,0.00518431,0.00408041,0.00429594,
0.00477584,0.00410182,0.00520604,0.00590278,0.00420474,0.00518431);


my @V=(1.55158,1.3027,1.61882,1.13856,1.37955,1.59477,1.62951,1.59514,1.09566,
1.62653,1.54518,1.48253,1.4596,1.01375,1.61882,1.5507,1.37955,1.32609,1.26224,
1.68434,1.37955,1.59315,1.5507,1.46354,1.62306,1.55158,1.62951,1.54972,1.75891,
1.32016,1.61882,1.5534,1.63222,1.37955,1.34523,1.32609,1.41622,1.42449,1.8148,
1.4783,1.35865,1.50535,1.35865,1.09566,1.4176,1.62653,1.59514,1.25811,1.4176,
1.31751,1.34486,5.91971,1.58522,1.37955,1.25575,1.26766,1.70328,1.63222,1.34486,
1.54972,1.36724,1.22086,1.59514,1.4783,1.35865,1.55929,1.15728,1.50535,1.4783,1.5507,
1.59044,1.4783,1.59514,1.59514,1.12073,1.35865,1.4596,1.55131,1.51405,1.5507,1.25811,
1.5507,0.993295,1.33709,0.993295,1.54585,1.46755,1.54972,1.50989,1.35865,1.22086,
1.46755,1.32609,1.22086,1.4176,1.15058,1.50535,1.58179,1.25677,	1.46755);

my @flat=(0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9);

my $lambda;
my $numerr;
($lambda,$delta,$numerr)=boxcox::get_lambda_delta(\@flat,3,1);
is($numerr,0,"numerical error get_lambda_delta");

random_set_seed_from_phrase('12345');

$N=100;
my @normal=random_normal($N,0,0.2); #n mean sd

foreach my $lambda (-2,-0.5,-0.2,-0.1,0,0.2,1.5){
	my $vector = boxcox::inverse_box_cox(\@normal,$lambda);

	my ($parvec,$delt)= boxcox::sort_and_shift_to_positive($vector,10);
	my $yvec =boxcox::get_quantile_data($N);
	my ($lambda_r,$dirt,$numerr1) = boxcox::r_of_lambda($parvec,$yvec,$lambda);

	my ($found,$delta,$numerr2)=boxcox::get_lambda_delta($vector,3,1);
	my ($own_r,$numerr3);
	($own_r,$dirt,$numerr3) = boxcox::r_of_lambda($parvec,$yvec,$found);

	cmp_ok($own_r,'>=',$lambda_r,"get lambda $lambda");
	cmp_ok($numerr1,'==',$numerr2,"numerr $lambda");
	cmp_ok($numerr3,'==',$numerr2,"numerr2 $lambda");
}

my %extra;
$extra{'type'}='second_degree';
$extra{'a'}=-1;
$extra{'b'}=-2;
$extra{'c'}=15;
my $x;
($x,$numerr) = boxcox::secant_method_maximize(10,0.005,30,[15,0,-2],\%extra);

cmp_float($x,-1,"second order 1");
cmp_ok($numerr,'==',0,"numerr second degree");

$extra{'a'}=-1;
$extra{'b'}=3;
$extra{'c'}=-1;
($x,$numerr) = boxcox::secant_method_maximize(10,0.005,30,[-1,0,3],\%extra);

cmp_float($x,1.5,"second order 2");
cmp_ok($numerr,'==',0,"numerr second degree 2");

my $arr =[];

boxcox::insert_sort_ascending($arr,[3,0],0);

is_deeply($arr->[0],[3,0],"insert sort 1");

boxcox::insert_sort_ascending($arr,[2,0],0);
is_deeply($arr->[0],[2,0],"insert sort 2");
is_deeply($arr->[1],[3,0],"insert sort 3");

boxcox::insert_sort_ascending($arr,[4,0],0);
is_deeply($arr->[0],[2,0],"insert sort 4");
is_deeply($arr->[1],[3,0],"insert sort 5");
is_deeply($arr->[2],[4,0],"insert sort 6");

boxcox::insert_sort_ascending($arr,[3.5,0],0);
is_deeply($arr->[0],[2,0],"insert sort 7");
is_deeply($arr->[1],[3,0],"insert sort 8");
is_deeply($arr->[2],[3.5,0],"insert sort 9");
is_deeply($arr->[3],[4,0],"insert sort 10");

my $matrix = [[],$arr,[[1,0],[5,0]]];

my ($small,$best) = boxcox::get_smallest_best_index($matrix);
is($small,1,'get smallest index 1');
is($best,2,'get  best index 1');

$matrix = [[[1,0],[5,0]],$arr,[[1,0],[2,0]]];

($small,$best) = boxcox::get_smallest_best_index($matrix);
is($small,0,'get smallest index 2');
is($best,0,'get  best index 2');

$matrix = [[[1,0],[2,0]],$arr];

($small,$best) = boxcox::get_smallest_best_index($matrix);
is($small,0,'get smallest index 3');
is($best,1,'get  best index 3');

%extra=();
$extra{'type'}='second_degree';
$extra{'a'}=-1;
$extra{'b'}=2;
$extra{'c'}=3;

$matrix = [[[3,0]],[],[],[],[]];
($small,$best) = boxcox::get_smallest_best_index($matrix);
is($small,0,'get smallest index 4');
is($best,0,'get  best index 4');

boxcox::make_splits($matrix,[0],3,\%extra);
is_deeply($matrix->[1]->[0],[0,-1],"after split 1a");
is_deeply($matrix->[1]->[1],[3,0],"after split 1b");
is_deeply($matrix->[1]->[2],[4,1],"after split 1c");

($small,$best) = boxcox::get_smallest_best_index($matrix);
is($small,1,'get smallest index 5');
is($best,1,'get  best index 5');

boxcox::make_splits($matrix,[1],3,\%extra);
is_deeply($matrix->[1]->[0],[0,-1],"after split 2a");
is_deeply($matrix->[1]->[1],[3,0],"after split 2b");
is_deeply($matrix->[2]->[2],[4,1],"after split 2c");
cmp_float($matrix->[2]->[0]->[1],(1+1/3),"after split 2d");
cmp_float($matrix->[2]->[1]->[1],(1-1/3),"after split 2e");

is((boxcox::turn([20,0.3],[19,1],[0.1,3]) > 0),1," convex ");
is((boxcox::turn([20,0.3],[1,1],[0.7,3]) < 0),1," concave ");
is((boxcox::turn([2,1],[3,2],[4,3]) == 0),1," straight ");

%extra=();
$extra{'type'}='second_degree';
$extra{'a'}=-7;
$extra{'b'}=-8;
$extra{'c'}=10;

my $opt = boxcox::direct_search_maximize(1.5,6,30,\%extra);
cmp_relative($opt->[0],10+16/7,6,"direct 2nd y");
cmp_relative($opt->[1],-4/7,1,"direct 2nd x");

%extra=();
$extra{'type'}='second_degree';
$extra{'a'}=-1;
$extra{'b'}=2;
$extra{'c'}=5;

$opt = boxcox::direct_search_maximize(4.5,6,20,\%extra);
cmp_float($opt->[0],6,"direct 2nd y");
cmp_float($opt->[1],1,"direct 2nd x");

#Joao very skewed
open( RRES, $includes::testfiledir . '/boxcoxtestvectors.csv') or die "could not open";
my @read_file = <RRES>;
close( RRES );

my $index=0;
foreach my $line (@read_file){
	$index++;
	chomp $line;
	my @vals=split(',',$line);
	my $est = shift(@vals);
	my ($lam,$del,$numerr) = boxcox::get_lambda_delta(\@vals,3,$est);
}

done_testing();
