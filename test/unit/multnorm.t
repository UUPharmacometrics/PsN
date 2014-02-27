#!/usr/bin/perl

use strict;
use warnings;
#use Test::More tests => 57;
use Test::More;
use Test::Exception;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages
use ext::Math::MatrixReal qw(all); 
use Math::Trig;	# For pi
use Math::Random;
use output;
use tool::sir;

# sub get_user_covmatrix (from csv file, verify square and symmetric)

# sub sample_multivariate_normal in is covmatrix as array of refs, mu, number of samples, array of lower_bound, array of upper_bound. Out
#     is array of refs to MatrixReal xvecs. Note: if inflation is used, that is done outside sample function, manipulation of covmatrix 


my $dir='/home/kajsa/kod-psn/3PsN/devel/';
my $file='matrixreal.lst';
my $output= output -> new (filename => $dir.$file);

my $icm = tool::sir::get_nonmem_inverse_covmatrix(output => $output);

cmp_ok($icm->element(1,1),'==',8.46492E+06,'inverse element 1,1');
cmp_ok($icm->element(3,1),'==',9.12148E+03,'inverse element 3,1');
cmp_ok($icm->element(1,3),'==',9.12148E+03,'inverse element 1,3');
cmp_ok($icm->element(1,5),'==',-2.86424E+04,'inverse element 1,5');
cmp_ok($icm->element(4,2),'==',-1.55874E+02,'inverse element 4,2');
cmp_ok($icm->element(5,3),'==',-1.03606E+02,'inverse element 5,3');
cmp_ok($icm->element(5,5),'==',5.96396E+03,'inverse element 5,5');

my $thetas = tool::sir::get_nonmem_parameters(output => $output);
	#no static methods, must have an object for these methods
my $mat = new Math::MatrixReal(1,1);
my $mu = $mat->new_from_rows( [$thetas] );

#print $mu;


cmp_ok($mu->element(1,1),'==',5.55363E-03,'mu 1,1');
cmp_ok($mu->element(1,2),'==',1.33638E+00,'mu 1,2');
cmp_ok($mu->element(1,3),'==',4.97064E-01,'mu 1,3');
cmp_ok($mu->element(1,4),'==',3.76272E-01,'mu 1,4');
cmp_ok($mu->element(1,5),'==',1.28122E-01,'mu 1,5');

my $xvec = $icm->new_from_rows( [[0.006,1,0.5,0.4,0.1]] );
#print $xvec;

my $diff=$mu->shadow(); #zeros matrix same size as $mu
$diff->subtract($xvec,$mu); #now $diff is $xvec - $mu
#print $diff;

my $matlab_invdeterminant =1.952310799901186e+17;
my $invdeterminant = $icm->det();
cmp_ok(abs($invdeterminant-$matlab_invdeterminant),'<',420,'invdeterminant diff to matlab'); #relative diff is 2e-15

my $determinant=1/$invdeterminant;
my $matlab_determinant =5.122135266836687e-18;
cmp_ok(abs($determinant-$matlab_determinant),'<',1e-31,'determinant diff to matlab');

my $k=5;

my $product_left = $diff->multiply($icm);
my $product=$product_left->multiply(~$diff); #~ is transpose
my $exponent=-0.5 * $product->element(1,1);

my $matlab_exponent=-15.028676658713202;
cmp_ok(abs($exponent-$matlab_exponent),'<',2e-14,'exponent diff to matlab');

#print "\nexponent $exponent\n";
my $base=tool::sir::get_determinant_factor(inverse_covmatrix => $icm,
										   k => $k);

#print "\nbase $base\n";
my $matlab_base = 4.465034382516543e+06;
cmp_ok(abs($base-$matlab_base),'<',0.00000001,'base diff to matlab');

my $pdf=tool::sir::mvnpdf(inverse_covmatrix => $icm,
						  mu => $mu,
						  xvec_array => [$xvec]);
#print "\npdf $pdf\n";

my $matlab_mvnpdf=1.327252231799129; #mvnpdf function
cmp_ok(abs($pdf->[0]-$matlab_mvnpdf),'<',0.0000000000001,'pdf diff to matlab');

my $nsamples=2;

my $covar = tool::sir::get_nonmem_covmatrix(output => $output);

cmp_ok($covar->[0]->[0],'==',1.55838E-07,'covar element 1,1');
cmp_ok($covar->[1]->[1],'==',6.38430E-03,'covar element 2,2');
cmp_ok($covar->[1]->[2],'==',-1.94326E-03,'covar element 2,3');
cmp_ok($covar->[2]->[1],'==',-1.94326E-03,'covar element 3,2');
cmp_ok($covar->[3]->[2],'==',-1.32639E-03,'covar element 4,3');
cmp_ok($covar->[4]->[4],'==',eval(1.75502E-04),'covar element 5,5');


my @samples = Math::Random::random_multivariate_normal($nsamples, @{$thetas}, @{$covar});

#    When called in an array context, returns an array of $n deviates (each deviate being an array reference) 
#generated from the multivariate normal distribution with mean vector @mean and variance-covariance matrix @covar. 

done_testing();
