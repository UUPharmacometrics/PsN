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

# subs in sir.pm
# sub get_nonmem_covmatrix (from output, make square)
# sub get_user_covmatrix (from csv file, verify square and symmetric)
# sub mvnpdf in is inverse_covm as matrixreal, mu, and array of refs to xvecs. Out is array of pdf-scalars
# sub sample_multivariate_normal in is covmatrix as array of refs, mu, number of samples, array of lower_bound, array of upper_bound. Out
#     is array of refs to xvecs. Note: if inflation is used, that is done outside sample function 
# sub compute_weights in is array of dOFV and pdf-scalars. Out is array of weights


sub make_square {
	my $m_ref = shift;
	my @matrix = @{$m_ref};
	# Make the matrix square:
	my $elements = scalar @matrix; # = M*(M+1)/2
	my $M = -0.5 + sqrt( 0.25 + 2 * $elements );
	my @square;
	for ( my $m = 1; $m <= $M; $m++ ) {
	    for ( my $n = 1; $n <= $m; $n++ ) {
			push( @{$square[$m-1]}, $matrix[($m-1)*$m/2 + $n - 1] );
			unless ( $m == $n ) {
				push( @{$square[$n-1]}, $matrix[($m-1)*$m/2 + $n - 1] );
			}
	    }
	}
	return \@square;
}

my $dir='/home/kajsa/kod-psn/3PsN/devel/';
my $file='matrixreal.lst';
my $output= output -> new (filename => $dir.$file);
unless( $output -> parsed_successfully ){
	print "Unable to read everything from outputfile, parser error message:\n";
	print $output -> parsing_error_message();
}
my $icm  = $output -> get_single_value(attribute => 'inverse_covariance_matrix');
#print $icm;															 

cmp_ok($icm->element(1,1),'==',8.46492E+06,'inverse element 1,1');
cmp_ok($icm->element(3,1),'==',9.12148E+03,'inverse element 3,1');
cmp_ok($icm->element(1,3),'==',9.12148E+03,'inverse element 1,3');
cmp_ok($icm->element(1,5),'==',-2.86424E+04,'inverse element 1,5');
cmp_ok($icm->element(4,2),'==',-1.55874E+02,'inverse element 4,2');
cmp_ok($icm->element(5,3),'==',-1.03606E+02,'inverse element 5,3');
cmp_ok($icm->element(5,5),'==',5.96396E+03,'inverse element 5,5');

my $thetas = $output -> get_single_value(attribute => 'thetas');
#print join(' ',@{$thetas})."\n";

#matrixreal.mod
#no static methods, must have an object for these methods
my $mu = $icm->new_from_rows( [$thetas] );
#print $mu;
my $xvec = $icm->new_from_rows( [[0.006,1,0.5,0.4,0.1]] );
#print $xvec;

my $diff=$mu->shadow(); #zeros matrix same size as $mu
$diff->subtract($xvec,$mu); #now $diff is $xvec - $mu
#print $diff;

cmp_ok($mu->element(1,1),'==',5.55363E-03,'mu 1,1');
cmp_ok($mu->element(1,2),'==',1.33638E+00,'mu 1,2');
cmp_ok($mu->element(1,3),'==',4.97064E-01,'mu 1,3');
cmp_ok($mu->element(1,4),'==',3.76272E-01,'mu 1,4');
cmp_ok($mu->element(1,5),'==',1.28122E-01,'mu 1,5');

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
my $base=sqrt($invdeterminant)/((2* pi)**($k/2));
#print "\nbase $base\n";
my $matlab_base = 4.465034382516543e+06;
cmp_ok(abs($base-$matlab_base),'<',0.00000001,'base diff to matlab');

my $pdf=$base*exp($exponent);
#print "\npdf $pdf\n";

my $matlab_mvnpdf=1.327252231799129; #mvnpdf function
cmp_ok(abs($pdf-$matlab_mvnpdf),'<',0.0000000000001,'pdf diff to matlab');

my $nsamples=2;

my $lower_covar  = $output -> get_single_value(attribute => 'covariance_matrix');

my $covar = make_square( $lower_covar);
#if (scalar(@{$covar}) > 0) {
#	$self->covariance_matrix(Math::MatrixReal->new_from_cols($covar));
#}

#foreach my $ref (@{$covar}){
#	print join(' ',@{$ref})."\n";
#}

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
