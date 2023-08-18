#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
#use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::MatrixReal qw(all); 
use Math::Trig;	# For pi
use Math::Random;
use output;
use tool::sir;




#Tests needed for SIR

# sub get_user_covmatrix (from csv file, verify square and symmetric)


#sub resample

my $dir = $includes::testfiledir . "/";
my $file = 'matrixreal.lst';
my $output = output->new(filename => $dir . $file);

my $icm = tool::sir::get_nonmem_inverse_covmatrix(output => $output);


my $hash = output::get_nonmem_parameters(output => $output);
my $thetas = $hash->{'values'};


#TODO test $hash->{'lower_bounds'} $hash->{'upper_bounds'} here

	#no static methods, must have an object for these methods
my $mat = new Math::MatrixReal(1,1);
my $mu = $mat->new_from_rows( [$thetas] );

#print $mu;


my $nsamples=3;

my $covar = tool::sir::get_nonmem_covmatrix(output => $output);


random_set_seed_from_phrase("hej pa dig");
my ($gotsamples,$dirt) = tool::sir::sample_multivariate_normal(samples=>$nsamples,
															   print_summary => 0,
															   check_cholesky_reparameterization => 0,
															   fix_theta_labels => [],
															   fix_theta_values => [],
															   labels => [],
															   covmatrix => $covar,
															   lower_bound => $hash->{'lower_bounds'},
															   upper_bound => $hash->{'upper_bounds'},
															   param => $hash->{'param'},
															   coords => $hash->{'coords'},
															   inflation => [],
															   block_number => $hash->{'block_number'},
															   choleskyform => $hash->{'choleskyform'},
															   mu => $mu,
															   adjust_blocks => 0
	);


#print "\nxvec [".join(' ',@{$gotsamples->[2]})."]\n";
#print "\labels ".join(' ',@{$hash->{'labels'}})."\n";



my $pdf=tool::sir::mvnpdf(inverse_covmatrix => $icm,
						  mu => $mu,
						  xvec_array => $gotsamples,
						  inflation => 1,
						  relative => 0);
my $matlab_mvnpdf=4.622416072199147e+05; #mvnpdf function
cmp_ok(abs($pdf->[0]-$matlab_mvnpdf),'<',1e-7,'pdf diff to matlab');
#print "\npdf ".$pdf->[0]."\n";


my $wghash = tool::sir::compute_weights(pdf_array => $pdf,
										dofv_array => [1,10,5]);

cmp_ok(abs($wghash->{'weights'}->[0]-1.312150724294432e-06),'<',0.000000000001e-06,'weight 1');
cmp_ok(abs($wghash->{'weights'}->[1]-5.059816747224142e-09),'<',0.000000000001e-09,'weight 2');
cmp_ok(abs($wghash->{'weights'}->[2]-3.077141488472004e-08),'<',0.000000000001e-08,'weight 3');

cmp_ok(abs($wghash->{'cdf'}->[0]-1.312150724294432e-06),'<',0.000000000001e-06,'cdf 1');
cmp_ok(abs($wghash->{'cdf'}->[1]-1.317210541041656e-06),'<',0.000000000001e-06,'cdf 2');
cmp_ok(abs($wghash->{'cdf'}->[2]-1.347981955926376e-06),'<',0.000000000001e-06,'cdf 3');


tool::sir::recompute_weights(weight_hash => $wghash,
							 reset_index => 1);

cmp_ok(abs($wghash->{'weights'}->[0]-1.312150724294432e-06),'<',0.000000000001e-06,'weight 1 recompute');
cmp_ok($wghash->{'weights'}->[1],'==',0,'weight 2 recompute');
cmp_ok(abs($wghash->{'weights'}->[2]-3.077141488472004e-08),'<',0.000000000001e-08,'weight 3 recompute');

cmp_ok(abs($wghash->{'cdf'}->[0]-1.312150724294432e-06),'<',0.000000000001e-06,'cdf 1 recompute');
cmp_ok(abs($wghash->{'cdf'}->[1]-1.312150724294432e-06),'<',0.000000000001e-06,'cdf 2 recompute');
cmp_ok(abs($wghash->{'cdf'}->[2]-1.342922139179152e-06),'<',0.000000000001e-06,'cdf 3 recompute');
cmp_ok(abs($wghash->{'sum_weights'}-1.342922139179152e-06),'<',0.000000000001e-06,'sum weights 3 recompute');


tool::sir::recompute_weights(weight_hash => $wghash,
							 reset_index => 0);

cmp_ok($wghash->{'weights'}->[0],'==',0,'weight 1 recompute');
cmp_ok($wghash->{'weights'}->[1],'==',0,'weight 2 recompute');
cmp_ok(abs($wghash->{'weights'}->[2]-3.077141488472004e-08),'<',0.000000000001e-08,'weight 3 recompute');

cmp_ok($wghash->{'cdf'}->[0],'==',0,'cdf 1 recompute');
cmp_ok($wghash->{'cdf'}->[1],'==',0,'cdf 2 recompute');
cmp_ok(abs($wghash->{'cdf'}->[2]-3.077141488472004e-08),'<',0.000000000001e-08,'cdf 3 recompute');
cmp_ok(abs($wghash->{'sum_weights'}-3.077141488472004e-08),'<',0.000000000001e-08,'sum weights 3 recompute');

#start over
$wghash = tool::sir::compute_weights(pdf_array => $pdf,
									 dofv_array => [9.8,8.1,9.5]);

my @times_sampled = (0) x $nsamples;

my $sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
$times_sampled[$sample_index]++;
#print "times sampled ".join(' ',@times_sampled)."\n";
tool::sir::recompute_weights(weight_hash => $wghash,
							 reset_index => $sample_index);
$sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
$times_sampled[$sample_index]++;
#print "times sampled ".join(' ',@times_sampled)."\n";

tool::sir::recompute_weights(weight_hash => $wghash,
							 reset_index => $sample_index);
$sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
$times_sampled[$sample_index]++;
#print "times sampled ".join(' ',@times_sampled)."\n";


#start over
$wghash = tool::sir::compute_weights(pdf_array => $pdf,
									 dofv_array => [9.8,8.1,9.5]);

@times_sampled = (0) x $nsamples;

for (my $k=0; $k< 100; $k++){
	my $sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
	$times_sampled[$sample_index]++;
}
#print "times sampled ".join(' ',@times_sampled)."\n";
cmp_ok($times_sampled[0],'==',39,'times sampled 0');
cmp_ok($times_sampled[1],'==',46,'times sampled 1');
cmp_ok($times_sampled[2],'==',15,'times sampled 2');



$dir = $includes::testfiledir . "/";
$file = 'mox_sir.lst';
$output= output->new (filename => $dir . $file);



#my $nsamples=3;

$covar = tool::sir::get_nonmem_covmatrix(output => $output);


my $inflated=tool::sir::inflate_covmatrix(matrix => $covar,
										  inflation => [2,2,2,2,2,2,2,2]);


cmp_ok($covar->[0]->[0],'==',6.10693E+00,'covar element 1,1 after inflation not changed');
cmp_ok($covar->[1]->[5],'==',1.18743E-02,'covar element 2,6 after inflation not changed');
cmp_ok($covar->[2]->[2],'==',3.75907E-04,'covar element 3,3 after inflation not changed');
cmp_ok($covar->[3]->[1],'==',-4.02777E-02,'covar element 4,2 after inflation not changed');
cmp_ok($covar->[4]->[7],'==',6.19395E-05,'covar element 5,8 after inflation not changed');
cmp_ok($covar->[7]->[0],'==',1.53110E-02,'covar element 8,1 after inflation not changed');
cmp_ok($covar->[6]->[5],'==',7.25938E-03,'covar element 7,6 after inflation not changed');
cmp_ok($covar->[7]->[7],'==',1.69362E-03,'covar element 8,8 after inflation not changed');
cmp_ok($covar->[6]->[3],'==',2.75131E-03,'covar element 7,4 after inflation not changed');
cmp_ok($covar->[4]->[6],'==',-3.05686E-04,'covar element 5,7 after inflation not changed');



$nsamples=3;


$icm = Math::MatrixReal->new_from_rows([[3,1,0.1],[1,5,0.3],[0.1,0.3,3]]);



$mu = Math::MatrixReal->new_from_rows([[1,2,3]]);

$pdf=tool::sir::mvnpdf(inverse_covmatrix => $icm,
						  mu => $mu,
						  xvec_array => [[3,0,1]],
						  inflation => 1,
					   relative => 0);

cmp_float($pdf->[0],2.807179347140791e-09,'mvnpdf well conditioned no inflation');

$pdf=tool::sir::mvnpdf(inverse_covmatrix => $icm,
						  mu => $mu,
						  xvec_array => [[3,0,1]],
						  inflation => 3,
					   relative => 0);

cmp_float($pdf->[0],1.498807247585522e-04,'mvnpdf well conditioned with inflation 3');


done_testing();
