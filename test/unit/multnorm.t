#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::MatrixReal;
use Math::Trig;	# For pi
use random;
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


my $pdf=tool::sir::mvnpdf(inverse_covmatrix => $icm,
						  mu => $mu,
						  xvec_array => $gotsamples,
						  inflation => 1,
						  relative => 0);

my $scipy_mvnpdf=523944.31635165535; #scipy function
cmp_ok(abs($pdf->[0] - $scipy_mvnpdf), '<', 1e-7, 'pdf diff to matlab');

my $wghash = tool::sir::compute_weights(pdf_array => $pdf,
										dofv_array => [1,10,5]);

cmp_ok(abs($wghash->{'weights'}->[0] - 1.15762427567884e-06), '<', 0.000000000001e-06, 'weight 1');
cmp_ok(abs($wghash->{'weights'}->[1] - 2.03757894529081e-08), '<', 0.000000000001e-09, 'weight 2');
cmp_ok(abs($wghash->{'weights'}->[2] - 5.59158025762047e-07), '<', 0.000000000001e-08, 'weight 3');

cmp_ok(abs($wghash->{'cdf'}->[0] - 1.15762427567884e-06), '<', 0.000000000001e-06, 'cdf 1');
cmp_ok(abs($wghash->{'cdf'}->[1] - 1.17800006513174e-06), '<', 0.000000000001e-06, 'cdf 2');
cmp_ok(abs($wghash->{'cdf'}->[2] - 1.73715809089379e-06), '<', 0.000000000001e-06, 'cdf 3');


tool::sir::recompute_weights(weight_hash => $wghash,
							 reset_index => 1);

cmp_ok(abs($wghash->{'weights'}->[0] - 1.15762427567884e-06), '<', 0.000000000001e-06, 'weight 1 recompute');
cmp_ok($wghash->{'weights'}->[1],'==',0,'weight 2 recompute');
cmp_ok(abs($wghash->{'weights'}->[2] - 5.59158025762047e-07), '<', 0.000000000001e-09, 'weight 3 recompute');

cmp_ok(abs($wghash->{'cdf'}->[0]-1.15762427567884e-06),'<',0.000000000001e-06,'cdf 1 recompute');
cmp_ok(abs($wghash->{'cdf'}->[1]-1.15762427567884e-06),'<',0.000000000001e-06,'cdf 2 recompute');
cmp_ok(abs($wghash->{'cdf'}->[2]-1.71678230144088e-06),'<',0.000000000001e-06,'cdf 3 recompute');
cmp_ok(abs($wghash->{'sum_weights'}-1.71678230144088e-06),'<',0.000000000001e-06,'sum weights 3 recompute');


tool::sir::recompute_weights(weight_hash => $wghash,
							 reset_index => 0);

cmp_ok($wghash->{'weights'}->[0],'==',0,'weight 1 recompute');
cmp_ok($wghash->{'weights'}->[1],'==',0,'weight 2 recompute');
cmp_ok(abs($wghash->{'weights'}->[2] - 5.59158025762047e-07),'<',0.000000000001e-08,'weight 3 recompute');

cmp_ok($wghash->{'cdf'}->[0],'==',0,'cdf 1 recompute');
cmp_ok($wghash->{'cdf'}->[1],'==',0,'cdf 2 recompute');
cmp_ok(abs($wghash->{'cdf'}->[2] - 5.59158025762047e-07),'<',0.000000000001e-08,'cdf 3 recompute');
cmp_ok(abs($wghash->{'sum_weights'} - 5.59158025762047e-07),'<',0.000000000001e-08,'sum weights 3 recompute');

#start over
$wghash = tool::sir::compute_weights(pdf_array => $pdf,
									 dofv_array => [9.8,8.1,9.5]);

my @times_sampled = (0) x $nsamples;

my $sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
$times_sampled[$sample_index]++;

tool::sir::recompute_weights(weight_hash => $wghash,
							 reset_index => $sample_index);
$sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
$times_sampled[$sample_index]++;

tool::sir::recompute_weights(weight_hash => $wghash,
							 reset_index => $sample_index);
$sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
$times_sampled[$sample_index]++;

#start over
$wghash = tool::sir::compute_weights(pdf_array => $pdf,
									 dofv_array => [9.8,8.1,9.5]);

@times_sampled = (0) x $nsamples;

for (my $k=0; $k< 100; $k++){
	my $sample_index = tool::sir::weighted_sample(cdf => $wghash->{'cdf'});
	$times_sampled[$sample_index]++;
}

cmp_ok($times_sampled[0],'==',11,'times sampled 0');
cmp_ok($times_sampled[1],'==',44,'times sampled 1');
cmp_ok($times_sampled[2],'==',45,'times sampled 2');



$dir = $includes::testfiledir . "/";
$file = 'mox_sir.lst';
$output= output->new (filename => $dir . $file);
my $inflated=tool::sir::inflate_covmatrix(matrix => $covar,
										  inflation => [2,2,2,2,2]);

cmp_ok($covar->[0]->[0],'==',1.55838e-07,'covar element 1,1 after inflation not changed');
cmp_ok($covar->[2]->[2],'==',0.0244759,'covar element 3,3 after inflation not changed');
cmp_ok($covar->[3]->[1],'==',0.00170324,'covar element 4,2 after inflation not changed');



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
