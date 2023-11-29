#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
#use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::MatrixReal;
use Math::Trig;	# For pi
use random;
use output;
use tool::sir;
use linear_algebra;
use File::Copy 'cp';




my ($d1,$d2,$d3)= get_major_minor_nm_version;
our $tempdir = create_test_dir('unit_sir');
chdir($tempdir);


my $mat = new Math::MatrixReal(1,1);
my $mu = $mat->new_from_rows( [[1,2,3]] );
my $icm = $mat->new_from_rows( [[3.356398464199823e-01,-2.953461879245602e-03,-3.312096536011139e-02],
								[    -2.953461879245602e-03, 1.257330914307413e-01, -1.856461752668664e-02],
								[    -3.312096536011139e-02, -1.856461752668664e-02,  5.060967891650141e-01]]);
#print $mu;

my $k=3;
my $base=tool::sir::get_determinant_factor(inverse_covmatrix => $icm,
										   k => $k,
										   inflation => 1);

cmp_relative($base,9.222143261486744e-03,13,'well conditioned determinant factor');
my $nsamples=1;

#my $covar = [[3,0.1,0.2],[0.1,8,0.3],[0.2,0.3,2]];

my $gotsamples = [[0,1,2]];




my $relpdf=tool::sir::mvnpdf(inverse_covmatrix => $icm,
						  mu => $mu,
						  xvec_array => $gotsamples,
						  inflation => 1);

cmp_relative($relpdf->[0],6.510975388450209e-01,13,'well conditioned exponent');

cmp_relative(($base*$relpdf->[0]),6.004514780430216e-03,13,' matlab mvnpdf ');

cmp_relative(($base),9.222143261486746e-03,13,' matlab mvnpdf center ');
cmp_relative($relpdf->[0],6.510975388450211e-01,13,' matlab mvnpdf rel pdf ');


my $dir = $includes::testfiledir . "/sir/";
my $file = 'run1_noblock.lst';
my $output = output->new(filename => $dir . $file);


my $values = $output->get_filtered_values(parameter => 'theta');

cmp_ok($values->[0],'==',2.66825E+01,' theta 1');
cmp_ok($values->[1],'==',1.10274E+02,' theta 2');
cmp_ok($values->[2],'==',4.49611E+00,' theta 3');
cmp_ok($values->[3],'==',2.40133E-01,' theta 4');
cmp_ok($values->[4],'==',3.30597E-01,' theta 5');
cmp_ok($values->[5],'==',7.50245E-02,' theta 6');
cmp_ok($values->[6],'==',5.63377E-02,' theta 7');
cmp_ok($values->[7],'==',7.18895E-01,' theta 8');

my $hash = output::get_nonmem_parameters(output => $output);

cmp_ok($hash->{'values'}->[0],'==',2.66825E+01,'hash theta 1');
cmp_ok($hash->{'values'}->[1],'==',1.10274E+02,'hash theta 2');
cmp_ok($hash->{'values'}->[2],'==',4.49611E+00,'hash theta 3');


my @resampled_params_arr =(
	{'ofv' =>100, 'theta'=> {'1 TVCL' => 1.0,'2 TVV' => 2.0,'3 TVKA'=> 3.0, '4 LAG' => 4.0,'5 RES ERR' => 5.0,'6 VAR IIV CL' => 6.0,
	 '7 VAR IIV V' => 7.0, '8 CORR IIV CL-V' => 8.0}, 'omega'=> {'3 IIV KA' => 1.0,'4 IOV CL OCC1' => 2.0,'6 IOV KA OCC1' => 3.0 },'sigma'=> {}},
	{'ofv' =>10, 'theta'=> {'1 TVCL' => 10.0,'2 TVV' => 10.0,'3 TVKA'=> 10.0, '4 LAG' => 10.0,'5 RES ERR' => 10.0,'6 VAR IIV CL' => 10.0,
	 '7 VAR IIV V' => 10.0, '8 CORR IIV CL-V' => 10.0}, 'omega'=> {'3 IIV KA' => 10.0,'4 IOV CL OCC1' => 10.0,'6 IOV KA OCC1' => 10.0 },'sigma'=> {}},
	{'ofv' =>100, 'theta'=> {'1 TVCL' => 1.0,'2 TVV' => 2.0,'3 TVKA'=> 3.0, '4 LAG' => 4.0,'5 RES ERR' => 5.0,'6 VAR IIV CL' => 6.0,
	 '7 VAR IIV V' => 7.0, '8 CORR IIV CL-V' => 8.0}, 'omega'=> {'3 IIV KA' => 1.0,'4 IOV CL OCC1' => 2.0,'6 IOV KA OCC1' => 3.0 },'sigma'=> {}},
	{'ofv' =>100, 'theta'=> {'1 TVCL' => 1.0,'2 TVV' => 2.0,'3 TVKA'=> 3.0, '4 LAG' => 4.0,'5 RES ERR' => 5.0,'6 VAR IIV CL' => 6.0,
	 '7 VAR IIV V' => 7.0, '8 CORR IIV CL-V' => 8.0}, 'omega'=> {'3 IIV KA' => 1.0,'4 IOV CL OCC1' => 2.0,'6 IOV KA OCC1' => 3.0 },'sigma'=> {}}
);

my ($errors,$newofv,$newvector) = tool::sir::get_min_ofv_values(sampled_params_arr => \@resampled_params_arr,
									parameter_hash => $hash);
cmp_ok(scalar(@{$errors}),'==',0,' count errors get_min_ofv_values');
cmp_ok($newofv,'==',10,' ofv get_min_ofv_values');
is_deeply($newvector,[10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0],'get_min_ofv_values');
@resampled_params_arr =(
	{'ofv' =>100, 'theta'=> {'TVCL' => 1.0,'2 TVV' => 2.0,'3 TVKA'=> 3.0, '4 LAG' => 4.0,'5 RES ERR' => 5.0,'6 VAR IIV CL' => 6.0,
							 '7 VAR IIV V' => 7.0, '8 CORR IIV CL-V' => 8.0}, 'omega'=> {'3 IIV KA' => 1.0,'4 IOV CL OCC1' => 2.0,'6 IOV KA OCC1' => 3.0 },
	 'sigma'=> {}});
($errors,$newofv,$newvector) = tool::sir::get_min_ofv_values(sampled_params_arr => \@resampled_params_arr,
								 parameter_hash => $hash);
cmp_ok(scalar(@{$errors}),'==',1,' count errors get_min_ofv_values');
cmp_ok($newofv,'==',100,' ofv get_min_ofv_values');

my $samples=[100,100,100,100];
my $resamples=[20,20,20,20];
my $successful_samples=[];
my $actual_resamples=[];
my $attempted_samples=[];

is(tool::sir::update_attempted_samples(samples=>$samples,
									   successful_samples=>$successful_samples,
									   attempted_samples=>$attempted_samples,
									   iteration=> 1),100,"update attempted samples 1");
is_deeply($attempted_samples,[100],"updated attempted samples 1 array");

is(tool::sir::update_actual_resamples(samples=>$samples,
									  resamples=>$resamples,
									  successful_samples=>$successful_samples,
									  actual_resamples=>$actual_resamples,
									  successful_count=>90,
									  iteration=> 1),18,"update actual resamples 10% loss");
is_deeply($successful_samples,[90],"updated successful samples");
is_deeply($actual_resamples,[18],"updated actual resamples");

is(tool::sir::update_attempted_samples(samples=>$samples,
									   successful_samples=>$successful_samples,
									   attempted_samples=>$attempted_samples,
									   iteration=> 2),111,"update attempted samples 2 iteration 2");
is_deeply($attempted_samples,[100,111],"updated attempted samples 2 array");

is(tool::sir::update_actual_resamples(samples=>$samples,
									  resamples=>$resamples,
									  successful_samples=>$successful_samples,
									  actual_resamples=>$actual_resamples,
									  successful_count=>102,
									  iteration=> 2),20,"update actual resamples 2% gain");
is_deeply($successful_samples,[90,102],"updated successful samples");
is_deeply($actual_resamples,[18,20],"updated actual resamples");

is(tool::sir::update_attempted_samples(samples=>$samples,
									   successful_samples=>$successful_samples,
									   attempted_samples=>$attempted_samples,
									   iteration=> 3),109,"update attempted samples 3");
is_deeply($attempted_samples,[100,111,109],"updated attempted samples 3 array");

is(tool::sir::update_actual_resamples(samples=>$samples,
									  resamples=>$resamples,
									  successful_samples=>$successful_samples,
									  actual_resamples=>$actual_resamples,
									  successful_count=>109,
									  iteration=> 3),22,"update actual resamples 10% gain");
is_deeply($successful_samples,[90,102,109],"updated successful samples");
is_deeply($actual_resamples,[18,20,22],"updated actual resamples");

is(tool::sir::update_attempted_samples(samples=>$samples,
									   successful_samples=>$successful_samples,
									   attempted_samples=>$attempted_samples,
									   iteration=> 4),100,"update attempted samples 4");
is_deeply($attempted_samples,[100,111,109,100],"updated attempted samples 4 array");

is(tool::sir::update_actual_resamples(samples=>$samples,
									  resamples=>$resamples,
									  successful_samples=>$successful_samples,
									  actual_resamples=>$actual_resamples,
									  successful_count=>98,
									  iteration=> 4),20,"update actual resamples 2% loss");
is_deeply($successful_samples,[90,102,109,98],"updated successful samples");
is_deeply($actual_resamples,[18,20,22,20],"updated actual resamples");


$values = $output->get_filtered_values(parameter => 'sigma');
cmp_ok(scalar(@{$values}),'==',0,' fixed sigma');

$values = $output->get_filtered_values(parameter => 'omega');
is_deeply($values,[2.81746E+00,1.46957E-02,5.06114E-01],'omega');
  

my $block_covar = tool::sir::get_nonmem_covmatrix(output => $output);

my $ref=[
[0.9271,2.27211,0.0275593,-0.000287334,0.00846576,-0.000618088,-0.00144621,0.0267219,0.103993,-0.000950558,0.027529],
[2.27211,15.718,0.655662,-0.00255354,0.0305418,0.00231333,-0.00930454,0.000530111,0.241725,0.00161953,0.168762],
[0.0275593,0.655662,1.87885,0.0042868,-0.0188686,-0.000476181,0.00183569,-0.0217546,0.97174,-6.89874E-005,0.13039],
[-0.000287334,-0.00255354,0.0042868,3.39565E-005,-5.09132E-005,3.51457E-006,-0.00000139695,-0.000120266,0.00278859,-0.00000351597,0.000341127],
[0.00846576,0.0305418,-0.0188686,-5.09132E-005,0.000710048,0.000029797,-5.58994E-005,0.000696781,-0.00712629,2.43304E-007,-0.000318354],
[-0.000618088,0.00231333,-0.000476181,3.51457E-006,0.000029797,0.000148953,1.18338E-005,0.000154661,0.00016596,-8.45776E-006,3.52465E-005],
[-0.00144621,-0.00930454,0.00183569,-0.00000139695,-5.58994E-005,1.18338E-005,0.000142996,3.19477E-005,0.00128481,9.76454E-006,-0.000460725],
[0.0267219,0.000530111,-0.0217546,-0.000120266,0.000696781,0.000154661,3.19477E-005,0.00581318,-0.00718936,3.62128E-005,0.000430627],
[0.103993,0.241725,0.97174,0.00278859,-0.00712629,0.00016596,0.00128481,-0.00718936,0.886165,-0.000513899,-0.0251719],
[-0.000950558,0.00161953,-6.89874E-005,-0.00000351597,2.43304E-007,-8.45776E-006,9.76454E-006,3.62128E-005,-0.000513899,2.36718E-005,0.000126788],
[0.027529,0.168762,0.13039,0.000341127,-0.000318354,3.52465E-005,-0.000460725,0.000430627,-0.0251719,0.000126788,0.0689168]
];
is_deeply($block_covar,$ref,'covariance matrix');

#my $mat = new Math::MatrixReal(1,1);
#my $mu = $mat->new_from_rows( [$output->get_filtered_values()] );

my $covar = [[3,0.1,0.2],[0.1,8,0.3],[0.2,0.3,2]];


#this destroys covar
my $err=linear_algebra::cholesky_transpose($covar);

my $matlabref =[
[1.732050807568877e+00,                         0,                         0],
[     5.773502691896259e-02,     2.827837807701613e+00,                         0],
[     1.154700538379252e-01,     1.037306073687962e-01,     1.405669458927513e+00]
	];
is_deeply($covar,$matlabref,'cholesky T');


my $root_determinant = $covar->[0][0];
for (my $i=1; $i< 3; $i++){
	$root_determinant = $root_determinant*$covar->[$i][$i];
}
cmp_float($root_determinant,6.884911037914724,'root determinant');
my $diff = [1,2,3];
$err = linear_algebra::upper_triangular_transpose_solve($covar,$diff);


is_deeply($diff,[5.773502691896258e-01,6.954665721317015e-01,2.035465838166839e+00],' solve R ');

my $sum = 0;
for (my $i=0; $i< scalar(@{$diff}); $i++){
	$sum = $sum + ($diff->[$i])**2;
}
cmp_float($sum,4.960128264630185,'vecotr prod ');
my $pdf = ((2*pi)**(-3/2))*(1/$root_determinant)*exp(-0.5*$sum);

cmp_float($pdf,7.722424963030007e-04,' chol pdf ');

$covar = [[3,0.1,0.2],[0.1,8,0.3],[0.2,0.3,2]];
$mu = [1,2,3];

my $xvectors=[[0,0,0],
	[1,2,3],
	[4,4,4],
	[0.1, -0.6, 8]];


#matlab code in sir.m
#inflation none relative no
my $results = linear_algebra::mvnpdf_cholesky($covar,$mu,$xvectors,[],0);

cmp_relative($results->[0],7.722424963030007e-04,12,'mvnpdf_chol vs matlab builtin mvnpdf 1');
cmp_relative($results->[1],9.222143261486746e-03,12,'mvnpdf_chol vs matlab builtin mvnpdf 2');
cmp_relative($results->[2],1.434657987390218e-03,12,'mvnpdf_chol vs matlab builtin mvnpdf 3');
cmp_relative($results->[3],6.415825193748951e-06,12,'mvnpdf_chol vs matlab builtin mvnpdf 4');

#inflation none relative yes
$results = linear_algebra::mvnpdf_cholesky($covar,$mu,$xvectors,[],1);

cmp_relative($results->[0],8.373785511747774e-02,12,'rel mvnpdf_chol vs matlab builtin mvnpdf 1');
cmp_relative($results->[1],1.000000000000000e+00,12,'rel mvnpdf_chol vs matlab builtin mvnpdf 2');
cmp_relative($results->[2],1.555666558967475e-01,12,'rel mvnpdf_chol vs matlab builtin mvnpdf 3');
cmp_relative($results->[3], 6.956978450489421e-04,12,'rel mvnpdf_chol vs matlab builtin mvnpdf 4');


#inflation [3,3,3] relative no
$results = linear_algebra::mvnpdf_cholesky($covar,$mu,$xvectors,[3,3,3],0);
cmp_relative($results->[0],7.764686515437722e-04,12,'infl mvnpdf_chol vs matlab builtin mvnpdf 1');
cmp_relative($results->[1],1.774802298174889e-03,12,'infl mvnpdf_chol vs matlab builtin mvnpdf 2');
cmp_relative($results->[2],9.545283267243817e-04,12,'infl mvnpdf_chol vs matlab builtin mvnpdf 3');
cmp_relative($results->[3],1.572619060514979e-04,12,'infl mvnpdf_chol vs matlab builtin mvnpdf 4');

#inflation [3,3,3] relative yes
$results = linear_algebra::mvnpdf_cholesky($covar,$mu,$xvectors,[3,3,3],1);
cmp_relative($results->[0],4.374958564918756e-01,12,'infl rel mvnpdf_chol vs matlab builtin mvnpdf 1');
cmp_relative($results->[1],1.000000000000000e+00,12,'infl rel mvnpdf_chol vs matlab builtin mvnpdf 2');
cmp_relative($results->[2],5.378223409480409e-01,12,'infl rel mvnpdf_chol vs matlab builtin mvnpdf 3');
cmp_relative($results->[3],8.860812621958941e-02,12,'infl rel mvnpdf_chol vs matlab builtin mvnpdf 4');


#inflation [5,1,2] relative yes
$results = linear_algebra::mvnpdf_cholesky($covar,$mu,$xvectors,[5,1,2],1);
cmp_relative($results->[0],2.695714028188652e-01,12,'ind infl rel mvnpdf_chol vs matlab builtin mvnpdf 1');
cmp_relative($results->[1],1.000000000000000e+00,12,'ind infl rel mvnpdf_chol vs matlab builtin mvnpdf 2');
cmp_relative($results->[2],5.409298739477048e-01,12,'ind infl rel mvnpdf_chol vs matlab builtin mvnpdf 3');
cmp_relative($results->[3],2.171039717093587e-02,12,'ind infl rel mvnpdf_chol vs matlab builtin mvnpdf 4');

#inflation [7,3,4] relative no
$results = linear_algebra::mvnpdf_cholesky($covar,$mu,$xvectors,[7,3,4],0);
cmp_relative($results->[0],5.386256507016714e-04,12,'ind infl abs mvnpdf_chol vs matlab builtin mvnpdf 1');
cmp_relative($results->[1],1.006218322987856e-03,12,'ind infl abs mvnpdf_chol vs matlab builtin mvnpdf 2');
cmp_relative($results->[2],7.237635960362615e-04,12,'ind infl abs mvnpdf_chol vs matlab builtin mvnpdf 3');
cmp_relative($results->[3],1.599715462696341e-04,12,'ind infl abs mvnpdf_chol vs matlab builtin mvnpdf 4');
     
#check covar not destoryed
is_deeply($covar,[[3,0.1,0.2],[0.1,8,0.3],[0.2,0.3,2]],'covar not destryed ');
is_deeply($mu,[1,2,3],'mu not destoryed');
is_deeply($xvectors,[[0,0,0],[1,2,3],	[4,4,4],	[0.1, -0.6, 8]],'xvectors not destoryed');

$file = 'moxo.lst';
$output = output->new(filename => $dir . $file);


# matlab test code in moxonidine.m
my $moxo_covar = tool::sir::get_nonmem_covmatrix(output => $output);

$ref=[
[0.873778,2.19014,0.13887,-2.98453E-005,-0.000785758,0.000235191,-0.00137167,0.148541,-0.00103326,0.0297615,0.0050723],
[2.19014,15.5223,0.622217,-0.0026337,0.000931442,-0.00686826,-0.0108224,0.212108,0.00116257,0.150677,0.0227413],
[0.13887,0.622217,0.887891,0.00247469,-0.000142814,-0.000284542,0.00131662,0.601221,-0.00019056,0.0850608,-0.00630654],
[-2.98453E-005,-0.0026337,0.00247469,3.10691E-005,3.33776E-006,-7.37406E-006,-2.46345E-006,0.00222128,-3.92063E-006,0.000279102,-2.62843E-005],
[-0.000785758,0.000931442,-0.000142814,3.33776E-006,0.0001542,7.27021E-005,1.88658E-005,0.000267381,-6.87112E-006,4.12506E-005,1.18544E-005],
[0.000235191,-0.00686826,-0.000284542,-7.37406E-006,7.27021E-005,0.000098852,8.18745E-005,0.000259973,7.13069E-006,-0.000147872,7.30389E-007],
[-0.00137167,-0.0108224,0.00131662,-2.46345E-006,1.88658E-005,8.18745E-005,0.000151409,0.00123871,1.15681E-005,-0.000440197,-4.17504E-005],
[0.148541,0.212108,0.601221,0.00222128,0.000267381,0.000259973,0.00123871,0.777145,-0.000582497,-0.0380049,-0.00324836],
[-0.00103326,0.00116257,-0.00019056,-3.92063E-006,-6.87112E-006,7.13069E-006,1.15681E-005,-0.000582497,2.41602E-005,0.000119454,-0.000000161],
[0.0297615,0.150677,0.0850608,0.000279102,4.12506E-005,-0.000147872,-0.000440197,-0.0380049,0.000119454,0.0672077,-0.000182254],
[0.0050723,0.0227413,-0.00630654,-2.62843E-005,1.18544E-005,7.30389E-007,-4.17504E-005,-0.00324836,-0.000000161,-0.000182254,0.000299476]
];
is_deeply($moxo_covar,$ref,'covariance matrix');

$mu = [26.6826,110.274,4.49576,0.240133,0.0750256,0.0467377,0.056338,2.81718,0.0146954,0.506077,0.109295];


$xvectors=[
[26.623,113.055,4.3679,0.247502,0.0796516,0.0395443,0.0568655,2.84291,0.0105876,0.385506,0.131366],
[26.9504,110.441,5.24368,0.240122,0.0686938,0.041067,0.0517129,2.0173,0.0121147,0.942027,0.100209],
[27.2189,106.401,6.39645,0.240122,0.0948644,0.0533097,0.0616498,4.31796,0.00736211,0.947794,0.110911],
[  28.038,109.534,3.54128,0.23322,0.0673617,0.0275188,0.0287049,1.65518,0.000104766,0.693002,0.116561],
[26.4126,107.534,5.35781,0.240122,0.0765838,0.0480814,0.0616885,3.65379,0.0164994,0.291707,0.0947614],
[26.3658,115.166,5.00756,0.238895,0.0911875,0.0553033,0.0610049,3.34658,0.0200679,0.557913,0.127942],
[26.7699,105.408,5.66196,0.240122,0.0673888,0.0381466,0.0382593,3.51061,0.00500059,0.908073,0.0822799],
[25.1499,106.82,4.42911,0.24821,0.0806821,0.0338323,0.0385349,3.25215,0.0130906,0.265113,0.0893015],
[28.0504,111.106,6.58429,0.240122,0.0578236,0.0300437,0.0303841,5.21521,0.00794128,0.616639,0.106735],
[27.9132,117.137,4.01264,0.238383,0.0755322,0.032908,0.0269704,1.16104,0.0115552,1.04045,0.128992],
[27.2229,114.4,5.66876,0.241196,0.0378813,0.021466,0.0281806,2.9152,0.0159949,0.578787,0.104014],
[27.2082,112.178,5.16698,0.244135,0.0902808,0.0542473,0.0447351,2.9452,0.0167429,0.655561,0.117871],
[26.4204,107.51,4.36827,0.239701,0.0607497,0.0399426,0.053148,3.18952,0.0185022,0.569186,0.0914089],
[28.2452,110.812,5.57434,0.24325,0.0812799,0.05597,0.063812,3.99574,0.00945897,0.28784,0.102739],
[27.4687,111.924,4.95307,0.231575,0.0723104,0.0456784,0.0403753,2.5557,0.0171464,0.715062,0.123952],
[26.2775,106.028,4.11019,0.249755,0.0667135,0.0519199,0.0793815,3.19723,0.0158766,0.584682,0.119545]  ];

#absolute, no inflation
$results = linear_algebra::mvnpdf_cholesky($moxo_covar,$mu,$xvectors,[],0);

my $matlababs=[8.408615352278130e+06, 3.320492620781928e+07,1.363184286581484e+04,2.934253934734758e+04,8.543514599035780e+07,1.943559169652895e+07,
2.039801927640183e+05,6.082921464337746e+06, 2.313966448511987e+04,1.309139347567398e+06, 3.638346261905838e+04,1.528343354309707e+07,
2.941693435563622e+07, 1.522664659748737e+07, 5.170528396931276e+06, 4.348549251948097e+05];

for (my $i=0; $i< scalar(@{$results}); $i++){
	cmp_relative($results->[$i],$matlababs->[$i],12,'abs moxo mvnpdf_chol vs matlab builtin mvnpdf '.$i);
}

#relative, no inflation
$results = linear_algebra::mvnpdf_cholesky($moxo_covar,$mu,$xvectors,[],1);
my $matlabrel=[ 9.923550589244195e-03, 3.918728009673332e-02, 1.608781905654550e-05, 3.462902766165926e-05, 1.008275391149960e-01, 2.293719826060845e-02, 
2.407302126799157e-04, 7.178848877348236e-03, 2.730861402454468e-05, 1.544999979150610e-03, 4.293847640613998e-05, 1.803696798916897e-02, 
3.471682602053407e-02, 1.796995004341245e-02, 6.102074832828437e-03, 5.132004103367098e-04];

for (my $i=0; $i< scalar(@{$results}); $i++){
	cmp_relative($results->[$i],$matlabrel->[$i],12,'rel moxo mvnpdf_chol vs matlab builtin mvnpdf '.$i);
}


$ref = tool::format_covmatrix(matrix => [[1,2,3],[4,5,6],[7,8,9]], 
											header => ['anna','bertil','cecilia'], 
											comma => 1, 
											print_labels => 1);

is_deeply($ref,['"NAME","anna","bertil","cecilia"'."\n",'"anna",1,2,3'."\n",'"bertil",4,5,6'."\n",'"cecilia",7,8,9'."\n"], "format covmatrix comma with labels");

$ref = tool::format_covmatrix(matrix => [[1,2,3],[4,5,6],[7,8,9]], 
											header => ['anna','bertil','cecilia'], 
											comma => 1, 
											print_labels => 0);

is_deeply($ref,['1,2,3'."\n",'4,5,6'."\n",'7,8,9'."\n"], "format covmatrix comma no labels");

$ref = tool::format_covmatrix(matrix => [[1,2,3],[4,5,6],[7,8,9]], 
											header => ['anna','bertil','cecilia'], 
											comma => 0, 
											print_labels => 1);



is($ref->[0]," NAME             anna           bertil         cecilia        \n", "format covmatrix space with labels 0");
like($ref->[1],'/^ anna    \s+1\.0000000E\+0+\s+2\.0000000E\+0+\s+3\.0000000E\+0+/', "format covmatrix space with labels 1");
like($ref->[2],'/^ bertil  \s+4\.0000000E\+0+\s+5\.0000000E\+0+\s+6\.0000000E\+0+/', "format covmatrix space with labels 2");
like($ref->[3],'/^ cecilia \s+7\.0000000E\+0+\s+8\.0000000E\+0+\s+9\.0000000E\+0+/', "format covmatrix space with labels 3");


$ref = tool::format_covmatrix(matrix => [[1,2,3],[4,5,6],[7,8,9]], 
											header => ['anna','bertil','cecilia'], 
											comma => 0, 
											print_labels => 0);


like($ref->[0],'/^\s+1\.0000000E\+0+\s+2.0000000E\+0+\s+3\.0000000E\+0+/', "format covmatrix space no labels 0");
like($ref->[1],'/^\s+4\.0000000E\+0+\s+5.0000000E\+0+\s+6\.0000000E\+0+/', "format covmatrix space no labels 1");
like($ref->[2],'/^\s+7\.0000000E\+0+\s+8.0000000E\+0+\s+9\.0000000E\+0+/', "format covmatrix space no labels 2");


#mox_sir_blcok2

@resampled_params_arr =(
{'theta'=> {'THETA1' => 1.0,'THETA2' => 1.0,'THETA3'=> 1.0, 'THETA4' => 1.0,'THETA5' => 1.0}, 
 'omega'=> {'OMEGA(1,1)' => 1.0,'OMEGA(2,2)' => 1.0,'OMEGA(3,2)' => 0.1,'OMEGA(3,3)' => 1.0 },
 'sigma'=> {}},
{	'theta'=> {'THETA1' => 10.0,'THETA2' => 10.0,'THETA3'=> 10.0, 'THETA4' => 10.0,'THETA5' => 10.0}, 
	'omega'=> {'OMEGA(1,1)' => 10.0,'OMEGA(2,2)' => 10.0,'OMEGA(3,2)' => 1,'OMEGA(3,3)' => 10.0 },
 'sigma'=> {}},
);

my $modeldir = $includes::testfiledir;

my $model = model->new(filename => "$modeldir/mox_sir_block2.mod", ignore_missing_data => 1);
# 5 theta 4 omega 0 sigma
is_deeply(tool::sir::setup_inflation(model => $model,
									theta_inflation => '1',
									omega_inflation => '2',
									sigma_inflation => '1'),[1,1,1,1,1,2,2,2,2],'inflation 1');
is_deeply(tool::sir::setup_inflation(model => $model,
									theta_inflation => '1,2,3,4,5',
									omega_inflation => '3',
									sigma_inflation => '1'),[1,2,3,4,5,3,3,3,3],'inflation 2');
is_deeply(tool::sir::setup_inflation(model => $model,
									theta_inflation => '1,2,3,4,5',
									omega_inflation => '6,16,25',
									sigma_inflation => '1'),[1,2,3,4,5,6,16,20,25],'inflation 3');
is_deeply(tool::sir::setup_inflation(model => $model,
									theta_inflation => '1,1,1,1,1',
									omega_inflation => '1',
									sigma_inflation => '1'),[],'inflation 4');


dies_ok {tool::sir::setup_inflation(model => $model,
									theta_inflation => '1',
									omega_inflation => '1',
									sigma_inflation => '2')} 'illegal inflation 1';
dies_ok {tool::sir::setup_inflation(model => $model,
									theta_inflation => '1,2,3,4',
									omega_inflation => '1',
									sigma_inflation => '1')} 'illegal inflation 2';
dies_ok {tool::sir::setup_inflation(model => $model,
									theta_inflation => '1',
									omega_inflation => '1,2,3,4',
									sigma_inflation => '1')} 'illegal inflation 3';
dies_ok {tool::sir::setup_inflation(model => $model,
									theta_inflation => '-11',
									omega_inflation => '1',
									sigma_inflation => '1')} 'illegal inflation 4';


my $parameter_hash = output::get_nonmem_parameters(output => $model->outputs->[0]);
# 5 theta 4 omega 0 sigma
#print join(',',@{$parameter_hash->{'values'}})."\n";
my @moxvalues =(32.8872,20.9156,0.296626,0.0992828,0.3337,0.409882,1.24558,0.136766,0.218255);
my @ans1=((32.8872*(0.2))**2,(20.9156*(0.2))**2,(0.296626*(0.2))**2,(0.0992828*(0.2))**2,(0.3337*(0.2))**2,
	(0.409882*(0.1))**2,(1.24558*(0.1))**2,(0.136766/(sqrt(1.24558)*sqrt(0.218255)))*(0.218255*(0.1))*(1.24558*(0.1)),(0.218255*(0.1))**2);
is_deeply(tool::sir::setup_variancevec_from_rse(rse_theta => 20,
											   rse_omega=> 10,
											   rse_sigma=> 20,
											   parameter_hash => $parameter_hash,
											   type => 2),\@ans1,'setup_variancevec 1');

@ans1=((32.8872*(0.2))**2,(20.9156*(0.3))**2,(0.296626*(0.4))**2,(0.0992828*(0.3))**2,(0.3337*(0.1))**2,
	(0.409882*(0.15))**2,(1.24558*(0.30))**2,(0.136766/(sqrt(1.24558)*sqrt(0.218255)))*(0.218255*(0.3))*(1.24558*(0.2)),(0.218255*(0.2))**2);
is_deeply(tool::sir::setup_variancevec_from_rse(rse_theta => '20,30,40,30,10',
											   rse_omega=> '15,30,20',
											   rse_sigma=> '',
											   parameter_hash => $parameter_hash,
											   type => 2),\@ans1,'setup_variancevec 2');

cmp_float(tool::sir::get_offdiagonal_variance(type=> 1, covariance => 0.03, rse_i=>20, rse_j=> 40, var_i=> 3, var_j=> 0.5),
	(0.03**2+1.5)/(25+(2.5)**2+1),'get offdiag variance type 1');


dies_ok{tool::sir::setup_variancevec_from_rse(rse_theta => '20,30,40,30,10',
											 rse_omega=> '0',
											 rse_sigma=> '10',
											 parameter_hash => $parameter_hash)};#, 'illegal setup_variancevec 1';
dies_ok{tool::sir::setup_variancevec_from_rse(rse_theta => '20,30,40,30,10',
											 rse_omega=> '1,2,3,4',
											 rse_sigma=> '10',
											 parameter_hash => $parameter_hash)};#, 'illegal setup_variancevec 2';
dies_ok{tool::sir::setup_variancevec_from_rse(rse_theta => '20,40,30,10',
											 rse_omega=> '1,2,3',
											 rse_sigma=> '10',
											 parameter_hash => $parameter_hash)};#, 'illegal setup_variancevec 3';

my $cov = tool::sir::setup_covmatrix_from_variancevec(variance => [1,2,3]);
is_deeply($cov->[0],[1,0,0],'covmatrix from variancevec 0');
is_deeply($cov->[1],[0,2,0],'covmatrix from variancevec 1');
is_deeply($cov->[2],[0,0,3],'covmatrix from variancevec 2');

#FIXME input check when 0 sigma or omega

my $Amatrix = tool::sir::tweak_inits_sampling(sampled_params_arr => \@resampled_params_arr,
											  parameter_hash => $parameter_hash,
											  model => $model,
											  degree => 0.1,
											  output => $model->outputs->[0],
	);

$Amatrix = tool::sir::tweak_inits_sampling(sampled_params_arr => [],
											  parameter_hash => $parameter_hash,
											  model => $model,
											  degree => 0.1,
											  output => $model->outputs->[0],
	);

chdir($tempdir);
my $recovery_filename = 'restart_information_do_not_edit.pl';

my $sirdir='sir_dir1';
mkdir($sirdir);
chdir($sirdir);
cp("$modeldir/pheno.mod",'pheno.mod');
cp("$modeldir/pheno.dta",'pheno.dta');

my @center_rawresults_vector=(1,2,3,4,5);

my @seed_array=(23,23);
random_set_seed(@seed_array);

my @in = random_get_seed;
use Data::Dumper;
print Dumper(\@in);
$err = tool::sir::save_restart_information(
	parameter_hash => $parameter_hash,
	nm_version  => 'default',
	done  => 1,
	adjust_blocks => 1,
	check_cholesky_reparameterization => 0,
	recenter  => 0,
	copy_data => 0,
	boxcox => 0,
	with_replacement => 1,
	cap_resampling => 1,
	iteration  => 2,
	mceta  => 3,
	problems_per_file  => 25,
	reference_ofv  => 830.4,
	center_rawresults_vector => [1,2,3,4,5],
	minimum_ofv => [834,830],
	negative_dofv => [4,0],
	samples => [100,100,100],
	resamples => [50,50,50],
	attempted_samples => [100,100],
	successful_samples => [98,97],
	actual_resamples => [50,50],
	intermediate_raw_results_files => ['rawres1.csv','rawres2.csv'],
	model_filename => 'pheno.mod',
	subjects => 59,
	seed_array => \@in);
	
chdir($tempdir); #back up
random_set_seed(54,89);

my $recoversir =tool::sir->new ( models				     => [model->create_dummy_model ],
	directory => $sirdir,
	samples => [],
	resamples => []
	);

is($recoversir->nm_version,'default','recovery info 1');
is($recoversir->done,1,'recovery info 2');
is($recoversir->recenter,0,'recovery info 3');
is($recoversir->adjust_blocks,1,'recovery info 3.5');
is($recoversir->check_cholesky_reparameterization,0,'recovery info 3.6');
is($recoversir->subjects,59,'recovery info 3.7');
is($recoversir->copy_data,0,'recovery info 4');
is($recoversir->boxcox,0,'recovery info 5');
is($recoversir->with_replacement,1,'recovery info 6');
is($recoversir->cap_resampling,1,'recovery info 6.5');
is($recoversir->iteration,2,'recovery info 7');
is($recoversir->mceta,3,'recovery info 8');
is($recoversir->problems_per_file,25,'recovery info 9');
is($recoversir->reference_ofv,830.4,'recovery info 10');
is_deeply($recoversir->center_rawresults_vector,\@center_rawresults_vector,'recovery info 10.5');
is_deeply($recoversir->minimum_ofv,[834,830],'recovery info 11');
is_deeply($recoversir->negative_dofv,[4,0],'recovery info 12');
is_deeply($recoversir->samples,[100,100,100],'recovery info 13');
is_deeply($recoversir->resamples,[50,50,50],'recovery info 14');
is_deeply($recoversir->attempted_samples,[100,100],'recovery info 15');
is_deeply($recoversir->successful_samples,[98,97],'recovery info 16');
is_deeply($recoversir->actual_resamples,[50,50],'recovery info 17');
is_deeply($recoversir->intermediate_raw_results_files,['rawres1.csv','rawres2.csv'],'recovery info 18');
is_deeply($recoversir->parameter_hash->{'labels'},$parameter_hash->{'labels'},'recovery info 19');
is_deeply($recoversir->models->[0]->filename,'pheno.mod','recovery info 20');

my @ans = random_get_seed;

random_set_seed(98,8105);

$recoversir =tool::sir->new ( models				     => [model->create_dummy_model ],
	directory => $sirdir,
	nm_version => 'nm73',
	add_iterations => 1,
	cap_resampling => 5,
	samples => [300,400],
	resamples => [100,100],
	);

is($recoversir->nm_version,'nm73','add_iterations info 1');
is($recoversir->done,0,'add_iterations info 2');
is($recoversir->recenter,1,'add_iterations info 3');
is($recoversir->adjust_blocks,0,'add_iterations info 3.5');
is($recoversir->check_cholesky_reparameterization,0,'add_iterations info 3.6');
is($recoversir->subjects,59,'add_iterations info 3.7');
is($recoversir->copy_data,1,'add_iterations info 4');
is($recoversir->boxcox,1,'add_iterations info 5');
is($recoversir->with_replacement,0,'add_iterations info 6');
is($recoversir->cap_resampling,5,'add_iterations info 6.5');
is($recoversir->iteration,2,'add_iterations info 7');
is($recoversir->mceta,0,'add_iterations info 8');
is($recoversir->problems_per_file,100,'add_iterations info 9');
is($recoversir->reference_ofv,830.4,'add_iterations info 10');
is_deeply($recoversir->center_rawresults_vector,\@center_rawresults_vector,'add_iterations info 10.5');
is_deeply($recoversir->minimum_ofv,[834,830],'add_iterations info 11');
is_deeply($recoversir->negative_dofv,[4,0],'add_iterations info 12');
is_deeply($recoversir->samples,[100,100,300,400],'add_iterations info 13');
is_deeply($recoversir->resamples,[50,50,100,100],'add_iterations info 14');
is_deeply($recoversir->attempted_samples,[100,100],'add_iterations info 15');
is_deeply($recoversir->successful_samples,[98,97],'add_iterations info 16');
is_deeply($recoversir->actual_resamples,[50,50],'add_iterations info 17');
is_deeply($recoversir->intermediate_raw_results_files,['rawres1.csv','raw_results_sir_iteration2.csv'],'add_iterations info 18');
is_deeply($recoversir->parameter_hash->{'labels'},$parameter_hash->{'labels'},'add_iterations info 19');
is_deeply($recoversir->models->[0]->filename,'pheno.mod','add_iterations info 20');

my @rawres = ();

my $hashref = tool::sir::augment_rawresults(
	raw_results => [[1,1,1],[2,2,2],[3,3,3]],
	filtered_pdf => [1,undef,2],
	dofv_array => [3,undef,5],
	id_array => [1,2,3],
	cap_resampling => 3);

is(scalar(@{$hashref->{'raw'}}),7,'augment rawresults total');
is_deeply($hashref->{'pdf'},[1,1,1,undef,2,2,2],'augment pdf');
is_deeply($hashref->{'id'},[1,1,1,2,3,3,3],'augment id');
is_deeply($hashref->{'dofv'},[3,3,3,undef,5,5,5],'augment dofv');
is_deeply($hashref->{'raw'}->[0],[1,1,1],'augment rawres 0');
is_deeply($hashref->{'raw'}->[1],[1,1,1],'augment rawres 1');
is_deeply($hashref->{'raw'}->[3],[2,2,2],'augment rawres 3');
is_deeply($hashref->{'raw'}->[4],[3,3,3],'augment rawres 4');


my $outobj = output->new(filename => $dir.'for_cv_matrix.lst');
$parameter_hash = output::get_nonmem_parameters(output => $outobj);
my $rsetheta ='3,3,30,10,10,3,3,50,10,30,10,10,10,10,10,10,10,10,50,10,10,10';
my $rseomega ='5,5,10,5,5,20,30,10,10,10,30,30,10,30,30';
my $rsesigma ='5,5,5,5';
	
$ref = tool::sir::setup_variancevec_from_rse(rse_theta => $rsetheta,
	rse_omega=> $rseomega,
	rse_sigma=> $rsesigma,
	parameter_hash => $parameter_hash);

my $covmatrix = tool::sir::setup_covmatrix_from_variancevec(variance => $ref);
ok(linear_algebra::is_matrix_posdef(matrix => $covmatrix), 'rse covmatrix from neg covariance is posdef');

my $arr = tool::sir::setup_auto_cholesky_block_posdef_check(
	param=>['theta','theta','theta','theta','theta','theta','theta',
			'theta','theta','theta','theta','theta'],
	labels =>['SD_A01','SD_A02','COR_A0201',
			  'SD_B1','SD_B2','SD_B3','COR_B31','COR_B32',
			  'SD_C1','SD_C2',
			  'log SD_D1','logit (COR_D21+1)/2',
	],
	fix_theta_labels => ['log SD_D2','COR_B21']);

is(scalar(@{$arr}),3,'setup auto chol block count');
is($arr->[0]->{'size'},2,'setup auto chol block size 1');
is($arr->[0]->{'bounded'},1,'setup auto chol block bounded 1');
is_deeply($arr->[0]->{'indices'},[0,2,1],
		  'setup auto chol block indices 1');
is($arr->[1]->{'size'},3,'setup auto chol block size 2');
is($arr->[1]->{'bounded'},1,'setup auto chol block bounded 2');
is_deeply($arr->[1]->{'indices'},[3,-2,4,6,7,5],
		  'setup auto chol block indices 2');
is($arr->[2]->{'size'},2,'setup auto chol block size 3');
is($arr->[2]->{'bounded'},0,'setup auto chol block bounded 3');
is_deeply($arr->[2]->{'indices'},[10,11,-1],
	'setup auto chol block indices 3');

my @aok = (0.3,1.2,0.5);
my @abad=(0.3,1.2,1);
my @bok =(1,2,3,0.1,0.02); #fix 0.1
my @bbad=(1,2,3,0.9,0.9); #fix 0.01
my @dok=(log(3),0.6); #fix log 2
my @dbad=(log(3),-100); #fix log 2

is(tool::sir::check_auto_cholesky_blocks_posdef(hash_array => $arr,
												fix_xvec => [log(2),0.1],
												xvec => [@aok,@bok,1,1,@dok]),1,'auto chol accept 1');

is(tool::sir::check_auto_cholesky_blocks_posdef(hash_array => $arr,
												fix_xvec => [log(2),0.1],
												xvec => [@abad,@bok,1,1,@dok]),0,'auto chol accept 2');
is(tool::sir::check_auto_cholesky_blocks_posdef(hash_array => $arr,
												fix_xvec => [log(2),0.01],
												xvec => [@aok,@bbad,1,1,@dok]),0,'auto chol accept 3');
is(tool::sir::check_auto_cholesky_blocks_posdef(hash_array => $arr,
												fix_xvec => [log(2),0.1],
												xvec => [@aok,@bok,1,1,@dbad]),0,'auto chol accept 4');

remove_test_dir($tempdir);

done_testing();
