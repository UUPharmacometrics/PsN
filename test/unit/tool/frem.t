#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

use tool::frem;
use model;
use ui;

my ($d1,$d2,$d3)= get_major_minor_nm_version;
ui->silent(1);

our $tempdir = create_test_dir('unit_frem');
chdir($tempdir);

my @old =([4,0,2],
		  [0,1,0],
		  [2,0,5]);
my @ans =([4,0.2,2],
		  [0.2,1,0.1*sqrt(5)],
		  [2,0.1*sqrt(5),5]);

my $newmatrix = tool::frem::replace_0_correlation(old_matrix => \@old,
									  is_covariance => 1,
									  low_correlation => 0.1);
is_deeply($newmatrix,\@ans,'replace 0 correlation covariance');

@old =([1,0,0.2],
	   [0,1,0],
	   [0.2,0,1]);
@ans =([1,0.1,0.2],
	   [0.1,1,0.1],
	   [0.2,0.1,1]);


$newmatrix = tool::frem::replace_0_correlation(old_matrix => \@old,
									  is_covariance => 0,
									  low_correlation => 0.1);
is_deeply($newmatrix,\@ans,'replace 0 correlation correlation');


my ($labels,$thetas,$code,$pkcode) = tool::frem::get_pred_error_pk_code(covariates => ['AGE','SEX','WT'],
																		maxeta => 3,
																		rescale => 0,
																		mu => 0,
																		use_pred => 0,
																		invariant_covmatrix => [],
																		invariant_mean => [54.4, 0, 89.5],
																		estimate_mean => [0,0,0],
																		ntheta => 5,
																		N_parameter_blocks => 1,
																		epsnum => 2,
																		indent => ' ');
is_deeply($pkcode,[],'get_pred_error_pk_code eta pkcode');
is_deeply($labels,['BSV_AGE','BSV_SEX','BSV_WT'],'get_pred_error_pk_code eta labels');
is_deeply($thetas,[' 54.4 FIX ; TV_AGE',' 0 FIX ; TV_SEX',' 89.5 FIX ; TV_WT'],'get_pred_error_pk_code thetastrings');
my @errcode =(
	';;;FREM CODE BEGIN COMPACT',
	';;;DO NOT MODIFY',
	' IF (FREMTYPE.EQ.100) THEN',
	';   AGE  1',
	'    Y = THETA(6) + ETA(4) + EPS(2)',
	'    IPRED = THETA(6) + ETA(4)',
	' END IF',
	' IF (FREMTYPE.EQ.200) THEN',
	';   SEX  1',
	'    Y = THETA(7) + ETA(5) + EPS(2)',
	'    IPRED = THETA(7) + ETA(5)',
	' END IF',
	' IF (FREMTYPE.EQ.300) THEN',
	';   WT  1',
	'    Y = THETA(8) + ETA(6) + EPS(2)',
	'    IPRED = THETA(8) + ETA(6)',
	' END IF',
	';;;FREM CODE END COMPACT'
);
is_deeply($code,\@errcode,'get_pred_error_pk_code code');


($labels,$thetas,$code,$pkcode) = tool::frem::get_pred_error_pk_code(covariates => ['AGE','SEX','WT'],
																		maxeta => 3,
																		rescale => 0,
																		mu => 1,
																		use_pred => 0,
																		invariant_covmatrix => [],
																		invariant_mean => [54.4, 0, 89.5],
																		estimate_mean => [0,0,0],
																		ntheta => 5,
																		N_parameter_blocks => 1,
																		epsnum => 2,
																	 indent => ' ');
my @mucode=(
	' MU_4 = THETA(6)',
	' COV4 = MU_4 + ETA(4)',
	' MU_5 = THETA(7)',
	' COV5 = MU_5 + ETA(5)',
	' MU_6 = THETA(8)',
	' COV6 = MU_6 + ETA(6)',
	);
is_deeply($pkcode,\@mucode,'get_pred_error_pk_code eta pkcode mu pk');
is_deeply($labels,['BSV_AGE','BSV_SEX','BSV_WT'],'get_pred_error_pk_code eta labels mu pk');
is_deeply($thetas,[' 54.4 FIX ; TV_AGE',' 0 FIX ; TV_SEX',' 89.5 FIX ; TV_WT'],'get_pred_error_pk_code thetastrings mu pk');
@errcode =(
	';;;FREM CODE BEGIN COMPACT',
	';;;DO NOT MODIFY',
	' IF (FREMTYPE.EQ.100) THEN',
	';   AGE  1',
	'    Y = COV4 + EPS(2)',
	'    IPRED = COV4',
	' END IF',
	' IF (FREMTYPE.EQ.200) THEN',
	';   SEX  1',
	'    Y = COV5 + EPS(2)',
	'    IPRED = COV5',
	' END IF',
	' IF (FREMTYPE.EQ.300) THEN',
	';   WT  1',
	'    Y = COV6 + EPS(2)',
	'    IPRED = COV6',
	' END IF',
	';;;FREM CODE END COMPACT'
);
is_deeply($code,\@errcode,'get_pred_error_pk_code code mu pk');




($labels,$thetas,$code,$pkcode) = tool::frem::get_pred_error_pk_code(covariates => ['AGE','SEX','WT'],
																	 maxeta => 3,
																	 rescale => 1,
																	 mu => 0,
																	 use_pred => 0,
																	 invariant_covmatrix => [[9,2,1],[2,4,0],[1,0,2]],
																	 invariant_mean => [54.4, 0, 89.5],
																	 estimate_mean => [1,1,0],
																	 ntheta => 5,
																	 N_parameter_blocks => 1,
																	 epsnum => 2,
																	 indent => ' ');
is_deeply($pkcode,[],'get_pred_error_pk_code eta pkcode 2');
is_deeply($labels,['BSV_AGE','BSV_SEX','BSV_WT'],'get_pred_error_pk_code eta labels 2');
is_deeply($thetas,[' 54.4 ; TV_AGE',' 0.001 ; TV_SEX',' 89.5 FIX ; TV_WT'],'get_pred_error_pk_code thetastrings 2'); 
@errcode =(
	';;;FREM CODE BEGIN COMPACT',
	';;;DO NOT MODIFY',
	' IF (FREMTYPE.EQ.100) THEN',
	';   AGE  3',
	'    Y = THETA(6) + ETA(4)*3 + EPS(2)',
	'    IPRED = THETA(6) + ETA(4)*3',
	' END IF',
	' IF (FREMTYPE.EQ.200) THEN',
	';   SEX  2',
	'    Y = THETA(7) + ETA(5)*2 + EPS(2)',
	'    IPRED = THETA(7) + ETA(5)*2',
	' END IF',
	' IF (FREMTYPE.EQ.300) THEN',
	';   WT  1.41421356237',
	'    Y = THETA(8) + ETA(6)*1.41421356237 + EPS(2)',
	'    IPRED = THETA(8) + ETA(6)*1.41421356237',
	' END IF',
	';;;FREM CODE END COMPACT'
);
is_deeply($code,\@errcode,'get_pred_error_pk_code code 2');

($labels,$thetas,$code,$pkcode) = tool::frem::get_pred_error_pk_code(covariates => ['AGE','SEX','WT'],
																	 maxeta => 3,
																	 rescale => 1,
																	 mu => 1,
																	 use_pred => 1,
																	 invariant_covmatrix => [[9,2,1],[2,4,0],[1,0,2]],
																	 invariant_mean => [54.4, 0, 89.5],
																	 estimate_mean => [1,1,0],
																	 ntheta => 5,
																	 N_parameter_blocks => 1,
																	 epsnum => 2,
																	 indent => ' ');
is_deeply($pkcode,[],'get_pred_error_pk_code eta pkcode mu pred rescale');
is_deeply($labels,['BSV_AGE','BSV_SEX','BSV_WT'],'get_pred_error_pk_code eta labels mu pred rescale');
is_deeply($thetas,[' 54.4 ; TV_AGE',' 0.001 ; TV_SEX',' 89.5 FIX ; TV_WT'],'get_pred_error_pk_code thetastrings mu pred rescale'); 
@errcode =(
	';;;FREM CODE BEGIN COMPACT',
	';;;DO NOT MODIFY',
	' MU_4 = THETA(6)',
	' COV4 = MU_4 + ETA(4)*3',
	' MU_5 = THETA(7)',
	' COV5 = MU_5 + ETA(5)*2',
	' MU_6 = THETA(8)',
	' COV6 = MU_6 + ETA(6)*1.41421356237',
	' IF (FREMTYPE.EQ.100) THEN',
	';   AGE  3',
	'    Y = COV4 + EPS(2)',
	'    IPRED = COV4',
	' END IF',
	' IF (FREMTYPE.EQ.200) THEN',
	';   SEX  2',
	'    Y = COV5 + EPS(2)',
	'    IPRED = COV5',
	' END IF',
	' IF (FREMTYPE.EQ.300) THEN',
	';   WT  1.41421356237',
	'    Y = COV6 + EPS(2)',
	'    IPRED = COV6',
	' END IF',
	';;;FREM CODE END COMPACT'
);
is_deeply($code,\@errcode,'get_pred_error_pk_code code mu pred rescale');








my $modeldir = $includes::testfiledir;
my $model = model->new(filename => "$modeldir/frem/mox_imp_4.mod", 
					   ignore_missing_data => 1);

my ($diag,$offdiag) = tool::frem::get_phi_coltypes(model => $model);
is($diag,'PHI','phi diag coltype classic');
is($offdiag,'PHC','phi offdiag coltype classic');

$model = model->new(filename => "$modeldir/frem/model_4.mod", 
					   ignore_missing_data => 1);

($diag,$offdiag) = tool::frem::get_phi_coltypes(model => $model);
is($diag,'ETA','phi diag coltype classic');
is($offdiag,'ETC','phi offdiag coltype classic');

my ($covnames,$rescaling,$omegaindex,$parnames,$size,$cov_means)=tool::frem::get_post_processing_data(model => $model);
is_deeply($covnames,['WT','SEX'],'frem post covnames 1');
is_deeply($rescaling,[1,1],'frem post rescale 1');
is($omegaindex,0,'frem post omegaindex 1');
is_deeply($parnames,['PAR1','PAR2','KA'],'frem post parnames 1');
is($size,5,'frem post size 1');
is_deeply($cov_means,[78.5076,1.20272],'frem post covmeans 1');

$model = model->new(filename => "$modeldir/frem/hamren_4.mod", 
					   ignore_missing_data => 1);
($covnames,$rescaling,$omegaindex,$parnames,$size,$cov_means)=tool::frem::get_post_processing_data(model => $model);
is_deeply($covnames,['AGE','WGT','LBW','CRCO','SEX'],'frem post covnames 2');
is_deeply($rescaling,[10.2318688078,17.738664519,11.2023916302,20.6542819724,0.492904320263],'frem post rescale 2');
is($omegaindex,3,'frem post omegaindex 2');
is_deeply($parnames,['GAM','KINH','HB50','FPG_BASELINE','EC50_FPG','PBO_FPG'],'frem post parnames 2');
is($size,11,'frem post size 2');
is_deeply($cov_means,[57.1553398058,88.9502427184,58.4322815534,69.5191747573,1.41262135922],'frem post covmeans 2');

my $compact_model = model->new(filename => "$modeldir/frem/compact.mod", 
							   ignore_missing_data => 1);

($covnames,$rescaling,$omegaindex,$parnames,$size,$cov_means)=tool::frem::get_post_processing_data(model => $compact_model);
is_deeply($covnames,['AGE','SEX'],'frem post covnames 3');
is_deeply($rescaling,[7.82226906804,0.404756978659],'frem post rescale 3');
is($omegaindex,0,'frem post omegaindex 3');
is_deeply($parnames,['PAR1','PAR2','KA'],'frem post parnames 3');
is($size,5,'frem post size 3');
is_deeply($cov_means,[65.1756756757,1.2027027027],'frem post covmeans 3');


$model = model->new(filename => "$modeldir/mox_no_bov.mod", 
					   ignore_missing_data => 1);


is_deeply(tool::frem::get_reordered_coordinate_strings(problem => $model->problems->[0],
													   omega_order => [1,2]),
		  $model->problems->[0]->get_estimated_attributes(),
		  'get reordered coordinate strings 1');

my $ans = ['THETA1','THETA2','THETA3','THETA4','THETA5',
		   'OMEGA(3,3)','OMEGA(1,1)','OMEGA(2,1)','OMEGA(2,2)'];
is_deeply(tool::frem::get_reordered_coordinate_strings(problem => $model->problems->[0],
													   omega_order => [2,1]),
		  $ans,
		  'get reordered coordinate strings 2');

is_deeply(tool::frem::get_eta_mapping(problem => $model->problems->[0],
									  omega_order => [2,1]),
		  {1 => 2,2=> 3,3=>1},
		  'get_eta_mapping 1');
is_deeply(tool::frem::get_eta_mapping(problem => $model->problems->[0],
									  omega_order => [1,2]),
		  {1 => 1,2=> 2,3=>3},
		  'get_eta_mapping 2');

my ($corr,$message) = tool::frem::get_correlation_matrix_from_phi(start_eta_1 => 1,
																  end_eta_1 => 3,
																  model => $model);
$ans=[[0.6389949068,0.9566914852,0.1573687298],
		 [0.9566914852, 0.996786336,0.0799357334 ],
		 [0.1573687298, 0.0799357334, 0.4438842175]];

cmp_float_matrix($corr,$ans,'get_correlation_matrix_from_phi 1');
is (length($message),0,'no error get_correlation_matrix 1');

($corr,$message) = tool::frem::get_correlation_matrix_from_phi(start_eta_1 => 1,
																  end_eta_1 => 1,
																  start_eta_2 => 3,
																  end_eta_2 => 3,
																  model => $model);
$ans=[[0.6389949068,0.1573687298],
		 [0.1573687298, 0.4438842175]];

cmp_float_matrix($corr,$ans,'get_correlation_matrix_from_phi 2');
is (length($message),0,'no error get_correlation_matrix 2');

my $imp_model = model->new(filename => "$modeldir/frem/mox_imp_4.mod", 
						   ignore_missing_data => 1);
($corr,$message) = tool::frem::get_correlation_matrix_from_phi(start_eta_1 => 2,
															   end_eta_1 => 3,
															   start_eta_2 => 5,
															   end_eta_2 => 5,
															   model => $imp_model);

$ans=[[0.1989847636,0.5241788523,0.5297193498],
	  [0.5241788523,1.3696569641,0.2012698903],
	  [0.5297193498,0.2012698903,0.8842831035 ]];


cmp_float_matrix($corr,$ans,'get_correlation_matrix_from_phi 3, importance sampling two est');
is (length($message),0,'no error get_correlation_matrix 3');



($corr,$message) = tool::frem::get_correlation_matrix_from_phi(start_eta_1 => 1,
																  end_eta_1 => 3,
																  start_eta_2 => 2,
																  end_eta_2 => 3,
																  model => $model);
is ($message," Input error end_eta_1, start 2, end eta 2: 3,2, 3",'input error get_correlation_matrix');

$model = model->new(filename => "$modeldir/mox1.mod", 
					   ignore_missing_data => 1);
my ($arr,$need) = tool::frem::get_new_omega_order(model=> $model,skip_omegas=>[3,4,5,6]);
is($need,1,'get new omega order 1');
is_deeply($arr,[3,4,5,6,1,2],'get new omega order 2');
($arr,$need) = tool::frem::get_new_omega_order(model=> $model,skip_omegas=>[1,3,4,5,6]);
is($need,1,'get new omega order 3');
is_deeply($arr,[1,3,4,5,6,2],'get new omega order 4');

my $bov_param = tool::frem::get_parameters_to_etas(model => $model,
												   use_pred => 0,
												   etas => [2,4]);
is_deeply($bov_param,['V','KPCL'],'get_parameters_to_etas 1');
lives_ok {tool::frem::check_input_bov(input_bov_parameters => $bov_param,parameters_bov => ['KA','KPCL','V']) } " check_input_bov 1";
dies_ok {tool::frem::check_input_bov(input_bov_parameters => $bov_param,parameters_bov => ['KA','V']) } " check_input_bov 2";

$model = model->new(filename => "$modeldir/mox_sir.mod", 
					ignore_missing_data => 1);


is_deeply(tool::frem::get_parameters_to_etas(model => $model,
											 use_pred => 0,
											 etas => [3]),['KA'],'get_parameters_to_etas 2');
		  
$model->update_inits(from_output => $model->outputs->[0]);
my $block;
($block,$message) = tool::frem::get_filled_omega_block(model => $model,
														  start_etas => [1,3],
														  end_etas => [2,3]);


$ans=[[4.08636E-01,5.26964000e-01,1.24823000e-02],
	  [5.26964000e-01,1.10186E+00,-2.37833000e-03],
	  [1.24823000e-02,-2.37833000e-03, 2.07708E-01]];

cmp_float_matrix($block,$ans,'get_filled_omega_block 1');

($block,$message) = tool::frem::get_filled_omega_block(model => $model,
														  start_etas => [1,2,3],
														  end_etas => [1,2,3]);

#$ans=[[4.08636E-01,1E-07,sqrt(4.08636E-01)*sqrt(2.07708E-01)*0.0428450599],
#	  [1E-07,1.10186E+00, -2.37832958e-03],
#	  [sqrt(4.08636E-01)*sqrt(2.07708E-01)*0.0428450599,-2.37832958e-03, 2.07708E-01]];
$ans=[[4.08636E-01,6.71014000e-03,1.24823000e-02],
	  [6.71014000e-03,1.10186E+00, -2.37833000e-03],
	  [1.24823000e-02,-2.37833000e-03, 2.07708E-01]];

cmp_float_matrix($block,$ans,'get_filled_omega_block 2');


$model = model->new(filename => "$modeldir/mox_no_bov.mod", 
					ignore_missing_data => 1);

($arr,$need) = tool::frem::get_new_omega_order(model=> $model,skip_omegas=>[]);
is($need,0,'get new omega order 5');
is_deeply($arr,[1,2],'get new omega order 6');
($arr,$need) = tool::frem::get_new_omega_order(model=> $model,skip_omegas=>[1]);
is($need,0,'get new omega order 7');
is_deeply($arr,[1,2],'get new omega order 8');

my $bov_parameters = ['PHI','LAG','CL','V','KA'];
my ($ctv_parameters,$etanum_to_parameterhash) =  tool::frem::get_CTV_parameters(model=> $model,
																				bov_parameters => $bov_parameters);
is_deeply($ctv_parameters,['CL','KA','LAG','PHI','V'],"get_CTV_parameters ctvparams"); 
is(scalar(keys(%{$etanum_to_parameterhash})),3,"get_CTV_parameters length hash keys");
is($etanum_to_parameterhash->{1},'CL',"get_CTV_parameters etanum_to_parameterhash 1");
is($etanum_to_parameterhash->{2},'V',"get_CTV_parameters etanum_to_parameterhash 2");
is($etanum_to_parameterhash->{3},'KA',"get_CTV_parameters etanum_to_parameterhash 3");

$bov_parameters = ['PHI','KA'];
($ctv_parameters,$etanum_to_parameterhash) =  tool::frem::get_CTV_parameters(model=> $model,
																				bov_parameters => $bov_parameters);
is_deeply($ctv_parameters,['CL','KA','PHI','V'],"get_CTV_parameters ctvparams"); 
is(scalar(keys(%{$etanum_to_parameterhash})),3,"get_CTV_parameters length hash keys");
is($etanum_to_parameterhash->{1},'CL',"get_CTV_parameters etanum_to_parameterhash 1");
is($etanum_to_parameterhash->{2},'V',"get_CTV_parameters etanum_to_parameterhash 2");
is($etanum_to_parameterhash->{3},'KA',"get_CTV_parameters etanum_to_parameterhash 3");

my $time_varying = ['CLCR','WT'];
my $invariant = ['AGE','SEX'];

my $start_omega_record=	tool::frem::get_start_numbers(model=>$model,
													  skip_etas => 0);
is($start_omega_record,1,"get_start_numbers start_omega_record");

my ($parameter_blocks,$eta_mapping) = tool::frem::get_parameter_blocks(model => $model,
														   skip_etas => 0,
														   n_covariates => 4,
														   start_omega_record => $start_omega_record);
is(scalar(@{$parameter_blocks}),2,"get_start_numbers parameter_blocks");
is($parameter_blocks->[0]->options->[0]->coordinate_string,'OMEGA(1,1)','parameter block 1');
is($parameter_blocks->[0]->options->[1]->coordinate_string,'OMEGA(2,1)','parameter block 2');
is($parameter_blocks->[0]->options->[2]->coordinate_string,'OMEGA(2,2)','parameter block 3');
is($parameter_blocks->[1]->options->[0]->coordinate_string,'OMEGA(7,7)','parameter block 4');
is($parameter_blocks->[1]->options->[0]->init,'.3','parameter block 4 init');
is_deeply($eta_mapping->{'eta_from'}->[0],[1,2],"get_start_numbers eta mapping from 1");
is_deeply($eta_mapping->{'eta_from'}->[1],[3],"get_start_numbers eta mapping from 2");
is_deeply($eta_mapping->{'eta_to'}->[0],[1,2],"get_start_numbers eta mapping to 1");
is_deeply($eta_mapping->{'eta_to'}->[1],[7],"get_start_numbers eta mapping to 2");

$start_omega_record=tool::frem::get_start_numbers(model=>$model,
												  skip_etas => 2);

($parameter_blocks,$eta_mapping) = tool::frem::get_parameter_blocks(model => $model,
														n_covariates => scalar(@{$invariant}), 
														skip_etas => 2,
														start_omega_record => $start_omega_record);

is($start_omega_record,2,"get_start_numbers start_omega_record 2");
is(scalar(@{$parameter_blocks}),1,"get_start_numbers parameter_blocks 2");
is($parameter_blocks->[0]->options->[0]->coordinate_string,'OMEGA(3,3)','parameter block 5');
is_deeply($eta_mapping->{'eta_from'}->[0],[3],"get_start_numbers eta mapping from 3");
is_deeply($eta_mapping->{'eta_to'}->[0],[3],"get_start_numbers eta mapping to 3");


my $labelshash = tool::frem::create_labels(covariates => $invariant,
										   bov_parameters => $bov_parameters,
										   time_varying => $time_varying,
										   etanum_to_parameter => $etanum_to_parameterhash,
										   occasionlist => [3,8],
										   occasion => 'VISI',
										   start_eta => 1,
										   bsv_parameters => 3);

is_deeply($labelshash->{'occasion_labels'},['VISI=3','VISI=8'],"create labels occasion");
is_deeply($labelshash->{'bov_par_labels'},['BOV par PHI','BOV par KA'],"create labels bov par");
is_deeply($labelshash->{'bov_cov_labels'},['BOV cov CLCR','BOV cov WT'],"create labels bov cov");
is_deeply($labelshash->{'bsv_par_labels'},['BSV par CL','BSV par V','BSV par KA'],"create labels bsv par");
is_deeply($labelshash->{'bsv_cov_labels'},['BSV cov AGE','BSV cov SEX'],"create labels bsv cov");

$labelshash = tool::frem::create_labels(covariates => $invariant,
										   bov_parameters => $bov_parameters,
										   time_varying => $time_varying,
										   etanum_to_parameter => $etanum_to_parameterhash,
										   occasionlist => [3,8],
										   occasion => 'VISI',
										   start_eta => 3,
										   bsv_parameters => 1);

is_deeply($labelshash->{'occasion_labels'},['VISI=3','VISI=8'],"create labels occasion");
is_deeply($labelshash->{'bov_par_labels'},['BOV par PHI','BOV par KA'],"create labels bov par");
is_deeply($labelshash->{'bov_cov_labels'},['BOV cov CLCR','BOV cov WT'],"create labels bov cov");
is_deeply($labelshash->{'bsv_par_labels'},['BSV par KA'],"create labels bsv par");
is_deeply($labelshash->{'bsv_cov_labels'},['BSV cov AGE','BSV cov SEX'],"create labels bsv cov");



my $full = tool::frem::create_full_block(top_block => [[3,1],[1,4]],
										 bottom_block => [[2,0.4,0.2],[0.4,4,0.1],[0.2,0.1,5]],
										 correlation => 0.01);
is_deeply($full->[0],[3,1,sqrt(3)*sqrt(2)*0.01,sqrt(3)*sqrt(4)*0.01 ,sqrt(3)*sqrt(5)*0.01],"create full_block 0");
is_deeply($full->[1],[1,4,sqrt(4)*sqrt(2)*0.01,sqrt(4)*sqrt(4)*0.01 ,sqrt(4)*sqrt(5)*0.01],"create full_block 1");
is_deeply($full->[2],[sqrt(3)*sqrt(2)*0.01,sqrt(4)*sqrt(2)*0.01 ,2,0.4,0.2],"create full_block 2");
is_deeply($full->[3],[sqrt(3)*sqrt(4)*0.01,sqrt(4)*sqrt(4)*0.01 ,0.4,4,0.1],"create full_block 3");
is_deeply($full->[4],[sqrt(3)*sqrt(5)*0.01,sqrt(4)*sqrt(5)*0.01 ,0.2,0.1,5],"create full_block 4");

my ($filtered_data_model,$filter_table_header,$extra_input_items); 
($filtered_data_model,$filter_table_header,$extra_input_items,$message) = 
	tool::frem::create_data2_model( model => $model,
									filename => 'filterdata.mod',
									filtered_datafile => 'filtertype0.dta',
									use_pred => 0,
									bov_parameters => scalar(@{$bov_parameters}), 
									dv  => 'DV',
									time_varying  => $time_varying,
									covariates  => $invariant,
									occasion  => 'VISI');

my $formatted = $filtered_data_model->problems->[0]->tables->[0]->_format_record;
@ans = qw (ID VISI XAT2 DGRP DOSE FLAG ONO XIME DVO NEUY SCR AGE SEX NYHA WT COMP ACE DIG DIU NUMB TAD TIME VIDD CLCR AMT SS II VID CMT CONO DV EVID OVID FREMTYPE NOAPPEND);
for (my $i=0; $i<scalar(@ans); $i++){
	is($filtered_data_model->problems->[0]->tables->[0]->options->[$i]->name, $ans[$i], 'table record options '.$i);
}
@ans = qw (ID VISI XAT2 DGRP DOSE FLAG ONO XIME DVO NEUY SCR AGE SEX NYHA WT COMP ACE DIG DIU NUMB TAD TIME VIDD CLCR AMT SS II VID CMT CONO DV EVID OVID FREMTYPE);
is_deeply($filter_table_header,\@ans,'filter table header');
is_deeply($extra_input_items,['FREMTYPE'],'extra_input_items create_data2');
is($message,"Running dummy model to filter data and add FREMTYPE for FREM data set",'create_data2 message');

my $first_timevar_type;
#is($first_timevar_type,scalar(@{$invariant})+1,'first timevar type create_data2');
my $indices = tool::frem::get_indices(target=>$filter_table_header, keys => ['VISI','EVID','MDV','FREMTYPE']);
is($indices->{'VISI'},1,'occ index');
is($indices->{'EVID'},31,'evid index');
is($indices->{'MDV'},undef,'mdv index');
is($indices->{'FREMTYPE'},33,'type index');


$model = model->new(filename => "$modeldir/pheno.mod", 
					ignore_missing_data => 1);
my @code = ('TVCL=THETA(1)','TVV=THETA(2)','CL=TVCL*EXP(ETA(1))','V=TVV*EXP(ETA(2))','S1=V');
$model->set_code(record => 'pk', code => \@code);

tool::frem::replace_tvpar_with_ctvpar( model => $model, 
									   ctvpar => ['V']);

@code = @{$model->get_code(record => 'pk')};
is_deeply(\@code,['TVCL=THETA(1)','TVV=CTVV','CL=TVCL*EXP(ETA(1))','V=TVV*EXP(ETA(2))','S1=V'],"replace tvpar with ctvpar");


my @head = qw (ID AGE SEX NYHA WT TYPE CLCR DV EVID OVID FREMTYPE);
my @newhead = qw (ID AGE SEX NYHA WT TYPE CLCR DV EVID OVID FREMTYPE NYHA_2 NYHA_1 TYPE_2 TYPE_1 TYPE_0);


my $reg = tool::frem::get_regular_covariates(categorical => ['SEX','NYHA','TYPE'],
											 covariates => ['WT','SEX','NYHA','CLCR','TYPE'],
											 log => []);

is_deeply($reg,['WT','CLCR'],'get regular cov');
#set_frem_records FIXME
#more dataset handling FIXME
#set_frem_code FIXME



$model = model->new(filename => "$modeldir/mox1.mod", 
					ignore_missing_data => 1);

@code = @{$model->problems->[0]->pks->[0]->code};
is ($code[9],'   KPCL  = VIS3*ETA(4)+VIS8*ETA(5)'."\n",' pk before renumbering line 9');
is ($code[10],'   KPKA  = VIS3*ETA(6)+VIS8*ETA(7)'."\n",' pk before renumbering line 10');
is ($code[17],'   CL    = TVCL*EXP(ETA(1)+KPCL)'."\n",' pk before renumbering line 17');
is ($code[18],'   V     = TVV*EXP(ETA(2))'."\n",' pk before renumbering line 18');
is ($code[19],'   KA    = THETA(3)*EXP(ETA(3)+KPKA)'."\n",'pk before renumbering line 19');


my $input_model_fix_omegas = tool::frem::get_or_set_fix(model => $model,
														type => 'omegas');

my ($new_omega_order,$need_to_move_omegas)=tool::frem::get_new_omega_order(model =>$model,
																		   skip_omegas => [3,4,5,6]);
is($need_to_move_omegas,1,'need to move 1');
is_deeply($new_omega_order,[3,4,5,6,1,2],'new order 1');
my ($skip_etas,$fix_omegas,$parameter_etanumbers) = 
	tool::frem::put_skipped_omegas_first(model => $model,
										 new_omega_order => $new_omega_order,
										 need_to_move => $need_to_move_omegas,
										 start_omega_record => 5,
										 input_model_fix_omegas => $input_model_fix_omegas);

is($skip_etas,4,'put skipped first skip_etas');
is_deeply($parameter_etanumbers,[[5,6],[7]],'put skipped first parameter_etanumbers');
is($model->problems->[0]->omegas->[0]->same,0,'reordered model record 1 not same');
is($model->problems->[0]->omegas->[1]->same,1,'reordered model record 2 same');
is($model->problems->[0]->omegas->[2]->same,0,'reordered model record 3 not same');
is($model->problems->[0]->omegas->[3]->same,1,'reordered model record 4 same');

@code = @{$model->problems->[0]->pks->[0]->code};
is ($code[9],'   KPCL  = VIS3*ETA(1)+VIS8*ETA(2)'."\n",' pk renumbered line 9');
is ($code[10],'   KPKA  = VIS3*ETA(3)+VIS8*ETA(4)'."\n",' pk renumbered line 10');
is ($code[17],'   CL    = TVCL*EXP(ETA(5)+KPCL)'."\n",' pk renumbered line 17');
is ($code[18],'   V     = TVV*EXP(ETA(6))'."\n",' pk renumbered line 18');
is ($code[19],'   KA    = THETA(3)*EXP(ETA(7)+KPKA)'."\n",'pk renumbered line 19');

$model = model->new(filename => "$modeldir/pheno.mod", 
					ignore_missing_data => 1);


$input_model_fix_omegas = tool::frem::get_or_set_fix(model => $model,
														type => 'omegas');
($new_omega_order,$need_to_move_omegas)=tool::frem::get_new_omega_order(model =>$model,
																		   skip_omegas => []);
is($need_to_move_omegas,0,'need to move 0');
is_deeply($new_omega_order,[1],'new order 0');

($skip_etas,$fix_omegas,$parameter_etanumbers) = 
	tool::frem::put_skipped_omegas_first(model => $model,
										 new_omega_order => $new_omega_order,
										 need_to_move => $need_to_move_omegas,
										 start_omega_record => 1,
										 input_model_fix_omegas => $input_model_fix_omegas);

is_deeply($fix_omegas,[],' put skipped first fix omegas');
is($skip_etas,0,'put skipped first skip_etas 2');
is_deeply($parameter_etanumbers,[[1],[2]],'put skipped first parameter_etanumbers');
is($model->problems->[0]->omegas->[0]->size,1,'reordered model record 1 size 1');
is($model->problems->[0]->omegas->[1]->size,1,'reordered model record 2 size 1');
is($model->problems->[0]->omegas->[0]->type,'BLOCK','reordered model record 1 size 1');
is($model->problems->[0]->omegas->[1]->type,'BLOCK','reordered model record 2 size 1');

sub countN{
	my $est = shift;
	my $se =shift;
	return (1+2*(($est/$se)**2));
}
#unshift @INC, File::Spec->catfile($includes::testfiledir, 'output');
#require answers;

#our $test_files = File::Spec->catfile($includes::testfiledir, 'output');

my $output = output -> new ('filename'=> $modeldir.'/frem/model_4.lst');
my $model1 = model->new(filename => "$modeldir/frem/model_1.mod", 
					   ignore_missing_data => 1);
my $model2 = model->new(filename => "$modeldir/frem/model_2.mod", 
					   ignore_missing_data => 1);


$ans = $output->perfect_individual_count();

my $true = {1=>countN(8.12648E-02,1.29305E-02),
			2=>countN(2.85876E+00,7.23777E-01),
			3=>countN(5.91259E-02,1.40023E-02),
			4=>countN(2.46471E+02,4.12291E+01),
			5=>countN(1.61613E-01,2.77878E-02)};
	
is($ans->{1},$true->{1},'perfect individual count 1');
is($ans->{2},$true->{2},'perfect individual count 2');
is($ans->{3},$true->{3},'perfect individual count 3');
is($ans->{4},$true->{4},'perfect individual count 4');
is($ans->{5},$true->{5},'perfect individual count 5');

$ans = tool::frem::perfect_individuals(output1=>$model1->outputs->[0],
									   output2=> $model2->outputs->[0], 
									   omega_order1 => [1,2]);
my $true2 = {1=>countN(8.15324E-02,1.27723E-02),   
			 2=>countN(2.79894E+00,8.12892E-01),     
			 3=>countN(5.22103E-02,1.33110E-02),
			 4=>countN(2.46465E+02,4.12278E+01),
			 5=>countN(1.61606E-01,2.77883E-02)};

is($ans->{1},$true2->{1},'perfect individual  1');
is($ans->{2},$true2->{2},'perfect individual  2');
is($ans->{3},$true2->{3},'perfect individual  3');
is($ans->{4},$true2->{4},'perfect individual  4');
is($ans->{5},$true2->{5},'perfect individual  5');

$ans = tool::frem::perfect_individuals(output1=>$model1->outputs->[0],output2=> $model2->outputs->[0], omega_order1 => [2,1]);
is($ans->{1},$true2->{3},'perfect individual  6');
is($ans->{2},$true2->{1},'perfect individual  7');
is($ans->{3},$true2->{2},'perfect individual  8');
is($ans->{4},$true2->{4},'perfect individual  9');
is($ans->{5},$true2->{5},'perfect individual  10');


my $matrix = [[1,2,3,4,5],
			  [2,6,7,8,9],
			  [3,7,10,11,12],
			  [4,8,11,13,14],
			  [5,9,12,14,15]];

$ans = tool::frem::reorder_covmatrix(matrix => $matrix,
										original_strings => ['A','B','C','D','E'],
										reordered_strings => ['A','C','D','B','E']);

my $reor = [
	[1,3,4,2,5],
	[3,10,11,7,12],
	[4,11,13,8,14],
	[2,7,8,6,9],
	[5,12,14,9,15]
];
is_deeply($ans,$reor,'reorder covmatrix 1');

$ans = tool::frem::reorder_covmatrix(matrix => $matrix,
										original_strings => ['A','B','C','D','E'],
										reordered_strings => ['A','B','C','D','E']);
is_deeply($ans,$matrix,'reorder covmatrix 2');


my $str1= ['TH1','TH2','OM11','OM22'];
my $str2= ['TH3','TH4','OM33','OM43','OM44'];
my $fullstr = ['TH1','TH2','TH3','TH4','OM11','OM21','OM22','OM31','OM32','OM33','OM41','OM42','OM43','OM44'];

my $cov1 = [[1,2,3,4],
			[2,5,6,7],
			[3,6,8,9],
			[4,7,9,10]];

my $cov2 = [[11,12,13,14,15],
			[12,16,17,18,19],
			[13,17,20,21,22],
			[14,18,21,23,24],
			[15,19,22,24,25]];

my $join = [
	[1,2,0,0  ,3,0,4 ,0,0,0 ,0,0,0 ,0],
	[2,5,0,0  ,6,0,7 ,0,0,0 ,0,0,0 ,0],
	[0,0,11,12,0,0,0 ,0,0,13,0,0,14,15],
	[0,0,12,16,0,0,0 ,0,0,17,0,0,18,19],
	[3,6, 0, 0,8,0,9 ,0,0,0 ,0,0,0 ,0],
	[0,0, 0, 0,0,0.2,0 ,0,0,0 ,0,0,0 ,0],
	[4,7, 0, 0,9,0,10,0,0,0 ,0,0,0 ,0],
	[0,0, 0, 0,0,0,0 ,0.44,0,0 ,0,0,0 ,0],
	[0,0, 0, 0,0,0,0 ,0,0.5,0 ,0,0,0 ,0],
	[0,0,13,17,0,0,0 ,0,0,20,0,0,21,22],
	[0,0, 0, 0,0,0,0 ,0,0, 0,0.7,0,0 ,0],
	[0,0, 0, 0,0,0,0 ,0,0, 0,0,0.8,0 ,0],
	[0,0,14,18,0,0,0 ,0,0,21,0,0,23,24],
	[0,0,15,19,0,0,0 ,0,0,22,0,0,24,25]	
	];


my $guess_hash = tool::frem::get_variance_guesses(values => [4,1,9,25,2,20],
												  strings => ['OMEGA(1,1)','OMEGA(2,1)','OMEGA(2,2)','OMEGA(3,3)','OMEGA(4,3)','OMEGA(4,4)',],
												  is_omega => 1,
												  perfect_individuals => {1=>4,2=>9,3=>16,4=>25});

cmp_float($guess_hash->{'OMEGA(1,1)'},(4**2+4*4)/4,'variance guess 1');
cmp_float($guess_hash->{'OMEGA(2,1)'},(1+36)/4,'variance guess 2');
cmp_float($guess_hash->{'OMEGA(2,2)'},(9**2+9*9)/9,'variance guess 3');
cmp_float($guess_hash->{'OMEGA(3,3)'},(25**2+25*25)/16,'variance guess 4');
cmp_float($guess_hash->{'OMEGA(4,3)'},(2**2+25*20)/16,'variance guess 5');
cmp_float($guess_hash->{'OMEGA(4,4)'},(20**2+20*20)/25,'variance guess 6');

$ans = tool::frem::join_covmats(full_strings => $fullstr,
								   rse_guess_hash => {'OM11'=> 0.11,'OM21'=> 0.22,'OM22'=> 0.33,'OM31'=> 0.44,
												  'OM32'=> 0.55,'OM33'=> 0.66,'OM41'=> 0.77,'OM42'=> 0.88,'OM43'=> 0.99,'OM44'=> 0.951},
								   variance_guess_hash => {'OM11'=> 0.1,'OM21'=> 0.2,'OM22'=> 0.3,
												  'OM32'=> 0.5,'OM33'=> 0.6,'OM41'=> 0.7,'OM42'=> 0.8,'OM43'=> 0.9,'OM44'=> 0.95},
								   partial_strings =>[$str1,$str2],
								   partial_covmats => [$cov1,$cov2]);

is_deeply($ans,$join,'join covmats');
remove_test_dir($tempdir);



done_testing();
