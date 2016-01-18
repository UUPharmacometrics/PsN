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

our $tempdir = create_test_dir('unit_frem');
chdir($tempdir);
my $modeldir = $includes::testfiledir;
my $model = model->new(filename => "$modeldir/mox_no_bov.mod", 
					   ignore_missing_data => 1);

my ($corr,$message) = tool::frem::get_correlation_matrix_from_phi(start_eta_1 => 1,
																  end_eta_1 => 3,
																  model => $model);
my $ans=[[0.6389949068,0.9566914852,0.1573687298],
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

($corr,$message) = tool::frem::get_correlation_matrix_from_phi(start_eta_1 => 1,
																  end_eta_1 => 3,
																  start_eta_2 => 2,
																  end_eta_2 => 3,
																  model => $model);
is ($message," Input error end_eta_1, start 2, end eta 2: 3,2, 3",'input error get_correlation_matrix');

my $model = model->new(filename => "$modeldir/mox1.mod", 
					   ignore_missing_data => 1);
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

my ($block,$message) = tool::frem::get_filled_omega_block(model => $model,
														  start_etas => [1,3],
														  end_etas => [2,3]);


$ans=[[4.08636E-01,sqrt(4.08636E-01)*sqrt(1.10186)*0.7853246843,sqrt(4.08636E-01)*sqrt(2.07708E-01)*0.0428450599],
	  [sqrt(4.08636E-01)*sqrt(1.10186)*0.7853246843,1.10186E+00, -2.37832958e-03],
	  [sqrt(4.08636E-01)*sqrt(2.07708E-01)*0.0428450599,-2.37832958e-03, 2.07708E-01]];

cmp_float_matrix($block,$ans,'get_filled_omega_block 1');

($block,$message) = tool::frem::get_filled_omega_block(model => $model,
														  start_etas => [1,2,3],
														  end_etas => [1,2,3]);

$ans=[[4.08636E-01,1E-07,sqrt(4.08636E-01)*sqrt(2.07708E-01)*0.0428450599],
	  [1E-07,1.10186E+00, -2.37832958e-03],
	  [sqrt(4.08636E-01)*sqrt(2.07708E-01)*0.0428450599,-2.37832958e-03, 2.07708E-01]];

cmp_float_matrix($block,$ans,'get_filled_omega_block 2');


$model = model->new(filename => "$modeldir/mox_no_bov.mod", 
					ignore_missing_data => 1);

my $bov_parameters = ['PHI','LAG','CL','V','KA'];
my ($ctv_parameters,$etanum_to_parameterhash) =  tool::frem::get_CTV_parameters(model=> $model,
																				bov_parameters => $bov_parameters);
is_deeply($ctv_parameters,['CL','KA','LAG','PHI','V'],"get_CTV_parameters ctvparams"); 
is(scalar(keys(%{$etanum_to_parameterhash})),3,"get_CTV_parameters length hash keys");
is($etanum_to_parameterhash->{1},'CL',"get_CTV_parameters etanum_to_parameterhash 1");
is($etanum_to_parameterhash->{2},'V',"get_CTV_parameters etanum_to_parameterhash 2");
is($etanum_to_parameterhash->{3},'KA',"get_CTV_parameters etanum_to_parameterhash 3");

my $bov_parameters = ['PHI','KA'];
my ($ctv_parameters,$etanum_to_parameterhash) =  tool::frem::get_CTV_parameters(model=> $model,
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

my ($filtered_data_model,$filter_table_header,$extra_input_items,$message) = 
	tool::frem::create_data2_model( model => $model,
									filename => 'filterdata.mod',
									filtered_datafile => 'filtertype0.dta',
									bov_parameters => scalar(@{$bov_parameters}), 
									dv  => 'DV',
									time_varying  => $time_varying,
									covariates  => $invariant,
									occasion  => 'VISI');

my $formatted = $filtered_data_model->problems->[0]->tables->[0]->_format_record;
my @ans = qw (ID VISI XAT2 DGRP DOSE FLAG ONO XIME DVO NEUY SCR AGE SEX NYHA WT COMP ACE DIG DIU NUMB TAD TIME VIDD CLCR AMT SS II VID CMT CONO DV EVID OVID FREMTYPE NOAPPEND);
for (my $i=0; $i<scalar(@ans); $i++){
	is($filtered_data_model->problems->[0]->tables->[0]->options->[$i]->name, $ans[$i], 'table record options '.$i);
}
@ans = qw (ID VISI XAT2 DGRP DOSE FLAG ONO XIME DVO NEUY SCR AGE SEX NYHA WT COMP ACE DIG DIU NUMB TAD TIME VIDD CLCR AMT SS II VID CMT CONO DV EVID OVID FREMTYPE);
is_deeply($filter_table_header,\@ans,'filter table header');
is_deeply($extra_input_items,['FREMTYPE'],'extra_input_items create_data2');
is($message,"Running dummy model to filter data and add FREMTYPE for Data set 2",'create_data2 message');

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

my $code = ['CL=THETA(2)*EXP(ETA(2))',
			'V=TVV+ETA(33)',
			'KA=THETA(4)*EXP(1+ETA(4))',
			'Y=ETA(5)+ETA(6)'];
my $ans = ['CL=THETA(2)*EXP(ETA(7))',
			'V=TVV+ETA(33)',
			'KA=THETA(4)*EXP(1+ETA(8))',
			'Y=ETA(10)+ETA(11)'];

tool::frem::renumber_etas(code => $code,
						  eta_from => [[2,4,3,5,6]],
						  eta_to => [[7,8,9,10,11]]);
is($code->[0],$ans->[0],'renumber etas 1');
is($code->[1],$ans->[1],'renumber etas 2');
is($code->[2],$ans->[2],'renumber etas 3');
is($code->[3],$ans->[3],'renumber etas 4');
done_testing();
