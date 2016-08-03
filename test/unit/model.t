#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model;
use PsN;			# Need to set PsN version as this is a global variable
$PsN::nm_major_version = 7;
$PsN::nm_minor_version = 1;

my $modeldir = $includes::testfiledir;

my $model = model->new(filename => "$modeldir/scm/pheno_ignore.mod", ignore_missing_data => 1);

is_deeply($model->nomegas(problem_numbers => [1], with_correlations => 1),[2],'nomegas 1');
is_deeply($model->nsigmas(problem_numbers => [1], with_correlations => 1),[1],'nsigmas 1');

is($model->need_data_filtering,1,'need data filtering 1');
# 2 theta 2 omega 1 sigma default METH
is($model->get_option_value(record_name => 'sizes',option_name => 'LTH',fuzzy_match => 0),undef,'input SIZES LTH');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR',fuzzy_match => 0),undef,'input SIZES LVR');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR2',fuzzy_match => 0),undef,'input SIZES LVR2');
is($model->get_option_value(record_name => 'sizes',option_name => 'PD',fuzzy_match => 0),undef,'input SIZES PD');
for (my $i=0; $i< 20; $i++){
	$model->add_records(type => 'omega',
						problem_numbers => [1],
						record_strings => [(1+$i/10).' ; extra'.$i]);
	$model->add_records(type => 'theta',
						problem_numbers => [1],
						record_strings => [(1+$i/10).' ; extra'.$i.'1',
										   (1+$i/10).' ; extra'.$i.'2',
										   (1+$i/10).' ; extra'.$i.'3',
										   (1+$i/10).' ; extra'.$i.'4',
										   (1+$i/10).' ; extra'.$i.'5']);
}
my $message = $model->check_and_set_sizes('LTH' => 1);
is($message,' Need LTH set to at least 102.','set error message');
is($model->get_option_value(record_name => 'sizes',option_name => 'LTH',fuzzy_match => 0),undef,'nm7.1 next SIZES LTH');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR',fuzzy_match => 0),undef,'nm7.1 next SIZES LVR');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR2',fuzzy_match => 0),undef,'nm7.1 next SIZES LVR2');
is($model->get_option_value(record_name => 'sizes',option_name => 'PD',fuzzy_match => 0),undef,'nm7.1 next SIZES PD');

#change to version that support $SIZES
$PsN::nm_major_version = 7;
$PsN::nm_minor_version = 2;
$message = $model->check_and_set_sizes('all' => 1);
is($message,'','set error message 2');
is($model->get_option_value(record_name => 'sizes',option_name => 'LTH',fuzzy_match => 0),102,'next SIZES LTH');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR',fuzzy_match => 0),undef,'next SIZES LVR');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR2',fuzzy_match => 0),undef,'next SIZES LVR2');
is($model->get_option_value(record_name => 'sizes',option_name => 'PD',fuzzy_match => 0),undef,'next SIZES PD');
$model -> add_option(record_name => 'estimation',
					 record_number => 1,
					 option_name => 'METHOD',
					 option_value => 'COND');
$model -> add_option(record_name => 'estimation',
					 record_number => 1,
					 option_name => 'LAPLACE');

$model->check_and_set_sizes('all' => 1);
is($model->get_option_value(record_name => 'sizes',option_name => 'LTH',fuzzy_match => 0),102,'next 2 SIZES LTH');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR',fuzzy_match => 0),undef,'next 2 SIZES LVR');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR2',fuzzy_match => 0),22,'next 2 SIZES LVR2');
is($model->get_option_value(record_name => 'sizes',option_name => 'PD',fuzzy_match => 0),undef,'next 2 SIZES PD');

$model->add_records(type => 'omega',
					problem_numbers => [1],
					record_strings => ['1 ','1 ','1 ','1 ','1 ','1 ','1 ','1 ','1 ','1 ']); #10
$model->check_and_set_sizes('LVR' => 1,'LVR2'=> 1, 'PD' => 1);
is($model->get_option_value(record_name => 'sizes',option_name => 'LTH',fuzzy_match => 0),102,'next 3 SIZES LTH');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR',fuzzy_match => 0),33,'next 3 SIZES LVR');
is($model->get_option_value(record_name => 'sizes',option_name => 'LVR2',fuzzy_match => 0),32,'next 3 SIZES LVR2');
is($model->get_option_value(record_name => 'sizes',option_name => 'PD',fuzzy_match => 0),undef,'next 3 SIZES PD');

$model->remove_records(type => 'sigma',problem_numbers => [1]);
is_deeply($model->nsigmas(problem_numbers => [1], with_correlations => 1),[0],'nsigmas 2');

$model = model->new(filename => "$modeldir/warfarin_saem_noest.mod", ignore_missing_data => 1);

is($model->need_data_filtering,0,'need data filtering 2');

my ( $values_ref, $positions_ref ) = $model ->_get_option_val_pos ( problem_numbers => [1], 
																	name        => 'NR',
																	record_name => 'input',
																	global_position => 1  );
is($positions_ref -> [0],3,"column number in input 1");

#print "is est ".$model->is_estimation(problem_number => 1)."\n";
is ($model->get_estimation_evaluation_problem_number,-1,"get_estimation_evaluation_problem_number saem-foci");

$model = model->new(filename => "$modeldir/pheno.mod");
is($model->need_data_filtering,0,'need data filtering 3');

is ($model->get_estimation_evaluation_problem_number,1,"get_estimation_evaluation_problem_number");

is (scalar(@{$model->problems}), 1, "Check number of problems");
my $problem = $model->problems->[0];
is (scalar(@{$problem->inputs}), 1, "Check number of inputs");
my $options = $model->problems->[0]->inputs->[0]->options;
is (scalar(@{$options}), 6, "Check number of options for inputs");

# Names of input options
my @input_option_names = qw(ID TIME AMT WGT APGR DV);

for (my $i = 0; $i < @input_option_names; $i++) {
	is ($options->[$i]->name, $input_option_names[$i], "Check \$INPUT options name $i");
	is ($options->[$i]->value, '', "Check \$INPUT options value $i");
}

# ouput_files method
my @output_files = qw(pheno.lst pheno.ext pheno.cov pheno.cor pheno.coi pheno.phi pheno.phm pheno.shk pheno.grd pheno.xml pheno.cnv pheno.smt pheno.rmt 
pheno.imp pheno.npd pheno.npe pheno.npi pheno.fgh pheno.log.xml pheno.cpu pheno.shm pheno.agh patab1 phenomsf);

my $files = $model->output_files;

for (my $i = 0; $i < @output_files; $i++) {
	is ($$files[$i], $output_files[$i], "output_files method $i");
}

# get_coordslabels method
my $coordslabels = $model->get_coordslabels(parameter_type => 'theta');
is ((sort keys %{$$coordslabels[0]})[0], 'THETA1', 'get_coordslabels theta 1');
is ((sort keys %{$$coordslabels[0]})[1], 'THETA2', 'get_coordslabels theta 2');
is ((sort values %{$$coordslabels[0]})[0], 'CL', 'get_coordslabels theta 3');
is ((sort values %{$$coordslabels[0]})[1], 'V', 'get_coordslabels theta 4');

$coordslabels = $model->get_coordslabels(parameter_type => 'omega');
is ((sort keys %{$$coordslabels[0]})[0], 'OMEGA(1,1)', 'get_coordslabels omega 1');
is ((sort keys %{$$coordslabels[0]})[1], 'OMEGA(2,2)', 'get_coordslabels omega 2');
is ((sort values %{$$coordslabels[0]})[0], 'IVCL', 'get_coordslabels omega 3');
is ((sort values %{$$coordslabels[0]})[1], 'IVV', 'get_coordslabels omega 4');

# idcolumns method
my $columns = $model->idcolumns(problem_numbers => [0]);

is ($$columns[0], 1, "idcolumns method");

# is_option_set method
ok ($model->is_option_set(record => 'input', name => 'APGR'), "is_option_set \$INPUT");
ok ($model->is_option_set(record => 'estimation', name => 'MAXEVALS'), "is_option_set \$INPUT");
ok (!$model->is_option_set(record => 'input', name => 'OPEL'), "is_option_set \$INPUT");

#setup_filter method
my @header = ('method','model','problem','significant_digits','minimization_successful','covariance_step_successful','ofv');
my @filter = ('minimization_successful.eq.1','significant_digits.gt.4','problem.lt.2','covariance_step_successful.ne.0');
my ($indices,$relations,$value) = model::setup_filter(filter => \@filter, header => \@header);
is_deeply($indices,[4,3,2,5], "setup_filter method, finding columns");
is_deeply($relations,['==','>','<','!='], "setup_filter method, finding relations");
is_deeply($value,[1,4,2,0], "setup_filter method, finding values");

@filter = ('method.eq.bootstrap');
($indices,$relations,$value) = model::setup_filter(filter => \@filter, header => \@header, string_filter => 1);
is ($indices->[0],0,"setup_filter method, method index");
is ($relations->[0],'eq',"setup_filter method, method relation");
is ($value->[0],'bootstrap',"setup_filter method, method value");

#method get_rawres_parameter_indices

#rawres header examples

#1) get rid of quotes
#2) Scan to ofv. Skip deltaofv, if next. If next is FACTOR, handle lasso.  
# skip (sir) "likelihood_ratio","relPDF","importance_ratio","probability_resample","resamples","sample_order",

#find first item that starts with seSOMETHING, count sequence of seOTHERS, store labels. Go backwards to
#check if labels mathc the preceding same length sequence. If yes then found labels. If not then find next sequence of
#seSOMETHING and repeat. End if find shrinkage_eta1(%) If gothrough whole without finding labels then complain

#store labels and column indices for them
#try to add coordinate strings, if possible. look for generic labels with omega and sigma, infer if possible,
#but if diagonal cannot say if previous is diagonal or same row. if off-diagonal will know the rest of the row

my @execute_seheader=("model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","CL","set","IVCL","settwo","SIGMA(1,1)","seCL","seset","seIVCL","sesettwo","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5");

my @execute_header=("model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","CL","V","IVCL","IVV","SIGMA(1,1)","seCL","seV","seIVCL","seIVV","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5");
my @randtest_header=("model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","deltaofv","THETA1","THETA2","CLWGT","IVCL","IVV","SIGMA(1,1)","seTHETA1","seTHETA2","seCLWGT","seIVCL","seIVV","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5","EI6");
my @lasso_header =("model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","FACTOR","LC_CLAPGR","LC_CLWGT","LC_VAPGR","LC_VWGT","CL","V","TH3 CLAPGR","TH4 AL_CLAPGR","TH5 CLWGT","TH6 AL_CLWGT","TH7 VAPGR","TH8 AL_VAPGR","TH9 VWGT","TH10 AL_VWGT","TH11 T-VALUE","IVCL","IVV","SIGMA(1,1)","seCL","seV","seTH3 CLAPGR","seTH4 AL_CLAPGR","seTH5 CLWGT","seTH6 AL_CLWGT","seTH7 VAPGR","seTH8 AL_VAPGR","seTH9 VWGT","seTH10 AL_VWGT","seTH11 T-VALUE","seIVCL","seIVV","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5","EI6","EI7","EI8","EI9","EI10","EI11","EI12","EI13","EI14");
my @cdd_header=("method","model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","THETA1","THETA2","THETA3","THETA4","THETA5","OMEGA(1,1)","OMEGA(2,1)","OMEGA(2,2)","OMEGA(3,3)","SIGMA(1,1)","seTHETA1","seTHETA2","seTHETA3","seTHETA4","seTHETA5","seOMEGA(1,1)","seOMEGA(2,1)","seOMEGA(2,2)","seOMEGA(3,3)","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_eta3(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5","EI6","EI7","EI8","EI9","EI10","cook.scores","jackknife.cook.scores","cov.ratios","outside.n.sd","cdd.delta.ofv","cook.par.THETA1","cook.par.THETA2","cook.par.THETA3","cook.par.THETA4","cook.par.THETA5","cook.par.OMEGA(1,1)","cook.par.OMEGA(2,1)","cook.par.OMEGA(2,2)","cook.par.OMEGA(3,3)","jack.cook.par.THETA1","jack.cook.par.THETA2","jack.cook.par.THETA3","jack.cook.par.THETA4","jack.cook.par.THETA5","jack.cook.par.OMEGA(1,1)","jack.cook.par.OMEGA(2,1)","jack.cook.par.OMEGA(2,2)","jack.cook.par.OMEGA(3,3)");

my @boot_header=("method","model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","CL","V","IVCL","IVV","SIGMA(1,1)","seCL","seV","seIVCL","seIVV","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5");
my @dofv_header=("model","problem","bs_data_id","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","deltaofv","CL","V","IVCL","IVV","SIGMA(1,1)","seCL","seV","seIVCL","seIVV","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5");
my @parallel_retries_header=("model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","CL","V","IVCL","IVV","SIGMA(1,1)","seCL","seV","seIVCL","seIVV","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5");
my @sir_header=("sample.id","model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","deltaofv","likelihood_ratio","relPDF","importance_ratio","probability_resample","resamples","sample_order","CL","V","IVCL","IVV","SIGMA(1,1)","seCL","seV","seIVCL","seIVV","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5");
my @sse_header=("hypothesis","sample","model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","CL","V","TH3","IVCL","IVV","SIGMA(1,1)","seCL","seV","seTH3","seIVCL","seIVV","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5","EI6");


my $hashref = model::get_rawres_parameter_indices(filename => $modeldir.'/raw_results_structure_for_model_test');
is_deeply($hashref->{'theta'},[20,21],'method get_rawres_parameter_indices 1, theta');
is_deeply($hashref->{'omega'},[22,23],'method get_rawres_parameter_indices 1, omega');
is_deeply($hashref->{'sigma'},[24],'method get_rawres_parameter_indices 1, sigma');
$hashref = model::get_rawres_parameter_indices(filename => 'raw_results_structure_for_model_test',
											   directory => $modeldir);
is_deeply($hashref->{'theta'},[20,21],'method get_rawres_parameter_indices 2, theta');
is_deeply($hashref->{'omega'},[22,23],'method get_rawres_parameter_indices 2, omega');
is_deeply($hashref->{'sigma'},[24],'method get_rawres_parameter_indices 2, sigma');

#get_rawres_params method
#here $model must still refer to pheno.mod, otherwise test will fail
my $arr;
($arr,$hashref) = model::get_rawres_params(filename => $modeldir.'/rawres_for_get_rawres_params.csv',
										   string_filter => ['method.eq.bootstrap'],
										   filter => ['significant_digits.gt.4'],
										   require_numeric_ofv => 1,
										   offset => 1,
										   model => $model);
is (scalar(@{$arr}),3,'method get_rawres_params, number of lines returned ');
is($arr->[0]->{'theta'}->{'CL'},1.1,'method get_rawres_params, theta 0');
is($arr->[0]->{'theta'}->{'V'},1.2,'method get_rawres_params, theta 0');
is($arr->[1]->{'theta'}->{'CL'},2.1,'method get_rawres_params, theta 1');
is($arr->[1]->{'theta'}->{'V'},2.2,'method get_rawres_params, theta 1');
is($arr->[2]->{'theta'}->{'CL'},3.1,'method get_rawres_params, theta 2');
is($arr->[2]->{'theta'}->{'V'},3.2,'method get_rawres_params, theta 2');
is($arr->[0]->{'omega'}->{'IVCL'},1.3,'method get_rawres_params, omega 0');
is($arr->[0]->{'omega'}->{'IVV'},1.4,'method get_rawres_params, omega 0');
is($arr->[1]->{'omega'}->{'IVCL'},2.3,'method get_rawres_params, omega 1');
is($arr->[1]->{'omega'}->{'IVV'},2.4,'method get_rawres_params, omega 1');
is($arr->[2]->{'omega'}->{'IVCL'},3.3,'method get_rawres_params, omega 2');
is($arr->[2]->{'omega'}->{'IVV'},3.4,'method get_rawres_params, omega 2');
is($arr->[0]->{'sigma'}->{"SIGMA(1,1)"},1.5,'method get_rawres_params, sigma 0');
is($arr->[1]->{'sigma'}->{"SIGMA(1,1)"},2.5,'method get_rawres_params, sigma 1');
is($arr->[2]->{'sigma'}->{"SIGMA(1,1)"},3.5,'method get_rawres_params, sigma 2');
is($arr->[0]->{'ofv'},752.0400284856,'method get_rawres_params, ofv 0');
is($arr->[1]->{'ofv'},761.1429095868,'method get_rawres_params, ofv 1');
is($arr->[2]->{'ofv'},674.1097668967,'method get_rawres_params, ofv 2');
is($arr->[0]->{'model'},1,'method get_rawres_params, model 0');
is($arr->[1]->{'model'},2,'method get_rawres_params, model 1');
is($arr->[2]->{'model'},5,'method get_rawres_params, model 2');

my $vectorsamples = $model->create_vectorsamples(sampled_params_arr => $arr);
is($vectorsamples->[0]->[0],1.1,'create vectorsamples 0,0');
is($vectorsamples->[0]->[1],1.2,'create vectorsamples 0,1');
is($vectorsamples->[0]->[2],1.3,'create vectorsamples 0,2');
is($vectorsamples->[0]->[3],1.4,'create vectorsamples 0,3');
is($vectorsamples->[0]->[4],1.5,'create vectorsamples 0,4');
is($vectorsamples->[1]->[0],2.1,'create vectorsamples 1,0');
is($vectorsamples->[1]->[1],2.2,'create vectorsamples 1,1');
is($vectorsamples->[1]->[2],2.3,'create vectorsamples 1,2');
is($vectorsamples->[1]->[3],2.4,'create vectorsamples 1,3');
is($vectorsamples->[1]->[4],2.5,'create vectorsamples 1,4');
is($vectorsamples->[2]->[0],3.1,'create vectorsamples 2,0');
is($vectorsamples->[2]->[1],3.2,'create vectorsamples 2,1');
is($vectorsamples->[2]->[2],3.3,'create vectorsamples 2,2');
is($vectorsamples->[2]->[3],3.4,'create vectorsamples 2,3');
is($vectorsamples->[2]->[4],3.5,'create vectorsamples 2,4');

# get_rawres_params with rawres
dies_ok { ($arr,$hashref) = model::get_rawres_params(filename => $modeldir.'/rawres_for_get_rawres_params_duplicated.csv',
										   string_filter => ['method.eq.bootstrap'],
										   require_numeric_ofv => 1,
										   offset => 1,
										   rawres_structure_filename => $modeldir.'/rawres_for_get_rawres_params_structure');
                                   } 'get_rawres_paramas cannot handle duplicated parameter labels';


($arr,$hashref) = model::get_rawres_params(filename => $modeldir.'/rawres_for_get_rawres_params.csv',
										   string_filter => ['method.eq.bootstrap'],
										   require_numeric_ofv => 1,
										   offset => 1,
										   rawres_structure_filename => $modeldir.'/rawres_for_get_rawres_params_structure');
is($arr->[0]->{'theta'}->{'CL'},1.1,'method get_rawres_params, theta 0 b');
is($arr->[0]->{'theta'}->{'V'},1.2,'method get_rawres_params, theta 0 b');
is($arr->[1]->{'omega'}->{'IVCL'},2.3,'method get_rawres_params, omega 1 b');
is($arr->[2]->{'sigma'}->{"SIGMA(1,1)"},0.05,'method get_rawres_params, sigma 2 b');
is($arr->[3]->{'omega'}->{'IVV'},3.4,'method get_rawres_params, omega 1 b');

# set_maxeval_zero


is ($model->get_option_value(record_name => 'estimation', option_name => 'MAXEVALS'), 9997, "before set_maxeval_zero");
$model->set_maxeval_zero;
is ($model->get_option_value(record_name => 'estimation', option_name => 'MAXEVALS'), 0, "before set_maxeval_zero");

$model->add_records( type  => 'estimation', record_strings => ['METHOD=IMP'] );
is ($model->get_option_value(record_name => 'estimation', option_name => 'METHOD', record_index => 1), 'IMP', "added estimation record");

$model->set_maxeval_zero;

ok ($model->is_option_set(record => 'estimation', name => 'POSTHOC'), "set_maxeval_zero: transferred option");
is ($model->problems->[0]->estimations->[1], undef, "set_maxeval_zero: Removed estimation");

# update_inits
$model = model->new(filename => "$modeldir/pheno.mod");

$model->update_inits(from_output_file => "$modeldir/pheno_test.lst");

my $lines = $model->record(record_name => 'sigma');
ok ($lines->[0]->[0] =~ 0.0164, "update_inits: new sigma");

is ($model->problems->[0]->thetas->[0]->options->[0]->lobnd, 0, "update_inits: new theta lobnd");
is ($model->problems->[0]->thetas->[0]->options->[0]->init, 0.00555, "update_inits: new theta init");
is ($model->problems->[0]->thetas->[1]->options->[0]->lobnd, 0, "update_inits: new theta lobnd");
is ($model->problems->[0]->thetas->[1]->options->[0]->init, 1.34, "update_inits: new theta init");

# has_code
$model = model->new(filename => "$modeldir/pheno.mod");

ok ($model->has_code(record => 'pk'), "has_code pk record");
ok (not ($model->has_code(record => 'pred')), "has_code pred record");

# set_code / get_code
$model = model->new(filename => "$modeldir/pheno.mod");

my $code = [ "TSTCDE", "ROW2" ];

$model->set_code(record => 'pk', code => $code);

my $new_code = $model->get_code(record => 'pk');

is ($new_code->[0], $code->[0], "set_code row 0");
is ($new_code->[1], $code->[1], "set_code row 1");

# create_output_filename
my $dummy = model->create_dummy_model;

$dummy->filename('test.mod');
is($dummy->create_output_filename, 'test.lst', "create_output_filename .mod name");
$dummy->filename('test.mod.mod');
is($dummy->create_output_filename, 'test.mod.lst', "create_output_filename multiple .mod extensions");
$dummy->filename('test.ctl');
is($dummy->create_output_filename, 'test.lst', "create_output_filename .ctl name");
$dummy->filename('model');
is($dummy->create_output_filename, 'model.lst', "create_output_filename no extension");

$model = model->new(filename => "$modeldir/mox_sir_block2.mod");
( $values_ref, $positions_ref ) = $model ->_get_option_val_pos ( problem_numbers => [1], 
																	name        => 'NYHA',
																	record_name => 'input',
																	global_position => 1  );
is($positions_ref -> [0],14,"column number in input 2");

is ($model->get_estimation_evaluation_problem_number,1,"get_estimation_evaluation_problem_number");
is_deeply($model->fixed_or_same(parameter_type => 'theta')->[0],[0,0,0,0,0],'fixed or same theta ');
is_deeply($model->fixed_or_same(parameter_type => 'omega')->[0],[0,0,0,0],'fixed or same omega ');
is_deeply($model->same(parameter_type => 'omega')->[0],[0,0,0,0],'same omega 1');
is_deeply($model->fixed_or_same(parameter_type => 'sigma')->[0],[1],'fixed or same sigma ');
is_deeply($model->same(parameter_type => 'sigma')->[0],[0],'same sigma 1');

$model = model->new(filename => "$modeldir/tbs1.mod");
is_deeply($model->fixed_or_same(parameter_type => 'omega')->[0],[0,0,0,0,0,1,0,1],'fixed or same omega 2');
is_deeply($model->fixed_or_same(parameter_type => 'sigma')->[0],[1],'fixed or same sigma 2');
is_deeply($model->same(parameter_type => 'omega')->[0],[0,0,0,0,0,1,0,1],'same omega 2');
is_deeply($model->same(parameter_type => 'sigma')->[0],[0],'same sigma 2');

$model = model->new(filename => "$modeldir/tnpri.mod");
is ($model->get_estimation_evaluation_problem_number,-1,"get_estimation_evaluation_problem_number");

$model = model->new(filename => "$modeldir/twoprobmsf_match.mod", ignore_missing_data =>1);
is ($model->msfo_to_msfi_mismatch,0,"msfo_to_msfi_mismatch false");
$model = model->new(filename => "$modeldir/twoprobmsf_mismatch.mod", ignore_missing_data =>1);
is ($model->msfo_to_msfi_mismatch,2,"msfo_to_msfi_mismatch true second prob");


# Test of different models

#pheno_multiple_sizes
lives_ok { my $model = model->new(filename => "$modeldir/model/pheno_multiple_sizes.mod") } "multiple sizes should not crash";
dies_ok { my $model = model->new(filename => "$modeldir/model/pheno_multiple_sizes_not_in_beginning.mod") } "one sizes not in beginning";


$model = model->new(filename => "$modeldir/pheno_flip_comments.mod");

my $flipped = model::flip_comments(from_model => $model,
								   new_file_name => "$modeldir/model/flip.mod",
								   write => 0);

is_deeply($model->datafiles(absolute_path=>1),$flipped->datafiles(absolute_path=>1),"same datafile abspath after flip_comments");

my ($dir,$file)=OSspecific::absolute_path($model->directory.'/model','file'); 
my ($dir2,$file2)=OSspecific::absolute_path($flipped->directory,'file'); 
is($dir,$dir2,'flip_comments output model in subdir');


done_testing();
