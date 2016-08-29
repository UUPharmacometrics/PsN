#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
require File::Copy::Recursive;
use model;
use tool::sir;
use PsN;

our $toolname = 'sir';
our $tempdir = create_test_dir('rplots_'.$toolname);

my $input_dir = $includes::testfiledir.'/rplots/'.$toolname.'/run3';
#my $input_dir = '/home/kajsa/kod-psn/devel/rplotstest'.'/rplots/'.$toolname.'/run3';

unless (File::Copy::Recursive::dircopy($input_dir, $tempdir)) {
	die " Could not copy contents of $input_dir to $tempdir : $!\n";
}

chdir($tempdir);
my $model = model->create_dummy_model;

my $template_dir = includes::get_template_directory_rplots();
my $toolobject = tool::sir->new(directory => 'rundir',
								samples => [100],
								resamples => [50],
								 models	     => [ $model ]);

$toolobject->full_rawres_header(["sample.id","model","problem","subproblem","covariance_step_run","minimization_successful","covariance_step_successful","covariance_step_warnings","estimate_near_boundary","rounding_errors","zero_gradients","final_zero_gradients","hessian_reset","s_matrix_singular","significant_digits","condition_number","est_methods","model_run_time","subprob_est_time","subprob_cov_time","ofv","deltaofv","likelihood_ratio","relPDF","importance_ratio","probability_resample","resamples","sample_order","CL","V","CLWGT","IVCL","IVV","SIGMA(1,1)","seCL","seV","seCLWGT","seIVCL","seIVV","seSIGMA(1,1)","shrinkage_eta1(%)","shrinkage_eta2(%)","shrinkage_iwres(%)","EI1","EI2","EI3","EI4","EI5","EI6"]);
$toolobject->template_directory_rplots($template_dir);
$toolobject->template_file_rplots($toolname.'_default.R');
$toolobject->rplots(2);
$toolobject -> create_R_script(tool_name => $toolname); 


ok (-e 'rundir/PsN_plots_base.pdf','pdf 1 exists. Check that 4 plots in '.$tempdir.'rundir/PsN_plots_base.pdf');
ok (-e 'rundir/PsN_plots_extended.pdf','pdf 2 exists. Check that 16 plots in '.$tempdir.'rundir/PsN_plots_extended.pdf'); 


done_testing();
