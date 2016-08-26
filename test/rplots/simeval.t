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
use tool::simeval;
use PsN;

our $toolname = 'simeval';
our $tempdir = create_test_dir('rplots_'.$toolname);

my $input_dir = $includes::testfiledir.'/rplots/'.$toolname.'/run1';
#my $input_dir = '/home/kajsa/kod-psn/devel/rplotstest'.'/rplots/'.$toolname;

unless (File::Copy::Recursive::dircopy($input_dir, $tempdir)) {
	die " Could not copy contents of $input_dir to $tempdir : $!\n";
}

chdir($tempdir);
my $model = model->new(filename => "run1.mod", ignore_missing_data => 1);

my $template_dir = includes::get_template_directory_rplots();
my $toolobject = tool::simeval->new(directory => 'rundir',
								 rplots => 2,
								 template_directory_rplots =>$template_dir,
								 template_file_rplots => $toolname.'_default.R',
								 models	     => [ $model ],
								 samples            => 100);
$toolobject->iiv_eta(['ETA(1)','ETA(2)']);
$toolobject->occasions(0);
$toolobject->successful_samples(100);
$toolobject->have_iwres(1);
$toolobject->subjects(59);
push(@{$toolobject->vpctab_filenames},$toolobject->directory.'vpc_dv_vs_pred/vpctab1');
push(@{$toolobject->vpc_result_files},$toolobject->directory.'vpc_dv_vs_pred/vpc_results.csv');
push(@{$toolobject->vpc_names},'DV vs PRED');

push(@{$toolobject->vpctab_filenames},$toolobject->directory.'vpc_cwres_vs_idv/vpctab1');
push(@{$toolobject->vpc_result_files},$toolobject->directory.'vpc_cwres_vs_idv/vpc_results.csv');
push(@{$toolobject->vpc_names},'CWRES vs '.$toolobject->idv);

$toolobject -> create_R_script(tool_name => $toolname); 


ok (-e 'rundir/PsN_ebe_npde_plots.pdf','pdf 1 exists. Check that 3 plots in '.$tempdir.'rundir/PsN_ebe_npde_plots.pdf'); 
ok (-e 'rundir/PsN_OFV_plots.pdf','pdf 2 exists. Check that 4 plots in '.$tempdir.'rundir/PsN_OFV_plots.pdf'); 
ok (-e 'rundir/PsN_residual_plots.pdf','pdf 3 exists. Check that 4 plots/tables in '.$tempdir.'rundir/PsN_residual_plots.pdf'); 
ok (-e 'rundir/PsN_simeval_vpc_plots.pdf','pdf 4 exists. Check that 2 plots in '.$tempdir.'rundir/PsN_simeval_vpc_plots.pdf'); 



done_testing();
