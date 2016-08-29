#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
require File::Copy::Recursive;
use model;
use tool::mcmp;
use PsN;

our $toolname = 'mcmp';
our $tempdir = create_test_dir('rplots_'.$toolname);

my $input_dir = $includes::rplots_testfiledir.'/'.$toolname.'/run1';
#my $input_dir = '/home/kajsa/kod-psn/devel/rplotstest'.'/rplots/'.$toolname.'/run1';

unless (File::Copy::Recursive::dircopy($input_dir, $tempdir)) {
	die " Could not copy contents of $input_dir to $tempdir : $!\n";
}

chdir($tempdir);
my $model = model->new(filename => "run1.mod", ignore_missing_data => 1);

my $template_dir = includes::get_template_directory_rplots();
my $toolobject = tool::mcmp->new(directory => 'rundir',
								 rplots => 2,
								 template_directory_rplots =>$template_dir,
								 template_file_rplots => $toolname.'_default.R',
								 full_model => $model,
								 reduced_model => $model,
								 models	     => [ $model ]);

$toolobject -> raw_results_file([$toolobject ->directory.'raw_results_run1.csv']);
$toolobject->significance_level(5);
$toolobject->ofv_full(6941.405);
$toolobject->ofv_reduced(7449.681);
$toolobject->n_individuals(590);
$toolobject->df(1);

$toolobject -> create_R_script(tool_name => $toolname); 


my %pdf_files_pages=($tempdir.'rundir/PsN_mcmp_plots.pdf' => 4);

includes::test_pdf_pages(\%pdf_files_pages);


done_testing();
