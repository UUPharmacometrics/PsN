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
use tool::cdd;
use PsN;

our $toolname = 'cdd';
our $tempdir = create_test_dir('rplots_'.$toolname);

my $input_dir = $includes::testfiledir.'/rplots/'.$toolname.'/run3';
#my $input_dir = '/home/kajsa/kod-psn/devel/rplotstest'.'/rplots/'.$toolname.'/run3';

unless (File::Copy::Recursive::dircopy($input_dir, $tempdir)) {
	die " Could not copy contents of $input_dir to $tempdir : $!\n";
}

chdir($tempdir);
my $model = model->new(filename => "run3.mod", ignore_missing_data => 1);

my $template_dir = includes::get_template_directory_rplots();
my $toolobject = tool::cdd->new(directory => 'rundir',
								rplots => 2,
								template_directory_rplots =>$template_dir,
								template_file_rplots => $toolname.'_default.R',
								case_column => 1,
								models	     => [ $model ]);

$toolobject -> raw_results_file([$toolobject ->directory.'raw_results_run3.csv']),
$toolobject -> create_R_script(tool_name => $toolname); 


ok (-e 'rundir/PsN_cdd_plots.pdf','pdf 1 exists. Check that 1 plot in '.$tempdir.'rundir/PsN_cdd_plots.pdf');


done_testing();
