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
use tool::npc;
use PsN;

our $toolname = 'vpc';
our $tempdir = create_test_dir('rplots_'.$toolname);

my $input_dir = $includes::rplots_testfiledir.'/'.$toolname.'/run1';
#my $input_dir = '/home/kajsa/kod-psn/devel/rplotstest'.'/rplots/'.$toolname.'/run1';

unless (File::Copy::Recursive::dircopy($input_dir, $tempdir)) {
	die " Could not copy contents of $input_dir to $tempdir : $!\n";
}

chdir($tempdir);
my ($major,$minor,$version) = includes::get_major_minor_nm_version();
my $model = model->new(filename => "run1.mod", ignore_missing_data => 1);

my $template_dir = includes::get_template_directory_rplots();
my $toolobject = tool::npc->new(directory => 'rundir',
								nm_version => $version,
								rplots => 2,
								is_vpc =>1,
								idv => 'TIME',
								categorized => 0,
								template_directory_rplots =>$template_dir,
								template_file_rplots => $toolname.'_default.R',
								samples => 100,
								models	     => [ $model ]);

$toolobject -> raw_results_file([$toolobject ->directory.'raw_results_run1.csv']);
$toolobject -> vpctab_filename($toolobject ->directory.'vpctab1');
$toolobject -> results_file('vpc_results.csv');
$toolobject -> create_R_script(tool_name => $toolname); 


my %pdf_files_pages=($tempdir.'rundir/PsN_vpc_plots.pdf' => 1);

includes::test_pdf_pages(\%pdf_files_pages);


done_testing();
