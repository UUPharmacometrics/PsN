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
use tool::pvar;
use PsN;

our $toolname = 'pvar';
our $tempdir = create_test_dir('rplots_'.$toolname);

my $input_dir = $includes::rplots_testfiledir.'/'.$toolname.'/run1';

unless (File::Copy::Recursive::dircopy($input_dir, $tempdir)) {
	die "Could not copy contents of $input_dir to $tempdir : $!\n";
}

chdir($tempdir);
my ($major,$minor,$version) = includes::get_major_minor_nm_version();
my $model = model->create_dummy_model();

my $template_dir = includes::get_template_directory_rplots();
my $toolobject = tool::pvar->new(directory => '.',
                                models => [ $model ],
								nm_version => $version,
								rplots => 2,
								idv => 'TIME',
								template_directory_rplots => $template_dir,
								template_file_rplots => $toolname.'_default.R',
								pvar_models	=> [ $model ]);

$toolobject->create_R_script(tool_name => $toolname); 

my %pdf_files_pages = ($tempdir . './PsN_pvar_plots.pdf' => 3);

includes::test_pdf_pages(\%pdf_files_pages);


done_testing();
