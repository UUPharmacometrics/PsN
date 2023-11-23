#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use input_checking;
use Config;
use Env qw(PATH);
use model;
use ui;

ui->silent(1);
#use File::Spec;
#open STDERR, '>', File::Spec->devnull();       # Do not Silence STDERR, use silent
my $modeldir = $includes::testfiledir;

my $model = model->create_dummy_model;
my %options;

my @samples=(1000,1000,1000,2000,2000);
my @resamples = (200,400,500,1000,1000);

input_checking::check_options(tool => 'sir', options => \%options, model => $model);
is_deeply($options{'samples'},\@samples,'default sir samples');
is_deeply($options{'resamples'},\@resamples,'default sir resamples');

%options=();
$options{'rawres_input'} = 'this_file_does_not_exist';
dies_ok { input_checking::check_options(tool => 'sir', options => \%options, model => $model) } "check rawres input not exist";


%options=();
$model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
$options{'rse_theta'} = '12';
input_checking::check_options(tool => 'sir', options => \%options, model => $model); 
is($options{'rse_omega'},'12','check sir default rse_omega');
is($options{'rse_sigma'},'12','check sir default rse_sigma');

%options=();
$options{'rse_theta'} = '12,13';
dies_ok { input_checking::check_options(tool => 'sir', options => \%options, model => $model) } "check rse_omega undef when rse_theta array ";

%options=();
dies_ok { input_checking::check_options(tool => 'simeval', options => \%options, model => $model) } "simeval croak METHOD=ZERO ";

%options=();
$options{'covariates'}='WGT';
$options{'skip_omegas'}='0';
dies_ok { input_checking::check_options(tool => 'frem', 
										options => \%options, 
										model => $model) } "frem croak skip_omegas < 1";

%options=();
$options{'covariates'}='WGT';
$options{'skip_omegas'}='5';
dies_ok { input_checking::check_options(tool => 'frem', 
										options => \%options, 
										model => $model) } "frem croak skip_omegas too high";

$model = model->new(filename => $includes::testfiledir."/mox1.mod", ignore_missing_data => 1);

%options=();
$model = model->new(filename => "$modeldir/pheno_cond.mod", ignore_missing_data => 1);
$options{'threads'}=2;
input_checking::check_options(tool => 'simeval', options => \%options, model => $model); 
is($options{'samples'},300,'check simeval default samples');
is($options{'n_simulation_models'},2,'check simeval default n_simulation_models');

#vpc
%options=();
$options{'rawres_input'} = 'this_file_does_not_exist';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc check rawres input not exist";

%options=();
$options{'rawres_input'} = $includes::testfiledir."/raw_pheno_for_rawres_input.csv";
$options{'msfo_file'} = 'msfo.file';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croks when defined rawres input and defined msfo file";

%options=();
$options{'rawres_input'} = $includes::testfiledir."/raw_pheno_for_rawres_input.csv";
$options{'samples'} = 20;
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is($options{'n_simulation_models'},20,'vpc check n_simulation_models = samples');

%options=();
$options{'covariance_file'} = 'this_file_does_not_exist';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when defined covariance file";

%options=();
$options{'bin_array'} = '1,2,3';
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'bin_array'}, [[1,2,3]], "check vpc, list of bin array converts to matrix 1");

%options=();
$options{'bin_array'} = '1,2,3:4,5,6';
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'bin_array'}, [[1,2,3],[4,5,6]], "check vpc, list of bin array converts to matrix 2");

%options=();
$options{'bin_array'} = '';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when bin array not defined";

%options=();
$options{'levels'} = 'level1,level2';
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'levels'}, ['level1','level2'], "check vpc, list of levels converts to an array reference");

%options=();
$options{'levels'} = '';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when levels not defined";

%options=();
$options{'no_of_strata'} = '6';
$options{'stratify_on'} = '3,5,6,7';
$options{'refstrat'} ='4';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when refstrat is defined";

%options=();
$options{'no_of_strata'} = '4';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when stratify_on not defined";

%options=();
$options{'refstrat'} = '4';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when stratify_on not defined";

%options=();
$options{'sim_table'} = $includes::testfiledir."/raw_pheno_for_rawres_input.csv";
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when sim_table id defined but orig_table not defined";

%options=();
$options{'orig_table'} = $includes::testfiledir."/raw_pheno_for_rawres_input.csv";
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when orig_table id defined but sim_table not defined";

%options=();
$options{'orig_table'} = $includes::testfiledir."/raw_pheno_for_rawres_input.csv";
$options{'sim_table'} = $includes::testfiledir."/file_not_exist.csv";
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when orig_table or sim_table no exist";

%options=();
$options{'orig_table'} = $includes::testfiledir."/raw_pheno_for_rawres_input.csv";
$options{'sim_table'} = "";
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when sim_table filename is empty";

%options=();
$options{'orig_table'} = $includes::testfiledir."/file_not_exist.csv";
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when sim_model not exist";

%options=();
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'auto_bin_mode'}, 'minmax', "check vpc, if not defined auto bin, auto_bin_mode = 'minmax'");

%options=();
$options{'bin_by_count'} = 1;
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'auto_bin_mode'}, undef, "check vpc, if not defined auto bin, auto_bin_mode = 'auto'");

%options=();
$options{'min_points_in_bin'} = 7;
$options{'no_of_bins'} = 5;
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when min_points_in_bin defined, but auto_bin_mode not defined";

%options=();
$options{'auto_bin'} = 7;
$options{'bin_array'} = '1,2,3:4,5,6';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when min_points_in_bin defined, but auto_bin_mode not defined";

%options=();
$options{'auto_bin'} = 'auto';
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'auto_bin_mode'}, 'auto', "check vpc, if auto bin = 'auto',then auto_bin_mode = 'auto'");

%options=();
$options{'auto_bin'} = 'unique';
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'auto_bin_mode'}, undef, "check vpc, if auto bin = 'auto', then auto_bin_mode = 'auto'");

%options=();
$options{'auto_bin'} = 5;
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'auto_bin_mode'}, 'minmax', "check vpc, if auto bin = N, then auto_bin_mode = 'minmax' 1");
is_deeply($options{'min_no_bins'}, [5], "check vpc, if auto bin = N, then min_no_bins is array 1");
is_deeply($options{'max_no_bins'}, [5], "check vpc, if auto bin = N, then max_no_bins is array 1");

%options=();
$options{'auto_bin'} = '3,9';
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'auto_bin_mode'}, 'minmax', "check vpc, if auto bin = N, then auto_bin_mode = 'minmax' 2");
is_deeply($options{'min_no_bins'}, [3], "check vpc, if auto bin = N, then min_no_bins is array 2");
is_deeply($options{'max_no_bins'}, [9], "check vpc, if auto bin = N, then max_no_bins is array 2");

%options=();
$options{'auto_bin'} = '3,9:4,10:1,4';
input_checking::check_options(tool => 'vpc', options => \%options, model => $model);
is_deeply($options{'auto_bin_mode'}, 'minmax', "check vpc, if auto bin = N, then auto_bin_mode = 'minmax' 3");
is_deeply($options{'min_no_bins'}, [3,4,1], "check vpc, if auto bin = N, then min_no_bins is array 3");
is_deeply($options{'max_no_bins'}, [9,10,4], "check vpc, if auto bin = N, then max_no_bins is array 3");

%options=();
$options{'auto_bin'} = '3,4,5:4,7,8';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when auto_bin ";

%options=();
$options{'auto_bin'} = 'some_other';
dies_ok { input_checking::check_options(tool => 'vpc', options => \%options, model => $model) } "vpc croaks when auto_bin is not N, unique, auto or min,max";

#npfit
$PsN::nm_major_version = 7; # For npfit NONMEM version must be 7.4 or later
$PsN::nm_minor_version = 4;

%options=();
$options{'npsupp'}='0,100,200,300';
$options{'min_retries'}=2;
input_checking::check_options(tool => 'npfit', options => \%options, model => $model); 
is_deeply($options{'npsupp'},[0,100,200,300],'check npfit converts to list npsupp 1');

%options=();
$options{'npsupp'}='0';
input_checking::check_options(tool => 'npfit', options => \%options, model => $model); 
is_deeply($options{'npsupp'},[0],'check npfit converts to list npsupp 2');

%options=();
$options{'threads'}=2;
$options{'nm_version'}='730';
dies_ok { input_checking::check_options(tool => 'npfit', 
					options => \%options, 
					model => $model) } "npfit croak when no npsupp";

%options=();
$options{'threads'}=5;
$options{'npsupp'}='';
dies_ok { input_checking::check_options(tool => 'npfit', 
					options => \%options, 
					model => $model) } "npfit croak when npsupp empty";

%options=();
$options{'npsupp'}='0,10.2,50';
dies_ok { input_checking::check_options(tool => 'npfit', 
					options => \%options, 
					model => $model) } "npfit croak when npsupp decimal";
					
%options=();
$options{'npsupp'}='0 10 50';
dies_ok { input_checking::check_options(tool => 'npfit', 
					options => \%options, 
					model => $model) } "npfit croak when npsupp not comma-separated";
		
%options=();
$model->remove_records(type => 'estimation');
$options{'npsupp'}='0,100,200,300';					
dies_ok { input_checking::check_options(tool => 'npfit', 
					options => \%options, 
					model => $model) } "npfit croak when model does not have estimation record.";
					
%options=();
$model->set_records(type => 'estimation',
					record_strings => ["METHOD=CON"]);					
$options{'npsupp'}='0,100,200,300';
input_checking::check_options(tool => 'npfit', options => \%options, model => $model); 
is_deeply($options{'npsupp'},[0,100,200,300],'check if all works as in first test after deleting estimation record and adding it back with METHOD=CON');

%options=();
$model->remove_records(type => 'estimation');
$model->set_records(type => 'estimation',
					record_strings => ["METHOD=0 POSTHOC"]);					
$options{'npsupp'}='0,100,200,300';
input_checking::check_options(tool => 'npfit', options => \%options, model => $model); 
is_deeply($options{'npsupp'},[0,100,200,300],'check if estimation record with method METHOD=0 POSTHOC works');

%options=();
$model->remove_records(type => 'estimation');
$model->set_records(type => 'estimation',
					record_strings => ["METHOD=0"]);					
$options{'npsupp'}='0,100,200,300';
dies_ok { input_checking::check_options(tool => 'npfit', 
					options => \%options, 
					model => $model) } "croak when estimation record has method METHOD=0.";

%options=();
$model->remove_records(type => 'estimation');
$model->set_records(type => 'estimation',
					record_strings => ["MAXEVAL=99"]);					
$options{'npsupp'}='0,100,200,300';
dies_ok { input_checking::check_options(tool => 'npfit', 
					options => \%options, 
					model => $model) } "croak when estimation record has method MAXEVAL=99.";

%options=();
$PsN::nm_major_version = 6;
$model->remove_records(type => 'estimation');
$model->set_records(type => 'estimation',
					record_strings => ["METHOD=CON"]);					
$options{'npsupp'}='0,100,200,300';
dies_ok { input_checking::check_options(tool => 'npfit', 
					options => \%options, 
					model => $model) } "croak when NONMEM version is 6, it has to be 7.4 or later.";
					
%options=();
$PsN::nm_major_version = 7;
$PsN::nm_minor_version = 3;
$model->remove_records(type => 'estimation');
$model->set_records(type => 'estimation',
					record_strings => ["METHOD=CON"]);					
$options{'npsupp'}='0,100,200,300';
dies_ok { input_checking::check_options(tool => 'npfit', 
					options => \%options, 
					model => $model) } "croak when NONMEM version is 7.3, it has to be 7.4 or later.";
										
done_testing();
