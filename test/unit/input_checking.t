#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::Random;
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
$options{'covariates'}='WT';
input_checking::check_options(tool => 'frem', options => \%options, model => $model); 
is_deeply($options{'skip_omegas'},[3,4,5,6],'frem auto-skip bov omegas 1');

%options=();
$options{'covariates'}='WT';
$options{'skip_omegas'}='5,1,3';
input_checking::check_options(tool => 'frem', options => \%options, model => $model); 
is_deeply($options{'skip_omegas'},[1,3,4,5,6],'frem auto-skip bov omegas 2');
%options=();
$options{'covariates'}='WT';
$options{'skip_omegas'}='1,2';
dies_ok { input_checking::check_options(tool => 'frem', 
										options => \%options, 
										model => $model) } "frem croak skipping all omegas";



%options=();
$model = model->new(filename => "$modeldir/pheno_cond.mod", ignore_missing_data => 1);
$options{'threads'}=2;
input_checking::check_options(tool => 'simeval', options => \%options, model => $model); 
is($options{'samples'},300,'check simeval default samples');
is($options{'n_simulation_models'},2,'check simeval default n_simulation_models');

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
