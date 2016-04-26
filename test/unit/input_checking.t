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


done_testing();
