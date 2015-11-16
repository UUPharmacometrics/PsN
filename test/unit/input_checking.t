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

open STDERR, '>', File::Spec->devnull();	# Silence STDERR
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
$options{'cv_theta'} = '12';
input_checking::check_options(tool => 'sir', options => \%options, model => $model); 
is($options{'cv_omega'},'12','check sir default cv_omega');
is($options{'cv_sigma'},'12','check sir default cv_sigma');

%options=();
$options{'cv_theta'} = '12,13';
dies_ok { input_checking::check_options(tool => 'sir', options => \%options, model => $model) } "check cv_omega undef when cv_theta array ";

%options=();
dies_ok { input_checking::check_options(tool => 'ebe_npde', options => \%options, model => $model) } "ebe_npde croak METHOD=ZERO ";


%options=();
$model = model->new(filename => "$modeldir/pheno_cond.mod", ignore_missing_data => 1);
$options{'threads'}=2;
input_checking::check_options(tool => 'ebe_npde', options => \%options, model => $model); 
is($options{'samples'},300,'check ebe_npde default samples');
is($options{'n_simulation_models'},2,'check ebe_npde default n_simulation_models');


done_testing();
