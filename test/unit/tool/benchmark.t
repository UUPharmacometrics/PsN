#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

use tool::benchmark;


my ($d1,$d2,$d3)= get_major_minor_nm_version;
my $modeldir = $includes::testfiledir;

my $model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
$model->filename('run1.mod');



my $model2 = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
$model2->filename('run2.mod');


my $ref = tool::benchmark::parse_theta_inits(
	theta_inits => 'CL:none,1,2,3,,2:1',
	models => [$model,$model2]);
is_deeply($ref,[{'CL' => ['none',1,2,3]},{2 => [1]}],"parse_theta_list 1");

dies_ok {tool::benchmark::parse_theta_inits(
			 theta_inits => '3:1',models => [$model])} " invalid thetaname";
dies_ok {tool::benchmark::parse_theta_inits(
			 theta_inits => '2:1,V:1,2,3',models => [$model])} " missed double comma";
dies_ok {tool::benchmark::parse_theta_inits(
			 theta_inits => ':1',models => [$model])} " empty thetaname";
dies_ok {tool::benchmark::parse_theta_inits(
			 theta_inits => 'CL:1,2,,V',models => [$model])} " empty list";


my $modellists = [[$model],[$model2]];

tool::benchmark::create_record_variant_models(model_lists => $modellists,change_list => [{'estimation' => ['none','FAST']}]);

is(scalar(@{$modellists->[0]}),2,"variant models count 1a");
is(scalar(@{$modellists->[1]}),2,"variant models count 1b");
is($modellists->[0]->[0]->filename,'run1.none.mod',"variant models name 1 a");
is($modellists->[0]->[1]->filename,'run1.FAST.mod',"variant models name 1 b");
is($modellists->[0]->[0]->is_option_set(name=>'FAST',record=>'estimation'),0,"variant models opt set 1 a");
is($modellists->[0]->[1]->is_option_set(name=>'FAST',record=>'estimation'),1,"variant models opt set 1 b");
is($modellists->[1]->[0]->filename,'run2.none.mod',"variant models name 1 c");
is($modellists->[1]->[1]->filename,'run2.FAST.mod',"variant models name 1 d");
is($modellists->[1]->[0]->is_option_set(name=>'FAST',record=>'estimation'),0,"variant models opt set 1 c");
is($modellists->[1]->[1]->is_option_set(name=>'FAST',record=>'estimation'),1,"variant models opt set 1 d");

tool::benchmark::create_nonmem_alt_models(model_lists => $modellists,nm_version=>'default',alt_nonmem=>['nm740']);
is($modellists->[0]->[0]->filename,'run1.none.default.mod',"nmvariant models name 1 a");
is($modellists->[0]->[1]->filename,'run1.FAST.default.mod',"nmvariant models name 1 b");
is($modellists->[1]->[0]->filename,'run2.none.default.mod',"variant models name 1 c");
is($modellists->[1]->[1]->filename,'run2.FAST.default.mod',"variant models name 1 d");
is($modellists->[2]->[0]->filename,'run1.none.nm740.mod',"nmvariant models name 1 e");
is($modellists->[2]->[1]->filename,'run1.FAST.nm740.mod',"nmvariant models name 1 f");
is($modellists->[3]->[0]->filename,'run2.none.nm740.mod',"nmvariant models name 1 g");
is($modellists->[3]->[1]->filename,'run2.FAST.nm740.mod',"nmvariant models name 1 h");

tool::benchmark::create_replicate_models(model_lists => $modellists,replicates => 2);
is($modellists->[0]->[0]->filename,'run1.none.default.1.mod',"replicate models name 2 a");
is($modellists->[0]->[1]->filename,'run1.FAST.default.1.mod',"replicate models name 2 b");
is($modellists->[0]->[2]->filename,'run1.none.default.2.mod',"replicate models name 2 c");
is($modellists->[0]->[3]->filename,'run1.FAST.default.2.mod',"replicate models name 2 d");
is($modellists->[1]->[0]->filename,'run2.none.default.1.mod',"replicate models name 2 e");
is($modellists->[1]->[1]->filename,'run2.FAST.default.1.mod',"replciate models name 2 f");
is($modellists->[1]->[2]->filename,'run2.none.default.2.mod',"replicate models name 2 g");
is($modellists->[1]->[3]->filename,'run2.FAST.default.2.mod',"replciate models name 2 h");
is($modellists->[2]->[0]->filename,'run1.none.nm740.1.mod',"replicate models name 2 i");
is($modellists->[2]->[1]->filename,'run1.FAST.nm740.1.mod',"replicate models name 2 j");
is($modellists->[2]->[2]->filename,'run1.none.nm740.2.mod',"replicate models name 2 k");
is($modellists->[2]->[3]->filename,'run1.FAST.nm740.2.mod',"replicate models name 2 l");
is($modellists->[3]->[0]->filename,'run2.none.nm740.1.mod',"replicate models name 2 m");
is($modellists->[3]->[1]->filename,'run2.FAST.nm740.1.mod',"replicate models name 2 n");
is($modellists->[3]->[2]->filename,'run2.none.nm740.2.mod',"replicate models name 2 o");
is($modellists->[3]->[3]->filename,'run2.FAST.nm740.2.mod',"replicate models name 2 p");

tool::benchmark::create_theta_variant_models(model_lists => $modellists,theta_list => [{'CL' => ['none',1]}]);
is($modellists->[0]->[0]->filename,'run1.none.default.1.none.mod',"theta mod models name 3 a");
is($modellists->[0]->[1]->filename,'run1.FAST.default.1.none.mod',"theta mod models name 3 b");
is($modellists->[0]->[2]->filename,'run1.none.default.2.none.mod',"theta mod models name 3 c");
is($modellists->[0]->[3]->filename,'run1.FAST.default.2.none.mod',"theta mod models name 3 d");
is($modellists->[0]->[4]->filename,'run1.none.default.1.1.mod',"theta mod models name 3 a");
is($modellists->[0]->[5]->filename,'run1.FAST.default.1.1.mod',"theta mod models name 3 b");
is($modellists->[0]->[6]->filename,'run1.none.default.2.1.mod',"theta mod models name 3 c");
is($modellists->[0]->[7]->filename,'run1.FAST.default.2.1.mod',"theta mod models name 3 d");
cmp_ok($modellists->[0]->[0]->problems->[0]->thetas->[0]->options->[0]->init,'==',0.0105,"theta mod th init 1");
cmp_ok($modellists->[0]->[4]->problems->[0]->thetas->[0]->options->[0]->init,'==',1,"theta mod th init 2");

is($modellists->[1]->[0]->filename,'run2.none.default.1.none.mod',"theta mod models name 3 e");
is($modellists->[1]->[1]->filename,'run2.FAST.default.1.none.mod',"theta mod models name 3 f");
is($modellists->[1]->[2]->filename,'run2.none.default.2.none.mod',"theta mod models name 3 g");
is($modellists->[1]->[3]->filename,'run2.FAST.default.2.none.mod',"theta mod models name 3 h");
is($modellists->[1]->[4]->filename,'run2.none.default.1.1.mod',"theta mod models name 3 e");
is($modellists->[1]->[5]->filename,'run2.FAST.default.1.1.mod',"theta mod models name 3 f");
is($modellists->[1]->[6]->filename,'run2.none.default.2.1.mod',"theta mod models name 3 g");
is($modellists->[1]->[7]->filename,'run2.FAST.default.2.1.mod',"theta mod models name 3 h");
cmp_ok($modellists->[1]->[1]->problems->[0]->thetas->[0]->options->[0]->init,'==',0.0105,"theta mod th init 3");
cmp_ok($modellists->[1]->[5]->problems->[0]->thetas->[0]->options->[0]->init,'==',1,"theta mod th init 4");


is($modellists->[2]->[0]->filename,'run1.none.nm740.1.none.mod',"theta mod models name 3 i");
is($modellists->[2]->[1]->filename,'run1.FAST.nm740.1.none.mod',"theta mod models name 3 j");
is($modellists->[2]->[2]->filename,'run1.none.nm740.2.none.mod',"theta mod models name 3 k");
is($modellists->[2]->[3]->filename,'run1.FAST.nm740.2.none.mod',"theta mod models name 3 l");
is($modellists->[2]->[4]->filename,'run1.none.nm740.1.1.mod',"theta mod models name 3 i");
is($modellists->[2]->[5]->filename,'run1.FAST.nm740.1.1.mod',"theta mod models name 3 j");
is($modellists->[2]->[6]->filename,'run1.none.nm740.2.1.mod',"theta mod models name 3 k");
is($modellists->[2]->[7]->filename,'run1.FAST.nm740.2.1.mod',"theta mod models name 3 l");
cmp_ok($modellists->[2]->[2]->problems->[0]->thetas->[0]->options->[0]->init,'==',0.0105,"theta mod th init 5");
cmp_ok($modellists->[2]->[6]->problems->[0]->thetas->[0]->options->[0]->init,'==',1,"theta mod th init 6");

is($modellists->[3]->[0]->filename,'run2.none.nm740.1.none.mod',"theta mod models name 3 m");
is($modellists->[3]->[1]->filename,'run2.FAST.nm740.1.none.mod',"theta mod models name 3 n");
is($modellists->[3]->[2]->filename,'run2.none.nm740.2.none.mod',"theta mod models name 3 o");
is($modellists->[3]->[3]->filename,'run2.FAST.nm740.2.none.mod',"theta mod models name 3 p");
is($modellists->[3]->[4]->filename,'run2.none.nm740.1.1.mod',"theta mod models name 3 m");
is($modellists->[3]->[5]->filename,'run2.FAST.nm740.1.1.mod',"theta mod models name 3 n");
is($modellists->[3]->[6]->filename,'run2.none.nm740.2.1.mod',"theta mod models name 3 o");
is($modellists->[3]->[7]->filename,'run2.FAST.nm740.2.1.mod',"theta mod models name 3 p");
cmp_ok($modellists->[3]->[3]->problems->[0]->thetas->[0]->options->[0]->init,'==',0.0105,"theta mod th init 7");
cmp_ok($modellists->[3]->[7]->problems->[0]->thetas->[0]->options->[0]->init,'==',1,"theta mod th init 8");


$model = model::create_dummy_model();

dies_ok {tool::benchmark::get_modified_filename(model=>$model,option=>'FAST')} " no extension";

$model->filename('run1.mod');
is(tool::benchmark::get_modified_filename(model=>$model,option=>'FAST'),'run1.FAST.mod','get_modified_filename 1');

$model->filename('run1.FAST.mod');
is(tool::benchmark::get_modified_filename(model=>$model,option=>'ATOL=8'),'run1.FAST.ATOL_8.mod',
   'get_modified_filename 2');

$model->filename('run1.FAST.ATOL_8.mod');
is(tool::benchmark::get_modified_filename(model=>$model,option=>'METHOD=COND'),'run1.FAST.ATOL_8.METHOD_COND.mod',
   'get_modified_filename 3');

is(tool::benchmark::get_modified_filename(model=>$model,replicate=>1),'run1.FAST.ATOL_8.1.mod',
   'get_modified_filename 4');



$ref = tool::benchmark::parse_record_options(
	record_options => 'estimation:none,FAST');

is_deeply($ref,[{'estimation' => ['none','FAST']}],"parse_record_options 1");

$ref = tool::benchmark::parse_record_options(
	record_options => 'estim:FAST,,est:ATOL=8,ATOL=10,ATOL=12');

is_deeply($ref,[{'estimation' => ['FAST']},
				{'estimation' => ['ATOL=8','ATOL=10','ATOL=12']}],
		  "parse_record_options 2");

$ref = tool::benchmark::parse_record_options(
	record_options => 'sim:(123),(432),,');

is_deeply($ref,[{'simulation' => ['(123)','(432)']}],
		  "parse_record_options 3");


dies_ok {tool::benchmark::parse_record_options(
	record_options => 'estimation:none,FAST,estimation:ATOL=8,ATOL=10')} 
" missed double comma";

dies_ok {tool::benchmark::parse_record_options(
			 record_options => 'EST:')} " empty opts";

dies_ok {tool::benchmark::parse_record_options(
			 record_options => ':FAST')} " empty rec";

dies_ok {tool::benchmark::parse_record_options(
	record_options => 'SAM:none,FAST')} " invalid record";


done_testing();
