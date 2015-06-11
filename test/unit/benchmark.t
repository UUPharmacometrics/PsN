#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use tool::benchmark;


my $modeldir = $includes::testfiledir;

my $model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
$model->filename('run1.mod');
my $model2 = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);
$model2->filename('run2.mod');
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


my $model = model::create_dummy_model();

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



my $ref = tool::benchmark::parse_record_options(
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
