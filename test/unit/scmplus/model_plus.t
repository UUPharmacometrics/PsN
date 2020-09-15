#!/usr/bin/perl

use strict;
use warnings;
use FindBin qw($Bin);
use lib "$Bin/../..";   #location of includes.pm
use includes;
use Test::More;
use Test::Exception;
use model;
use model_plus;


my %options = ('some' => 'value');

is(model_plus::set_specific_and_common_maxevals(options => \%options),undef,'handle maxevals undef');

$options{'maxevals'} = '10000';
is(model_plus::set_specific_and_common_maxevals(options => \%options),undef,'handle maxevals large integer');
is($options{'maxevals'},'10000','keep common maxevals large integer');

$options{'maxevals'} = '1000';
is(model_plus::set_specific_and_common_maxevals(options => \%options),'1000','handle maxevals small integer');
is($options{'maxevals'},undef,'remove common maxevals small integer');

$options{'maxevals'} = '2.5';
is(model_plus::set_specific_and_common_maxevals(options => \%options),'2.5','handle maxevals decimal');
$options{'maxevals'} = '3.1';
is(model_plus::set_specific_and_common_maxevals(options => \%options),'3.1','handle maxevals decimal');

is($options{'maxevals'},undef,'remove common maxevals decimal');


is(model_plus::get_new_maxeval(specific_maxevals =>'1.3',reference_model_evaluations =>100),130,'get new maxevals 1');
is(model_plus::get_new_maxeval(specific_maxevals =>'12',reference_model_evaluations =>100),12,'get new maxevals 2');
is(model_plus::get_new_maxeval(specific_maxevals =>'1.5',reference_model_evaluations =>undef),undef,'get new maxevals 3');
is(model_plus::get_new_maxeval(specific_maxevals =>undef,reference_model_evaluations =>100),undef,'get new maxevals 4');
is(model_plus::get_new_maxeval(specific_maxevals =>'100.0',reference_model_evaluations =>100),9999,'get new maxevals 5');

dies_ok { model_plus::get_new_maxeval(specific_maxevals =>'1.0E10',reference_model_evaluations =>100)} "illegal maxevals";

my $model = model->new(filename => $includes::testfiledir . '/scmplus/pheno_with_cov.mod', 
					   ignore_missing_data => 1);

is(model_plus::get_number_of_evaluations(model => $model),124,'get function evaluations');

is(model_plus::get_advan_number(model => $model),'1','pheno_with_cov ADVAN');

$model = model->new(filename => $includes::testfiledir.'/pheno.mod',
					ignore_missing_data => 1);
is(model_plus::get_number_of_evaluations(model => $model),89,'get function evaluations');

is(scalar(@{$model->problems->[-1]->tables()}),1,'tune model pre table');
is(scalar(@{$model->problems->[-1]->covariances()}),1,'tune model pre cov');

my ($errors,$warnings,$information) = 
	model_plus::tune(model => $model,
					 keep_covariance => 0,
					 etas => 0,
					 keep_tables => 0,
					 ctype4 => 1,
					 ignore_no_sigl => 0,
					 maxevals => 3.1,
					 reference_evaluations => 100);
is($model->problems->[-1]->tables(),undef,'tune model remove table');
is($model->problems->[-1]->covariances(),undef,'tune model remove cov');
is($model->get_option_value(record_name => 'estimation',option_name =>'MAXEVALS'),310,'tune model maxeval');
is($model->get_option_value(record_name => 'estimation',option_name =>'CTYPE'),4,'tune model ctype');

is_deeply($errors,[],'tune model errors');
is_deeply($warnings,[],'tune model warnings');
is_deeply($information,['Set MAXEVALS=310','Set CTYPE=4',
						"Removing all \$COVARIANCE from the model",
						"Removing all \$TABLE from the model",
						"ADVAN1 is used and SIGL is not set in \$EST"],'tune model info');

$model = model->new(filename => $includes::testfiledir . '/scmplus/sde8.ctl',
					ignore_missing_data => 1);


is(model_plus::get_advan_number(model => $model),'6','nm example ADVAN');

($errors,$warnings,$information) = 
	model_plus::tune(model => $model,
					 keep_covariance => 1,
					 keep_tables => 1,
					 etas => 0,
					 ctype4 => 1,
					 ignore_no_sigl => 0,
					 maxevals => undef,
					 reference_evaluations => 100);
is($model->get_option_value(record_name => 'estimation',option_name =>'CTYPE'),4,'tune model ctype');

is_deeply($errors,["ADVAN6 is used but SIGL is not set in \$EST. To allow this use option -ignore_no_sigl"],'tune model errors');
is_deeply($warnings,[],'tune model warnings');
is_deeply($information,['Set CTYPE=4'],'tune model info');

($errors,$warnings,$information) = 
	model_plus::tune(model => $model,
					 keep_covariance => 1,
					 etas => 0,
					 keep_tables => 1,
					 ctype4 => 1,
					 ignore_no_sigl => 1,
					 maxevals => undef,
					 reference_evaluations => 100);
is_deeply($warnings,["ADVAN6 is used but SIGL is not set in \$EST"],'tune model warnings');
is_deeply($errors,[],'tune model errors');
is_deeply($information,['Set CTYPE=4'],'tune model info');


done_testing();
