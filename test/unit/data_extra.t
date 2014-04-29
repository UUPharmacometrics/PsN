#!/usr/bin/perl

# Unit tests for data.pm
#

use strict;
use warnings;
use Test::More;# tests=>199;
use Test::Exception;
use Math::Random;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;
use model::problem::data;
use model::problem;
use model;

#my $tempdir = create_test_dir('unit_dataextra');

my $datarec_at = model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)','IGN=@']);
is($datarec_at->ignoresign,'@','data record ignoresign at');

my $datarec_empty = model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)']);
is($datarec_empty->ignoresign,undef,'data record ignoresign undef');

my $datarec_hash = model::problem::data->new(record_arr => ['file.csv IGNOR=# IGNORE=(DOSE.GT.5)']);
is($datarec_hash->ignoresign,'#','data record ignoresign hash');

my $datarec_I = model::problem::data->new(record_arr => ['file.csv IGNO=I ','IGNOR=(DOSE.GT.5)']);
is($datarec_I->ignoresign,'I','data record ignoresign I');

my $datarec_C = model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)','IGNORE=C REWIND']);
is($datarec_C->ignoresign,'C','data record ignoresign C');

dies_ok { model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)',"IGNORE=';' REWIND"]) } "Quoted ignoresign";


#model->idcolumn

my @inputs = ('$INPUT C ID','$INPUT ID DV MDV','$INPUT C IDCOL ID=PAT DV','$INPUT ID=PAT DV','$INPUT DV ID=PAT IDC');
my @idcolnum = (2,1,3,1,2);

for( my $i=0; $i<scalar(@inputs); $i++){
	my $dummy_prob = model::problem->new(ignore_missing_files=> 1,
										 prob_arr       => ['$PROB',$inputs[$i],'$DATA dummy.txt']);
	
	my $model = model->new(filename => 'dummy',
						   problems => [$dummy_prob],
						   skip_data_parsing=> 1,
						   ignore_missing_files => 1);
	is($model->idcolumn->[0],$idcolnum[$i],'model idcolumn test '.$inputs[$i]);
}

my $dummy_prob = model::problem->new(ignore_missing_files=> 1,
									 prob_arr       => ['$PROB','$INPUT C PAT=ID','$DATA dummy.txt']);

dies_ok {my $model = model->new(filename => 'dummy', problems => [$dummy_prob], skip_data_parsing=> 1, ignore_missing_files => 1)} "PAT=ID in \$INPUT";


#model->idcolumns

for( my $i=0; $i<scalar(@inputs); $i++){
	my $dummy_prob = model::problem->new(ignore_missing_files=> 1,
										 prob_arr       => ['$PROB',$inputs[$i],'$DATA dummy.txt']);
	
	my $model = model->new(filename => 'dummy',
						   problems => [$dummy_prob],
						   skip_data_parsing=> 1,
						   ignore_missing_files => 1);
	is($model->idcolumns->[0],$idcolnum[$i],'model idcolumns test '.$inputs[$i]);
}


my $datadir = $includes::testfiledir.'/data/';

my @datafiles = qw(1_nohead_leading_empty.csv 2_nohead.csv 3_id_header_leading_empty.csv 4_header_ID.csv 5_hash_header_leading_empty.csv 6_header_hash.csv 7_C_lead_empty.csv 8_C_lead_dummy.csv 9_lead_posneg.csv);

#file parsing
my $index=0;
my $dummy_prob = model::problem->new(ignore_missing_files=> 1,
									 prob_arr       => ['$PROB','$INPUT C ID TIME AMT WGT APGR DV','$DATA '.$datadir.$datafiles[$index].' IGN=C']);

my $model = model->new(filename => 'dummy',
					   problems => [$dummy_prob],
					   skip_data_parsing=> 0,
					   ignore_missing_files => 1);

is($model->datas->[0]->count_ind,5,'n individuals '.$datafiles[$index].' ignore C');
is($model->datas->[0]->idcolumn,2,'idcol '.$datafiles[$index]);

$dummy_prob = model::problem->new(ignore_missing_files=> 1,
									 prob_arr       => ['$PROB','$INPUT C ID TIME AMT WGT APGR DV','$DATA '.$datadir.$datafiles[$index].' IGN=@']);

$model = model->new(filename => 'dummy',
					   problems => [$dummy_prob],
					   skip_data_parsing=> 0,
					   ignore_missing_files => 1);

is($model->datas->[0]->count_ind,5,'n individuals '.$datafiles[$index].' ignore @ ');
is($model->datas->[0]->idcolumn,2,'idcol '.$datafiles[$index]);

$dummy_prob = model::problem->new(ignore_missing_files=> 1,
									 prob_arr       => ['$PROB','$INPUT C ID TIME AMT WGT APGR DV','$DATA '.$datadir.$datafiles[$index].' ']);

$model = model->new(filename => 'dummy',
					   problems => [$dummy_prob],
					   skip_data_parsing=> 0,
					   ignore_missing_files => 1);

is($model->datas->[0]->count_ind,5,'n individuals '.$datafiles[$index].' default IGNORE ');
is($model->datas->[0]->idcolumn,2,'idcol '.$datafiles[$index]);

$index=1;
$dummy_prob = model::problem->new(ignore_missing_files=> 1,
									 prob_arr       => ['$PROB','$INPUT ID TIME AMT WGT APGR DV','$DATA '.$datadir.$datafiles[$index].' IGN=C']);

$model = model->new(filename => 'dummy',
					   problems => [$dummy_prob],
					   skip_data_parsing=> 0,
					   ignore_missing_files => 1);

is($model->datas->[0]->count_ind,5,'n individuals '.$datafiles[$index]);
is($model->datas->[0]->idcolumn,1,'idcol '.$datafiles[$index]);

$index=2;
$dummy_prob = model::problem->new(ignore_missing_files=> 1,
									 prob_arr       => ['$PROB','$INPUT C ID TIME AMT WGT APGR DV','$DATA '.$datadir.$datafiles[$index].' IGN=C']);

dies_ok {$model = model->new(filename => 'dummy',problems => [$dummy_prob],skip_data_parsing=> 0,ignore_missing_files => 1)} "bad ignore";

$dummy_prob = model::problem->new(ignore_missing_files=> 1,
									 prob_arr       => ['$PROB','$INPUT C ID TIME AMT WGT APGR DV','$DATA '.$datadir.$datafiles[$index].' IGN=@']);

$model = model->new(filename => 'dummy',
					   problems => [$dummy_prob],
					   skip_data_parsing=> 0,
					   ignore_missing_files => 1);

is($model->datas->[0]->count_ind,5,'n individuals '.$datafiles[$index]);
is($model->datas->[0]->idcolumn,2,'idcol '.$datafiles[$index]);

#my $dataobj = data->new(filename => $filename, directory => $tempdir);

#remove_test_dir($tempdir);

done_testing;
