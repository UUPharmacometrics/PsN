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

my @datafiletests = (
	{ filename => '1_nohead_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=C', idcolumn => 2, row => 3, col => 2, val => 24.5 },
	{ filename => '1_nohead_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 2, row => 9, col => 1, val => 1 },
	{ filename => '1_nohead_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => '', idcolumn => 2, row => 0, col => 3, val => 25.0 },
	{ filename => '2_nohead.csv', input => 'ID TIME AMT WGT APGR DV', data => 'IGN=C', idcolumn => 1, row => 7, col => 1, val => 72.5 },
	{ filename => '3_id_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 2, row => 1, col => 1, val => 1 },
	{ filename => '3_id_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=C', crash => 1, row => 0, col => 5, val => 7 },
	{ filename => '3_id_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => '', crash => 1 },
	{ filename => '4_header_ID.csv', input => 'ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 1, row => 4, col => 1, val => 37 },
	{ filename => '4_header_ID.csv', input => 'ID TIME AMT WGT APGR DV', data => 'IGN=C', crash => 1 },
	{ filename => '4_header_ID.csv', input => 'ID TIME AMT WGT APGR DV', data => '', crash => 1 },
	{ filename => '5_hash_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 2, row => 0, col => 2, val => 0 },
	{ filename => '5_hash_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=C', crash => 1 },
	{ filename => '5_hash_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => '', idcolumn => 2, row => 2, col => 3, val => 3.5 },
	{ filename => '6_header_hash.csv', input => 'ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 1, col => 0, row => 0, val => 1 },
	{ filename => '6_header_hash.csv', input => 'ID TIME AMT WGT APGR DV', data => 'IGN=C', crash => 1 },
	{ filename => '6_header_hash.csv', input => 'ID TIME AMT WGT APGR DV', data => '', idcolumn => 1, col => 1, row => 1, val => 2 },
	{ filename => '7_C_lead_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 2, row => 2, col => 2, val => 12.5 },
	{ filename => '7_C_lead_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=C', idcolumn => 2, row => 0, col => 4, val => 1.4 },
	{ filename => '7_C_lead_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => '', crash => 1 },
	{ filename => '8_C_lead_dummy.csv', input => 'C ID TIME AMT WGT APGR DC', data => 'IGN=@', idcolumn => 2, row => 7, col => 3, val => 3.5 },
	{ filename => '8_C_lead_dummy.csv', input => 'C ID TIME AMT WGT APGR DC', data => 'IGN=C', idcolumn => 2, row => 1, col => 2, val => 2.0 },
	{ filename => '8_C_lead_dummy.csv', input => 'C ID TIME AMT WGT APGR DC', data => '', crash => 1 },
	{ filename => '9_lead_posneg.csv', input => 'PNEG ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 2, row => 0, col => 4, val => '1.4' },
	{ filename => '9_lead_posneg.csv', input => 'PNEG ID TIME AMT WGT APGR DV', data => 'IGN=C', crash => 1 },
	{ filename => '9_lead_posneg.csv', input => 'PNEG ID TIME AMT WGT APGR DV', data => '', crash => 1 },
);
my $problem;
my $model;

foreach my $test_hash (@datafiletests) {
	$problem = model::problem->new(ignore_missing_files => 1, 
		prob_arr => ['$PROB', '$INPUT ' . $test_hash->{'input'}, '$DATA ' . $datadir . $test_hash->{'filename'} . ' ' . $test_hash->{'data'}] );
	if (not $test_hash->{'crash'}) {
		$model = model->new(filename => 'dummy',
			problems => [$problem],
			skip_data_parsing => 0,
			ignore_missing_files => 1);
	} else {
		dies_ok { $model = model->new(filename => 'dummy', problems => [$dummy_prob], skip_data_parsing => 0, ignore_missing_files => 1) }
			"bad ignore " . $test_hash->{'filename'};
	}

	if (not $test_hash->{'crash'}) {
		is($model->datas->[0]->count_ind, 5, 'n individuals ' . $test_hash->{'filename'} . ' ' . $test_hash->{'data'});
		is($model->datas->[0]->idcolumn, $test_hash->{'idcolumn'}, 'idcol ' . $test_hash->{'filename'});
		if (exists $test_hash->{'row'}) {
			my $column_ref = $model->datas->[0]->column_to_array(column => $test_hash->{'col'});
			cmp_float ($column_ref->[$test_hash->{'row'}], $test_hash->{'val'}, $test_hash->{'filename'} .
				' value check row=' . $test_hash->{'row'} . ' col=' . $test_hash->{'col'});
		}
	}
}

done_testing;
