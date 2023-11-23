#!/usr/bin/perl

# Unit tests for data.pm
#

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;
use model::problem::data;
use model::problem;
use model;
use File::Copy 'cp';
use Cwd;
use Config;
use OSspecific;
use File::Spec qw(catfile);

#test for data class subroutine for frem dataset generation
#TODO add checks of contents of findme.dta (data2name file)
# -time_var=WT -occ=VISI -param=PHI,LAG -invar=SEX,DGRP -vpc -no-check $model_dir/mox_no_bov.mod -dir=$dir",
ui->silent(1);
my $tempdir = create_test_dir('unit_data_extra');

cp($includes::testfiledir.'/frem_filtered_data.dta',$tempdir);
cp($includes::testfiledir.'/frem_filtered_nomdv.dta',$tempdir);


my $filtered_data_nomdv = data->new(filename => $tempdir.'frem_filtered_nomdv.dta',
							  ignoresign => '@',
							  idcolumn => 1,
							  missing_data_token => -99);


my $resultref = data::frem_compute_covariate_properties(filtered_data => $filtered_data_nomdv,
													 invariant_covariates => ['WT'],
													 directory => $filtered_data_nomdv->directory,
													 data2name => 'findme3.dta', #ends up in tempdir
													 evid_index => undef,
													 mdv_index => undef,
													 dv_index => 3,
													 type_index => 5,
													 N_parameter_blocks => 1,
													 cov_indices => [4], #WT
													 is_log => [0]);

is_deeply($resultref->{'invariant_median'},[70.5],'frem median WT');
cmp_float_array($resultref->{'invariant_mean'},[6.95833333e+01],'frem mean WT');

cmp_float($resultref->{'invariant_covmatrix'}->[0]->[0],90.308787878788,'frem inv covmatrix nomdv 1,1');
is_deeply($resultref->{'has_missingness'},[0],'frem missing covariates nomdv');


my $filtered_data = data->new(filename => $tempdir.'frem_filtered_data.dta',
							  ignoresign => '@',
							  idcolumn => 1,
							  missing_data_token => -99);

$resultref = data::frem_compute_covariate_properties(filtered_data => $filtered_data,
														invariant_covariates => ['SEX','DGRP'],
														occ_index => 1,
														directory => $filtered_data->directory,
														data2name => 'findme.dta', #ends up in tempdir
														evid_index => 31,
														mdv_index => undef,
														dv_index => 30,
														type_index => 33,
														N_parameter_blocks => 1,
														cov_indices => [12,3,14], #SEX DGRP WT
														is_log => [0,0,0],
														first_timevar_type => 2);    #index 3 in cov_indices


is($resultref->{'occasionlist'}->[0],3,'frem occasion 1');
is($resultref->{'occasionlist'}->[1],8,'frem occasion 2');
is($resultref->{'invariant_median'}->[0],1,'frem median SEX');
is($resultref->{'invariant_median'}->[1],8,'frem median DGRP');
cmp_float_array($resultref->{'invariant_mean'},[1.20270270,8],'frem median SEX, DGRP');
cmp_float($resultref->{'invariant_covmatrix'}->[0]->[0],0.163828211773417,'frem inv covmatrix 1,1');
cmp_float($resultref->{'invariant_covmatrix'}->[0]->[1],-0.013698630136986,'frem inv covmatrix 1,2');
cmp_float($resultref->{'invariant_covmatrix'}->[1]->[0],-0.013698630136986,'frem inv covmatrix 2,1');
cmp_float($resultref->{'invariant_covmatrix'}->[1]->[1],0.657534246575342,'frem inv covmatrix 2,2');
is($resultref->{'timevar_median'}->[0],77.5,'frem median WT');
cmp_float($resultref->{'timevar_covmatrix'}->[0]->[0],241.6312939651981,'frem var covmatrix 1,1');
is_deeply($resultref->{'has_missingness'},[0,0],'frem missing covariates 1');

$filtered_data->missing_data_token(9);

$resultref = data::frem_compute_covariate_properties(filtered_data => $filtered_data,
													 invariant_covariates => ['SEX','DGRP'],
													 directory => $filtered_data->directory,
													 data2name => 'findme2.dta', #ends up in tempdir
													 evid_index => 31,
													 mdv_index => undef,
													 dv_index => 30,
													 type_index => 33,
													 N_parameter_blocks => 1,
													 cov_indices => [12,3], #SEX DGRP
													 is_log => [0,0]);


is_deeply($resultref->{'invariant_median'},[1,8],'frem median SEX, DGRP');
cmp_float_array($resultref->{'invariant_mean'},[1.20270270,7.52],'frem median SEX, DGRP');

cmp_float($resultref->{'invariant_covmatrix'}->[0]->[0],0.163828211773417,'frem inv covmatrix 1,1');
is_deeply($resultref->{'has_missingness'},[0,1],'frem missing covariates 2');


remove_test_dir($tempdir);

#TODO datarec new must use base directory model file directory, not cwd, when not absolute input name
#require model directory as input to new


my $model = model->new(filename => $includes::testfiledir.'/pheno.mod');
my ($dir,$file)=OSspecific::absolute_path($includes::testfiledir,'pheno.dta');
is($model->datafiles(absolute_path => 1)->[0],$dir.$file,' datafilename abspath');
is($model->datafiles(absolute_path => 0)->[0],'pheno.dta',' datafilename bare');

#need OSspecific to make tests platform independend
my $datarec = model::problem::data->new(record_arr => ['$DATA "'.$tempdir.'subdir space/file.csv" ']);
($dir,$file)=OSspecific::absolute_path($tempdir.'subdir space','file');
is($datarec ->get_directory,$dir,'data record dir double quotes space');
is($datarec ->get_filename,'file.csv','data record filename double quotes space');

my $dirsep = '/';
if($Config{'osname'} eq 'MSWin32') {
	$dirsep = "\\";
}

$datarec = model::problem::data->new(record_arr => ['$DATA "'.$tempdir.'subdir space'.$dirsep.'sub space'.$dirsep.'..'.$dirsep.'file.csv" ']);
($dir,$file)=OSspecific::absolute_path($tempdir.'subdir space','file');
is($datarec ->get_directory,$dir,'data record dir double quotes space up down');
is($datarec ->get_filename,'file.csv','data record filename double quotes space');
is($datarec->format_filename(write_directory=>$tempdir.'other space',
							 relative_data_path=>1),'"..'.$dirsep.'subdir space'.$dirsep.'file.csv"','format_filname space relative up then down');
is($datarec->format_filename(write_directory=>$tempdir,
							 relative_data_path=>1),'"subdir space'.$dirsep.'file.csv"','format_filname relative down');

$datarec = model::problem::data->new(record_arr => ['$DATA "file.csv" '], model_directory => $tempdir);
is($datarec->format_filename(write_directory=>$tempdir.'subdir',
							 relative_data_path=>1),'..'.$dirsep.'file.csv','format_filname '.$datarec->format_filename(write_directory=>$tempdir.'subdir',
																														relative_data_path=>1));


$datarec = model::problem::data->new(record_arr => ["\$DATA '".$tempdir."subdir space".$dirsep."file.csv' "]);
($dir,$file)=OSspecific::absolute_path($tempdir.'subdir space','file');
is($datarec ->get_directory,$dir,'data record dir single quotes space');
is($datarec ->get_filename,'file.csv','data record filename single quotes space');

$datarec = model::problem::data->new(record_arr => [" '".$tempdir."subdir space".$dirsep."file.csv' "]);
($dir,$file)=OSspecific::absolute_path($tempdir.'subdir space','file');
is($datarec ->get_directory,$dir,'data record dir single quotes space no recname');
is($datarec ->get_filename,'file.csv','data record filename single quotes space no recname');

$datarec = model::problem::data->new(record_arr => [$tempdir.'subdir/file.csv ']);
($dir,$file)=OSspecific::absolute_path($tempdir.'subdir','file');
is($datarec ->get_directory,$dir,'data record dir');
is($datarec ->get_filename,'file.csv','data record filename');

is($datarec->format_filename(write_directory=>$tempdir.'subdir',
							 relative_data_path=>1),'file.csv','format_filname relative local');
is($datarec->format_filename(write_directory=>$tempdir,
							 relative_data_path=>1),File::Spec->catfile('subdir','file.csv'),'format_filname relative down');
is($datarec->format_filename(write_directory=>$tempdir.'subdir/subdir2',
							 relative_data_path=>1),File::Spec->catfile('..','file.csv'),'format_filname relative up');
is($datarec->format_filename(write_directory=>$tempdir.'subdir3',
							 relative_data_path=>1),File::Spec->catfile('..','subdir','file.csv'),'format_filname relative up then down');

is($datarec->format_filename(write_directory=>$tempdir.'subdir',
							 relative_data_path=>0),File::Spec->catfile($tempdir.'subdir','file.csv'),'format_filname abs local');
is($datarec->format_filename(write_directory=>$tempdir,
							 relative_data_path=>0),File::Spec->catfile($tempdir.'subdir','file.csv'),'format_filname abs up');
is($datarec->format_filename(write_directory=>$tempdir.'subdir/subdir2',
							 relative_data_path=>0),File::Spec->catfile($tempdir.'subdir','file.csv'),'format_filname abs down');
is($datarec->format_filename(write_directory=>$tempdir.'subdir3',
							 relative_data_path=>0),File::Spec->catfile($tempdir.'subdir','file.csv'),'format_filname abs up-down');


my ($homedir,$dirt) = OSspecific::absolute_path(getcwd(),'file');
$datarec->set_filename(filename=> 'new.csv');
is($datarec ->get_directory,$homedir,'data record dir after change');
is($datarec ->get_filename,'new.csv','data record filename after change');
($dir,$dirt) = OSspecific::absolute_path($homedir.'sub','file');
$datarec->set_filename(filename=> $homedir.'sub/new2.csv');
is($datarec ->get_directory,$dir,'data record dir after change 2');
is($datarec ->get_filename,'new2.csv','data record filename after change 2');
($dir,$dirt) = OSspecific::absolute_path($homedir.'other','file');
$datarec->set_filename(filename=> $homedir.'sub/../other/new3.csv');
is($datarec ->get_directory,$dir,'data record dir after change 3');
is($datarec ->get_filename,'new3.csv','data record filename after change 3');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)','IGN =@']);
is($datarec->ignoresign,'@','data record ignoresign at');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=@ IGNORE=(DOSE.GT.5)'."\n",'format record ignoresign at');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)']);
is($datarec->ignoresign,undef,'data record ignoresign undef');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=(DOSE.GT.5)'."\n",'format record ignoresign undef');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNOR= # IGNORE =(DOSE.GT.5)']);
is($datarec->ignoresign,'#','data record ignoresign hash');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=# IGNORE=(DOSE.GT.5)'."\n",'format record ignoresign hash');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNO = I ','IGNOR=(DOSE.GT.5)']);
is($datarec->ignoresign,'I','data record ignoresign I');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=I IGNOR=(DOSE.GT.5)'."\n",'format record ignoresign I');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE = ( DOSE.GT.5)','IGNORE=C REWIND']);
is($datarec->ignoresign,'C','data record ignoresign C');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=C IGNORE=(DOSE.GT.5) REWIND'."\n",'format record ignoresign C');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE=C  IGNORE = ( DOSE.GT.5,',' APGR.LT.2 ) REWIND']);
is($datarec->ignoresign,'C','data record ignoresign C split list');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=C IGNORE=(DOSE.GT.5, APGR.LT.2) REWIND'."\n",'format record split list');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE=C  IGNORE =  ','( DOSE.GT.5,',' APGR.LT.2 ) REWIND']);
is($datarec->ignoresign,'C','data record ignoresign C split list after =');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=C IGNORE (DOSE.GT.5, APGR.LT.2) REWIND'."\n",'format record split after =');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE=C  IGNORE ','( DOSE.GT.5,',' APGR.LT.2 ) REWIND']);
is($datarec->ignoresign,'C','data record ignoresign C split list after space');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=C IGNORE (DOSE.GT.5, APGR.LT.2) REWIND'."\n",'format record split after space list');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE=C  IGNORE  ( ',' DOSE.GT.5, APGR.LT.2 ) REWIND']);
is($datarec->ignoresign,'C','data record ignoresign C split list 2');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=C IGNORE ( DOSE.GT.5,APGR.LT.2) REWIND'."\n",'format record split list 2');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE=C  IGNORE  (  DOSE.GT.5, APGR.LT.2 ) REWIND']);
is($datarec->ignoresign,'C','data record ignoresign C list without =');
is($datarec->_format_record(write_directory=>$datarec->get_directory,relative_data_path=>1)->[0],
   '$DATA      file.csv IGNORE=C IGNORE (DOSE.GT.5,APGR.LT.2) REWIND'."\n",'format record without =');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE="  IGNORE  (  DOSE.GT.5, APGR.LT.2 ) REWIND']);
is($datarec->ignoresign,'"','data record ignoresign " 1');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE  (  DOSE.GT.5, APGR.LT.2 )  IGNORE="  ']);
is($datarec->ignoresign,'"','data record ignoresign " 2');

$datarec = model::problem::data->new(record_arr => ['file.csv IGNORE ',' (  DOSE.GT.5, APGR.LT.2 )  IGNORE = "  ']);
is($datarec->ignoresign,'"','data record ignoresign " 3');

dies_ok { model::problem::data->new(record_arr => ['file.csv IGNORE=c1  IGNORE  (  DOSE.GT.5, APGR.LT.2 ) REWIND'])} "IGNORE=c1";
dies_ok { model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)',"IGNORE='C' REWIND"]) } "Quoted ignoresign a";
dies_ok { model::problem::data->new(record_arr => ['file.csv IGNORE=(',"DOSE.GT.5) REWIND"]) } "split after opening par";
dies_ok { model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)','IGNORE="C" REWIND']) } "Quoted ignoresign b";
dies_ok { model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)','IGNORE " REWIND']) } "Quoted ignoresign no =";
dies_ok { model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)','IGNORE @ REWIND']) } "ignoresign no =";
dies_ok { model::problem::data->new(record_arr => ['file.csv IGNORE=(DOSE.GT.5)',"IGNORE=' REWIND"]) } "ignoresign ' ";




#model->idcolumn

my @inputs = ('$INPUT C ID','$INPUT ID DV MDV','$INPUT C IDCOL ID=PAT DV','$INPUT ID=PAT DV','$INPUT DV ID=PAT IDC');
my @idcolnum = (2,1,3,1,2);

for( my $i=0; $i<scalar(@inputs); $i++){
	my $dummy_prob = model::problem->new(ignore_missing_files=> 1,
										 prob_arr       => ['$PROB',$inputs[$i],'$DATA dummy.txt']);

	my $model = model->new(filename => 'dummy',
						   problems => [$dummy_prob],
						   ignore_missing_files => 1);
	is($model->idcolumn,$idcolnum[$i],'model idcolumn test '.$inputs[$i]);
}

my $dummy_prob = model::problem->new(ignore_missing_files=> 1,
									 prob_arr       => ['$PROB','$INPUT C PAT=ID','$DATA dummy.txt']);
$model = model->new(filename => 'dummy', problems => [$dummy_prob], ignore_missing_files => 1);
dies_ok {$model->idcolumn(problem_number=>1)} "PAT=ID in \$INPUT";


#model->idcolumns

for( my $i=0; $i<scalar(@inputs); $i++){
	my $dummy_prob = model::problem->new(ignore_missing_files=> 1,
										 prob_arr       => ['$PROB',$inputs[$i],'$DATA dummy.txt']);

	my $model = model->new(filename => 'dummy',
						   problems => [$dummy_prob],
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
	{ filename => '3_id_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', idcolumn => 2,data => ''},
	{ filename => '4_header_ID.csv', input => 'ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 1, row => 4, col => 1, val => 37 },
	{ filename => '4_header_ID.csv', input => 'ID TIME AMT WGT APGR DV', data => 'IGN=C', crash => 1 },
	{ filename => '4_header_ID.csv', input => 'ID TIME AMT WGT APGR DV', idcolumn => 1,data => ''},
	{ filename => '5_hash_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 2, row => 0, col => 2, val => 0 },
	{ filename => '5_hash_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=C', crash => 1 },
	{ filename => '5_hash_header_leading_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => '', idcolumn => 2, row => 2, col => 3, val => 3.5 },
	{ filename => '6_header_hash.csv', input => 'ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 1, col => 0, row => 0, val => 1 },
	{ filename => '6_header_hash.csv', input => 'ID TIME AMT WGT APGR DV', data => 'IGN=C', crash => 1 },
	{ filename => '6_header_hash.csv', input => 'ID TIME AMT WGT APGR DV', data => '', idcolumn => 1, col => 1, row => 1, val => 2 },
	{ filename => '7_C_lead_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 2, row => 2, col => 2, val => 12.5 },
	{ filename => '7_C_lead_empty.csv', input => 'C ID TIME AMT WGT APGR DV', data => 'IGN=C', idcolumn => 2, row => 0, col => 4, val => 1.4 },
	{ filename => '7_C_lead_empty.csv', input => 'C ID TIME AMT WGT APGR DV', idcolumn => 2,data => ''},
	{ filename => '8_C_lead_dummy.csv', input => 'C ID TIME AMT WGT APGR DC', data => 'IGN=@', idcolumn => 2, row => 7, col => 3, val => 3.5 },
	{ filename => '8_C_lead_dummy.csv', input => 'C ID TIME AMT WGT APGR DC', data => 'IGN=C', idcolumn => 2, row => 1, col => 2, val => 2.0 },
	{ filename => '8_C_lead_dummy.csv', input => 'C ID TIME AMT WGT APGR DC', idcolumn => 2,data => ''},
	{ filename => '9_lead_posneg.csv', input => 'PNEG ID TIME AMT WGT APGR DV', data => 'IGN=@', idcolumn => 2, row => 0, col => 4, val => '1.4' },
	{ filename => '9_lead_posneg.csv', input => 'PNEG ID TIME AMT WGT APGR DV', data => 'IGN=C', crash => 1 },
	{ filename => '9_lead_posneg.csv', input => 'PNEG ID TIME AMT WGT APGR DV', idcolumn => 2,data => ''},
	{ filename => '10_quoted_header.csv', input => 'ID TIME AMT WGT APGR DV', data => ' IGN=" ', idcolumn => 1, row => 4, col => 2, val => 3.5 },
	{ filename => '10_quoted_header.csv', input => 'ID TIME AMT WGT APGR DV', data => ' IGN=@ ', crash => 1},
);
my $problem;


foreach my $test_hash (@datafiletests) {
	$problem = model::problem->new(ignore_missing_files => 1,
								   prob_arr => ['$PROB', '$INPUT ' . $test_hash->{'input'},
												'$DATA ' . $datadir . $test_hash->{'filename'} . ' ' . $test_hash->{'data'}] );
	$model = model->new(filename => 'dummy',
						problems => [$problem],
						ignore_missing_files => 1);
	my $ignoresign = $problem->datas->[0]->ignoresign;
	my $idcol = $model->idcolumn(problem_number=>1);
	my $dataname = $model->datafiles(problem_numbers => [1],
									 absolute_path =>1)->[0];
	my $data;
	if (not $test_hash->{'crash'}) {
		$data = data->new(filename => $dataname,
						  idcolumn => $idcol,
						  ignoresign => $ignoresign);
	} else {
		if ($test_hash->{'filename'} eq '10_quoted_header.csv'){
			open STDERR, '>', File::Spec->devnull();       # Silence STDERR warning of non-ignored header in last test
		}
		dies_ok { data->new(filename => $dataname, idcolumn => $idcol, ignoresign => $ignoresign) }
		"bad ignore " . $test_hash->{'filename'};

	}

	if (not $test_hash->{'crash'}) {
		is($data->count_ind, 5, 'n individuals ' . $test_hash->{'filename'} . ' ' . $test_hash->{'data'});
		is($data->idcolumn, $test_hash->{'idcolumn'}, 'idcol ' . $test_hash->{'filename'});
		if (exists $test_hash->{'row'}) {
			my $column_ref = $data->column_to_array(column => $test_hash->{'col'});
			cmp_float ($column_ref->[$test_hash->{'row'}], $test_hash->{'val'}, $test_hash->{'filename'} .
					   ' value check row=' . $test_hash->{'row'} . ' col=' . $test_hash->{'col'});
		}
	}
}

done_testing();
