#!/usr/bin/perl

# Unit tests for data.pm
# Note that only a small portion of the module is tested

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;
use random;

#TODO check data copy with write_copy => 0, check that individuals and header are copied, and idcolumn attributes

my $data = data->new( 
	idcolumn             => 1,
	filename             => 'testdata_no_missing.csv',
	directory            => $includes::testfiledir,
	ignoresign => '@',
	ignore_missing_files => 0);
is($data->column_count,9,'column count 1');
my $dotdata = data->new( 
	idcolumn             => 1,
	filename             => 'testdata_with_dot.csv',
	directory            => $includes::testfiledir,
	ignoresign => '@',
	ignore_missing_files => 0);
is($dotdata->column_count,9,'column count 2');
is($dotdata->count_ind,10,'count indiv dotdata 1');


is_deeply($dotdata->factors(column=> 3),{ 0 =>[0,1,2,3,4,5,6,7,8,9],1 =>[0,1,2,3,4,5,6,7,8,9], 
										  2 =>[0,1,2,3,4,5,6,7,8,9],3 =>[0,1,2,3,4,5,6,7,8,9],
										  'Non-unique values found' => 1},'factors a');

is_deeply($dotdata->factors(column=> 3, return_occurences => 1), 
		  {0 =>  10,1=>10,2=> 10, 3=>10,'Non-unique values found' => 1}, 'factors b');
is_deeply($dotdata->factors(column_head=> 'TIME', return_occurences => 1), 
		  {0 =>  10,1=>10,2=> 10, 3=>10,'Non-unique values found' => 1}, 'factors c');

is_deeply($dotdata->factors(column=> 3, unique_in_individual => 0),{ 0 =>[0,1,2,3,4,5,6,7,8,9],1 =>[0,1,2,3,4,5,6,7,8,9], 
																	 2 =>[0,1,2,3,4,5,6,7,8,9],3 =>[0,1,2,3,4,5,6,7,8,9]},'factors d');

is_deeply($dotdata->factors(column=> 3, return_occurences => 1, unique_in_individual =>0), 
		  {0 =>  10,1=>10,2=> 10, 3=>10}, 'factors e');

is_deeply($dotdata->factors(column=> 2),{ 0 =>[0,1,2,3,4,5,6,7,8,9], 
										  30 =>[3],40 =>[4],50 =>[5],60 =>[6],70 =>[7],80 =>[8],90 =>[9],
										  'Non-unique values found' => 1},'factors f');

is_deeply($dotdata->factors(column=> 2,ignore_missing => 1),{ 0 =>[0,1,2,3,4,5,6,7,8,9], 
															  30 =>[3],40 =>[4],50 =>[5],60 =>[6],70 =>[7],80 =>[8],90 =>[9],
															  'Non-unique values found' => 1},'factors g');

is_deeply($dotdata->factors(column=> 2,ignore_missing => 1, unique_in_individual => 0),{ 0 =>[0,1,2,3,4,5,6,7,8,9], 
										  30 =>[3],40 =>[4],50 =>[5],60 =>[6],70 =>[7],80 =>[8],90 =>[9]},'factors h');

my @parts = @{$dotdata->individuals()}[0 .. 2];

my $dotpartdata = data->new( 
	idcolumn             => 1,
	filename             => 'dummy',
	directory            => $includes::testfiledir,
	ignoresign => '@',
	ignore_missing_files => 0,
	header => $dotdata->header,
	individuals => \@parts
	);

is_deeply($dotpartdata->factors(column=> 2,ignore_missing => 1, unique_in_individual => 1),
		  { 0 =>[0,1,2] },'factors i');

is_deeply($dotpartdata->factors(column=> 2,ignore_missing => 0, unique_in_individual => 1),
		  { 0 =>[0,1,2] },'factors j');

# Test of reconcile_column
is_deeply ($data->reconcile_column(template_values=>[1,2,3,4], old_values=> [1,1,1], equal_obs =>1), [1,2,3], 
   "reconcile_column equal_obs template longer");
is_deeply ($data->reconcile_column(template_values=>[1,2,3], old_values=> [1,1,1,1], equal_obs =>1), [1,2,3,3], 
   "reconcile_column equal_obs template shorter");
is_deeply ($data->reconcile_column(template_values=>[1,2,3], old_values=> [1,1,1], equal_obs =>1), [1,2,3], 
   "reconcile_column equal_obs template equal");
is_deeply ($data->reconcile_column(template_values=>[1,'.',3], old_values=> [1,1,1], equal_obs =>1), [1,'.',3], 
   "reconcile_column equal_obs template equal with dots");
is_deeply ($data->reconcile_column(template_values=>[0,2,2], old_values=> [5,5,1], equal_obs =>0), [0,0,2], 
   "reconcile_column not equal_obs template equal");
is_deeply ($data->reconcile_column(template_values=>[0,0,0], old_values=> [8,8,1], equal_obs =>0), [0,0,0], 
   "reconcile_column not equal_obs template shorter");
is_deeply ($data->reconcile_column(template_values=>[0,1,1,2], old_values=> [3,3,4], equal_obs =>0), [0,0,1], 
   "reconcile_column not equal_obs template longer");

is_deeply ($data->reconcile_column(template_values=>[0,1,1], old_values=> [0,0,2,2], equal_obs =>1), [0,1,1,1], 
	  "reconcile_column equal_obs template shorter userguide");
is_deeply ($data->reconcile_column(template_values=>[0,1,-99,2], old_values=> [3,3,4], equal_obs =>1), [0,1,-99], 
	  "reconcile_column equal_obs missing value");
is_deeply ($data->reconcile_column(template_values=>[0,1,1], old_values=> [0,0,2,2], equal_obs =>0), [0,0,1,1], 
	  "reconcile_column not equal_obs userguide ");

is_deeply ($data->reconcile_column(template_values=>[0,1,1,1], old_values=> [0,0,2,3], equal_obs =>0), [0,0,1,1], 
	  "reconcile_column not equal_obs userguide ");

is_deeply ($data->reconcile_column(template_values=>[1,-99,1,0], old_values=> [0,0,2,2], equal_obs =>0), [1,1,0,0], 
	  "reconcile_column not equal_obs missing userguide 1");
is_deeply ($data->reconcile_column(template_values=>[1,1,1,0], old_values=> [0,-99,2,2], equal_obs =>0), [1,1,0,0], 
	  "reconcile_column not equal_obs missing userguide 2");
is_deeply ($data->reconcile_column(template_values=>[-99,1,1,0], old_values=> [0,0,2,2], equal_obs =>0), [1,1,0,0], 
	  "reconcile_column not equal_obs missing userguide 3");
is_deeply ($data->reconcile_column(template_values=>[0,0,1,1], old_values=> ['.',0,0,0], equal_obs =>0), [0,1,1,1], 
	  "reconcile_column not equal_obs missing userguide 4");

dies_ok {$data->reconcile_column(template_values=>[0,1,1,2], old_values=> [3,3,4], equal_obs =>undef)  } "undef param equal_obs";
dies_ok {$data->reconcile_column(template_values=>[], old_values=> [3,3,4], equal_obs =>undef)  } "empty template";

#split_vertically
my ($left, $right, $split_val, $strat_val)=$data->split_vertically(split_index => 1, stratify_index =>8);
for (my $i = 0; $i < 10; $i++) {
	is_deeply ($split_val->[$i],[$i*10,$i*10,$i*10,$i*10],"split vertically split_val indiv $i");
}
is_deeply($strat_val,[1,1,1,1,2,2,2,2,2,2],"split_vertically strat values");
#append_column
#$data->individuals

my $tempdir = create_test_dir('unit_data');

random_set_seed_from_phrase('12345');
my $arr = $data->_randomize_data(samples => 1, rand_index => 1, stratify_index => 8, equal_obs => 1, directory => $tempdir);
my $filename = 'rand_1.dta';

my $newdata = data->new( 
   idcolumn             => 1,
   filename             => $filename,
   directory            => $tempdir,
   ignoresign => '@',
   ignore_missing_files => 0
);

is_deeply ($newdata->individuals()->[3]->subject_data(), ['4,10,0,110.44,0,84,1,1,1',
							'4,10,1,96.554,0,84,1,1,1',
							'4,10,2,104.34,0,84,1,1,1',
							'4,10,3,123.64,0,84,1,1,1'], "randomized data indiv 3");

is_deeply ($newdata->individuals()->[7]->subject_data(),['8,50,0,64.938,5,66,1,1,2',
							'8,50,1,91.229,5,66,1,1,2',
							'8,50,2,100.24,5,66,1,1,2',
							'8,50,3,87.51,5,66,1,1,2'],"randomized data indiv 7");

unlink("$tempdir/$filename");

random_set_seed_from_phrase('12345');
$arr = $dotdata->_randomize_data(samples => 1,rand_index=> 1, equal_obs=>0, directory => $tempdir);

$newdata = data ->new( 
	idcolumn             => 1,
	filename             => $filename,
	directory            => $tempdir,
	ignoresign => '@',
	ignore_missing_files => 0);

is_deeply ($newdata->individuals()->[0]->subject_data(),['1,0,0,207.53,0,57,1,1,1',
							'1,0,1,227.64,0,57,1,1,1',
							'1,0,2,256.08,0,57,1,1,1',
							'1,0,3,262.82,0,57,1,1,1'],"randomized dot data indiv 0");

is_deeply ($newdata->individuals()->[3]->subject_data(),['4,0,0,110.44,0,84,1,1,1',
							'4,0,1,96.554,0,84,1,1,1',
							'4,0,2,104.34,0,84,1,1,1',
							'4,0,3,123.64,0,84,1,1,1'],"randomized dot data indiv 3");
unlink("$tempdir/$filename");

#this is for frem helper functions
my $phifile = $includes::testfiledir . "/mox1.phi";
my $phi = data->new(
	filename => $phifile,
	ignoresign => '@',
	parse_header => 1
);
my $eta_matrix = $phi->get_eta_matrix(start_eta => 4, n_eta => 4);
is_deeply ($eta_matrix->[0],['1.97271E-06','-2.92739E-06','1.83230E-05','5.73266E-06'],"eta matrix start_eta 4 n_eta 4 row index 0");
is_deeply ($eta_matrix->[5],['1.29845E-05','-1.44714E-05','1.98524E-05','8.67448E-05'],"eta matrix start_eta 4 n_eta 4 row index 5");
is_deeply ($eta_matrix->[73],['8.98237E-06','-1.58557E-05','-6.15118E-05','9.96136E-05'],"eta matrix start_eta 4 n_eta 4 row index 73");

# full_name
$data = data->new(filename => $phifile,
	ignoresign => '@',
	parse_header => 1,
);
$data->filename('perl');

if ($^O =~ /Win/) {
	$data->directory('C:\usr\bin');
	is($data->full_name, 'C:\usr\bin\perl', "Test windows path"); 
	$data->directory("C:\\usr\\bin\\");
	is($data->full_name, 'C:\usr\bin\perl', "Test windows path with trailing slash");
} else {
	$data->directory('/usr/bin');
	is($data->full_name, '/usr/bin/perl', "Test UNIX path");
	$data->directory('/usr/bin/');
	is($data->full_name, '/usr/bin/perl', "Test UNIX path with trailing slash");
}

# New
$filename = "$tempdir/test.dta";
open my $fh, '>', $filename;
print $fh "C,ID,SMTH,TEST\n";
print $fh ",0,1,-99\n";
print $fh ",0,2,-99\n";
print $fh ",0,3,-99\n";
print $fh ",1,10,5\n";
print $fh ",1,10,-99\n";
print $fh ",1,10,3\n";
print $fh ",1,10,3\n";
print $fh ",1,10,-99\n";
print $fh ",1,10,5\n";
print $fh ",2,3,7\n";
close $fh;

$data = data->new(filename => $filename, 
				  directory => $tempdir,
				  ignoresign => '@', 
				  parse_header => 1,
				  missing_data_token => '-99');

my $factors = $data ->factors(column => 4,ignore_missing =>1);
is(data::_have_non_unique_values($factors),1,'non-unique values in data yes');
is(data::_have_missing_values(factors=>$factors, missing_data_token => -99),1,'mising values in data yes');
$factors = $data ->factors(column => 2,ignore_missing =>1);
is(data::_have_non_unique_values($factors),0,'non-unique values in data ID');
is(data::_have_missing_values(factors=>$factors, missing_data_token => -99),0,'mising values in data no');

my $statistics = $data->lasso_get_categorical_statistics(column_number => 4,
														 missing_data_token => '-99');

is($statistics->{'most_common'},7,"lasso_get_categorical_statistics most common");
cmp_float($statistics->{'cat_hash'}{-99},4/3,"lasso_get_categorical_statistics sum -99");
cmp_float($statistics->{'cat_hash'}{5},1/3,"lasso_get_categorical_statistics sum 5");
cmp_float($statistics->{'cat_hash'}{3},1/3,"lasso_get_categorical_statistics sum 3");
cmp_float($statistics->{'cat_hash'}{7},1,"lasso_get_categorical_statistics sum 7");
cmp_float($statistics->{'mean'}{-99},4/9,"lasso_get_categorical_statistics mean -99");
cmp_float($statistics->{'mean'}{5},1/9,"lasso_get_categorical_statistics mean 5");
cmp_float($statistics->{'mean'}{3},1/9,"lasso_get_categorical_statistics mean 3");
cmp_float($statistics->{'mean'}{7},1/3,"lasso_get_categorical_statistics mean 7");
cmp_float($statistics->{'sd'}{-99},sqrt((25/81+1/81+16/81)/2),"lasso_get_categorical_statistics sd -99");
cmp_float($statistics->{'sd'}{5},sqrt((1/81+4/81+1/81)/2) ,"lasso_get_categorical_statistics sd 5");
cmp_float($statistics->{'sd'}{3},sqrt((1/81+4/81+1/81)/2),"lasso_get_categorical_statistics sd 3");
cmp_float($statistics->{'sd'}{7},sqrt((1/9+1/9+4/9)/2),"lasso_get_categorical_statistics sd 7");

$statistics = $data->lasso_get_categorical_statistics(column_number => 3,
														 missing_data_token => '-99');

is($statistics->{'most_common'},3,"lasso_get_categorical_statistics most common");
cmp_float($statistics->{'cat_hash'}{10},1,"lasso_get_categorical_statistics sum 10");
cmp_float($statistics->{'cat_hash'}{3},4/3,"lasso_get_categorical_statistics sum 3");
cmp_float($statistics->{'cat_hash'}{2},1/3,"lasso_get_categorical_statistics sum 2");
cmp_float($statistics->{'cat_hash'}{1},1/3,"lasso_get_categorical_statistics sum 1");
cmp_float($statistics->{'mean'}{10},1/3,"lasso_get_categorical_statistics mean 10");
cmp_float($statistics->{'mean'}{3},4/9,"lasso_get_categorical_statistics mean 3");
cmp_float($statistics->{'mean'}{2},1/9,"lasso_get_categorical_statistics mean 2");
cmp_float($statistics->{'mean'}{1},1/9,"lasso_get_categorical_statistics mean 1");
cmp_float($statistics->{'sd'}{10},sqrt((1/9+1/9+4/9)/2),"lasso_get_categorical_statistics sd 10");
cmp_float($statistics->{'sd'}{3},sqrt((1/81+16/81+25/81)/2) ,"lasso_get_categorical_statistics sd 3");
cmp_float($statistics->{'sd'}{2},sqrt((4/81+1/81+1/81)/2),"lasso_get_categorical_statistics sd 2");
cmp_float($statistics->{'sd'}{1},sqrt((4/81+1/81+1/81)/2),"lasso_get_categorical_statistics sd 1");




# mean
is ($data->mean(column => 3), 5, "data->mean of small data set column 1");
is ($data->mean(column => 3, global_mean => 1 ), 6.9, "global data->mean of small data set column 1");
is ($data->mean(column => 4), 5.5, "data->mean of small data set column 2");
is ($data->mean(column => 4,global_mean=> 1), eval(23/5), "global data->mean of small data set column 2");
dies_ok { $data->mean(column => 5) } "data->mean for non existing column";
is ($data->mean(column_head => 'TEST'), 5.5, "data->mean with column name");
is ($data->mean(column => 3, hi_cutoff => 4), 2, "data->mean with hi_cutoff");
is ($data->mean(column => 4, hi_cutoff => 4), 1.75, "data->mean with hi_cutoff");

#sd
my $sd = (((2-5)**2+(10-5)**2+(3-5)**2)/2)**0.5;
is ($data->sd(column => 3), $sd, "data->sd of small data set column 1");
$sd = (((1-6.9)**2+(2-6.9)**2+(3-6.9)**2+((10-6.9)**2)*6+(3-6.9)**2)/9)**0.5;
is ($data->sd(column => 3, global_sd => 1), $sd, "data->sd of small data set column 1");
cmp_float ($data->sd(column => 3, hi_cutoff => 4), 3.4641016151378, "data->sd with hi_cutoff");
cmp_float ($data->sd(column => 4, hi_cutoff => 4), 1.25, "data->sd with hi_cutoff");

#min
is ($data->min(column => 3), 1, "data->min of small data set column 1");
is ($data->min(column => 4), 3, "data->min of small data set column 2");
dies_ok { $data->min(column => 8) } "data->min for non existing column";
is ($data->min(column_head => 'TEST'), 3, "data->min with column name");

#max
is ($data->max(column => 3), 10, "data->max of small data set column 1");
is ($data->max(column => 4), 7, "data->max of small data set column 2");
dies_ok { $data->max(column => 7) } "data->max for non existing column";
is ($data->max(column_head => 'TEST'), 7, "data->max with column name");

#median
is ($data->median(column => 3), 3, "data->median of small data set column 1");
is ($data->median(column => 3,global_median=>1), 10, "global data->median of small data set column 1");
is ($data->median(column => 4), 5.5, "data->median of small data set column 2");
is ($data->median(column => 4, global_median=>1), 5, "global data->median of small data set column 2");
dies_ok { $data->median(column => 6) } "data->median for non existing column";
is ($data->median(column_head => 'TEST'),5.5 , "data->median with column name");

#range
is ($data->range(column => 3), 9, "data->range of small data set column 1");
is ($data->range(column => 3), 9, "data->range of small data set column 1 again");
is ($data->range(column => 4), 4, "data->range of small data set column 2");
is ($data->range(column => 4), 4, "data->range of small data set column 2 again");
dies_ok { $data->range(column => 10) } "data->range for non existing column";
is ($data->range(column_head => 'TEST'), 4, "data->range with column name");
is ($data->range(column_head => 'TEST'), 4, "data->range with column name again");
is($data->column_head_indices->{'TEST'},4,'column head indices start at 1');

#column_to_array
is_deeply($data->column_to_array(column => 2), [1,2,3,10,10,10,10,10,10,3] , "data->column_to_array of small data set column 1");
is_deeply($data->column_to_array(column => 2, filter => [0, 1, 1, 0, 1, 0, 0, 0 ,1 ,1]), [2,3,10,10,3], "data->column_to_array of small data set with filter 1");
is_deeply($data->column_to_array(column => 1, filter => [0, 0, 0, 0 ,0 ,0, 0 ,0 ,0 ,0]), [], "data->column_to_array of small data set with zero filter");
dies_ok { $data->column_to_array(column => 1, filter => [0, 0, 0]) } "data->column_to_array of small data set with non-mulitple length filter";

#merge
my $filename_merge = "$tempdir/test_merge.dta";
open $fh, '>', $filename_merge;
print $fh "C,ID,SMTH,TEST\n";
print $fh ",1,10,5\n";
print $fh ",1,20,6\n";
print $fh ",1,30,7.23\n";
close $fh;

my $data_merge = data->new(filename => $filename_merge, directory => $tempdir, ignoresign => '@',parse_header =>1);
$data_merge->merge(mergeobj => $data);
is ($data_merge->mean(column => 3), eval(35/4), "data->merge checking new mean");

#count_ind
is ($data_merge->count_ind, 4, "data->count_ind");
is ($data_merge->idcolumn, 2, "data->idcolumn");

my $copy = $data_merge->copy(filename => 'dirt',
							 write_copy => 0);
is ($copy->count_ind, 4, "copied data count_ind");
is ($copy->idcolumn, 2, "copied data idcol");


#data set parsing
my $filename_spec = "$tempdir/test_spec.dta";
open $fh, '>', $filename_spec;
print $fh "BACK,SMTH,ID\n";
print $fh "1, 0, 1\n";
print $fh ", 0, 1\n";
print $fh ",,  1\n";
close $fh;

my $data_spec = data->new(filename => $filename_spec, directory => $tempdir, ignoresign => '@',parse_header =>1);
is_deeply ($data_spec->individuals->[0]->subject_data, ['1,0,1', '0,0,1', '0,0,1'], "data->new starts with commas");


# New
$filename = "$tempdir/test.dta";
open $fh, '>', $filename;
print $fh "ID,A,SMTH,TEST\n";
print $fh "0,-99,1.0,-99\n";
print $fh "0,-99,2.0,-99\n";
print $fh "0,-99,3.0,-99\n";
print $fh "1,2,9.0,5.1\n";
print $fh "1,2,9.0,-99\n";
print $fh "1,2,9.0,3\n";
print $fh "1,2,9.0,3\n";
print $fh "1,2,9.0,-99\n";
print $fh "1,2,9.0,5.1\n";
print $fh "2,1,3.0,7\n";
print $fh "3,1,3.0,7\n";
close $fh;

$data = data->new(filename => $filename, 
					 directory => $tempdir,
					 ignoresign => '@', 
					 parse_header => 1);

my ($mapping,$new_indices,$new_categorical,$warn) = $data->append_binary_columns(indices => [1,2,3],
																				 baseline_only => 1,
																				 mdv_evid_indices => []); 

is_deeply($mapping->[0],[],'mapping 0 only two nonmiss baseline');
is_deeply($mapping->[1],['9.0','3.0'],'mapping 1 baseline skipping 2');
is_deeply($mapping->[2],[],'mapping 2 baseline only two nonmiss');
is_deeply($warn,['SMTH','TEST'],'baseline warn multiple');

is_deeply($new_indices,[1,4,5,3],'new indices baseline');

is($data->column_count,6,'column count after append baseline');
is_deeply($data->header,['ID','A','SMTH','TEST','SMTH_9','SMTH_3'],'new header after append basline');
is_deeply($new_categorical,['A','SMTH_9','SMTH_3','TEST'],'new categorical baseline');



$data = data->new(filename => $filename, 
					 directory => $tempdir,
					 ignoresign => '@', 
					 parse_header => 1);

($mapping,$new_indices,$new_categorical,$warn) = $data->append_binary_columns(indices => [1,2,3],
																		   baseline_only => 0); 

is_deeply($mapping->[0],[],'mapping 0 only two nonmiss');
is_deeply($mapping->[1],['9.0','3.0','2.0'],'mapping 1');
is_deeply($mapping->[2],[7,5.1],'mapping 2');
is_deeply($warn,[],'regular warn multiple');

is_deeply($new_indices,[1,4,5,6,7,8],'new indices');

is($data->column_count,9,'column count after append');
is_deeply($data->header,['ID','A','SMTH','TEST','SMTH_9','SMTH_3','SMTH_2','TEST_7','TEST_51'],'new header after append');
is_deeply($new_categorical,['A','SMTH_9','SMTH_3','SMTH_2','TEST_7','TEST_51'],'new categorical');

$data = data->new(filename => $filename, 
					 directory => $tempdir,
					 ignoresign => '@', 
					 parse_header => 1);

($mapping,$new_indices,$new_categorical,$warn) = $data->append_binary_columns(indices => [1,2,3],
																		   baseline_only => 0,
																		   start_header => ['ID','F','G','H']); 
is_deeply($data->header,['ID','F','G','H','G_9','G_3','G_2','H_7','H_51'],'new header after append 2');
is_deeply($new_categorical,['F','G_9','G_3','G_2','H_7','H_51'],'new categorical');
is_deeply($warn,[],'regular warn multiple 2');

#have_unique_ids
$data = data->new(filename => '4_header_ID.csv', directory => $includes::testfiledir . '/data', ignoresign => '@', parse_header => 1);
ok ($data->have_unique_ids(), "4_header_ID.csv has unique IDs");
$data = data->new(filename => 'non_unique_ids.csv', directory => $includes::testfiledir . '/data', ignoresign => '@', parse_header => 1);
ok (!$data->have_unique_ids(), "non_unique_ids.csv does not have unique IDs");

remove_test_dir($tempdir);

done_testing;
