#!/usr/bin/perl

# Unit tests for data.pm
# Note that only a small portion of the module is tested

use strict;
use warnings;
use Test::More tests=>199;
use Test::Exception;
use Math::Random;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;

my $data = data->new( 
   idcolumn             => 1,
   filename             => 'testdata_no_missing.csv',
   directory            => $includes::testfiledir,
   ignoresign => '@',
   ignore_missing_files => 0,
   skip_parsing         => 0,
   target               => 'mem');
my $dotdata = data->new( 
   idcolumn             => 1,
   filename             => 'testdata_with_dot.csv',
   directory            => $includes::testfiledir,
   ignoresign => '@',
   ignore_missing_files => 0,
   skip_parsing         => 0,
   target               => 'mem');

# Test of reconcile_column
is_array ($data->reconcile_column(template_values=>[1,2,3,4], old_values=> [1,1,1], equal_obs =>1), [1,2,3], 
   "reconcile_column equal_obs template longer");
is_array ($data->reconcile_column(template_values=>[1,2,3], old_values=> [1,1,1,1], equal_obs =>1), [1,2,3,3], 
   "reconcile_column equal_obs template shorter");
is_array ($data->reconcile_column(template_values=>[1,2,3], old_values=> [1,1,1], equal_obs =>1), [1,2,3], 
   "reconcile_column equal_obs template equal");
is_array ($data->reconcile_column(template_values=>[1,'.',3], old_values=> [1,1,1], equal_obs =>1), [1,'.',3], 
   "reconcile_column equal_obs template equal with dots");
is_array ($data->reconcile_column(template_values=>[0,2,2], old_values=> [5,5,1], equal_obs =>0), [0,0,2], 
   "reconcile_column not equal_obs template equal");
is_array ($data->reconcile_column(template_values=>[0,0,0], old_values=> [8,8,1], equal_obs =>0), [0,0,0], 
   "reconcile_column not equal_obs template shorter");
is_array ($data->reconcile_column(template_values=>[0,1,1,2], old_values=> [3,3,4], equal_obs =>0), [0,0,1], 
   "reconcile_column not equal_obs template longer");

is_array ($data->reconcile_column(template_values=>[0,1,1], old_values=> [0,0,2,2], equal_obs =>1), [0,1,1,1], 
	  "reconcile_column equal_obs template shorter userguide");
is_array ($data->reconcile_column(template_values=>[0,1,-99,2], old_values=> [3,3,4], equal_obs =>1), [0,1,-99], 
	  "reconcile_column equal_obs missing value");
is_array ($data->reconcile_column(template_values=>[0,1,1], old_values=> [0,0,2,2], equal_obs =>0), [0,0,1,1], 
	  "reconcile_column not equal_obs userguide ");

is_array ($data->reconcile_column(template_values=>[0,1,1,1], old_values=> [0,0,2,3], equal_obs =>0), [0,0,1,1], 
	  "reconcile_column not equal_obs userguide ");

is_array ($data->reconcile_column(template_values=>[1,-99,1,0], old_values=> [0,0,2,2], equal_obs =>0), [1,1,0,0], 
	  "reconcile_column not equal_obs missing userguide 1");
is_array ($data->reconcile_column(template_values=>[1,1,1,0], old_values=> [0,-99,2,2], equal_obs =>0), [1,1,0,0], 
	  "reconcile_column not equal_obs missing userguide 2");
is_array ($data->reconcile_column(template_values=>[-99,1,1,0], old_values=> [0,0,2,2], equal_obs =>0), [1,1,0,0], 
	  "reconcile_column not equal_obs missing userguide 3");
is_array ($data->reconcile_column(template_values=>[0,0,1,1], old_values=> ['.',0,0,0], equal_obs =>0), [0,1,1,1], 
	  "reconcile_column not equal_obs missing userguide 4");

dies_ok {$data->reconcile_column(template_values=>[0,1,1,2], old_values=> [3,3,4], equal_obs =>undef)  } "undef param equal_obs";
dies_ok {$data->reconcile_column(template_values=>[], old_values=> [3,3,4], equal_obs =>undef)  } "empty template";

#split_vertically
my ($left, $right, $split_val, $strat_val)=$data->split_vertically(split_index => 1, stratify_index =>8);
for (my $i = 0; $i < 10; $i++) {
	is_array ($split_val->[$i],[$i*10,$i*10,$i*10,$i*10],"split vertically split_val indiv $i");
}
is_array($strat_val,[1,1,1,1,2,2,2,2,2,2],"split_vertically strat values");
#append_column
#$data->individuals


my $tempdir = create_test_dir('unit_data');

random_set_seed_from_phrase('12345');
my $arr = $data->randomize_data(samples => 1, rand_index => 1, stratify_index => 8, equal_obs => 1, directory => $tempdir);
my $filename = 'rand_1.dta';

my $newdata = data->new( 
   idcolumn             => 1,
   filename             => $filename,
   directory            => $tempdir,
   ignoresign => '@',
   ignore_missing_files => 0,
   skip_parsing         => 0,
   target               => 'mem'
);



is_array ($newdata->individuals()->[3]->subject_data(), ['4,30,0,110.44,0,84,1,1,1',
							'4,30,1,96.554,0,84,1,1,1',
							'4,30,2,104.34,0,84,1,1,1',
							'4,30,3,123.64,0,84,1,1,1'], "randomized data indiv 3");

is_array ($newdata->individuals()->[7]->subject_data(),['8,60,0,64.938,5,66,1,1,2',
							'8,60,1,91.229,5,66,1,1,2',
							'8,60,2,100.24,5,66,1,1,2',
							'8,60,3,87.51,5,66,1,1,2'],"randomized data indiv 7");

unlink("$tempdir/$filename");
random_set_seed_from_phrase('12345');
$arr = $dotdata->randomize_data(samples => 1,rand_index=> 1, equal_obs=>0, directory => $tempdir);

$newdata = data ->new( 
   idcolumn             => 1,
   filename             => $filename,
   directory            => $tempdir,
   ignoresign => '@',
   ignore_missing_files => 0,
   skip_parsing         => 0,
   target               => 'mem');

is_array ($newdata->individuals()->[0]->subject_data(),['1,0,0,207.53,0,57,1,1,1',
							'1,70,1,227.64,0,57,1,1,1',
							'1,70,2,256.08,0,57,1,1,1',
							'1,70,3,262.82,0,57,1,1,1'],"randomized dot data indiv 0");

is_array ($newdata->individuals()->[3]->subject_data(),['4,.,0,110.44,0,84,1,1,1',
							'4,0,1,96.554,0,84,1,1,1',
							'4,0,2,104.34,0,84,1,1,1',
							'4,0,3,123.64,0,84,1,1,1'],"randomized dot data indiv 3");


unlink("$tempdir/$filename");

#this is for frem helper functions
my $phifile = $includes::testfiledir . "/mox1.phi";
my $phi = data->new(filename => $phifile,
					ignoresign => '@'
	);
my $eta_matrix = $phi->get_eta_matrix(start_eta => 4, n_eta => 4);
is_array ($eta_matrix->[0],['1.97271E-06','-2.92739E-06','1.83230E-05','5.73266E-06'],"eta matrix start_eta 4 n_eta 4 row index 0");
is_array ($eta_matrix->[5],['1.29845E-05','-1.44714E-05','1.98524E-05','8.67448E-05'],"eta matrix start_eta 4 n_eta 4 row index 5");
is_array ($eta_matrix->[73],['8.98237E-06','-1.58557E-05','-6.15118E-05','9.96136E-05'],"eta matrix start_eta 4 n_eta 4 row index 73");


# full_name
my $data = data->new(filename => $phifile,
					ignoresign => '@'
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
my $filename = "$tempdir/test.dta";
open my $fh, '>', $filename;
print $fh "ID SMTH TEST\n";
print $fh "0 12.2 23.4\n";
print $fh "0 23.56 14\n";
print $fh "0 1.2 2.8\n";
close $fh;

my $data = data->new(filename => $filename, directory => $tempdir);

# mean
is ($data->mean(column => 2), 12.32, "data->mean of small data set column 1");
is ($data->mean(column => 3), 13.4, "data->mean of small data set column 2");
dies_ok { $data->mean(column => 4) } "data->mean for non existing column";
is ($data->mean(column_head => 'TEST'), 13.4, "data->mean with column name");
#is ($data->mean(column => 2, hi_cutoff => 10), 0, "data->mean with hi_cutoff");

#min
is ($data->min(column => 2), 1.2, "data->min of small data set column 1");
is ($data->min(column => 3), 2.8, "data->min of small data set column 2");
dies_ok { $data->min(column => 4) } "data->min for non existing column";
is ($data->min(column_head => 'TEST'), 2.8, "data->min with column name");

#max
is ($data->max(column => 2), 23.56, "data->max of small data set column 1");
is ($data->max(column => 3), 23.4, "data->max of small data set column 2");
dies_ok { $data->max(column => 4) } "data->max for non existing column";
is ($data->max(column_head => 'TEST'), 23.4, "data->max with column name");

#median
is ($data->median(column => 2), 12.2, "data->median of small data set column 1");
is ($data->median(column => 3), 14, "data->median of small data set column 2");
dies_ok { $data->median(column => 4) } "data->median for non existing column";
is ($data->median(column_head => 'TEST'), 14, "data->median with column name");

#range
is ($data->range(column => 2), 22.36, "data->range of small data set column 1");
is ($data->range(column => 3), 20.6, "data->range of small data set column 2");
dies_ok { $data->range(column => 4) } "data->range for non existing column";
is ($data->range(column_head => 'TEST'), 20.6, "data->range with column name");

#column_to_array
is_array($data->column_to_array(column => 2), [23.4, 14, 2.8] , "data->column_to_array of small data set column 1");
is_array($data->column_to_array(column => 2, filter => [0, 1, 1]), [14, 2.8], "data->column_to_array of small data set with filter 1");
is_array($data->column_to_array(column => 1, filter => [0, 0, 0]), [], "data->column_to_array of small data set with zero filter");
dies_ok { $data->column_to_array(column => 1, filter => [0, 0]) } "data->column_to_array of small data set with too short filter";

#merge
my $filename_merge = "$tempdir/test_merge.dta";
open my $fh, '>', $filename_merge;
print $fh "ID SMTH TEST\n";
print $fh "1 19 5\n";
print $fh "1 28.9 6\n";
print $fh "1 33.1 7.23\n";
close $fh;

my $data_merge = data->new(filename => $filename_merge, directory => $tempdir);
$data_merge->merge(mergeobj => $data);
is ($data_merge->mean(column => 2), 19.66, "data->merge checking new mean");

#count_ind
is ($data_merge->count_ind, 2, "data->count_ind");


remove_test_dir($tempdir);

done_testing;
