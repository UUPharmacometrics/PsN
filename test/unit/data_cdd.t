#!/usr/bin/perl

# Unit tests for cdd in data.pm
#

use strict;
use warnings;
use Test::More tests=>28;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;
use model::problem::data;
use model::problem;
use model;
use File::Copy 'cp';

#test for data class subroutine for cdd 
my $tempdir = create_test_dir('unit_data_cdd');

cp($includes::testfiledir.'/pheno5.dta',$tempdir);
my ($new_datas, $skip_ids, $skip_keys, $skip_values, $remainders, $pr_bins) = 
	data::cdd_create_datasets(input_filename => $tempdir.'pheno5.dta',
							  bins => undef,
							  case_column => 1, #number 1 is first
							  selection_method => undef,
							  output_directory => $tempdir,
							  ignoresign => '@',
							  idcolumn => 1,  #number not index
							  missing_data_token => '-99');

is($pr_bins,5,'cdd actual bins');
is($new_datas->[0],$tempdir.'cdd_1.dta','cdd 1');
is($new_datas->[1],$tempdir.'cdd_2.dta','cdd 2');
is($new_datas->[2],$tempdir.'cdd_3.dta','cdd 3');
is($new_datas->[3],$tempdir.'cdd_4.dta','cdd 4');
is($new_datas->[4],$tempdir.'cdd_5.dta','cdd 5');
is($remainders->[0],$tempdir.'rem_1.dta','rem 1');
is($remainders->[1],$tempdir.'rem_2.dta','rem 2');
is($remainders->[2],$tempdir.'rem_3.dta','rem 3');
is($remainders->[3],$tempdir.'rem_4.dta','rem 4');
is($remainders->[4],$tempdir.'rem_5.dta','rem 5');
is(-e $new_datas->[0],1,'e cdd 1');
is(-e $new_datas->[1],1,'e cdd 2');
is(-e $new_datas->[2],1,'e cdd 3');
is(-e $new_datas->[3],1,'e cdd 4');
is(-e $new_datas->[4],1,'e cdd 5');
is(-e $remainders->[0],1,'e rem 1');
is(-e $remainders->[1],1,'e rem 2');
is(-e $remainders->[2],1,'e rem 3');
is(-e $remainders->[3],1,'e rem 4');
is(-e $remainders->[4],1,'e rem 5');
is($skip_ids->[0]->[0],1,'skip id 1');
is($skip_ids->[1]->[0],2,'skip id 2');
is($skip_ids->[2]->[0],3,'skip id 3');
is($skip_ids->[3]->[0],4,'skip id 4');
is($skip_ids->[4]->[0],5,'skip id 5');

my $data = data->new( 
	idcolumn             => 1,
	filename             => $new_datas->[3],
	ignoresign => '@');
is($data->count_ind,4,'count cdd data 4');
$data = data->new( 
	idcolumn             => 1,
	filename             => $remainders->[2],
	ignoresign => '@');
is($data->count_ind,1,'count rem data 3');

remove_test_dir($tempdir);

#done_testing;
