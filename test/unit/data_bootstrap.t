#!/usr/bin/perl

# Unit tests for bootstrap in data.pm
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

#test for data class subroutine for bootstrap
my $tempdir = create_test_dir('unit_data_bootstrap');

cp($includes::testfiledir.'/pheno.dta',$tempdir);
my ( $new_datas, $incl_ids, $incl_keys, $new_subjects, $orig_count_ind )
	= data::bootstrap_create_datasets( output_directory   => $tempdir,
									   name_stub   => 'bs_pr1',
									   samples     => 2,
									   subjects    => {},
									   stratify_on => undef, 
									   input_filename => $tempdir.'pheno.dta',
									   ignoresign => '@',
									   idcolumn => 1,  #number not index
									   missing_data_token => '-99'	);


is($orig_count_ind,59,'bootstrap inds');
is($new_subjects->{'default'},59,'bootstrap subjects');
is($new_datas->[0],$tempdir.'bs_pr1_1.dta','boot 1');
is($new_datas->[1],$tempdir.'bs_pr1_2.dta','boot 2');
is(-e $new_datas->[0],1,'e boot 1');
is(-e $new_datas->[1],1,'e boot 2');
my $data = data->new( 
	idcolumn             => 1,
	filename             => $new_datas->[1],
	ignoresign => '@');
is($data->count_ind,59,'count boot data 2');
$data = data->new( 
	idcolumn             => 1,
	filename             => $new_datas->[0],
	ignoresign => '@');
is($data->count_ind,59,'count boot data 1');


cp($includes::testfiledir.'/data/with_dates_and_times.csv',$tempdir);
($new_datas, $incl_ids, $incl_keys, $new_subjects, $orig_count_ind )
	= data::bootstrap_create_datasets( output_directory   => $tempdir,
	name_stub   => 'bsa_',
	samples     => 2,
	subjects    => {},
	stratify_on => undef, 
	input_filename => $tempdir.'with_dates_and_times.csv',
	ignoresign => '@',
	idcolumn => 1,  #number not index
	missing_data_token => '-99'	);

is($orig_count_ind,11,'bootstrap inds');
is($new_subjects->{'default'},11,'bootstrap subjects');

remove_test_dir($tempdir);

done_testing;
