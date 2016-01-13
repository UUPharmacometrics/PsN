#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data::individual;

#factors
my $ind = data::individual->new(idcolumn => 1, subject_data => ['1,0.0000e0,1.0000e0', '1,1.0000e0,0.0000e0', '1,1.0000e0,0.0000e0']);
my %factors = %{$ind->factors(column => 2)};
is_deeply([sort keys %factors], ['0.0000e0', '1.0000e0'], "data::individual->factors");

#update_idnumber
my $ind = data::individual->new(idcolumn => 1, subject_data => ['1,0.0000e0,1.0000e0', '1,1.0000e0,0.0000e0', '1,1.0000e0,0.0000e0']);
$ind->idnumber(2);
$ind->update_idnumber();
is_deeply($ind->subject_data, ['2,0.0000e0,1.0000e0', '2,1.0000e0,0.0000e0', '2,1.0000e0,0.0000e0'], "update_idnumber");

#append_column
my $ind = data::individual->new(idcolumn => 1, subject_data => ['1,0.0000e0,1.0000e0', '1,1.0000e0,0.0000e0', '1,1.0000e0,0.0000e0']);
$ind->append_column(new_values => [2, 3 ,4]);
is_deeply($ind->subject_data, ['1,0.0000e0,1.0000e0,2', '1,1.0000e0,0.0000e0,3', '1,1.0000e0,0.0000e0,4'], "append_column");
dies_ok { $ind->append_column(new_values => [1, 2]) } "append_column with illegal input";

#append_individual
my $ind = data::individual->new(idcolumn => 1, subject_data => ['1,0.0000e0,1.0000e0', '1,1.0000e0,0.0000e0', '1,1.0000e0,0.0000e0']);
my $ind2 = data::individual->new(idcolumn => 1, subject_data => ['1,2,3', '1,5,6', '1,8,9']);
$ind->append_individual(new_individual => $ind2);
is_deeply($ind->subject_data, [ '1,0.0000e0,1.0000e0,1,2,3', '1,1.0000e0,0.0000e0,1,5,6', '1,1.0000e0,0.0000e0,1,8,9' ], "append_individual");
my $ind3 = data::individual->new(idcolumn => 1, subject_data => ['1,2']);
dies_ok { $ind->append_individual(new_individual => $ind3) } "append_individual with illegal input";

#copy
my $ind = data::individual->new(idcolumn => 1, subject_data => ['1,0.0000e0,1.0000e0', '1,1.0000e0,0.0000e0', '1,1.0000e0,0.0000e0']);
my $ind2 = $ind->copy;
$ind2->append_column(new_values => [8, 9, 10]);
is_deeply($ind->subject_data, ['1,0.0000e0,1.0000e0', '1,1.0000e0,0.0000e0', '1,1.0000e0,0.0000e0'], "copy source individual");
is_deeply($ind2->subject_data, ['1,0.0000e0,1.0000e0,8', '1,1.0000e0,0.0000e0,9', '1,1.0000e0,0.0000e0,10'], "copy destination");


#add_frem_lines
#ID DV MDV TIME WGT DUM AGE FREMTYPE 
my $ind = data::individual->new(idcolumn => 1, subject_data => ['1,0,1,0,70,1,35,0', 
																'1,34,0,10,70,1,35,0',
																'1,21,0,20,70,1,35,0']);

$ind->add_frem_lines(type_index => 7,
					 N_parameter_blocks => 2,
					 mdv_index => 2,
					 dv_index => 1,
					 cov_indices => [4,6],
					 is_log => [0,0],
					 first_timevar_type => 2);

my $ans = [
	[1,70,0,10,70,1,35,100],
	[1,70,0,10,70,1,35,101],
	[1,35,0,10,70,1,35,200],
	[1,35,0,10,70,1,35,201]];

is($ind->subject_data->[0],'1,0,1,0,70,1,35,0', "add_frem_lines 0");
is($ind->subject_data->[1], join(',',@{$ans->[0]}), "add_frem_lines 0");
is($ind->subject_data->[2], join(',',@{$ans->[1]}), "add_frem_lines 1");
is($ind->subject_data->[3], join(',',@{$ans->[2]}), "add_frem_lines 2");
is($ind->subject_data->[4], join(',',@{$ans->[3]}), "add_frem_lines 3");
is($ind->subject_data->[5],'1,34,0,10,70,1,35,0', "add_frem_lines 4");
is($ind->subject_data->[6],'1,21,0,20,70,1,35,0', "add_frem_lines 5");

$ind = data::individual->new(idcolumn => 1, subject_data => ['1,0,1,0,70,1,35,0', 
																'1,34,0,10,70,1,35,0',
																'1,21,0,20,70,1,35,0']);

$ind->add_frem_lines(type_index => 7,
					 N_parameter_blocks => 1,
					 mdv_index => 2,
					 dv_index => 1,
					 cov_indices => [4,6],
					 is_log => [0,1],
					 first_timevar_type => 2);

my $ans = [
	[1,70,0,10,70,1,35,100],
	[1,log(35),0,10,70,1,35,200]];

is($ind->subject_data->[1], join(',',@{$ans->[0]}), "add_frem_lines 6");
my @arr = split(',',$ind->subject_data->[2]);
cmp_float_array(\@arr,$ans->[1], "add_frem_lines 7");



$ind = data::individual->new(idcolumn => 1, subject_data => ['1,0,1,0,70,1,0', 
															 '1,34,0,10,-99,2,0',
															 '1,21,0,20,70,3,35']);

my $indices = [4,5,6];
$ind->append_bivariate_columns(categorical_indices => [4,5,6],
							   mapping => [[1,70],[3,2],[35,0]],
							   missing_data_token => -99);

$ans = [
	[1,0,1,0,70,1,   0 ,0  ,70 ,0,0,0 ,0],
	[1,34,0,10,-99,2,0 ,-99,-99,0,2,0 ,0],
	[1,21,0,20,70,3 ,35,0  ,70 ,3,0,35,1]
];

is($ind->subject_data->[0], join(',',@{$ans->[0]}), "append_bivariate_columns 0");
is($ind->subject_data->[1], join(',',@{$ans->[1]}), "append_bivariate_columns 1");
is($ind->subject_data->[2], join(',',@{$ans->[2]}), "append_bivariate_columns 2");



done_testing;
