#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model::problem::input;

# Test new and read option
my $record = model::problem::input->new(record_arr => ['$INPUT ID DV AMT MDV,WT','CLCR SEX,BMI']);

my @ans = qw(ID DV AMT MDV WT CLCR SEX BMI);
for (my $i=0; $i<scalar(@ans); $i++){
	is($record->options->[$i]->name,$ans[$i],'input record options mix comma and space');
}

my ($arr,$bool) = $record->get_filter_table_names; 
is_deeply($arr,\@ans,"input get_filter_table_names 1");
is($bool,0,"input get_filter_table_names 1 b");

$record = model::problem::input->new(record_arr => ['$INPUT DROP ID DV AMT MDV=DROP WT','SKIP=CLCR SEX,BMI SKIP HEJ=SKIP OJ DROP=FEM']);
my @ans2 = qw(ID ID DV AMT ID WT ID SEX BMI ID ID OJ ID); 
($arr,$bool) = $record->get_filter_table_names; 

is_deeply($arr,\@ans2,"input get_filter_table_names 2");
is($bool,0,"input get_filter_table_names 2 b");

$record = model::problem::input->new(record_arr => ['$INPUT DROP DROP DROP']);
($arr,$bool) = $record->get_filter_table_names; 
is($arr,undef,"input get_filter_table_names 3");
is($bool,0,"input get_filter_table_names 3 b");

$record = model::problem::input->new(record_arr => ['$INPUT DROP ID DV AMT DATE=DROP WT']);
@ans = qw(ID ID DV AMT ID WT TIME); 
($arr,$bool) = $record->get_filter_table_names; 

is_deeply($arr,\@ans,"input get_filter_table_names 4");
is($bool,1,"input get_filter_table_names 4 b");

done_testing();
