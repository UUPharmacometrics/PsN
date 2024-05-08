#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

use nmtablefile;
use tool::resmod;
use model;

# Mock mkpath to avoid directory creation
no warnings qw(redefine);
*tool::mkpath = sub { 1 };

our $test_files = $includes::testfiledir;
our $tempdir = create_test_dir("unit_resmod");

my $model = model->new(
    filename => "$test_files/resmod/pheno.mod",
    ignore_missing_files => 1,
    skip_data_parsing => 1,
    ignore_missing_data => 1,
);

#my $table = $model->problems->[0]->tables->[1];

my $cwres_table_name = $model->problems->[0]->find_table(columns => ['ID', 'TIME', 'CWRES']);
my $table = nmtablefile->new(filename => $test_files . '/' . $cwres_table_name); 
$table = $table->tables->[0];

delete $tool::resmod::{BUILD};
my $resmod = tool::resmod->new(models => [$model]);

my $input;
$input = $resmod->_create_input(table => $table, columns => [ 'ID', 'TIME', 'CWRES' ]); 
is ($input, "ID TIME DROP DROP DROP ", "resmod _create_input 1");
$input = $resmod->_create_input(table => $table, columns => [ 'ID', 'TIME', 'CWRES', 'IPRED' ]);
is ($input, "ID TIME DROP DROP IPRED ", "resmod _create_input ipred included");
$input = $resmod->_create_input(table => $table, columns => [ 'ID', 'TIME', 'CWRES', 'IPRED' ], ipred => 0);
is ($input, "ID TIME DROP DROP DROP ", "resmod _create_input ipred excluded");


remove_test_dir($tempdir);

done_testing();
