#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Copy qw(cp);
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

use nmtablefile;
use tool::resmod;
use model;

our $test_files = $includes::testfiledir;
our $tempdir = create_test_dir("unit_resmod");

my $model = model->new(
    filename => "$test_files/resmod/pheno.mod",
    ignore_missing_files => 1,
    skip_data_parsing => 1,
    ignore_missing_data => 1,
);

my $table = $model->problems->[0]->tables->[1];

my $input;
$input = tool::resmod::_create_input(table => $table, columns => [ 'ID', 'TIME', 'CWRES' ]); 
is ($input, "ID TIME DROP DROP DROP DROP DROP DROP DROP DROP DV", "resmod _create_input 1");
$input = tool::resmod::_create_input(table => $table, columns => [ 'ID', 'TIME', 'CWRES', 'IPRED' ]);
is ($input, "ID TIME DROP DROP DROP IPRED DROP DROP DROP DROP DV", "resmod _create_input ipred included");
$input = tool::resmod::_create_input(table => $table, columns => [ 'ID', 'TIME', 'CWRES', 'IPRED' ], ipred => 0);
is ($input, "ID TIME DROP DROP DROP DROP DROP DROP DROP DROP DV", "resmod _create_input ipred excluded");


$table = nmtablefile->new(filename => "$test_files/resmod/sdtab"); 
$table = $table->tables->[0];
my @quartiles = tool::resmod::_calculate_quartiles(table => $table, column => 'TIME');

is_deeply (\@quartiles, [ 6, 63.5, 131.8 ], "_calculate_quartiles 1");


remove_test_dir($tempdir);

done_testing();
