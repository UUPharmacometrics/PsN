#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests => 5;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model::problem::simulation;

my $test_seed = 153;
my $test_seed2 = 1020489783;

my $record = model::problem::simulation->new;
$record->seed1($test_seed);
is (join(' ', @{$record->_format_record}), "\$SIMULATION ($test_seed)\n", "add seed to empty record");

$record = model::problem::simulation->new(record_arr => ["(908907", "NORMAL", "NEW)", 'SUBPROBLEMS=28']);
$record->seed1($test_seed);
is ($record->seed1, $test_seed, "read out seed1");
is (join(' ', @{$record->_format_record}), "\$SIMULATION ($test_seed NORMAL NEW) SUBPROBLEMS=28\n", "change seed1");

$record->seed2($test_seed2);
is ($record->seed2, $test_seed2, "read out seed2");
is (join(' ', @{$record->_format_record}), "\$SIMULATION ($test_seed NORMAL NEW) ($test_seed2) SUBPROBLEMS=28\n", "add seed2");

done_testing();
