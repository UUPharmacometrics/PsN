#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use code;

is_deeply (code::generate_sum(name => "ESUM1", terms => [ "ETA1", "ETA2", "ETA3" ]), "ESUM1 = ETA1 + ETA2 + ETA3", "generate_sum 1");
is_deeply (code::generate_sum(name => "T", terms => [ "X" ]), "T = X", "generate_sum 2");


done_testing();
