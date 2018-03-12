#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use code_parsing;

is (code_parsing::check_additive_eta(expression => 'THETA(1) + ETA(1)'), 1, "check_additive_eta 1");
is (code_parsing::check_additive_eta(expression => 'ETA(2) + THETA(3)'), 2, "check_additive_eta 2");
is (code_parsing::check_additive_eta(expression => 'ETA(5) + THETA(1)*15+CL'), 5, "check_additive_eta 3");
is (code_parsing::check_additive_eta(expression => '23 * THETA(1)+CL + ETA(10)'), 10, "check_additive_eta 4");

done_testing();
