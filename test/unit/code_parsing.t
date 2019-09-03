#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model;
use code_parsing;

is (code_parsing::check_additive_eta(expression => 'THETA(1) + ETA(1)'), 1, "check_additive_eta 1");
is (code_parsing::check_additive_eta(expression => 'ETA(2) + THETA(3)'), 2, "check_additive_eta 2");
is (code_parsing::check_additive_eta(expression => 'ETA(5) + THETA(1)*15+CL'), 5, "check_additive_eta 3");
is (code_parsing::check_additive_eta(expression => '23 * THETA(1)+CL + ETA(10)'), 10, "check_additive_eta 4");


is_deeply(code_parsing::find_assignments(['CL = THETA(1)*EXP(ETA(1))']), [ { 'CL' => [ 'THETA(1)*EXP(ETA(1))', 0 ] } ], "code_parsing simple expression");
is_deeply(code_parsing::find_assignments(['    V=TVV+ETA(2)']), [ { 'V' => [ 'TVV+ETA(2)', 0 ] } ], "code_parsing simple expression 2");
is_deeply(code_parsing::find_assignments(['IF (X.EQ.23) CL=THETA(1)']), [ { 'CL' => [ 'THETA(1)', 1 ] } ], "code_parsing IF expression");
is_deeply(code_parsing::find_assignments(['   IF    (   X.EQ.28   )    CL = THETA(1)']), [ { 'CL' => [ 'THETA(1)', 1 ] } ], "code_parsing IF expression 2");
is_deeply(code_parsing::find_assignments(['TVCL=THETA(1)', 'CL=TVCL*EXP(ETA(1)) ;comment']), [ { 'TVCL' => [ 'THETA(1)', 0 ] }, { 'CL' => [ 'TVCL*EXP(ETA(1)) ', 0]  } ], "code_parsing two lines");


done_testing();
