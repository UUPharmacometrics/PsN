#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Config;

use rplots;

my $string = 'Z:\Desktop\diagnostics\course_material\Course_Material\hands_on\HO_Simulation_evaluation_diagnostics\Files_provided\task1\vpc_dv_vs_pred\vpctab1';

# It seems as slashes are getting collapsed by the is() function. Therefore need quadruple backslashes here to get two in the results comparison.
my $ans= 'Z:\\\\Desktop\\\\diagnostics\\\\course_material\\\\Course_Material\\\\hands_on\\\\HO_Simulation_evaluation_diagnostics\\\\Files_provided\\\\task1\\\\vpc_dv_vs_pred\\\\vpctab1';

is(rplots::double_backslashes(string=> $string),$ans,'single to double backslash windows');

is(rplots::double_backslashes(string=> $ans),$ans,'double backslash not changed windows');

$string ='/home/kajsa/kod-psn/devel/simeval_dir1/vpc_dv_vs_pred/vpctab';
is(rplots::double_backslashes(string=>$string),$string,'forward slash not changes');

done_testing();
