#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests=>8;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model::problem::record::theta_option;

# Test new and read option
my $option1 = model::problem::record::init_option->new(option_string => '0.01',
													   on_diagonal => 1);

my $option2 = model::problem::record::init_option->new(option_string => '0.009',
													   on_diagonal => 0);

my $option3 = model::problem::record::init_option->new(option_string => '1000000',
													   on_diagonal => 1);

my $option4 = model::problem::record::init_option->new(option_string => '-0.009',
													   on_diagonal => 0);

#test get_range
my $range = $option1->get_range(degree=> 0.5);
cmp_float($range->[0],(0.01-0.5*0.01), "Option1->range lower");
cmp_float($range->[1],(0.01+0.5*0.01), "Option1->range upper");

$range = $option2->get_range(degree=> 0.9);
cmp_float($range->[0],(0.009-0.9*0.009), "Option2->range lower");
cmp_float($range->[1],0.01, "Option2->range upper");

$range = $option3->get_range(degree=> 0.5);
cmp_float($range->[0],(1000000-0.5*1000000), "Option3->range lower");
cmp_float($range->[1],(1000000  - 1), "Option3->range upper");

$range = $option4->get_range(degree=> 0.9);
cmp_float($range->[0],-0.01, "Option4->range lower");
cmp_float($range->[1],(-0.009+0.9*0.009), "Option4->range upper");


done_testing();
