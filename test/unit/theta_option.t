#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Test::Exception;

use model::problem::record::theta_option;

dies_ok {model::problem::record::theta_option->new(option_string => ' (0,82.11705' )} "theta unmatched parentheses 1";
dies_ok {model::problem::record::theta_option->new(option_string => '); CL' )} "theta unmatched parentheses 2";

# Test new and read option
my $option1 = model::problem::record::theta_option->new(option_string => '(0,0.0105,2)');
my $option2 = model::problem::record::theta_option->new(option_string => '(0,1,INF)');
my $option3 = model::problem::record::theta_option->new(option_string => '(1 FIX)');
my $option4 = model::problem::record::theta_option->new(option_string => '(1, ,4)');
my $option5 = model::problem::record::theta_option->new(option_string => '28');
my $option6 = model::problem::record::theta_option->new(option_string => '0,0.0105');
my $option7 = model::problem::record::theta_option->new(option_string => '0,0.0105,0.011');
my $option8 = model::problem::record::theta_option->new(option_string => '-1,-0.9,1');

is ($option1->name, undef, "Option1->name");
is ($option1->value, undef, "Option1->value");
is ($option1->option_string, undef, "Option1->option_string");
is ($option2->fix, 0, "Option2->fix");
is ($option3->fix, 1, "Option3->fix");
is ($option4->lobnd, 1, "Option4->lobnd");
is ($option4->upbnd, 4, "Option4->upbnd");
is ($option4->init, '', "Option4->init");
is ($option5->lobnd, undef, "Option5->lobnd");
is ($option5->upbnd, undef, "Option5->upbnd");
is ($option5->init, 28, "Option5->init");
is ($option6->lobnd, 0, "Option6->lobnd");
is ($option6->upbnd, undef, "Option6->upbnd");
is ($option6->init, 0.0105, "Option6->init");
is ($option7->lobnd, 0, "Option7->lobnd");
is ($option7->upbnd, 0.011, "Option7->upbnd");
is ($option7->init, 0.0105, "Option7->init");
is ($option8->lobnd, -1, "Option8->lobnd");
is ($option8->upbnd, 1, "Option8->upbnd");
is ($option8->init, -0.9, "Option8->init");

# Test format option
is ($option1->_format_option, '(0,0.0105,2)', "Option1->_format_option");
is ($option2->_format_option, '(0,1,1000000)', "Option2->_format_option");
is ($option3->_format_option, '1 FIX', "Option3->_format_option");
is ($option5->_format_option, 28, "Option5->_format_option");

#test get_range
my $range = $option1->get_range(degree=> 0.5);
cmp_float($range->[0],(0.0105-0.5*0.0105), "Option1->range lower");
cmp_float($range->[1],(0.0105+0.5*0.0105), "Option1->range upper");

$range = $option7->get_range(degree=> 0.9);
cmp_float($range->[0],(0.0105-0.9*0.0105), "Option7->range lower");
cmp_float($range->[1],(0.011- (1e-10)), "Option7->range upper");

$range = $option8->get_range(degree=> 0.5);
cmp_float($range->[0],(-1+ (1e-10)), "Option8->range lower");
cmp_float($range->[1],(-0.9 + 0.5*0.9), "Option8->range upper");

my $range = $option1->get_range(degree => 2);
cmp_float($range->[0], 1e-10, "get_range degree=2 option1 lower");
cmp_float($range->[1], (0.0105 + 2 * 0.0105), "get_range degree=2 option1 upper");

done_testing();
