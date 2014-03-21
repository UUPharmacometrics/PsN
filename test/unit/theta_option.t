#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests=>15;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model::problem::record::theta_option;

# Test new and read option
my $option1 = model::problem::record::theta_option->new(option_string => '(0,0.0105,2)');
my $option2 = model::problem::record::theta_option->new(option_string => '(0,1,INF)');
my $option3 = model::problem::record::theta_option->new(option_string => '(1 FIX)');
my $option4 = model::problem::record::theta_option->new(option_string => '(1, ,4)');
my $option5 = model::problem::record::theta_option->new(option_string => '28');

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

# Test format option
is ($option1->_format_option, '(0,0.0105,2)', "Option1->_format_option");
is ($option2->_format_option, '(0,1,1000000)', "Option2->_format_option");
is ($option3->_format_option, '1 FIX', "Option3->_format_option");
is ($option5->_format_option, 28, "Option4->_format_option");

done_testing();
