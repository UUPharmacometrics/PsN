#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests=>8;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages
use debug;

use model::problem::record::option;


# Test new and read option
my $option1 = model::problem::record::option->new(option_string => 'TRT=DROP');
my $option2 = model::problem::record::option->new(option_string => 'ID');

is ($option1->name, 'TRT', "Option1->name");
is ($option1->value, 'DROP', "Option1->value");
is ($option1->option_string, undef, "Option1->option_string");
is ($option2->name, 'ID', "Option2->name");
is ($option2->value, '', "Option2->value");
is ($option2->option_string, undef, "Option2->option_string");

# Test format option
is ($option1->_format_option, 'TRT=DROP', "Option1->_format_option");
is ($option2->_format_option, 'ID', "Option2->_format_option");

done_testing();
