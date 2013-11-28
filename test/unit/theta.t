#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages

use model::problem::theta;

# Test new and read option
my $record = model::problem::theta->new(record_arr => ['28', '(1,2,10)']);

is ($record->options->[1]->lobnd, 1, "option->lobnd");
is ($record->options->[1]->init, 2, "option->init");
is ($record->options->[1]->upbnd, 10, "option->upbnd");

# Test _format_record
my $r = $record->_format_record;
my @str = split /\s+/, $$r[0];
is ($str[0], '$THETA', "record->_format_record");
is ($str[1], '28', "record->_format_record");
is ($str[2], '(1,2,10)', "record->_format_record");

done_testing();
