#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests=>6;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages

use model::problem::record;

# Test new and read option
my $record = model::problem::record->new(record_arr => ['TEST', 'ANY=SOUP']);

is ($record->options->[1]->name, 'ANY', "Option->name");
is ($record->options->[1]->value, 'SOUP', "Option->name");

# Test _format_record
my $r = $record->_format_record;
my @str = split /\s+/, $$r[0];
is ($str[0], '$RECORD', "record->_format_record");
is ($str[1], 'TEST', "record->_format_record");
is ($str[2], 'ANY=SOUP', "record->_format_record");

# Test remove_option
$record->remove_option(name => 'TEST');
is ($record->options->[0]->name, 'ANY', "remove_option");

done_testing();
