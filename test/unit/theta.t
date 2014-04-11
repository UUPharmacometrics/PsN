#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests=>17;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::Random;
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


$record = model::problem::theta->new(record_arr => ['10 FIX', '(1,2,10)','100']);
is ($record->options->[0]->lobnd, undef, "option->lobnd");
is ($record->options->[0]->init, 10, "option->init");
is ($record->options->[0]->upbnd, undef, "option->upbnd");
is ($record->options->[2]->lobnd, undef, "option->lobnd");
is ($record->options->[2]->init, 100, "option->init");
is ($record->options->[2]->upbnd, undef, "option->upbnd");

random_set_seed_from_phrase('12345');
$record->set_random_inits(degree => 0.1);
is ($record->options->[0]->init, 10, "option->init");
cmp_float($record->options->[1]->init, 2.098214, "option->init");
cmp_float($record->options->[2]->init, 107.6874, "option->init");
$record->set_random_inits(degree => 0.1);
cmp_float($record->options->[1]->init, 1.929032, "option->init");
cmp_float($record->options->[2]->init, 99.38351, "option->init");

done_testing();
