#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use random;
use model::problem::theta;

# Test new and read option
my $record = model::problem::theta->new(record_arr => ['(28) FIXED (1,,10)']);

is ($record->options->[0]->lobnd, undef, "option->lobnd");
is ($record->options->[0]->init, 28, "option->init");
is ($record->options->[0]->fix, 1, " undef init fix");
is ($record->options->[1]->fix, 0, " empty init fix");
is ($record->options->[1]->lobnd, 1, "empty init lobnd");
is ($record->options->[1]->init, '', "empty init");
is ($record->options->[1]->upbnd, 10, "empty init upbnd");

$record = model::problem::theta->new(record_arr => ['(28 FIXED); hej', ' ( .2 4 ) (.1 +2E+00 10 ) ; (-4,) ;hej',
													   '$THETA (0,3) 2 FIXED (0,.6,1) 10 (-INF,-2.7,0) (37 FIXED)']);

is ($record->options->[0]->init, 28, "0 init space");
is ($record->options->[0]->fix, 1, "0 fixed space ");
is ($record->options->[0]->label, 'hej', "0 option->label");
is (eval($record->options->[1]->lobnd), eval(.2), "1 option->lobnd");
is ($record->options->[1]->init, 4, "1 option->init");
is ($record->options->[1]->upbnd, undef, "1 option->upbnd");
is (eval($record->options->[2]->lobnd), eval(.1), "2 option->lobnd");
is (eval($record->options->[2]->init), eval(2), "2 option->init");
is ($record->options->[2]->upbnd, 10, "2 option->upbnd");

is ($record->options->[3]->lobnd, 0, "3 option->lobnd");
is ($record->options->[3]->init, 3, "3 option->init");
is ($record->options->[3]->fix, 0, "3 fixed  ");
is ($record->options->[4]->init, 2, "4 option->init");
is ($record->options->[4]->fix, 1, "4 fixed  ");
is ($record->options->[5]->lobnd, 0, "5 option->lobnd");
is (eval($record->options->[5]->init), eval(0.6), "5 option->init");
is ($record->options->[5]->upbnd, 1, "5 option->upbnd");
is ($record->options->[5]->fix, 0, "5 fixed ");
is ($record->options->[6]->init, 10, "6 option->init");
is ($record->options->[6]->fix, 0, "6 fixed ");

is ($record->options->[7]->lobnd, $PsN::config -> {'low_INF'}, "7 option->lobnd");
is ($record->options->[7]->init, -2.7, "7 option->init");
is ($record->options->[7]->upbnd, 0, "7 option->upbnd");
is ($record->options->[7]->fix, 0, "7 fixed ");

is ($record->options->[8]->lobnd, undef, "8 option->lobnd");
is ($record->options->[8]->init, 37, "8 option->init");
is ($record->options->[8]->fix, 1, "8 fixed ");


$record = model::problem::theta->new(record_arr => ['28', '(1,2,10)']);

is ($record->options->[1]->lobnd, 1, "option->lobnd");
is ($record->options->[1]->init, 2, "option->init");
is ($record->options->[1]->upbnd, 10, "option->upbnd");
# Test _format_record
my $r = $record->_format_record;
my @str = split /\s+/, $$r[0];
is ($str[0], '$THETA', "record->_format_record");
is ($str[1], '28', "record->_format_record");
is ($str[2], '(1,2,10)', "record->_format_record");

$record = model::problem::theta->new(record_arr => ['(-INF .43 INF), (-INF 2 INF FIXED)']);
is_deeply($record->get_estimated_coordinate_strings,
		  ['THETA1'],
		  'estimated coordinate strings 4');

is ($record->options->[0]->lobnd, $PsN::config -> {'low_INF'}, "space option->lobnd");
is (eval($record->options->[0]->init), eval(.43), "option->init");
is ($record->options->[0]->upbnd, $PsN::config -> {'high_INF'}, "option->upbnd");
is ($record->options->[0]->fix, 0, " fixed ");
is ($record->options->[1]->lobnd, $PsN::config -> {'low_INF'}, "option->lobnd");
is ($record->options->[1]->init, 2, "option->init");
is ($record->options->[1]->upbnd, $PsN::config -> {'high_INF'}, "option->upbnd");
is ($record->options->[1]->fix, 1, " fixed ");



$record = model::problem::theta->new(record_arr => ['10 FIX (1,2,10),(100)']);
is_deeply($record->get_estimated_coordinate_strings,
		  ['THETA2','THETA3'],
		  'estimated coordinate strings 5');


is ($record->options->[0]->lobnd, undef, "option->lobnd");
is ($record->options->[0]->init, 10, "option->init");
is ($record->options->[0]->upbnd, undef, "option->upbnd");
is ($record->options->[2]->lobnd, undef, "option->lobnd");
is ($record->options->[2]->init, 100, "option->init");
is ($record->options->[2]->upbnd, undef, "option->upbnd");

$PsN::nm_major_version = 6; #affects formatting in init_option.pm 

random_set_seed_from_phrase('12345');
$record->set_random_inits(degree => 0.1);
is ($record->options->[0]->init, 10, "option->init");
cmp_float($record->options->[1]->init, 2.152759, "option->init");
cmp_float($record->options->[2]->init, 90.10181, "option->init");
$record->set_random_inits(degree => 2);
cmp_float($record->options->[1]->init, 3.285657, "option->init");
cmp_float($record->options->[2]->init, 21.45399, "option->init");

done_testing();
