#!/usr/bin/perl

use strict;
use warnings;
use Test::More tests=>3;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages


use model::problem::code_record;

# Test new and read option
my $record = model::problem::code_record->new(record_arr => ['$PK', 'TVCL=THETA(1)']);

is ($record->code->[1], 'TVCL=THETA(1)', "Record->code");

# Test _format_record
my $r = $record->_format_record;
is ($$r[0], '$CODE_RECORD ', "record->_format_record");
is ($$r[1], 'TVCL=THETA(1)', "record->_format_record");

done_testing();
