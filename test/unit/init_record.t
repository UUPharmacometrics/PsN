#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../../lib"; 	# PsN packages

use model::problem::init_record;

# Test new and read option
my $record = model::problem::init_record->new(record_arr => ['2']);
my $r = $record->_format_record;
my @str = split /\s+/, $$r[0];
is ($str[0], '$INIT_RECORD', "record->_format_record");
is ($str[1], '2', "record->_format_record");

done_testing();
