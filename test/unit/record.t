#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
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


# Test add_option
my $record = model::problem::record->new(record_arr => [ 'TEST' ]);
$record->add_option(init_data => { name => 'LEGU', value => 'CNCR' });
is ($record->options->[1]->name, 'LEGU', "add_options name");
is ($record->options->[1]->value, 'CNCR', "add_options value");

my $record = model::problem::record->new(record_arr => [ ]);
$record->add_option(init_data => { name => 'LEGU', value => 'CNCR' });
is ($record->options->[0]->name, 'LEGU', "add_options empty name");
is ($record->options->[0]->value, 'CNCR', "add_options empty value");

# Test _add_option
my $record = model::problem::record->new();
$record->_add_option(option_string => "CITY=SALA");
is ($record->options->[0]->name, 'CITY', "_add_options empty name");
is ($record->options->[0]->value, 'SALA', "_add_options empty value");

done_testing();
