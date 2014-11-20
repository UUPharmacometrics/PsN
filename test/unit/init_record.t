#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::Random;
use model::problem::init_record;

# Test new and read_option
my $record = model::problem::init_record->new(record_arr => ['2']);
my $r = $record->_format_record;
my @str = split /\s+/, $$r[0];
is ($str[0], '$INIT_RECORD', "record->_format_record");
is ($str[1], '2', "record->_format_record");

random_set_seed_from_phrase('12345');

$record = model::problem::init_record->new(record_arr => ['BLOCK(2) 0.02','0 0.01 FIX']);
is ($record->options->[0]->init,0.02,'record 1 init 0');
is ($record->options->[1]->init,0,'record 1 init 1');
is ($record->options->[2]->init,0.01,'record 1 init 2');
is ($record->fix,1,'record 1 fix');
is ($record->type,'BLOCK','record 1 type');
is ($record->size,2,'record 1 size');
$record->set_random_inits(degree => 0.1);
is($record->options->[0]->init,0.02,'record 1 init 0');
is($record->options->[1]->init,0,'record 1 init 1');
is($record->options->[2]->init,0.01,'record 1 init 2');

$record = model::problem::init_record->new(record_arr => ['DIAGONAL(2) 0.02','0.01']);
is ($record->options->[0]->init,0.02,'record 2 init 0');
is ($record->options->[1]->init,0.01,'record 2 init 1');
is ($record->fix,0,'record 2 fix');
is ($record->type,'DIAGONAL','record 2 type');
is ($record->size,2,'record 2 size');
$record->set_random_inits(degree => 0.1);
cmp_float($record->options->[0]->init,0.020982,'record 2 init 0');
cmp_float ($record->options->[1]->init,0.010769,'record 2 init 1');

$record = model::problem::init_record->new(record_arr => ['BLOCK(3) 0.02','-0.002 0.5','0.003 -0.005 1']);
is ($record->options->[0]->init,0.02,'record 3 init 0');
is ($record->options->[1]->init,-0.002,'record 3 init 1');
is ($record->options->[2]->init,0.5,'record 3 init 2');
is ($record->options->[3]->init,0.003,'record 3 init 3');
is ($record->options->[4]->init,-0.005,'record 3 init 4');
is ($record->options->[5]->init,1,'record 3 init 5');
is ($record->fix,0,'record 3 fix');
is ($record->type,'BLOCK','record 3 type');
is ($record->size,3,'record 3 size');
$record->set_random_inits(degree => 0.1);
cmp_float ($record->options->[0]->init,0.018387,'record 3 init 0');
cmp_float ($record->options->[1]->init,-0.00215,'record 3 init 1');
cmp_float ($record->options->[2]->init,0.531022,'record 3 init 2');
cmp_float ($record->options->[3]->init,0.002922,'record 3 init 3');
cmp_float ($record->options->[4]->init,-0.00476,'record 3 init 4');
cmp_float ($record->options->[5]->init,1.048290,'record 3 init 5');

$record = model::problem::init_record->new(record_arr => ['BLOCK(1) 28 FIXED']);
is ($record->options->[0]->init, 28, 'record 4 init');
is ($record->fix, 1, 'record 4 fix');
my $a = $record->_format_record;
my @str = split /\s+/, $$a[0];
is ($str[2], 'FIX', "_format_record 4");
is ($str[3], 28, "_format record 4");


my $record = model::problem::init_record->new(record_arr => ['2 ; Malta', '; Corse']);
is ($record->comment->[0], "; Corse\n", "init_record full line comment");
my $r = $record->_format_record;
is ($r->[1], "; Corse\n", "init_record full line comment after _format_record");

done_testing();
