#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use Data::Dumper;

use model;
use model::problem::code_record;
use model::problem::mix;

# Test new and read option
my $record = model::problem::code_record->new(record_arr => ['$PK', 'TVCL=THETA(1)']);
is ($record->code->[0], 'TVCL=THETA(1)', "Record->code");

# Test _format_record ($CODE_RECORD results from class name)
my $r = $record->_format_record;
is ($$r[0], '$CODE_RECORD', "record->_format_record simple");
is ($$r[1], 'TVCL=THETA(1)', "record->_format_record simple");

# Test _format_record pseudo-assignment order
$record = model::problem::code_record->new(record_arr => ['$PK', '(ONLY OBSERVATIONS)', '; just code']);
unshift(@{$record->code}, "; inserted first");
$r = $record->_format_record;
# (ONLY OBSERVATIONS) should remain first (and join with $CODE_RECORD)
is ($$r[0], '$CODE_RECORD (ONLY OBSERVATIONS)', "record->_format_record pseudo-assignment");

# Test _format_record pseudo-assignment order with "FIRST code
$record = model::problem::code_record->new(record_arr => ['$PK', '(ONLY OBSERVATIONS)', '; pre-verbatim comment', '"FIRST', '"ANY=SOUP']);
unshift(@{$record->code}, "; code inserted first");
$r = $record->_format_record;
# (ONLY OBSERVATIONS) should still be first after code prefix insertion, and inserted comment after (including pre-verbatim and verbatim)
is_deeply ($r, ['$CODE_RECORD (ONLY OBSERVATIONS)',
                '; pre-verbatim comment',
                '"FIRST',
                '"ANY=SOUP',
                '; code inserted first'], "record->_format_record pseudo-assignment with \"FIRST");

# Test _format_record pseudo-assignment order with CALLFL
$record = model::problem::code_record->new(record_arr => ['$PK', '"ANY=SOUP', 'CALLFL=1', '; comment']);
$r = $record->_format_record;
# CALLFL=1 expected to move first and NOT join $CODE_RECORD (not parenthesized pseudo-assignment)
is_deeply ($r, ['$CODE_RECORD',
                'CALLFL=1',
                '"ANY=SOUP',
                '; comment'], "record->_format_record pseudo-assignment with CALLFL move");
$record = model::problem::code_record->new(record_arr => ['$PK', '"ANY=SOUP', '; comment', 'CALLFL=1']);
$r = $record->_format_record;
# CALLFL=1 expected to NOT move (user might not expect move after non-verbatim code)
is_deeply ($r, ['$CODE_RECORD',
                '"ANY=SOUP',
                '; comment',
                'CALLFL=1'], "record->_format_record pseudo-assignment with static CALLFL");

my $modeldir = $includes::testfiledir;

my $model = model->new(filename => "$modeldir/mixture.mod", ignore_missing_data => 1);
is($model->problems->[0]->mixs->[0]->nspop,2,'mixure model nspop');

done_testing();
