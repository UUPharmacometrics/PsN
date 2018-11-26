#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model;
use code;

# append_verbatim_code

my $modeldir = $includes::testfiledir;
my $model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);

code::append_verbatim_code(model => $model, code_record => 'pk', pos => 'FIRST', code => [ 'X = X + 1' ]);
my $record = $model->problems->[0]->pks->[0];
my $code = $record->_format_record();
is ($code->[0], '$PK "FIRST', "append_verbatim_code line 1");
is ($code->[1], '"X = X + 1', "append_verbatim_code line 2");

# generate_sum
is_deeply (code::generate_sum(name => "ESUM1", terms => [ "ETA1", "ETA2", "ETA3" ]), "ESUM1 = ETA1 + ETA2 + ETA3", "generate_sum 1");
is_deeply (code::generate_sum(name => "T", terms => [ "X" ]), "T = X", "generate_sum 2");


done_testing();
