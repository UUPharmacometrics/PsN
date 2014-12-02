#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use model::annotation;


# format
my $annotation = model::annotation->new();
$annotation->based_on("run2.mod");
is_deeply ($annotation->format(), [ ';; 1. Based on: run2.mod' ], "format with based_on");
$annotation->covariate_model(["Accra", "Cairo"]);
is_deeply ($annotation->format(), [ ';; 1. Based on: run2.mod', ';; 5. Covariate model:', 'Accra', 'Cairo' ], "format with based_on covariate model");

# parse
my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on: run3.mod' ]);
is ($annotation->based_on, 'run3.mod', "parse based_on");

my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on: run3.mod', ';; 2. Description:', 'Added an OMEGA BLOCK(2)' ]);
is ($annotation->based_on, 'run3.mod', "parse 2 based_on");
is_deeply ($annotation->description, [ 'Added an OMEGA BLOCK(2)' ], "parse 2 based_on");

my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 2. Description:', 'Helium', ';; 5. Covariate model:', 'Cobolt' ]);
$annotation->covariate_model(['Iron']);
is_deeply ($annotation->description, [ 'Helium' ], 'parse 3 description');
is_deeply ($annotation->covariate_model, [ 'Iron' ], 'parse 3 covariate_model');

done_testing();
