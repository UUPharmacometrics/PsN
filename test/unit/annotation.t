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
$annotation->based_on("2");
is_deeply ($annotation->format(), [ ';; 1. Based on: 2' ], "format with based_on");
$annotation->covariate_model(["Accra", "Cairo"]);
is_deeply ($annotation->format(), [ ';; 1. Based on: 2', ';; 5. Covariate model:', 'Accra', 'Cairo' ], "format with based_on covariate model");

# parse
my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on: 3' ]);
is ($annotation->based_on, '3', "parse based_on");

my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on: 3', ';; 2. Description:', 'Added an OMEGA BLOCK(2)' ]);
is ($annotation->based_on, '3', "parse 2 based_on");
is_deeply ($annotation->description, [ 'Added an OMEGA BLOCK(2)' ], "parse 2 based_on");

my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 2. Description:', 'Helium', ';; 5. Covariate model:', 'Cobolt' ]);
$annotation->covariate_model(['Iron']);
is_deeply ($annotation->description, [ 'Helium' ], 'parse 3 description');
is_deeply ($annotation->covariate_model, [ 'Iron' ], 'parse 3 covariate_model');

my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on: 23', ';; 2. Description:', 'Claus', ';; x1. Author: Santa' ]);
is_deeply ($annotation->description, [ 'Claus'], 'parse 4 description');
is_deeply ($annotation->unknown_tags, [ ';; x1. Author: Santa' ], 'parse 4 unknown_tags');

my $annotation = model::annotation->new();
$annotation->add_empty_tags();
my $a = $annotation->format();
is_deeply ($a, [
          ';; 1. Based on: ',
          ';; 2. Description:',
          ';; 3. Label:',
          ';; 4. Structural model:',
          ';; 5. Covariate model:',
          ';; 6. Inter-individual variability:',
          ';; 7. Inter-occasion variability:',
          ';; 8. Residual variability:',
          ';; 9. Estimation:'
        ], 'add_empty_tags');

done_testing();
