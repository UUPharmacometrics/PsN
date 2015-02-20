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
$annotation->set_tag(name => 'Based on', content => [ "2" ]);
is_deeply ($annotation->format(), [ ';; 1. Based on: 2' ], "format with based_on");
$annotation->set_tag(name => 'Covariate model', content => [ "Accra", "Cairo" ]);
is_deeply ($annotation->format(), [ ';; 1. Based on: 2', ';; 5. Covariate model:', ';; Accra', ';; Cairo' ], "format with based_on covariate model");

my $annotation = model::annotation->new();
$annotation->set_based_on("4");
$annotation->set_nodOFV(1);
is_deeply ($annotation->format(), [ ';; 1. Based on: 4 [nodOFV]' ], "format nodOFV");

# parse
my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on: 3' ]);
is ($annotation->get_based_on, '3', "parse based_on");
ok (!$annotation->get_nodOFV, "nodOFV not present");

my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on: 3', ';; 2. Description:', ';; Added an OMEGA BLOCK(2)' ]);
is ($annotation->get_based_on, '3', "parse 2 based_on");
is_deeply ($annotation->get_tag_content(name => 'Description'), [ '', ' Added an OMEGA BLOCK(2)' ], "parse 2 based_on");

my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 2. Description:', ';;Helium', ';; 5. Covariate model:', ';;Cobolt' ]);
$annotation->set_tag(name => 'Covariate model', content => [ '', 'Iron']);
is_deeply ($annotation->get_tag_content(name => 'Description'), [ '', 'Helium' ], 'parse 3 description');
is_deeply ($annotation->get_tag_content(name => 'Covariate model'), [ '', 'Iron' ], 'parse 3 covariate_model');

my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on: 23', ';; 2. Description:', ';;Claus', ';; x1. Author: Santa' ]);
is_deeply ($annotation->get_tag_content(name => 'Description'), [ '', 'Claus'], 'parse 4 description');
is_deeply ($annotation->get_tag_content(name => 'Author'), [ ' Santa' ], 'parse 4 unknown_tags');

my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on: 2 [nodOFV]' ]);
ok ($annotation->get_nodOFV, "nodOFV present");
my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';; 1. Based on:  [nodOFV]   2' ]);
ok ($annotation->get_nodOFV, "nodOFV present before runno");

# parse Census style parent
my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';;;C parent  = 28' ]); 
ok (!$annotation->get_nodOFV, "Census style no nodOFV");
is ($annotation->get_based_on, '28', "Census style based_on");
my $annotation = model::annotation->new();
$annotation->parse(annotation_rows => [ ';;;C parent  =' ]); 
ok ($annotation->get_nodOFV, "Census style nodOFV");

# add_empty_tags
my $annotation = model::annotation->new();
$annotation->add_empty_tags();
my $a = $annotation->format();
is_deeply ($a, [
          ';; 1. Based on:',
          ';; 2. Description:',
          ';; 3. Label:',
          ';; 4. Structural model:',
          ';; 5. Covariate model:',
          ';; 6. Inter-individual variability:',
          ';; 7. Inter-occasion variability:',
          ';; 8. Residual variability:',
          ';; 9. Estimation:'
        ], 'add_empty_tags');

#parse_model
my $annotation = model::annotation->new();
my @model = ( '$PROBLEM tomte', ';; 1. Based on: 23', ';; Extra', '$INPUT FDRT' );
$annotation->parse_model(model_lines => \@model);
is ($annotation->get_based_on, 23, "parse_model based_on");
is_deeply (\@model, [ '$PROBLEM tomte', '$INPUT FDRT' ], "parse_model remaining model lines");

done_testing();
