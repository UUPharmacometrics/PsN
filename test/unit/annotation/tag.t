#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use model::annotation::tag;

# Parse format and change Based on
my $tag = model::annotation::tag->new();
$tag->parse(rows => [ ';; 1. Based on: 001' ]);
is ($tag->number, '1', "Based on tag number");
is ($tag->name, 'Based on', "Based on tag name");
ok ($tag->known, "Based on tag known");
is_deeply ($tag->content, [ " 001" ], "Based on content");
is_deeply ($tag->get_content_nospace(), [ "001" ], "Based on content row");
is_deeply ($tag->format, [ ";; 1. Based on: 001" ], "Based on format");

$tag->nodOFV(1);
is_deeply ($tag->format, [ ";; 1. Based on: 001 [nodOFV]" ], "Based on nodOFV");

# Census style Based on
my $tag = model::annotation::tag->new();
$tag->parse(rows => [ ';;;C Parent= 001' ]);
is ($tag->number, '1', "Census Based on tag number");
is ($tag->name, 'Based on', "Census Based on tag name");
ok ($tag->known, "Census Based on tag known");
is_deeply ($tag->content, [ " 001" ], "Census Based on content");
is_deeply ($tag->format, [ ";;;C parent = 001" ], "Census Based on format");

$tag->census_style(0);
is_deeply ($tag->format, [ ";; 1. Based on: 001" ], "Census Based on reformat");

# Census style nodOFV
my $tag = model::annotation::tag->new();
$tag->parse(rows => [ ';;;C parent =' ]);
is ($tag->number, '1', "Census Based nodOFV on tag number");
is ($tag->name, 'Based on', "Census Based nodOFV on tag name");
ok ($tag->known, "Census nodOFV on tag known");
ok ($tag->nodOFV, "Cenus style nodOFV");
is_deeply ($tag->content, [ "" ], "Census Based on nodOFV content");
is_deeply ($tag->format, [ ";;;C parent = 0" ], "Census Based on nodOFV format");


# Parse and format Description
my $tag = model::annotation::tag->new();
$tag->parse(rows => [ ';; 2. Description: This is a model' ]);
is ($tag->number, '2', "Description tag number");
is ($tag->name, 'Description', "Description tag name");
ok ($tag->known, "Description tag known");
is_deeply ($tag->content, [ " This is a model" ], "Description content");
is_deeply ($tag->format, [ ";; 2. Description: This is a model" ], "Description format");

# Parse format and change Description
my $tag = model::annotation::tag->new();
$tag->parse(rows => [ ';; 3. Label: My row 1', ';;  My row 2' ]);
is ($tag->number, '3', "Label tag number");
is ($tag->name, 'Label', "Label tag name");
ok ($tag->known, "Label tag known");
is_deeply ($tag->content, [ " My row 1", "  My row 2" ], "Label content");
is_deeply ($tag->format, [ ";; 3. Label: My row 1", ";;  My row 2" ], "Label format");

$tag->content([ "New content" ]);
is_deeply ($tag->format, [ ";; 3. Label: New content" ], "Label new content");
$tag->content([ "", "New row" ]);
is_deeply ($tag->format, [ ";; 3. Label:", ";; New row" ], "Label new row");

#set_tag
my $tag = model::annotation::tag->new();
$tag->set_tag(name => 'Based on', content => [ '28' ]);
is ($tag->number, '1', "set_tag Based on number");
is ($tag->name, 'Based on', "set_tag Based on tag name");
ok ($tag->known, "set_tag Based on tag known");
is_deeply ($tag->content, [ "28" ], "set_tag Based on content");
is_deeply ($tag->format, [ ";; 1. Based on: 28" ], "set_tag Based on format");

done_testing();
