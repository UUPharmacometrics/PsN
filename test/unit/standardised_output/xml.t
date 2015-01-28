#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

use standardised_output;
use standardised_output::xml;
use XML::LibXML;

# mangle_symbol_idtype
my $xml = standardised_output::xml->new();
is ($xml->mangle_symbol_idtype("nomangle"), "nomangle", "mangle_symbol_idtype nomangle");
is ($xml->mangle_symbol_idtype(":first"), "_first", "mangle_symbol_idtype first invalid 1");
is ($xml->mangle_symbol_idtype("?first"), "_first", "mangle_symbol_idtype first invalid 2");
is ($xml->mangle_symbol_idtype("SIGMA(1,1)"), "SIGMA_1_1_", "mangle_symbol_idtype SIGMA");

# create_typed_element 
my $xml = standardised_output::xml->new();
my $node = $xml->create_typed_element(type => "MyType", content => "MyContent");
is ($node->textContent, "MyContent", "create_typed_element content");
is ($node->nodeName, "ct:MyType", "create_typed_element name");

done_testing();
