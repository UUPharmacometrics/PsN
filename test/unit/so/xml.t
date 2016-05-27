#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed",4 if $@;

    require so;
    require so::xml;

# mangle_symbol_idtype
    is (so::xml::mangle_symbol_idtype("nomangle"), "nomangle", "mangle_symbol_idtype nomangle");
    is (so::xml::mangle_symbol_idtype(":first"), "_first", "mangle_symbol_idtype first invalid 1");
    is (so::xml::mangle_symbol_idtype("?first"), "_first", "mangle_symbol_idtype first invalid 2");
    is (so::xml::mangle_symbol_idtype("SIGMA(1,1)"), "SIGMA_1_1_", "mangle_symbol_idtype SIGMA");

}

done_testing();
