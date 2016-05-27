#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../../../.."; #location of includes.pm
use includes; #file with paths to PsN packages


SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed",1 if $@;

    require so::soblock::rawresults::datafile;

    my $df = so::soblock::rawresults::datafile->new(
        oid => 'd1',
        Description => 'myDesc',
        path => 'aPath'
    );

    my $xml = $df->xml();
    my $xml_string = $xml->toString();
    
    is ($xml_string, '<DataFile><ds:ExternalFile oid="d1"><ct:Description>myDesc</ct:Description><ds:path>aPath</ds:path></ds:ExternalFile></DataFile>', "DataFile"); 
}

done_testing();
