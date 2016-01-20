#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../../../.."; #location of includes.pm
use includes; #file with paths to PsN packages


SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed" if $@;

    require so::soblock::taskinformation;

    my $msg = so::soblock::taskinformation->new(
        RunTime => 3.1416,
    );

    my $xml = $msg->xml();
    my $xml_string = $xml->toString();

    is ($xml_string, '<TaskInformation><RunTime>3.1416</RunTime></TaskInformation>', "RunTime");
}

done_testing();
