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

    require so::soblock::taskinformation::runtime;

    my $msg = so::soblock::taskinformation::runtime->new();
    is ($msg->xml(), undef, "Empty Runtime");

    my $msg = so::soblock::taskinformation::runtime->new(
        Description => 'iamrunning',
        Real => 3.1416,
    );

    my $xml = $msg->xml();
    my $xml_string = $xml->toString();

    is ($xml_string, '<RunTime><ct:Description>iamrunning</ct:Description><ct:Real>3.1416</ct:Real></RunTime>', "RunTime");
}

done_testing();
