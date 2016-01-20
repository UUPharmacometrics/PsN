#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../../../../"; #location of includes.pm
use includes; #file with paths to PsN packages


SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed" if $@;

    require so::soblock::estimation::ofmeasures;

    my $msg = so::soblock::estimation::ofmeasures->new(
        Deviance => 1.56,
    );

    my $xml = $msg->xml();
    my $xml_string = $xml->toString();

    is ($xml_string, '<OFMeasures><Deviance>1.56</Deviance></OFMeasures>', "OFMeasures");
}

done_testing();
