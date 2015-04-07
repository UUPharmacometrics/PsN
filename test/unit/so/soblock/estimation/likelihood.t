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

    require so::soblock::estimation::likelihood;

    my $msg = so::soblock::estimation::likelihood->new(
        Deviance => 1.56,
    );

    my $xml = $msg->xml();
    my $xml_string = $xml->toString();

    is ($xml_string, '<Likelihood><Deviance>1.56</Deviance></Likelihood>', "Likelihood");
}

done_testing();
