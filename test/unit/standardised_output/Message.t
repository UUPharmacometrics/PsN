#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages


SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed" if $@;

    require so::SOBlock::TaskInformation::Message;

    my $msg = so::SOBlock::TaskInformation::Message->new(
        type => 'ERROR',
        Toolname => 'myTool',
        Name => 'myName',
        Content => 'TheContent',
        Severity => 3,
    );

    my $xml = $msg->xml();
    my $xml_string = $xml->toString();
    is ($xml_string, '<Message type="ERROR"><Toolname><ct:String>myTool</ct:String></Toolname><Name><ct:String>myName</ct:String></Name><Content><ct:String>TheContent</ct:String></Content><Severity><ct:Int>3</ct:Int></Severity></Message>', "Message");
}

done_testing();
