#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../../.."; #location of includes.pm
use includes; #file with paths to PsN packages


SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed" if $@;

    require so::soblock::taskinformation;

    #Empty xml
    my $ti = so::soblock::taskinformation->new();
    my $xml = $ti->xml();
    is ($xml, undef, "Empty TaskInformation");

    # Only runtime
    $ti = so::soblock::taskinformation->new();
    $ti->RunTime(28);
    $xml = $ti->xml();
    is ($xml->toString(), "<TaskInformation><RunTime>28</RunTime></TaskInformation>", "TaskInformation with only runtime");

    # add_message
    $ti = so::soblock::taskinformation->new();
    $ti->add_message(
        type => "ERROR",
        toolname => "PsN",
        name => "ourName",
        content => "theContent",
        severity => 9,
    );
    is_deeply($ti, { Message => [ { type => "ERROR", Toolname => "PsN", Name => "ourName", Content => "theContent", Severity => 9 } ] }, "add_message"); 

    # message and runtime
    $ti->RunTime(28);
    is ($ti->xml()->toString(),
        '<TaskInformation><Message type="ERROR"><Toolname>PsN</Toolname><Name>ourName</Name><Content>theContent</Content><Severity>9</Severity></Message><RunTime>28</RunTime></TaskInformation>', "TaskInformation message and runtime");
}

done_testing();
