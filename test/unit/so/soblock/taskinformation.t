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
    my $ti = so::soblock::taskinformation->new();
    $ti->RunTime->Real(28);
    my $xml = $ti->xml();
    is ($xml->toString(), "<TaskInformation><RunTime><ct:Real>28</ct:Real></RunTime></TaskInformation>", "TaskInformation with only runtime");

    # Runtime and description
    $ti->RunTime->Description("myDescri");
    is ($ti->xml()->toString(), "<TaskInformation><RunTime><ct:Description>myDescri</ct:Description><ct:Real>28</ct:Real></RunTime></TaskInformation>", "TaskInformation with runtime+description");

    # add_message
    my $ti = so::soblock::taskinformation->new();
    $ti->add_message(
        type => "ERROR",
        toolname => "PsN",
        name => "ourName",
        content => "theContent",
        severity => 9,
    );
    is_deeply($ti, { Message => [ { type => "ERROR", Toolname => "PsN", Name => "ourName", Content => "theContent", Severity => 9 } ], RunTime => {  } }, "add_message"); 

    # message and runtime
    $ti->RunTime->Real(28);
    is ($ti->xml()->toString(),
        '<TaskInformation><Message type="ERROR"><Toolname><ct:String>PsN</ct:String></Toolname><Name><ct:String>ourName</ct:String></Name><Content><ct:String>theContent</ct:String></Content><Severity><ct:Int>9</ct:Int></Severity></Message><RunTime><ct:Real>28</ct:Real></RunTime></TaskInformation>', "TaskInformation message and runtime");
}

done_testing();
