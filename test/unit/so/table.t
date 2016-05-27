#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed",1 if $@;

    require so::table;

    my $doc = XML::LibXML::Document->new('1.0', 'utf-8');

    my $table = so::table->new(
        columnId => [ "POP_CL", "POP_V" ],
        columnType => [ "type1", "type2" ],
        valueType => [ "real", "string" ],
        columns => [ [ 1, 2, 3, 4 ], [ 5, 6, 7, 8 ] ],
        name => 'myTable',
        document => $doc,
        precision => 2,
    );

    my $xml = $table->xml();
    my $xml_string = $xml->toString();

    is ($xml_string, '<myTable><ds:Definition><ds:Column columnId="POP_CL" columnType="type1" valueType="real" columnNum="1"/><ds:Column columnId="POP_V" columnType="type2" valueType="string" columnNum="2"/></ds:Definition><ds:Table><ds:Row><ct:Real>1</ct:Real><ct:String>5</ct:String></ds:Row><ds:Row><ct:Real>2</ct:Real><ct:String>6</ct:String></ds:Row><ds:Row><ct:Real>3</ct:Real><ct:String>7</ct:String></ds:Row><ds:Row><ct:Real>4</ct:Real><ct:String>8</ct:String></ds:Row></ds:Table></myTable>', "table->xml");

}

done_testing();
