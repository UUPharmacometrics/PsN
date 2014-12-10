#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use standardised_output;
use XML::LibXML;

our $tempdir = create_test_dir('unit_standardised_output');
copy_test_files($tempdir, [ "missingdata.lst", "missingmodel.lst", "psnmissingdata.out", "psnmissingmodel.out" ]);

chdir $tempdir;

# missingdata.lst
my $so = standardised_output->new(lst_files => [ "missingdata.lst" ]);
$so->parse;
my $doc = XML::LibXML->load_xml(location => "missingdata.SO.xml");
my $xpc = XML::LibXML::XPathContext->new($doc);
$xpc->registerNs('x' => 'http://www.pharmml.org/2013/03/StandardisedOutput');
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/x:TargetToolMessages/x:Errors');
ok (defined $node, "missingdata.lst");
my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
is (scalar(@nodes), 1, "missingdata.lst nothing more than TargetToolMessages"); 


# missingmodel.lst
my $so = standardised_output->new(lst_files => [ "missingmodel.lst" ]);
$so->parse;
my $doc = XML::LibXML->load_xml(location => "missingmodel.SO.xml");
my $xpc = XML::LibXML::XPathContext->new($doc);
$xpc->registerNs('x' => 'http://www.pharmml.org/2013/03/StandardisedOutput');
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/x:TargetToolMessages/x:Errors');
ok (defined $node, "missingmodel.lst");
my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
is (scalar(@nodes), 1, "missingmodel.lst nothing more than TargetToolMessages"); 

# psnmissingdata.out
my $so = standardised_output->new(lst_files => [ "psnmissingdata.out" ]);
$so->parse;
my $doc = XML::LibXML->load_xml(location => "psnmissingdata.SO.xml");
my $xpc = XML::LibXML::XPathContext->new($doc);
$xpc->registerNs('x' => 'http://www.pharmml.org/2013/03/StandardisedOutput');
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/x:TargetToolMessages/x:Errors');
ok (defined $node, "psnmissingdata.out");
my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
print $node->toString(1);
is (scalar(@nodes), 1, "psnmissingdata.out nothing more than TargetToolMessages");

# psnmissingmodel.out
my $so = standardised_output->new(lst_files => [ "psnmissingmodel.out" ]);
$so->parse;
my $doc = XML::LibXML->load_xml(location => "psnmissingmodel.SO.xml");
my $xpc = XML::LibXML::XPathContext->new($doc);
$xpc->registerNs('x' => 'http://www.pharmml.org/2013/03/StandardisedOutput');
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/x:TargetToolMessages/x:Errors');
ok (defined $node, "psnmissingmodel.out");
my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
print $node->toString(1);
is (scalar(@nodes), 1, "psnmissingmodel.out nothing more than TargetToolMessages");



remove_test_dir($tempdir);

done_testing();
