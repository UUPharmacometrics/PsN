#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use standardised_output;
use standardised_output::so;
use XML::LibXML;

sub get_xml
{
    my $filename = shift;

    my $doc = XML::LibXML->load_xml(location => $filename);
    my $xpc = XML::LibXML::XPathContext->new($doc);
    $xpc->registerNs('x' => 'http://www.pharmml.org/so/0.1/StandardisedOutput');

    return $xpc;
}

sub test_number_of_children
{
    my $xpc = shift;
    my $xpath = shift;
    my $number = shift;
    my $text = shift;

    my @nodes = $xpc->findnodes($xpath);
    is (scalar(@nodes), $number, $text);
}

our $tempdir = create_test_dir('unit_standardised_output');
copy_test_files($tempdir,
    [ "output/special_mod/data_missing.lst", "output/special_mod/missingmodel.lst", "output/special_mod/psnmissingdata.out", "output/special_mod/psnmissingmodel.out", "SO/bootstrap_results.csv", "SO/pheno.lst", "SO/patab", "SO/sdtab" ]);

chdir $tempdir;

# _get_included_columns
is_deeply (standardised_output::_get_included_columns(header => { ID => 1, TIME => 2, WT => 3, AMT => 4 }, columns => [ 'WT', 'TIME', 'SNOW' ]),
    [ 'WT', 'TIME' ], "_get_included_columns");

# _get_remaining_columns
is_deeply (standardised_output::_get_remaining_columns(header => { ID => 1, TIME => 2, WT => 3, AMT => 4 }, columns => [ 'ID', 'WT' ]),
    [ 'TIME', 'AMT' ], "_get_remaining_columns");

# non existing lst file
my $so = standardised_output->new(lst_files => [ "this_file_is_missing.lst" ]);
$so->parse;
my $xpc = get_xml("this_file_is_missing.SO.xml");
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
ok (defined $node, "missing lst file");
test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 1, "missing lst file nothing more than TaskInformation");

# non existing multiple lst file
my $so = standardised_output->new(lst_files => [ "this_file_is_missing.lst", "this_too.lst" ]);
$so->parse;
my $xpc = get_xml("this_file_is_missing.SO.xml");
(my $node1, my $node2) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
ok (defined $node1, "multiple non existing lst files 1");
ok (defined $node2, "multiple non existing lst files 2");
test_number_of_children($xpc, '/x:SO/x:SOBlock', 2, "multiple non existing lst files number of SOBlocks");
test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "multiple non existing lst files nothing more than TaskInformation");

# missingdata.lst
my $so = standardised_output->new(lst_files => [ "data_missing.lst" ]);
$so->parse;
my $xpc = get_xml("data_missing.SO.xml");
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
ok (defined $node, "data_missing.lst");
test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "data_missing.lst nothing more than TaskInformation+RawResults"); 

# missingmodel.lst
my $so = standardised_output->new(lst_files => [ "missingmodel.lst" ]);
$so->parse;
my $xpc = get_xml("missingmodel.SO.xml");
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
ok (defined $node, "missingmodel.lst");
test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "missingmodel.lst nothing more than TaskInformation+RawResults"); 

# psnmissingdata.out
my $so = standardised_output->new(lst_files => [ "psnmissingdata.out" ]);
$so->parse;
my $xpc = get_xml("psnmissingdata.SO.xml");
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
ok (defined $node, "psnmissingdata.out");
test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "psnmissingdata.out nothing more than TaskInformation");

# psnmissingmodel.out
my $so = standardised_output->new(lst_files => [ "psnmissingmodel.out" ]);
$so->parse;
my $xpc = get_xml("psnmissingmodel.SO.xml");
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
ok (defined $node, "psnmissingmodel.out");
test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "psnmissingmodel.out nothing more than TaskInformation");

# bootstrap_results with no lst-files
my $so = standardised_output->new(bootstrap_results => "bootstrap_results.csv");
$so->parse;
my $xpc = get_xml("bootstrap.SO.xml");
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/x:PrecisionPopulationEstimates/x:Bootstrap');
ok (defined $node, "lone bootstrap");
test_number_of_children($xpc, '/x:SO/x:SOBlock/x:Estimation/*', 1, "lone bootstrap nothing more than Bootstrap");

# add info message
my $so = standardised_output->new(lst_files => [ "missingmodel.lst" ], message => "Testing");
$so->parse;
my $xpc = get_xml("missingmodel.SO.xml");
my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="INFORMATION"]');
is (scalar(@nodes), 2, "information message");  # 2 for counting the version message

# set toolname
my $so = standardised_output->new(lst_files => [ "missingmodel.lst" ], toolname => "MyTool");
$so->parse;
my $xpc = get_xml("missingmodel.SO.xml");
(my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]/x:Toolname/ct:String');
is ($node->textContent, "MyTool", "Toolname");

# normal model pheno.lst
my $so = standardised_output->new(lst_files => [ "pheno.lst" ]);
$so->parse;
my $xpc = get_xml("pheno.SO.xml");

my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
my %hash;
my %results_hash = (
    PopulationEstimates => 1,
    PrecisionPopulationEstimates => 1,
    IndividualEstimates => 1,
    Predictions => 1,
    Residuals => 1,
    Likelihood => 1,
);

foreach $node (@nodes) {
    $hash{$node->nodeName} = 1;
}

is_deeply(\%hash, \%results_hash, "pheno.lst has all elements under Estimation");

# exclude_elements
my $so = standardised_output->new(lst_files => [ "pheno.lst" ], exclude_elements => [ 'Estimation/PopulationEstimates', 'Estimation/Likelihood' ] );
$so->parse;
my $xpc = get_xml("pheno.SO.xml");

my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
my %hash;
my %results_hash = (
    PrecisionPopulationEstimates => 1,
    IndividualEstimates => 1,
    Predictions => 1,
    Residuals => 1,
);

foreach $node (@nodes) {
    $hash{$node->nodeName} = 1;
}

is_deeply(\%hash, \%results_hash, "pheno.lst with exlution has all elements under Estimation");

# only_include_elements
my $so = standardised_output->new(lst_files => [ "pheno.lst" ], only_include_elements => [ 'Estimation/PopulationEstimates', 'Estimation/Likelihood' ] );
$so->parse;
my $xpc = get_xml("pheno.SO.xml");

my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
my %hash;
my %results_hash = (
    PopulationEstimates => 1,
    Likelihood => 1,
);

foreach $node (@nodes) {
    $hash{$node->nodeName} = 1;
}

is_deeply(\%hash, \%results_hash, "pheno.lst with only_include_elements has all elements under Estimation");

# no-use_tables
my $so = standardised_output->new(lst_files => [ "pheno.lst" ], use_tables => 0);
$so->parse;
my $xpc = get_xml("pheno.SO.xml");

my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
my %hash;
my %results_hash = (
    PopulationEstimates => 1,
    PrecisionPopulationEstimates => 1,
    Likelihood => 1,
);

foreach $node (@nodes) {
    $hash{$node->nodeName} = 1;
}

is_deeply(\%hash, \%results_hash, "pheno.lst with no-use_tables has all elements under Estimation");

# pheno.lst without sdtab
unlink("sdtab");
my $so = standardised_output->new(lst_files => [ "pheno.lst" ]);
$so->parse;
my $xpc = get_xml("pheno.SO.xml");

my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
my %hash;
my %results_hash = (
    PopulationEstimates => 1,
    PrecisionPopulationEstimates => 1,
    IndividualEstimates => 1,
    Likelihood => 1,
);

foreach $node (@nodes) {
    $hash{$node->nodeName} = 1;
}

is_deeply(\%hash, \%results_hash, "pheno.lst without sdtab has all elements under Estimation");

# pheno.lst without sdtab and patab
unlink("patab");
my $so = standardised_output->new(lst_files => [ "pheno.lst" ]);
$so->parse;
my $xpc = get_xml("pheno.SO.xml");

my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
my %hash;
my %results_hash = (
    PopulationEstimates => 1,
    PrecisionPopulationEstimates => 1,
    Likelihood => 1,
);

foreach $node (@nodes) {
    $hash{$node->nodeName} = 1;
}

is_deeply(\%hash, \%results_hash, "pheno.lst without sdtab and patab has all elements under Estimation");


# option pharmml
my $so = standardised_output->new(pharmml => 'test.xml', lst_files => [ "pheno.lst" ]);
$so->parse;
my $xpc = get_xml("pheno.SO.xml");
(my $node) = $xpc->findnodes('/x:SO/x:PharmMLRef[@name]');
ok (defined $node, "PharmMLRef");

my $so = standardised_output->new(lst_files => [ "pheno.lst" ]);
$so->parse;
my $xpc = get_xml("pheno.SO.xml");
(my $node) = $xpc->findnodes('/x:SO/x:PharmMLRef[@name]');
ok (!defined $node, "PharmMLRef");

# Using so.pm
my $standardised_output = standardised_output->new(lst_files => [ "pheno.lst" ], use_tables => 0);
$standardised_output->parse;
my $so = standardised_output::so->new(filename => "pheno.SO.xml");

is (scalar(@{$so->SOBlock}), 1, "Pheno: number of SOBlocks");
is ($so->SOBlock->[0]->blkId, 'pheno', "Pheno: name of SOBlock");

remove_test_dir($tempdir);

done_testing();
