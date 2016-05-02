#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;
use Config;
use ui;

ui->silent(1);

SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed" if $@;

    require so::parsers::nmoutput;

#    open STDERR, '>', File::Spec->devnull();	# Do not silence stderr! use silent instead
    sub get_xml
    {
        my $filename = shift;

        my $doc = XML::LibXML->load_xml(location => $filename);
        my $xpc = XML::LibXML::XPathContext->new($doc);
        $xpc->registerNs('x' => 'http://www.pharmml.org/so/0.3/StandardisedOutput');

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
    is_deeply (so::parsers::nmoutput::_get_included_columns(header => { ID => 1, TIME => 2, WT => 3, AMT => 4 }, columns => [ 'WT', 'TIME', 'SNOW' ]),
        [ 'WT', 'TIME' ], "_get_included_columns");

# _get_remaining_columns
    is_deeply (so::parsers::nmoutput::_get_remaining_columns(header => { ID => 1, TIME => 2, WT => 3, AMT => 4 }, columns => [ 'ID', 'WT' ]),
        [ 'TIME', 'AMT' ], "_get_remaining_columns");

# non existing lst file
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "this_file_is_missing.lst");
    $so->write();
    my $xpc = get_xml("this_file_is_missing.SO.xml");
    (my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node, "missing lst file");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 1, "missing lst file nothing more than TaskInformation");

# non existing multiple lst file
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "this_file_is_missing.lst");
    so::parsers::nmoutput->new(so => $so, lst_file => "this_too.lst");
    $so->write();
    $xpc = get_xml("this_file_is_missing.SO.xml");
    (my $node1, my $node2) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node1, "multiple non existing lst files 1");
    ok (defined $node2, "multiple non existing lst files 2");
    test_number_of_children($xpc, '/x:SO/x:SOBlock', 2, "multiple non existing lst files number of SOBlocks");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "multiple non existing lst files nothing more than TaskInformation");

# missingdata.lst
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "data_missing.lst");
    $so->write();
    $xpc = get_xml("data_missing.SO.xml");
    ($node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node, "data_missing.lst");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "data_missing.lst nothing more than TaskInformation+RawResults"); 

# missingmodel.lst
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "missingmodel.lst");
    $so->write();
    $xpc = get_xml("missingmodel.SO.xml");
    ($node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node, "missingmodel.lst");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "missingmodel.lst nothing more than TaskInformation+RawResults"); 

# psnmissingdata.out
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "psnmissingdata.out");
    $so->write();
    $xpc = get_xml("psnmissingdata.SO.xml");
    ($node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node, "psnmissingdata.out");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "psnmissingdata.out nothing more than TaskInformation");

# psnmissingmodel.out
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "psnmissingmodel.out");
    $so->write();
    $xpc = get_xml("psnmissingmodel.SO.xml");
    ($node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node, "psnmissingmodel.out");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "psnmissingmodel.out nothing more than TaskInformation");

# add info message
    $so = so->new(message => "Testing");
    so::parsers::nmoutput->new(so => $so, lst_file => "missingmodel.lst");
    $so->write();
    $xpc = get_xml("missingmodel.SO.xml");
    my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="INFORMATION"]');
    is (scalar(@nodes), 2, "information message");  # 2 for counting the version message

# set toolname
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "missingmodel.lst", toolname => "MyTool");
    $so->write();
    $xpc = get_xml("missingmodel.SO.xml");
    ($node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]/x:Toolname');
    is ($node->textContent, "MyTool", "Toolname");

# normal model pheno.lst
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    $xpc = get_xml("pheno.SO.xml");

    @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
    my %hash;
    my %results_hash = (
        PopulationEstimates => 1,
        PrecisionPopulationEstimates => 1,
        IndividualEstimates => 1,
        Predictions => 1,
        Residuals => 1,
        OFMeasures => 1,
    );

    foreach $node (@nodes) {
        $hash{$node->nodeName} = 1;
    }

    is_deeply(\%hash, \%results_hash, "pheno.lst has all elements under Estimation");

# exclude_elements
    $so = so->new(exclude_elements => [ 'Estimation/PopulationEstimates', 'Estimation/OFMeasures' ]);
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    $xpc = get_xml("pheno.SO.xml");

    @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
    undef %hash;
    %results_hash = (
        PrecisionPopulationEstimates => 1,
        IndividualEstimates => 1,
        Predictions => 1,
        Residuals => 1,
    );

    foreach $node (@nodes) {
        $hash{$node->nodeName} = 1;
    }

    is_deeply(\%hash, \%results_hash, "pheno.lst with exclution has all elements under Estimation");

# only_include_elements
    $so = so->new(only_include_elements => [ 'Estimation/PopulationEstimates', 'Estimation/OFMeasures' ]);
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    $xpc = get_xml("pheno.SO.xml");

    @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
    undef %hash;
    %results_hash = (
        PopulationEstimates => 1,
        OFMeasures => 1,
    );

    foreach $node (@nodes) {
        $hash{$node->nodeName} = 1;
    }

    is_deeply(\%hash, \%results_hash, "pheno.lst with only_include_elements has all elements under Estimation");

# no-use_tables
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst", use_tables => 0);
    
    $so->write();
    $xpc = get_xml("pheno.SO.xml");

    @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
    undef %hash;
    %results_hash = (
        PopulationEstimates => 1,
        PrecisionPopulationEstimates => 1,
        OFMeasures => 1,
    );

    foreach $node (@nodes) {
        $hash{$node->nodeName} = 1;
    }

    is_deeply(\%hash, \%results_hash, "pheno.lst with no-use_tables has all elements under Estimation");

# _create_eta_table
    my $eta_table = so::parsers::nmoutput::_create_eta_table(id => [ 1, 1, 2, 2], etas => [ [ 23, 23, 24, 24  ], [ 7, 7, 9, 9 ] ]);
    is_deeply($eta_table, [ [ 1, 2 ], [ 23, 24 ], [ 7, 9 ] ], "create_eta_table 1");
    $eta_table = so::parsers::nmoutput::_create_eta_table(id => [ 1, 1, 2, 2 ], occ => [ 1, 2, 1, 2 ], etas => [ [ 23, 23, 24, 24 ], [ 7, 7, 9, 9 ] ]);
    is_deeply($eta_table, [ [ 1, 1, 2, 2 ], [ 1, 2, 1, 2], [ 23, 23, 24, 24 ], [ 7, 7, 9, 9 ] ], "create_eta_table 2");

# option pharmml
    $so = so->new(PharmMLRef => 'test.xml');
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    $xpc = get_xml("pheno.SO.xml");
    ($node) = $xpc->findnodes('/x:SO/x:PharmMLRef[@name]');
    ok (defined $node, "PharmMLRef");

    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    $xpc = get_xml("pheno.SO.xml");
    ($node) = $xpc->findnodes('/x:SO/x:PharmMLRef[@name]');
    ok (!defined $node, "PharmMLRef");

# Using so.pm
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst", use_tables => 0);

    is (scalar(@{$so->SOBlock}), 1, "Pheno: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'pheno', "Pheno: name of SOBlock");

    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], "Pheno: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ 'structParameter', 'structParameter', 'varParameter_var', 'varParameter_var', 'varParameter_var' ], "Pheno: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 5 ], "Pheno: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, [ [0.00555], [1.34], [0.247], [0.142], [0.0164] ], "Pheno: PopulationEstimates columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnId, [ 'Parameter', 'SE' ], "Pheno: StandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnType, [ ('undefined') x 2 ], "Pheno: StandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->valueType, [ 'string', 'real' ], "Pheno: StandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columns, [ [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], [ 0.000395, 0.0799, 0.156, 0.0349, 0.00339 ] ], "Pheno: StandardError columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->RelativeStandardError->columnId, [ 'Parameter', 'RSE' ], "Pheno: RelativeStandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->RelativeStandardError->columnType, [ ('undefined') x 2 ], "Pheno: RelativeStandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->RelativeStandardError->valueType, [ 'string', 'real' ], "Pheno: RelativeStandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->RelativeStandardError->columns, [ [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], [ 7.11711711711712, 5.96268656716418, 63.1578947368421, 24.5774647887324, 20.6707317073171 ]  ], "Pheno: RelativeStandardError columns");

    is($so->SOBlock->[0]->Estimation->OFMeasures->Deviance, 742.051, "Pheno: Deviance");

    is(scalar(@{$so->SOBlock->[0]->RawResults->DataFile}), 1, "Pheno: Number of RawResults files");
    is($so->SOBlock->[0]->RawResults->DataFile->[0]->{path}, "pheno.lst", "Pheno: Name of lst file");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->RowNames,  [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], "Pheno: CovarianceMatrix RowNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->ColumnNames,  [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], "Pheno: CovarianceMatrix ColumnNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->MatrixRow, [
        [ 1.56e-07, 4.58e-06, -2.72e-05, 3.56e-06, 7.25e-08 ],
        [ 4.58e-06, 0.00638, -0.00193, 0.00128, 2.13e-05 ],
        [ -2.72e-05, -0.00193, 0.0242, -0.000992, 7.08e-05 ],
        [ 3.56e-06, 0.00128, -0.000992, 0.00122, -5.33e-07 ],
        [ 7.25e-08, 2.13e-05, 7.08e-05, -5.33e-07, 1.15e-05 ] ], "Pheno: CovarianceMattrix MatrixRow");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->RowNames,  [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], "Pheno: CorrelationMatrix RowNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->ColumnNames,  [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], "Pheno: CorrelationMatrix ColumnNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->MatrixRow, [
          [ 0.000395, 0.145, -0.444, 0.258, 0.0541 ],
          [ 0.145, 0.0799, -0.155, 0.46, 0.0784 ],
          [ -0.444, -0.155, 0.156, -0.183, 0.134 ],
          [ 0.258, 0.46, -0.183, 0.0349, -0.0045 ],
          [ 0.0541, 0.0784, 0.134, -0.0045, 0.00339 ] ], "Pheno CorrelationMatrix MatrixRow");


    unlink "pheno.SO.xml";
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst", use_tables => 1);


    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->columnId, [ 'ID', 'TVCL', 'TVV' ], "Pheno: Individual Estimates Mean columnId");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->columnType, [ 'id', ('undefined') x 2 ], "Pheno: Individual Estimates Mean columnType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->valueType, [ 'string', ('real') x 2 ], "Pheno: Individual Estimates Mean valueType");
     is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->columns, 
[ [ (1..59) ], 
[ ('5.5536E-03') x 59 ],
[ ('1.3364E+00') x 59 ],
], "Simeoni: Individual Estimates Mean valueType");

    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->columnId, [ 'ID', 'ETA_CL', 'ETA_V' ], "Pheno: RandomEffects EffectMean columnId");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->columnType, [ 'id', 'undefined', 'undefined' ], "Pheno: RandomEffects EffectMean columnType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->valueType, [ 'string', 'real', 'real' ], "Pheno: RandomEffects EffectMean valueType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->columns,
[ [ (1 .. 59) ],
[ '-7.2231E-02', '-3.1672E-01', '3.5414E-01', '-2.9573E-01', '2.8434E-01', '-8.9678E-02', '-7.6765E-02',
 '2.6307E-04', '-1.6139E-01',  '1.2774E-01',  '1.0385E-01',  '3.6670E-02', '-2.8186E-01', '3.9043E-02',  '6.0713E-02',
'-3.1078E-01', '-3.1421E-01',  '2.5095E-01', '-3.0838E-02',  '3.7305E-02', '2.9768E-01', '5.5099E-02',  '4.8199E-01',
 '6.8286E-01', '-4.4888E-01',  '2.3037E-01',  '2.0168E-01',  '4.9683E-03', '-1.1982E-01',  '4.0466E-01', '-9.2750E-04',
 '1.2184E-01',  '5.9511E-03',  '9.7250E-02',  '3.8826E-02',  '3.5663E-01', '-4.5052E-02',  '2.4976E-01',  '5.0822E-01',
'-8.7975E-04',  '9.5203E-02',  '1.9230E-01', '-1.1837E-02', '1.9503E-01', '-2.7395E-01', '-2.9906E-03',  '6.8518E-02',
'-1.4038E+00',  '4.0623E-02', '-1.4838E-01', '-3.7865E-01', '-5.8515E-03', '-9.9951E-02',  '3.1131E-02',  '8.4074E-02',
'-1.9180E-01',  '2.5603E-01',  '1.4262E-01', '-2.8016E-01' ],
[ qw(4.9380E-02 5.4754E-03 2.2366E-01 -3.9072E-01 3.3195E-01 2.5281E-02 -2.4741E-01 -3.6548E-01
 -7.8470E-02 3.9657E-02 3.2448E-01 5.1533E-02 -3.7635E-01 -3.4878E-01 4.2274E-02 -5.9049E-02 -2.0001E-01
 -2.4897E-01 -2.2312E-01 -2.3420E-01 1.8600E-01 2.8633E-02 6.8679E-01 8.2787E-01 -1.7685E-01 7.2344E-01
 1.3880E-02 6.5015E-01 -2.8092E-01 3.2300E-01 -1.1372E-01 1.0595E+00  1.4750E-01  1.3830E-01  3.7058E-01
 1.3774E-02 -8.7319E-02  5.8649E-02  2.6826E-01  1.9017E-02  3.4759E-01  9.3407E-01 -4.9606E-01  9.9125E-02
-3.5943E-01 -1.9316E-01  4.6323E-01 -5.0007E-01  7.0094E-02 -4.3567E-01 -1.0422E-01 -7.1755E-01  1.5743E-01
 3.9987E-02  1.9037E-01 -7.2781E-01  3.8824E-01 -1.2430E-01 -3.0386E-01) ]
], "Pheno: RandomEffects EffectMean columns");



    $so->SOBlock->[0]->create_sdtab(filename => 'sdtab.out');

    my $sdtab_out = data->new(
        filename => 'sdtab.out',
        ignoresign => '@',
        parse_header => 1,
    );

    my $sdtab = data->new(
        filename => 'sdtab',
        ignoresign => '@',
        parse_header => 1,
    );

    SKIP: {
        skip "Windows" if $Config{osname} eq 'MSWin32'; # Skip for now due to different scientific formats eg E+001 and E+01

        foreach my $colname (@{$sdtab_out->header}) {
            my $col = $sdtab->column_to_array(column => $colname); 
            my $col2 = $sdtab_out->column_to_array(column => $colname); 
            is_deeply($col, $col2, "Pheno sdtab $colname");
        }
    }

# pheno.lst without sdtab
    unlink("sdtab");
    $so = so->new(); 
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    $xpc = get_xml("pheno.SO.xml");

    @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
    undef %hash;
    %results_hash = (
        PopulationEstimates => 1,
        PrecisionPopulationEstimates => 1,
        IndividualEstimates => 1,
        OFMeasures => 1,
    );

    foreach $node (@nodes) {
        $hash{$node->nodeName} = 1;
    }

    is_deeply(\%hash, \%results_hash, "pheno.lst without sdtab has all elements under Estimation");

# pheno.lst without sdtab and patab
    unlink("patab");
    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    $xpc = get_xml("pheno.SO.xml");

    @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
    undef %hash;
    %results_hash = (
        PopulationEstimates => 1,
        PrecisionPopulationEstimates => 1,
        OFMeasures => 1,
    );

    foreach $node (@nodes) {
        $hash{$node->nodeName} = 1;
    }

    is_deeply(\%hash, \%results_hash, "pheno.lst without sdtab and patab has all elements under Estimation");

    remove_test_dir($tempdir);

    $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        [ "SO/Nock/Nock_2013_Carboplatin_PK_MONOLIX.cor","SO/Nock/Nock_2013_Carboplatin_PK_MONOLIX.cov",
		  "SO/Nock/Nock_2013_Carboplatin_PK_MONOLIX.lst","SO/Nock/Nock_2013_Carboplatin_PK_MONOLIX.ext"  ]);

    chdir $tempdir;

    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "Nock_2013_Carboplatin_PK_MONOLIX.lst", use_tables => 0);

    is (scalar(@{$so->SOBlock}), 1, "Nock: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'Nock_2013_Carboplatin_PK_MONOLIX', "Nock: name of SOBlock");
    
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, [ 'THCL', 'THV1', 'THQ', 'THV2', 'SDADD','SDPROP','CLCLCR_COV','V1KG_COV','OMCL','OMV1','SIGMA' ], "Nock: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ ('structParameter') x 8, ('varParameter_var') x 3 ], "Nock: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 11 ], "Nock: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, [ [6.80426E+00], [2.13669E+01], [7.02052E-01], [2.89742E+01], [1.23705E-02], [1.92477E-01], [1.38350E+00], [9.92102E-01], [1.88014E-02], ["0"], ["1"] ], "Nock: PopulationEstimates columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnId, [ 'Parameter', 'SE' ], "Nock: StandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnType, [ ('undefined') x 2 ], "Nock: StandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->valueType, [ 'string', 'real' ], "Nock: StandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columns, [ ['THCL', 'THV1', 'THQ', 'THV2', 'SDADD','SDPROP','CLCLCR_COV','V1KG_COV','OMCL', 'OMV1', 'SIGMA'], 
														   [2.18242E-01, 6.24475E-01, 3.87137E-02, 1.92971E+00, 2.10896E-03, 1.29605E-02, 2.50889E-01, 3.82189E-01,6.06153E-03, undef, undef ]  ], 
			  "Nock: StandardError columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->RowNames, [ 'THCL', 'THV1', 'THQ', 'THV2', 'SDADD','SDPROP','CLCLCR_COV','V1KG_COV','OMCL' ], "Nock Covariance matrix RowNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->ColumnNames, [ 'THCL', 'THV1', 'THQ', 'THV2', 'SDADD','SDPROP','CLCLCR_COV','V1KG_COV','OMCL' ], "Nock Covariance matrix ColumnNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->MatrixRow,
[ [ 4.76297E-02, 3.42333E-02, 1.46835E-03, 2.96782E-02, -5.38607E-05, 7.11927E-04, -6.62054E-03, -6.94073E-03, 2.97933E-04 ],
[ 3.42333E-02, 3.89969E-01, 1.31621E-02, 7.06424E-01, 2.08494E-05, 1.26525E-03, -1.29799E-02, 6.36062E-02, 8.37629E-04 ],
[ 1.46835E-03, 1.31621E-02, 1.49875E-03, 5.48852E-02, 3.60452E-05, -6.47254E-05, 2.73967E-03, -1.20797E-03, -6.11735E-05 ],
[ 2.96782E-02, 7.06424E-01, 5.48852E-02, 3.72379E+00, 6.46479E-04, 2.74345E-03, -1.59594E-02, 1.19457E-01, 1.56975E-04 ],
[ -5.38607E-05, 2.08494E-05, 3.60452E-05, 6.46479E-04, 4.44772E-06, -1.25832E-05, 1.76699E-04, 1.57445E-04, -7.45912E-06 ],
[ 7.11927E-04, 1.26525E-03, -6.47254E-05, 2.74345E-03, -1.25832E-05, 1.67974E-04, -9.30403E-04, 1.24400E-03, 1.63195E-05 ],
[ -6.62054E-03, -1.29799E-02, 2.73967E-03, -1.59594E-02, 1.76699E-04, -9.30403E-04, 6.29452E-02, -1.97132E-02, -1.05782E-03 ],
[ -6.94073E-03, 6.36062E-02, -1.20797E-03, 1.19457E-01, 1.57445E-04, 1.24400E-03, -1.97132E-02, 1.46069E-01, -1.02889E-04 ],
[ 2.97933E-04, 8.37629E-04, -6.11735E-05, 1.56975E-04, -7.45912E-06, 1.63195E-05, -1.05782E-03, -1.02889E-04, 3.67421E-05 ] ]
, "Nock: Covariance matrix MatrixRow");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->RowNames, [ 'THCL', 'THV1', 'THQ', 'THV2', 'SDADD','SDPROP','CLCLCR_COV','V1KG_COV','OMCL' ], "Nock Correlation matrix RowNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->ColumnNames, [ 'THCL', 'THV1', 'THQ', 'THV2', 'SDADD','SDPROP','CLCLCR_COV','V1KG_COV','OMCL' ], "Nock Correlation matrix ColumnNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->MatrixRow,
[ [ 2.18242E-01, 2.51186E-01, 1.73791E-01, 7.04704E-02, -1.17021E-01, 2.51696E-01, -1.20913E-01, -8.32123E-02, 2.25215E-01 ],
[ 2.51186E-01, 6.24475E-01, 5.44432E-01, 5.86217E-01, 1.58311E-02, 1.56330E-01, -8.28465E-02, 2.66505E-01, 2.21286E-01 ],
[ 1.73791E-01, 5.44432E-01, 3.87137E-02, 7.34679E-01, 4.41483E-01, -1.29000E-01, 2.82066E-01, -8.16420E-02, -2.60685E-01 ],
[ 7.04704E-02, 5.86217E-01, 7.34679E-01, 1.92971E+00, 1.58852E-01, 1.09694E-01, -3.29642E-02, 1.61972E-01, 1.34201E-02 ],
[ -1.17021E-01, 1.58311E-02, 4.41483E-01, 1.58852E-01, 2.10896E-03, -4.60365E-01, 3.33953E-01, 1.95336E-01, -5.83495E-01 ],
[ 2.51696E-01, 1.56330E-01, -1.29000E-01, 1.09694E-01, -4.60365E-01, 1.29605E-02, -2.86134E-01, 2.51143E-01, 2.07733E-01 ],
[ -1.20913E-01, -8.28465E-02, 2.82066E-01, -3.29642E-02, 3.33953E-01, -2.86134E-01, 2.50889E-01, -2.05588E-01, -6.95581E-01 ],
[ -8.32123E-02, 2.66505E-01, -8.16420E-02, 1.61972E-01, 1.95336E-01, 2.51143E-01, -2.05588E-01, 3.82189E-01, -4.44128E-02 ],
[ 2.25215E-01, 2.21286E-01, -2.60685E-01, 1.34201E-02, -5.83495E-01, 2.07733E-01, -6.95581E-01, -4.44128E-02, 6.06153E-03 ] ]
, "Nock: Correlation matrix MatrixRow");

    remove_test_dir($tempdir);

    $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        [ "output/onePROB/oneEST/noSIM/warfarin_ddmore.lst","output/onePROB/oneEST/noSIM/warfarin_ddmore.ext"  ]);

    chdir $tempdir;

    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "warfarin_ddmore.lst", use_tables => 0);

    is (scalar(@{$so->SOBlock}), 1, "warfarin SAEM: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'warfarin_ddmore', "warfarin SAEM: name of SOBlock");

    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, [ 'POP_CL','POP_V','POP_KA','POP_TLAG','RUV_PROP','RUV_ADD','BETA_CL_WT', 'BETA_V_WT', 'PPV_CL','CORR_PPV_CL_V','PPV_V','PPV_KA','PPV_TLAG' ], "warfarin SAEM: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ ('structParameter') x 8, 'varParameter_stdev', 'varParameter_corr', ('varParameter_stdev') x 3 ], "warfarin SAEM: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 13 ], "warfarin SAEM: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, [ [1.32779E-01], [8.14751E+00], [  1.78924E+00 ], [8.74897E-01], [  1.06388E-01], 
																 [ -1.35376E-15], [7.50000E-01], [1], [2.62728E-01], [ 2.34500E-01], 
																 [  1.36931E-01], [9.93333E-01 ], [3.13633E-01] ], "warfarin SAEM: PopulationEstimates columns");


    remove_test_dir($tempdir);

    $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        [ "SO/DelBene/DelBene_2009_oncology_in_vitro_EPS_in_OBS.lst",
		  "SO/DelBene/DelBene_2009_oncology_in_vitro_EPS_in_OBS.ext",
		  "SO/DelBene/DelBene_2009_oncology_in_vitro_EPS_in_OBS.cov",
		  "SO/DelBene/DelBene_2009_oncology_in_vitro_EPS_in_OBS.cor",
		  "SO/DelBene/sdtab",
		  "SO/DelBene/cotab",
		  "SO/DelBene/patab",
		]);

    chdir $tempdir;

    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "DelBene_2009_oncology_in_vitro_EPS_in_OBS.lst", use_tables => 1);

    is (scalar(@{$so->SOBlock}), 1, "DelBene: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'DelBene_2009_oncology_in_vitro_EPS_in_OBS', "DelBene: name of SOBlock");
    

    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, ['LAMBDA0','K1','K2','N0','CV','SIGMA_RES_W'], 
			  "DelBene: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ ('structParameter') x 5, 'varParameter_var' ], "DelBene: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 6 ], "DelBene: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, 
			  [ [2.89324E-02 ],[ 7.37283E-02],[  8.20023E-02],[  2.15178E+03 ],[ 1.09068E-01], ["1"] ], "DelBene: PopulationEstimates columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnId, [ 'Parameter', 'SE' ], "DelBene: StandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnType, [ ('undefined') x 2 ], "DelBene: StandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->valueType, [ 'string', 'real' ], "DelBene: StandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columns, [ ['LAMBDA0','K1','K2','N0','CV', 'SIGMA_RES_W'], 
														   [4.28999E-04, 6.31929E-04, 2.30774E-03, 3.73786E+01, 1.28301E-02, undef ]  ], 
			  "DelBene: StandardError columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->RowNames, [ 'LAMBDA0', 'K1', 'K2', 'N0', 'CV' ], "Delbene: Covariance matrix RowNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->ColumnNames, [ 'LAMBDA0', 'K1', 'K2', 'N0', 'CV' ], "Delbene: Covariance matrix ColumnNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->MatrixRow,
[ [ 1.84040E-07, -1.24127E-07, 4.67258E-07, -5.65475E-03, 2.88256E-06 ],
[ -1.24127E-07, 3.99334E-07, -2.30590E-07, 1.52957E-02, 2.21489E-06 ],
[ 4.67258E-07, -2.30590E-07, 5.32568E-06, 4.31972E-02, -6.14332E-06 ],
[ -5.65475E-03, 1.52957E-02, 4.31972E-02, 1.39716E+03, -1.42088E-01 ],
[ 2.88256E-06, 2.21489E-06, -6.14332E-06, -1.42088E-01, 1.64611E-04 ] ], "DelBene: Covariance matrix MatrixRow");


    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->RowNames, [ 'LAMBDA0', 'K1', 'K2', 'N0', 'CV' ], "Delbene: Correlation matrix RowNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->ColumnNames, [ 'LAMBDA0', 'K1', 'K2', 'N0', 'CV' ], "Delbene: Correlation matrix ColumnNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->MatrixRow,
[ [ 4.28999E-04, -4.57871E-01, 4.71969E-01, -3.52642E-01, 5.23714E-01 ],
[ -4.57871E-01, 6.31929E-04, -1.58119E-01, 6.47557E-01, 2.73184E-01 ],
[ 4.71969E-01, -1.58119E-01, 2.30774E-03, 5.00777E-01, -2.07485E-01 ],
[ -3.52642E-01, 6.47557E-01, 5.00777E-01, 3.73786E+01, -2.96282E-01 ],
[ 5.23714E-01, 2.73184E-01, -2.07485E-01, -2.96282E-01, 1.28301E-02 ] ], "DelBene: Correlation matrix MatrixRow");

    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->xml(), undef, "Delbene: No Individual Estimates");



    remove_test_dir($tempdir);

    $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        ["SO/Simeoni/Simeoni_2004_oncology_TGI_ETA.lst",
		 "SO/Simeoni/Simeoni_2004_oncology_TGI_ETA.ext",
		 "SO/Simeoni/Simeoni_2004_oncology_TGI_ETA.cov",
		 "SO/Simeoni/Simeoni_2004_oncology_TGI_ETA.cor",
		 "SO/Simeoni/sdtab",
		 "SO/Simeoni/patab",
		]);

    chdir $tempdir;

    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "Simeoni_2004_oncology_TGI_ETA.lst", use_tables => 1);

    is (scalar(@{$so->SOBlock}), 1, "Simeoni: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'Simeoni_2004_oncology_TGI_ETA', "Simeoni: name of SOBlock");

    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, ['POP_LAMBDA0','LAMBDA1','K1','K2','W0','CV','OMEGA_LAMBDA0','SIGMA_RES_W'], 
			  "Simeoni: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ ('structParameter') x 6, ('varParameter_var') x 2 ], "Simeoni: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 8 ], "Simeoni: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, 
			  [ [2.98716E-01],[7.74151E-01],[7.86758E-01],[7.14701E-01],[4.21873E-02],[1.00394E-01], ["0"], ["1"] ], 
			  "Simeoni: PopulationEstimates columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnId, [ 'Parameter', 'SE' ], "Simeoni: StandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnType, [ ('undefined') x 2 ], "Simeoni: StandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->valueType, [ 'string', 'real' ], "Simeoni: StandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columns, [ ['POP_LAMBDA0','LAMBDA1','K1','K2','W0','CV','OMEGA_LAMBDA0', 'SIGMA_RES_W'], 
														   [ 1.77204E-02,2.00100E-02,1.33069E-01,4.72133E-02,8.84125E-03, 1.29423E-02, undef, undef ]  ], 
			  "Simeoni: StandardError columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->RowNames, [ 'POP_LAMBDA0', 'LAMBDA1', 'K1', 'K2', 'W0', 'CV' ], "Simeoni Covariance matrix RowNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->ColumnNames, [ 'POP_LAMBDA0', 'LAMBDA1', 'K1', 'K2', 'W0', 'CV' ], "Simeoni: Covariance matrix ColumnNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CovarianceMatrix->MatrixRow,
[ [ 3.14011E-04, 3.54584E-04, -2.35803E-03, 8.36636E-04, -1.56670E-04, -2.29342E-04 ],
[ 3.54584E-04, 4.00399E-04, -2.66271E-03, 9.44737E-04, -1.76913E-04, -2.58974E-04 ],
[ -2.35803E-03, -2.66271E-03, 1.77074E-02, -6.28263E-03, 1.17650E-03, 1.72222E-03 ],
[ 8.36636E-04, 9.44737E-04, -6.28263E-03, 2.22909E-03, -4.17425E-04, -6.11047E-04 ],
[ -1.56670E-04, -1.76913E-04, 1.17650E-03, -4.17425E-04, 7.81678E-05, 1.14426E-04 ],
[ -2.29342E-04, -2.58974E-04, 1.72222E-03, -6.11047E-04, 1.14426E-04, 1.67502E-04 ] ]
, "Simeoni Covariance matrix MatrixRow");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->RowNames, [ 'POP_LAMBDA0', 'LAMBDA1', 'K1', 'K2', 'W0', 'CV' ], "Simeoni Correlation matrix RowNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->ColumnNames, [ 'POP_LAMBDA0', 'LAMBDA1', 'K1', 'K2', 'W0', 'CV' ], "Simeoni: Correlation matrix ColumnNames");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->CorrelationMatrix->MatrixRow,
[ [ 1.77204E-02, 1.00000E+00, -1.00000E+00, 1.00000E+00, -1.00000E+00, -1.00000E+00 ],
[ 1.00000E+00, 2.00100E-02, -9.99999E-01, 9.99999E-01, -1.00000E+00, -1.00000E+00 ],
[ -1.00000E+00, -9.99999E-01, 1.33069E-01, -1.00000E+00, 1.00000E+00, 1.00000E+00 ],
[ 1.00000E+00, 9.99999E-01, -1.00000E+00, 4.72133E-02, -1.00000E+00, -1.00000E+00 ],
[ -1.00000E+00, -1.00000E+00, 1.00000E+00, -1.00000E+00, 8.84125E-03, 1.00000E+00 ],
[ -1.00000E+00, -1.00000E+00, 1.00000E+00, -1.00000E+00, 1.00000E+00, 1.29423E-02 ] ]
, "Simeoni Correlation matrix MatrixRow");

    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->columnId, [ 'ID', 'K10', 'K12', 'K21', 'V1', 'PSI', 'LAMBDA0' ], "Simeoni: Individual Estimates Mean columnId");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->columnType, [ 'id', ('undefined') x 6 ], "Simeoni: Individual Estimates Mean columnType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->valueType, [ 'string', ('real') x 6 ], "Simeoni: Individual Estimates Mean valueType");
     is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->columns, [ [ 1, 2], [ "2.0832E+01", "2.0832E+01" ], [ '1.4400E-01', '1.4400E-01' ], [ '2.0112E+00', '2.0112E+00' ], [ '8.1000E-01', '8.1000E-01' ], [ '2.0000E+01', '2.0000E+01' ], [ '2.9872E-01', '2.9872E-01' ] ], "Simeoni: Individual Estimates Mean valueType");

    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->columnId, [ 'ID', 'eta_OMEGA_LAMBDA0' ], "Simeoni: RandomEffects EffectMean columnId");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->columnType, [ 'id', 'undefined' ], "Simeoni: RandomEffects EffectMean columnType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->valueType, [ 'string', 'real' ], "Simeoni: RandomEffects EffectMean valueType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->columns, [ [ 1, 2], [ '0.0000E+00', '0.0000E+00' ] ], "Simeoni: RandomEffects EffectMean columns");


    remove_test_dir($tempdir);


    $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        ["SO/BOV/run3.lst",
		 "SO/BOV/patab",
		]);

    chdir $tempdir;

    $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "run3.lst", use_tables => 1);

    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->columnId, [ 'ID', 'OCC', 'CL', 'V', 'KA', 'TL' ], "BOV: Individual Estimates Mean columnId");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->columnType, [ 'id', 'occasion', ('undefined') x 4 ], "BOV: Individual Estimates Mean columnType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->valueType, [ 'string', ('real') x 5 ], "BOV: Individual Estimates Mean valueType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->Estimates->Mean->columns,
 [
[ qw(1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 12 12 13 13 14 14 15 15 16 16 17 17 18 18 19 19 20 20 22 22 23 23 24 24 25 25 26 26 28 28 30 30 32 32 33 33) ], 
[ ('1.0000E+00', '2.0000E+00') x 28 ],
[ '6.3058E-02', '1.2934E-01', '1.0687E-01', '1.6547E-01', '1.6024E-01', '1.2100E-01', '8.3581E-02', '6.6153E-02', '1.5523E-01', '6.6576E-02',
'9.8316E-02', '1.5472E-01', '2.2338E-01', '1.2572E-01', '1.0222E-01', '1.1658E-01', '8.2783E-02', '1.3661E-01', '4.1597E-02', '5.7156E-02',
'1.3816E-01', '1.1630E-01', '7.7010E-02', '1.0140E-01', '1.4539E-01', '7.1227E-02', '7.2849E-02', '1.0030E-01', '1.3399E-01', '1.2618E-01',
'5.9995E-02', '1.3095E-01', '1.1097E-01', '8.6728E-02', '7.3611E-02', '2.0642E-01', '7.2516E-02', '1.0383E-01', '1.2004E-01', '1.0664E-01',
'7.6405E-02', '1.4928E-01', '8.1024E-02', '1.2154E-01', '1.0236E-01', '1.1334E-01', '9.9044E-02', '2.1108E-01', '1.3764E-01', '1.0848E-01',
'1.1693E-01', '1.8901E-01', '9.8119E-02', '1.4357E-01', '8.5855E-02', '1.2541E-01' ],

[ '6.9217E+00', '7.9959E+00', '1.7188E+01', '5.0314E+00', '1.5101E+01', '3.9503E+00', '4.7144E+00', '1.2941E+01', '6.3007E+00',
'5.1414E+00', '7.1592E+00', '1.0697E+01', '4.5321E+00', '5.0688E+00', '1.1819E+01', '1.6318E+01', '6.2796E+00', '4.6366E+00',
'6.0399E+00', '7.5251E+00', '1.4468E+01', '1.1572E+01', '7.1344E+00', '3.9302E+00', '1.5787E+01', '6.3942E+00', '5.2562E+00',
'3.6068E+00', '6.7291E+00', '1.8034E+01', '4.0737E+00', '9.4103E+00', '9.9442E+00', '1.1902E+01', '1.1733E+01', '6.9907E+00',
'6.9197E+00', '9.5471E+00', '8.4782E+00', '8.1461E+00', '7.9246E+00', '4.9983E+00', '7.5600E+00', '1.0880E+01', '4.8229E+00',
'9.5686E+00', '8.0577E+00', '8.3499E+00', '5.3902E+00', '1.0560E+01', '2.1568E+01', '1.0700E+01', '5.6796E+00', '1.0871E+01',
'1.8200E+01', '1.2000E+01' ],

[ '1.0841E+01', '4.2922E-01', '1.1755E+00', '1.1690E+00', '1.2603E+00', '5.4366E-01', '4.7471E-01', '1.4463E+00', '1.6643E+00', '1.2434E+00',
'1.2187E+00', '1.1868E+00', '1.0141E+00', '1.1024E+00', '2.2180E+00', '8.4001E-01', '6.1014E-01', '4.1296E-01', '1.1127E+00', '1.1363E+00',
'1.6205E+00', '1.1147E+00', '2.7641E+00', '9.3123E-01', '6.9672E-01', '1.1622E+00', '9.8418E-01', '6.3646E-01', '1.0046E+00', '5.3990E-01',
'1.0967E+00', '1.1917E+00', '1.1558E+00', '1.1539E+00', '1.1513E+00', '1.2081E+00', '1.1379E+00', '1.1664E+00', '1.1770E+00', '1.1654E+00',
'1.1526E+00', '1.1828E+00', '1.1358E+00', '1.1687E+00', '1.0902E+00', '1.1574E+00', '1.1450E+00', '1.2196E+00', '1.1164E+00', '1.1560E+00',
'1.1639E+00', '1.1689E+00', '1.1007E+00', '1.1698E+00', '1.1724E+00', '1.1811E+00' ],

[ ('9.6176E-01') x 56 ]
]
     
        , "BOV: Individual Estimates Mean columns");

    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->columnId, [ 'ID', 'OCC', 'ETA_BSV_CL', 'ETA_BSV_V', 'ETA_BSV_KA', 'ETA_BSV_TL', 'ETA_BOV_CL', 'ETA_BOV_V', 'ETA_BOV_KA', 'ETA_BOV_TL' ], "BOV: RandomEffects EffectMean columnId");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->columnType, [ 'id', 'occasion', ('undefined') x 8 ], "BOV: RandomEffects EffectMean columnType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->valueType, [ 'string', ('real') x 9 ], "BOV: RandomEffects EffectMean valueType");
    is_deeply($so->SOBlock->[0]->Estimation->IndividualEstimates->RandomEffects->EffectMean->columns, 
       [
[ qw(1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 12 12 13 13 14 14 15 15 16 16 17 17 18 18 19 19 20 20 22 22 23 23 24 24 25 25 26 26 28 28 30 30 32 32 33 33) ], 
[ ('1.0000E+00', '2.0000E+00') x 28 ],

[ '-1.5877E-04', '-1.5877E-04', '2.5793E-04', '2.5793E-04', '3.0747E-04', '3.0747E-04', '-5.1496E-04', '-5.1496E-04', '3.8169E-04', '3.8169E-04',
'7.8889E-05', '7.8889E-05', '5.9253E-04', '5.9253E-04', '-1.9661E-04', '-1.9661E-04', '2.4999E-04', '2.4999E-04', '-8.6156E-04', '-8.6156E-04',
'3.9539E-05', '3.9539E-05', '-2.8014E-04', '-2.8014E-04', '-1.2815E-04', '-1.2815E-04', '1.4785E-05', '1.4785E-05', '3.6493E-04', '3.6493E-04',
'-6.6049E-05', '-6.6049E-05', '-1.9605E-04', '-1.9605E-04', '8.4755E-05', '8.4755E-05', '-1.6466E-04', '-1.6466E-04', '1.4295E-04', '1.4295E-04',
'1.3469E-04', '1.3469E-04', '-1.3348E-04', '-1.3348E-04', '-8.1874E-05', '-8.1874E-05', '2.5658E-04', '2.5658E-04', '1.9982E-05', '1.9982E-05',
'3.4926E-05', '3.4926E-05', '-4.4011E-05', '-4.4011E-05', '4.9783E-05', '4.9783E-05' ],

[ '-8.6627E-06', '-8.6627E-06', '1.5906E-05', '1.5906E-05', '-4.5381E-06', '-4.5381E-06', '-2.3319E-05', '-2.3319E-05', '1.8149E-05', '1.8149E-05',
'-4.1361E-06', '-4.1361E-06', '-4.5412E-05', '-4.5412E-05', '2.7074E-05', '2.7074E-05', '-1.2293E-05', '-1.2293E-05', '-2.4821E-05', '-2.4821E-05',
'2.9537E-05', '2.9537E-05', '-5.9448E-05', '-5.9448E-05', '1.1070E-05', '1.1070E-05', '-3.5913E-05', '-3.5913E-05', '5.2442E-05', '5.2442E-05',
'-1.3492E-05', '-1.3492E-05', '1.5949E-05', '1.5949E-05', '5.2390E-07', '5.2390E-07', '6.1487E-06', '6.1487E-06', '1.1573E-05', '1.1573E-05',
'-1.1691E-05', '-1.1691E-05', '2.7604E-06', '2.7604E-06', '-3.4048E-05', '-3.4048E-05', '-1.0384E-05', '-1.0384E-05', '-2.7139E-05', '-2.7139E-05',
'2.3174E-05', '2.3174E-05', '-2.7112E-05', '-2.7112E-05', '7.4949E-05', '7.4949E-05' ],

[ '4.4695E-05', '4.4695E-05', '9.2032E-07', '9.2032E-07', '-2.4057E-05', '-2.4057E-05', '-2.3983E-05', '-2.3983E-05', '1.5614E-05', '1.5614E-05',
'2.7589E-06', '2.7589E-06', '-6.4869E-06', '-6.4869E-06', '1.1846E-05', '1.1846E-05', '-5.9961E-05', '-5.9961E-05', '-2.0683E-06', '-2.0683E-06',
'1.0738E-05', '1.0738E-05', '2.3445E-05', '2.3445E-05', '-1.8063E-05', '-1.8063E-05', '-2.7276E-05', '-2.7276E-05', '-3.2443E-05', '-3.2443E-05',
'-8.8044E-07', '-8.8044E-07', '-1.5227E-07', '-1.5227E-07', '1.3535E-06', '1.3535E-06', '-3.2696E-07', '-3.2696E-07', '8.5716E-07', '8.5716E-07',
'6.3571E-07', '6.3571E-07', '-3.2136E-07', '-3.2136E-07', '-2.1407E-06', '-2.1407E-06', '1.4970E-06', '1.4970E-06', '-1.3323E-06', '-1.3323E-06',
'5.6231E-07', '5.6231E-07', '-1.4160E-06', '-1.4160E-06', '1.1962E-06', '1.1962E-06' ],

[ ('0.0000E+00') x 56 ],

[ '-5.0647E-01', '2.1193E-01', '2.0657E-02', '4.5783E-01', '4.2565E-01', '1.4475E-01', '-3.6073E-01', '-5.9457E-01', '7.7731E-01', '-6.9241E-02',
'-1.5354E-01', '2.9989E-01', '8.3699E-01', '2.6221E-01', '-2.4809E-01', '-1.1663E-01', '-1.8578E-02', '4.8233E-01', '-9.5802E-01', '-6.4026E-01',
'1.2280E-01', '-4.9446E-02', '-3.9743E-01', '-1.2226E-01', '2.3792E-01', '-4.7565E-01', '-1.4618E-01', '1.7361E-01', '3.6850E-01', '3.0849E-01',
'-4.5155E-01', '3.2902E-01', '-5.8598E-02', '-3.0509E-01', '-4.3694E-01', '5.9417E-01', '-3.3220E-01', '2.6736E-02', '1.9178E-01', '7.3410E-02',
'-2.0995E-01', '4.5982E-01', '-3.2657E-01', '7.8949E-02', '-1.2686E-01', '-2.5028E-02', '-1.4034E-01', '6.1633E-01', '1.3754E-01', '-1.0047E-01',
'-2.0770E-01', '2.7249E-01', '-2.3115E-01', '1.4950E-01', '-1.4328E-01', '2.3564E-01' ],

[ '-1.5081E-01', '-6.5380E-03', '7.5871E-01', '-4.6980E-01', '6.2926E-01', '-7.1168E-01', '-7.1666E-01', '2.9312E-01', '2.6649E-01', '6.3147E-02',
'-2.3835E-01', '1.6323E-01', '-4.6838E-01', '-3.5646E-01', '8.4568E-02', '4.0719E-01', '4.0021E-02', '-2.6330E-01', '-3.3535E-01', '-1.1549E-01',
'3.7994E-01', '1.5655E-01', '-2.4177E-01', '-8.3800E-01', '5.5244E-01', '-3.5137E-01', '-1.3786E-01', '-5.1445E-01', '-1.6664E-02', '9.6918E-01',
'-5.4116E-01', '2.9610E-01', '5.4988E-02', '2.3470E-01', '2.6369E-01', '-2.5417E-01', '-1.0509E-01', '2.1677E-01', '1.2508E-01', '8.5118E-02',
'1.2427E-01', '-3.3661E-01', '-1.5697E-01', '2.0711E-01', '-6.5176E-01', '3.3346E-02', '-1.1211E-01', '-7.6497E-02', '-5.8269E-01', '8.9759E-02',
'5.6093E-01', '-1.4000E-01', '-5.7082E-01', '7.8380E-02', '8.8892E-01', '4.7240E-01' ],

[ '2.2372E+00', '-9.9194E-01', '1.5578E-02', '1.0063E-02', '8.5254E-02', '-7.5551E-01', '-8.9112E-01', '2.2293E-01', '3.6328E-01', '7.1738E-02',
'5.1681E-02', '2.5185E-02', '-1.3212E-01', '-4.8608E-02', '6.5049E-01', '-3.2046E-01', '-6.4012E-01', '-1.0305E+00', '-3.9299E-02', '-1.8327E-02',
'3.3664E-01', '-3.7487E-02', '8.7057E-01', '-2.1738E-01', '-5.0746E-01', '4.2185E-03', '-1.6202E-01', '-5.9791E-01', '-1.4144E-01', '-7.6244E-01',
'-5.3772E-02', '2.9242E-02', '-1.3038E-03', '-2.9386E-03', '-5.2322E-03', '4.2941E-02', '-1.6919E-02', '7.8092E-03', '1.6900E-02', '6.9814E-03',
'-4.0487E-03', '2.1760E-02', '-1.8774E-02', '9.8209E-03', '-5.9732E-02', '8.8889E-05', '-1.0721E-02', '5.2428E-02', '-3.5990E-02', '-1.1280E-03',
'5.6778E-03', '9.9889E-03', '-5.0160E-02', '1.0709E-02', '1.2963E-02', '2.0363E-02' ],

[ ('0.0000E+00') x 56 ]
]
        , "BOV: RandomEffects EffectMean columns");

    remove_test_dir($tempdir);
}

done_testing();
