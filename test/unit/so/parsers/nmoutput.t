#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;

SKIP: {
    eval { require XML::LibXML };
    skip "XML::LibXML not installed" if $@;

    require so::parsers::nmoutput;

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
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "this_file_is_missing.lst");
    so::parsers::nmoutput->new(so => $so, lst_file => "this_too.lst");
    $so->write();
    my $xpc = get_xml("this_file_is_missing.SO.xml");
    (my $node1, my $node2) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node1, "multiple non existing lst files 1");
    ok (defined $node2, "multiple non existing lst files 2");
    test_number_of_children($xpc, '/x:SO/x:SOBlock', 2, "multiple non existing lst files number of SOBlocks");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "multiple non existing lst files nothing more than TaskInformation");

# missingdata.lst
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "data_missing.lst");
    $so->write();
    my $xpc = get_xml("data_missing.SO.xml");
    (my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node, "data_missing.lst");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "data_missing.lst nothing more than TaskInformation+RawResults"); 

# missingmodel.lst
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "missingmodel.lst");
    $so->write();
    my $xpc = get_xml("missingmodel.SO.xml");
    (my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node, "missingmodel.lst");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "missingmodel.lst nothing more than TaskInformation+RawResults"); 

# psnmissingdata.out
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "psnmissingdata.out");
    $so->write();
    my $xpc = get_xml("psnmissingdata.SO.xml");
    (my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node, "psnmissingdata.out");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "psnmissingdata.out nothing more than TaskInformation");

# psnmissingmodel.out
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "psnmissingmodel.out");
    $so->write();
    my $xpc = get_xml("psnmissingmodel.SO.xml");
    (my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]');
    ok (defined $node, "psnmissingmodel.out");
    test_number_of_children($xpc, '/x:SO/x:SOBlock/*', 2, "psnmissingmodel.out nothing more than TaskInformation");

# add info message
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "missingmodel.lst", message => "Testing");
    $so->write();
    my $xpc = get_xml("missingmodel.SO.xml");
    my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="INFORMATION"]');
    is (scalar(@nodes), 2, "information message");  # 2 for counting the version message

# set toolname
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "missingmodel.lst", toolname => "MyTool");
    $so->write();
    my $xpc = get_xml("missingmodel.SO.xml");
    (my $node) = $xpc->findnodes('/x:SO/x:SOBlock/x:TaskInformation/x:Message[@type="ERROR"]/x:Toolname/ct:String');
    is ($node->textContent, "MyTool", "Toolname");

# normal model pheno.lst
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    my $xpc = get_xml("pheno.SO.xml");

    my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
    my %hash;
    my %results_hash = (
        PopulationEstimates => 1,
        PrecisionPopulationEstimates => 1,
        IndividualEstimates => 1,
        Predictions => 1,
        Residual => 1,
        Likelihood => 1,
    );

    foreach $node (@nodes) {
        $hash{$node->nodeName} = 1;
    }

    is_deeply(\%hash, \%results_hash, "pheno.lst has all elements under Estimation");

# exclude_elements
    my $so = so->new(exclude_elements => [ 'Estimation/PopulationEstimates', 'Estimation/Likelihood' ]);
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    my $xpc = get_xml("pheno.SO.xml");

    my @nodes = $xpc->findnodes('/x:SO/x:SOBlock/x:Estimation/*');
    my %hash;
    my %results_hash = (
        PrecisionPopulationEstimates => 1,
        IndividualEstimates => 1,
        Predictions => 1,
        Residual => 1,
    );

    foreach $node (@nodes) {
        $hash{$node->nodeName} = 1;
    }

    is_deeply(\%hash, \%results_hash, "pheno.lst with exclution has all elements under Estimation");

# only_include_elements
    my $so = so->new(only_include_elements => [ 'Estimation/PopulationEstimates', 'Estimation/Likelihood' ]);
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
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
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst", use_tables => 0);
    $so->write();
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



# option pharmml
    my $so = so->new(PharmMLRef => 'test.xml');
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    my $xpc = get_xml("pheno.SO.xml");
    (my $node) = $xpc->findnodes('/x:SO/x:PharmMLRef[@name]');
    ok (defined $node, "PharmMLRef");

    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
    my $xpc = get_xml("pheno.SO.xml");
    (my $node) = $xpc->findnodes('/x:SO/x:PharmMLRef[@name]');
    ok (!defined $node, "PharmMLRef");

# Using so.pm
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst", use_tables => 0);

    is (scalar(@{$so->SOBlock}), 1, "Pheno: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'pheno', "Pheno: name of SOBlock");

    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], "Pheno: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ ('undefined') x 5 ], "Pheno: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 5 ], "Pheno: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, [ [0.00555], [1.34], [0.247], [0.142], [0.0164] ], "Pheno: PopulationEstimates columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnId, [ 'parameter', 'SE' ], "Pheno: StandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnType, [ ('undefined') x 2 ], "Pheno: StandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->valueType, [ 'string', 'real' ], "Pheno: StandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columns, [ [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], [ 0.000395, 0.0799, 0.156, 0.0349, 0.00339 ]  ], "Pheno: StandardError columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->RelativeStandardError->columnId, [ 'parameter', 'RSE' ], "Pheno: RelativeStandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->RelativeStandardError->columnType, [ ('undefined') x 2 ], "Pheno: RelativeStandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->RelativeStandardError->valueType, [ 'string', 'real' ], "Pheno: RelativeStandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->RelativeStandardError->columns, [ [ 'CL', 'V', 'IVCL', 'IVV', 'SIGMA_1_1_' ], [ 0.0711711711711712, 0.0596268656716418, 0.631578947368421, 0.245774647887324, 0.206707317073171 ]  ], "Pheno: RelativeStandardError columns");

    is($so->SOBlock->[0]->Estimation->Likelihood->Deviance, 742.051, "Pheno: Deviance");

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
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst", use_tables => 1);

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

    foreach my $colname (@{$sdtab_out->header}) {
        my $col = $sdtab->column_to_array(column => $colname); 
        my $col2 = $sdtab_out->column_to_array(column => $colname); 
        is_deeply($col, $col2, "Pheno sdtab $colname");
    }

# pheno.lst without sdtab
    unlink("sdtab");
    my $so = so->new(); 
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
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
    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "pheno.lst");
    $so->write();
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

    remove_test_dir($tempdir);

    our $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        [ "SO/Nock/Nock_2013_Carboplatin_PK_MONOLIX.cor","SO/Nock/Nock_2013_Carboplatin_PK_MONOLIX.cov",
		  "SO/Nock/Nock_2013_Carboplatin_PK_MONOLIX.lst","SO/Nock/Nock_2013_Carboplatin_PK_MONOLIX.ext"  ]);

    chdir $tempdir;

    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "Nock_2013_Carboplatin_PK_MONOLIX.lst", use_tables => 0);

    is (scalar(@{$so->SOBlock}), 1, "Nock: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'Nock_2013_Carboplatin_PK_MONOLIX', "Nock: name of SOBlock");
    
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, [ 'THCL', 'THV1', 'THQ', 'THV2', 'SDADD','SDPROP','CLCLCR_COV','V1KG_COV','OMCL','OMV1','SIGMA' ], "Nock: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ ('undefined') x 11 ], "Nock: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 11 ], "Nock: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, [ [6.80426E+00], [2.13669E+01], [7.02052E-01], [2.89742E+01], [1.23705E-02], [1.92477E-01], [1.38350E+00], [9.92102E-01], [1.88014E-02], [0], [1] ], "Nock: PopulationEstimates columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnId, [ 'parameter', 'SE' ], "Nock: StandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnType, [ ('undefined') x 2 ], "Nock: StandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->valueType, [ 'string', 'real' ], "Nock: StandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columns, [ ['THCL', 'THV1', 'THQ', 'THV2', 'SDADD','SDPROP','CLCLCR_COV','V1KG_COV','OMCL'], 
														   [2.18242E-01, 6.24475E-01, 3.87137E-02, 1.92971E+00, 2.10896E-03, 1.29605E-02, 2.50889E-01, 3.82189E-01,6.06153E-03 ]  ], 
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

    our $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        [ "output/onePROB/oneEST/noSIM/warfarin_ddmore.lst","output/onePROB/oneEST/noSIM/warfarin_ddmore.ext"  ]);

    chdir $tempdir;

    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "warfarin_ddmore.lst", use_tables => 0);

    is (scalar(@{$so->SOBlock}), 1, "warfarin SAEM: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'warfarin_ddmore', "warfarin SAEM: name of SOBlock");

    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, [ 'POP_CL','POP_V','POP_KA','POP_TLAG','RUV_PROP','RUV_ADD','PPV_CL','CORR_PPV_CL_V','PPV_V','PPV_KA','PPV_TLAG','BETA_CL_WT','BETA_V_WT' ], "warfarin SAEM: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ ('undefined') x 13 ], "warfarin SAEM: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 13 ], "warfarin SAEM: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, [ [1.32779E-01], [8.14751E+00], [  1.78924E+00 ], [8.74897E-01], [  1.06388E-01], 
																 [ -1.35376E-15], [2.62728E-01],[  2.34500E-01], 
																 [  1.36931E-01], [9.93333E-01 ], [3.13633E-01],[7.50000E-01], [1.00000E+00]], "warfarin SAEM: PopulationEstimates columns");


    remove_test_dir($tempdir);

    our $tempdir = create_test_dir('unit_standardised_output');
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

    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "DelBene_2009_oncology_in_vitro_EPS_in_OBS.lst", use_tables => 1);

    is (scalar(@{$so->SOBlock}), 1, "DelBene: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'DelBene_2009_oncology_in_vitro_EPS_in_OBS', "DelBene: name of SOBlock");
    

    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, ['LAMBDA0','K1','K2','N0','CV','SIGMA_RES_W'], 
			  "DelBene: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ ('undefined') x 6 ], "DelBene: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 6 ], "DelBene: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, 
			  [ [2.89324E-02 ],[ 7.37283E-02],[  8.20023E-02],[  2.15178E+03 ],[ 1.09068E-01], [1] ], "DelBene: PopulationEstimates columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnId, [ 'parameter', 'SE' ], "DelBene: StandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnType, [ ('undefined') x 2 ], "DelBene: StandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->valueType, [ 'string', 'real' ], "DelBene: StandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columns, [ ['LAMBDA0','K1','K2','N0','CV'], 
														   [4.28999E-04, 6.31929E-04, 2.30774E-03, 3.73786E+01, 1.28301E-02 ]  ], 
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


    remove_test_dir($tempdir);

    our $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        ["SO/Simeoni/Simeoni_2004_oncology_TGI_ETA.lst",
		 "SO/Simeoni/Simeoni_2004_oncology_TGI_ETA.ext",
		 "SO/Simeoni/Simeoni_2004_oncology_TGI_ETA.cov",
		 "SO/Simeoni/Simeoni_2004_oncology_TGI_ETA.cor",
		 "SO/Simeoni/sdtab",
		 "SO/Simeoni/patab",
		]);

    chdir $tempdir;

    my $so = so->new();
    so::parsers::nmoutput->new(so => $so, lst_file => "Simeoni_2004_oncology_TGI_ETA.lst", use_tables => 1);

    is (scalar(@{$so->SOBlock}), 1, "Simeoni: number of SOBlocks");
    is ($so->SOBlock->[0]->blkId, 'Simeoni_2004_oncology_TGI_ETA', "Simeoni: name of SOBlock");

    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnId, ['POP_LAMBDA0','LAMBDA1','K1','K2','W0','CV','OMEGA_LAMBDA0','SIGMA_RES_W'], 
			  "Simeoni: PopulationEstimates names");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columnType, [ ('undefined') x 8 ], "Simeoni: PopulationEstimates column types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->valueType, [ ('real') x 8 ], "Simeoni: PopulationEstimates value types");
    is_deeply($so->SOBlock->[0]->Estimation->PopulationEstimates->MLE->columns, 
			  [ [2.98716E-01],[7.74151E-01],[7.86758E-01],[7.14701E-01],[4.21873E-02],[1.00394E-01], [0], [1] ], 
			  "Simeoni: PopulationEstimates columns");

    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnId, [ 'parameter', 'SE' ], "Simeoni: StandardError names");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columnType, [ ('undefined') x 2 ], "Simeoni: StandardError column types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->valueType, [ 'string', 'real' ], "Simeoni: StandardError value types");
    is_deeply($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->MLE->StandardError->columns, [ ['POP_LAMBDA0','LAMBDA1','K1','K2','W0','CV'], 
														   [ 1.77204E-02,2.00100E-02,1.33069E-01,4.72133E-02,8.84125E-03, 1.29423E-02 ]  ], 
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


    remove_test_dir($tempdir);
}

done_testing();
