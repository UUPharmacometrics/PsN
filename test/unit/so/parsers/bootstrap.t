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

    require so::parsers::bootstrap;
    require so::soblock::estimation::precisionpopulationestimates::bootstrap;
    require so;

    our $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        [ "SO/bootstrap_results.csv" ]);

    chdir $tempdir;

    # small pheno bootstrap
    my $so = so->new();
    so::parsers::bootstrap->new(so => $so, bootstrap_results => 'bootstrap_results.csv');
    my $percentiles = $so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->Bootstrap->Percentiles;

    is_deeply($percentiles->columnId, [ 'Percentile', 'CL', 'V', 'IVCL', 'IVV', 'mySIGMA' ], "Boostrap percentiles columnId");
    is_deeply($percentiles->columnType, [ ('undefined') x 6 ], "Boostrap percentiles columnType");
    is($percentiles->name, "Percentiles", "Boostrap percentiles name");
    is_deeply($percentiles->valueType, [ ('real') x 6 ], "Boostrap percentiles valueType");
    is_deeply($percentiles->columns, 
        [ [ '0.5', '2.5', '5', '95', '97.5', '99.5' ],
            [ '0.004301517', '0.004653769', '0.004869242', '0.006252975', '0.006352113', '0.008914669' ],
            [ '1.186331', '1.204392', '1.222876', '1.518211', '1.562815', '1.79133' ],
            [ '0.01000931', '0.02489113', '0.037101', '0.5110723', '0.5738347', '0.9316135' ],
            [ '0.06321175', '0.08111132', '0.08900512', '0.2390193', '0.2647495', '0.3057257' ],
            [ '0.008183303', '0.01005964', '0.01067467', '0.0223192', '0.02395466', '0.02928763' ]
        ], "Bootstrap percentiles columns");

    my $mean = $so->SOBlock->[0]->Estimation->PopulationEstimates->Bootstrap->Mean;
    is_deeply($mean->columnId, [ 'CL', 'V', 'IVCL', 'IVV', 'mySIGMA' ], "Bootstrap mean columnId");
    is_deeply($mean->columnType, [ ('undefined') x 5 ], "Bootstrap mean columnType");
    is_deeply($mean->name, "Mean", "Bootstrap mean name");
    is_deeply($mean->valueType, [ ('real') x 5 ], "Boostrap mean valueType");
    is_deeply($mean->columns, [ [ 0.005616624 ], [ 1.361894 ], [ 0.2532448 ], [ 0.1511835 ], [ 0.01635697 ] ], "Bootstrap mean columns");

    my $median = $so->SOBlock->[0]->Estimation->PopulationEstimates->Bootstrap->Median;
    is_deeply($median->columnId, [ 'CL', 'V', 'IVCL', 'IVV', 'mySIGMA' ], "Bootstrap median columnId");
    is_deeply($median->columnType, [ ('undefined') x 5 ], "Bootstrap median columnType");
    is_deeply($median->name, "Median", "Bootstrap median name");
    is_deeply($median->valueType, [ ('real') x 5 ], "Boostrap median valueType");
    is_deeply($median->columns, [ [ 0.005622315 ], [ 1.356135 ], [ 0.256029 ], [ 0.1475405 ], [ 0.01651655 ] ], "Bootstrap median columns");

    # non-existing bootstrap file
    my $so = so->new();
    so::parsers::bootstrap->new(so => $so, bootstrap_results => 'nofile.csv');
    is ($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->Bootstrap->Percentiles, undef, "Bootstrap with non-existing file");
    is($so->SOBlock->[0]->TaskInformation->Message->[0]->type, "ERROR", "Bootstrap with non-existing file get error");

    remove_test_dir($tempdir);
}

done_testing();
