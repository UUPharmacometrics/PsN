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
    require so::parsers::nmoutput;
    require so::soblock::estimation::precisionpopulationestimates::bootstrap;
    require so;

    our $tempdir = create_test_dir('unit_standardised_output');
    copy_test_files($tempdir,
        [ "SO/bootstrap_results.csv", "SO/bootstrap_results_sdcorr.csv", "SO/pheno_sdcorr.lst" ]);

    chdir $tempdir;

    # small pheno bootstrap
    my $so = so->new();
    so::parsers::bootstrap->new(so => $so, bootstrap_results => 'bootstrap_results.csv');
    my $percentiles = $so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->Bootstrap->PercentilesCI;

    is_deeply($percentiles->columnId, [ 'Percentile', 'CL', 'V', 'IVCL', 'IVV', 'mySIGMA' ], "Boostrap percentiles columnId");
    is_deeply($percentiles->columnType, [ ('undefined') x 6 ], "Boostrap percentiles columnType");
    is($percentiles->name, "PercentilesCI", "Boostrap percentiles name");
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

    my $precision_estimates = $so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->Bootstrap->AsymptoticCI;
    is_deeply($precision_estimates->columnId, [ 'Parameter', 'CI', 'LowerBound', 'UpperBound' ], "Bootstrap precision estimates columnId");
    is_deeply($precision_estimates->columnType, [ ('undefined') x 4 ], "Bootstrap precision estimates columnType");
    is_deeply($precision_estimates->name, "AsymptoticCI", "Bootstrap precision estimates name");
    is_deeply($precision_estimates->valueType, [ 'string', ('real') x 3 ], "Boostrap precision estimates valueType");
    is_deeply($precision_estimates->columns, [ 
        [ ("CL", "V", "IVCL", "IVV", "mySIGMA") x 4 ],
        [ (0.9) x 5, (0.95) x 5, (0.99) x 5, (0.999) x 5 ],
        [ 0.004747337, 1.188761, -0.0337135165173141, 0.0700093, 0.01060636, 0.004592882, 1.160483, -0.0875016777761174, 0.05629889, 0.009493592,  0.004291031, 1.105219, -0.192619893273328, 0.02950465, 0.007318905, 0.003940701, 1.041079, -0.314620524348119, -0.0015928643486797, 0.004794953 ],
        [ 0.006359923, 1.483999, 0.5278615, 0.2131527, 0.02222424, 0.006514378, 1.512277, 0.5816497, 0.2268631, 0.02333701, 0.006816229, 1.567541, 0.6867679, 0.2536574, 0.0255117, 0.007166559, 1.631681, 0.8087685, 0.2847549, 0.02803565 ],
         ], "Bootstrap precision estimates columns");

    # bootstrap with omegas on sd and corr form without model
    $so = so->new();
    so::parsers::bootstrap->new(so => $so, bootstrap_results => 'bootstrap_results_sdcorr.csv');
    $percentiles = $so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->Bootstrap->PercentilesCI;
    is_deeply($percentiles->columns, [ [ '2.5', '5', '95', '97.5' ], 
            [ 0.005660052, 0.00570095, 0.007192636, 0.007342812 ],
            [ 1.19874, 1.21128, 1.411374, 1.43194 ],
            [ 0.05607026, 0.06598894, 0.6062314, 0.6406822 ],
            [ 0.06605678, 0.06847714, 0.22189, 0.2483232 ],
            [ 0.05865366, 0.06483184, 0.157961, 0.170973 ],
            [ 0.009947088, 0.01072602, 0.0233814, 0.02432164 ] ], "bootstrap pheno sd/corr percentiles columns");

    $mean = $so->SOBlock->[0]->Estimation->PopulationEstimates->Bootstrap->Mean;
    is_deeply($mean->columns, [ [ 0.006366352 ], [ 1.311643 ], [ 0.268831 ], [ 0.1311976 ], [ 0.1104773 ], [ 0.01641997 ] ], "bootstrap pheno sd/corr mean columns");
    $median = $so->SOBlock->[0]->Estimation->PopulationEstimates->Bootstrap->Median;
    is_deeply($median->columns, [ [ 0.00635686 ], [ 1.30753], [ 0.256321 ], [ 0.125554 ], [ 0.108852], [ 0.016365 ] ], "bootstrap pheno sd/corr median columns");

    # bootstrap with omegas on sd and corr form with model
    # This will currently give the same result as var/cov above
    $so = so->new();
    my $nmparser = so::parsers::nmoutput->new(so => $so, lst_file => 'pheno_sdcorr.lst');
    so::parsers::bootstrap->new(so => $so, bootstrap_results => 'bootstrap_results_sdcorr.csv', labels_hash => $nmparser->labels_hash);
    $percentiles = $so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->Bootstrap->PercentilesCI;
    is_deeply($percentiles->columns, [ [ '2.5', '5', '95', '97.5' ], 
            [ 0.005660052, 0.00570095, 0.007192636, 0.007342812 ],
            [ 1.19874, 1.21128, 1.411374, 1.43194 ],
            [ 0.05607026, 0.06598894, 0.6062314, 0.6406822 ],
            [ 0.06605678, 0.06847714, 0.22189, 0.2483232 ],
            [ 0.05865366, 0.06483184, 0.157961, 0.170973 ],
            [ 0.009947088, 0.01072602, 0.0233814, 0.02432164 ] ], "bootstrap pheno sd/corr percentiles columns");

    $mean = $so->SOBlock->[0]->Estimation->PopulationEstimates->Bootstrap->Mean;
    is_deeply($mean->columns, [ [ 0.006366352 ], [ 1.311643 ], [ 0.268831 ], [ 0.1311976 ], [ 0.1104773 ], [ 0.01641997 ] ], "bootstrap pheno sd/corr mean columns");
    $median = $so->SOBlock->[0]->Estimation->PopulationEstimates->Bootstrap->Median;
    is_deeply($median->columns, [ [ 0.00635686 ], [ 1.30753], [ 0.256321 ], [ 0.125554 ], [ 0.108852], [ 0.016365 ] ], "bootstrap pheno sd/corr median columns");

    # non-existing bootstrap file
    $so = so->new();
    so::parsers::bootstrap->new(so => $so, bootstrap_results => 'nofile.csv');
    is ($so->SOBlock->[0]->Estimation->PrecisionPopulationEstimates->Bootstrap->PercentilesCI, undef, "Bootstrap with non-existing file");
    is($so->SOBlock->[0]->TaskInformation->Message->[0]->type, "ERROR", "Bootstrap with non-existing file get error");

    remove_test_dir($tempdir);
}

done_testing();
