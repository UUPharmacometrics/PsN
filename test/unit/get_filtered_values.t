#!/usr/bin/perl

# Unit tests for output.pm

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use ui;

unshift @INC, $includes::testfiledir . '/output';
require answers;
use output;

ui->silent(1);

our $test_files = $includes::testfiledir . '/output/';

$SIG{__WARN__} = sub {
	my $message = shift;
};

sub cmp_array
{
    my $func=shift;
    my $facit=shift;
    my $label=shift;

    is (scalar(@{$func}),scalar(@{$facit}),"$label, equal length");

    my $min = scalar(@{$func});
    $min = scalar(@{$facit}) if (scalar(@{$facit})< $min);
    for (my $i=0; $i<$min; $i++){
    	cmp_ok($func->[$i],'==',$facit->[$i],"$label, index $i");
    }		
	
}


my $fname = 'onePROB/oneEST/noSIM/warfarin_ddmore.lst';
my $outfile = $test_files.$fname;
ok(-e $outfile, "output file $outfile exists");
unless(-e $outfile) {
	print "file $outfile does not exist\n";
}

my $outobj = output -> new ('filename' => $outfile);
cmp_ok($outobj->parsed_successfully,'==',1, "output file $outfile parsed successfully");

unless( $outobj -> parsed_successfully ){
	cmp_ok(length($outobj->parsing_error_message),'>',5,'error message exists');
	next;
}


my $valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 1, category => 'estimate');
cmp_array($valuesref, [1.32779E-01,8.14751E+00,1.78924E+00,8.74897E-01,1.06388E-01,-1.35376E-15,
					   2.62728E-01,2.34500E-01,1.36931E-01,9.93333E-01,3.13633E-01], 'warfarin allow sdcorr with ext');

$valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 0, category => 'estimate');
cmp_array($valuesref, [1.32779E-01,8.14751E+00,1.78924E+00,8.74897E-01,1.06388E-01,-1.35376E-15,
					   6.90262E-02,8.43631E-03,1.87501E-02,9.86710E-01,9.83656E-02], 'warfarin do not allow sdcorr with ext');


$fname = 'onePROB/oneEST/noSIM/warfarin_noext.lst';
$outfile = $test_files.$fname;
ok(-e $outfile, "output file $outfile exists");
unless(-e $outfile) {
	print "file $outfile does not exist\n";
}

$outobj = output -> new ('filename' => $outfile);
cmp_ok($outobj->parsed_successfully,'==',1, "output file $outfile parsed successfully");


# test for when only lst-file present, not ext
$valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 1, category => 'estimate');
cmp_array($valuesref, [1.33E-01,8.15E+00,1.79E+00,8.75E-01,1.06E-01,-1.35E-15,
					   2.63E-01,2.35E-01,1.37E-01,9.93E-01,3.14E-01], 'warfarin allow sdcorr no ext');

$valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 0, category => 'estimate');
cmp_array($valuesref, [1.33E-01,8.15E+00,1.79E+00,8.75E-01,1.06E-01,-1.35E-15,
					   6.90E-02,8.44E-03,1.88E-02,9.87E-01,9.84E-02], 'warfarin do not allow sdcorr no ext');



$fname = 'onePROB/oneEST/noSIM/phenocorr.lst';
$outfile = $test_files.$fname;
ok(-e $outfile, "output file $outfile exists");
unless(-e $outfile) {
	print "file $outfile does not exist\n";
}

$outobj = output -> new ('filename' => $outfile);
cmp_ok($outobj->parsed_successfully,'==',1, "output file $outfile parsed successfully");



$valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 0, category => 'estimate');
cmp_array($valuesref, [5.55362E-03,1.33638E+00,
					   2.47073E-01,1.41582E-01,1.64152E-02
					   ],'phenocorr est without sdcorr');

$valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 1, category => 'estimate');
cmp_array($valuesref, [5.55362E-03,1.33638E+00,
					   4.97064E-01,3.76274E-01,1.64152E-02
					   ], 'phenocorr est with sdcorr' );


$valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 0, category => 'se');
cmp_array($valuesref, [3.94765E-04,7.99017E-02,
					   1.55529E-01,3.48943E-02,3.39464E-03  
		  ], 'phenocorr se without sdcorr');

$valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 1, category => 'se');
cmp_array($valuesref, [3.94765E-04,7.99017E-02,
					   1.56448E-01,4.63682E-02,3.39464E-03 
		  ],'phenocorr se with sdcorr ');



$fname = '../SO/pheno.lst';
$outfile = $test_files.$fname;
ok(-e $outfile, "output file $outfile exists");
unless(-e $outfile) {
	print "file $outfile does not exist\n";
}

$outobj = output -> new ('filename' => $outfile);
cmp_ok($outobj->parsed_successfully,'==',1, "output file $outfile parsed successfully");


# test for when only lst-file present, not ext
$valuesref = $outobj->get_filtered_values(allow_sdcorrform => 1, category => 'se');
is_deeply($valuesref, [ 0.000395, 0.0799, 0.156, 0.0349, 0.00339 ], 'pheno allow sdcorr no ext');

$valuesref = $outobj->get_filtered_values(allow_sdcorrform => 0, category => 'se');
is_deeply($valuesref, [ 0.000395, 0.0799, 0.156, 0.0349, 0.00339 ], 'pheno do not allow sdcorr no ext');


$outfile = $includes::testfiledir . '/mox_sir_block2.lst';
$outobj = output -> new ('filename' => $outfile);
my ($correl,$labels,$uncert,$err) = $outobj->get_eta_eps_correlations(num_to_sd_rescale => {});
is_deeply($labels,['ETA1','ETA2','ETA3'],'correlations labels');

is_deeply($correl,[[1,undef,undef],
				   [undef,1,0.136766/sqrt(1.24558*0.218255)],
				   [undef,0.136766/sqrt(1.24558*0.218255),1]],'correlation values');

my $init_problem = $outobj->problems->[0]->input_problem;


my ($correlations,$coefficients,$covariances) = output::correlations_coefficients_covariances(
	size => 3,
	coordinate_strings => $init_problem->get_estimated_attributes(attribute => 'coordinate_strings'),
	param_vector => $init_problem->get_estimated_attributes(attribute => 'param'),
	parameter => 'omega',
	coords => $init_problem->get_estimated_attributes(attribute => 'coords'),
	off_diagonal => $init_problem->get_estimated_attributes(attribute => 'off_diagonal'),
	diagonal_estimates => {1 => 0.409882,2 => 1.24558, 3 => 0.218255},
	diagonal_position => {1 => 0,2 => 1, 3 => 2},
	rescale_sd => {3 => 1},
	estimates => $outobj->get_filtered_values (category => 'estimate'));

is_deeply($correlations,[[1,undef,undef],
						 [undef,1,0.136766/sqrt(1.24558*0.218255)],
						 [undef,0.136766/sqrt(1.24558*0.218255),1]],'correlation values');

is_deeply($covariances,[[0.409882,undef,undef],
						[undef,1.24558,0.136766],
						[undef,0.136766,0.218255]],'covariances values');

is_deeply($coefficients,[[1,undef,undef],
						 [undef,1,0.136766/0.218255],
						 [undef,0.136766/0.218255,1]],'coefficients values');

($correlations,$coefficients,$covariances) = output::correlations_coefficients_covariances(
	size => 3,
	coordinate_strings => $init_problem->get_estimated_attributes(attribute => 'coordinate_strings'),
	param_vector => $init_problem->get_estimated_attributes(attribute => 'param'),
	parameter => 'omega',
	coords => $init_problem->get_estimated_attributes(attribute => 'coords'),
	off_diagonal => $init_problem->get_estimated_attributes(attribute => 'off_diagonal'),
	diagonal_estimates => {1 => 0.409882,2 => 1.24558, 3 => 0.218255},
	diagonal_position => {1 => 0,2 => 1, 3 => 2},
	rescale_sd => {2 => 4},
	estimates => $outobj->get_filtered_values (category => 'estimate'));

is_deeply($correlations,[[1,undef,undef],
						 [undef,1,0.136766/sqrt(1.24558*0.218255)],
						 [undef,0.136766/sqrt(1.24558*0.218255),1]],'correlation values');

is_deeply($covariances,[[0.409882,undef,undef],
						[undef,1.24558*16,0.136766*4],
						[undef,0.136766*4,0.218255]],'covariances values');

is_deeply($coefficients,[[1,undef,undef],
						 [undef,1,0.136766/(4*1.24558)],
						 [undef,0.136766/(4*1.24558),1]],'coefficients values');

($correlations,$coefficients,$covariances) = output::correlations_coefficients_covariances(
	size => 3,
	coordinate_strings => $init_problem->get_estimated_attributes(attribute => 'coordinate_strings'),
	param_vector => $init_problem->get_estimated_attributes(attribute => 'param'),
	parameter => 'omega',
	coords => $init_problem->get_estimated_attributes(attribute => 'coords'),
	off_diagonal => $init_problem->get_estimated_attributes(attribute => 'off_diagonal'),
	diagonal_estimates => {1 => 0.409882,2 => 1.24558, 3 => 0.218255},
	diagonal_position => {1 => 0,2 => 1, 3 => 2},
	rescale_sd => {},
	estimates => $outobj->get_filtered_values (category => 'estimate'));

is_deeply($coefficients,[[1,undef,undef],
						 [undef,1,undef],
						 [undef,undef,1]],'coefficients values');

#todo uncert
done_testing;
