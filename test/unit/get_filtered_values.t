#!/usr/bin/perl

# Unit tests for output.pm

use strict;
use warnings;
use Test::More;
use Test::Exception;
use Math::Random;
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

done_testing;
