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

unshift @INC, $includes::testfiledir . '/output';
require answers;
use output;

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


my @files = ('onePROB/oneEST/noSIM/warfarin_ddmore.lst');

for (my $i=0; $i< scalar(@files); $i++){
	my $fname = $files[$i];
	my $outfile = $test_files.$fname;
	ok(-e $outfile, "output file $outfile exists");
	unless(-e $outfile) {
		print "file $outfile does not exist\n";
	    next;
	}

	my $outobj = output -> new ('filename' => $outfile);
	cmp_ok($outobj->parsed_successfully,'==',1, "output file $outfile parsed successfully");

	unless( $outobj -> parsed_successfully ){
		cmp_ok(length($outobj->parsing_error_message),'>',5,'error message exists');
	    next;
	}
	
	
	my $valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 1, category => 'estimate');
	cmp_array($valuesref, [1.32779E-01,8.14751E+00,1.78924E+00,8.74897E-01,1.06388E-01,-1.35376E-15,
						   2.62728E-01,2.34500E-01,1.36931E-01,9.93333E-01,3.13633E-01]);

	$valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 0, category => 'estimate');
	cmp_array($valuesref, [1.32779E-01,8.14751E+00,1.78924E+00,8.74897E-01,1.06388E-01,-1.35376E-15,
						   6.90262E-02,8.43631E-03,1.87501E-02,9.86710E-01,9.83656E-02]);

	#TODO test for when only lst-file present, not ext

#	$valuesref = $outobj ->get_filtered_values(allow_sdcorrform => 1, category => 'se');



}

done_testing;
