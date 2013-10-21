#!/usr/bin/perl

# Unit tests for output.pm

use strict;
use warnings;
use Test::More;
use Test::Exception;
use Math::Random;
use FindBin qw($Bin);
use lib "$Bin/../../lib"; 	# PsN packages
use output;

our $test_files = $Bin.'/../test_files/output/';

sub cmp_array{
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

my @answer_hashes;
my $index=0;
$answer_hashes[$index]={};
$answer_hashes[$index]->{file}='nm73/anneal2_V7_30_beta.lst';
$answer_hashes[$index]->{answers}={};
#problem index  0 subproblem index 0
$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=859.85332322386887;
$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
#$answer_hashes[$index]->{answers}->{0}->{0}->{'condition_number'}=1;
$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[3.49590E+00,6.27891E+00,1.05572E+00,1.52839E+00];
$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[8.07614E-01,6.74746E-01,9.41826E-01,0.00000E+00,0.00000E+00,2.28296E-01,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00];
$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[2.64841E+00];
$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[2.01673E-01,1.80612E-01,8.08876E-02,6.94340E-02];
$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[2.33550E-01,2.57089E-01,2.93394E-01,0.00000E+00,0.00000E+00,6.04961E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00];
$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[6.99733E-01];


for (my $i=0; $i< scalar(@answer_hashes); $i++){
	my $outfile = $test_files.$answer_hashes[$index]->{file};
	ok(-e $outfile, "output file $outfile exists");
	unless(-e $outfile) {
		print "file $outfile does not exist\n";
	    next;
	}

	my $outobj = output -> new ('filename'=> $outfile);
	ok($outobj->parsed_successfully, "output file $outfile parsed successfully");

	unless( $outobj -> parsed_successfully ){
	    print "Unable to read everything from outputfile, parser error message:\n";
	    print $outobj -> parsing_error_message();
	    next;
	}
	foreach my $prob (keys $answer_hashes[$index]->{answers}){
		foreach my $subprob (keys $answer_hashes[$index]->{answers}->{$prob}){
			foreach my $attr (keys $answer_hashes[$index]->{answers}->{$prob}->{$subprob}){
				if ($attr =~ /^(sethetas|seomegas|sesigmas|thetas|omegas|sigmas)/){
					cmp_array($outobj->get_single_value(problem_index => $prob, 
														subproblem_index=> $subprob, 
														attribute=>$attr),
							 $answer_hashes[$index]->{answers}->{$prob}->{$subprob}->{$attr},"$outfile $attr");
				}else{
					cmp_ok($outobj->get_single_value(problem_index => $prob, 
													 subproblem_index=> $subprob,
													 attribute=>$attr),'==',
						   $answer_hashes[$index]->{answers}->{$prob}->{$subprob}->{$attr},"$outfile $attr");
				}
			}
		}
	}
}


done_testing;

