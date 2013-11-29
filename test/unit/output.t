#!/usr/bin/perl

# Unit tests for output.pm

use strict;
use warnings;
use Test::More tests=>1386;
use Test::Exception;
use Math::Random;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages

use lib "../test_files/output";
use answers;
use output;

our $test_files = '../test_files/output/';

$SIG{__WARN__} = sub {
	my $message = shift;
};

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

my $ref = answers::read_answers();
my @answer_hashes = @{$ref};


for (my $i=0; $i< scalar(@answer_hashes); $i++){
	my $fname = $answer_hashes[$i]->{file};
	my $outfile = $test_files.$answer_hashes[$i]->{file};
	ok(-e $outfile, "output file $outfile exists");
	unless(-e $outfile) {
		print "file $outfile does not exist\n";
	    next;
	}

	my $outobj = output -> new ('filename'=> $outfile);
	cmp_ok($outobj->parsed_successfully,'==',$answer_hashes[$i]->{parsed_successfully}, "output file $outfile parsed successfully");

	unless( $outobj -> parsed_successfully ){
	    next;
	}
	foreach my $prob (keys %{$answer_hashes[$i]->{answers}}){
		foreach my $subprob (keys %{$answer_hashes[$i]->{answers}->{$prob}}){
			foreach my $attr (keys %{$answer_hashes[$i]->{answers}->{$prob}->{$subprob}}){
				if ($attr =~ /^(sethetas|seomegas|sesigmas|thetas|omegas|sigmas)/){
					cmp_array($outobj->get_single_value(problem_index => $prob, 
														subproblem_index=> $subprob, 
														attribute=>$attr),
							 $answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr},"$fname $attr prob $prob subprob $subprob");
				}elsif ($attr =~ /^condition_number/){
					my $tval = sprintf ("%.7f",$outobj->get_single_value(problem_index => $prob, 
																		 subproblem_index=> $subprob,
																		 attribute=>$attr));
					my $ansval = sprintf ("%.7f",$answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr} );
					cmp_ok($tval,'eq',$ansval,"$fname $attr prob $prob subprob $subprob");
				}else{
					cmp_ok($outobj->get_single_value(problem_index => $prob, 
													 subproblem_index=> $subprob,
													 attribute=>$attr),'==',
						   $answer_hashes[$i]->{answers}->{$prob}->{$subprob}->{$attr},"$fname $attr prob $prob subprob $subprob");
				}
			}
		}
	}
}


done_testing;

