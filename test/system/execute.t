#!/etc/bin/perl

# Testing the following features of execute:
#		* shrinking

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use List::Util qw(first);

our $dir = 'execute_test';

my $model_dir = "../test_files";

my @a;
rmtree([ "./$dir" ]);

# Test option shrinking
my @shrinking_results = (40.5600924453085, -0.185810314125491, 89.4892871889343);		# Calculated with PsN-3.6.2 on Doris
my @shrinking_headings = ('shrinkage_eta1(%)', 'shrinkage_eta2(%)', 'shrinkage_iwres(%)');

my @command_line = ("execute $model_dir/pheno.mod -shrinkage -directory=$dir",
                   );

my $is_equal;

foreach my $i (0..$#command_line) {
  system $command_line[$i];

	# Search the raw_results file for the specific columns and compare values
	open my $fh, "<", "$dir/raw_results_pheno.csv";

	my $headings = <$fh>;
	my @head_array = split /\"/, $headings;
	@head_array = grep { $_ ne ',' and $_ ne ''} @head_array;	

	my $numbers = <$fh>;
	my @numbers_array = split /,/, $numbers;

	for (my $k = 0; $k < @shrinking_results; $k++) {
		my $index = first { $shrinking_headings[$k] eq $head_array[$_] } 0..$#head_array;
		# truncating with sprintf
		is (sprintf("%.5f", $numbers_array[$index]), sprintf("%.5f", $shrinking_results[$k]), $shrinking_headings[$k]);
	}

	close $fh;
}

rmtree([ "./$dir" ]);

done_testing();
