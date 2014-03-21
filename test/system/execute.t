#!/etc/bin/perl

# Testing the following features of execute:
#		* smoke test
#		* shrinkage
#		* tbs

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>12;
use List::Util qw(first);
use Config;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition


our $dir = 'execute_test';
my $model_dir = $includes::testfiledir;

my @a;
rmtree([ "./$dir" ]);

# Test option shrinking
my @shrinking_results = (40.5600924453085, -0.185810314125491, 89.4892871889343);		# Calculated with PsN-3.6.2 on Doris
my @shrinking_headings = ('shrinkage_eta1(%)', 'shrinkage_eta2(%)', 'shrinkage_iwres(%)');

my @command_line = ($includes::execute." $model_dir/pheno.mod -shrinkage -directory=$dir",
	$includes::execute." $model_dir/tbs1.mod -tbs  -directory=$dir", #prop
	$includes::execute." $model_dir/tbs1.mod -dtbs  -directory=$dir", #prop
	$includes::execute." $model_dir/tbs1a.mod -tbs  -directory=$dir", #add
	$includes::execute." $model_dir/tbs1a.mod -dtbs  -directory=$dir", #add
	$includes::execute." $model_dir/tbs1.mod -tbs_delta='(-1,0.01,1)'  -directory=$dir",
	$includes::execute." $model_dir/tbs1a.mod -tbs_delta='(-1,0.01,1)'  -directory=$dir",
	$includes::execute." $model_dir/tbs1a.mod -tbs_zeta='(-1,0.01,1)'  -directory=$dir",
	$includes::execute." $model_dir/tbs1.mod -tbs_lambda='(-2,1,2)'  -directory=$dir",
	$includes::execute." $model_dir/tbs1.mod -tbs_lambda='(-2,1,2)' -dtbs  -directory=$dir",
);

# If we are running on Windows remove ' in command line
if ($Config{osname} eq 'MSWin32') {
	foreach (@command_line) {
		tr/'//d;
	}
}

my $is_equal;

foreach my $i (0..$#command_line) {

  if ($i == 0){
	  #shrinkage
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
  }else{
	  my $command= $command_line[$i];
	  print "Running $command\n";
	  my $rc = system($command);
	  $rc = $rc >> 8;
	  ok ($rc == 0, "$command, should run ok");
#	  die;
  }
  rmtree([ "./$dir" ]);
}




done_testing();
