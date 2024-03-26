#!/etc/bin/perl

# Testing the following features of execute:
#		* smoke test
#		* shrinkage
#		* tbs

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use List::Util qw(first);
use Config;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use File::Copy 'copy';


our $tempdir = create_test_dir('system_execute');

my $model_dir = $includes::testfiledir;
#put pheno.mod in testdir so that .ext etc in testfiledir are not modified
copy_test_files($tempdir,["pheno.mod", "pheno.dta","notable.mod","tbs1.mod", "tbs1a.mod", "mox_sim_tbs.dta"]);


#test spaces in file path
#TODO make sure path to pheno.dta not so long that execute will die because of path too long, 
#then test will fail although PsN does what it should
chdir($tempdir);

SKIP: {
    skip "", 1 if $Config{'osname'} eq 'darwin';      # Temp file paths are too long on MacOS

    my $spacedir = 'a b';
    mkdir($spacedir);
    copy('pheno.mod',$spacedir);
    copy('pheno.dta',$spacedir);
    chdir($spacedir);
    my $command = get_command('execute') . " pheno.mod -no-copy_data";
    print "Running $command\n";
    my $rc = system($command);
    $rc = $rc >> 8;
    ok ($rc == 0, "$command, spaces in data path");
    chdir($tempdir);
}

my @a;

# Test option shrinkage
my @shrinking_results = (40.5600924453085, -0.185810314125491, 89.4892871889343);		# Calculated with PsN-3.6.2 on Doris
my @shrinking_headings = ('shrinkage_eta1(%)', 'shrinkage_eta2(%)', 'shrinkage_iwres(%)');

my @command_line = (
	get_command('execute') . " notable.mod -shrinkage -dir=shrink",
	get_command('execute') . " notable.mod -cwres ",
	get_command('execute') . " pheno.mod -min_retries=2 ",
	get_command('execute') . " pheno.mod -mirror_plots=2 -mirror_from_lst ",
	get_command('execute') . " pheno.mod -mirror_plots=2  ",
	get_command('execute') . " tbs1.mod -tbs  ", #prop
	get_command('execute') . " tbs1.mod -dtbs  ", #prop
	get_command('execute') . " tbs1a.mod -tbs  ", #add
	get_command('execute') . " tbs1a.mod -dtbs  ", #add
	get_command('execute') . " tbs1.mod -tbs_delta='(-1,0.01,1)'  ",
	get_command('execute') . " tbs1a.mod -tbs_delta='(-1,0.01,1)'  ",
	get_command('execute') . " tbs1a.mod -tbs_zeta='(-1,0.01,1)'  ",
	get_command('execute') . " tbs1.mod -tbs_lambda='(-2,1,2)'  ",
	get_command('execute') . " tbs1.mod -tbs_lambda='(-2,1,2)' -dtbs  ",
);

# If we are running on Windows remove ' in command line
if ($Config{osname} eq 'MSWin32') {
	foreach (@command_line) {
		tr/'//d;
	}
}

my $is_equal;

foreach my $i (0..$#command_line) {

  if ($i == 0) {
	  #shrinkage
	  system $command_line[$i];
	  # Search the raw_results file for the specific columns and compare values
	  open my $fh, "<", "shrink/raw_results_notable.csv";
	  
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
  } else {
	  my $command= $command_line[$i];
	  print "Running $command\n";
	  my $rc = system($command);
	  $rc = $rc >> 8;
	  ok ($rc == 0, "$command, should run ok");

	  if ($i==1){
		  ok(-e 'cwtab0.est',"cwres intermediate results exist");
	  }
  }

}

remove_test_dir($tempdir);

done_testing();
