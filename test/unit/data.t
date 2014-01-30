#!/usr/bin/perl

# Unit tests for data.pm
# Note that only a small portion of the module is tested

use strict;
use warnings;
use Test::More tests=>168;
use Test::Exception;
use Math::Random;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages
use FindBin qw($Bin);
use data;


sub is_array
{
	my $func=shift;
	my $facit=shift;
	my $label=shift;

	is (scalar(@{$func}),scalar(@{$facit}),"$label, equal length");

	my $min = scalar(@{$func});
	$min = scalar(@{$facit}) if (scalar(@{$facit})< $min);
	for (my $i=0; $i<$min; $i++){
		is ($func->[$i],$facit->[$i],"$label, index $i");
	}		
}


my $data = data->new( 
   idcolumn             => 1,
   filename             => 'testdata_no_missing.csv',
   directory            => "$Bin/../test_files",
   ignore_missing_files => 0,
   skip_parsing         => 0,
   target               => 'mem');
my $dotdata = data->new( 
   idcolumn             => 1,
   filename             => 'testdata_with_dot.csv',
   directory            => "$Bin/../test_files",
   ignore_missing_files => 0,
   skip_parsing         => 0,
   target               => 'mem');

# Test of reconcile_column
is_array ($data->reconcile_column(template_values=>[1,2,3,4], old_values=> [1,1,1], equal_obs =>1), [1,2,3], 
   "reconcile_column equal_obs template longer");
is_array ($data->reconcile_column(template_values=>[1,2,3], old_values=> [1,1,1,1], equal_obs =>1), [1,2,3,3], 
   "reconcile_column equal_obs template shorter");
is_array ($data->reconcile_column(template_values=>[1,2,3], old_values=> [1,1,1], equal_obs =>1), [1,2,3], 
   "reconcile_column equal_obs template equal");
is_array ($data->reconcile_column(template_values=>[1,'.',3], old_values=> [1,1,1], equal_obs =>1), [1,'.',3], 
   "reconcile_column equal_obs template equal with dots");
is_array ($data->reconcile_column(template_values=>[0,2,2], old_values=> [5,5,1], equal_obs =>0), [0,0,2], 
   "reconcile_column not equal_obs template equal");
is_array ($data->reconcile_column(template_values=>[0,0,0], old_values=> [8,8,1], equal_obs =>0), [0,0,0], 
   "reconcile_column not equal_obs template shorter");
is_array ($data->reconcile_column(template_values=>[0,1,1,2], old_values=> [3,3,4], equal_obs =>0), [0,0,1], 
   "reconcile_column not equal_obs template longer");

is_array ($data->reconcile_column(template_values=>[0,1,1], old_values=> [0,0,2,2], equal_obs =>1), [0,1,1,1], 
	  "reconcile_column equal_obs template shorter userguide");
is_array ($data->reconcile_column(template_values=>[0,1,-99,2], old_values=> [3,3,4], equal_obs =>1), [0,1,-99], 
	  "reconcile_column equal_obs missing value");
is_array ($data->reconcile_column(template_values=>[0,1,1], old_values=> [0,0,2,2], equal_obs =>0), [0,0,1,1], 
	  "reconcile_column not equal_obs userguide ");

is_array ($data->reconcile_column(template_values=>[0,1,1,1], old_values=> [0,0,2,3], equal_obs =>0), [0,0,1,1], 
	  "reconcile_column not equal_obs userguide ");

is_array ($data->reconcile_column(template_values=>[1,-99,1,0], old_values=> [0,0,2,2], equal_obs =>0), [1,1,0,0], 
	  "reconcile_column not equal_obs missing userguide 1");
is_array ($data->reconcile_column(template_values=>[1,1,1,0], old_values=> [0,-99,2,2], equal_obs =>0), [1,1,0,0], 
	  "reconcile_column not equal_obs missing userguide 2");
is_array ($data->reconcile_column(template_values=>[-99,1,1,0], old_values=> [0,0,2,2], equal_obs =>0), [1,1,0,0], 
	  "reconcile_column not equal_obs missing userguide 3");
is_array ($data->reconcile_column(template_values=>[0,0,1,1], old_values=> ['.',0,0,0], equal_obs =>0), [0,1,1,1], 
	  "reconcile_column not equal_obs missing userguide 4");

dies_ok {$data->reconcile_column(template_values=>[0,1,1,2], old_values=> [3,3,4], equal_obs =>undef)  } "undef param equal_obs";
dies_ok {$data->reconcile_column(template_values=>[], old_values=> [3,3,4], equal_obs =>undef)  } "empty template";

#split_vertically
my ($left,$right,$split_val,$strat_val)=$data->split_vertically(split_index => 1, stratify_index =>8);
for (my $i=0; $i< 10; $i++){
    is_array ($split_val->[$i],[$i*10,$i*10,$i*10,$i*10],"split vertically split_val indiv $i");
}
is_array($strat_val,[1,1,1,1,2,2,2,2,2,2],"split_vertically strat values");
#append_column
#$data->individuals

random_set_seed_from_phrase('12345');
my $arr= $data->randomize_data(samples => 1,rand_index=> 1, stratify_index=> 8, equal_obs=>1, directory => $Bin);
my $filename = 'rand_1.dta';

my $newdata = data ->new( 
   idcolumn             => 1,
   filename             => $filename,
   directory            => $Bin,
   ignore_missing_files => 0,
   skip_parsing         => 0,
   target               => 'mem');



is_array ($newdata->individuals()->[3]->subject_data(),['4,30,0,110.44,0,84,1,1,1',
							'4,30,1,96.554,0,84,1,1,1',
							'4,30,2,104.34,0,84,1,1,1',
							'4,30,3,123.64,0,84,1,1,1'],"randomized data indiv 3");

is_array ($newdata->individuals()->[7]->subject_data(),['8,60,0,64.938,5,66,1,1,2',
							'8,60,1,91.229,5,66,1,1,2',
							'8,60,2,100.24,5,66,1,1,2',
							'8,60,3,87.51,5,66,1,1,2'],"randomized data indiv 7");

unlink($Bin.'/'.$filename);
random_set_seed_from_phrase('12345');
$arr = $dotdata->randomize_data(samples => 1,rand_index=> 1, equal_obs=>0, directory => $Bin);

$newdata = data ->new( 
   idcolumn             => 1,
   filename             => $filename,
   directory            => $Bin,
   ignore_missing_files => 0,
   skip_parsing         => 0,
   target               => 'mem');

is_array ($newdata->individuals()->[0]->subject_data(),['1,0,0,207.53,0,57,1,1,1',
							'1,70,1,227.64,0,57,1,1,1',
							'1,70,2,256.08,0,57,1,1,1',
							'1,70,3,262.82,0,57,1,1,1'],"randomized dot data indiv 0");

is_array ($newdata->individuals()->[3]->subject_data(),['4,.,0,110.44,0,84,1,1,1',
							'4,0,1,96.554,0,84,1,1,1',
							'4,0,2,104.34,0,84,1,1,1',
							'4,0,3,123.64,0,84,1,1,1'],"randomized dot data indiv 3");


unlink($Bin.'/'.$filename);

#this is for frem helper functions
my $phifile=$Bin."/../test_files/mox1.phi";
my $phi = data -> new(filename=>$phifile);
my $eta_matrix = $phi -> get_eta_matrix( start_eta => 4,
										 n_eta => 4);
is_array ($eta_matrix->[0],['1.97271E-06','-2.92739E-06','1.83230E-05','5.73266E-06'],"eta matrix start_eta 4 n_eta 4 row index 0");
is_array ($eta_matrix->[5],['1.29845E-05','-1.44714E-05','1.98524E-05','8.67448E-05'],"eta matrix start_eta 4 n_eta 4 row index 5");
is_array ($eta_matrix->[73],['8.98237E-06','-1.58557E-05','-6.15118E-05','9.96136E-05'],"eta matrix start_eta 4 n_eta 4 row index 73");


# full_name
my $data = data->new(filename => $phifile);
$data->filename('perl');

if ($^O =~ /Win/) {
	$data->directory('C:\usr\bin');
	is($data->full_name, 'C:\usr\bin\perl', "Test windows path"); 
	$data->directory("C:\\usr\\bin\\");
	is($data->full_name, 'C:\usr\bin\perl', "Test windows path with trailing slash");
} else {
	$data->directory('/usr/bin');
	is($data->full_name, '/usr/bin/perl', "Test UNIX path");
	$data->directory('/usr/bin/');
	is($data->full_name, '/usr/bin/perl', "Test UNIX path with trailing slash");
}

done_testing;
