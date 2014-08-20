#!/etc/bin/perl

# Blackbox testing of the autobinning feature in vpc.

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>9;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_vpcbinning');
our $dir = "$tempdir/vpc_test";

sub get_bins
{
  open my $fh, "<", "$dir/vpc_bins.txt";

  my $s = <$fh>;
  $s =~ s/-bin_array=//;

  my @a = split ",", $s;

  close $fh;

  return @a;
}

sub compare_bins
{
  my $a1 = shift;
  my $a2 = shift;

  return 0 if (!defined($a1) or !defined($a2));

  return 0 if (!scalar(@$a1) or !scalar($a2));

  for my $i (0..$#$a1) {
    return 0 if ($$a1[$i] != $$a2[$i]);
  }

  return 1;
}

my $model_dir = $includes::testfiledir;

my @a;

# Commands that should return error
my @command_line = (get_command('vpc') ." -samples=20 $model_dir/pheno.mod -min_points_in_bin=28 -bin_array=10,20,30 -directory=$dir",       # Min points in bin without -auto_bin
					get_command('vpc') . " -samples=20 $model_dir/pheno.mod -auto_bin=20 -bin_array=10,20,30 -directory=$dir",     # Mixing auto_bin with bin_array
					get_command('vpc') . " -samples=20 $model_dir/pheno.mod -auto_bin=auto -bin_by_count=0 -directory=$dir",       # Mixing auto_bin with bin_by_count
                 ); 
my $rc;

foreach my $i (0..$#command_line) {
  $rc = system($command_line[$i]);
  $rc = $rc >> 8;

	ok ($rc != 0, "Command that should return error: $command_line[$i]");
}

rmtree([$dir]);

# Commands that should store bins in vpc_bins.txt
my @results = ([-8.888, 16.5, 42.65, 68.15, 93, 127.9, 148.4, 204.8, 390.1888],
               [-1.475, 6.15, 16.5, 42.65, 65.4, 76.5, 102.75, 127.9, 148.4, 204.8, 390.1888],
               [-16.0681818181818, 26.85, 68.15, 102.75, 127.9, 204.8, 390.1888],
               [-8.888, 16.5, 42.65, 68.15, 93, 127.9, 148.4, 204.8, 390.1888],
               [1.0000E+00,1.1000E+00,1.2000E+00,1.4000E+00,1.5000E+00,1.7000E+00,1.8000E+00,2.0000E+00,2.3000E+00,2.8000E+00,3.0000E+00,4.0000E+00,4.5000E+00,5.5000E+00,6.0000E+00,6.3000E+00,6.5000E+00,8.3000E+00,8.5000E+00,9.5000E+00,9.7000E+00,1.1000E+01,1.2000E+01,1.4000E+01,1.9000E+01,2.0000E+01,2.1000E+01,2.2200E+01,2.4000E+01,2.9700E+01,3.2000E+01,3.6000E+01,3.6500E+01,3.7500E+01,3.8300E+01,4.7000E+01,4.7500E+01,4.8300E+01,5.1000E+01,5.6000E+01,5.7500E+01,5.9000E+01,5.9300E+01,5.9500E+01,6.0000E+01,6.1500E+01,6.2500E+01,6.3200E+01,6.3500E+01,6.5000E+01,6.5800E+01,7.0500E+01,7.0700E+01,7.1300E+01,7.2000E+01,7.3700E+01,7.3800E+01,7.4000E+01,7.9000E+01,8.1000E+01,8.2700E+01,8.3000E+01,8.3200E+01,8.3500E+01,8.4800E+01,8.6800E+01,8.9000E+01,9.0500E+01,9.5500E+01,9.9000E+01,1.0650E+02,1.0900E+02,1.1040E+02,1.1050E+02,1.1130E+02,1.1230E+02,1.1250E+02,1.1880E+02,1.2150E+02,1.2500E+02,1.3080E+02,1.3150E+02,1.3180E+02,1.3200E+02,1.3220E+02,1.3400E+02,1.3430E+02,1.3550E+02,1.3790E+02,1.3800E+02,1.4000E+02,1.4210E+02,1.4220E+02,1.4280E+02,1.4380E+02,1.4570E+02,1.4670E+02,1.4680E+02,1.5000E+02,1.5350E+02,1.5500E+02,1.5800E+02,1.5980E+02,1.6200E+02,1.6530E+02,1.6900E+02,1.7600E+02,1.8330E+02,2.2630E+02,2.6040E+02,2.6060E+02,2.9200E+02,3.0250E+02,3.0330E+02,3.0400E+02,3.1030E+02,3.1260E+02,3.8980E+02],
               [49.6,98.2,146.8,195.4,244,292.6,341.2,389.8],
              );

@command_line = (get_command('vpc') . " -samples=20 $model_dir/pheno.mod -auto_bin=auto -directory=$dir",       # The automatic option (same as default below)
				 get_command('vpc') . " -samples=20 $model_dir/pheno.mod -auto_bin=10 -directory=$dir",         # Pre-defined number of bins
				 get_command('vpc') . " -samples=20 $model_dir/pheno.mod -auto_bin=4,7 -directory=$dir",        # Search in a range
				 get_command('vpc') . " -samples=20 $model_dir/pheno.mod -directory=$dir",                       # Auto find number of bins. Default behaviour
				 get_command('vpc') . " -samples=20 $model_dir/pheno.mod -auto_bin=unique  -directory=$dir",     # Bin on unique values
				 get_command('vpc') . " -samples=20 $model_dir/pheno.mod -bin_by_count=0 -no_of_bins=8  -directory=$dir",
	);

my $is_equal;

foreach my $i (0..$#command_line) {
  system $command_line[$i];

  @a = get_bins;

	$is_equal = compare_bins(\@a, $results[$i]);

  if (not $is_equal) {
    print "Error in binning: $command_line[$i]\n";
    print "Should be: @{$results[$i]}\n";
    print "Was: @a\n";
  	rmtree([$dir]);
  }

	ok ($is_equal, "Testing: $command_line[$i]");
}

remove_test_dir($tempdir);

done_testing();
