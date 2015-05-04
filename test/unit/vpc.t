#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
#use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use tool::npc;

my @data = (0.45,2,3,4,5,6,49.9);
my $ceilings = tool::npc::get_bin_ceilings_from_value(data_column => \@data,
													  data_indices => [0 .. $#data ],
													  n_bins => 12);
cmp_ok($data[-1],'<=',$ceilings->[scalar(@{$ceilings})-1],'highest data point inside last bin n_bins'); 
$ceilings = tool::npc::get_bin_ceilings_from_value(data_column => \@data,
													  data_indices => [0 .. $#data ],
													  single_bin_size => 4.13);
cmp_ok($data[-1],'<=',$ceilings->[scalar(@{$ceilings})-1],'highest data point inside last bin bin_size'); 

$ceilings = tool::npc::get_bin_ceilings_from_value(data_column => \@data,
													  data_indices => [0 .. $#data ],
													  list_boundaries => [2, 10]);
cmp_ok($data[-1],'<=',$ceilings->[scalar(@{$ceilings})-1],'highest data point inside last bin list_boundaries'); 

my %bin_hash = (0=>0.45,1=>2,2=>3,3=>4,4=>5,5=>6,6=>49.9);

$ceilings = tool::npc::get_bin_ceilings_from_count(	data_column			=> \@data,
													value_hash			=> \%bin_hash,
													data_indices		=> [0 .. $#data ],
													n_bins				=> undef,
													single_bin_size     => undef,
													list_counts => [4,3]);
cmp_ok($data[-1],'<=',$ceilings->[scalar(@{$ceilings})-1],'highest data point '.$data[-1].' inside last count bin n_bins'); 


$ceilings = tool::npc::get_bin_ceilings_from_count(	data_column			=> \@data,
													value_hash			=> \%bin_hash,
													data_indices		=> [0 .. $#data ],
													n_bins				=> 3,
													single_bin_size     => undef);
cmp_ok($data[-1],'<=',$ceilings->[scalar(@{$ceilings})-1],'highest data point '.$data[-1].' inside last count bin n_bins'); 

$ceilings = tool::npc::get_bin_ceilings_from_count(	data_column			=> \@data,
													value_hash			=> \%bin_hash,
													data_indices		=> [0 .. $#data ],
													single_bin_size     => 4.00);
cmp_ok($data[-1],'<=',$ceilings->[scalar(@{$ceilings})-1],'highest data point '.$data[-1].' inside last count bin size'); 

@data = (0.45,0.45,2,2,3,3,49.9);
%bin_hash = (0=>0.45,1=>2,2=>3,3=>49.9);

$ceilings = tool::npc::get_bin_ceilings_from_count(	data_column			=> \@data,
													value_hash			=> \%bin_hash,
													data_indices		=> [0 .. $#data ],
													list_counts     => [2,3,2]);
cmp_ok($data[-1],'<=',$ceilings->[scalar(@{$ceilings})-1],'highest data point '.$data[-1].' inside last count list_counts'); 
is_deeply($ceilings,[0.45,2,49.9],'ceilings after odd count');


done_testing();
