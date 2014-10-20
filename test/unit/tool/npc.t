#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages

use tool::npc;
use model;

# Mock mkpath to avoid directory creation
*tool::mkpath = sub { 1 };

my $dummy_model = model::create_dummy_model;

# new
lives_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', auto_bin_mode => 'auto', samples => 20) } "Minimal option set";

# attribute samples
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', auto_bin_mode => 'auto') } "Missing samples";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', auto_bin_mode => 'auto', samples => 1) } "Samples too low";

# attributes autobin_mode, min_no_bins, max_no_bins
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', auto_bin_mode => 'camel', samples => 20) } "Wrong auto_bin_mode";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', auto_bin_mode => 'minmax', samples => 20) } "minmax without limits";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', auto_bin_mode => 'minmax', min_no_bins => [ 2 ], samples => 20) } "minmax with only min limit";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', auto_bin_mode => 'minmax', max_no_bins => [ 2 ], samples => 20) } "minmax with only max limit";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', auto_bin_mode => 'minmax', min_no_bins => [ 5 ], max_no_bins => [ 3 ], samples => 20) } "minmax with max smaller than min";
lives_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', auto_bin_mode => 'minmax', min_no_bins => [ 3 ], max_no_bins => [ 5 ], samples => 20) } "minmax with max smaller than min";

# attributes bin_by_count
lives_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', bin_by_count => 0, bin_array => [[0, 10]], samples => 20) } "Minimal option set bin_by_count";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', bin_by_count => 0, samples => 20) } "bin_by_count, no bin_array";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', bin_by_count => 0, bin_array => [[0, 10], []], samples => 20) } "bin_by_count, empty subarray";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', bin_by_count => 0, bin_array => [[0, 10], [8, 2]], samples => 20) } "bin_by_count, not sorted";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', bin_by_count => 1, bin_array => [[0, 10], []], samples => 20) } "bin_by_count, empty subarray";
dies_ok { tool::npc->new(models => [$dummy_model], is_vpc => 1, idv => 'ID', bin_by_count => 1, bin_array => [[0, 10], [8]], samples => 20) } "bin_by_count, not sorted";

done_testing();
