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

#these methods are now static, add tests
#TODO create_unique_values_hash, with and without reference
# TODO get_bin_boundaries_overlap_count 
# TODO get_bin_boundaries_overlap_value
#TODO 	index_matrix_binned_values
# TODO do_predcorr_and_varcorr
# TODO get_npc_result_labels

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



my @pred_int = sort {$a <=> $b} 0,50,90;

my $c_i =95;
my $no_sim = 200;
my ($lower_index,$upper_index,$low_ind,$high_ind)= tool::npc::get_npc_indices('ci' => $c_i,
																			  'no_sim' => $no_sim,
																			  'pred_intervals' => \@pred_int);

is_deeply ($lower_index,[100,50,10],'get_npc_indices lower_index');
is_deeply ($upper_index,[99,149,189],'get_npc_indices upper_index');
is($low_ind,5,'get_npc_indices low_ind');
is($high_ind,194,'get_npc_indices high_ind');

$no_sim = 20;
($lower_index,$upper_index,$low_ind,$high_ind)= tool::npc::get_npc_indices('ci' => $c_i,
																			  'no_sim' => $no_sim,
																			  'pred_intervals' => \@pred_int);

is_deeply ($lower_index,[10,5,1],'get_npc_indices lower_index');
is_deeply ($upper_index,[9,14,18],'get_npc_indices upper_index');
is($low_ind,0,'get_npc_indices low_ind');
is($high_ind,19,'get_npc_indices high_ind');

my $strat_data = [['4,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20',  
				   '5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20',
				   '15,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20']];

my $strat_data2 = [['0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20',  
				   '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20',
				   '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20']];


my $cens_data3 =  [['0,1,0,0,0,0,0,0,0,0,0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0',   
				    '1,1,0,0,0,0,0,0,0,0,0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0']];

my $strat_data3 = [['12,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20',  
					'5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20']];

#lower count 0       0,0,2,2,2,2,2,1,1,1,0,0,0,0,0,0,0,0,0,0,0
#    50              0,0,2,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
#90                  0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 

#upper      0        1,0,0,0,0,0,0,1,1,1,1,1,1,0,1,1,0,1,1,1,1
# 50                 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,1,1,1
#90                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1

#lower
#perc real: 0 arr(100,100,100,100,100,50,50,50,0,0,0,0,0,0,0,0,0)            
#           0 arr(100,100,50,50,0,0,0,0,0,0,0,0,0,0,0,0,0)           
#           0 arr(50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)           
 
#upper
#perc real 100 arr(0,0,0,0,0,50,50,50,50,100,100,100,100,100,100,100,50)           
#perc real   0 arr(0,0,0,0,0, 0, 0,0, 50, 0,  0,   0,0  ,100,100,100,50)           
#perc real   0 arr(0,0,0,0,0, 0, 0,0, 50, 0,  0,   0,0  , 0, 0, 0,50)           


# count_max        1,0,2,2,2,2,2,2,2,2,2, 1, 1, 0, 1, 1, 0, 1, 1, 1, 2     nonzer0s = 17 lowind 0 highind 16

# sumwarnings      0,-99,0,0,0,0,0,0,0,0,0, 0, 0,-99,0 ,0,-99,0, 0, 0, 0

#
#sorted simvalues = 2,3,4,5,6,7,8,9,10,11,12,14,15,17,18,19,20   count 17  lmits  10,6,3 upper 10,15,19 
#                   2,3,4,5,6,7,8,9,10,20                              10   lim   7,4,2  upper 6,9,20 

my @total = (0)x 21;
my @lower_count = ([(0)x 21],[(0)x 21]);
my @upper_count = ([(0)x 21],[(0)x 21]);
tool::npc::update_counts(values =>       [18,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
			  censor_values => [0,1,0,0,0,0,0,0,0,0,0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0],
			  censored => 1,
			  predcorr => 0,
			  lower_limit => [10,4],
			  upper_limit => [15,17],
			  lower_count => \@lower_count,
			  upper_count => \@upper_count,
			  total_count => \@total);

is_deeply (\@lower_count,[[0,0,1,1,1,1,1,1,1,1,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[0,0,1,1,0,0,0,0,0,0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],'update_counts 1a');
is_deeply (\@upper_count,[[1,0,0,0,0,0,0,0,0,0,0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1],[1,0,0,0,0,0,0,0,0,0,0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1]],'update_counts 1b');
is_deeply (\@total,[1,0,1,1,1,1,1,1,1,1,1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1],'update_counts 1c');


my ($low1,$up1,$alert)= tool::npc::get_lower_and_upper_limits(alert_written => 1,
															  values => [2,3,4,5,6,7,8,9,10,11,12,14,15,17,18,19,20],
															  'pred_intervals' => \@pred_int);
is_deeply ($low1,[10,6,3],'get_lower_and_upper_limits 1a');
is_deeply ($up1,[10,15,19],'get_lower_and_upper_limits 1b');

($low1,$up1,$alert)= tool::npc::get_lower_and_upper_limits(alert_written => 1,
															  values => [2,3,4,5,6,7,8,9,10,20],
															  'pred_intervals' => \@pred_int);
is_deeply ($low1,[7,4,2],'get_lower_and_upper_limits 2a');
is_deeply ($up1,[6,9,20],'get_lower_and_upper_limits 2b');

my ($low1,$up1,$low2,$high2)= tool::npc::get_npc_indices('ci' => $c_i,
														 'no_sim' => 17,
														 'pred_intervals' => \@pred_int);

# 0 50 90
is_deeply ($low1,[8,4,1],'get_npc_indices lower_index 2');
is_deeply ($up1,[8,12,15],'get_npc_indices upper_index 2');

($low1,$up1,$low2,$high2)= tool::npc::get_npc_indices('ci' => $c_i,
														 'no_sim' => 10,
														 'pred_intervals' => \@pred_int);

# 0 50 90
is_deeply ($low1,[5,2,0],'get_npc_indices lower_index 3');
is_deeply ($up1,[4,7,9],'get_npc_indices upper_index 3');

#array over predint, one 0 per obs
my $realposanswer = [[-1,-1,1],[-1,-1,0],[0,0,0]];
my $realposanswer2 = [[-1,-1,-1],[-1,-1,-1],[-1,-1,-1]];
my $realposanswer3 = [[1,-99],[0,-99],[0,-99]]; #censored

#pi            0   50 95
#lowerlimit    11  6  2
#upperlimit    10  15 19
#sum warnings obs + all sim, (0)x 
#count max is 3 for all, uncensored obs
#lower_count[predint][real and obs] number below lowerlimit over observation (3x21 ints)
# lowerlim obs2  obs sim1 2 3 4 5 6 7                     realperc   percarr                     realperc2 
#    11     3     2   3  3 3 3 3 3 3 3 3 3 3 0000         66         (100)x10 (0)x10 no warn      100      warn 
#    6      3     2   3  3 3 3 3 00000000                 66         (100)x5 (0)x15  no           100      warn
#    2      3     0   3  0 0 0000000000000                 0         (100)x1 (0)x19  no           100      warn



#upper_count[predint][real and obs] number above upperlimit
# upperlim obs2 obs sim1 2 3 4 5 6 7                 20        realperc                       realperc2
#    10     0     1                 000 3 3 3 3 3 3 3 3 3 3      (100)x10 (0)x10      no       0           no 
#    15     0     0                  00000  3  3  3 3  3          100x5    0 x 15              0           no
#    19     0     0               0000000000000000000  3           100x1   0 x 19              0           no

#sumwarnigns
# non_zeros=20
my $resvalanswer = [[2,66.6666666666667,' ',0,100,1,33.3333333333333,' ',0,100],
					[2,66.6666666666667,' ',0,100,0,0,' ',0,100],
					[0,0,' ',0,100,0,0,' ',0,100]];

my $resvalanswer2 = [[3,100,' ',0,100,0,0,' ',0,100],
					[3,100,' ',0,100,0,0,' ',0,100],
					[3,100,' ',0,100,0,0,' ',0,100]];

my $statswarnanswer = [0,0,0,0,0,0];

my $statswarnanswer2 = [0,0,0,0,0,0];

my ($result_values,$realpos,$stats_warnings,$alert) = tool::npc::subset_npc_analyze(strata_index => 0,
													   pred_intervals => \@pred_int,
													   censored => 0,
													   ci  => $c_i,
													   no_sim => $no_sim,
													   stratified_data => $strat_data,
													   predcorr  => 0,
													   npc_alert_written => 1);


is($alert,1,'subset_npc_analyze alert');
is_deeply($realpos,$realposanswer,'subset_npc_analyze real positions');
is_deeply($result_values,$resvalanswer,'subset_npc_analyze resultvalues');
is_deeply($stats_warnings,$statswarnanswer,'subset_npc_analyze statswarn');

($result_values,$realpos,$stats_warnings,$alert) = tool::npc::subset_npc_analyze(strata_index => 0,
													   pred_intervals => \@pred_int,
													   censored => 0,
													   ci  => $c_i,
													   no_sim => $no_sim,
													   stratified_data => $strat_data2,
													   predcorr  => 0,
													   npc_alert_written => 1);


is($alert,1,'subset_npc_analyze alert');
is_deeply($realpos,$realposanswer2,'subset_npc_analyze real positions 2');
is_deeply($result_values,$resvalanswer2,'subset_npc_analyze resultvalues 2');
is_deeply($stats_warnings,$statswarnanswer2,'subset_npc_analyze statswarn 2');

($result_values,$realpos,$stats_warnings,$alert) = tool::npc::subset_npc_analyze(strata_index => 0,
													   pred_intervals => \@pred_int,
													   censored => 1,
													   ci  => $c_i,
													   no_sim => $no_sim,
													   stratified_data => $strat_data3,
													   censor_stratified_data  => $cens_data3,
													   predcorr  => 0,
													   npc_alert_written => 0);

my $resvalanswer3 = [[0,0,' ',0,100,1,100,' ',0,100],
					[0,0,' ',0,100,0,0,' ',0,100],
					[0,0,' ',0,50,0,0,' ',0,50]];

is($alert,0,'subset_npc_analyze alert');
is_deeply($realpos,$realposanswer3,'subset_npc_analyze real positions 3');
is_deeply($result_values,$resvalanswer3,'subset_npc_analyze resultvalues 3');
is_deeply($stats_warnings,$statswarnanswer2,'subset_npc_analyze statswarn 3');


#use two bins three strata, first stratum is refstrat
my $no_sim=20;
my $ref_n_bins=2;
my $reference_mean_limit_singlesim;
for (my $i=0; $i<$ref_n_bins; $i++){
	@{$reference_mean_limit_singlesim->[$i]} = (0) x ($no_sim);
}

my $perc_limit=['mean','delta-mean',50,25,75,10,90];
my $limit_singlesim;
for (my $i=0; $i<scalar(@{$perc_limit}); $i++){
	@{$limit_singlesim->[$i]} = (0) x ($no_sim);
}

my @merged_censored_sim=(
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20
);
my $censcount= tool::npc::compute_PI_each_simset(
	limit_singlesim => $limit_singlesim,#{ isa => 'Ref', optional => 0 },
	merged_uncensored_simvalues => [],#{ isa => 'Maybe[Ref]', optional => 0},
	merged_censor_values_sim => [],#{ isa => 'Maybe[Ref]', optional => 0 },
	merged_censored_sim => \@merged_censored_sim,#{ isa => 'Maybe[Ref]', optional => 0 },
	predcorr => 0,
	censored => 0,
	perc_limit => $perc_limit,
	reference_mean_limit_singlesim => $reference_mean_limit_singlesim,#{ isa => 'Maybe[Ref]', optional => 0 },
	no_sim => $no_sim,
	max_bin_observations => 10,
	meantext => 'mean',
	deltameantext => 'delta-mean',
	strat_ind => 0,
	bin_index => 0
	);



my $ans_limit_singlesim=[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						 [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  #delta-mean for reference
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]];

my $ans_limit_singlesim2=[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20], #delta-mean from 0
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
						 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]];
my $ans_reference = [[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
					 [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]];
is_deeply($limit_singlesim,$ans_limit_singlesim,'compute_PI_eachs_simset singlesim 1');
is_deeply($reference_mean_limit_singlesim,$ans_reference,'compute_PI_eachs_simset reference 1');
is($censcount,20,'compute_PI_each_simset count 1');

#then redo with strat_ind 1 so not modify reference_mean
$censcount= tool::npc::compute_PI_each_simset(
	limit_singlesim => $limit_singlesim,
	merged_uncensored_simvalues => [],
	merged_censor_values_sim => [],
	merged_censored_sim => \@merged_censored_sim,
	predcorr => 0,
	censored => 0,
	perc_limit => $perc_limit,
	reference_mean_limit_singlesim => $reference_mean_limit_singlesim,
	no_sim => $no_sim,
	max_bin_observations => 10,
	meantext => 'mean',
	deltameantext => 'delta-mean',
	strat_ind => 1,
	bin_index => 1
	);
is_deeply($limit_singlesim,$ans_limit_singlesim2,'compute_PI_eachs_simset singlesim 2');
is_deeply($reference_mean_limit_singlesim,$ans_reference,'compute_PI_eachs_simset reference 2');
is($censcount,20,'compute_PI_each_simset count 2');

done_testing();
