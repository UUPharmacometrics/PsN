#!/etc/bin/perl


use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use tool::mcmp;

my $hashref = tool::mcmp::get_df_table(max_df=>10,significance_levels=>[5,1,0.1]);

my %df_table;
$df_table{1}=[(3.84,6.63490,10.828)];
$df_table{2}=[(5.99,9.21,13.816)];
$df_table{3}=[(7.81,11.34,16.266)];
$df_table{4}=[(9.49,13.28,18.467)];
$df_table{5}=[(11.07,15.09,20.515)];
$df_table{6}=[(12.59,16.81,22.458)];
$df_table{7}=[(14.07,18.48,24.322)];
$df_table{8}=[(15.51,20.09,26.125)];
$df_table{9}=[(16.9190,21.6660,27.877)];
$df_table{10}=[(18.3070,23.2093,29.588)];

for (my $df=1; $df<=10; $df++){
	for (my $i=0; $i<3; $i++){
		#compare two decimals
		cmp_relative($hashref->{$df}->[$i],$df_table{$df}->[$i],2,"dftable df $df, $i");
	}
}

$hashref = tool::mcmp::get_df_table();
print "\n";
for (my $df=1; $df<=20; $df++){
	print '$df_table{'.$df.'}=[('.join(',',@{$hashref->{$df}}).')];'."\n";
}
print "\n";

done_testing();
