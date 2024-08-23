#!/etc/bin/perl

# sse with uncertainty from rawres

use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

use File::Spec;

#in psn.conf must set output_style = SPLUS, otherwise tests will fail. fix by setting here.



sub is_array{
    my $facit=shift;
    my $func=shift;
    my $label=shift;

    is (scalar(@{$func}),scalar(@{$facit}),"$label, equal length");

    my $min = scalar(@{$func});
    $min = scalar(@{$facit}) if (scalar(@{$facit})< $min);

    for (my $i=0; $i<$min; $i++){
		if ($facit->[$i] eq 'NA'){
			cmp_ok($func->[$i],'eq',$facit->[$i],"$label, index $i");
		}else{
			#facit from R validation script has 7 significant digits
			my $val = sprintf(sprintf("%.7g", $func->[$i]));
			cmp_ok($val,'==',$facit->[$i],"$label, index $i");
		}
    }		
	
}

sub get_stats
{
	my $filename = shift;
	open my $fh, "<", "$filename";

	my @arr = <$fh>;
	close $fh;

	my %hash1;
	my %hash2;
	my $model_num=0;
	foreach my $line (@arr){
		chomp $line;
		if ($line =~ /mean/){
			$model_num++;
		}
		if ($model_num > 0){
			my @row=();
			my @fields = split(',',$line);
			my $name;
			foreach my $fi (@fields){
				$fi =~ s/"//g;
				$fi =~ s/\s//g;
				next if (length($fi)<1);
				if (not defined $name){
					$name = $fi;
				}else{
					if ($model_num==1){
						push(@{$hash1{$name}},$fi);
					}elsif ($model_num==2){
						push(@{$hash2{$name}},$fi);
					}
				}
			}
		}
	}

	return \%hash1,\%hash2;
}


my %hash1_answer;

#these are computed using R scripts 
$hash1_answer{'mean'}=[850.6972,0.005899348,1.254811,0.4766418,0.1560518,0.04421124];
$hash1_answer{'median'}=[886.1627,0.00599329,1.19442,0.335839,0.175554,0.0384054];
$hash1_answer{'sd'}=[70.36831,0.00115304,0.2547368,0.3676744,0.04235623,0.02369889];
$hash1_answer{'max'}=[893.1189,0.00758713,1.59578,1.07335,0.202299,0.0843929];
$hash1_answer{'min'}=[727.193,0.00442164,0.938734,0.141833,0.0965318,0.0230567];
$hash1_answer{'skewness'}=[-2.042407,0.3994587,0.2504938,1.370175,-0.6295755,1.670993];
$hash1_answer{'kurtosis'}=[4.212362,1.152748,-0.7715953,1.698831,-1.035507,3.217267];
$hash1_answer{'rmse'}=[0.00122483,0.1010062,0.3262588,0.04596163,0.01793642];
$hash1_answer{'relative_rmse'}=[37.66996,8.413131,79.28844,37.23602,37.2402];
$hash1_answer{'bias'}=[0.000899348,-0.0451892,0.0766418,0.03605176,0.00421124];
$hash1_answer{'relative_bias'}=[24.16255,-4.223588,19.41001,28.77439,12.81321];
$hash1_answer{'relative_absolute_bias'}=[24.20728,7.132579,53.26867,30.16167,32.02171];


my %hash2_answer;

$hash2_answer{'mean'}=[850.6972,0.00589932,1.254811,0.476638,0.1560516,0.04421048];
$hash2_answer{'median'}=[886.1627,0.00599325,1.19442,0.335836,0.175552,0.038405];
$hash2_answer{'sd'}=[70.36831,0.001153031,0.2547386,0.3676715,0.04235662,0.0236974];
$hash2_answer{'max'}=[893.1189,0.00758708,1.59579,1.07334,0.202301,0.0843893];
$hash2_answer{'min'}=[727.193,0.00442161,0.938734,0.14183,0.0965308,0.0230568];
$hash2_answer{'skewness'}=[-2.042407,0.3994172,0.2505576,1.370156,-0.6295301,1.670922];
$hash2_answer{'kurtosis'}=[4.212362,1.152742,-0.7713754,1.69876,-1.035229,3.217022];
$hash2_answer{'rmse'}=[0.001224817,0.1010078,0.3262553,0.0459619,0.01793496];
$hash2_answer{'relative_rmse'}=[37.66972,8.413217,79.28755,37.23635,37.23736];
$hash2_answer{'bias'}=[0.00089932,-0.0451892,0.076638,0.03605156,0.00421048];
$hash2_answer{'relative_bias'}=[24.16201,-4.223598,19.40893,28.77425,12.81159];
$hash2_answer{'relative_absolute_bias'}=[24.20701,7.132569,53.26867,30.16193,32.02012];


my %hashmox_answer;
$hashmox_answer{'mean'}=[-1148.98,30.17064,18.01546,0.23299,0.039783,0.2464976,0.3155574,0.1294438,0.2151308,0.373764,22.94536,22.94536,1];
$hashmox_answer{'median'}=[-1141.225,30.2658,18.1574,0.234252,0.0442771,0.243868,0.306378,0.132853,0.246455,0.337736,5.8553,5.8553,1];
$hashmox_answer{'sd'}=[50.00287,1.77051,1.077573,0.02299923,0.01885817,0.008384241,0.03021856,0.008454041,0.07599248,0.0807813,33.76875,33.76875,0];
$hashmox_answer{'max'}=[-1086.825,32.8309,19.4243,0.263074,0.0655013,0.254987,0.364952,0.137677,0.285843,0.504608,82.6812,82.6812,1];
$hashmox_answer{'min'}=[-1224.706,28.2713,16.6843,0.198514,0.016473,0.235139,0.289358,0.116258,0.104729,0.301842,4.17741,4.17741,1];
$hashmox_answer{'skewness'}=[-0.6358259,0.7447218,0.05322196,-0.468084,0.1587506,-0.2483907,1.416928,-1.117962,-0.8542906,1.376131,2.119473,2.119473,'NA'];
$hashmox_answer{'kurtosis'}=[1.574383,0.3810282,-1.049555,1.972346,-0.4794161,-1.341873,1.888187,0.7032894,-0.9901115,1.565721,4.534767,4.534767,'NA'];
$hashmox_answer{'rmse'}=[3.104848,5.107227,0.03887816,0.06253472,0.01812201,0.03118592,0.03039924,0.1087321,0.1032551,37.75017,37.75017,0];
$hashmox_answer{'relative_rmse'}=[11.29036,39.28636,19.43908,62.53472,7.879134,10.39531,30.39924,36.24403,34.41838,12583.39,12583.39,0];
$hashmox_answer{'bias'}=[2.67064,5.01546,0.03299,-0.060217,0.0164976,0.0155574,0.0294438,-0.0848692,0.073764,22.64536,22.64536,0];
$hashmox_answer{'relative_bias'}=[9.711418,38.58046,16.495,-60.217,7.17287,5.1858,29.4438,-28.28973,24.588,7548.452,7548.452,0];
$hashmox_answer{'relative_absolute_bias'}=[9.711418,38.58046,16.7922,60.217,7.17287,7.213267,29.4438,28.28973,24.588,7548.452,7548.452,0];

my $model_dir = $includes::testfiledir . "/";

SKIP: {
    eval { require File::Copy::Recursive };
    skip "File::Copy::Recursive not installed" if $@;

	my $dir = create_test_dir('unit_sse');

	File::Copy::Recursive::dircopy($model_dir . 'sse_pheno', $dir);
	chdir($dir);

	my $resultsfile = 'sse_test/sse_results_recompute1.csv';

	unlink($resultsfile);
	my $command = get_command("sse") . " -samples=5 pheno.mod  -recompute=sse_test/raw_results_pheno.csv -silent";

	system $command;

	my ($h1, $h2) = get_stats($resultsfile);

	foreach my $key (keys %hash1_answer) {
		is_array ($hash1_answer{$key}, $h1->{$key}, "sse sim model compare $key");
	}
	foreach my $key (keys %hash2_answer) {
		is_array ($hash2_answer{$key}, $h2->{$key}, "sse alt model compare $key");
	}
	unlink($resultsfile);
	chdir('..');

	rmtree([$dir]);
	File::Copy::Recursive::dircopy($model_dir . 'sse_mox', $dir);
	chdir($dir);

	unlink($resultsfile);
	$command= get_command("sse") . " -samples=5 moxonidine.mod -recompute=sse_test/raw_results_moxonidine.csv -silent";

	system $command;

	($h1, $h2) = get_stats($resultsfile);

	
	foreach my $key (keys %hashmox_answer){
		is_array ($hashmox_answer{$key},$h1->{$key},"sse sim model compare $key");
	}
	foreach my $key (keys %hashmox_answer){
		is_array ($hashmox_answer{$key},$h2->{$key},"sse alt model compare $key");
	}
	unlink($resultsfile);
	
	remove_test_dir($dir);
	
}
done_testing();
