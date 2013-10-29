#!/etc/bin/perl

# sse with uncertainty from rawres

use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);

#in psn.conf must set output_style = SPLUS, otherwise tests will fail. fix by setting here.

our $dir = 'sse_test';
our $private_test_files = $ENV{HOME}.'/.test_files';
my $path = "$Bin/../../bin/";


sub is_array{
    my $func=shift;
    my $facit=shift;
    my $label=shift;

    is (scalar(@{$func}),scalar(@{$facit}),"$label, equal length");

    my $min = scalar(@{$func});
    $min = scalar(@{$facit}) if (scalar(@{$facit})< $min);
    for (my $i=0; $i<$min; $i++){
	if ($facit->[$i] eq 'NA'){
	    cmp_ok($func->[$i],'eq',$facit->[$i],"$label, index $i");
	}else{
	    cmp_ok($func->[$i],'==',$facit->[$i],"$label, index $i");
	}
    }		
	
}



sub get_stats
{
  open my $fh, "<", "$dir/sse_results.csv";

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

$hash1_answer{'mean'}=[850.69721,0.0059,1.25481,0.47664,0.15605,0.04421];
$hash1_answer{'median'}=[886.1627,0.00599,1.19442,0.33584,0.17555,0.03841];
$hash1_answer{'sd'}=[70.36831,0.00115,0.25474,0.36767,0.04236,0.02370];
$hash1_answer{'max'}=[893.11888,0.00759,1.59578,1.07335,0.20230,0.08439];
$hash1_answer{'min'}=[727.19295,0.00442,0.93873,0.14183,0.09653,0.02306];
$hash1_answer{'skewness'}=[-0.980355361852059,0.19174,0.12024,0.65768,-0.302196258427144,0.80208];
$hash1_answer{'kurtosis'}=[-1.0460220774539,-1.53556034798459,-1.84345525268636,-1.44818697131046,-1.88568106779018,-1.20523729858608];
$hash1_answer{'rmse'}=[0.00122,0.10101,0.32626,0.04596,0.01794];
$hash1_answer{'relative_rmse'}=[37.66996,8.41313,79.28844,37.23602,37.24020];
$hash1_answer{'bias'}=[0.0009,-0.0451892,0.07664,0.03605,0.00421];
$hash1_answer{'relative_bias'}=[24.16255,-4.22358821178821,19.41001,28.77439,12.81321];
$hash1_answer{'relative_absolute_bias'}=[24.16255,-4.22358821178821,19.41001,28.77439,12.81321];

my %hash2_answer;

$hash2_answer{'mean'}=[850.69721,0.0059,1.25481,0.47664,0.15605,0.04421];
$hash2_answer{'median'}=[886.1627,0.00599,1.19442,0.33584,0.17555,0.03840];
$hash2_answer{'sd'}=[70.36831,0.00115,0.25474,0.36767,0.04236,0.02370];
$hash2_answer{'max'}=[893.11888,0.00759,1.59579,1.07334,0.2023,0.08439];
$hash2_answer{'min'}=[727.19295,0.00442,0.93873,0.14183,0.09653,0.02306];
$hash2_answer{'skewness'}=[-0.980355361814824,0.19172,0.12027,0.65768,-0.302174460293086,0.80204];
$hash2_answer{'kurtosis'}=[-1.0460220775012,-1.53556130456128,-1.84342005719353,-1.44819839841088,-1.88563671487153,-1.20527643870814];
$hash2_answer{'rmse'}=[0.00122,0.10101,0.32626,0.04596,0.01793];
$hash2_answer{'relative_rmse'}=[37.66972,8.41322,79.28755,37.23635,37.23736];
$hash2_answer{'bias'}=[0.0009,-0.0451892,0.07664,0.03605,0.00421];
$hash2_answer{'relative_bias'}=[24.16201,-4.22359773559773,19.40893,28.77425,12.81159];
$hash2_answer{'relative_absolute_bias'}=[24.16201,-4.22359773559773,19.40893,28.77425,12.81159];

my%hashmox_answer;
$hashmox_answer{'mean'}=[-1148.97958067097,30.17064,18.01546,0.23299,0.03978,0.24650,0.31556,0.12944,0.21513,0.37376,22.94535,22.94535,1];
$hashmox_answer{'median'}=[-1141.2249648679,30.26580,18.15740,0.23425,0.04428,0.24387,0.30638,0.13285,0.24646,0.33774,5.85530,5.85530,1];
$hashmox_answer{'sd'}=[50.00287,1.77051,1.07757,0.02300,0.01886,0.00838,0.03022,0.00845,0.07599,0.08078,33.76870,33.76870,0];
$hashmox_answer{'max'}=[-1086.82499328701,32.83090,19.42430,0.26307,0.06550,0.25499,0.36495,0.13768,0.28584,0.50461,82.68110,82.68110,1];
$hashmox_answer{'min'}=[-1224.70638959385,28.27130,16.68430,0.19851,0.01647,0.23514,0.28936,0.11626,0.10473,0.30184,4.17741,4.17741,1];
$hashmox_answer{'skewness'}=[-0.305196455055743,0.35747,0.02555,-0.224680316229159,0.07620,-0.119227558227755,0.68013,-0.536621825097605,-0.410059498452492,0.66054,1.01735,1.01735,'NA'];
$hashmox_answer{'kurtosis'}=[-1.46809876241865,-1.65903549264592,-1.88792877240051,-1.40442461660688,-1.79670925403668,-1.93469969899971,-1.4178901307938,-1.60747369374649,-1.87841784407265,-1.4694960897965,-0.994437335497539,-0.994437335497539,'NA'];
$hashmox_answer{'rmse'}=[3.10485,5.10723,0.03888,0.06253,0.01812,0.03119,0.03040,0.10873,0.10326,37.75012,37.75012,0];
$hashmox_answer{'relative_rmse'}=[11.29036,39.28636,19.43908,62.53475,7.87913,10.39531,30.39924,36.24403,34.41844,12583.37478,12583.37478,0];
$hashmox_answer{'bias'}=[2.67064,5.01546,0.03299,-0.06021702,0.01650,0.01556,0.02944,-0.0848692,0.07376,22.64535,22.64535,0];
$hashmox_answer{'relative_bias'}=[9.71142,38.58046,16.49500,-60.21702,7.17287,5.18580,29.44380,-28.2897333333333,24.58807,7548.44933,7548.44933,0];
$hashmox_answer{'relative_absolute_bias'}=[9.71142,38.58046,16.49500,-60.21702,7.17287,5.18580,29.44380,-28.2897333333333,24.58807,7548.44933,7548.44933,0];


my $model_dir = "../test_files";


rmtree([ "./$dir" ]);

my $command= $path."sse -samples=5 $model_dir/pheno.mod -alt=$model_dir/pheno.mod -seed=290805 -rawres=$model_dir/rawres_for_sse.csv  -directory=$dir -offset=1";

system $command;

my ($h1,$h2)=get_stats();

foreach my $key (keys %hash1_answer){
    is_array ($hash1_answer{$key},$h1->{$key},"sse sim model compare $key");
}
foreach my $key (keys %hash2_answer){
    is_array ($hash2_answer{$key},$h2->{$key},"sse alt model compare $key");
}



rmtree([ "./$dir" ]);

$command= $path."sse -samples=5 $private_test_files/moxonidine.mod -alt=$private_test_files/moxonidine.mod -seed=630992 -directory=$dir";

system $command;

($h1,$h2)=get_stats();

foreach my $key (keys %hashmox_answer){
    is_array ($hashmox_answer{$key},$h1->{$key},"sse sim model compare $key");
}
foreach my $key (keys %hashmox_answer){
    is_array ($hashmox_answer{$key},$h2->{$key},"sse alt model compare $key");
}



rmtree([ "./$dir" ]);


done_testing();
