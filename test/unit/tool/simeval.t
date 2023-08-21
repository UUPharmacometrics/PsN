#!/etc/bin/perl


use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use tool::simeval;
use ui;


ui->silent(0);

my $have_cdf = 1 if eval('require Statistics::Distributions'); #enough, now loaded

#SKIP FIXME unless have_cdf
#SKIP: {
#    skip "no stats dist",2 unless $have_cdf; 
#}
our $tempdir = create_test_dir('unit_simeval');
#library npde
#setwd("/tmp/PsN_test_unit_simeval_1/simeval_test_split")
#npiw <- autonpde(namobs="auto_orig_IWRES.tab",namsim="auto_sim_IWRES.tab",iid=1,ix=2,iy=3,imdv=0,boolsave=FALSE)
#npcw <- autonpde(namobs="auto_orig_CWRES.tab",namsim="auto_sim_CWRES.tab",iid=1,ix=2,iy=3,imdv=0,boolsave=FALSE)
#npofv <- autonpde(namobs="auto_orig_IOFV.tab",namsim="auto_sim_IOFV.tab",iid=1,ix=2,iy=3,imdv=0,boolsave=FALSE)
#npebe <- autonpde(namobs="auto_orig_EBE.tab",namsim="auto_sim_EBE.tab",iid=1,ix=2,iy=3,imdv=0,boolsave=FALSE)
#npiw["results"]["res"]$npde
#npcw["results"]["res"]$npde
#npofv["results"]["res"]$npde
#npebe["results"]["res"]$npde

my $facit_npde_iwres = [-0.3853205,0.6744898,-1.9599640,0.6744898,0.6744898,0.3853205,1.2815516,-1.6448536,1.2815516,
						-0.5244005,1.6448536,0.2533471,1.0364334,-1.6448536];

my $facit_npde_iwres_rounded=[];# = [-0.38532,0.67449,-1.96,0.67449,0.67449,0.38532,1.2816,-1.6449,1.2816,-0.5244,1.6449,0.25335,1.0364,-1.6449];
my @facit_outlier_iwres=(0) x 70;
$facit_outlier_iwres[13]=1;
$facit_outlier_iwres[19]=1;
my @facit_outlier_cwres=(0) x 70;
$facit_outlier_cwres[13]=1;
$facit_outlier_cwres[43]=1;

my $bound1= -(Statistics::Distributions::udistr(1/20));
my $bound2= -(Statistics::Distributions::udistr(19/20));

#ignore ties that are handled special way in R npde
foreach my $val (@{$facit_npde_iwres}){
	if ($val == -1.9599640){
		push(@{$facit_npde_iwres_rounded},$bound1);
	}elsif($val == 1.9599640){
		push(@{$facit_npde_iwres_rounded},$bound2);
	}else{
		push(@{$facit_npde_iwres_rounded},tool::simeval::formatnpde($val));
	}
}

my $facit_npde_cwres = [-0.2533471,0.3853205,-1.9599640,1.0364334,0.6744898,0.1256613,0.0000000,-1.6448536,1.9599640,
						-0.3853205,1.6448536,-0.2533471,-0.1256613,-1.6448536];

my $facit_npde_cwres_rounded= [];#[-0.25335,0.38532,-1.96,1.0364,0.67449,0.12566,0,-1.6449,1.96,-0.38532,1.6449,-0.25335,-0.12566,-1.6449];

foreach my $val (@{$facit_npde_cwres}){
	if ($val == -1.9599640){
		push(@{$facit_npde_cwres_rounded},$bound1);
	}elsif($val == 1.9599640){
		push(@{$facit_npde_cwres_rounded},$bound2);
	}else{
		push(@{$facit_npde_cwres_rounded},tool::simeval::formatnpde($val));
	}
}


my $facit_npde_iofv = [-0.8416212,0.6744898,0.2533471,1.2815516,0.2533471];
my $facit_npde_iofv_rounded = []; #[-0.84162,0.67449,0.25335,1.2816,0.25335];
foreach my $val (@{$facit_npde_iofv}){
	push(@{$facit_npde_iofv_rounded},tool::simeval::formatnpde($val));
}

my $facit_npde_ebe = [-0.3853205,-0.1256613,-1.2815516,0.2533471, 1.0364334,0.3853205,-0.6744898,-1.6448536,1.0364334,1.2815516];

my $facit_npde_ebe_rounded = [];
foreach my $val (@{$facit_npde_ebe}){
	push(@{$facit_npde_ebe_rounded},tool::simeval::formatnpde($val));
}

my $prev_succ;
my $prev_subj;

foreach my $type ('split','merged'){
	chdir($tempdir);
	my $indir = $includes::testfiledir . "/simeval/input1$type/";
	my $outputdir = "$tempdir/simeval_test_$type/";
	mkdir($outputdir);
	chdir($outputdir);

	my @tablefiles=($indir.'original_res_table.dta');
	my @eta_files = ($indir.'original.phi');
	for (my $i=1; $i<= 20; $i++){
		push(@tablefiles,$indir."sim_res_table-$i.dta");
		push(@eta_files,$indir."sim-$i.phi");
		last if ($type eq 'merged');
	}
	my ($succ_samp,$subjects) = tool::simeval::simeval_analyze(have_mdv => 1,
															   have_iwres => 1,
															   gls_data_file => 'gls.dta',
															   simeval_all_table_files => \@tablefiles,
															   simeval_all_eta_files => \@eta_files,
															   missing_value => -99,
															   have_CDF => $have_cdf,
															   iiv_eta => ['ETA(1)','ETA(2)'],
															   iov_eta => [],
															   occasions => 0,
															   m1dir => $indir,
															   testing => 1,
		);

	is($succ_samp,20,'successful samples '.$type);
	is($subjects,5,'subjects split '.$type);

	my $filename = 'summary_iofv.csv';
	open my $fh, '<', $filename;
	my @ans=();
	foreach my $line (<$fh>) {
		next if ($line =~ /^ID/);
		chomp $line;
		my @val = split(',',$line);
		push(@ans,$val[5]);
	}
	cmp_float_array(\@ans,$facit_npde_iofv_rounded,'iofv npde comp');
	close $fh;
	
	$filename = 'summary_iwres.csv';
	open $fh, '<', $filename;
	@ans=();
	my $i=0;
	my $j=-1;
	my $tol=4;
	foreach my $line (<$fh>) {
		next if ($line =~ /^ID/);
		chomp $line;
		my @val = split(',',$line);
		$j++;
		unless ($val[1] == 0){
			is(0,$facit_outlier_iwres[$j],"outlier iwres $j");
			next;
		}
		push(@ans,$val[3]);
		cmp_float($val[3],$facit_npde_iwres_rounded->[$i],"iwres item $i ".$val[3].' outlier '.$val[4]." bounds $bound1 $bound2");
		is($val[4],$facit_outlier_iwres[$j],"outlier iwres $j");
		$i++;
	}
	close $fh;
#	cmp_float_array(\@ans,$facit_npde_iwres_rounded,'iwres npde comp');

	$filename = 'summary_cwres.csv';
	open $fh, '<', $filename;
	@ans=();
	$i=0;
	$j=-1;
	
	foreach my $line (<$fh>) {
		next if ($line =~ /^ID/);
		chomp $line;
		my @val = split(',',$line);
		$j++;
		unless ($val[1] == 0){
			is(0,$facit_outlier_cwres[$j],"outlier cwres $j");
			next;
		}
#		$val[3] =~ s/0*$//;
		push(@ans,$val[3]);
		cmp_float($val[3],$facit_npde_cwres_rounded->[$i],"cwres item $i ".$val[3].' outlier '.$val[4]." bounds $bound1 $bound2");
		is($val[4],$facit_outlier_cwres[$j],"outlier cwres $j");
		$i++;
	}
	close $fh;
#	cmp_float_array(\@ans,$facit_npde_cwres_rounded,'cwres npde comp');

	$filename = 'ebe_npde.csv';
	open $fh, '<', $filename;
	@ans=();
	$i=0;

	foreach my $line (<$fh>) {
		next if ($line =~ /^ID/);
		chomp $line;
		my @val = split(',',$line);
#		$val[3] =~ s/0*$//;
		push(@ans,$val[2]);
		push(@ans,$val[3]);
#		cmp_relative($val[3],$facit_npde_cwres_rounded->[$i],$tol,"cwres item $i");
		$i++;
	}
	close $fh;
	cmp_float_array(\@ans,$facit_npde_ebe_rounded,'ebe npde comp');
	
}

remove_test_dir($tempdir);

done_testing();
