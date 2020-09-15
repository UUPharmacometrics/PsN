#!/etc/bin/perl


use strict;
use warnings;
use FindBin qw($Bin);
use lib "$Bin/../..";
use includes; #file with paths to PsN packages
use Test::More;
use scmlogstep;



my $logstep = scmlogstep->new(directory => 'dummy');

$logstep->parse_header(header => 'MODEL            TEST     BASE OFV     NEW OFV         TEST OFV (DROP)    GOAL     dDF    SIGNIFICANT PVAL');
my @lines=(
"V1SEX-2          PVAL -10610.81224-10679.13889             68.32665  >   6.63490    1        YES!  1.39e-16 ",
"V1QUAT-2         PVAL -10610.81224-10612.05676              1.24453  >   6.63490    1              0.264600 "
	);

my @ans=([1,2,0,0,'-10610.81224-10679.13889',1.39e-16],[0,2,0,0,'-10610.81224-10612.05676',0.264600]);
for (my $i=0; $i< scalar(@lines); $i++){
	my $hashref = $logstep->_parse_candidate(line => $lines[$i]);
	cmp_float($hashref->{'SIGNIFICANT'},$ans[$i]->[0]," significant test $i");
	is($hashref->{'state'},$ans[$i]->[1]," state test $i");
	is($hashref->{'failed'},$ans[$i]->[2]," failed test $i");
	is($hashref->{'local_min'},$ans[$i]->[3]," local min test $i");
	if (defined $ans[$i]->[4]){
		is($hashref->{'merged_ofv'},$ans[$i]->[4]," merged ofv test $i");
	}else{
		is((not defined $hashref->{'merged_ofv'}),1," merged ofv test $i");
	}
	cmp_float($hashref->{'PVAL'},$ans[$i]->[5]," pval test $i merged ofv");

}
$logstep->add_candidate(line => $lines[0],parcov_lookup=> {});
$logstep->add_candidate(line => $lines[1],parcov_lookup=> {});

if ("BASE_MODEL_OFV      -10610.81224 " =~ /^BASE_MODEL_OFV\s*([\-0-9\.]+)/){
	$logstep->base_model_ofv($1);
}
$logstep->split_merged_ofv();
cmp_float($logstep->candidates()->[0]->{'BASE OFV'},-10610.81224,'base ofv 1');
cmp_float($logstep->candidates()->[1]->{'BASE OFV'},-10610.81224,'base ofv 2');
cmp_float($logstep->candidates()->[0]->{'NEW OFV'},-10679.13889,'new ofv 1');
cmp_float($logstep->candidates()->[1]->{'NEW OFV'},-10612.05676,'new ofv 2');



#old
$logstep = scmlogstep->new(directory => 'dummy');

$logstep->parse_header(header => 'MODEL            TEST     BASE OFV     NEW OFV         TEST OFV (DROP)    GOAL     dDF    SIGNIFICANT PVAL');
@lines=(
"PHAAGE-5         PVAL 219210.90878219167.36222             43.54656  >   6.63490    1        YES!  4.14e-11 ",
"PHADIAG-2        PVAL 219210.90878219206.01527              4.89351  >   6.63490    1              0.026958 ",
"PHALBEST-5       PVAL 219210.90878    FAILED                 FAILED  >   6.63490    1                    999"
	);

@ans=([1,5,0,0,'219210.90878219167.36222',4.14e-11],[0,2,0,0,'219210.90878219206.01527',0.026958],[0,5,1,0,undef,999]);
for (my $i=0; $i< scalar(@lines); $i++){
	my $hashref = $logstep->_parse_candidate(line => $lines[$i]);
	cmp_float($hashref->{'SIGNIFICANT'},$ans[$i]->[0]," significant test $i");
	is($hashref->{'state'},$ans[$i]->[1]," state test $i");
	is($hashref->{'failed'},$ans[$i]->[2]," failed test $i");
	is($hashref->{'local_min'},$ans[$i]->[3]," local min test $i");
	if (defined $ans[$i]->[4]){
		is($hashref->{'merged_ofv'},$ans[$i]->[4]," merged ofv test $i");
	}else{
		is((not defined $hashref->{'merged_ofv'}),1," merged ofv test $i");
	}
	cmp_float($hashref->{'PVAL'},$ans[$i]->[5]," pval test $i merged ofv 2");
}
$logstep->add_candidate(line => $lines[0],parcov_lookup=> {});
$logstep->add_candidate(line => $lines[1],parcov_lookup=> {});
$logstep->add_candidate(line => $lines[2],parcov_lookup=> {});

if ("BASE_MODEL_OFV      219210.90878 " =~ /^BASE_MODEL_OFV\s*([\-0-9\.]+)/){
	$logstep->base_model_ofv($1);
}
$logstep->split_merged_ofv();
cmp_float($logstep->candidates()->[0]->{'BASE OFV'},219210.90878,'base ofv 1');
cmp_float($logstep->candidates()->[1]->{'BASE OFV'},219210.90878,'base ofv 2');
cmp_float($logstep->candidates()->[2]->{'BASE OFV'},219210.90878,' base ofv 3');
cmp_float($logstep->candidates()->[0]->{'NEW OFV'},219167.36222,'new ofv 1');
cmp_float($logstep->candidates()->[1]->{'NEW OFV'},219206.01527,'new ofv 2');
is($logstep->candidates()->[2]->{'NEW OFV'},'FAILED','new ofv 3');

$logstep->add_posterior(line=>"PHA     AGE-5   BMI-5   DISDUR-5MENS-2  ");
is_deeply($logstep->posterior_included_relations(),[{'parameter' => 'PHA','covariate' =>'AGE' ,'state' => 5},
													{'parameter' => 'PHA','covariate' => 'BMI','state' => 5},
													{'parameter' => 'PHA','covariate' => 'DISDUR','state' => 5},
													{'parameter' => 'PHA','covariate' => 'MENS','state' => 2}]," logstep add posterior");

$logstep = scmlogstep->new(directory => 'dummy');
$logstep->parse_header(header => 'MODEL            TEST NAME     BASE VAL     NEW VAL         TEST VAL (DROP)    GOAL  SIGNIFICANT');
is($logstep->gof_is_pvalue(),0,'detect ofv');
is($logstep->is_forward(),1,'detect forward');

$logstep->parse_header(header => 'MODEL      TEST NAME    BASE VAL     NEW VAL                                   TEST VAL (DROP)      GOAL (IN)SIGNIFICANT');
is($logstep->gof_is_pvalue(),0,'detect ofv');
is($logstep->is_forward(),0,'detect backward');

@lines =(
	"CLWGT-2        OFV     629.27205   590.06016                                       39.21190  >   2.84000      YES!  ",
	"VCV2-2         OFV     629.27205   626.61183                                        2.66023  >   2.84000",
	"VCVD1-2        OFV     629.27205   625.89605                                        3.37601  >   2.84000      YES!  ",
	"VCV2-1         OFV     584.15462   587.15896                                       -3.00434  >  -7.63000      YES!  ",
	"VWGT-1         OFV     584.15462   669.56839                                      -85.41377  >  -7.63000",
	);

@ans = ([1,2,0,1],[0,2,0,1],[1,2,0,1],[1,1,0,0],[0,1,0,0]);

for (my $i=0; $i< scalar(@lines); $i++){
	my $hashref = $logstep->_parse_candidate(line => $lines[$i]);
	cmp_float($hashref->{'SIGNIFICANT'},$ans[$i]->[0]," significant test $i");
	is($hashref->{'state'},$ans[$i]->[1]," state test $i");
	is($hashref->{'failed'},$ans[$i]->[2]," failed test $i");
	is($hashref->{'local_min'},$ans[$i]->[3]," local min test $i");
}


$logstep->parse_header(header => 'MODEL            TEST     BASE OFV     NEW OFV         TEST OFV (DROP)    GOAL     dDF  INSIGNIFICANT PVAL');
is($logstep->gof_is_pvalue(),1,'detect pval');
is($logstep->is_forward(),0,'detect backward');

$logstep->parse_header(header => 'MODEL            TEST     BASE OFV     NEW OFV         TEST OFV (DROP)    GOAL     dDF    SIGNIFICANT PVAL');
is($logstep->gof_is_pvalue(),1,'detect pval');
is($logstep->is_forward(),1,'detect forward');

# 99 is n-param-diff 0 and delta-ofv 0
# 999 is n-param-diff 0 and local min OR failed (non-ofv) run
# -1 lower ofv with fewer params -> previous base ofv was local min
# 9999 is change in df and local min

@lines =(
	"CLAPGR-2         PVAL    582.62460   571.64659             10.97801  >  16.91900    9              0.277220     \n",
	"CLWGT-2          PVAL    618.00249   582.62460             35.37789  >   3.84150    1        YES!  2.72e-09\n",
	"CLWGT-3          PVAL    582.62460   582.62460              0.00000  >   3.84150    1              0.998580", 
	"CLWGT-2          PVAL    742.05105    FAILED                 FAILED  >   3.84150    1                    999",
	"VWGT-2           PVAL    742.05105    FAILED                 FAILED  >   3.84150    1                    999",
	"CLXY-4          PVAL    618.00249   618.00300             -0.00051  >   3.84150    1                  9999"
	);

@ans = ([0,0.277220,2,0,0,9],[1,2.72e-09,2,0,0,1],[0,0.998580,3,0,0,1],[0,999,2,1,0,1],[0,999,2,1,0,1],[0,9999,4,0,1,1]);

for (my $i=0; $i< scalar(@lines); $i++){
	my $hashref = $logstep->_parse_candidate(line => $lines[$i]);
	cmp_float($hashref->{'SIGNIFICANT'},$ans[$i]->[0]," significant test $i");
	cmp_float($hashref->{'PVAL'},$ans[$i]->[1]," pval test $i");
	is($hashref->{'state'},$ans[$i]->[2]," state test $i");
	is($hashref->{'failed'},$ans[$i]->[3]," failed test $i");
	is($hashref->{'local_min'},$ans[$i]->[4]," local min test $i");
	is($hashref->{'dDF'},$ans[$i]->[5]," dDF test $i");
}

$logstep->add_candidate(line => $lines[0], parcov_lookup => {'CLWGT'=>['CL','WGT'],'CLAPGR'=>['CL','APGR'],'VWGT'=>['V','WGT']});

is($logstep->candidates->[0]->{'parameter'},'CL','new candidate parameter');
is($logstep->candidates->[0]->{'covariate'},'APGR','new candidate covariate');
$logstep->add_candidate(line => $lines[1], parcov_lookup => {'CLWGT'=>['CL','WGT'],'CLXY'=>['CL','XY'],'CLAPGR'=>['CL','APGR'],'VWGT'=>['V','WGT']});
$logstep->add_candidate(line => $lines[4], parcov_lookup => {'CLWGT'=>['CL','WGT'],'CLXY'=>['CL','XY'],'CLAPGR'=>['CL','APGR'],'VWGT'=>['V','WGT']});
$logstep->add_candidate(line => $lines[5], parcov_lookup => {'CLWGT'=>['CL','WGT'],'CLXY'=>['CL','XY'],'CLAPGR'=>['CL','APGR'],'VWGT'=>['V','WGT']});

$logstep->set_chosen(line => 'Parameter-covariate relation chosen in this forward step: CL-WGT-2');
is($logstep->candidates->[0]->{'chosen'},0,'not chosen');
is($logstep->candidates->[1]->{'chosen'},1,'chosen');
is($logstep->candidates->[2]->{'chosen'},0,'not chosen');

my ($have,$stashed,$reduced,$drop);

($have,$stashed,$drop) = $logstep->get_dropped_relations(p_cutoff => 0.5, keep_local_min => 1,keep_failed => 0);
is($have,1,"have failed candidates");
is($stashed,1,"have stashed candidates");
is_deeply($drop,{'V' => {'WGT'=>1} },'relations filter high cutoff');

($have,$stashed,$drop) = $logstep->get_dropped_relations(p_cutoff => 0.5, keep_local_min => 1,keep_failed => 1);
is($have,1,"have failed candidates");
is($stashed,0,"have stashed candidates");
is_deeply($drop,{},'relations filter high cutoff keep fail');


($have,$stashed,$drop) = $logstep->get_dropped_relations(p_cutoff => 0.01, keep_local_min => 0,keep_failed => 0);
is($have,1,"have failed candidates");
is($stashed,3,"have stashed candidates");
is_deeply($drop,{'V' => {'WGT'=>1},'CL' => {'APGR'=>1, 'XY' => 1} },'relations filter low cutoff');

($have,$stashed,$drop) = $logstep->get_dropped_relations(p_cutoff => 0.01, keep_local_min => 0,keep_failed => 1);
is($have,1,"have failed candidates");
is($stashed,2,"have stashed candidates");
is_deeply($drop,{'CL' => {'APGR'=>1, 'XY' => 1} },'relations filter low cutoff keep fail');

done_testing();
