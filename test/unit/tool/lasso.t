#!/etc/bin/perl


use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;
use file;
use tool::lasso;
use common_options;

#our $tempdir = create_test_dir('unit_lasso');
#our $dir = "$tempdir/lasso_test";
our $modeldir = $includes::testfiledir;

#use File::Spec;
#open STDERR, '>', File::Spec->devnull();		# Silence STDERR


#TODO
# data->lasso_calculate_covariate_statistics elsewhere

#setup_covariates
#parse_row
#setup_lasso_model

#my $dataobj = data->new(filename => $includes::testfiledir . '/mox_simulated.csv',
#						idcolumn => 1,
#						missing_data_token => '-99',
#						ignoresign => '@');

#categ 
#SEX 13   1 or 2
#NYHA 14 
#ACE 17
#DIG 18
#DIU 19

#cont 
#AGE 12
#WT 15 
#CLCR 24 

dies_ok { tool::lasso::check_name(parameter => 'CL',covariate=>'APGR',factor => '2',version => 5) } "check name too long v 5";
dies_ok { tool::lasso::check_name(parameter => 'V',covariate=>'HEJSAN',H => 'H',version => 6) } "check name too long v 6";
lives_ok { tool::lasso::check_name(parameter => 'KA',covariate=>'OTHER',version => 7) } "check name long v 7";


my $model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);

my $relations = 'CL:WGT-2,SEX-1,RACE-1,,V:WGT-3-45.2,,KA:WGT-3,APGR-2';

my ($ref1,$breakpoints) = tool::lasso::parse_relations(relations => $relations);
is($breakpoints->{'V:WGT'},45.2,"lasso parse_relations breakpoint");
is($ref1->{'CL'}{'WGT'},2,"lasso parse_relations 1");
is($ref1->{'CL'}{'SEX'},1,"lasso parse_relations 2");
is($ref1->{'CL'}{'RACE'},1,"lasso parse_relations 3");
is($ref1->{'V'}{'WGT'},3,"lasso parse_relations 4");
is($ref1->{'KA'}{'WGT'},3,"lasso parse_relations 5");
is($ref1->{'KA'}{'APGR'},2,"lasso parse_relations 6");
is((scalar(keys %{$ref1})),3,'lasso parse relations count parameters');

is(tool::lasso::factor_string(parameter => 'CL',covariate => 'WT',thetanumber=>5, mean => 3, sd => 2),
   'CLWT = THETA(5)*(WT-3.00000)/2.00000*FACTOR', "factor string 1");


#WGT
my $mean2=1.52542372881356;
my $sd2=0.704564727537013;

#SEX
my $mean1=0.202702702702702;
my $sd1=0.404756978659315; 

#APGR hstick
my $mean=6.42373;
my $sd=2.23764;
my $hmean=1.49153;
my $hsd=1.33420;
my $break = 5.5;
my $nthetas = $model->nthetas;

$nthetas++;
tool::lasso::add_lasso_theta(model => $model,
							 parameter => 'CL',
							 covariate => 'SEX0',
							 thetanumber => $nthetas,
							 mean => $mean1,
							 sd => $sd1,
							 max => 1,
							 min => 0);
is($model->problems->[0]->thetas->[-1]->options->[0]->label,'TH'.$nthetas.' CLSEX0','add_lasso_theta label');
is($model->problems->[0]->thetas->[-1]->options->[0]->init,0.0001,'add_lasso_theta init');
cmp_float($model->problems->[0]->thetas->[-1]->options->[0]->lobnd,-0.50766,'add_lasso_theta lowbnd cat');
cmp_float($model->problems->[0]->thetas->[-1]->options->[0]->upbnd,1.99680,'add_lasso_theta upbnd cat');

$nthetas++;
tool::lasso::add_lasso_theta(model => $model,
							 parameter => 'CL',
							 covariate => 'WGT',
							 thetanumber => $nthetas,
							 mean => $mean2,
							 sd => $sd2,
							 max => 3.6,
							 min => 0.6);
is($model->problems->[0]->thetas->[-1]->options->[0]->label,'TH'.$nthetas.' CLWGT','add_lasso_theta label');
is($model->problems->[0]->thetas->[-1]->options->[0]->init,0.0001,'add_lasso_theta init');
cmp_float($model->problems->[0]->thetas->[-1]->options->[0]->lobnd,-0.33962,'add_lasso_theta lowbnd cont');
cmp_float($model->problems->[0]->thetas->[-1]->options->[0]->upbnd,0.76134,'add_lasso_theta upbnd cont');

$nthetas++;
tool::lasso::add_lasso_theta(model => $model,
							 parameter => 'CL',
							 covariate => 'APGR',
							 thetanumber => $nthetas,
							 mean => $mean,
							 sd => $sd,
							 max => 10,
							 min => 1);
is($model->problems->[0]->thetas->[-1]->options->[0]->label,'TH'.$nthetas.' CLAPGR','add_lasso_theta label');
is($model->problems->[0]->thetas->[-1]->options->[0]->init,0.0001,'add_lasso_theta init');
cmp_float($model->problems->[0]->thetas->[-1]->options->[0]->lobnd,-0.62569,'add_lasso_theta lowbnd hstick lo');
cmp_float($model->problems->[0]->thetas->[-1]->options->[0]->upbnd,0.41256,'add_lasso_theta upbnd hstick lo');

$nthetas++;
tool::lasso::add_lasso_theta(model => $model,
							 parameter => 'CL',
							 covariate => 'HAPGR',
							 thetanumber => $nthetas,
							 mean => $hmean,
							 sd => $hsd,
							 max => (10-$break),
							 min => 0);
is($model->problems->[0]->thetas->[-1]->options->[0]->label,'TH'.$nthetas.' CLHAPGR','add_lasso_theta label');
is($model->problems->[0]->thetas->[-1]->options->[0]->init,0.0001,'add_lasso_theta init');
cmp_float($model->problems->[0]->thetas->[-1]->options->[0]->lobnd,-0.44348,'add_lasso_theta lowbnd hstick hi');
cmp_float($model->problems->[0]->thetas->[-1]->options->[0]->upbnd,0.89452,'add_lasso_theta upbnd hstick hi');


is(tool::lasso::get_abssum(old_thetas => 2,nthetas => 6,adaptive => 0),
   'ABSSUM = ABS(THETA(3))+ABS(THETA(4))+ABS(THETA(5))+ABS(THETA(6))','get_abssum no adaptive');

done_testing();
