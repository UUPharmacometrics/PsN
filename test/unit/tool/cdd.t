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
use tool::cdd;
use output;
use model;
use File::Copy 'cp';
use File::Spec;


my $model_dir = $includes::testfiledir . "/";

my $orig_outobj = output->new(filename => 'run87.lst', 
							  directory => $model_dir.'/cdd/some_cov_fail');

my @cdd_models=();
for (my $i=1; $i<=10; $i++){
	push(@cdd_models,model->new(filename => $model_dir.'/cdd/some_cov_fail/cdd_'.$i.'.mod', 
								ignore_missing_data => 1));
}

my ($cook_scores,$cov_ratios,$parameter_cook_scores, $rel_changes,$bias, $rel_bias) = 
	tool::cdd::cook_scores_and_cov_ratios(original => $orig_outobj,
										  cdd_models => \@cdd_models,
										  bins => 10);



#my $ans_cook=[0,3.170302989709680,
#			  0.65269,1.34665,2.21297,1.66382,3.5193,1.98862,7.93781,59.76814,7.57732];

my @ans_matlab=qw(3.170302989709680
   1.688748627536337
   2.124978570238009
   1.411264227801416
   1.101918373487039
   4.163924682508454
   1.208984035596749
   8.601880029180297
  60.973505155513635
   8.373315686778515);

#rawres ratio  $ans_ratio = [1,1.06352,undef,undef,1.43112,2.32917,undef,3.17268,undef,undef,undef];

my @matlab_ratio=(undef,undef,undef,undef,0.089474167447793,undef,undef,undef,undef,undef);

my @origval = qw(-114.32816869952981 2.79297E+01  3.25318E+00  2.13679E-01  2.08010E-01  3.35031E-01 5.02845E-02 -2.75714E-01  5.02606E+00  1.98562E-03  2.22826E+00  2.98861E+00  1.27533E-02  9.91432E-03  5.46461E-02  2.83978E-02  1.31650E-01  2.16481E+00  7.67732E-03 );
my @cdd2val = qw(-94.739924125155298 2.88971E+01  2.91297E+00  2.17259E-01  2.04975E-01  3.46860E-01 4.59164E-02 -2.61705E-01  5.47917E+00  1.25305E-03 2.30100E+00  3.02348E+00  1.35446E-02  9.28655E-03  5.70912E-02 3.12844E-02  1.26988E-01  2.44864E+00 7.44874E-03    );
my @answer_changes = ();
my @par_cook_answers=();
for (my $i=0; $i< scalar(@origval); $i++){
	push(@answer_changes,100*($cdd2val[$i]- $origval[$i])/$origval[$i]);
	if ($i>0 and $i <10){
		push(@par_cook_answers,(abs($cdd2val[$i]- $origval[$i])/$origval[$i+9]));
	}
}
cmp_float_array($cook_scores,\@ans_matlab,'cook scores all');
cmp_float_array($cov_ratios,\@matlab_ratio,'cov ratios');
cmp_float_array($parameter_cook_scores->[1],\@par_cook_answers,'paramtere cook');
cmp_float_array($rel_changes->[1],\@answer_changes,'relative changes');

cmp_float_array($bias,[0.297629999999955,-0.375965999999998,-0.00278280000000009,0.0147546000000004,-0.0129662999999997,-0.00464805000000001,-0.138554999999999,2.782017,0.004517487],'bias');
cmp_float_array($rel_bias,[1.06563980279042,-11.5568766560718,-1.30232732275988,7.09321667227555,-3.87017917744915,-9.24350445962476,50.2531608841043,55.3518461777217,227.51014796386],'rel bias');

done_testing();
