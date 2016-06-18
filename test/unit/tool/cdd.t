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
use File::Copy 'cp';
use File::Spec;


my $model_dir = $includes::testfiledir . "/";

my $orig_outobj = output->new(filename => 'run87.lst', 
							  directory => $model_dir.'/cdd/some_cov_fail');

my @cdd_outputs=();
for (my $i=1; $i<=10; $i++){
	push(@cdd_outputs,output->new(filename => 'cdd_'.$i.'.lst',
								 directory => $model_dir.'/cdd/some_cov_fail'));
}

my ($cook_scores,$cov_ratios,$parameter_cook_scores) = 
	tool::cdd::cook_scores_and_cov_ratios(original => $orig_outobj,
										  cdd_outputs => \@cdd_outputs);

#TODO replace all with matlab. Now only 0 and 1
#my $ans_cook=[0,3.170302989709680,
#			  0.65269,1.34665,2.21297,1.66382,3.5193,1.98862,7.93781,59.76814,7.57732];

my @ans_matlab=qw(0 3.170302989709680
   1.688748627536337
   2.124978570238009
   1.411264227801416
   1.101918373487039
   4.163924682508454
   1.208984035596749
   8.601880029180297
  60.973505155513635
   8.373315686778515);
#TODO replace all with matlab. Now only 0 
#rawres my $ans_ratio = [1,1.06352,1,undef,1.43112,2.32917,1,3.17268,undef,undef,1];
cmp_float_array($cook_scores,\@ans_matlab,'cook scores all');
cmp_float_array($cov_ratios,$ans_ratio,'cov ratios');
done_testing();
