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
use tool::scm::config_file;
use tool::scm;
use common_options;

our $tempdir = create_test_dir('unit_scm');
our $dir = "$tempdir/scm_test";
our $scm_file_dir = $includes::testfiledir . '/scm';
our $file_dir = $includes::testfiledir;

use File::Spec;
open STDERR, '>', File::Spec->devnull();		# Silence STDERR

sub get_stats
{
	open( STAT, '<'."$dir/covariate_statistics.txt" );
	my $tmp;
	for ( <STAT> ) {
		$tmp = $tmp.$_;
	}
	close( STAT );
	my $VAR1;
	eval( $tmp );
	return $VAR1;
}

my $hash1_answer = {
          'APGR' => {
                      'min' => 1,
                      'max' => 10,
                      'mean' => '6.42',
                      'median' => '7.00'
                    },
          'CVD1' => {
                      'min' => 0,
                      'max' => 1,
                      'median' => '1'
                    },
          'WGT' => {
                     'min' => '0.6',
                     'max' => '3.6',
                     'mean' => '1.53',
                     'median' => '1.30'
                   },
          'CVD3' => {
                      'min' => 0,
                      'max' => 1,
                      'median' => '1'
                    },
          'CV3' => {
                     'min' => '0.30240083',
                     'max' => '99.29394117',
                     'mean' => '48.42',
                     'median' => '47.26'
                   },
          'CVD2' => {
                      'min' => 0,
                      'max' => 1,
                      'median' => '1'
                    },
          'CV2' => {
                     'min' => '0.58306634',
                     'max' => '97.66991011',
                     'mean' => '47.85',
                     'median' => '49.53'
                   },
          'CV1' => {
                     'min' => '4.063810715',
                     'max' => '99.56096983',
                     'mean' => '49.42',
                     'median' => '46.67'
                   }
        };


my $file = file -> new( name => 'config_nostep.scm', path => $scm_file_dir );
my $config_file = 'tool::scm::config_file' -> new ( file => $file );

my $models_array = [ model -> new ( filename => $scm_file_dir.'/pheno_with_cov.mod')] ;

my  $scm = tool::scm->new(nmfe => 1,
						  models	=> $models_array,
						  directory => $dir,
						  lst_file => $scm_file_dir.'/pheno_with_cov.lst',
						  config_file => $config_file,
						  both_directions => 0);


my $h1 = get_stats();



foreach my $key1 (keys %{$hash1_answer}) {
	foreach my $key2 (keys %{$hash1_answer->{$key1}}) {
		cmp_ok($h1->{$key1}->{$key2},'eq',$hash1_answer->{$key1}->{$key2},"$key1 $key2");
	}
}

rmtree([$dir]);

$file = file->new( name => 'config_nostep.scm', path => $scm_file_dir );
$config_file = 'tool::scm::config_file' -> new ( file => $file );

$models_array = [ model -> new ( filename  => $scm_file_dir.'/pheno_missing.mod') ] ;

my %options;
$options{'nmfe'}=1;
$options{'directory'}=$dir;
common_options::setup( \%options, 'scm' ); 

$scm = tool::scm ->  new ( nmfe =>1,
			       models	=> $models_array,
			       directory => $dir,
			       lst_file => $scm_file_dir.'/pheno_with_cov.lst',
			       config_file => $config_file,
			       both_directions => 0);


$h1 = get_stats(); #use same dir name (global $dir)

#use same answers, pattern of missing data should not affect stats in this case
$hash1_answer->{'CVD2'}->{'median'}=0;
$hash1_answer->{'APGR'}->{'mean'}='6.10';
$hash1_answer->{'APGR'}->{'median'}='6.00';
foreach my $key1 (keys %{$hash1_answer}) {
	foreach my $key2 (keys %{$hash1_answer->{$key1}}) {
		cmp_ok($h1->{$key1}->{$key2},'eq',$hash1_answer->{$key1}->{$key2},"$key1 $key2");
	}
}

rmtree([$dir]);

$file = file -> new( name => 'config_nostep.scm', path => $scm_file_dir );
$config_file = 'tool::scm::config_file' -> new ( file => $file );

$models_array = [ model -> new ( filename           => $scm_file_dir.'/pheno_missing_9999.mod',
								 missing_data_token => 9999) ] ;

$scm = tool::scm->new(nmfe => 1,
						   models	=> $models_array,
						   missing_data_token => 9999,
						   directory => $dir,
						   lst_file => $scm_file_dir.'/pheno_with_cov.lst',
						   config_file => $config_file,
						   both_directions => 0);


$h1 = get_stats(); #use same dir name (global $dir)

#use same answers, pattern of missing data should not affect stats in this case
$hash1_answer->{'CVD2'}->{'median'}=0;
$hash1_answer->{'APGR'}->{'mean'}='6.10';
$hash1_answer->{'APGR'}->{'median'}='6.00';
foreach my $key1 (keys %{$hash1_answer}) {
	foreach my $key2 (keys %{$hash1_answer->{$key1}}) {
		cmp_ok($h1->{$key1}->{$key2},'eq',$hash1_answer->{$key1}->{$key2},"$key1 $key2");
	}
}

remove_test_dir($tempdir);



my %results;

$results{'min'} = -4.29; #'WAZ min');
$results{'median'} = -1.77; #'WAZ median'
$results{'max'}=0.6; #,'WAZ max');

my $bounds = {};
my $inits = [];
my $global_init=0.001;
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
											max => $results{'max'}, 
											min => $results{'min'}, 
											median => $results{'median'},
											type => 'linear',
											ntheta => 1,
											inits => $inits,
											global_init => $global_init,
											linearize => 0,
											sum_covariates => 0);

is_deeply($bounds->{'lower'},[sprintf("%6.3f",-1/2.37)],'lower linear bound neg median');
is_deeply($bounds->{'upper'},[sprintf("%5.3f",1/2.52)],'upper linear bound neg median');
is_deeply($inits,[sprintf("%6.3f",1/2.52)*0.001],'linear inits');

my $bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
											max => 9, 
											min => -1, 
											median => 5,
											type => 'linear',
											inits => $inits,
											global_init => $global_init,
											ntheta => 1,
											linearize => 0,
											sum_covariates => 0);

is_deeply($bounds->{'lower'},[sprintf("%6.3f",-1/4)],'lower linear bound pos median');
is_deeply($bounds->{'upper'},[sprintf("%5.3f",1/6)],'upper linear bound pos median');
is_deeply($inits,[sprintf("%6.3f",1/6)*0.001],'linear inits');


$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
								 max => $results{'max'}, 
								 min => $results{'min'}, 
								 median => $results{'median'},
								 type => 'linear',
											inits => $inits,
											global_init => $global_init,
									  ntheta => 1,
									  linearize => 0,
								 sum_covariates => 1);


is_deeply($bounds->{'lower'},[-20],'lower linear bound sumcov 2');
is_deeply($bounds->{'upper'},[20],'upper linear bound sumcov 2');
is_deeply($inits,[0.02],'linear inits');

$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
									  max => $results{'max'}, 
									  min => $results{'min'}, 
									  median => $results{'median'},
									  type => 'hockey-stick',
											inits => $inits,
											global_init => $global_init,
									  ntheta => 2,
									  linearize => 0,
									  sum_covariates => 1);


is_deeply($bounds->{'lower'},[-20,-20],'lower linear bound sumcov 2');
is_deeply($bounds->{'upper'},[20,20],'upper linear bound sumcov 2');
is_deeply($inits,[0.02,0.02],'hockey inits');

$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
									  max => $results{'median'}, 
									  min => $results{'median'}, 
											inits => $inits,
											global_init => $global_init,
									  median => $results{'median'},
									  type => 'linear',
									  ntheta => 1,
									  linearize => 0,
									  sum_covariates => 0);

is_deeply($bounds->{'lower'},[-100000],'lower linear bound div 0 3');
is_deeply($bounds->{'upper'},[100000],'upper linear bound div 0 3');
is_deeply($inits,[0.1],'linear inits');

tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
									  max => $results{'max'}, 
									  min => $results{'min'}, 
									  median => $results{'median'},
											inits => $inits,
											global_init => $global_init,
									  type => 'linear',
									  ntheta => 1,
									  linearize => 0,
									  sum_covariates => 0);

is_deeply($bounds->{'lower'},[-100000],'lower linear bound predefined 4');
is_deeply($bounds->{'upper'},[100000],'upper linear bound predefined 4');
is_deeply($inits,[0.1],'linear inits predefined 4');

$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
									  max => $results{'max'}, 
									  min => $results{'min'}, 
									  median => $results{'median'},
											inits => $inits,
											global_init => $global_init,
									  type => 'power',
									  ntheta => 1,
									  linearize => 0,
									  sum_covariates => 0);

is_deeply($bounds->{'lower'},[-100000],'lower bound neg median power');
is_deeply($bounds->{'upper'},[100000],'upper bound neg median power');
is_deeply($inits,[0.001],'power inits');

$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
									  max => $results{'max'}, 
									  min => $results{'min'}, 
									  median => $results{'median'},
									  type => 'exponential',
									  ntheta => 1,
											inits => $inits,
											global_init => $global_init,
									  linearize => 0,
									  sum_covariates => 0);

is_deeply($bounds->{'lower'},[-100000],'lower bound neg median exponential');
is_deeply($bounds->{'upper'},[100000],'upper bound neg median exponential');
is_deeply($inits,[0.001],'exponential inits');


#print '10000001.4352 :'.sprintf("%.3f",10000001.4352)."\n";
#print '1.4352 :'.sprintf("%.3f",1.4352)."\n";
#print '-1.4352 :'.sprintf("%.3f",-1.4352)."\n";
#print '1.43 :'.sprintf("%.3f",1.43)."\n";

my $stats1 = {'have_missing_data' => 1,
			  max => 12.24,
			  min => 3.21,
			  median => 8.56};
my $stats2 = {'have_missing_data' => 0,
			  max => 12.24,
			  min => 3.21,
			  median => 8.56};
my $stats3 = {'have_missing_data' => 1,
			  max => 2.24,
			  min => -4.21,
			  median => -1.56};
my $stats4 = {'have_missing_data' => 0,
			  max => 2.24,
			  min => -4.21,
			  median => -1.56};
my $stats5 = {'have_missing_data' => 0,
			  max => -0.24,
			  min => -4.21,
			  median => -1.56};

my $coderef=[];
my ($num,$frac) = tool::scm::get_covariate_code(statistics => $stats1,
												type => 'linear',
												sum_covariates => 0, 
												linearize =>0,
												missing_data_token => '-99',
												parameter => 'PAR',
												covariate => 'COV',
												code => $coderef,
												theta_number =>2);
is($num,3,'theta number 1');
is($frac,undef,'fraction 1');
is_deeply($coderef,["IF(COV.EQ.-99) THEN\n",
					"   PARCOV = 1\n",
					"ELSE\n",
					"   PARCOV = ( 1 + THETA(2)*(COV - 8.56))\n",
					"ENDIF\n"],'get_covariate_code linear 1');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats2,
												   type => 'linear',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,3,'theta number 2');
is_deeply($coderef,["PARCOV = ( 1 + THETA(2)*(COV - 8.56))\n"],'get_covariate_code linear 2');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats3,
												   type => 'linear',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,3,'theta number 3');
is_deeply($coderef,["IF(COV.EQ.-99) THEN\n",
					"   PARCOV = 1\n",
					"ELSE\n",
					"   PARCOV = ( 1 + THETA(2)*(COV + 1.56))\n",
					"ENDIF\n"],'get_covariate_code linear 3');


$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats4,
												   type => 'linear',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,3,'theta number 4');
is_deeply($coderef,["PARCOV = ( 1 + THETA(2)*(COV + 1.56))\n"],'get_covariate_code linear 4');


$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats1,
												   type => 'linear',
												   sum_covariates => 1, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is_deeply($coderef,["IF(COV.EQ.-99) THEN\n",
					"   PARCOV = 0\n",
					"ELSE\n",
					"   PARCOV = ( 0 + THETA(2)*(COV - 8.56))\n",
					"ENDIF\n"],'get_covariate_code linear 5');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats4,
												   type => 'linear',
												   sum_covariates => 1, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is_deeply($coderef,["PARCOV = ( 0 + THETA(2)*(COV + 1.56))\n"],'get_covariate_code linear 6');

#hockey 
$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats1,
												   type => 'hockey-stick',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,4,'theta number 5');
is_deeply($coderef,["IF(COV.LE.8.56) PARCOV = ( 1 + THETA(2)*(COV - 8.56))\n",
					"IF(COV.GT.8.56) PARCOV = ( 1 + THETA(3)*(COV - 8.56))\n",
					"IF(COV.EQ.-99)   PARCOV = 1\n"],
		  'get_covariate_code hockey-stick 1');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats2,
												   type => 'hockey-stick',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,4,'theta number 6');
is_deeply($coderef,["IF(COV.LE.8.56) PARCOV = ( 1 + THETA(2)*(COV - 8.56))\n",
					"IF(COV.GT.8.56) PARCOV = ( 1 + THETA(3)*(COV - 8.56))\n"],
		  'get_covariate_code hockey-stick 2');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats3,
												   type => 'hockey-stick',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,4,'theta number 7');

is_deeply($coderef,["IF(COV.LE.-1.56) PARCOV = ( 1 + THETA(2)*(COV + 1.56))\n",
					"IF(COV.GT.-1.56) PARCOV = ( 1 + THETA(3)*(COV + 1.56))\n",
					"IF(COV.EQ.-99)   PARCOV = 1\n"],
		  'get_covariate_code hockey-stick 3');


                           #  -3 till 0  
#"IF(COV.LE.-4) PARCOV = ( 1 + THETA(2)*(COV + 4))\n",
	                                    #0  6   
#"IF(COV.GT.-4) PARCOV = ( 1 + THETA(3)*(COV + 4))\n",
$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
									  max => 2,
									  min => -7, 
									  median => -4, 
									  type => 'hockey-stick',
												inits => $inits,
											global_init => $global_init,
								  ntheta => 2,
									  linearize => 0,
									  sum_covariates => 0);

is_deeply($bounds->{'lower'},[-100000,sprintf("%6.3f",-1/6)],'lower bound neg median hockeystick');
is_deeply($bounds->{'upper'},[sprintf("%5.3f",1/3),100000],'upper bound neg median hockey-stick');
is_deeply($inits,[0.001*sprintf("%5.3f",1/3),0.001*sprintf("%6.3f",-1/6)],'hockey inits 2');

                                      #  -9 till 0  
#"IF(COV.LE.2) PARCOV = ( 1 + THETA(2)*(COV - 2))\n",
	                                    #0  3   
#"IF(COV.GT.2) PARCOV = ( 1 + THETA(3)*(COV - 2))\n",
$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
									  max => 5,
									  min => -7, 
									  median => 2, 
									  type => 'hockey-stick',
									  ntheta => 2,
											inits => $inits,
											global_init => $global_init,
									  linearize => 0,
									  sum_covariates => 0);

is_deeply($bounds->{'lower'},[-100000,sprintf("%6.3f",-1/3)],'lower bound pos median hockeystick');
is_deeply($bounds->{'upper'},[sprintf("%5.3f",1/9),100000],'upper bound pos median hockey-stick');
is_deeply($inits,[0.001*sprintf("%5.3f",1/9),0.001*sprintf("%6.3f",-1/3)],'hockey inits 3');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats4,
												   type => 'hockey-stick',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,4,'theta number 8');
is_deeply($coderef,["IF(COV.LE.-1.56) PARCOV = ( 1 + THETA(2)*(COV + 1.56))\n",
					"IF(COV.GT.-1.56) PARCOV = ( 1 + THETA(3)*(COV + 1.56))\n"],
		  'get_covariate_code hockey-stick 4');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats1,
												   type => 'hockey-stick',
												   sum_covariates => 1, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is_deeply($coderef,["IF(COV.LE.8.56) PARCOV = ( 0 + THETA(2)*(COV - 8.56))\n",
					"IF(COV.GT.8.56) PARCOV = ( 0 + THETA(3)*(COV - 8.56))\n",
					"IF(COV.EQ.-99)   PARCOV = 0\n"],
		  'get_covariate_code hockey-stick 5');


$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats4,
												   type => 'hockey-stick',
												   sum_covariates => 1, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is_deeply($coderef,["IF(COV.LE.-1.56) PARCOV = ( 0 + THETA(2)*(COV + 1.56))\n",
					"IF(COV.GT.-1.56) PARCOV = ( 0 + THETA(3)*(COV + 1.56))\n"],
		  'get_covariate_code hockey-stick 6');


#exponential

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats1,
												   type => 'exponential',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,3,'theta number 9');
is_deeply($coderef,["IF(COV.EQ.-99) THEN\n".
					"   PARCOV = 1\n".
					"ELSE\n".
					"   PARCOV = EXP(THETA(2)*(COV - 8.56))\n".
					"ENDIF\n"],
		  'get_covariate_code exponential 1');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats2,
												   type => 'exponential',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,3,'theta number 10');
is_deeply($coderef,["   PARCOV = EXP(THETA(2)*(COV - 8.56))\n"], 'get_covariate_code exponential 2');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats3,
												   type => 'exponential',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,3,'theta number 11');
is_deeply($coderef,["IF(COV.EQ.-99) THEN\n".
					"   PARCOV = 1\n".
					"ELSE\n".
					"   PARCOV = EXP(THETA(2)*(COV + 1.56))\n".
					"ENDIF\n"],
		  'get_covariate_code exponential 3');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats4,
												   type => 'exponential',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,3,'theta number 12');
is_deeply($coderef,["   PARCOV = EXP(THETA(2)*(COV + 1.56))\n"],'get_covariate_code exponential 4');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats1,
												   type => 'exponential',
												   sum_covariates => 1, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is_deeply($coderef,["IF(COV.EQ.-99) THEN\n".
					"   PARCOV = 0\n".
					"ELSE\n".
					"   PARCOV = EXP(THETA(2)*(COV - 8.56))\n".
					"ENDIF\n"],
		  'get_covariate_code exponential 5 (warning)');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats4,
												   type => 'exponential',
												   sum_covariates => 1, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);

is_deeply($coderef,["   PARCOV = EXP(THETA(2)*(COV + 1.56))\n"],'get_covariate_code exponential 6 warning ');

#power

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats1,
												   type => 'power',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,3,'theta number 13');
is_deeply($coderef,["IF(COV.EQ.-99) THEN\n".
					"   PARCOV = 1\n".
					"ELSE\n".
					"   PARCOV = ((COV/8.56)**THETA(2))\n".
					"ENDIF\n"],
		  'get_covariate_code power 1');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats2,
												   type => 'power',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($num,3,'theta number 14');
is_deeply($coderef,["   PARCOV = ((COV/8.56)**THETA(2))\n"], 'get_covariate_code power 2');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats5,
												   type => 'power',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												code => $coderef,
												theta_number =>2);

is_deeply($coderef,["   PARCOV = ((-COV/1.56)**THETA(2))\n"], 'get_covariate_code power 5');

#categorical

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => {'min' => -3,'median' => 1, 'max' => 5,
															   'have_missing_data' => 1,
															   'factors' => {
																   '2.0000E+02' => 3,
																   '1.5000E+02' => 33,
																   '3.0000E+02' => 9,
																   '2.2500E+02' => 25,
																   '-99' => 20,
															   }},
												type => 'categorical',
												sum_covariates => 0, 
												linearize =>0,
												missing_data_token => '-99',
												parameter => 'PAR',
												covariate => 'COV',
												code => $coderef,
												theta_number =>2);
is($num,5,'theta number 14');
is_deeply($coderef,["IF(COV.EQ.1.5000E+02) PARCOV = 1  ; Most common\n",
					"IF(COV.EQ.2.2500E+02) PARCOV = ( 1 + THETA(2))\n",
					"IF(COV.EQ.-99) PARCOV = 1  ; Missing data\n",
					"IF(COV.EQ.3.0000E+02) PARCOV = ( 1 + THETA(3))\n",
					"IF(COV.EQ.2.0000E+02) PARCOV = ( 1 + THETA(4))\n",
		  ], 
		  'get_covariate_code categorical 1 with missing');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => {'min' => -3,'median' => 1, 'max' => 5,
															   'have_missing_data' => 0,
															   'factors' => {
																   '2.0000E+02' => 3,
																   '1.5000E+02' => 33,
																   '3.0000E+02' => 9,
																   '2.2500E+02' => 25
															   }},
												type => 'categorical',
												sum_covariates => 0, 
												linearize =>0,
												missing_data_token => '-99',
												parameter => 'PAR',
												covariate => 'COV',
												code => $coderef,
												theta_number =>2);
is($num,5,'theta number 15');
is_deeply($coderef,["IF(COV.EQ.1.5000E+02) PARCOV = 1  ; Most common\n",
					"IF(COV.EQ.2.2500E+02) PARCOV = ( 1 + THETA(2))\n",
					"IF(COV.EQ.3.0000E+02) PARCOV = ( 1 + THETA(3))\n",
					"IF(COV.EQ.2.0000E+02) PARCOV = ( 1 + THETA(4))\n",
		  ], 
		  'get_covariate_code categorical 2 without missing');


$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
											max => 2,
											min => -7, 
											median => -4, 
											type => 'categorical',
											ntheta => 2,
											linearize => 0,
											global_init => $global_init,
											inits => $inits,
											sum_covariates => 0);

is_deeply($bounds->{'lower'},[-1,-1],'lower bound categorical');
is_deeply($bounds->{'upper'},[5,5],'upper bound categorical');
is_deeply($inits,[-0.001,-0.001],'categorical inits 1');


$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => {'min' => -3,'median' => 1, 'max' => 5,
															   'have_missing_data' => 1,
															   'factors' => {
																   '3.0000E+02' => 9,
																   '2.2500E+02' => 25,
																   '-99' => 20,
															   }},
												type => 'categorical',
												sum_covariates => 0, 
												linearize =>1,
												missing_data_token => '-99',
												parameter => 'PAR',
												covariate => 'COV',
												code => $coderef,
												theta_number =>2);
is($num,3,'theta number 14');
is($frac,0.735294,'fraction linearize');
is_deeply($coderef,[
			  "PARCOV_COMMON=0  ",
			  "; Frequency of most common case is 25/34=0.735294",
			  "IF(COV.EQ.2.2500E+02) PARCOV_COMMON=1; Most common case, indicator variable is 1",
			  "IF(COV.EQ.3.0000E+02) PARCOV_COMMON=0",
			  "PARCOV = (1 + THETA(2)*(0.735294-PARCOV_COMMON)) \n",
			  "IF(COV.EQ.-99) PARCOV = 1  ; Missing data\n"
		  ], 'get_covariate_code categorical 3 with missing linearize');

$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
									  max => 2,
									  min => -7, 
									  median => -4, 
									  type => 'categorical',
											inits => $inits,
											global_init => $global_init,
									  ntheta => 1,
									  linearize => 1,
									  fraction => 0.75,
									  sum_covariates => 0);

is_deeply($bounds->{'lower'},[sprintf("%5.3f",-1/0.75)],'lower bound categorical linearize');
is_deeply($bounds->{'upper'},[sprintf("%5.3f",4.000)],'upper bound categorical linearize');
is_deeply($inits,[0.001*sprintf("%5.3f",-1/0.75)],'categorical linearize inits 2');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => {'min' => -3,'median' => 1, 'max' => 5,
															   'have_missing_data' => 0,
															   'factors' => {
																   '3.0000E+02' => 9,
																   '2.2500E+02' => 25,
															   }},
												type => 'categorical',
												sum_covariates => 0, 
												linearize =>1,
												missing_data_token => '-99',
												parameter => 'PAR',
												covariate => 'COV',
												code => $coderef,
												theta_number =>2);
is($num,3,'theta number 15');
is($frac,0.735294,'linearize fraction 2');
is_deeply($coderef,[
			  "; Frequency of most common case is 25/34=0.735294",
			  "IF(COV.EQ.2.2500E+02) PARCOV_COMMON=1; Most common case, indicator variable is 1",
			  "IF(COV.EQ.3.0000E+02) PARCOV_COMMON=0",
			  "PARCOV = (1 + THETA(2)*(0.735294-PARCOV_COMMON)) \n",
		  ], 'get_covariate_code categorical 4 no missing linearize');


$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => {'min' => -3,'median' => 1, 'max' => 5,
															   'have_missing_data' => 1,
															   'factors' => {
																   '2.0000E+02' => 3,
																   '1.5000E+02' => 33,
																   '3.0000E+02' => 9,
																   '2.2500E+02' => 25,
																   '-99' => 20,
															   }},
												type => 'categorical',
												sum_covariates => 1, 
												linearize =>0,
												missing_data_token => '-99',
												parameter => 'PAR',
												covariate => 'COV',
												code => $coderef,
												theta_number =>2);
is($num,5,'theta number 16');
is_deeply($coderef,["IF(COV.EQ.1.5000E+02) PARCOV = 0  ; Most common\n",
					"IF(COV.EQ.2.2500E+02) PARCOV = ( 0 + THETA(2))\n",
					"IF(COV.EQ.-99) PARCOV = 0  ; Missing data\n",
					"IF(COV.EQ.3.0000E+02) PARCOV = ( 0 + THETA(3))\n",
					"IF(COV.EQ.2.0000E+02) PARCOV = ( 0 + THETA(4))\n",
		  ], 
		  'get_covariate_code categorical 5 with missing');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => {'min' => -3,'median' => 1, 'max' => 5,
															   'have_missing_data' => 0,
															   'factors' => {
																   '2.0000E+02' => 3,
																   '1.5000E+02' => 33,
																   '3.0000E+02' => 9,
																   '2.2500E+02' => 25
															   }},
												type => 'categorical',
												sum_covariates => 1, 
												linearize =>0,
												missing_data_token => '-99',
												parameter => 'PAR',
												covariate => 'COV',
												code => $coderef,
												theta_number =>2);
is($num,5,'theta number 17');
is_deeply($coderef,["IF(COV.EQ.1.5000E+02) PARCOV = 0  ; Most common\n",
					"IF(COV.EQ.2.2500E+02) PARCOV = ( 0 + THETA(2))\n",
					"IF(COV.EQ.3.0000E+02) PARCOV = ( 0 + THETA(3))\n",
					"IF(COV.EQ.2.0000E+02) PARCOV = ( 0 + THETA(4))\n",
		  ], 
		  'get_covariate_code categorical 6 without missing');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => {'min' => -3,'median' => 1, 'max' => 5,
															   'have_missing_data' => 1,
															   'factors' => {
																   '3.0000E+02' => 9,
																   '2.2500E+02' => 25,
																   '-99' => 20,
															   }},
												type => 'categorical',
												sum_covariates => 1, 
												linearize =>1,
												missing_data_token => '-99',
												parameter => 'PAR',
												covariate => 'COV',
												code => $coderef,
												theta_number =>2);
is($num,3,'theta number 18');
is($frac,0.735294,'linearize fraction 3');
is_deeply($coderef,[
			  "PARCOV_COMMON=0  ",
			  "; Frequency of most common case is 25/34=0.735294",
			  "IF(COV.EQ.2.2500E+02) PARCOV_COMMON=1; Most common case, indicator variable is 1",
			  "IF(COV.EQ.3.0000E+02) PARCOV_COMMON=0",
			  "PARCOV = (0 + THETA(2)*(0.735294-PARCOV_COMMON)) \n",
			  "IF(COV.EQ.-99) PARCOV = 0  ; Missing data\n"
		  ], 'get_covariate_code categorical 7 with missing linearize');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => {'min' => -3,'median' => 1, 'max' => 5,
															   'have_missing_data' => 0,
															   'factors' => {
																   '3.0000E+02' => 9,
																   '2.2500E+02' => 25,
															   }},
												type => 'categorical',
												sum_covariates => 1, 
												linearize =>1,
												missing_data_token => '-99',
												parameter => 'PAR',
												covariate => 'COV',
												code => $coderef,
												theta_number =>2);
is($num,3,'theta number 19');

is_deeply($coderef,[
			  "; Frequency of most common case is 25/34=0.735294",
			  "IF(COV.EQ.2.2500E+02) PARCOV_COMMON=1; Most common case, indicator variable is 1",
			  "IF(COV.EQ.3.0000E+02) PARCOV_COMMON=0",
			  "PARCOV = (0 + THETA(2)*(0.735294-PARCOV_COMMON)) \n",
		  ], 'get_covariate_code categorical 8 no missing linearize');


$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats2,
												   type => 'none',
												   sum_covariates => 0, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($frac,undef,'fraction none');
is($num,2,'theta number none');
is_deeply($coderef,["   PARCOV = 1\n"], 'get_covariate_code none');

$coderef=[];
($num,$frac) = tool::scm::get_covariate_code(statistics => $stats2,
												   type => 'none',
												   sum_covariates => 1, 
												   linearize =>0,
												   missing_data_token => '-99',
												   parameter => 'PAR',
												   covariate => 'COV',
												   code => $coderef,
												theta_number =>2);
is($frac,undef,'fraction none');
is($num,2,'theta number none');
is_deeply($coderef,["   PARCOV = 0\n"], 'get_covariate_code none 2');

$bounds = {};
$inits=[];
tool::scm::get_covariate_theta_bounds_inits(bounds => $bounds,
											max => 2,
											min => -7, 
											median => -4, 
											type => 'none',
											inits => $inits,
											global_init => $global_init,
											ntheta => 0,
											linearize => 1,
											fraction => undef,
											sum_covariates => 0);

is_deeply($bounds->{'lower'},[],'lower bound none');
is_deeply($bounds->{'upper'},[],'upper bound none');
is_deeply($inits,[],'inits none');

my ($max,$min,$median,$mean) = tool::scm::format_max_min_median_mean(statistics => {'min' => -3,'median' => 1, 'max' => 5,
																				   'have_missing_data' => 1,
																				   'factors' => {
																					   '3.0000E+02' => 9,
																					   '2.2500E+02' => 25,
																					   '-99' => 20,
																				   }});
															  
is($max,'5.00','format max');
is($min,'-3.00','format min');
is($median,'1.00','format median');
is($mean,'','format mean');

$coderef=['PARCOV = 1 + THETA(1)*median + THETA(2)*COV'];
($num,$frac) = tool::scm::get_covariate_code(statistics => {'min' => -3,'median' => 1.5, 'max' => 5,
															'have_missing_data' => 1,
															'factors' => {
																'3.0000E+02' => 9,
																'2.2500E+02' => 25,
																'-99' => 20,
															}},
											 type => 'user',
											 sum_covariates => 0, 
											 linearize =>0,
											 missing_data_token => '-99',
											 parameter => 'PAR',
											 covariate => 'COV',
											 code => $coderef,
											 theta_number =>2);
is($frac,undef,'fraction user');
is($num,4,'theta number user');
is_deeply($coderef,['PARCOV = 1 + THETA(1)*1.50 + THETA(2)*COV'], 'get_covariate_code user');



done_testing();
