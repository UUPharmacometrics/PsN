#!/etc/bin/perl


use strict;
use warnings;
use Test::More tests=>87;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;
use file;
use tool::scm::config_file;
use tool::scm;
use common_options;

our $tempdir = create_test_dir;
our $dir = "$tempdir/scm_test";
our $scm_file_dir = $includes::testfiledir . '/scm';
our $file_dir = $includes::testfiledir;

use File::Spec;
open STDERR, '>', File::Spec->devnull();		# Silence STDERR

sub is_array
{
	my $func=shift;
	my $facit=shift;
	my $label=shift;

	is (scalar(@{$func}),scalar(@{$facit}),"$label, equal length");

	my $min = scalar(@{$func});
	$min = scalar(@{$facit}) if (scalar(@{$facit})< $min);
	for (my $i = 0; $i < $min; $i++) {
		if ($facit->[$i] eq 'NA') {
			cmp_ok($func->[$i], 'eq', $facit->[$i], "$label, index $i");
		} else {
			cmp_ok($func->[$i], '==', $facit->[$i], "$label, index $i");
		}
	}		

}

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
#    $this -> covariate_statistics($VAR1);

}

my $hash1_answer = {
          'APGR' => {
                      'min' => 1,
                      'max' => 10,
                      'mean' => '  6.42',
                      'median' => '  7.00'
                    },
          'CVD1' => {
                      'min' => 0,
                      'max' => 1,
                      'median' => '1'
                    },
          'WGT' => {
                     'min' => '0.6',
                     'max' => '3.6',
                     'mean' => '  1.53',
                     'median' => '  1.30'
                   },
          'CVD3' => {
                      'min' => 0,
                      'max' => 1,
                      'median' => '1'
                    },
          'CV3' => {
                     'min' => '0.30240083',
                     'max' => '99.29394117',
                     'mean' => ' 48.42',
                     'median' => ' 47.26'
                   },
          'CVD2' => {
                      'min' => 0,
                      'max' => 1,
                      'median' => '1'
                    },
          'CV2' => {
                     'min' => '0.58306634',
                     'max' => '97.66991011',
                     'mean' => ' 47.85',
                     'median' => ' 49.53'
                   },
          'CV1' => {
                     'min' => '4.063810715',
                     'max' => '99.56096983',
                     'mean' => ' 49.42',
                     'median' => ' 46.67'
                   }
        };


my $file = file -> new( name => 'config_nostep.scm', path => $scm_file_dir );
my $config_file = 'tool::scm::config_file' -> new ( file => $file );

my $models_array = [ model -> new ( filename           => $scm_file_dir.'/pheno_with_cov.mod',
				    target             => 'disk' ) ] ;

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

$models_array = [ model -> new ( filename           => $scm_file_dir.'/pheno_missing.mod',
				    target             => 'disk' ) ] ;

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
$hash1_answer->{'APGR'}->{'mean'}='  6.10';
$hash1_answer->{'APGR'}->{'median'}='  6.00';
foreach my $key1 (keys %{$hash1_answer}) {
	foreach my $key2 (keys %{$hash1_answer->{$key1}}) {
		cmp_ok($h1->{$key1}->{$key2},'eq',$hash1_answer->{$key1}->{$key2},"$key1 $key2");
	}
}

rmtree([$dir]);

$file = file -> new( name => 'config_nostep.scm', path => $scm_file_dir );
$config_file = 'tool::scm::config_file' -> new ( file => $file );

$models_array = [ model -> new ( filename           => $scm_file_dir.'/pheno_missing_9999.mod',
								 missing_data_token => 9999,
								 target             => 'disk' ) ] ;

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
$hash1_answer->{'APGR'}->{'mean'}='  6.10';
$hash1_answer->{'APGR'}->{'median'}='  6.00';
foreach my $key1 (keys %{$hash1_answer}) {
	foreach my $key2 (keys %{$hash1_answer->{$key1}}) {
		cmp_ok($h1->{$key1}->{$key2},'eq',$hash1_answer->{$key1}->{$key2},"$key1 $key2");
	}
}

remove_test_dir($tempdir);

done_testing();
