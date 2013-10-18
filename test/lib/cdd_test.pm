package cdd_test;

use Data::Dumper;
use strict;
use Test::More;

my @tests = ( 'pheno' );

sub test {
  my $self = shift;
  my %parms = @_;
  my $verbose          = $parms{'verbose'};
  my $debug            = $parms{'debug'};
  my $debug_package    = $parms{'debug_package'};
  my $debug_subroutine = $parms{'debug_subroutine'};
  my $directory        = $parms{'directory'};
  my $threads          = $parms{'threads'};

  foreach my $test( @tests ){
    cdd_test -> $test( verbose          => $verbose,
			     debug            => $debug,
			     debug_package    => $debug_package,
			     debug_subroutine => $debug_subroutine,
			     directory        => $directory,
			     threads          => $threads );
  }
}

sub pheno {
  my $self = shift;
  my %parms = @_;
  my $verbose          = $parms{'verbose'};
  my $debug            = $parms{'debug'};
  my $debug_package    = $parms{'debug_package'};
  my $debug_subroutine = $parms{'debug_subroutine'};
  my $directory        = $parms{'directory'};
  my $threads          = $parms{'threads'};
  my $bin = "$directory/../bin/cdd.pl -directory=$directory/cdd_dir2 ";
  my $options = "-mod=$directory/files/pheno.mod -case_column=ID -bins=10 -results_file=cdd_results.csv";
  $options = $options." -seed=testing";
  $options = $options." -debug=$debug" if defined $debug;
  $options = $options." -debug_package=$debug_package" if defined $debug_package;
  $options = $options." -debug_subroutine=$debug_subroutine" if defined $debug_subroutine;
  $options = $options." -threads=$threads" if defined $threads;
  system( $bin." ".$options );
  ok( -e "$directory/cdd_results.csv", 'cdd results file exist' );
  open( TESTRES, "$directory/cdd_results.csv" );
  open( ORIGRES, "$directory/files/cdd_results.csv.test" );
  my @test_array = <TESTRES>;
  my @orig_array = <ORIGRES>;
  close( TESTRES );
  close( ORIGRES );
  is_deeply( \@test_array,
	     \@orig_array,
	     'cdd results file has the correct content' );
#  unlink( "$directory/cdd_results.csv" );
#  system( 'rm -rf cdd_dir*' );
}

sub mox {
  my $self = shift;
  my %parms = @_;
  my $verbose          = $parms{'verbose'};
  my $debug            = $parms{'debug'};
  my $debug_package    = $parms{'debug_package'};
  my $debug_subroutine = $parms{'debug_subroutine'};
  my $directory        = $parms{'directory'};
  my $threads          = $parms{'threads'};
  my $bin = "$directory/../bin/cdd.pl";
  my $options = "-mod=$directory/files/mox_basic.mod -case_column=ID -bins=5";
  $options = $options." -seed=testing";
  $options = $options." -debug=$debug" if defined $debug;
  $options = $options." -debug_package=$debug_package" if defined $debug_package;
  $options = $options." -debug_subroutine=$debug_subroutine" if defined $debug_subroutine;
  $options = $options." -threads=$threads" if defined $threads;
  system( $bin." ".$options );
  ok( -e "$directory/cdd_results.csv", 'mox cdd results file exist' );
  open( TESTRES, "$directory/cdd_results.csv" );
  open( ORIGRES, "$directory/files/cdd_results.csv.test.mox" );
  my @test_array = <TESTRES>;
  my @orig_array = <ORIGRES>;
  close( TESTRES );
  close( ORIGRES );
  is_deeply( \@test_array,
	     \@orig_array,
	     'mox cdd results file has the correct content' );
#  unlink( "$directory/cdd_results.csv" );
  system( 'rm -rf cdd_dir*' );
}

sub dof {
  my $self = shift;
  my %parms = @_;
  my $verbose          = $parms{'verbose'};
  my $debug            = $parms{'debug'};
  my $debug_package    = $parms{'debug_package'};
  my $debug_subroutine = $parms{'debug_subroutine'};
  my $directory        = $parms{'directory'};
  my $threads          = $parms{'threads'};
  my $bin = "$directory/../bin/cdd.pl";
  my $options = "-mod=$directory/files/dof.run51.FO.mod ".
    "-case_column=ID -bins=5";
  $options = $options." -seed=testing";
  $options = $options." -debug=$debug" if defined $debug;
  $options = $options." -debug_package=$debug_package" if defined $debug_package;
  $options = $options." -debug_subroutine=$debug_subroutine" if defined $debug_subroutine;
  $options = $options." -threads=$threads" if defined $threads;
  system( $bin." ".$options );
  ok( -e "$directory/cdd_results.csv", 'dof cdd results file exist' );
  open( TESTRES, "$directory/cdd_results.csv" );
  open( ORIGRES, "$directory/files/cdd_results.csv.test.dof" );
  my @test_array = <TESTRES>;
  my @orig_array = <ORIGRES>;
  close( TESTRES );
  close( ORIGRES );
  is_deeply( \@test_array,
	     \@orig_array,
	     'dof cdd results file has the correct content' );
#  unlink( "$directory/cdd_results.csv" );
  system( 'rm -rf cdd_dir*' );
}

sub war {
  my $self = shift;
  my %parms = @_;
  my $verbose          = $parms{'verbose'};
  my $debug            = $parms{'debug'};
  my $debug_package    = $parms{'debug_package'};
  my $debug_subroutine = $parms{'debug_subroutine'};
  my $directory        = $parms{'directory'};
  my $threads          = $parms{'threads'};
  my $bin = "$directory/../bin/cdd.pl";
  my $options = "-mod=$directory/files/war.mod ".
    "-bins=5 -case_column=ID -nm_version=6";
  $options = $options." -seed=testing";
  $options = $options." -debug=$debug" if defined $debug;
  $options = $options." -debug_package=$debug_package" if defined $debug_package;
  $options = $options." -debug_subroutine=$debug_subroutine" if defined $debug_subroutine;
  $options = $options." -threads=$threads" if defined $threads;
  system( $bin." ".$options );
  ok( -e "$directory/cdd_results.csv", 'war cdd results file exist' );
  open( TESTRES, "$directory/cdd_results.csv" );
  open( ORIGRES, "$directory/files/cdd_results.csv.test.war" );
  my @test_array = <TESTRES>;
  my @orig_array = <ORIGRES>;
  close( TESTRES );
  close( ORIGRES );
  is_deeply( \@test_array,
	     \@orig_array,
	     'war cdd results file has the correct content' );
#  unlink( "$directory/cdd_results.csv" );
  system( 'rm -rf cdd_dir*' );
}

1;
