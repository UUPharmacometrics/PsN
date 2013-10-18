package scripts_test;

use strict;
use Test::More tests => 42;
use Config;
use ui;

my @tests = ( 'single_valued_columns',
	      'create_extra_data_model',
	      'llp',
	      'bootstrap',
	      'cdd',
	      'scm'
	      );

sub test {
  my $self = shift;
  my %parms = @_;
  my $verbose          = $parms{'verbose'};
  my $debug            = $parms{'debug'};
  my $debug_package    = $parms{'debug_package'};
  my $debug_subroutine = $parms{'debug_subroutine'};
  my $directory        = $parms{'directory'};
  my $generate_facit   = $parms{'generate_facit'};
  my @opt_tests        = defined $parms{'tests'} ? split( /,/, $parms{'tests'} ) : @tests;

  foreach my $test( @opt_tests ){
    scripts_test -> $test( verbose          => $verbose,
			   debug            => $debug,
			   debug_package    => $debug_package,
			   debug_subroutine => $debug_subroutine,
			   generate_facit   => $generate_facit,
			   directory        => $directory );
  }
}

sub os_format {
  my $str = shift;
  $str =~ s/\/\//\//g;
  if( $Config{osname} eq 'MSWin32' ){
    $str =~ s/\//\\/g;
    $str = "perl $str";
 }
  return $str;
}

sub single_valued_columns {
  my $self = shift;
  my %parms = @_;
  my $verbose          = $parms{'verbose'};
  my $debug            = $parms{'debug'};
  my $debug_package    = $parms{'debug_package'};
  my $debug_subroutine = $parms{'debug_subroutine'};
  my $directory        = $parms{'directory'};
  my $generate_facit   = $parms{'generate_facit'};

  my $bin = "$directory/../bin/single_valued_columns";
  my $options = "-data=$directory/files/utilities/pheno.dta ".
    "-sub=testsub.dta -rem=testrem.dta -do_not=4";

  system( &os_format( $bin . " ". $options ) );

  my $success;
 SKIP: {
   if( $generate_facit ){
     system("cp $directory/files/utilities/testsub.dta $directory/files/utilities/pheno_single.dta" );
     system("cp $directory/files/utilities/testrem.dta $directory/files/utilities/pheno_reduced.dta" );
     skip "Generating facit", 1;
   }
   $success = ok( -e "$directory/files/utilities/testsub.dta", 'single valued column data file existance test' );
   skip "Skipping contents test", 1 unless( $success );
   open( TESTSUB, "$directory/files/utilities/testsub.dta" );
   open( ORIGSUB, "$directory/files/utilities/pheno_single.dta" );
   my @test_array = <TESTSUB>;
   my @orig_array = <ORIGSUB>;
   close( TESTSUB );
   close( ORIGSUB );
   $success = is_deeply( \@test_array,
			 \@orig_array,
			 'single valued column data file content test' );
   skip "Contents test fail, skipping forward", 1 unless( $success );
   $success = ok( -e "$directory/files/utilities/testrem.dta", 'remaining data file existance test' );
   skip "Skipping contents test", 1 unless( $success );
   open( TESTREM, "$directory/files/utilities/testrem.dta" );
   open( ORIGREM, "$directory/files/utilities/pheno_reduced.dta" );
   @test_array = <TESTREM>;
   @orig_array = <ORIGREM>;
   close( TESTREM );
   close( ORIGREM );
   $success = is_deeply( \@test_array,
			 \@orig_array,
			 'remaining data file content test' );
   skip "Contents test fail, skipping forward", 1 unless( $success );
   unlink( "$directory/files/utilities/testsub.dta", "$directory/files/utilities/testrem.dta" );
 }
}

sub create_extra_data_model {
  my $self = shift;
  my %parms = @_;
  my $verbose          = $parms{'verbose'};
  my $debug            = $parms{'debug'};
  my $debug_package    = $parms{'debug_package'};
  my $debug_subroutine = $parms{'debug_subroutine'};
  my $directory        = $parms{'directory'};
  my $generate_facit   = $parms{'generate_facit'};
  my $bin = "$directory/../bin/create_extra_data_model";
  my $options = "-mod=$directory/files/utilities/pheno.mod ".
    "-extra=$directory/files/utilities/pheno_single.dta ".
      "-header='ID,APGR' -new=$directory/files/utilities/testnew.mod";
  system( &os_format( $bin . " ". $options ) );

  my $success;
 SKIP: {
   if( $generate_facit ){
     system("cp $directory/files/utilities/testnew.mod $directory/files/utilities/pheno_extra_data.mod" );
     system("cp $directory/reader0.f $directory/files/utilities/reader_orig.f" );
     system("cp $directory/get_sub0.f $directory/files/utilities/get_sub_orig.f" );
     skip "Generating facit", 1;
   }

   $success = ok( -e "$directory/files/utilities/testnew.mod", 'model file for extra data existance test' );
   skip "Skipping contents test", 1 unless( $success );
   open( TESTSUB, "$directory/files/utilities/testnew.mod" );
   open( ORIGSUB, "$directory/files/utilities/pheno_extra_data.mod" );
   my @test_array = <TESTSUB>;
   my @orig_array = <ORIGSUB>;
   close( TESTSUB );
   close( ORIGSUB );
   $success = is_deeply( \@test_array,
			\@orig_array,
			'model file for extra data content test' );
   skip "Contents test fail, skipping forward", 1 unless( $success );
   $success = ok( -e "$directory/reader0.f", 'reader file existance test' );
   skip "Skipping contents test", 1 unless( $success );
   open( TESTREM, "$directory/reader0.f" );
   open( ORIGREM, "$directory/files/utilities/reader_orig.f" );
   @test_array = <TESTREM>;
   @orig_array = <ORIGREM>;
   close( TESTREM );
   close( ORIGREM );
   $success = is_deeply( \@test_array,
			\@orig_array,
			'reader file content test' );
   skip "Contents test fail, skipping forward", 1 unless( $success );
   $success = ok( -e "$directory/get_sub0.f", 'get_sub file existance test' );
   skip "Skipping contents test", 1 unless( $success );
   open( TESTREM, "$directory/get_sub0.f" );
   open( ORIGREM, "$directory/files/utilities/get_sub_orig.f" );
   @test_array = <TESTREM>;
   @orig_array = <ORIGREM>;
   close( TESTREM );
   close( ORIGREM );
   $success = is_deeply( \@test_array,
			\@orig_array,
			'get_sub file content test' );
   skip "Contents test fail, skipping forward", 1 unless( $success );
   unlink( "$directory/files/utilities/testnew.mod",
	   "$directory/reader0.f",
	   "$directory/get_sub0.f" );
 }
}

# Fixa generiska mapp-namn
sub generic_tester {
  my $self = shift;
  my %parms = @_;
  my $options          = $parms{'options'};
  my $verbose          = $parms{'verbose'};
  my $debug            = $parms{'debug'};
  my $debug_package    = $parms{'debug_package'};
  my $debug_subroutine = $parms{'debug_subroutine'};
  my $directory        = $parms{'directory'};
  my $script           = $parms{'script'};
  my $sub_test         = $parms{'sub_test'};
  my $generate_facit   = $parms{'generate_facit'};

  $options = $options." -debug=$debug" if defined $debug;
  $options = $options." -debug_package=$debug_package" if defined $debug_package;
  $options = $options." -debug_subroutine=$debug_subroutine" if defined $debug_subroutine;

  system( &os_format( $directory . "/../bin/$script -directory=$script".'_dir'. $options ) );

  my $success;
 SKIP: {
   if( $generate_facit ){
     system("cp $directory/".$script."_dir/".$script."_results.csv $directory/files/utilities/" . $script . "_results.csv.".$sub_test );
     system('rm -rf ' . $script .'_dir*');
     skip "Generating facit", 1;
   }

   # Remove any previous (failed) test results.
   system('rm -rf ' . $script .'_dir');

   $success = ok( -e "$directory/".$script."_dir/".$script."_results.csv", "$script results file existance test" );
   skip "Skipping contents test", 1 unless( $success );
   open( TESTRES, "$directory/" . $script ."_dir/".$script."_results.csv" );
   open( ORIGRES, "$directory/files/utilities/" . $script . "_results.csv.".$sub_test );
   my @test_array = <TESTRES>;
   my @orig_array = <ORIGRES>;
   close( TESTRES );
   close( ORIGRES );
   $success = is_deeply( \@test_array,
			 \@orig_array,
			 $script . ' results file content test' );
   if( $success ){
     system( 'rm -rf '.$script.'_dir*' );
   }
 }
  return $success;
}

sub llp {
  my $self = shift;
  my $success;
  
  $success = scripts_test -> generic_tester( @_,
					     script => 'llp',
					     sub_test => 'pheno',
					     options => "files/utilities/pheno.mod ".
				                        "-thetas=1 -omegas=1 -sigmas=1 --rse_sigmas=0.3 -max_it=8".
				                        " -significant_digits=3 -silent --seed=22154" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'llp',
				  sub_test => 'dofetilide',
				  options => "files/utilities/dofetilide.mod ".
				             "-thetas=1 -rse_theta=0.1 -omegas=1 -rse_omegas=0.3 -max_it=8".
				             " -significant_digits=3 -silent --seed=13443 --threads=6" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'llp',
				  sub_test => 'moxonidine',
				  options => "files/utilities/moxonidine.mod ".
				             "-thetas=1 -rse_theta=0.1 -omegas=1 -rse_omegas=0.3 -max_it=8".
				             " -significant_digits=3 -silent --seed=7483723 --threads=6" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'llp',
				  sub_test => 'warfarin',
				  options => "files/utilities/warfarin.mod ".
				             "-thetas=1 -rse_theta=0.1 -omegas=4 -rse_omegas=0.3 -max_it=8".
				             " -significant_digits=3 -silent --seed=935829 --threads=6" );
  return;
}


sub bootstrap {
  my $self = shift;
  my $success;

  $success = scripts_test -> generic_tester( @_,
				  script => 'bootstrap',
				  sub_test => 'pheno',
				  options => "files/utilities/pheno.mod ".
				             "--seed=494574 -silent" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'bootstrap',
				  sub_test => 'dofetilide',
				  options => "files/utilities/dofetilide.mod ".
				             "--seed=2193487 -silent --threads=6" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'bootstrap',
				  sub_test => 'moxonidine',
				  options => "files/utilities/moxonidine.mod ".
				             "--seed=89430284 -silent --threads=6 --strat=VISI" );  

  $success = scripts_test -> generic_tester( @_,
				  script => 'bootstrap',
				  sub_test => 'moxonidine',
				  options => "files/utilities/moxonidine.mod ".
				             "--seed=89430284 -silent --threads=6 --strat=2" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'bootstrap',
				  sub_test => 'warfarin',
				  options => "files/utilities/warfarin.mod ".
				             "-seed=153095 -silent --threads=6" );
  return;
}


sub cdd {
  my $self = shift;
  my $success;

  $success = scripts_test -> generic_tester( @_,
				  script => 'cdd',
				  sub_test => 'pheno',
				  options => "files/utilities/pheno.mod ".
				             "--case_column=1 --seed=408463 -silent" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'cdd',
				  sub_test => 'dofetilide',
				  options => "files/utilities/dofetilide.mod ".
				             "--case_column=2 --seed=87424 -silent --threads=6" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'cdd',
				  sub_test => 'moxonidine',
				  options => "files/utilities/moxonidine.mod ".
				             "--case_column=2 --seed=656816 -silent --threads=6" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'cdd',
				  sub_test => 'warfarin',
				  options => "files/utilities/warfarin.mod ".
				             "--case_column=2 --seed=3225479 -silent --threads=6" );
  return;
}


sub scm {
  my $self = shift;
  my $success;

  $success = scripts_test -> generic_tester( @_,
				  script => 'scm',
				  sub_test => 'pheno',
				  options => "--conf=files/utilities/pheno.scm --seed=12345 -silent" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'scm',
				  sub_test => 'dofetilide',
				  options => "--conf=files/utilities/dofetilide.scm --seed=12345 -silent --threads=6" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'scm',
				  sub_test => 'moxonidine',
				  options => "--conf=files/utilities/moxonidine.scm --seed=12345 -silent --threads=6" );
#  return unless $success;
  $success = scripts_test -> generic_tester( @_,
				  script => 'scm',
				  sub_test => 'warfarin',
				  options => "--conf=files/utilities/warfarin.scm --seed=12345 -silent --threads=6" );
  return;
}

1;
