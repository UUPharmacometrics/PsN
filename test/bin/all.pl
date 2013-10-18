#!/usr/bin/perl

use FindBin qw($Bin);
use lib "$Bin/../lib";		# Test packages
use lib "$Bin/../../lib"; 	# PsN packages
use data_test;
use scripts_test;
use debug;
use Getopt::Long;
use vars qw/ $opt_debug
             $opt_debug_package
             $opt_debug_subroutine
             $opt_tests
             $opt_verbose
             $opt_generate_facit /;

## Configure the command line parsing
Getopt::Long::config("auto_abbrev");

## Declare the options
my $res = GetOptions("debug:1",
		     "debug_package:s",
		     "debug_subroutine:s",
		     "tests:s",
		     "verbose", 
		     "generate_facit" );

debug -> level( $opt_debug );
debug -> package( $opt_debug_package );
debug -> subroutine( $opt_debug_subroutine );

my @tests = ( 'scripts_test' );

foreach my $test( @tests ){
  my $top_dir = $Bin . '/..';
  $top_dir =~ s/[^\/]*\/\.\.//;
  
  $test -> test( verbose          => $opt_verbose,
		 debug            => $opt_debug,
		 debug_package    => $opt_debug_package,
		 debug_subroutine => $opt_debug_subroutine,
		 tests            => $opt_tests,
		 generate_facit   => $opt_generate_facit,
		 directory        => $top_dir );
}
