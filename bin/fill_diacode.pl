#!/usr/bin/perl -w

use strict;

my $diacodefile = $ARGV[0];
my $subcodefile = $ARGV[1];
my $use_debug_class = @ARGV > 2 ? $ARGV[2] : 1;

print "$diacodefile does not exist\n" unless ( -e $diacodefile );
print "$subcodefile does not exist\n" unless ( -e $subcodefile );

open ( DIACODE, $diacodefile );
my @diacode = <DIACODE>;
close ( DIACODE );

open ( SUBCODE, $subcodefile );
my @subcode = <SUBCODE>;
close ( SUBCODE );

open ( NEWCODE, '>tmp.txt' );

my $in_sub = 0;
my $diacode_line_number = 1;
my @include  = ();

my $package;
my $have_package = 0;

my ( $junk, $junk2, $sub );
my @transfer = ();
my $in_non_dia = 0;

foreach ( @diacode ) {
  $diacode_line_number ++;

  if( $have_package ){
    foreach ( @subcode ) {
      next unless ( /\s*start include statements/ or $in_sub );
      if ( /\s*start include statements/ ) {
	$in_sub = 1;
	next;
      }
      if ( /\s*end include/ ) {
	$in_sub = 0;
	push( @include, "use debug;\n" ) unless( $package eq 'debug.pm' or ! $use_debug_class );
	last;
      }
      push ( @include, $_ );
    }
    
    if( $in_sub ){
      die "Warning, no end of include statements found in $subcodefile \n";
    }
    
    $diacode_line_number += scalar( @include ) + 1;
    print NEWCODE @include, "\n";
    $have_package = 0;
  }

  if ( /^package / ) {
    ($junk, $package, $junk2) = split(' ', $_, 3);
    $package =~ s/\;//;
    $have_package = 1;
  }
  
  if ( /^sub / ) {
    ($junk, $sub, $junk2) = split(' ', $_, 3);
  }    
  
  if( /# Start of Non-Dia code #/ ) {
    my $line_number = 1;
    foreach ( @subcode ) {
      $line_number ++;
      next unless ( /\s*start $sub\s*$/ or $in_sub );
      if ( /\s*start $sub\s*$/ ) {
	$in_sub = 1;
	if( $package ne 'debug' and $use_debug_class ) {
	  if ( $sub eq 'new' ) {
	    push( @transfer, '        \'debug\' -> warn(level => 3, message => "Entering \t" . ref($this). \'-> '.$sub."\');\n" );
	  } else {
	    push( @transfer, '        \'debug\' -> warn(level => 3, message => "Entering \t" . ref($self). \'-> '.$sub."\');\n" );
	  }
	}
	push ( @transfer,"# line $line_number \"$subcodefile\" \n" );
	next ;
      }
      if ( /\s*end $sub\s*$/ ) {
	$in_sub = 0;
	$diacode_line_number += scalar( @transfer ) + 1;
	my $local_diacode_file = $diacodefile;
	$local_diacode_file =~ s/_temp//;
	push ( @transfer,"# line $diacode_line_number $local_diacode_file \n" );
	if( $package ne 'debug' and $use_debug_class ) {
	  $diacode_line_number ++;
	  if ( $sub eq 'new' ) {
	    push( @transfer, '        \'debug\' -> warn(level => 3, message => "Leaving \t" . ref($this). \'-> '.$sub."\');\n" );
	  } else {
	    push( @transfer, '        \'debug\' -> warn(level => 3, message => "Leaving \t" . ref($self). \'-> '.$sub."\');\n" );
	  }
	}
	last;
      }
      push ( @transfer, $_ );
    }
  }


  if ( /# End of Non-Dia code #/ ) {
       $in_non_dia = 0;
       print NEWCODE @transfer;
       @transfer = ();
  }  
  print NEWCODE;
}

close ( NEWCODE );

system( "mv tmp.txt $diacodefile" );
