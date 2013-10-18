package ext::Math::SigFigs;

# Copyright (c) 1995-2003 Sullivan Beck. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

########################################################################
# HISTORY
########################################################################

# Written by:
#    Sullivan Beck (sbeck@cpan.org)
# Any suggestions, bug reports, or donations :-) should be sent to me.

# Version 1.00  12/05/1996
#    Initial creation
#
# Version 1.01  01/28/1997
#    Used croak and changed die's to confess.
#    "101" is now returned as "101." .
#    Fixed where 9.99 wasn't being correctly returned with 1 sigfig.
#       Kyle Krom <kromk@pt.Cyanamid.COM>
#
# Version 1.02  01/10/2000
#    Fixed where 1249.01 wasn't correctly rounded to 1200.
#       Janna Wemekamp <jwemekam@erin.gov.au>
#
# Version 1.03  09/11/2003
#    Fixed a bug where I left off the sign.  Steve Reaser
#       <steve_reaser@webassign.net>
#    Fixed a bug in subSF where numbers ending in zero were truncated.
#       Andrew Grall <AGrall@dcds.edu>

########################################################################

require 5.000;
require Exporter;
use Carp;
@ISA = qw(Exporter);
@EXPORT = qw(FormatSigFigs
             CountSigFigs
);
@EXPORT_OK = qw(FormatSigFigs CountSigFigs addSF subSF multSF divSF VERSION);
%EXPORT_TAGS = (all => \@EXPORT_OK);

$VERSION = 1.03;

use strict;

sub addSF {
  my($n1,$n2)=@_;
  return ()  if (! IsReal($n1)  or  ! IsReal($n2));
  return $n2 if ($n1==0);
  return $n1 if ($n2==0);
  my($m1,$m2,$m,$n)=();
  if ($n1 =~ /\.(.*)$/) {
    $m1=length($1);
  } else {
    $n1 =~ /(0*)$/;
    $m1=-length($1);
  }
  if ($n2 =~ /\.(.*)$/) {
    $m2=length($1);
  } else {
    $n2 =~ /(0*)$/;
    $m2=-length($1);
  }
  $m=($m1<$m2 ? $m1 : $m2);
  $n=$n1+$n2;
  return 0  if ($n==0);

  if ($n=~ /[+-]?(.*)\.(.*)/) {           # 123.456
    ($m1,$m2)=(length($1),length($2));
    if ($m<0) {                           # 120
      $n=FormatSigFigs($n,$m1+$m);
    } elsif ($m>=0) {                     # 123. 123.5
      $n=sprintf("%.$m"."f",$n);
      $n .= "."  if ($m==0);
    }

  } else {                                # 12340
    if ($m<0) {                           # 12300
      $n=FormatSigFigs($n,length($n)+$m);
    } elsif ($m>=0) {                     # 12340. 12340.0
      $n .= "." . "0"x$m;
    }
  }
  $n;
}

sub subSF {
  my($n1,$n2)=@_;
  if ($n2<0) {
    $n2 =~ s/\-//;
  } else {
    $n2 =~ s/^\+?/-/;
  }
  addSF($n1,$n2);
}

sub multSF {
  my($n1,$n2)=@_;
  return ()  if (! IsReal($n1)  or  ! IsReal($n2));
  return 0   if ($n1==0  or  $n2==0);
  my($m1)=CountSigFigs($n1);
  my($m2)=CountSigFigs($n2);
  my($m)=($m1<$m2 ? $m1 : $m2);
  my($n)=$n1*$n2;
  FormatSigFigs($n,$m);
}

sub divSF {
  my($n1,$n2)=@_;
  return ()  if (! IsReal($n1)  or  ! IsReal($n2));
  return 0   if ($n1==0);
  return ()  if ($n2==0);
  my($m1)=CountSigFigs($n1);
  my($m2)=CountSigFigs($n2);
  my($m)=($m1<$m2 ? $m1 : $m2);
  my($n)=$n1/$n2;
  FormatSigFigs($n,$m);
}

sub FormatSigFigs {
  my($N,$n)=@_;
  my($ret);
  return ""  if (! IsReal($N)  or  ! IsInt($n)  or  $n<1);
  my($l,$l1,$l2,$m,$s)=();
  $N=~ s/\s+//g;               # Remove all spaces
  $N=~ s/^([+-]?)//;           # Remove sign
  $s=(defined $1 ? $1 : "");
  $N=~ s/^0+//;                # Remove all leading zeros
  $N=~ s/0+$//  if ($N=~/\./); # Remove all trailing zeros (when decimal point)
  $N=~ s/\.$//;                # Remove a trailing decimal point
  $N= "0$N"  if ($N=~ /^\./);  # Turn .2 into 0.2

  # If $N has fewer sigfigs than requested, pad it with zeros and return it.
  $m=CountSigFigs($N);
  if ($m==$n) {
    $N="$N."  if (length($N)==$n);
    return "$s$N";
  } elsif ($m<$n) {
    if ($N=~ /\./) {
      return "$s$N" . "0"x($n-$m);
    } else {
      $N=~ /(\d+)$/;
      $l=length($1);
      return "$s$N"  if ($l>$n);
      return "$s$N." . "0"x($n-$l);
    }
  }

  if ($N=~ /^([1-9]\d*)\.([0-9]*)/) {     # 123.4567  (l1=3, l2=4)
    ($l1,$l2)=(length($1),length($2));
    if ($n>=$l1) {                        # keep some decimal points
      $l=$n-$l1;
      ($l2>$l) && ($N=~ s/5$/6/);         # 4.95 rounds down... make it go up
      $ret=sprintf("%.${l}f",$N);
      $m=CountSigFigs($ret);
      if ($m==$n) {
        $ret="$ret."  if ($l==0 && $m==length($ret));
        return "$s$ret";
      }

      # special case 9.99 (2) -> 10.
      #              9.99 (1) -> 10

      $l--;
      if ($l>=0) {
        $ret=sprintf("%.${l}f",$N);
        $ret="$ret."  if ($l==0);
        return "$s$ret";
      }
      return "$s$ret";
    } else {
      my($a)=substr($N,0,$n);             # Turn 1234.56 into 123.456 (n=3)
      $N =~ /^$a(.*)\.(.*)$/;
      my($b,$c)=($1,$2);
      $N="$a.$b$c";
      $N=sprintf("%.0f",$N);              # Turn it to 123
      $N .= "0" x length($b);             # Turn it to 1230
      return "$s$N";
    }

  } elsif ($N=~ /^0\.(0*)(\d*)$/) {       # 0.0123
    ($l1,$l2)=(length($1),length($2));
    ($l2>$n) && ($N=~ s/5$/6/);
    $l=$l1+$n;
    $ret=sprintf("%.${l}f",$N);
    $m=CountSigFigs($ret);
    return "$s$ret"  if ($n==$m);

    # special cases 0.099 (1) -> 0.1
    #               0.99  (1) -> 1.

    $l--;
    $ret=sprintf("%.${l}f",$N);
    $m=CountSigFigs($ret);
    $ret="$ret."  if ($l==0);
    return "$s$ret"  if ($n==$m);
    $ret =~ s/0$//;
    return "$s$ret";
  }

  return 0  if ($N==0);

  if ($N=~ /^(\d+?)(0*)$/) {              # 123
    ($l1,$l2)=(length($1),length($2));
    ($l1>$n) && ($N=~ s/5(0*)$/6$1/);
    $l=$n;
    $m=sprintf("%.${l}f",".$N");          # .123
    if ($m>1) {
      $l--;
      $m=~ s/\.\d/\.0/  if ($l==0);
    } else {
      $m =~ s/^0//;
    }
    $m=~ s/\.//;
    $N=$m . "0"x($l1+$l2-$n);
    $N="$N."  if (length($N)==$n);
    return "$s$N";
  }
  "";

}

sub CountSigFigs {
  my($N)=@_;
  return ()  if (! IsReal($N));
  return 0   if ($N==0);

  my($tmp)=();
  if ($N=~ /^\s*[+-]?\s*0*([1-9]\d*)\s*$/) {
    $tmp=$1;
    $tmp=~ s/0*$//;
    return length($tmp);
  } elsif ($N=~ /^\s*[+-]?\s*0*\.0*(\d*)\s*$/) {
    return length($1);
  } elsif ($N=~ /^\s*[+-]?\s*0*([1-9]?\d*\.\d*)\s*$/) {
    return length($1)-1;
  }
  ();
}

########################################################################
# NOT FOR EXPORT
########################################################################

sub IsReal {
  my($N,$low,$high)=@_;
  return 0 if ($N eq "");

  my($sign)='^\s* [-+]? \s*';
  my($int) ='\d+';
  my($dec) ='(\.\d*)? \s* $ ';
  my($Dec) =' \.\d*   \s* $ ';

  if ($N =~ /$sign $int $dec/x or
      $N =~ /$sign $Dec/x) {
    if (defined $low  and  defined $high) {
      $N=~ s/\s+//;
      return 1  if ($N>=$low  and  $N<=$high);
      return 0;
    }
    return 1;
  }
  return 0;
}

sub IsInt {
  my($N,$low,$high)=@_;
  return 0 if ($N eq "");
  my($sign)='^\s* [-+]? \s*';
  my($int) ='\d+ \s* $ ';
  if ($N =~ /$sign $int/x) {
    if (defined $low  and  defined $high) {
      return 1  if ($N>=$low  and  $N<=$high);
      return 0;
    }
    return 1;
  }
  return 0;
}

1;

########################################################################
########################################################################
# POD
########################################################################
########################################################################

=head1 NAME

Math::SigFigs - do math with correct handling of significant figures

=head1 SYNOPSIS

If you only need to use CountSigFigs and FormatSigFigs, use the first
form.  If you are going to be doing arithmetic, use the second line.

  use Math::SigFigs;
  use Math::SigFigs qw(:all);

The following routines do simple counting/formatting:

  $n=CountSigFigs($num);
  $num=FormatSigFigs($num,$n);

Use the following routines to do arithmetic operations.

  $num=addSF($n1,$n2);
  $num=subSF($n1,$n2);
  $num=multSF($n1,$n2);
  $num=divSF($n1,$n2);

=head1 DESCRIPTION

In many scientific applications, it is often useful to be able to format
numbers with a given number of significant figures, or to do math in
such a way as to maintain the correct number of significant figures.
The rules for significant figures are too complicated to be handled solely
using the sprintf function (unless you happen to be Randal Schwartz :-).

These routines allow you to correctly handle significan figures.

It can count the number of significan figures, format a number to a
given number of significant figures, and do basic arithmetic.

=head1 ROUTINES

=over 4

=item CountSigFigs

  $n=CountSigFigs($N);

This returns the number of significant figures in a number.  It returns
() if $N is not a number.

  $N      $n
  -----   --
  240     2
  240.    3
  241     3
  0240    2
  0.03    1
  0       0
  0.0     0

=item FormatSigFigs

  $str=FormatSigFigs($N,$n)

This returns a string containing $N formatted to $n significant figures.
This will work for all cases except something like "2400" formatted to
3 significant figures.

  $N     $n   $str
  ------ --   -------
  2400    1   2000
  2400    2   2400
  2400    3   2400
  2400    4   2400.
  2400    5   2400.0

  141     3   141.
  141     2   140

  0.039   1   0.04
  0.039   2   0.039

  9.9     1   10
  9.9     2   9.9
  9.9     3   9.90

=item addSF, subSF, multSF, divSF

These routines add/subtract/multiply/divide two numbers while maintaining
the proper number of significant figures.

=back

=head1 KNOWN PROBLEMS

=over 4

=item Without scientific notation, some numbers are ambiguous

These routines do not work with scientific notation (exponents).  As a
result, it is impossible to unambiguously format some numbers.  For
example,

  $str=FormatSigFigs("2400",3);

will by necessity return the string "2400" which does NOT have 3
significant figures.  This is not a bug.  It is simply a fundamental
problem with working with significant figures when not using scientific
notation.

=item A bug in some printf library calls on the Mac

One of the tests

   FormatSigFigs(0.99,1)  =>  1.

fails on at least some Mac OS versions.  It gives "0." instead of "1."
and comes when the call:

   printf("%.0f","0.99")

returns 0 instead of 1.  I have not added a workaround for this.

=back

=head1 AUTHOR

Sullivan Beck (sbeck@cpan.org)

=cut
