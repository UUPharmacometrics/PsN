package ext::Statistics::Distributions;
 
use strict;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);
use constant PI => 3.1415926536;
use constant SIGNIFICANT => 5; # number of significant digits to be returned
 
require Exporter;
 
@ISA = qw(Exporter AutoLoader);
@EXPORT_OK = qw(chisqrdistr udistr uprob chisqrprob);
$VERSION = '1.02';
 
sub chisqrdistr { # Percentage points  X^2(x^2,n)
        my ($n, $p) = @_;
        if ($n <= 0 || abs($n) - abs(int($n)) != 0) {
                die "Invalid n: $n\n"; # degree of freedom
        }
        if ($p <= 0 || $p > 1) {
                die "Invalid p: $p\n"; 
        }
        return precision_string(_subchisqr($n, $p));
}
 
sub udistr { # Percentage points   N(0,1^2)
        my ($p) = (@_);
        if ($p > 1 || $p <= 0) {
                die "Invalid p: $p\n";
        }
        return precision_string(_subu($p));
}
 
sub uprob { # Upper probability   N(0,1^2)
        my ($x) = @_;
        return precision_string(_subuprob($x));
}
 
sub chisqrprob { # Upper probability   X^2(x^2,n)
        my ($n,$x) = @_;
        if (($n <= 0) || ((abs($n) - (abs(int($n)))) != 0)) {
                die "Invalid n: $n\n"; # degree of freedom
        }
        return precision_string(_subchisqrprob($n, $x));
}
 
sub _subchisqrprob {
        my ($n,$x) = @_;
        my $p;
 
        if ($x <= 0) {
                $p = 1;
        } elsif ($n > 100) {
                $p = _subuprob((($x / $n) ** (1/3)
                                - (1 - 2/9/$n)) / sqrt(2/9/$n));
        } elsif ($x > 400) {
                $p = 0;
        } else {   
                my ($a, $i, $i1);
                if (($n % 2) != 0) {
                        $p = 2 * _subuprob(sqrt($x));
                        $a = sqrt(2/PI) * exp(-$x/2) / sqrt($x);
                        $i1 = 1;
                } else {
                        $p = $a = exp(-$x/2);
                        $i1 = 2;
                }
 
                for ($i = $i1; $i <= ($n-2); $i += 2) {
                        $a *= $x / $i;
                        $p += $a;
                }
        }
        return $p;
}
 
sub _subu {
        my ($p) = @_;
        my $y = -log(4 * $p * (1 - $p));
        my $x = sqrt(
                $y * (1.570796288
                  + $y * (.03706987906
                        + $y * (-.8364353589E-3
                          + $y *(-.2250947176E-3
                                + $y * (.6841218299E-5
                                  + $y * (0.5824238515E-5
                                        + $y * (-.104527497E-5
                                          + $y * (.8360937017E-7
                                                + $y * (-.3231081277E-8
                                                  + $y * (.3657763036E-10
                                                        + $y *.6936233982E-12)))))))))));
        $x = -$x if ($p>.5);
        return $x;
}
 
sub _subuprob {
        my ($x) = @_;
        my $p = 0; # if ($absx > 100)
        my $absx = abs($x);
 
        if ($absx < 1.9) {
                $p = (1 +
                        $absx * (.049867347
                          + $absx * (.0211410061
                                + $absx * (.0032776263
                                  + $absx * (.0000380036
                                        + $absx * (.0000488906
                                          + $absx * .000005383)))))) ** -16/2;
        } elsif ($absx <= 100) {
                for (my $i = 18; $i >= 1; $i--) {
                        $p = $i / ($absx + $p);
                }
                $p = exp(-.5 * $absx * $absx) 
                        / sqrt(2 * PI) / ($absx + $p);
        }
 
        $p = 1 - $p if ($x<0);
        return $p;
}
 
sub _subchisqr {
        my ($n, $p) = @_;
        my $x;
 
        if (($p > 1) || ($p <= 0)) {
                die "Invalid p: $p\n";
        } elsif ($p == 1){
                $x = 0;
        } elsif ($n == 1) {
                $x = _subu($p / 2) ** 2;
        } elsif ($n == 2) {
                $x = -2 * log($p);
        } else {
                my $u = _subu($p);
                my $u2 = $u * $u;
 
                $x = max(0, $n + sqrt(2 * $n) * $u
                        + 2/3 * ($u2 - 1)
                        + $u * ($u2 - 7) / 9 / sqrt(2 * $n)
                        - 2/405 / $n * ($u2 * (3 *$u2 + 7) - 16));
 
                if ($n <= 100) {
                        my ($x0, $p1, $z);
                        do {
                                $x0 = $x;
                                if ($x < 0) {
                                        $p1 = 1;
                                } elsif ($n>100) {
                                        $p1 = _subuprob((($x / $n)**(1/3) - (1 - 2/9/$n))
                                                / sqrt(2/9/$n));
                                } elsif ($x>400) {
                                        $p1 = 0;
                                } else {
                                        my ($i0, $a);
                                        if (($n % 2) != 0) {
                                                $p1 = 2 * _subuprob(sqrt($x));
                                                $a = sqrt(2/PI) * exp(-$x/2) / sqrt($x);
                                                $i0 = 1;
                                        } else {
                                                $p1 = $a = exp(-$x/2);
                                                $i0 = 2;
                                        }
 
                                        for (my $i = $i0; $i <= $n-2; $i += 2) {
                                                $a *= $x / $i;
                                                $p1 += $a;
                                        }
                                }
                                $z = exp((($n-1) * log($x/$n) - log(4*PI*$x) 
                                        + $n - $x - 1/$n/6) / 2);
                                $x += ($p1 - $p) / $z;
                                $x = sprintf("%.5f", $x);
                        } while (($n < 31) && (abs($x0 - $x) > 1e-4));
                }
        }
        return $x;
}
 
sub log10 {
        my $n = shift;
        return log($n) / log(10);
}
  
sub max {
        my $max = shift;
        my $next;
        while (@_) {
                $next = shift;
                $max = $next if ($next > $max);
        }       
        return $max;
}
 
sub min {
        my $min = shift;
        my $next;
        while (@_) {
                $next = shift;
                $min = $next if ($next < $min);
        }       
        return $min;
}
 
sub precision {
        my ($x) = @_;
        return abs int(log10(abs $x) - SIGNIFICANT);
}
 
sub precision_string {
        my ($x) = @_;
        if ($x) {
                return sprintf "%." . precision($x) . "f", $x;
        } else {
                return "0";
        }
}
 
1;

=head1 AVAILABILITY 
 
The latest version of this module is available from the Distribution Perl Archive Network (CPAN). Please visit http://www.cpan.org/ to find a CPAN site near you or see http://www.cpan.org/authors/id/M/MI/MIKEK/ .
 
=head1 AUTHOR
 
Michael Kospach <mike.perl@gmx.at>
 
Nice formating, simplification and bug repair by Matthias Trautner Kromann <mtk@id.cbs.dk>
 
=head1 COPYRIGHT 
 
Copyright 2003 Michael Kospach. All rights reserved. 
 
This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself. 
 
=cut
