#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 14;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use math qw(:all);

my (@a, @b, $a, $b, $c);

# round
is (round(1.23), 1, "round positive down");
is (round(23.5), 24, "round positive x.5");
is (round(99.73), 100, "round positive up");
is (round(518), 518, "round positive integer");
is (round(0), 0, "round zero");
is (round(-1.23), -1, "round negative up");
is (round(-1.5), -2, "round negative x.5");
is (round(-18.999), -19, "round negative down");
is (round(-199), -199, "round negative integer");

# eps
is (eps(1), 2.220446049250313e-16, "eps(1)");
is (eps(-1), 2.220446049250313e-16, "eps(-1)");
is (eps(0.00073), 1.084202172485504e-19, "eps(0.00073)");
is (eps(400000000000000), 0.0625, "eps(400000000000000)");

# inf
is (inf(), 'inf', "inf()");

done_testing();
