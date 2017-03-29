#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
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
is (inf(), inf()+1, "inf()");

#usable_number
my @not_ok = ('hej','Inf','Infinity','NaN','nan','NAN','NA','.',
			  '-nan','-1.#IND','-1.#INF','1.#IND','1.#INF','undef',undef,inf(),-inf());
my @is_ok = ('6','1.000','0','0.000','+10e+03','1.0E+01','-1.0E+01','0.000E-00',
			 ' 6','1.000 ',' 0 ','0.000  ',' +10e+03  ',
6,1.000,0,0.000,+10e+03,1.0E+01,-1.0E+01,0.000E-00,.123,-.2,.1e4
);


foreach my $number (@not_ok) {
	no warnings qw(uninitialized);
	is(usable_number($number),0,"not usable number $number");
}
foreach my $number (@is_ok) {
	is(usable_number($number),1,"usable number $number");
}

# to_precision
is (to_precision(12.12345678912, 10), 12.12345679, "to_precision with 10");
is (to_precision(12.12345678912, 2), 12, "to_precision with 2");

# convert_float_string
is (convert_float_string("7.2120e-02"), "0.07212", "convert_float_string 1");
is (convert_float_string("0.001e+04"), "10", "convert_float_string 2");
is (convert_float_string("12.300"), "12.3", "convert_float_string 3");
is (convert_float_string("0.0"), "0", "convert_float_string 4");
is (convert_float_string("1.0"), "1", "convert_float_string 5");
is (convert_float_string("+23.0"), "23", "convert_float_string 6");
is (convert_float_string("-12.3"), "-12.3", "convert_float_string 7");
is (convert_float_string("+1.23456e6"), "1234560", "convert_float_string 8");
is (convert_float_string("+1.23456e-6"), "0.00000123456", "convert_float_string 9");


done_testing();
