#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model::problem::record::theta_option;
use model::problem::init_record;
use PsN;			# Need to set PsN version as this is a global variable

# Test new and read option
my $option1 = model::problem::record::init_option->new(option_string => '0.01',
													   on_diagonal => 1);

my $option2 = model::problem::record::init_option->new(option_string => '0.009',
													   on_diagonal => 0);

my $option3 = model::problem::record::init_option->new(option_string => '1000000',
													   on_diagonal => 1);

my $option4 = model::problem::record::init_option->new(option_string => '-0.009',
													   on_diagonal => 0);

#test get_range
my $range = $option1->get_range(degree=> 0.5);
cmp_float($range->[0],(0.01-0.5*0.01), "Option1->range lower");
cmp_float($range->[1],(0.01+0.5*0.01), "Option1->range upper");

$range = $option2->get_range(degree=> 0.9);
cmp_float($range->[0],(0.009-0.9*0.009), "Option2->range lower");
cmp_float($range->[1],(0.009+0.9*0.009), "Option2->range upper");

$range = $option3->get_range(degree=> 0.5);
cmp_float($range->[0],(1000000-0.5*1000000), "Option3->range lower");
cmp_float($range->[1],(1000000  - 1), "Option3->range upper");

$range = $option4->get_range(degree=> 0.9);
cmp_float($range->[0],(-0.009-0.9*0.009), "Option4->range lower");
cmp_float($range->[1],(-0.009+0.9*0.009), "Option4->range upper");

$PsN::nm_major_version = 6; #affects formatting in init_option.pm

my $option1 = model::problem::record::theta_option->new(option_string => '(-1.679,0,2472000)');
is($option1->on_diagonal,undef,'theta undef on_diagonal');

my ($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -0.25395343214);
cmp_ok($new_value,'==','-0.25395',' check_and_set NM6 1');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 0.25395343214);
cmp_ok($new_value,'==','0.253953',' check_and_set NM6 2');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -0.2063198);
cmp_ok($new_value,'==','-0.20632',' check_and_set NM6 3');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 0.2063198);
cmp_ok($new_value,'==','0.20632',' check_and_set NM6 4');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 1E-20);
cmp_ok($new_value,'==','0.000001',' check_and_set NM6 5');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -1E-20);
cmp_ok($new_value,'==','-0.00001',' check_and_set NM6 6');

my $record = model::problem::init_record->new(record_arr => ['BLOCK(2) 0.02','0.001 0.03']);
($succ,$err,$new_value) = $record->options->[1]->check_and_set_init(new_value => -1E-9);
cmp_ok($new_value,'==','-0.00001',' check_and_set NM6 7');

($succ,$err,$new_value) = $record->options->[0]->check_and_set_init(new_value => 1E-9);
cmp_ok($new_value,'==','0.000001',' check_and_set NM6 8');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 1);
cmp_ok($new_value,'==','1',' check_and_set NM6 9');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -1.0E00);
cmp_ok($new_value,'==','-1',' check_and_set NM6 10');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -1.200000);
cmp_ok($new_value,'==','-1.2',' check_and_set NM6 11');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 1200.126656);
cmp_ok($new_value,'==','1200.127',' check_and_set NM6 12');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -1200.126656);
cmp_ok($new_value,'==','-1200.13',' check_and_set NM6 13');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -1200.000);
cmp_ok($new_value,'==','-1200',' check_and_set NM6 14');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 12000000);
cmp_ok($new_value,'==','12000000',' check_and_set NM6 15');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -1200000);
cmp_ok($new_value,'==','-1200000',' check_and_set NM6 16');


($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 99.38357761418);
cmp_ok($new_value,'==','99.38358',' check_and_set NM6 17');


$PsN::nm_major_version = undef; #affects formatting in init_option.pm should get nm7 per default

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -0.25395343214);
cmp_ok($new_value,'==','-0.25395343214',' check_and_set NM7 1');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 0.25395343214);
cmp_ok($new_value,'==','0.25395343214',' check_and_set NM7 2');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -0.2063198);
cmp_ok($new_value,'==','-0.2063198',' check_and_set NM7 3');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 0.2063198);
cmp_ok($new_value,'==','0.2063198',' check_and_set NM7 4');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 1E-20);
cmp_ok($new_value,'==','1E-20',' check_and_set NM7 5');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -1E-20);
cmp_ok($new_value,'==','-1E-20',' check_and_set NM7 6');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 0.25395343214123456);
cmp_ok($new_value,'==','0.253953432141235',' check_and_set NM7 7');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => -0.25395343214123456);
cmp_ok($new_value,'==','-0.253953432141235',' check_and_set NM7 8');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 2.5395343214123456E20);
cmp_ok($new_value,'==','2.53953432141235E+20',' check_and_set NM7 9');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 2.5395000);
cmp_ok($new_value,'==','2.5395',' check_and_set NM7 10');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 1.0000);
cmp_ok($new_value,'==','1',' check_and_set NM7 11');

($succ,$err,$new_value) = $option1->check_and_set_init(new_value => 0);
cmp_ok($new_value,'==','0',' check_and_set NM7 12');



done_testing();
