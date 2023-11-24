#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use random;
use model::problem::init_record;
use model::problem::omega;
use model::problem::sigma;
use model::problem::theta;
use Test::Exception;
use ui;


use PsN;			# Need to set PsN version as this is a global variable
$PsN::nm_major_version = 6; #affects formatting in init_option.pm

ui -> silent(1);
# Test new and read_option
my $record = model::problem::init_record->new(record_arr => ['2']);
use Data::Dumper;
print Dumper($record);
my $r = $record->_format_record;
print Dumper($r);
my @str = split /\s+/, $$r[0];
is ($str[0], '$INIT_RECORD', "record->_format_record");
is ($str[1], '2', "record->_format_record");
is($record->is_block,0, 'anonymous record is_block');
is ($record->get_size(),1, 'record size is 1');

random_set_seed_from_phrase('12345');

$record = model::problem::omega->new(record_arr => ['BLOCK(2) 0.02','0 0.01 FIX']);
is($record->is_block,1, 'anonymous record is_block yes');
is ($record->options->[0]->init,0.02,'record 1 init 0');
is ($record->options->[1]->init,0,'record 1 init 1');
is ($record->options->[2]->init,0.01,'record 1 init 2');
is ($record->fix,1,'record 1 fix');
is ($record->type,'BLOCK','record 1 type');
is ($record->size,2,'record 1 size');
is_deeply($record->get_estimated_coordinate_strings,[],'estimated coordinate strings 1');

my $matrix = $record->get_matrix();
is_deeply($matrix,[[0.02,0],[0,0.01]],"get matrix 1");


$record->set_random_inits(degree => 0.1);
is($record->options->[0]->init,0.02,'record 1 init 0');
is($record->options->[1]->init,0,'record 1 init 1');
is($record->options->[2]->init,0.01,'record 1 init 2');
$record->set_1_fix();
$matrix = $record->get_matrix();
is_deeply($matrix,[[1,0],[0,1]],"get matrix 1");

$record->unfix();
is ($record->fix,0,'record unfix');

$record = model::problem::sigma->new(record_arr => ['DIAGONAL(2) 0.02','0.01']);
is($record->is_block,0, 'diagonal sigma record is_block');
is($record->get_size(),2,'record size is 2');

is_deeply($record->get_estimated_coordinate_strings,['SIGMA(1,1)','SIGMA(2,2)'],
		  'estimated coordinate strings 2');
is_deeply($record->get_estimated_coordinate_strings(only_eta_eps => 1),[1,2],
		  'estimated coordinate strings 2 b');

my $vector = $record->get_vector();
is_deeply($vector,[0.02,0.01],"get vector 1");
is ($record->options->[0]->init,0.02,'record 2 init 0');
is ($record->options->[1]->init,0.01,'record 2 init 1');
is ($record->fix,0,'record 2 fix');
is ($record->type,'DIAGONAL','record 2 type');
is ($record->size,2,'record 2 size');
$record->set_random_inits(degree => 0.1);
cmp_float($record->options->[0]->init,0.01953,'record 2 init 0');
cmp_float ($record->options->[1]->init,0.010764,'record 2 init 1');
my $ok = $record->set_vector(vector =>[5,4]);
is ($ok,1,'set_vector diag ok');
is ($record->options->[0]->init,5,'set_vector diag init 0');
is ($record->options->[1]->init,4,'set_vector diag init 1');

$record = model::problem::omega->new(record_arr => ['BLOCK(3) 0.02','-0.002 0.5','0.003 -0.005 1']);
is_deeply($record->get_estimated_coordinate_strings,
		  ['OMEGA(1,1)','OMEGA(2,1)','OMEGA(2,2)','OMEGA(3,1)','OMEGA(3,2)','OMEGA(3,3)'],
		  'estimated coordinate strings 3');
is_deeply($record->get_estimated_coordinate_strings(only_eta_eps => 1),
		  [1,2,3],'estimated coordinate strings 3b');

$matrix = $record->get_matrix();
is_deeply($matrix,[[0.02,-0.002,0.003],[-0.002,0.5,-0.005],[0.003,-0.005,1]],"get matrix 2");

is ($record->get_size(),3,'record size is 3');
is ($record->options->[0]->init,0.02,'record 3 init 0');
is ($record->options->[1]->init,-0.002,'record 3 init 1');
is ($record->options->[2]->init,0.5,'record 3 init 2');
is ($record->options->[3]->init,0.003,'record 3 init 3');
is ($record->options->[4]->init,-0.005,'record 3 init 4');
is ($record->options->[5]->init,1,'record 3 init 5');
is ($record->fix,0,'record 3 fix');
is ($record->type,'BLOCK','record 3 type');
is ($record->size,3,'record 3 size');
$record->set_random_inits(degree => 0.1);
cmp_float ($record->options->[0]->init,0.018014,'record 3 init 0');
cmp_float ($record->options->[1]->init,-0.0022,'record 3 init 1');
cmp_float ($record->options->[2]->init,0.544906,'record 3 init 2');
cmp_float ($record->options->[3]->init,0.002951,'record 3 init 3');
cmp_float ($record->options->[4]->init,-0.00502,'record 3 init 4');
cmp_float ($record->options->[5]->init,0.961905,'record 3 init 5');

#make band
$record = model::problem::omega->new(record_arr => ['BLOCK(3) 0.02','-0.002 0.5','0 -0.005 1']);
is ($record->options->[0]->init,0.02,'record 3.5 init 0');
is ($record->options->[1]->init,-0.002,'record 3.5 init 1');
is ($record->options->[2]->init,0.5,'record 3.5 init 2');
is ($record->options->[3]->init,0,'record 3.5 init 3');
is ($record->options->[4]->init,-0.005,'record 3.5 init 4');
is ($record->options->[5]->init,1,'record 3.5 init 5');
$record->set_random_inits(degree => 0.1);
cmp_float ($record->options->[0]->init,0.020012,'record 3.5 init 0');
cmp_float ($record->options->[1]->init,-0.00208,'record 3.5 init 1');
cmp_float ($record->options->[2]->init,0.456577,'record 3.5 init 2');
is ($record->options->[3]->init,0,'record 3.5 init 3');
cmp_float ($record->options->[4]->init,-0.0051,'record 3.5 init 4');
cmp_float ($record->options->[5]->init,1.090233,'record 3.5 init 5');

#nonposdef, to make cholesky fail couple of times and need deflation off-diag
$PsN::nm_major_version = 7; #affects formatting in init_option.pm
$record = model::problem::sigma->new(record_arr => ['BLOCK(3) 2','2 2','2 2 2']);
$record->set_random_inits(degree => 0.1);
cmp_float ($record->options->[0]->init,1.68769122459,'record 3.6 init 0');
cmp_float ($record->options->[1]->init,0.763776429425,'record 3.6 init 1');
cmp_float ($record->options->[2]->init,2.73001132765,'record 3.6 init 2');
cmp_float ($record->options->[3]->init,1.45924992008,'record 3.6 init 2');
cmp_float ($record->options->[4]->init,1.15412538192,'record 3.6 init 4');
cmp_float ($record->options->[5]->init,1.41236980162,'record 3.6 init 5');


#very high degree, to get cholesky fail a couple of times
$PsN::nm_major_version = 6; #affects formatting in init_option.pm
$record = model::problem::omega->new(record_arr => ['BLOCK(3) 2','2 2','0 2 2']);
$record->set_random_inits(degree => 0.9);
cmp_float ($record->options->[0]->init,0.967023,'record 3.7 init 0');
cmp_float ($record->options->[1]->init,0.652371,'record 3.7 init 1');
cmp_float ($record->options->[2]->init,1.834813,'record 3.7 init 2');
is($record->options->[3]->init,0,'record 3.7 init 2');
cmp_float ($record->options->[4]->init,0.462744,'record 3.7 init 4');
cmp_float ($record->options->[5]->init,1.17502,'record 3.7 init 5');

$ok = $record->set_vector(vector => [4,0.1,3,0.2,0.3,5]);
is ($ok,1,'set_vector block ok');

is($record->options->[0]->init,4,'record 3.8 set_vector 1');
is($record->options->[1]->init,0.1,'record 3.8  set_vector init 2');
is($record->options->[2]->init,3,'record 3.8  set_vector init 3');
is($record->options->[3]->init,0.2,'record 3.8  set_vector init 4');
is($record->options->[4]->init,0.3,'record 3.8  set_vector init 5');
is($record->options->[5]->init,5,'record 3.8  set_vector init 6');


$record = model::problem::init_record->new(record_arr => ['BLOCK(1) 28 FIXED']);
is ($record->options->[0]->init, 28, 'record 4 init');
is ($record->fix, 1, 'record 4 fix');
my $a = $record->_format_record;
@str = split /\s+/, $$a[0];
is ($str[2], 'FIX', "_format_record 4");
is ($str[3], 28, "_format record 4");


$record = model::problem::theta->new(record_arr => ['2 ; Malta', '; Corse']);
is ($record->comment->[0], "; Corse\n", "init_record full line comment");
$r = $record->_format_record;
is ($r->[1], "; Corse\n", "init_record full line comment after _format_record");

$record = model::problem::omega->new(record_arr => ['$OMEGA','(0.01642,FIXED) 0.112 FIXED  (FIXED, 1.0724 )',
														  ' (0.45)   ;; ETA(4)',
														  '  0.4 SD CHOLESK 0.09     ;; ETA(6)']);

is_deeply($record->get_estimated_coordinate_strings,
		  ['OMEGA(4,4)','OMEGA(5,5)','OMEGA(6,6)'],
		  'estimated coordinate strings 4');
is_deeply($record->get_estimated_coordinate_strings(only_eta_eps=>1),
		  [4,5,6],
		  'estimated coordinate strings 4b');

is ($record->fix, 0, 'record 5 fix');
is ($record->options->[0]->init, 0.01642, 'record 5 1 init');
ok ($record->options->[0]->fix, 'record 5 1 fix');
ok (!$record->options->[0]->sd, 'record 5 1 sd');
ok (!$record->options->[0]->chol, 'record 5 1 cholesky');

is ($record->options->[1]->init, 0.112, 'record 5 2 init');
is ($record->options->[1]->fix, 1, 'record 5 2 fix');

is ($record->options->[2]->init, 1.0724, 'record 5 3 init');
is ($record->options->[2]->fix, 1, 'record 5 3 fix');

is ($record->options->[3]->init, 0.45, 'record 5 4 init');
is ($record->options->[3]->fix, 0, 'record 5 4 fix');

is ($record->options->[4]->init, 0.4, 'record 5 5 init');
is ($record->options->[4]->fix, 0, 'record 5 5 fix');
is ($record->options->[4]->sd, 1, 'record 5 5 sd');
is ($record->options->[4]->chol, 1, 'record 5 5 cholesky');

is ($record->options->[5]->init, 0.09, 'record 5 6 init');
is ($record->options->[5]->fix, 0, 'record 5 6 fix');
is ($record->options->[5]->sd, 0, 'record 5 6 sd');
is ($record->options->[5]->chol, 0, 'record 5 6 cholesky');

$record->unfix();
is ($record->fix, 0, 'record unfix');
is ($record->options->[0]->fix, 0, 'record 0 unfix');
is ($record->options->[1]->fix, 0, 'record 1 unfix');
is ($record->options->[2]->fix, 0, 'record 2 unfix');
is ($record->options->[3]->fix, 0, 'record 3 unfix');
is ($record->options->[4]->fix, 0, 'record 4 unfix');
is ($record->options->[5]->fix, 0, 'record 5 unfix');

# DIAGONAL SD
$record = model::problem::omega->new(record_arr => [ '$OMEGA (1 SD) (28)' ]);
ok (!$record->sd, 'DIAGONAL SD 1 sd');
ok ($record->options->[0]->sd, "DIAGONAL SD 1 0 sd");
ok (!$record->options->[1]->sd, "DIAGONAL SD 1 1 sd"); 

$record = model::problem::omega->new(record_arr => [ '$OMEGA (1) (STANDARD 28)' ]);
$vector = $record->get_vector();
is_deeply($vector,[1,28],"get vector 2");
ok (!$record->sd, 'DIAGONAL SD 2 sd');
ok (!$record->options->[0]->sd, "DIAGONAL SD 2 0 sd");
ok ($record->options->[1]->sd, "DIAGONAL SD 2 1 sd"); 

$record = model::problem::omega->new(record_arr => [ '$OMEGA DIAGONAL(2) 1 (28 STANDARD)' ]);
$vector = $record->get_vector();
is_deeply($vector,[1,28],"get vector 3");
ok (!$record->sd, 'DIAGONAL SD 3 sd');
ok (!$record->options->[0]->sd, "DIAGONAL SD 3 0 sd");
ok ($record->options->[1]->sd, "DIAGONAL SD 3 1 sd"); 

$record->set_1_fix();
$vector = $record->get_vector();
is_deeply($vector,[1,1],"get vector 4");


# DIAGONAL VARIANCE
$record = model::problem::omega->new(record_arr => [ '$OMEGA (1 VARIANCE) (28)' ]);
ok (!$record->sd, 'DIAGONAL VARIANCE 1 sd');
ok (!$record->options->[0]->sd, "DIAGONAL VARIANCE 1 0 sd");
ok (!$record->options->[1]->sd, "DIAGONAL VARIANCE 1 1 sd"); 

$record = model::problem::omega->new(record_arr => [ '$OMEGA (SD 1) (28 VARIANCE)' ]);
ok (!$record->sd, 'DIAGONAL VARIANCE 2 sd');
ok ($record->options->[0]->sd, "DIAGONAL VARIANCE 2 0 sd");
ok (!$record->options->[1]->sd, "DIAGONAL VARIANCE 2 1 sd"); 

# BLOCK SD/CORR/VARIANCE/COVARIANCE
$record = model::problem::omega->new(record_arr => [ '$OMEGA BLOCK(2) 27 SD 28 29' ]);
ok ($record->sd, 'BLOCK SD/CORR 1 sd');
ok (!$record->corr, 'BLOCK SD/CORR 1 corr');
ok ($record->options->[0]->sd, "BLOCK SD/CORR 1 0 sd");
ok (!$record->options->[1]->sd, "BLOCK SD/CORR 1 1 sd"); 
ok ($record->options->[2]->sd, "BLOCK SD/CORR 1 2 sd"); 
ok (!$record->options->[0]->corr, "BLOCK SD/CORR 1 0 corr");
ok (!$record->options->[1]->corr, "BLOCK SD/CORR 1 1 corr"); 
ok (!$record->options->[2]->corr, "BLOCK SD/CORR 1 2 corr"); 

$record = model::problem::omega->new(record_arr => [ '$OMEGA BLOCK(2) 27 28 29 CORR' ]);
ok (!$record->sd, 'BLOCK SD/CORR 2 sd');
ok ($record->corr, 'BLOCK SD/CORR 2 corr');
ok (!$record->options->[0]->sd, "BLOCK SD/CORR 2 0 sd");
ok (!$record->options->[1]->sd, "BLOCK SD/CORR 2 1 sd"); 
ok (!$record->options->[2]->sd, "BLOCK SD/CORR 2 2 sd"); 
ok (!$record->options->[0]->corr, "BLOCK SD/CORR 2 0 corr");
ok ($record->options->[1]->corr, "BLOCK SD/CORR 2 1 corr"); 
ok (!$record->options->[2]->corr, "BLOCK SD/CORR 2 2 corr"); 

$record = model::problem::omega->new(record_arr => [ '$OMEGA BLOCK(2) 27 28 STANDARD 29 CORR' ]);
ok ($record->sd, 'BLOCK SD/CORR 3 sd');
ok ($record->corr, 'BLOCK SD/CORR 3 corr');
ok ($record->options->[0]->sd, "BLOCK SD/CORR 3 0 sd");
ok (!$record->options->[1]->sd, "BLOCK SD/CORR 3 1 sd"); 
ok ($record->options->[2]->sd, "BLOCK SD/CORR 3 2 sd"); 
ok (!$record->options->[0]->corr, "BLOCK SD/CORR 3 0 corr");
ok ($record->options->[1]->corr, "BLOCK SD/CORR 3 1 corr"); 
ok (!$record->options->[2]->corr, "BLOCK SD/CORR 3 2 corr"); 

$record = model::problem::omega->new(record_arr => [ '$OMEGA BLOCK(2) 27 28 SD 29 COVARIANCE' ]);
ok ($record->sd, 'BLOCK SD/CORR 4 sd');
ok (!$record->corr, 'BLOCK SD/CORR 4 corr');
ok ($record->options->[0]->sd, "BLOCK SD/CORR 4 0 sd");
ok (!$record->options->[1]->sd, "BLOCK SD/CORR 4 1 sd"); 
ok ($record->options->[2]->sd, "BLOCK SD/CORR 4 2 sd"); 
ok (!$record->options->[0]->corr, "BLOCK SD/CORR 4 0 corr");
ok (!$record->options->[1]->corr, "BLOCK SD/CORR 4 1 corr"); 
ok (!$record->options->[2]->corr, "BLOCK SD/CORR 4 2 corr"); 

$record = model::problem::omega->new(record_arr => [ '$OMEGA BLOCK(2) 27 VARIANCE 28 29 CORR' ]);
ok (!$record->sd, 'BLOCK SD/CORR 2 sd');
ok ($record->corr, 'BLOCK SD/CORR 2 corr');
ok (!$record->options->[0]->sd, "BLOCK SD/CORR 2 0 sd");
ok (!$record->options->[1]->sd, "BLOCK SD/CORR 2 1 sd"); 
ok (!$record->options->[2]->sd, "BLOCK SD/CORR 2 2 sd"); 
ok (!$record->options->[0]->corr, "BLOCK SD/CORR 2 0 corr");
ok ($record->options->[1]->corr, "BLOCK SD/CORR 2 1 corr"); 
ok (!$record->options->[2]->corr, "BLOCK SD/CORR 2 2 corr"); 


#case1
$record = model::problem::omega->new(record_arr => [ '$OMEGA 0.4 0.25 FIXED' ]);
ok (!$record->fix, 'DIAGONAL case 1 dim 2 1 FIX rec ');
ok (!$record->options->[0]->fix, "DIAGONAL case 1 dim 2 1 FIX opt 1");
ok ($record->options->[1]->fix, "DIAGONAL case 1 dim 2 1 FIX opt 2"); 
is ($record->get_size(),2,'record size is 2');

#case2
$record = model::problem::omega->new(record_arr => [ '$OMEGA (0.4 FIXED) 0.25 ' ]);
ok (!$record->fix, 'DIAGONAL case 2 dim 2 1 FIX rec ');
ok ($record->options->[0]->fix, "DIAGONAL case 2 dim 2 1 FIX opt 1");
ok (!$record->options->[1]->fix, "DIAGONAL case 2 dim 2 1 FIX opt 2"); 

#case3
$record = model::problem::omega->new(record_arr => [ '$OMEGA 0.4 ( 0.25 FIX)' ]);
ok (!$record->fix, 'DIAGONAL case 3 dim 2 1 FIX rec ');
ok (!$record->options->[0]->fix, "DIAGONAL case 3 dim 2 1 FIX opt 1");
ok ($record->options->[1]->fix, "DIAGONAL case 3 dim 2 1 FIX opt 2"); 

#case4
$record = model::problem::omega->new(record_arr => [ '$OMEGA 0.4 FIXED 0.25 ' ]);
ok (!$record->fix, 'DIAGONAL case 4 dim 2 1 FIX rec ');
ok ($record->options->[0]->fix, "DIAGONAL case 4 dim 2 1 FIX opt 1");
ok (!$record->options->[1]->fix, "DIAGONAL case 4 dim 2 1 FIX opt 2"); 

#5 fails
dies_ok {model::problem::omega->new(record_arr => [ '$OMEGA FIXED 0.4  0.25 ' ])} "fix option before init";

#case6
$record = model::problem::omega->new(record_arr => [ '$OMEGA (FIX 0.4)  0.25 ' ]);
ok (!$record->fix, 'DIAGONAL case 6 dim 2 1 FIX rec ');
ok ($record->options->[0]->fix, "DIAGONAL case 6 dim 2 1 FIX opt 1");
ok (!$record->options->[1]->fix, "DIAGONAL case 6 dim 2 1 FIX opt 2"); 


#case8
$record = model::problem::omega->new(record_arr => [ '$OMEGA 0.4 (FIX 0.25) ' ]);
ok (!$record->fix, 'DIAGONAL case 8 dim 2 1 FIX rec ');
ok (!$record->options->[0]->fix, "DIAGONAL case 8 dim 2 1 FIX opt 1");
ok ($record->options->[1]->fix, "DIAGONAL case 8 dim 2 1 FIX opt 2"); 

#case11
$record = model::problem::omega->new(record_arr => [ '$OMEGA 0.4 0.25 STANDARD' ]);
ok (!$record->sd, 'DIAGONAL case 11 dim 2 1 SD rec ');
ok (!$record->options->[0]->sd, "DIAGONAL case 11 dim 2 1 SD opt 1");
ok ($record->options->[1]->sd, "DIAGONAL case 11 dim 2 1 SD opt 2"); 

#case12
$record = model::problem::omega->new(record_arr => [ '$OMEGA (0.4 STANDARD) 0.25 ' ]);
ok (!$record->sd, 'DIAGONAL case 2 dim 2 1 SD rec ');
ok ($record->options->[0]->sd, "DIAGONAL case 12 dim 2 1 SD opt 1");
ok (!$record->options->[1]->sd, "DIAGONAL case 12 dim 2 1 SD opt 2"); 

#case13
$record = model::problem::omega->new(record_arr => [ '$OMEGA 0.4 ( 0.25 SD)' ]);
ok (!$record->sd, 'DIAGONAL case 13 dim 2 1 SD rec ');
ok (!$record->options->[0]->sd, "DIAGONAL case 13 dim 2 1 SD opt 1");
ok ($record->options->[1]->sd, "DIAGONAL case 13 dim 2 1 SD opt 2"); 

#case14
$record = model::problem::omega->new(record_arr => [ '$OMEGA 0.4 STANDARD 0.25 ' ]);
ok (!$record->sd, 'DIAGONAL case 14 dim 2 1 SD rec ');
ok ($record->options->[0]->sd, "DIAGONAL case 14 dim 2 1 SD opt 1");
ok (!$record->options->[1]->sd, "DIAGONAL case 14 dim 2 1 SD opt 2"); 

#15 fails
dies_ok {model::problem::omega->new(record_arr => [ '$OMEGA STANDARD 0.4  0.25 ' ])} "fix option before init";

#case16
$record = model::problem::omega->new(record_arr => [ '$OMEGA (SD 0.4)  0.25 ' ]);
ok (!$record->sd, 'DIAGONAL case 16 dim 2 1 SD rec ');
ok ($record->options->[0]->sd, "DIAGONAL case 16 dim 2 1 SD opt 1");
ok (!$record->options->[1]->sd, "DIAGONAL case 16 dim 2 1 SD opt 2"); 


#17 fails
dies_ok {model::problem::omega->new(record_arr => [ '$OMEGA 0.4  0.25 CORR' ])} "corr option to diag";

#case18
$record = model::problem::omega->new(record_arr => [ '$OMEGA 0.4 (SD 0.25) ' ]);
ok (!$record->sd, 'DIAGONAL case 18 dim 2 1 SD rec ');
ok (!$record->options->[0]->sd, "DIAGONAL case 18 dim 2 1 SD opt 1");
ok ($record->options->[1]->sd, "DIAGONAL case 18 dim 2 1 SD opt 2"); 

#case19
$record = model::problem::omega->new(record_arr => [ '$OMEGA 2,FIXED' ]);
ok ($record->options->[0]->fix, "case 19 fix");
is ($record->options->[0]->init, 2, "case 19 init");

# set_random_inits for degree > 1
$record = model::problem::init_record->new(record_arr => ['BLOCK(2) 0.02','0 0.01']);
$record->set_random_inits(degree => 2);
is($record->options->[0]->init, 0.049875, 'record 1 init 0 degee=2');
is($record->options->[1]->init, 0, 'record 1 init 1 degree=2');
is($record->options->[2]->init, 0.005443, 'record 1 init 2 degree=2');

# Multiple FIX in BLOCK
dies_ok { model::problem::init_record->new(record_arr => ['BLOCK(2) 28 FIX', '19 FIX']) } "Multiple FIX in BLOCK dies";
lives_ok { model::problem::init_record->new(record_arr => ['BLOCK(2) 28 FIX', '19']) } "One FIX in BLOCK lives";

done_testing();
