#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>6;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use tool;

#black box testing of data class and progs that are not covered by other test files

our $tempdir = create_test_dir('unit_tool');

chdir($tempdir);

my $dir = tool::get_rundir(create => 1,
						   basename => 'parallel_retries_dir',
						   model_dir_name => 0,
						   modelname => '',
						   directory_option => '');

#use substr to shave off last path slash
is(substr($dir,0,-1),$tempdir.'parallel_retries_dir1','tool get_rundir 1');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 0,
						modelname => '',
						directory_option => undef);

is(substr($dir,0,-1),$tempdir.'parallel_retries_dir2','tool get_rundir 2');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 0,
						modelname => '',
						directory_option => 'newdir');

is(substr($dir,0,-1),$tempdir.'newdir','tool get_rundir 3');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 1,
						modelname => 'run123.mod',
						directory_option => '');

is(substr($dir,0,-1),$tempdir.'run123.dir1','tool get_rundir 4');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 1,
						modelname => 'path/run123.mod',
						directory_option => '');

is(substr($dir,0,-1),$tempdir.'run123.dir1','tool get_rundir 5');

my $newdir=$tempdir.'parallel_retries_dir1'; #created above
chdir($tempdir.'parallel_retries_dir1');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 0,
						modelname => 'run123.mod',
						directory_option => '../updir');
is(substr($dir,0,-1),$tempdir.'updir','tool get_rundir 6');

remove_test_dir($tempdir);

done_testing();
