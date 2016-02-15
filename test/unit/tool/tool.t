#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use tool;

ui -> silent(1);
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

my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =localtime(time);
my $timestring = sprintf("-PsN-%s-%02i-%02i",($year+1900),($mon+1),$mday);
my $dir1 = tool::get_rundir(create => 1,
						basename => 'parallel_retries_dir',
						timestamp => 1,
						model_dir_name => 0,
						modelname => 'run123.mod');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						timestamp => 1,
						model_dir_name => 0,
						modelname => 'run123.mod');

#test will fail if done exactly across midnight
is (substr($dir1,0,length($tempdir.'run123'.$timestring)),$tempdir.'run123'.$timestring,'tool get_rundir timestamp');
#test will fail if change second between $dir1 and $dir above
#is (substr($dir,0,-1) =~ /dir1$/,1,'tool get_rundir timestamp numbered');
#print $dir1."\n$dir\n";

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


# compress_m1
my $dir = "$tempdir/bootstrap_dir1";
mkdir($dir);
mkdir("$dir/m1");
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
copy_test_files("$dir/m1", ["bootstrap/bootstrap_dir1/m1/bs_pr1_1.cor"]);
my $model = model->create_dummy_model();
my $tool = tool->new(models => [ $model ], directory => $dir);

$tool->compress_m1();

ok (-e "$dir/m1.zip", "compress_m1 m1.zip exists");
ok (not (-e "$dir/m1"), "compress_m1 m1 folder was removed");
ok (-e "$dir/command.txt", "compress_m1 command.txt still exists");

# uncompress_m1

$tool->uncompress_m1();

ok (not (-e "$dir/m1.zip"), "uncompress_m1 m1.zip was removed");
ok (-e "$dir/m1", "uncompress_m1 m1 folder was created");
ok (-e "$dir/m1/bs_pr1_1.cor", "uncompress_m1 file inside m1 was created");
ok (-e "$dir/command.txt", "uncompress_m1 command.txt still exists");

# tool->new with compressed m1
$tool->compress_m1();
my $tool = tool->new(models => [ $model ], directory => $dir);

ok (not (-e "$dir/m1.zip"), "tool->new m1.zip was removed");
ok (-e "$dir/m1", "tool->new m1 folder was created");
ok (-e "$dir/m1/bs_pr1_1.cor", "tool->new file inside m1 was created");
ok (-e "$dir/command.txt", "tool->new command.txt still exists");



remove_test_dir($tempdir);

done_testing();
