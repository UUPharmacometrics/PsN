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


#list_candidate_latest_rundirs
my $dir = create_test_dir("unit_tool_list_candidate_latest_rundirs");

chdir($dir);
mkdir "modelfit_dir28";
my @candidates = tool::list_candidate_latest_rundirs("jason");
is_deeply(\@candidates, [ "modelfit_dir28" ], "list_candidate_latest_rundirs one modelfit");

mkdir "modelfit_dir1";
@candidates = tool::list_candidate_latest_rundirs("jason");
is_deeply(\@candidates, [ "modelfit_dir28" ], "list_candidate_latest_rundirs two modelfit");

mkdir "modelfit_dir29";
@candidates = tool::list_candidate_latest_rundirs("jason");
is_deeply(\@candidates, [ "modelfit_dir29" ], "list_candidate_latest_rundirs three modelfit");

mkdir "jason-PsN-2015-02-26-134234";
@candidates = tool::list_candidate_latest_rundirs("jason");
is_deeply(\@candidates, [ "jason-PsN-2015-02-26-134234", "modelfit_dir29" ], "list_candidate_latest_rundirs one dated three modelfit");

mkdir "jason-PsN-2015-04-26-134234";
@candidates = tool::list_candidate_latest_rundirs("jason");
is_deeply(\@candidates, [ "jason-PsN-2015-04-26-134234", "modelfit_dir29" ], "list_candidate_latest_rundirs two dated three modelfit");

mkdir "jason-PsN-2013-04-26-134234";
@candidates = tool::list_candidate_latest_rundirs("jason");
is_deeply(\@candidates, [ "jason-PsN-2015-04-26-134234", "modelfit_dir29" ], "list_candidate_latest_rundirs three dated three modelfit");

mkdir "jason.dir99";
@candidates = tool::list_candidate_latest_rundirs("jason.ctl");
is_deeply(\@candidates, [ "jason.dir99", "jason-PsN-2015-04-26-134234", "modelfit_dir29" ], "list_candidate_latest_rundirs one named three dated three modelfit");

mkdir "jason.dir100";
@candidates = tool::list_candidate_latest_rundirs("jason");
is_deeply(\@candidates, [ "jason.dir100", "jason-PsN-2015-04-26-134234", "modelfit_dir29" ], "list_candidate_latest_rundirs two named three dated three modelfit");

mkdir "jason.dir2";
@candidates = tool::list_candidate_latest_rundirs("jason");
is_deeply(\@candidates, [ "jason.dir100", "jason-PsN-2015-04-26-134234", "modelfit_dir29" ], "list_candidate_latest_rundirs three named three dated three modelfit");

rmdir "modelfit_dir29";
@candidates = tool::list_candidate_latest_rundirs("jason");
is_deeply(\@candidates, [ "jason.dir100", "jason-PsN-2015-04-26-134234", "modelfit_dir28" ], "list_candidate_latest_rundirs three named three dated two modelfit");



remove_test_dir($dir);



remove_test_dir($tempdir);

done_testing();
