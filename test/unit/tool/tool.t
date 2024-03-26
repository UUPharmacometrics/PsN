#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use File::Copy 'copy';
use Test::More;
use Test::Exception;
use Cwd 'abs_path';
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use tool;
use model;
use Config;

sub abspath
{
    my $path = shift;
    if ($Config{'osname'} ne 'MSWin32') {
        $path = abs_path($path);
    } else {
        $path =~ s|/|\\|g;
    }
    return $path;
}

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
is(abspath(substr($dir, 0, -1)), File::Spec->catfile(abspath($tempdir), 'parallel_retries_dir1'), 'tool get_rundir 1');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 0,
						modelname => '',
						directory_option => undef);

is(abspath(substr($dir, 0, -1)), File::Spec->catfile(abspath($tempdir), 'parallel_retries_dir2'), 'tool get_rundir 2');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 0,
						modelname => '',
						directory_option => 'newdir');

is(abspath(substr($dir, 0, -1)), File::Spec->catfile(abspath($tempdir), 'newdir'), 'tool get_rundir 3');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 1,
						modelname => 'run123.mod',
						directory_option => '');

is(abspath(substr($dir, 0, -1)), File::Spec->catfile(abspath($tempdir), 'run123.dir1'), 'tool get_rundir 4');

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
my $correct = abspath($tempdir . 'run123' . $timestring);
is (substr(abspath($dir1), 0, length($correct)), $correct, 'tool get_rundir timestamp');
#test will fail if change second between $dir1 and $dir above

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 1,
						modelname => 'path/run123.mod',
						directory_option => '');

is(abspath(substr($dir, 0, -1)), File::Spec->catfile(abspath($tempdir), 'run123.dir1'), 'tool get_rundir 5');

my $newdir=$tempdir.'parallel_retries_dir1'; #created above
chdir($tempdir.'parallel_retries_dir1');

$dir = tool::get_rundir(create => 0,
						basename => 'parallel_retries_dir',
						model_dir_name => 0,
						modelname => 'run123.mod',
						directory_option => '../updir');

is(abspath(substr($dir, 0, -1)), File::Spec->catfile(abspath($tempdir), 'updir'), 'tool get_rundir 6');


# compress_m1
$dir = "$tempdir/bootstrap_dir1";
mkdir($dir);
mkdir("$dir/m1");
my $m1_zip = "$dir/m1.zip";
my $m1_dir = "$dir/m1";
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
copy_test_files("$dir/m1", ["bootstrap/bootstrap_dir1/m1/bs_pr1_1.cor"]);
my $model = model->create_dummy_model();
my $tool = tool->new(models => [ $model ], directory => $dir);

$tool->compress_m1();

ok (-e "$dir/m1.zip", "compress_m1 m1.zip exists");
ok (not (-e "$dir/m1"), "compress_m1 m1 folder was removed");
ok (-e "$dir/command.txt", "compress_m1 command.txt still exists");

# uncompress_m1

mkdir("$tempdir/bak");
copy("$dir/m1.zip", "$tempdir/bak");

# m1.zip exists and is non-empty, m1 does not exist
$tool->uncompress_m1();

ok (not (-e "$dir/m1.zip"), "uncompress_m1 m1.zip was removed");
ok (-e "$dir/m1", "uncompress_m1 m1 folder was created");
ok (-e "$dir/m1/bs_pr1_1.cor", "uncompress_m1 file inside m1 was created");
ok (-e "$dir/command.txt", "uncompress_m1 command.txt still exists");

# m1=empty, m1.zip=empty
rmtree([$dir]);
mkdir($dir);
mkdir($m1_dir);
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
open my $fh, ">", $m1_zip; 
close $fh;
$tool->uncompress_m1();
ok (-e "$m1_dir", "uncompress_m1 (empty, empty) m1 exists");
ok (not (-e "$m1_zip"), "uncompress_m1 (empty, empty) m1.zip does not exist"); 
ok (-e "$dir/command.txt", "uncompress_m1 (empty, empty) command.txt still exists");

# m1=empty, m1.zip=non-existing
rmtree([$dir]);
mkdir($dir);
mkdir($m1_dir);
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
$tool->uncompress_m1();
ok (-e "$m1_dir", "uncompress_m1 (empty, non-existing) m1 exists");
ok (-e "$dir/command.txt", "uncompress_m1 (empty, non-existing) command.txt still exists");

# m1=empty, m1.zip=not-empty
rmtree([$dir]);
mkdir($dir);
mkdir($m1_dir);
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
copy("$tempdir/bak/m1.zip", $dir);
$tool->uncompress_m1();
ok (-e $m1_dir, "uncompress_m1 (empty, exists) m1 created");
ok (-e "$dir/command.txt", "uncompress_m1 (empty, exists) command.txt still exists");
ok (-e "$dir/m1/bs_pr1_1.cor", "uncompress_m1 (empty, non-existing) inside m1 was created");
ok (not (-e "$dir/m1.zip"), "uncompress_m1 (empty, non-existing) m1.zip was removed");

# m1=not-empty, m1.zip=empty
rmtree([$dir]);
mkdir($dir);
mkdir($m1_dir);
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
copy_test_files("$dir/m1", ["bootstrap/bootstrap_dir1/m1/bs_pr1_1.cor"]);
open $fh, ">", $m1_zip; 
close $fh;
$tool->uncompress_m1();
ok (-e $m1_dir, "uncompress_m1 (non-empty, empty) m1 still exists");
ok (-e "$dir/command.txt", "uncompress_m1 (non-empty, empty) command.txt still exists");
ok (-e "$dir/m1/bs_pr1_1.cor", "uncompress_m1 (non-empty, empty) inside m1 still exists");
ok (not (-e "$dir/m1.zip"), "uncompress_m1 (non-empty, empty) m1.zip was removed");

# m1=not-empty, m1.zip=non-existing
rmtree([$dir]);
mkdir($dir);
mkdir($m1_dir);
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
copy_test_files("$dir/m1", ["bootstrap/bootstrap_dir1/m1/bs_pr1_1.cor"]);
$tool->uncompress_m1();
ok (-e $m1_dir, "uncompress_m1 (non-empty, non-existing) m1 still exists");
ok (-e "$dir/command.txt", "uncompress_m1 (non-empty, non-existing) command.txt still exists");
ok (-e "$dir/m1/bs_pr1_1.cor", "uncompress_m1 (non-empty, non-existing) inside m1 still exists");
ok (not (-e "$dir/m1.zip"), "uncompress_m1 (non-empty, non-existing) m1.zip still does not exist");

#m1=not-empty, m1.zip=not-empty
rmtree([$dir]);
mkdir($dir);
mkdir($m1_dir);
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
copy_test_files("$dir/m1", ["bootstrap/bootstrap_dir1/m1/bs_pr1_1.cor"]);
copy("$tempdir/bak/m1.zip", $dir);
dies_ok { $tool->uncompress_m1() } "uncompress_m1 (non-empty, non-empty) dies";

#m1=non-existing, m1.zip=empty
rmtree([$dir]);
mkdir($dir);
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
open $fh, ">", $m1_zip; 
close $fh;
$tool->uncompress_m1();
ok (-e "$dir/command.txt", "uncompress_m1 (non-existing, empty) command.txt still exists");
ok (not (-e "$dir/m1.zip"), "uncompress_m1 (non-existing, empty) m1.zip removed");

#m1=non-existing, m1.zip=non-existing
rmtree([$dir]);
mkdir($dir);
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
$tool->uncompress_m1();
ok (-e "$dir/command.txt", "uncompress_m1 (non-existing, non-exisitng) command.txt still exists");


# tool->new with compressed m1
rmtree([$dir]);
mkdir($dir);
mkdir($m1_dir);
copy_test_files($dir, ["bootstrap/bootstrap_dir1/command.txt"]);
copy_test_files("$dir/m1", ["bootstrap/bootstrap_dir1/m1/bs_pr1_1.cor"]);

$tool->compress_m1();
$tool = tool->new(models => [ $model ], directory => $dir);

ok (not (-e "$dir/m1.zip"), "tool->new m1.zip was removed");
ok (-e "$dir/m1", "tool->new m1 folder was created");
ok (-e "$dir/m1/bs_pr1_1.cor", "tool->new file inside m1 was created");
ok (-e "$dir/command.txt", "tool->new command.txt still exists");

is($tool->nm_output,undef,'nm_output default undef');
$tool->add_to_nmoutput(extensions => ['phi','ext']);
is($tool->nm_output,'phi,ext','nm_output added phi, ext');
$tool->add_to_nmoutput(extensions => ['phi','cov']);
is($tool->nm_output,'phi,ext,cov','nm_output added phi, cov');
$tool->add_to_nmoutput(extensions => ['phm','cov']);
is($tool->nm_output,'phi,ext,cov,phm','nm_output added phm, cov');


remove_test_dir($tempdir);

# Mock mkpath to avoid directory creation


done_testing();
