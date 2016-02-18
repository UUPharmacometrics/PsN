#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_gls');

my $model_dir = $includes::testfiledir;
chdir($tempdir);
my $command = get_command('gls') . " $model_dir/glstags.mod -samples=5 -set_simest -ind_shrink -dir=glsdir";

my  $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "gls that should run ok");
ok (-e "glsdir/m1/gls4tab-gls", "gls table created");

remove_test_dir($tempdir);


done_testing();
