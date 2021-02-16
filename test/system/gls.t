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

# gls runtest with (ONLY OBSERVATIONS) which should work now that code_record.pm is aware of pseudo-assignments
my $command = get_command('gls') . " $model_dir/glstags-pseudoassign.mod -seed=12345 -samples=5 -set_simest -ind_shrink -dir=glsdir-pseudoassign";
my $rc = system($command);
$rc = $rc >> 8;
ok ($rc == 0, "gls with pseudo-assignment that should run ok");
ok (-e "glsdir-pseudoassign/m1/gls4tab-gls", "gls table created");

remove_test_dir($tempdir);

done_testing();
