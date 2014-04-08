#!/etc/bin/perl


use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>1;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir();
our $dir = "$tempdir/gls_test";
my $model_dir = $includes::testfiledir;

my $command = $includes::gls." $model_dir/glstags.mod -samples=3 -set_simest -ind_shrink -dir=$dir";

my  $rc = system($command);
$rc = $rc >> 8;

ok ($rc == 0, "gls that should run ok");

remove_test_dir($tempdir);


done_testing();
