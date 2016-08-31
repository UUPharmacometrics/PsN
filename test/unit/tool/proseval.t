#!/etc/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages
use tool::proseval;
use ui;

our $tempdir = create_test_dir('unit_proseval');
chdir $tempdir;

my $model;
my $proseval;

# Model with no EVID
$model = model->new(filename => $includes::testfiledir."/pheno.mod", ignore_missing_data => 1);
dies_ok { $proseval = tool::proseval->new(models => [ $model ]); } "proseval: BUILD with no EVID";

# Model with EVID
$model = model->new(filename => $includes::testfiledir."/mox1.mod", ignore_missing_data => 1);
$proseval = tool::proseval->new(models => [ $model ]);

# set_evid
my $data = data->new(
    filename => $includes::testfiledir."/mox_simulated.csv",
    idcolumn => 1,
);

my $continue = $proseval->set_evid(dataset => $data, numzeros => 1, evid_column => $proseval->evid_column);
is ($continue, 1, "set_evid continue");

my @evid;
for my $line (@{$data->individuals->[0]->subject_data}) {
    my @a = split(/,/, $line);
    push @evid, int($a[scalar(@a) - 2]);
}

is_deeply(\@evid, [1, 0, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 2], "set_evid numzeros=1");

$continue = $proseval->set_evid(dataset => $data, numzeros => 20, evid_column => $proseval->evid_column);
is ($continue, 0, "set_evid no continue");


remove_test_dir($tempdir);

done_testing();
