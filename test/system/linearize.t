#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More tests=>12;
use File::Copy 'cp';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_linearize');
chdir($tempdir);
my $model_dir = $includes::testfiledir;

# model basenames to test (2 models)
my @model_basenames = (
    "mox1",
    "mox1_etasfile"
);
my @commands = ();
foreach my $model (@model_basenames) {
    push @commands, get_command('linearize')." $model_dir/$model.mod"
}

# simple execution tests (1 test per model)
foreach my $command (@commands){
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

# some linearized model and lst tests (5 tests per execution)
foreach my $model_basename (@model_basenames) {
	# names of model, lst and $ETAS input file
	my $model_file = $model_basename."_linbase.mod";
	my $lst_file   = $model_basename."_linbase.lst";
	my $etas_file  = "mox1.phi";

	# test model file contents
	SKIP: {
		ok (-e $model_file, "$model_file, linearized model, should exist")
			or skip "can't test model file", 2;

		my ($est_correct, $etas_correct);
		open (my $fh, $model_file);
		while (my $line = <$fh>) {
			# test $EST record for MCETA=1 set by linearize
			if ($line =~ /\$EST.+MCETA=1/) {
				$est_correct = $line;
			}
			# test $ETAS record for existence and FILE= set by linearize
			if ($line =~ /\$ETAS.+FILE=.+$etas_file/) {
				$etas_correct = $line;
			}
		}
		ok($est_correct, "$model_file, \$EST should have correct MCETA attr")
			and note("good line: $est_correct");
		ok($etas_correct, "$model_file, \$ETAS should exist and have correct FILE attr")
			and note("good line: $etas_correct");
	}

	# test lst file contents
	SKIP: {
		ok (-e $lst_file, "$lst_file, linearized model lst, should exist")
			or skip "can't test lst file", 1;

		my $loaded_etas;
		open (my $fh, $lst_file);
		while (my $line = <$fh>) {
			# test NONMEM loaded the correct $ETAS input file
			if ($line =~ /LOADED.+DATA FROM FILE $etas_file/) {
				$loaded_etas = $line;
				last;
			}
		}
		ok(defined $loaded_etas, "$lst_file, should successfully have loaded $etas_file")
			and note("good line: $loaded_etas");
	}

}

remove_test_dir($tempdir);

done_testing();
