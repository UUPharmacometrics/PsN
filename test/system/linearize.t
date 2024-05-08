#!/etc/bin/perl

use strict;
use warnings;
use File::Path qw(make_path);
use File::Spec;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

our $tempdir = create_test_dir('system_linearize');
chdir($tempdir);
my $model_dir = $includes::testfiledir;

# models (relative to test files dir) to test (3 models)
my @models = (
    "mox1.mod",
    "mox1_etasfile.mod",
#    "space path/mox1_etasfile_spacepath.mod"
);

# get commands and working directories
my @commands = ();
my @run_dirs = ();
foreach my $model (@models) {
	  my $command = get_command('linearize') . " \"".File::Spec->catdir($model_dir, $model)."\"";
    push @commands, $command;
    (my $vol, my $path, my $file) = File::Spec->splitpath($model);
    my $wd = File::Spec->catdir($vol, $tempdir, $file);
		$wd = $tempdir . $file;
    make_path($wd) unless (-d $wd);
    push @run_dirs, $wd;
}

# simple execution tests (1 test per model)
for my $i (0 .. $#commands) {
    # change working directory
    my $command = $commands[$i];
    my $run_dir = $run_dirs[$i];
    chdir($run_dir);

    # execute linearize command
	note "Running $command in wd $run_dir\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
}

# some linearized model and lst tests (5 tests per execution)
for my $i (0 .. $#commands) {
    # get basename and extensions of non-linear model
    chdir($run_dirs[$i]);
    (undef, undef, my $orig_filename) = File::Spec->splitpath($models[$i]);
    (my $orig_basename = $orig_filename) =~ s/\..*$//;
    my ($orig_ext) = $orig_filename =~ /(\.[^.]*)$/;

	# names of linearized model, lst and $ETAS input file
	my $model_file = $orig_basename."_linbase$orig_ext";
	my $lst_file   = $orig_basename."_linbase.lst";
	my $etas_file  = "mox1.phi";

	# test model file contents
	SKIP: {
		ok (-e $model_file, "$model_file, linearized model, should exist")
			or skip "tests on $model_file and LST (missing in $run_dirs[$i])", 4;

		my ($est_correct, $etas_correct);
		open (my $fh, $model_file);
		while (my $line = <$fh>) {
			# test $EST record for MCETA=1 set by linearize
			if ($line =~ /\$EST.+MCETA=1/) {
				$est_correct = $line;
			}
			# test $ETAS record for existence and FILE= set by linearize
			if ($line =~ /FILE=.+$etas_file/) {     # Not checking for $ETAS as it can appear on separate line
				$etas_correct = $line;
			}
		}
		ok($est_correct, "$model_file, \$EST should have correct MCETA attr")
			and note("good line: $est_correct");
		ok($etas_correct, "$model_file, \$ETAS should exist and have correct FILE attr")
			and note("good line: $etas_correct");

        # test lst file contents
        SKIP: {
            ok (-e $lst_file, "$lst_file, linearized model lst, should exist")
                or skip "tests on LST from $model_file", 1;

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
}

if (Test::More->builder->is_passing)
{
	remove_test_dir($tempdir);
} else {
    diag "some tests failed, run directory $tempdir not cleaned";
}

done_testing();
