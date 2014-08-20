package includes;

use strict;
use File::Spec;
use Cwd;
use Test::More;
use File::Path 'rmtree';
use File::Copy 'cp';

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(get_command cmp_float create_test_dir remove_test_dir copy_test_files like_file_row unlike_file_row is_array do_course_tests);

# Set this variable to something else if you are testing on a cluster
my $tempdir = File::Spec->tmpdir;

# Setup an include path to the lib directory
# First get the path of this module and split out the directory part
# Then make it into an absolute path
BEGIN
{
	# The following row will be edited by setup.pl
	my $libpath = '';
	if ($libpath eq '') {
		my ($volume, $directory, $file) = File::Spec->splitpath(__FILE__);
		$libpath = Cwd::abs_path($volume . $directory . '../lib');
	}
	unshift @INC, $libpath;
}

# Get an absolute path to the scripts
my ($volume, $directory, $file) = File::Spec->splitpath(__FILE__);

# The following row will be edited by setup.pl
our $path = '';
if ($path eq '') {
	$path = Cwd::abs_path($volume . $directory . '../bin');
	$path .= '/';
}

our $testfiledir = Cwd::abs_path($volume . $directory . 'test_files');

use OSspecific;
use PsN;

our $version = '';

unless ($PsN::version eq 'dev'){
	$version = '-'.$PsN::version;
}

sub get_command
{
	my $command_name = shift;

	return $path . $command_name . $version;
}

sub cmp_float
{
	my $x = shift;
	my $y = shift;
	my $text = shift;

	$x = sprintf("%.13e", $x);
	$y = sprintf("%.13e", $y);

	cmp_ok($x, '==', $y, $text);
}

# Test if a regular expression can match any line in a file
sub like_file_row
{
	my $filename = shift;
	my $re = shift;
	my $text = shift;

	if (open my $fh, '<', $filename) {
		while (<$fh>) {
			chomp;
			if (/$re/) {
				close $fh;
				pass($text);
				return;
			}
		}
		fail($text);
		close $fh;
	} else {
		fail($text);
	}
}

# Test if a regular expression cannot match any line in a file
sub unlike_file_row
{
	my $filename = shift;
	my $re = shift;
	my $text = shift;

	if (open my $fh, '<', $filename) {
		while (<$fh>) {
			chomp;
			if (/$re/) {
				close $fh;
				fail($text);
				return;
			}
		}
		pass($text);
		close $fh;
	} else {
		fail($text);
	}
}

sub create_test_dir
{
	my $testname = shift;
	my $dir = OSspecific::unique_path('PsN-test_'.$testname.'_', $tempdir);
	rmtree([$dir]);
	mkdir($dir);
	return $dir;
}

sub remove_test_dir
{
	my $dir = shift;
	chdir $tempdir;		# Move out of test directories
	rmtree([$dir]);
}

sub copy_test_files
{
	my $testdir = shift;
	my $array = shift;
	foreach $file (@{$array}) {
		cp("$testfiledir/$file", $testdir);
	}
}

sub is_array
{
	my $func = shift;
	my $facit = shift;
	my $label = shift;

	is (scalar(@{$func}), scalar(@{$facit}), "$label, equal length");

	my $min = scalar(@{$func});
	$min = scalar(@{$facit}) if (scalar(@{$facit}) < $min);
	for (my $i = 0; $i < $min; $i++) {
		is ($func->[$i], $facit->[$i], "$label, index $i");
	}	
}

sub _parse_course_file
{
	my $file = shift;

	my @commands;
	my @comments;

	open my $fh, '<', $file;

	my $line;
	while ($line = <$fh>) {
		chomp $line;
		next if $line =~ /^\s*$/;
		$line =~ s/^#\s*//;
		push @comments, $line;
		$line = <$fh>;
		chomp $line;
		$line =~ /^\s*(\w+)(\s+.*)$/;
		$line = $path . $1 . $version . $2;
		push @commands, $line;
	}

	close $fh;

	return (\@commands, \@comments);
}

sub do_course_tests
{
	my $dir = shift;
	my $course_name = shift;

	(my $commands, my $comments) = _parse_course_file($course_name . '.txt');

	my $tempdir = create_test_dir("courses_$course_name");

	my $model_dir = "$dir/$course_name";
	my @needed = <$model_dir/*>;
	foreach my $file (@needed) {
		cp($file, $tempdir . '/.');
	}
	chdir($tempdir);

	plan tests => scalar(@$commands);

	foreach my $i (0 .. @$commands - 1) {
		my $command = $$commands[$i];
		my $comment = $$comments[$i];
		print "Running $comment:\n$command\n";
		my $rc = system($command);
		$rc = $rc >> 8;
		ok ($rc == 0, "$comment ");
	}

	chdir($dir);
	remove_test_dir($tempdir);

	done_testing();
}

1;
