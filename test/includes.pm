package includes;

use strict;
use File::Spec;
use Cwd;
use Test::More;
use File::Path 'rmtree';
use File::Copy 'cp';
use Config;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(get_major_minor_nm_version get_command get_command_without_args get_psn_options cmp_float cmp_float_matrix cmp_float_array create_test_dir remove_test_dir copy_test_files like_file_row unlike_file_row do_course_tests cmp_relative redirect_stderr);

# Change the $tempdir variable to the full path of an existing empty folder if you are testing on a Mac
# Change the $tempdir variable to the full path of an existing empty folder accessible from all nodes if you are testing on a cluster
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

if (not $PsN::dev) {
	$version = '-' . $PsN::version;
}

sub redirect_stderr
{
	my $redirectstderr = ' 2> /dev/null ';
	if ($Config{osname} eq 'MSWin32'){
		$redirectstderr = ' 2> nul ';
	}
	return $redirectstderr;
}


sub get_command_without_args
{
	my $command_name = shift;

    my $command_line = $path . $command_name . $version;

    return $command_line;
}

sub get_psn_options
{
	my %options = ();
	foreach my $arg (@ARGV){
		next unless $arg =~ /^-/;
		if ($arg =~ /^-no-(.*)/){
			$options{$1}=0;
		}elsif($arg =~ /^-(.*)=(.*)/){
			$options{$1}=$2;
		}elsif($arg =~ /^-(.*)/){
			$options{$1}=1
		}
	}
    return \%options;
}

sub get_major_minor_nm_version
{
	require PsN;
	my $options = get_psn_options;
	my $version= 'default';
	if (defined $options->{'nm_version'}){
		$version = $options->{'nm_version'};
	}
	PsN::set_nonmem_info($version);
	return ($PsN::nm_major_version,$PsN::nm_minor_version,$version);
	
}

sub get_template_directory_rplots
{
	require PsN;
	my $template_dir = $PsN::lib_dir.'/R-scripts';
	unless (-d $template_dir){
		#development directory structure
		$template_dir = $PsN::lib_dir.'/../R-scripts';
	}
	return $template_dir;
}

	
sub get_command
{
	my $command_name = shift;

    my $args;
    if ($command_name ne "data_stats" and $command_name ne "nmoutput2so" and $command_name ne "sumo" and $command_name ne "runrecord" and
        $command_name ne "covmat" and $command_name ne "psn" and $command_name ne "psn_clean" and $command_name ne "psn_options") {
        $args = ' ' . join(' ', @ARGV);
    }
    my $command_line = $path . $command_name . $version . $args;

    return $command_line;
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

sub cmp_relative
{
	my $x = shift;
	my $y = shift;
	my $tol = shift;
	my $text = shift;

	$x = sprintf("%.".$tol."e", $x);
	$y = sprintf("%.".$tol."e", $y);

	cmp_ok($x, '==', $y, $text);
}

sub cmp_float_array
{
    my $x = shift;
    my $y = shift;
    my $text = shift;

    foreach my $e (@$x) {
        $e = sprintf("%.8e", $e);
    }
    foreach my $e (@$y) {
        $e = sprintf("%.8e", $e);
    }

    is_deeply($x, $y, $text);
}
        
sub cmp_float_matrix
{
    my $A = shift;
    my $B = shift;
    my $text = shift;

    my $new_A = [ @$A ];
    my $new_B = [ @$B ];

    foreach my $row (@$new_A) {
        foreach my $e (@$row) {
            $e = sprintf("%.8e", $e);
        }
    }

    foreach my $row (@$new_B) {
        foreach my $e (@$row) {
            $e = sprintf("%.8e", $e);
        }
    }

    is_deeply($new_A, $new_B, $text);
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
		cp("$testfiledir/$file", $testdir) or die "copy_test_files failed: $!";
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

	(my $commands, my $comments) = _parse_course_file("$dir/$course_name.txt");

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
