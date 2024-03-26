package includes;

use strict;
use File::Spec;
use Cwd;
use Test::More;
use File::Path 'rmtree';
use File::Copy 'copy';
use Config;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(get_major_minor_nm_version get_command get_command_without_args get_psn_options cmp_float cmp_float_matrix cmp_float_array create_test_dir remove_test_dir copy_test_files like_file_row unlike_file_row do_course_tests cmp_relative redirect_stderr have_pharmpy);

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
our $rplots_testfiledir = $volume.(File::Spec->catdir($directory,'rplots','test_files'));

use OSspecific;
use PsN;

our $version = '';

if (not $PsN::dev) {
	$version = '-' . $PsN::version;
}

# Set 
# PsN_test_tempdir = /full/path/to/existing/empty/directory/
# in the top section of psn.conf, i.e. before any [bracket section]
# if you are testing on a Mac or if you are testing on a cluster. 
# If on cluster the directory must be reachable from all nodes.
my $tempdir = File::Spec->tmpdir;

if (defined $PsN::config and (defined $PsN::config-> {'_'}) and
	defined ($PsN::config -> {'_'} -> {'PsN_test_tempdir'})){
	#use tempdir from psn.conf instead of default
	$tempdir = $PsN::config -> {'_'} -> {'PsN_test_tempdir'};
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
	return $PsN::Rscripts_dir;
}

sub get_command
{
	my $command_name = shift;

    my $args='';
    if ($command_name ne "nmoutput2so" and $command_name ne "sumo" and $command_name ne "runrecord" and
        $command_name ne "covmat" and $command_name ne "psn" and $command_name ne "psn_clean" and $command_name ne "psn_options") {
        $args = ' ' . join(' ', @ARGV);
		if ($command_name ne "boot_scm" and $command_name ne "update_inits" and $command_name ne "update" and 
			$command_name ne "xv_scm" and $command_name ne "frem" and $command_name ne "sse"  and $command_name ne "vpc" and $command_name ne "rawresults" ){
			$args .= ' -abort_on_fail ';
		}
    }
    my $command_line = $path . $command_name . $version . $args;

    return $command_line;
}

sub cmp_float
{
	my $x = shift;
	my $y = shift;
	my $text = shift;

	$x = sprintf("%.11e", $x);
	$y = sprintf("%.11e", $y);

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

sub like_file_row
{
    # Test if a regular expression can match any line in a file
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

sub unlike_file_row
{
    # Test if a regular expression cannot match any line in a file
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

    if (Test::More->builder->is_passing) {
	    chdir $tempdir;		# Move out of test directories
	    rmtree([$dir]);
    } else {
        print "\ntests failed, execution directory '$dir' not cleaned\n";
    }
}

sub is_windows
{
	return ($Config{osname} eq 'MSWin32');
}

sub test_pdf_pages
{
    my $no_r = not defined PsN::get_R_exec();

	my $pdf_files_pages = shift;
	my $no_pdf_files_list = shift;
	foreach my $file (sort { lc($a) cmp lc($b) } keys %{$pdf_files_pages}){
        SKIP: {
            skip "R not installed",1 if ($no_r);
		    ok (-e $file,"pdf $file exists, check that page count is ".$pdf_files_pages->{$file});
        }
	    SKIP: {
		    skip "Cannot check pdf page count automatically",1 if (is_windows or $no_r);
		    is(pdf_page_count($file),$pdf_files_pages->{$file},"auto-check page count is ".$pdf_files_pages->{$file});
		}
	}
	if (defined $no_pdf_files_list){
		foreach my $file (@{$no_pdf_files_list}){
            SKIP: {
                skip "R not installed",1 if ($no_r);
			    ok ((not -e $file),"No $file created");
            }
		}
	}
}

sub pdf_page_count
{
	my $filename = shift;

	my $count = undef;
	my $command = "pdfinfo $filename ".' 2>/dev/null'." | grep ^Pages"; #unix, keep stdout and redirect stderr to /dev/null
	my @outp = readpipe($command);
	if ($outp[0] =~ /^Pages:\s*(\d+)/){
		$count = $1;
	}
	return $count;
}

sub copy_test_files
{
	my $testdir = shift;
	my $array = shift;
	foreach $file (@{$array}) {
		copy("$testfiledir/$file", $testdir) or die "copy_test_files failed: $!";
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
		copy($file, $tempdir . '/.');
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

sub have_pharmpy
{
    my $pharmpy_version = PsN::call_pharmpy('--version');
    if ($pharmpy_version =~ /^\d+\.\d+\.\d+/) {
        return 1;
    } else {
        return 0;
    }
}
1;
