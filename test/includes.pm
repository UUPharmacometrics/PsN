package includes;

use strict;
use File::Spec;
use Cwd;
use Test::More;
use File::Path 'rmtree';
use File::Copy 'cp';

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(cmp_float create_test_dir remove_test_dir copy_test_files like_file_row unlike_file_row);


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

#n41 cluster 
#use lib "/opt/local64/PsN/PsN_3_7_6";
#use lib "/opt/local64/PsN";
#use PsN_3_7_6;
#our $path = "/opt/local64/PsN/bin/";
#our $version = '-3.7.6';

#doris
#use lib "/opt/PsN/PsN_3_7_6";
#use lib "/opt/PsN";
#use PsN_3_7_6;
#our $path = "/opt/PsN/bin/";
#our $version = '-3.7.6';

#MAC
#use lib "/opt/PsN/PsN_3_7_6";
#use lib "/opt/PsN";
#use PsN_3_7_6;
#our $path = "/opt/PsN/bin/";
#our $version = '-3.7.6';

#XP under MAC
#use lib "C:\Perl\site\lib\PsN_3_7_6";
#use lib "C:\Perl\site\lib";
#use PsN_3_7_6;
#our $path = "C:\Perl\bin";
#our $version = ''; #can't have numbers on windows

our $boot_scm = $path.'boot_scm'.$version;
our $bootstrap = $path.'bootstrap'.$version;
our $cdd = $path.'cdd'.$version;
our $crossval = $path.'crossval'.$version;
our $data_stats = $path.'data_stats'.$version;
our $ebe_npde = $path.'ebe_npde'.$version;
our $execute = $path.'execute'.$version;
our $extended_grid = $path.'extended_grid'.$version;
our $frem = $path.'frem'.$version;
our $gls = $path.'gls'.$version;
our $lasso = $path.'lasso'.$version;
our $linearize = $path.'linearize'.$version;
our $llp = $path.'llp'.$version;
our $mcmp = $path.'mcmp'.$version;
our $mimp = $path.'mimp'.$version;
our $nonpb = $path.'nonpb'.$version;
our $npc = $path.'npc'.$version;
our $parallel_retries = $path.'parallel_retries'.$version;
our $pind = $path.'pind'.$version;
our $randtest = $path.'randtest'.$version;
our $runrecord = $path.'runrecord'.$version;
our $scm = $path.'scm'.$version;
our $sse = $path.'sse'.$version;
our $sumo = $path.'sumo'.$version;
our $update_inits = $path.'update_inits'.$version;
our $vpc = $path.'vpc'.$version;
our $xv_scm = $path.'xv_scm'.$version;
our $sir = $path.'sir'.$version;
our $pvar = $path.'pvar'.$version;


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

sub _test_dir_name
{
	return OSspecific::unique_path('PsN-test_dir', File::Spec->tmpdir());
}

sub create_test_dir
{
	my $dir = _test_dir_name;
	rmtree([$dir]);
	mkdir($dir);
	return $dir;
}

sub remove_test_dir
{
	my $dir=shift;
	chdir;		# Move to home in case we are cd:ed into the directory to remove.
	rmtree([$dir]);
}

sub copy_test_files
{
	my $testdir=shift;
	my $array=shift;
	foreach $file (@{$array}) {
		cp("$testfiledir/$file", $testdir);
	}
}

#uncomment for MAC, doris, n41, (5.10.0, 5.8.8, 5.10.1)
#sub done_testing{
#	1;
#}

1;
