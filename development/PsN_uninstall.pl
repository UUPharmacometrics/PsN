use strict;
use Config;
use File::Spec;
use File::Path qw(rmtree);
use File::Glob;
use lib 'lib';

my $default_sitelib;
my $default_bin;
my $default_perlpath;

my $directory_separator;
my $is_win7 = 0;
my $is_Vista = 0;
my $binary_dir;
my $library_dir;
my $perl_binary;

setup_globals();

$default_bin = $Config{bin};
if (running_on_windows()) {
	#"trying to find the perl binary via system command\n";
	my $wherebin;
	if ($is_win7 or $is_Vista) {
		$wherebin = `where perl`;
	}else{
		my $command = 'for %i in (perl.exe) do @echo. %~$PATH:i';
		$wherebin=`$command`;
	}
	chomp $wherebin;
	my $local_bin = $wherebin;
	$local_bin =~ s/perl\.exe$//;

	if ((defined $default_bin) and (-e $default_bin) and (-d $default_bin)) {
		#default  ok
		1;
	}elsif(length($local_bin)>0 and -d $local_bin) {
		$default_bin = $local_bin;
	}else{
		$default_bin = undef;
	}
}

print "\nThis is the PsN uninstall script.\n";

print "Directory from which to uninstall PsN utilities [$default_bin]:";
$binary_dir = get_input($default_bin);

unless (-d $binary_dir){
	abort("$binary_dir is not a directory");
}

#find default version, use execute
(my $volume, my $directory, my $file) = File::Spec->splitpath($binary_dir,1);
my $execute = File::Spec->catpath( $volume, $directory, 'execute');
my $defaultversion='notfound';
if (-e $execute){
	$defaultversion=uninstall_version($binary_dir,$execute,1);
}else{
	print "Could not find $execute\n";
}

#find other versions
my @executeversions = <$execute-*>; 

foreach my $file (@executeversions){
	next if ($file =~ /\.bat$/);
	next if ($file eq $defaultversion); #do not ask about this twice
	uninstall_version($binary_dir,$file,0);
}
print "\n\nPress ENTER to exit the uninstall program.\n";
my $dirt = <STDIN>;


sub get_bin_files{
	my $volume = shift;
	my $direcory = shift;
	my $versionref = shift;

	my @utilities = (
		'bootstrap', 'cdd', 'execute', 'llp', 'scm', 'sumo', 'sse',
		'data_stats', 'se_of_eta', 'update_inits', 'update', 'npc', 'vpc',
		'pind','nonpb','extended_grid','psn','psn_options','psn_clean',
		'runrecord','mcmp','lasso','mimp','xv_scm','parallel_retries',
		'boot_scm','gls','ebe_npde','frem','randtest','linearize', 'crossval', 'pvar', 'nca', 'vpctable','sir','rawresults',
		'precond', 'covmat','nmoutput2so'
		);

	my @extensions = ('');
	if (running_on_windows()) {
		push(@extensions,'.bat','.pl');
	}
	my @files=();

	foreach my $util (@utilities){
		foreach my $version (@{$versionref}){
			foreach my $ext (@extensions){
				my $file = File::Spec->catpath( $volume, $directory, $util.$version.$ext);
				if (-e $file){
					push(@files,$file);
				}else{
#					print "Not exist: $file\n";
					1;
				}
			}
		}
	}
	return \@files;
}

sub get_windows_version
{
	if (eval('require Win32')) { 
		my $winver = Win32::GetOSName();
		$is_win7 = 1 if ($winver eq 'Win7');
		$is_Vista = 1 if ($winver eq 'WinVista');
	}
}

sub running_on_windows
{
	return $Config{osname} eq 'MSWin32';
}

sub setup_globals
{
	$| = 1; # Turn on autoflush

	if (running_on_windows()) {
		$directory_separator = "\\";
	} else {
		$directory_separator = "/";
	}
}

sub confirm
{
	while (1) {
		my $input = <STDIN>;
		if ($input =~ /^\s*[yY]\s*$/) {
			return 1;
		} elsif ($input =~ /^\s*[nN]\s*$/) {
			return 0;
		} else {
			print "Please answer y for yes or n for no: ";
		}
	}
}



sub get_lib_dir
{
	my $file = shift;
	my $libpath;
	unless (open(INST, "<", $file)) {
		abort("Could not open " . $file. " for reading\n");
	}
	while (<INST>) {
		if (/\s*use\s+lib\s+[^']*\'(.*PsN_\d+_\d+_\d+)\'/){
			$libpath=$1;
			last;
		} else{
#			print "skipped $_\n";
		}
	}
	close INST;
	return $libpath;		
}


sub abort
{
	my $message = shift;

	print $message;
	print "\n\nThe uninstall process has been aborted.";
	print "\n\nPress ENTER to exit the program.\n";
	my $dirt = <STDIN>;
	exit;
}

sub get_input
{
	my $default = shift;
	my $input = <STDIN>;
	chomp($input);
	if ($input =~ /^\s*$/) {
		if (defined $default) {
			return($default);
		} else {
			abort("No input given and no default available, must exit.\n");
		}
	} else {
		return($input);
	}
}







sub get_psn_version
{
	my $version = shift;

	$version =~ /(\d+)\.(\d+)\.(\d+)/;
	return ($1, $2, $3);
}



sub uninstall_version{
	my $binary_dir = shift;
	my $execute = shift;
	my $is_default = shift;

	my $returnstring='';
	(my $volume, my $directory, my $file) = File::Spec->splitpath($binary_dir,1);

	my $library_dir;
	$library_dir = get_lib_dir($execute);
	if ($library_dir =~/^\.\./){
		#relative path, concatenate
		$library_dir = File::Spec->catdir($binary_dir,$library_dir);
	}else{
		#absolute path, leave it
		1;
	}
	my $versionstring;
	my $testdir;
	if (-d $library_dir){
		my @dirs = File::Spec->splitdir($library_dir);
		if ($dirs[$#dirs] =~ /PsN_(\d+)_(\d+)_(\d+)/){
			$versionstring = '-'.$1.'.'.$2.'.'.$3;
		}
		my $tmp = $library_dir;
		$tmp =~ s/PsN(_\d+_\d+_\d+)/PsN_test\1/;
		if (-d $tmp){
			$testdir = $tmp;
		}
	}
	unless (defined $versionstring){
		if ($execute =~ /execute(-\d+\.\d+\.\d+)/){
			$versionstring = $1;
		}
	}
	my @exts = ();
	push (@exts,$versionstring) if (defined $versionstring);
	push(@exts,'') if ($is_default);
	my $files = get_bin_files($volume,$directory,\@exts);

	if ($is_default){
		$returnstring = $execute.$versionstring;
		$returnstring .= '.pl' if running_on_windows();
	}

	unless ((not -d $library_dir) and (scalar(@{$files})==0)){
		my $message = "Do you want to remove PsN$versionstring, i.e. remove\n";
		$message .= "$library_dir\n" if (-d $library_dir);
		$message .= "$testdir\n" if (defined $testdir);
		$message .= $files->[0]."\n".$files->[1]."\n".$files->[2]."\n".$files->[3].' etc';
		print $message."\n?:";
		if (confirm()){
			rmtree($library_dir) if (-d $library_dir);
			rmtree($testdir) if (defined $testdir);
			unlink @{$files};
		}
	}
	return $returnstring;
}


