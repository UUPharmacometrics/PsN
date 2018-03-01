use strict;
use Config;
use File::Spec;
use File::Path qw(mkpath);
use File::Glob;
use lib 'lib';
use ext::Config::Tiny;
use ext::File::HomeDir;
use PsN (); #pass empty list so that import, which reads config file, is not called

my $version = $PsN::version;

my $default_user_name;
my $default_sitelib;
my $default_bin;
my $default_perlpath;
my $use_user_name;
my $is_root = 0;
my $uid;
my $gid;
my $have_file_copy = 0;
my $directory_separator;
my $is_win7 = 0;
my $is_Vista = 0;
my $binary_dir;
my $library_dir;
my $perl_binary;
my $old_psn_config_file;
my $old_default_psn_config_file;
my $copy_cmd;
my $copy_recursive_cmd;
my $relative_lib_path=0;

if (scalar(@ARGV)>0){
	if ($ARGV[0] eq 'relative'){
		$relative_lib_path=1;
		print "\nUsing relative library path, suitable for portable installations\n";
	}else{
		die("unknown input ".$ARGV[0]."\n");
	}
}

setup_globals();

if (eval('require File::Copy::Recursive')) {
	eval('import File::Copy::Recursive qw/fcopy rcopy dircopy fmove rmove dirmove/');
	$have_file_copy = 1;
	print "\nInformation: Using File::Copy::Recursive for copying\n"; 
}

if (running_on_windows()) {
	get_windows_version();
}

#NEW
my $default_installation = PsN::get_default_psn_installation_info();
my $new_defaults = PsN::get_new_installation_defaults($version,$default_installation);

$default_sitelib = $new_defaults->{'base_lib_dir'};
$default_bin = $new_defaults->{'bin_dir'};
$default_perlpath = $Config{perlpath};
my $old_default_psn_config_file = $new_defaults->{'old_config_file'};
my $old_default_psn_version = $new_defaults->{'version'};

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
	my $local_perlpath = $wherebin;
	my $local_sitelib = $wherebin;
	$local_sitelib =~ s/bin\\perl\.exe$/site\\lib/;
	my $local_bin = $wherebin;
	$local_bin =~ s/perl\.exe$//;

	if ((defined $default_sitelib) and (-e $default_sitelib) and (-d $default_sitelib)) {
		#default-sitelib ok
		1;
	}elsif(length($local_sitelib) > 0 and -d $local_sitelib) {
		$default_sitelib = $local_sitelib;
	} else {
		$default_sitelib = undef;
	}
	if ((defined $default_bin) and (-e $default_bin) and (-d $default_bin)) {
		#default  ok
		1;
	}elsif(length($local_bin)>0 and -d $local_bin) {
		$default_bin = $local_bin;
	}else{
		$default_bin = undef;
	}
	if ((defined $default_perlpath) and (-e $default_perlpath) and (-x $default_perlpath) and (not -d $default_perlpath)) {
		#default ok
		1;
	}elsif(length($local_perlpath)>0 and -x $local_perlpath and (not -d $local_perlpath)) {
		$default_perlpath = $local_perlpath;
	}else{
		$default_perlpath = undef;
	}
}
unless (defined $default_perlpath and defined $default_bin and defined $default_sitelib) {
	print "\nWarning: There is something unusual with your Perl installation and configuration.\n".
		"Will try to install anyway, but there may be problems.\n";
}

my $name_safe_version = $version;
$name_safe_version =~ s/\./_/g;
my @utilities = (
	'bootstrap', 'cdd', 'execute', 'llp', 'scm', 'sumo', 'sse',
	'data_stats', 'update_inits', 'update', 'npc', 'vpc',
	'pind','nonpb','extended_grid','psn','psn_options','psn_clean',
	'runrecord','mcmp','lasso','mimp','xv_scm','parallel_retries',
	'boot_scm','gls','simeval','frem','randtest','linearize', 'crossval', 'pvar', 'nca', 'proseval', 'vpctable','sir','rawresults',
	'precond','covmat','nmoutput2so', 'benchmark','postfrem','npfit', 'resmod', 'cddsimeval', 'qa', 'transform', 'improve', 'boot_randtest'
	);

my @win_modules = ('Moose', 'MooseX::Params::Validate', 'Math::Random', 'YAML::XS');
my @nix_modules = ('Moose', 'MooseX::Params::Validate', 'Math::Random', 'Storable', 'YAML::XS');
my @recommended_modules = ('Archive::Zip','Statistics::Distributions');

my @modules;

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

sub get_default_nm_versions 
{
	my %versionhash;
	if (running_on_windows()) {
		my $home = $ENV{USERPROFILE};
		my @dirs = <C:/nmvi* C:/NONMEM/nmvi* $home/nmvi* $home/NONMEM/nmvi*>;
		foreach my $dir (@dirs){
			$versionhash{$dir}=6 if ((-d $dir) and (
										 ((-d $dir.'\run') and (-x $dir.'\run\nmfe6.bat')) or 
										 ((-d $dir.'\util') and (-x $dir.'\util\nmfe6.bat'))
									 ));
		}
		@dirs = <C:/nm7* C:/nm_7* C:/NONMEM/nm7* C:/NONMEM/nm_7*>;
		foreach my $dir (@dirs){
			if ((-d $dir) and ((-d $dir.'\run') or (-d $dir.'\util'))){
				my $version;
				if ((-x $dir.'\run\nmfe72.bat') or (-x $dir.'\util\nmfe72.bat')){
					$version='7.2';
				}elsif ((-x $dir.'\run\nmfe73.bat') or (-x $dir.'\util\nmfe73.bat')){
					$version='7.3';
				}elsif ((-x $dir.'\run\nmfe74.bat') or (-x $dir.'\util\nmfe74.bat')){
					$version='7.4';
				}elsif ((-x $dir.'\run\nmfe75.bat') or (-x $dir.'\util\nmfe75.bat')){
					$version='7.5';
				}elsif ((-x $dir.'\run\nmfe76.bat') or (-x $dir.'\util\nmfe76.bat')){
					$version='7.6';
				}elsif ((-x $dir.'\run\nmfe7.bat') or (-x $dir.'\util\nmfe7.bat')){
					$version='7.1';
				}
				$versionhash{$dir}=$version if (defined $version);
			}
		}
	}else{
		my $home = home();
		my @dirs = </opt/nmvi* /opt/NONMEM/nmvi* $home/nmvi* $home/NONMEM/nmvi*>;
		foreach my $dir (@dirs){
			$versionhash{$dir}=6 if ((-d $dir) and (
										 ((-d $dir.'/run') and (-x $dir.'/run/nmfe6')) or 
										 ((-d $dir.'/util') and (-x $dir.'/util/nmfe6'))
									 ));
		}
		@dirs = </opt/nm7* /opt/nm_7* /opt/NONMEM/nm7* /opt/NONMEM/nm_7* /opt/nonmem/nm7* /opt/nonmem/nm_7*>;
		push(@dirs,<$home/nm7* $home/nm_7* $home/NONMEM/nm7* $home/NONMEM/nm_7* $home/nonmem/nm7* $home/nonmem/nm_7*>);
		foreach my $dir (@dirs){
			if ((-d $dir) and ((-d $dir.'/run') or (-d $dir.'/util'))){
				my $version;
				if ((-x $dir.'/run/nmfe72') or (-x $dir.'/util/nmfe72')){
					$version='7.2';
				}elsif ((-x $dir.'/run/nmfe73') or (-x $dir.'/util/nmfe73')){
					$version='7.3';
				}elsif ((-x $dir.'/run/nmfe74') or (-x $dir.'/util/nmfe74')){
					$version='7.4';
				}elsif ((-x $dir.'/run/nmfe75') or (-x $dir.'/util/nmfe75')){
					$version='7.5';
				}elsif ((-x $dir.'/run/nmfe76') or (-x $dir.'/util/nmfe76')){
					$version='7.6';
				}elsif ((-x $dir.'/run/nmfe7') or (-x $dir.'/util/nmfe7')){
					$version='7.1';
				}
				$versionhash{$dir}=$version if (defined $version);
			}
		}
	}

	return \%versionhash;
}

sub get_nm_version 
{
	print "Enter the *complete* path of the NM-installation directory: ";
	my $dir = <STDIN>;
	chomp ($dir);
	unless( -e $dir ){
		print "Directory $dir does not exist. Please try again\n";
		return '0','0';
	}
	if (running_on_windows()) {
		if ($dir =~ /^[A-Za-z]:\\/){
			1;
		}elsif ($dir =~ /^%USERPROFILE%/){
			my $home = $ENV{USERPROFILE};
			if (defined $home){
				$dir =~ s/^%USERPROFILE%/$home/;
			}else{
				print "$dir does not look like a full search path including drive letter. Please try again.\n";
			}
		}else{
			print "$dir does not look like a full search path including drive letter. Please try again.\n";
			return '0','0';
		}
	}else{
		if ($dir =~ /^\//){
			1;
		}elsif ($dir =~ /^~/){
			my $home=home();
			$dir =~ s/^~/$home/;
		}else{
			print "$dir does not look like a full search path (does not begin with /). Please try again.\n";
			return '0','0';
		}
	}

	my $version;
	if (running_on_windows()) {
		$version=6 if ((-d $dir) and (
						   ((-d $dir.'\run') and (-x $dir.'\run\nmfe6.bat')) or 
						   ((-d $dir.'\util') and (-x $dir.'\util\nmfe6.bat'))
					   ));
		if ((-d $dir) and ((-d $dir.'\run') or (-d $dir.'\util'))){
			if ((-x $dir.'\run\nmfe72.bat') or (-x $dir.'\util\nmfe72.bat')){
				$version='7.2';
			}elsif ((-x $dir.'\run\nmfe73.bat') or (-x $dir.'\util\nmfe73.bat')){
				$version='7.3';
			}elsif ((-x $dir.'\run\nmfe74.bat') or (-x $dir.'\util\nmfe74.bat')){
				$version='7.4';
			}elsif ((-x $dir.'\run\nmfe75.bat') or (-x $dir.'\util\nmfe75.bat')){
				$version='7.5';
			}elsif ((-x $dir.'\run\nmfe76.bat') or (-x $dir.'\util\nmfe76.bat')){
				$version='7.6';
			}elsif ((-x $dir.'\run\nmfe7.bat') or (-x $dir.'\util\nmfe7.bat')){
				$version='7.1';
			}
		}
	}else{
		$version=6 if ((-d $dir) and (
						   ((-d $dir.'/run') and (-x $dir.'/run/nmfe6')) or 
						   ((-d $dir.'/util') and (-x $dir.'/util/nmfe6'))
					   ));
		if ((-d $dir) and ((-d $dir.'/run') or (-d $dir.'/util'))){
			if ((-x $dir.'/run/nmfe72') or (-x $dir.'/util/nmfe72')){
				$version='7.2';
			}elsif ((-x $dir.'/run/nmfe73') or (-x $dir.'/util/nmfe73')){
				$version='7.3';
			}elsif ((-x $dir.'/run/nmfe74') or (-x $dir.'/util/nmfe74')){
				$version='7.4';
			}elsif ((-x $dir.'/run/nmfe75') or (-x $dir.'/util/nmfe75')){
				$version='7.5';
			}elsif ((-x $dir.'/run/nmfe76') or (-x $dir.'/util/nmfe76')){
				$version='7.6';
			}elsif ((-x $dir.'/run/nmfe7') or (-x $dir.'/util/nmfe7')){
				$version='7.1';
			}
		}
	}

	unless (defined $version) {
		print "Did not find an nmfe script in the util or run or . subdirectory of $dir. Please try again\n";
		return '0','0';
	}
	return $dir,$version;
}

sub get_psnname
{
	my $path = shift;
	my $default = shift;
	print "Enter the name you want to use for the NM-version in\n$path\n".
		"with PsN option -nm_version,\nor press ENTER to use the name $default : ";
	my $psnname = <STDIN>;
	chomp ($psnname);
	unless (length($psnname)>0){
		$psnname=$default;
	}
	return $psnname;
}

sub copy_and_modify_bin_files
{
	my $file = shift;
	my $binary_dir = shift;

	my $includepath;
	my @includelines=();
	if ($relative_lib_path){
		#from binary_dir to library
		my $path = File::Spec->abs2rel($library_dir,$binary_dir);
		#must have forward slash also on windows, otherwise slash will disappear
		#when quoting path to get value of $Bin
		if ($path eq '.'){
			$includepath = "PsN_$name_safe_version";
		}else{

			$includepath = File::Spec->catfile($path,"PsN_$name_safe_version");
		} 
		@includelines=('use FindBin qw($Bin);','use lib "$Bin/".'."'$includepath';");
	}else{
		$includepath = File::Spec->catfile($library_dir,"PsN_$name_safe_version");
		#need empty line to keep correct number
		@includelines=("use lib '$includepath';",'');
	}

	unless (open(INST, "<", File::Spec->catfile("bin", $file))) {
		abort("Could not open " . File::Spec->catfile("bin", $file) . " for reading, unable to install $file: $!\n");
	}
	unless (open(UTIL, ">", File::Spec->catfile($binary_dir, "$file-$version"))) { 
		abort("Could not open " . File::Spec->catfile($binary_dir, "$file-$version") . " for writing, unable to install $file: $!\n");
	}
	my $replace_line_found = 0;
	while (<INST>) {
		if (/\# Everything above this line will be replaced \#/) {
			print UTIL "\#!", $perl_binary, "\n";
			print UTIL join("\n",@includelines)."\n";
			print UTIL "\# Everything above this line was entered by the setup script \#\n";
			$replace_line_found = 1;
		} elsif ($replace_line_found) {
			print UTIL $_;
		}
	}
	close UTIL;
	close INST;

	chmod(0755, File::Spec->catfile($binary_dir, "$file-$version"));
}

sub create_conf
{
	my $confpath = shift;
	my $perlbin = shift;
	my $template = $confpath.'psn.conf_template';
	my $newconf = $confpath.'psn.conf';

	unless (-e $template){
		print "Error: Could not find $template.\n"."No config file created\n";
		return 0;
	}
	my $config = ext::Config::Tiny -> read( $template );
	## this is not used anywhere $config -> {'_'} -> {'perl'} = $perlbin;


	#fix R
	my @paths;
	my $file;
	if (running_on_windows()) {
		my $path = $ENV{PATH};
		@paths= split(';',$path);
		$path = $ENV{Path};
		push(@paths,split(';',$path));
		push(@paths,'C:\R\bin\R');
		$file='\R.exe';
	}else{
		my $path = $ENV{PATH};
		@paths= split(':',$path);
		$file='/R';
	}
	foreach my $alt (@paths){
		if (-e $alt.$file){
			if (running_on_windows()) {
				$alt =~ s/Program Files/Progra~1/;
			}
			$config -> {'_'} -> {'R'} = $alt.$file;
			last;
		}
	}

	#fix nm

	my %nm_versions;

	print "\nSearching for NM versions on your file system...\n";
	my $ref = get_default_nm_versions();
	if ( defined $ref ) {
		%nm_versions=%{$ref};
	}

	while (1){
		if (%nm_versions){
			print "These are the loaded NM versions:\n";
			foreach my $key (keys %nm_versions){
				print "$key\n";
			}
			print"Would you like to add another one [y/n] ?\n";
			last unless( confirm() );
		}else{
			print "No NM versions have been found. You need to add at least one.\n";
		}
		
		my ($path,$version_label) = get_nm_version();
		unless ($path eq '0'){
			$nm_versions{$path}=$version_label;
		}
	}
	if (%nm_versions){
		my %psn_names;
		foreach my $version (keys %nm_versions){
			my @dirarr = File::Spec->splitdir( $version);
			my $default= pop(@dirarr);
			while (defined $psn_names{$default}){
				$default = pop(@dirarr).'_'.$default;
			}
			my $name = get_psnname($version,$default);
			$psn_names{$name}=$version;
		}
		
		if (scalar(keys %psn_names)==1){
			my @keys = keys %psn_names;
			$psn_names{'default'} = $psn_names{$keys[0]};
		}
		
		while (not defined $psn_names{'default'}){
			print "These are the defined psn-names:\n";
			print (join('  ',(keys %psn_names))."\n");
			print "Enter the psn-name of the NM version that should be the default : ";
			my $name = <STDIN>;
			chomp ($name);
			if (defined $psn_names{$name}){
				$psn_names{'default'} = $psn_names{$name};
			}else{
				print "No NM version with psn-name $name is defined.\n";
			}
		}
		foreach my $name (keys %psn_names){
			my $version_label = $nm_versions{$psn_names{$name}};
			$config -> {'nm_versions'} -> { $name } = $psn_names{$name}.','.$version_label;
		}
		
	} else {
		print "\nError: No NM versions defined. You must add at least one manually to $newconf, otherwise PsN will not run.";
	}

	$config->write($newconf);

	print "\n\nA new configuration file has been created:\n$newconf\n";
	print "If you want to add cluster options or personalized defaults or\n".
		"NMqual settings you can do this manually by editing\n"."$newconf\n".
		"in a regular text editor.\n";
	return 1;
}

sub abort
{
	my $message = shift;

	print $message;
	print "\n\nThe installation process has been aborted, PsN was not installed.";
	print "\n\nPress ENTER to exit the installation program.\n";
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

sub copy_documentation
{
	print "The documentation will be copied to a directory named PsN_documentation\n";
	my $ok = 0;
	my $input;
	my $default;
	if (running_on_windows()) {
		my $home = $ENV{USERPROFILE};
		if ($is_win7){
			$default = $home.'\Desktop';
		}else{
			$default = $home.'\Start Menu';
		}
		$default = '' unless (defined $home);
	} else {
		my $home = home();
		$default = $home;
	}
	while (not $ok) {
		if (length($default) > 0) {
			print "Please enter the full search path to where do you want to place this directory,\nor press ENTER to use $default\n : ";
			$input = <STDIN>;
			chomp( $input );
			$input = $default unless (length($input)>0);
		}else{
			print "Please enter the full search path to where do you want to place this directory\n : ";
			$input = <STDIN>;
			chomp( $input );
		}
		if (running_on_windows()) {
			if ($input =~ /^[A-Za-z]:\\/){
				$ok = 1;
			}elsif ($input =~ /^%USERPROFILE%/){
				my $home = $ENV{USERPROFILE};
				if (defined $home){
					$input =~ s/^%USERPROFILE%/$home/;
					$ok = 1;
				} else {
					print "$input does not look like a full search path including drive letter. Please try again.\n";
				}
			} else {
				print "$input does not look like a full search path including drive letter. Please try again.\n";
			}
		} else {
			if ($input =~ /^\//){
				$ok = 1;
			} elsif ($input =~ /^~/) {
				my $home = home();
				$input =~ s/^~/$home/;
				$ok = 1;
			} else {
				print "$input does not look like a full search path (does not begin with /). Please try again.\n";
			}
		}
	}

	if ($is_root) {
		if (length($default_user_name)>0){
			print "Please enter the user name for the owner of the documentation copies,\nor press ENTER to use $default_user_name\n : ";
			$use_user_name = <STDIN>;
			chomp( $use_user_name );
			$use_user_name = $default_user_name unless (length($use_user_name) > 0);
		} else {
			print "Please enter the user name for the owner of the documentation files copy (it is unpractical if it is root)\n : ";
			$use_user_name = <STDIN>;
			chomp($use_user_name);
		}
		if (length($use_user_name) > 0){
			my ($login, $pass);
			unless (($login, $pass, $uid, $gid) = getpwnam($use_user_name)) {
				$ok = 0;
				print "$use_user_name not found, please try again\n";
			}
		}
	}

	unless (-d $input) {
		print "$input does not exist. Would you like to create it? [y/n] ";
		if (confirm()) {
			unless (mkpath($input)) {
				print "Unable to create $input. Cannot copy documentation\n";
				$ok = 0;
			}
		} else {
			print "Cannot copy documentation\n";
			$ok = 0;
		}
	}
	my $newdir = File::Spec->catdir($input,"PsN_documentation");
	if ($ok) {
		if( mkpath( $newdir ) ) { 
			unless (running_on_windows()) {
				if (length($use_user_name)>0){
					my @arr = ($newdir);
					chown $uid, $gid, @arr;
				}
			}
		} else { 
			if (-d $newdir) {
				if (not running_on_windows()) {
					if (length($use_user_name) > 0) {
						my @arr = ($newdir);
						chown $uid, $gid, @arr;
					}
				}
			}else{
				print "Unable to create $newdir. Cannot copy documentation\n";
				$ok = 0;
			}
		}
	}
	if ($ok) {
		if ($have_file_copy) {
			my @file_list;
			if (running_on_windows()) {
				@file_list = <doc\\*.scm doc\\*.pdf doc\\*.xls>;
			} else {
				@file_list = <doc/*.scm doc/*.pdf doc/*.xls>;
			}
			my $dest = File::Spec->catfile("$newdir", ".");
			foreach my $ff (@file_list) {
				fcopy($ff, $dest);
			}
		} else {
			my $full_command = $copy_recursive_cmd . " " . File::Spec->catfile("doc", "*.scm") . " \"" . $newdir. "\""; 
			system($full_command);
			my $full_command = $copy_recursive_cmd . " " . File::Spec->catfile("doc", "*.pdf") . " \"" . $newdir. "\""; 
			system($full_command);
			my $full_command = $copy_recursive_cmd . " " . File::Spec->catfile("doc", "*.xls") . " \"" . $newdir. "\""; 
			system($full_command);
		}
	}
	if ($ok and (-e "$newdir" . "/bootstrap_userguide.pdf") ){
		if (not running_on_windows()) {
			if (length($use_user_name)>0){
				my @ary = glob(File::Spec -> catfile( $newdir,"*.scm")); # expand filenames
				chown $uid, $gid, @ary;
				@ary = glob(File::Spec -> catfile( $newdir,"*.pdf"));
				chown $uid, $gid, @ary;
				@ary = glob(File::Spec -> catfile( $newdir,"*.xls"));
				chown $uid, $gid, @ary;
			}
		}
		print "Copying of documentation to $newdir complete.\n";
	}else{
		print "Copying of files to $newdir failed, documentation was not copied\n";
	}
}

sub create_directory
{
	my $directory_name = shift;

	unless (-e $directory_name) {
		print "Directory $directory_name does not exist. Would you like to create it?[y/n]\n";
		if (confirm()) {
			if (not mkpath($directory_name)) {
				abort("Unable to create $directory_name.\n");
			}
		} else {
			abort();
		}
	}
}

sub test_perl_modules
{
	print "\nTesting required modules:\n";
	foreach my $module (@modules) {
		my $ok = 0;
		$ok = 1 if eval("require " . $module);
		if ($ok) {
			print "Module $module ok\n";
		} else {
			print "Perl module $module is missing, you must install it before running PsN.\n";
		}
	} 
	print "\nDone testing required modules.\n";
	print "\nTesting recommended but not required modules...\n";
	foreach my $module (@recommended_modules){
		my $ok = 0;
		$ok = 1 if eval("require " . $module);
		if ($ok) {
			print "Module $module ok\n";
		} else {
			print "Perl module $module is missing, you can run PsN without it but some features will be disabled.\n";
		}
	} 

	print "Tests done.\n\n";
	print "Continue installing PsN (installing is possible even if modules are missing)[y/n]?";
	abort() unless (confirm());
}

sub warn_about_local_configuration
{
	if (-e home() . "/psn.conf") {
		print "\nPlease note that for you personally the configuration file you\n".
		"already have in ".home()."$directory_separator"."psn.conf\n".
		"will override the settings in\n".
		"$library_dir"."$directory_separator"."PsN_$name_safe_version"."$directory_separator".
		"psn.conf\n";
	} elsif (running_on_windows()) {
		print "\nPlease note that if you have a psn.conf file in your Desktop folder,\n".
		"the settings in that file will override the settings in\n".
		"$library_dir"."$directory_separator"."PsN_$name_safe_version"."$directory_separator".
		"psn.conf\n";
	} else {
		print "\nPlease note that if you have a psn.conf file in your home directory,\n".
		"the settings in that file will override the settings in\n".
		"$library_dir" . "$directory_separator" . "PsN_$name_safe_version" . "$directory_separator" . "psn.conf\n";
	}
}

sub copy_file
{
	my $source = shift;
	my $dest = shift;
	
	# This sub works currently only on windows
	if ($have_file_copy) {
		unless (fcopy($source, $dest)) {
			abort("Could not copy $source to $dest : $!\n");
		}
	} else {
		system("copy /Y \"$source\" \"$dest\"");
	}
}

sub create_bat_file
{
	my $name = shift;

	my $bat_file;

	unless (open $bat_file, ">", $name) {
		abort("Could not open $name: $!\n");
	}

	print $bat_file '@echo off' . "\n" .'perl %~dp0%~n0.pl %*' . "\n";

	close $bat_file;
}

sub get_psn_version
{
	my $version = shift;

	$version =~ /(\d+)\.(\d+)\.(\d+)/;
	return ($1, $2, $3);
}

# Update from the old comma separated format of nmfe_options
sub update_conf_file
{
	my $conf_file = shift;
	my $old_version = shift;

	(my $major, my $minor, my $maint) = get_psn_version($old_version);
	unless ($major > 4 or ($major == 4 and $minor > 1) or ($major == 4 and $minor == 1 and $maint > 3)) {
		open my $old_conf, "<", $conf_file;
		open my $new_conf, ">", "$conf_file.new";
		while (<$old_conf>) {
			if (/nmfe_options=(.*)/) {
				my @arr = split /,/, $1;
				my $str = "nmfe_options=";
				foreach my $opt (@arr) {
					$str .= "-$opt ";
				}
				$str .= "\n";
				print $new_conf $str;
			} else {
				print $new_conf $_;
			}
		}
		close $old_conf;
		close $new_conf;
		unlink $conf_file;
		rename "$conf_file.new", $conf_file;
	}
}

print "\nThis is the PsN installer. I will install PsN version $version.\n".
    "You need to answer a few questions. If a default value is presented\n".
    "you may accept it by pressing ENTER.\n\n";

if ($Config{osname} eq 'linux' or $Config{osname} eq 'darwin') {
	my $user = `whoami`;
	chomp($user);
	if ($user =~ /^root$/) {
		$is_root = 1;
		my $home = $ENV{HOME};
		if ($home =~ /\/([^\/]*)$/) {
			$default_user_name = $1;
		}
	} else {
		print "Hi $user, you don't look like root. Please note that you need root privileges to install PsN systemwide.\n";
	}
} else {
	if (not running_on_windows()) {
		print "OS ".$Config{osname}." is not explicitly supported by PsN, but if it is a unix/Mac type\n".
			"system everything should work anyway. Problems are expected only if it is a Windows system.\n".
			"Do you want to proceed with the installation [y/n] ?\n";
		abort() unless(confirm());
	}
}

print "PsN Utilities installation directory [$default_bin]:";
$binary_dir = get_input($default_bin);
create_directory($binary_dir);

print "Path to perl binary used to run Utilities [$default_perlpath]:";
$perl_binary = get_input($default_perlpath);

(my $volume, my $directory, my $file) = File::Spec->splitpath($perl_binary);
my $runperl_name = "runperl.bat";
my $runperl_binary = File::Spec->catpath( $volume, $directory, $runperl_name);


print "PsN Core and Toolkit installation directory [$default_sitelib]:";
$library_dir = get_input($default_sitelib);
create_directory($library_dir);


if (running_on_windows()) {
	$copy_cmd = "copy /Y";
	$copy_recursive_cmd = "xcopy /Y /E";
	@modules = @win_modules;
} else {
	@modules = @nix_modules;
	$copy_cmd = "cp";
	$copy_recursive_cmd = "cp -r";
}

print "\nThe next step is to check Perl module dependencies.
If a module is missing, you must install it, e.g. from CPAN,
http://www.cpan.org/modules/index.html
before PsN can be run.

Would you like this script to check Perl modules [y/n]?";

if (confirm()) {
	test_perl_modules();
}

my $overwrite = 0;
my $old_version = 'X_X_X';
if (defined $old_default_psn_version){
	$old_version = $old_default_psn_version;
	$old_version =~ s/\./_/g;
}
my $keep_conf = 0;

if (-d "$library_dir/PsN_$name_safe_version"){
	print "Directory $library_dir/PsN_$name_safe_version already exists.\n";
	print "PsN $version is already (partially) installed. Would you like to continue anyway [y/n] ?";
	abort() unless (confirm());
}else{
	unless (mkpath("$library_dir/PsN_$name_safe_version")) {
		print "Failed to create $library_dir/PsN_$name_safe_version: $!\n";
		print "Would you like to continue anyway [y/n] ?";
		abort() unless (confirm());
	}
}

if (-e "$library_dir/PsN_$name_safe_version/psn.conf") {
	print "An older version of psn.conf exists in $library_dir/PsN_$name_safe_version.\n";
	print "Keep the old version [y/n] ?";
	$keep_conf = confirm();
}

my $newconf = File::Spec->catfile( $library_dir, "PsN_$name_safe_version", "psn.conf" );
my $thelibdir = File::Spec->catdir($library_dir,"PsN_$name_safe_version");
if ($have_file_copy) {
	if ($keep_conf) {
		unless (fcopy($newconf, "old.conf")) {
			abort("Could not copy $newconf to old.conf : $!\n");
		}
	}
	unless (dircopy("lib", $thelibdir)) {
		abort(" Could not copy contents of lib directory to $thelibdir : $!\n");
	}
	if ($keep_conf){
		unless (fcopy("old.conf",$newconf)) {
			abort("Could not copy old.conf to $newconf : $!\n");
		}
	}
} else {
	#do not have File::Copy
	system($copy_cmd . " \"" . $newconf . "\" old.conf") if $keep_conf;
	
	my $full_command = $copy_recursive_cmd . " " . File::Spec -> catfile( "lib", "*" ) . " \"" . $thelibdir . "\""; 
	system($full_command);

	unless (-e "$library_dir" . "/PsN_$name_safe_version/model.pm" ) {
		print "Copying of files to $library_dir" . "/PsN_$name_safe_version/".
			" failed, the following command did not work\n$full_command\n";
		if (running_on_windows()) {
			abort("It is recommended to run the command\n".
				"cpan install File::Copy::Recursive (Strawberry Perl) or ppm install file-copy-recursive (ActiveState Perl)\n".
				"in a command window, and then try to install PsN again, using the same setup script.\n");
		} else {
			abort("It is recommended to install File::Copy::Recursive".
				"and then try to install PsN again, using the same setup script.\n");
		}
	}
	
	system($copy_cmd . " old.conf \"" . $newconf . "\"") if $keep_conf;
}

my $confirmed = 0;
foreach my $file (@utilities) {
	copy_and_modify_bin_files($file, $binary_dir);

	my $copy_the_binaries = 0;
	if (-e "$binary_dir/$file") {
		
		if (not running_on_windows()) {
			my $link = readlink("$binary_dir/execute");
			if ($old_version eq 'X_X_X' and not($link eq '')) {
				$link =~ /-(\d+\.\d+\.\d+)/;
				$old_version = $1;
			}
		} elsif (running_on_windows()) {
			my $link = open(IN, "$binary_dir/execute");
			if ($old_version eq 'X_X_X' and $link) {
				while (<IN>) {
					if (/^use lib '.*PsN_(.*)';/) {
						$old_version = $1;
						$old_version =~ tr/_/./;
						last;
					} elsif (/^use PsN_\d+_\d+_\d+/) {		# The old system
						s/^use PsN\_//;
						s/\s.*//;
						s/\_/\./g;
						s/\;//;
						$old_version = $_;
						last;
					}
				}
				close(IN);
			}
		}

		my $tmp = $old_version;
		$tmp =~ s/\./\_/g;
		if (defined $old_default_psn_config_file and -e $old_default_psn_config_file){
			$old_psn_config_file = $old_default_psn_config_file;
		}else{
			$old_psn_config_file = File::Spec->catfile($library_dir, "PsN_$tmp", "psn.conf");
			unless (-e $old_psn_config_file){
				$old_psn_config_file = undef ;
			}
		}
		if ($old_version eq $version) {
			if (not $confirmed) {
				print("\nThis version ($version) looks like an older installed\n",
					   "version ($old_version) of PsN. Would you like to make\n",
					   "this version ($version) the default? [y/n]");
				$confirmed = 1;
				$overwrite = confirm();
			}
		}
		if (not $confirmed) {
			print("\nAn older version($old_version) of PsN is installed. Would you like to\n",
				   "make this version ($version) the default? [y/n]");
			$confirmed = 1;
			$overwrite = confirm();
		}
		if ($overwrite) {
			unlink("$binary_dir/$file");
			$copy_the_binaries = 1;
		}
		
	} else {
		$copy_the_binaries = 1;
	}  
	
	if ($copy_the_binaries) {
		if (running_on_windows()) {
			copy_file("$binary_dir\\$file-$version", "$binary_dir\\$file");
			copy_file($runperl_binary, "$binary_dir\\$file.bat");
		} else {
			symlink("$binary_dir/$file-$version", "$binary_dir/$file");
		}
	}

	# Make the versioned script directly executable
	if (running_on_windows()) {	
		rename("$binary_dir\\$file-$version", "$binary_dir\\$file-$version.pl");
		create_bat_file("$binary_dir\\$file-$version.bat");

		# Conversion of previously installed scripts
		foreach my $name (glob "$binary_dir\\$file-*") {
			next if $name =~ /(.bat|.pl)$/;
			rename($name, "$name.pl");
			create_bat_file("$name.bat");
		}
	}
}

if (running_on_windows()) {
	require Win32;
	$library_dir = Win32::GetShortPathName($library_dir);
}

print "\nWould you like to copy the PsN documentation to a file system location of your choice?  [y/n] ";
if (confirm()) {
	copy_documentation();
}

my $default_test = $library_dir;
my $test_library_dir;
print "\nWould you like to install the PsN test library? [y/n] ";
if (confirm()) {
	print "PsN test library installation directory [$default_test]:";
	$test_library_dir = get_input($default_test);
	$test_library_dir .= "/PsN_test_$name_safe_version";

	if ($have_file_copy) {
		unless (dircopy("test", $test_library_dir)) {
			abort("Could not copy contents of test directory to $test_library_dir : $!\n");
		}
	} else {
		mkpath($test_library_dir);
		my $full_command = $copy_recursive_cmd . " " . File::Spec->catfile("test", "*") . " \"" . $test_library_dir . "\""; 
		system($full_command);

		unless (-e "$test_library_dir/includes.pm" ) {
			print "Copying of files to $test_library_dir/ failed, the following command did not work\n$full_command\n";
			if (running_on_windows()) {
				abort("It is recommended to run the command\n".
					"cpan install File::Copy::Recursive (Strawberry Perl) or ppm install file-copy-recursive (ActiveState Perl)\n".
					"in a command window, and then try to install PsN again, using the same setup script.\n");
			} else {
				abort("It is recommended to install File::Copy::Recursive and then try to install PsN again, using the same setup script.\n");
			}
		}
	
		system($copy_cmd . " old.conf \"" . $newconf . "\"") if $keep_conf;
	}

	# Put library and bin paths into includes.pm
	my $includes_file = "$test_library_dir/includes.pm";
	rename($includes_file, "$includes_file.temp");
	open(my $sh, "<", "$test_library_dir/includes.pm.temp") or abort("Error when copying includes.pm: $!");
	open(my $dh, ">", "$test_library_dir/includes.pm") or abort("Error when copying includes.pm: $!");

	while (<$sh>) {
		if (/^(\s*my \$libpath\s*=\s*)'';/) {
			print $dh "$1'$library_dir/PsN_$name_safe_version';\n";
		} elsif (/^(\s*our \$path\s*=\s*)'';/) {
			print $dh "$1'$binary_dir/';\n";
		} else {
			print $dh $_;
		}
	}

	close $sh;
	close $dh;
	unlink("$includes_file.temp");

	#change perl binary in runsystem
	open(my $sh, "+< $test_library_dir/runsystem")                 or die "Opening: $!";
	my @ARRAY = <$sh>;
	$ARRAY[0] = "\#!".$perl_binary."\n";
	seek($sh,0,0)                        or die "Seeking: $!";
	print $sh @ARRAY                     or die "Printing: $!";
	truncate($sh,tell($sh))               or die "Truncating: $!";
	close($sh)                           or die "Closing: $!";

	
	print "PsN test library installed successfully in [$test_library_dir].\n";
	print "Please read the 'testing' chapter of the developers_guide.pdf for information on how to run the tests\n\n";
}


my $configuration_done = 0;
$configuration_done = 1 if ($keep_conf);

if (not $keep_conf) {
	my $conf_ok = 0;
	print "\nNow you must edit " . "$library_dir" . "$directory_separator" . "PsN_$name_safe_version" . "$directory_separator".
		"psn.conf\n"."so that PsN can find your NONMEM installations.\n";

	my $offer_help = 1;
	if (defined $old_psn_config_file){
		print "Would you like to copy psn.conf from the previous default version ".
			"to this new installation? [y/n] ";
		my $newf = File::Spec->catfile($library_dir, "PsN_$name_safe_version", "psn.conf");

		if (confirm()) {
			if ($have_file_copy) {
				fcopy($old_psn_config_file, $newf);
			} else {
				system($copy_cmd . " \"" . $old_psn_config_file . "\" \"" . $newf . "\"") ;
			}
			if (-e $newf) {
				$offer_help = 0;
				$conf_ok = 1;
				$configuration_done = 1;
				print "Copied $old_psn_config_file to $newf.\n";
				update_conf_file($newf, $old_version);
			} else {
				print "Copying of old psn.conf failed.\n";
			}
		}
	}
	if ($offer_help) {
		print "You can get help to create a bare-bones configuration file that will work\n".
			"when running PsN locally. If you are running PsN on a cluster and/or want\n".
			"to set personalized defaults and/or will run PsN with NMQual,\n".
			"you can manually add the relevant options to the file afterwards.\n".
			"Would you like help to create a configuration file? [y/n] ";
		if (confirm()) {
			$conf_ok = create_conf ("$library_dir"."$directory_separator"."PsN_$name_safe_version"."$directory_separator",$perl_binary);
			$configuration_done = $conf_ok;
		}
	}

}

warn_about_local_configuration();

if ($configuration_done) {
	print "\nInstallation complete.\n";
} else {
	print "\nInstallation partially complete. You still have to create psn.conf before you can run PsN.\n";
	print "A template psn.conf to edit is found in\n";
	print "$library_dir"."$directory_separator"."PsN_$name_safe_version\n"; 
	print "Detailed instructions are found in ";
	print "$library_dir$directory_separator"."PsN_$name_safe_version$directory_separator"."doc$directory_separator"."psn_configuration.pdf"."\n"; 
}

print "\n\nPress ENTER to exit the installation program.\n";
my $dirt = <STDIN>;
