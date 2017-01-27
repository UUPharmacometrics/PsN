package PsN;
use ext::Carp;
use File::Spec;
use Cwd;
use Config;
use strict;

our ($dev,$version,$lib_dir,$config_file,$config,$Rscripts_dir);
#the version line is extracted in Makefile using regular expression
# /\$version\s*=\s*.[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*.;/
# so be careful when you edit!!!
$version = '4.6.15';

# The following line will be changed by make
$dev = 1;

my ($volume, $directory, $file) = File::Spec->splitpath(__FILE__);
$lib_dir = Cwd::abs_path($volume . $directory);
$Rscripts_dir = get_Rscripts_dir($lib_dir);

	
$config_file = $lib_dir . '/psn.conf';

use ext::Config::Tiny;
use ext::File::HomeDir;

our $nm_version;
our $nmdir;
our $nm_major_version;
our $nm_minor_version;

our $out_miss_data;
our $output_header;
our $factorize_strings;

our $warnings_enabled=0;
our @nm7_extensions = ('.ext','.cov','.cor','.coi','.phi','.phm', '.shk','.grd','.xml','.cnv','.smt','.rmt',
					   '.imp','.npd','.npe','.npi','.fgh','.log.xml','.cpu','.shm','.agh',
					   '.vpd','.clt','.npl'); #nm74

# Default disable all warnings except those coming from Getopt
# Enable all warnings if $warnings_enabled is set
#$SIG{__WARN__} = sub {
#        my $package = caller;
#        my $message = shift;
#        if ($warnings_enabled or $package =~ /Getopt::Long/i) {
#                warn $message;
#        }
#};

if( -e home() . "/psn.conf" ){
	$config_file = home() . "/psn.conf";
}

sub get_Rscripts_dir{
	my $lib = shift;
	my $rdir = $lib.'/R-scripts';
	unless (-d $rdir){
		$rdir = $lib.'/../R-scripts';
	}
	if (-d $rdir){
		return Cwd::abs_path($rdir);
	}else{
		return undef;
	}
}

sub import
{
	unless ($config){
		$config = ext::Config::Tiny -> read( $config_file );

		unless( $config ){
			croak("In PsN configuration file[" . $config_file . "]:" . $ext::Config::Tiny::errstr );
		}

		unless( exists $config -> {'low_INF'} ){
			$config -> {'low_INF'} = -1000000;
		}

		unless( exists $config -> {'high_INF'} ){
			$config -> {'high_INF'} = 1000000;
		}

		if ( $config -> {'_'} -> {'output_style'} eq 'SPLUS' ) {
			$out_miss_data = 'NA';
			$output_header = 1;
			$factorize_strings = 0;
		} elsif ( $config -> {'_'} -> {'output_style'} eq 'MATLAB' ) {
			$out_miss_data = 'NaN';
			$output_header = 0;
			$factorize_strings = 1;
		} else { # Default style EXCEL
			$out_miss_data = undef;
			$output_header = 1;
			$factorize_strings = 0;
		}
	}

}


sub get_nmversion_info
{
    # Gets the information about a specific nmversion from the psn.conf
    my $nmversion = shift;

    if (not defined $nmversion) {
        croak("No version label input to get_nmversion_info");
    }

    my @list = split(/,/, $config->{'nm_versions'}->{$nmversion});

    my $dir = shift(@list);
    $dir =~ s/^\s+//g;
    my $version = shift(@list);
    $version =~ s/\s+//g;

    if (not defined $dir or not defined $version) {
        croak("No NONMEM version with name \"".$nmversion.
                    "\" defined in psn.conf. Format should be: name=directory,version");
    }

    my @version_list = split(/\./, $version);
    my $major_version = shift(@version_list);
    my $minor_version = shift(@version_list);

    #make sure minor version is one character
    $minor_version = substr($minor_version, 0, 1) if (defined $minor_version);

    unless ($major_version =~ /^(5|6|7)$/) {
        croak(" NONMEM major version must be either 5,6 or 7. Could not ".
            "extract version from \"".$nmversion.
            "\" in psn.conf. Format should be: name=directory,version");
    }

    return ($dir, $major_version, $minor_version);
}

sub set_nonmem_info
{
    my $version_label = shift;
    unless (defined $version_label){
        croak("No version label input to set_nonmem_info");
    }
    $nm_version = $version_label;

    ($nmdir, $nm_major_version, $nm_minor_version) = get_nmversion_info($version_label);

    #now handle $nmdir that is just name of executable, when in path. Only for run local
    #this is when no slashes, forward or backward, in $nmdir
    #then use which or where to find full path and replace $nmdir with full path after checking exists

    my $result = find_nmfe_from_system_path($nmdir);
    if ($result) {
        $nmdir = $result;
    }
}

sub get_R_exec
{
	import(); #read config if not done already

	my $rexec;
	#check in PsN config, or try R --version
	if ( defined $config -> {'_'} -> {'R'} ) {
		$rexec = $config -> {'_'} -> {'R'};
	}else{
		my $null = '/dev/null';
		if ($Config{osname} eq 'MSWin32'){
			$null = 'NUL';
		}
		my $rc = system('R --version >'.$null.' 2>&1');
		$rc = $rc >> 8;
		if ($rc == 0){
			$rexec = 'R';
		}
	}
    $rexec =~ s/\.exe$//;
	return $rexec;
}

sub get_default_psn_installation_info
{
	my $command = 'psn installation_info 2>/dev/null'; #unix, keep stdout and redirect stderr to /dev/null
	if ($Config{osname} eq 'MSWin32'){
		# keep stdout but redirect stderr to nul
		$command = 'psn installation_info 2>nul';
	}
	my @outp = readpipe($command);
	my $current_config_file;
	my $current_lib_dir;
	my $current_base_lib_dir;
	my $current_bin_dir;
	my $current_version;
	eval(join(' ',@outp));
	if (defined $current_lib_dir){
		my ($volume, $directory, $file) = File::Spec->splitpath($current_lib_dir,1); #no file is true, i.e. only directory
		my @directories = File::Spec->splitdir($directory);
		my $last = pop(@directories);
		$current_base_lib_dir = $volume.File::Spec->catdir(@directories);
	}
	if (defined $current_config_file){
		unless (-e $current_config_file){
			$current_config_file = "\\".$current_config_file; #special case \\ path on windows
			unless (-e $current_config_file){
				$current_config_file = undef;
			}
		}
	}
	return {'config_file'=> $current_config_file,'lib_dir' => $current_lib_dir, 'base_lib_dir' => $current_base_lib_dir,
			'bin_dir' =>$current_bin_dir, 'version' =>$current_version};
}

sub get_new_installation_defaults
{
	my $new_version = shift;
	my $default_installation = shift;
	
	my $name_safe_version = $new_version;
	$name_safe_version =~ s/\./_/g;

	my $base_lib_dir =  $Config{sitelib};

	my $bin_dir = $Config{bin};
	if (($Config{osname} ne 'MSWin32') and (-d '/usr/local/bin')){
		$bin_dir = '/usr/local/bin';
	}
		
	my %hash=('bin_dir' => $bin_dir);

	if (defined $default_installation->{'bin_dir'} and 
		length($default_installation->{'bin_dir'})>0 and
		-d $default_installation->{'bin_dir'}){
		$hash{'bin_dir'} = $default_installation->{'bin_dir'};
	}
	if (defined $default_installation->{'base_lib_dir'} and 
		length($default_installation->{'base_lib_dir'})>0 and
		-d $default_installation->{'base_lib_dir'}){
		$base_lib_dir = $default_installation->{'base_lib_dir'};
	}
	$hash{'base_lib_dir'} = $base_lib_dir;
	my ($volume, $directory, $file) = File::Spec->splitpath($base_lib_dir,1); #no file is true, i.e. only directory
	$hash{'lib_dir'} = $volume.File::Spec->catdir($directory,'PsN_'.$name_safe_version);
	if (defined $default_installation->{'config_file'} and
		length($default_installation->{'config_file'})>0 and
		-e $default_installation->{'config_file'}){
		$hash{'old_config_file'} = $default_installation->{'config_file'};
	}else{
		$hash{'old_config_file'} = undef;
	}
	if (defined $default_installation->{'version'} and
		length($default_installation->{'version'})>0){
		$hash{'old_default_version'} = $default_installation->{'version'};
	}else{
		$hash{'old_default_version'} = undef;
	}
	return \%hash;
}

sub find_nmfe_from_system_path
{
	my $string = shift;
	my $result = 0;
	if (($string =~ /\\/) or ($string =~ /\//)){
		#slashes, assume full path, do nothing
		1;
	}elsif(length($string) < 1){
		1; #deal with error elsewhere
	}else{
		my $command = "which $string".' 2>/dev/null'; #unix, keep stdout and redirect stderr to /dev/null
		if ($Config{osname} eq 'MSWin32'){
			#where does nor work on XP, use this loop
			# keep stdout but redirect stderr to nul
			$command = 'for %i in ('.$string.','.$string.'.bat,'.$string.'.exe) do @echo.   %~$PATH:i 2>nul';
		}
		my @outp = readpipe($command);
		foreach my $line (@outp){
			next unless (defined $line);
			chomp($line);
			$line =~ s/^\s*//;
			$line =~ s/\s*$//;
			if ((length($line)>0) and -e $line){
				$result = $line;
				last;
			} 
		}
	}
	return $result;
}

1;
