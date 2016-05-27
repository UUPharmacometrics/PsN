package PsN;
use ext::Carp;
use File::Spec;
use Cwd;
use Config;
use strict;

our ($dev,$version,$lib_dir,$config_file,$config);
#the version line is extracted in Makefile using regular expression
# /\$version\s*=\s*.[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*.;/
# so be careful when you edit!!!
$version = '4.6.1';

# The following line will be changed by make
$dev = 1;

my ($volume, $directory, $file) = File::Spec->splitpath(__FILE__);
$lib_dir = Cwd::abs_path($volume . $directory);
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
					   '.imp','.npd','.npe','.npi','.fgh','.log.xml','.cpu','.shm','.agh');

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
