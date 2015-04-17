package PsN;
use ext::Carp;
use File::Spec;
use Cwd;
use Config;

#the version line is extracted in Makefile using regular expression
# /\$version\s*=\s*.[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*.;/
# so be careful when you edit!!!
$version = '4.4.0';

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

our $warnings_enabled;
our @nm7_extensions = ('.ext','.cov','.cor','.coi','.phi','.phm', '.shk','.grd','.xml','.cnv','.smt','.rmt',
					   '.imp','.npd','.npe','.npi','.fgh','.log.xml');


# Default disable all warnings except those coming from Getopt
# Enable all warnings if $warnings_enabled is set
$SIG{__WARN__} = sub {
        my $package = caller;
        my $message = shift;
        if ($warnings_enabled or $package =~ /Getopt::Long/i) {
                warn $message;
        }
};

$config = ext::Config::Tiny -> read( $config_file );

unless( $config ){
  croak("In PsN configuration file[" . $config_file . "]:" . $ext::Config::Tiny::errstr );
}

if( -e home() . "/psn.conf" ){
  my $personal_config = ext::Config::Tiny -> read( home() . '/psn.conf' );
  %{$config} = (%{$config}, %{$personal_config});
}

unless( exists $config -> {'low_INF'} ){
  $config -> {'low_INF'} = -1000000;
}

unless( exists $config -> {'high_INF'} ){
  $config -> {'high_INF'} = 1000000;
}

our $out_miss_data;
our $output_header;
our $factorize_strings;
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

sub set_nonmem_info
{
  my $version_label = shift;
  unless (defined $version_label){
    croak("No version label input to set_nonmem_info");
  }
	$nm_version = $version_label;

  #reset values if set earlier
  $nmdir = undef;
  $nm_major_version = undef;
  $nm_minor_version = undef;
  my $version = undef;
  my @list = split(/,/ , $config -> {'nm_versions'} -> { $version_label } );
  $nmdir = shift(@list);
  $nmdir =~ s/^\s+//g;
  $version = shift(@list);
  $version =~ s/\s+//g;

  unless (defined $nmdir){
    croak("No NONMEM version with name \"".$version_label.
                    "\" defined in psn.conf. Format should be: name=directory,version");
  }
  unless (defined $version){
    croak("No NONMEM version with name \"".$version_label.
                    "\" defined in psn.conf. Format should be: name=directory,version");
  }

  my @list2 = split(/\./ , $version);
  $nm_major_version = shift(@list2);
  my $minor = shift(@list2);
  #make sure minor version is one character
  $nm_minor_version = substr($minor,0,1) if (defined $minor);

  unless ($nm_major_version =~ /^(5|6|7)$/){
    croak(" NONMEM major version must be either 5,6 or 7. Could not ".
                    "extract version from \"".$version_label.
                    "\" in psn.conf. Format should be: name=directory,version");
  }

  #now handle $nmdir that is just name of executable, when in path. Only for run local
  #this is when no slashes, forward or backward, in $nmdir
  #then use which or where to find full path and replace $nmdir with full path after checking exists

  my $result = find_nmfe_from_system_path($nmdir);
  if ($result){
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
		my $command = "which $string"; #unix
		if ($Config{osname} eq 'MSWin32'){
			#where does nor work on XP, use this loop
			$command = 'for %i in ('.$string.','.$string.'.bat,'.$string.'.exe) do @echo.   %~$PATH:i';
		}
		my @outp = readpipe($command);
		foreach my $line (@outp){
			next unless (defined $line);
			chomp($line);
			$line =~ s/^\s*//;
			if (-e $line){
				$result = $line;
				last;
			} 
		}
	}
	return $result;
}

1;
