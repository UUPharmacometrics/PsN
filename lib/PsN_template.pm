use ext::Carp;
use ext::Config::Tiny;
use ext::File::HomeDir;

$nmdir;
$nm_major_version;
$nm_minor_version;
$compiler_label;

$warnings_enabled;

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

$out_miss_data;
$output_header;
$factorize_strings;
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

sub set_nonmem_info {
  my $version_label = shift;
  unless (defined $version_label){
    croak("No version label input to set_nonmem_info");
  }
  #reset values if set earlier
  $nmdir = undef;
  $nm_major_version = undef;
  $nm_minor_version = undef;
  $compiler_label = undef;
  my $version = undef;
  my @list = split(/,/ , $config -> {'nm_versions'} -> { $version_label } );
  #($nmdir,$version,$compiler_label) = split(/,/ , $config -> {'nm_versions'} -> { $version_label } );
  $nmdir = shift(@list);
  $nmdir =~ s/^\s+//g;
  $version = shift(@list);
  $version =~ s/\s+//g;
  $compiler_label = shift(@list);

  unless (defined $nmdir){
    croak("No NONMEM version with name \"".$version_label.
		    "\" defined in psn.conf. Format should be: name=directory,version ".
		    "or name=directory,version,compiler." );
  }
  unless (defined $version){
    croak("No NONMEM version with name \"".$version_label.
		    "\" defined in psn.conf. Format should be: name=directory,version ".
		    "or name=directory,version,compiler." );
  }

  my @list2 = split(/\./ , $version);
  $nm_major_version = shift(@list2);
  my $minor = shift(@list2);
  #make sure minor version is one character
  $nm_minor_version = substr($minor,0,1) if (defined $minor);

  unless ($nm_major_version =~ /^(5|6|7)$/){
    croak(" NONMEM major version must be either 5,6 or 7. Could not ".
		    "extract version from \"".$version_label.
		    "\" in psn.conf. Format should be: name=directory,version ".
		    "or name=directory,version,compiler" );
  }

}

1;
