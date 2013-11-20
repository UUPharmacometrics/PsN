use strict;
use Config;
use File::Spec;
use File::Path qw(mkpath);
use File::Glob;
use lib 'lib';
use ext::Config::Tiny;
use ext::File::HomeDir;


my $version = '4.0.0';
my $default_user_name;
my $default_sitelib;
my $default_bin;
my $default_perlpath;
my $use_user_name;
my $is_root=0;
my $uid;
my $gid;
my $have_file_copy=0;
if (eval('require File::Copy::Recursive')){
  eval('import File::Copy::Recursive qw/fcopy rcopy dircopy fmove rmove dirmove/');
  $have_file_copy=1;
  print "\nInformation: Using File::Copy::Recursive for copying\n"; 
}


my $is_win7=0;
my $is_Vista=0;
if ( $Config{osname} eq 'MSWin32' ){
  if (eval('require Win32')){ 
    #enough, now loaded
    my $winver=Win32::GetOSName();
    $is_win7 = 1 if ($winver eq 'Win7');
	$is_Vista = 1 if ($winver eq 'WinVista');

#Currently the possible values for the OS name are
#    WinWin32s
#    Win95
#    Win98
#    WinMe
#    WinNT3.51
#    WinNT4
#    Win2000
#    WinXP/.Net
#    Win2003
#    WinHomeSvr
#    WinVista
#    Win2008
#    Win7
  }
}
if (1){
	$default_sitelib = $Config{sitelib};
	$default_bin = $Config{bin};
	$default_perlpath = $Config{perlpath};
	if ( $Config{osname} eq 'MSWin32' ){
		#"trying to find the perl binary via system command\n";
		my $wherebin;
		if ($is_win7 or $is_Vista){
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

		if ((defined $default_sitelib) and (-e $default_sitelib) and (-d $default_sitelib)){
			#default-sitelib ok
			1;
		}elsif(length($local_sitelib)>0 and -d $local_sitelib){
			$default_sitelib=$local_sitelib;
		}else{
			$default_sitelib=undef;
		}
		if ((defined $default_bin) and (-e $default_bin) and (-d $default_bin)){
			#default  ok
			1;
		}elsif(length($local_bin)>0 and -d $local_bin){
			$default_bin=$local_bin;
		}else{
			$default_bin=undef;
		}
		if ((defined $default_perlpath) and (-e $default_perlpath) and (-x $default_perlpath) and (not -d $default_perlpath)){
			#default ok
			1;
		}elsif(length($local_perlpath)>0 and -x $local_perlpath and (not -d $local_perlpath)){
			$default_perlpath=$local_perlpath;
		}else{
			$default_perlpath=undef;
		}
	}
	unless (defined $default_perlpath and defined $default_bin and defined $default_sitelib){
		print "\nWarning: There is something unusual with your Perl installation and configuration.\n".
			"Will try to install anyway, but there may be problems.\n";
	}

}





my $name_safe_version = $version;
$name_safe_version =~ s/\./_/g;
my @utilities = (
		 'bootstrap', 'cdd', 'execute', 'llp', 'scm', 'sumo', 'sse',
		 'data_stats',
		 'se_of_eta',
		 'update_inits',
		 'npc',
		 'vpc',
		 'pind','nonpb','extended_grid','psn','psn_options','psn_clean',
		 'runrecord','mcmp','lasso','mimp','xv_scm','parallel_retries',
                 'boot_scm','gls','ebe_npde','frem','randtest','linearize'
		 );

my @win_modules = ('Math::Random');
my @nix_modules = ('Math::Random','Storable');
my @recommended_modules = ('Archive::Zip','Statistics::Distributions');

my @modules;

sub confirm {
 while (1){
   my $input = <STDIN>;
   if( $input =~ /^\s*[yY]\s*$/ ){
     return 1;
   }elsif ( $input =~ /^\s*[nN]\s*$/ ){
     return 0;
   }else{
     print "Please answer y for yes or n for no: ";
   }
 }
}


sub get_default_nm_versions 
{
  my %versionhash;
  if ( $Config{osname} eq 'MSWin32' ){
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
  if( $Config{osname} eq 'MSWin32' ){
    if ($dir =~ /^[A-Za-z]:\\/){
      1;
    }elsif ($dir =~ /^%USERPROFILE%/){
      my $home = $ENV{USERPROFILE};
      if (defined $home){
	$dir =~ s/^%USERPROFILE%/$home/;
#	print "$dir\n";
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
#      print "$dir\n";
    }else{
      print "$dir does not look like a full search path (does not begin with /). Please try again.\n";
      return '0','0';
    }
  }

  my $version;
  if ( $Config{osname} eq 'MSWin32' ){
    $version=6 if ((-d $dir) and (
		     ((-d $dir.'\run') and (-x $dir.'\run\nmfe6.bat')) or 
		     ((-d $dir.'\util') and (-x $dir.'\util\nmfe6.bat'))
		   ));
    if ((-d $dir) and ((-d $dir.'\run') or (-d $dir.'\util'))){
      if ((-x $dir.'\run\nmfe72.bat') or (-x $dir.'\util\nmfe72.bat')){
	$version='7.2';
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
      }elsif ((-x $dir.'/run/nmfe7') or (-x $dir.'/util/nmfe7')){
	$version='7.1';
      }
    }
  }

  unless (defined $version){
    print "Did not find an nmfe script in the util or run or . subdirectory of $dir. Please try again\n";
    return '0','0';
  }
  return $dir,$version;
}

sub get_psnname {
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

sub create_conf {
  my $confpath = shift;
  my $perlbin = shift;
  my $template = $confpath.'psn.conf_template';
  my $newconf = $confpath.'psn.conf';

  unless (-e $template){
    print "Error: Could not find $template.\n"."No config file created\n";
    return 0;
  }
  my $config = ext::Config::Tiny -> read( $template );
  #fix perl
  $config -> {'_'} -> {'perl'} = $perlbin;


  #fix R
  my @paths;
  my $file;
  if ( $Config{osname} eq 'MSWin32' ){
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
      if ( $Config{osname} eq 'MSWin32' ){
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
    
  }else{
    print "\nError: No NM versions defined. You must add at least one manually to $newconf, otherwise PsN will not run.";
  }

  $config -> write($newconf );

  print "\n\nA new configuration file has been created:\n$newconf\n";
  print "If you want to add cluster options or personalized defaults or\n".
      "NMqual settings you can do this manually by editing\n"."$newconf\n".
      "in a regular text editor.\n";
  return 1;

}


sub quit {
 my $reason = shift;
 if ($reason eq 'abort'){
   print "\n\nThe installation process has been aborted, PsN was not installed.";
 }
 print "\n\nPress ENTER to exit the installation program.\n";
 my $dirt=<STDIN>;
 exit;
}

sub get_input {
	my $default = shift;
	my $input = <STDIN>;
	chomp( $input );
	if( $input =~ /^\s*$/ ){
		if (defined $default){
			return( $default );
		}else{
			print "No input given and no default available, must exit.\n";
			quit('abort');
		}
	} else {
		return( $input );
	}

}

$| = 1; # Make sure autoflush is on
$, = ',';
print "\nThis is the PsN installer. I will install PsN version $version.\n".
    "You need to answer a few questions. If a default value is presented\n".
    "you may accept it by pressing ENTER.\n\n";
$, ='';

if( $Config{osname} eq 'linux' or $Config{osname} eq 'darwin'){
 my $user = `whoami`;
 chomp($user);
 if( $user =~ /^root$/ ){
   $is_root=1;
   my $home = $ENV{HOME};
   if ($home =~ /\/([^\/]*)$/){
     $default_user_name=$1;
   }
 }else{
   print "Hi $user, you don't look like root. Please note that you need root privileges to install PsN systemwide.\n";
 }
}else{
  unless ($Config{osname} eq 'MSWin32'){
    print "OS ".$Config{osname}." is not explicitly supported by PsN, but if it is a unix/Mac type\n".
	"system everything should work anyway. Problems are expected only if it is a Windows system.\n".
	"Do you want to proceed with the installation [y/n] ?\n";
    quit('abort') unless( confirm() );
  }
}

my $binary_dir;
my $library_dir;
my $perl_binary;
my $add_psn_aliases=0;
my $old_psn_config_file;

if (0){
  print "Would you like to allow using prefix psn_ for all PsN program names,\n".
      "for example psn_execute for execute? Only needed if there is already installed\n".
      "software with the same name, e.g. scm, as any of the PsN programs. [y/n]\n";
  if( confirm() ){
    $add_psn_aliases=1;
  }
}


print "PsN Utilities installation directory [$default_bin]:";

$binary_dir = get_input( $default_bin );

unless( -e $binary_dir ){
 print "Directory $binary_dir does not exist. Would you like to create it?[y/n]\n";
 if( confirm() ){
   unless( mkpath( $binary_dir ) ){ 
     print "Unable to create $binary_dir.\n"; 
     quit('abort');
   }
 } else {
   quit('abort');
 }
}

print "Path to perl binary used to run Utilities [$default_perlpath]:";

$perl_binary = get_input( $default_perlpath );


(my $volume,my $directory,my $file ) = File::Spec->splitpath( $perl_binary);
my $runperl_name="runperl.bat";
my $runperl_binary = File::Spec->catpath( $volume, $directory, $runperl_name);

#print "\n $runperl_binary \n";
if(( $Config{osname} eq 'MSWin32' ) and ($perl_binary ne 'C:\Perl\bin\perl.exe')){
  print "Warning: If you are running on Windows and the path to the perl binary \n".
      "set in the previous question was something other than ".'C:\Perl\bin\perl.exe'.
      "\n".
      "the same path must be set in psn.conf. This will be done for you if you create ".
      "the configuration file interactively.\n\n"

}

print "PsN Core and Toolkit installation directory [$default_sitelib]:";

$library_dir = get_input( $default_sitelib );

unless( -e $library_dir ){
 print "Directory $library_dir does not exist. Would you like to create it? [y/n]\n";
 if( confirm() ){
   unless( mkpath( $library_dir ) ){ print "Unable to create $library_dir.\n"; quit('abort') };
 } else {
   quit('abort');
 }
}
my $copy_cmd;
my $copy_recursive_cmd;

if( $Config{osname} eq 'MSWin32' ){
 $copy_cmd = "copy /Y";
 $copy_recursive_cmd = "xcopy /Y /E";
 @modules = @win_modules;
} else {
 @modules = @nix_modules;
 $copy_cmd = "cp";
 $copy_recursive_cmd = "cp -r";
}


print "\n"."The next step is to check Perl module dependencies.
If a module is missing, you must install it, e.g. from CPAN,
http://www.cpan.org/modules/index.html
before PsN can be run.

Would you like this script to check Perl modules [y/n]?";

if(confirm()){  
  print "\nTesting required modules:\n";
  foreach my $module (@modules){
    my $ok=0;
    $ok=1 if eval("require ".$module);
    if ($ok){
      print "Module $module ok\n";
    }else{
      print "Perl module $module is missing, you must install it before running PsN.\n";
    }
  } 
  print "\nDone testing required modules.\n";
  print "\nTesting recommended but not required modules...\n";
  foreach my $module (@recommended_modules){
    my $ok=0;
    $ok=1 if eval("require ".$module);
    if ($ok){
      print "Module $module ok\n";
    }else{
      print "Perl module $module is missing, you can run PsN without it but some features will be disabled.\n";
    }
  } 

  print "Tests done.\n\n";
  print "Continue installing PsN (installing is possible even if modules are missing)[y/n]?";
  quit('abort') unless (confirm());
}

my $overwrite = 0;
my $old_version = 'X_X_X';
my $keep_conf = 0;

unless( mkpath( "$library_dir/PsN_$name_safe_version" ) ){
  print "Failed to create $library_dir/PsN_$name_safe_version: $!\n";
  print "PsN is probably already installed. Would you like to continue anyway [y/n] ?";
  quit('abort') unless( confirm() );
}

if( -e "$library_dir/PsN_$name_safe_version/psn.conf" ){
  print "An older version of psn.conf exists in $library_dir/PsN_$name_safe_version.\n";
  print "Keep the old version [y/n] ?";
  $keep_conf = confirm();
}


my $newconf = File::Spec -> catfile( $library_dir, "PsN_$name_safe_version", "psn.conf" );
my $thelibdir = File::Spec -> catdir($library_dir,"PsN_$name_safe_version");
if ($have_file_copy){
  if ($keep_conf){
    unless (fcopy($newconf,"old.conf")){
      print "Could not copy $newconf to old.conf : $!\n";
      quit('abort');
    }
  }
  unless (dircopy("lib", $thelibdir)){
    print " Could not copy contents of lib directory to $thelibdir : $!\n";
    quit('abort');
  }
  if ($keep_conf){
    unless (fcopy("old.conf",$newconf )){
      print "Could not copy old.conf to $newconf : $!\n";
      quit('abort');
    }
  }
} else{
  #do not have File::Copy
  system( $copy_cmd . " \"" . $newconf . "\" old.conf" ) if $keep_conf;
  
  my $full_command = $copy_recursive_cmd . " " . File::Spec -> catfile( "lib", "*" ) . " \"" . $thelibdir . "\""; 
  system($full_command);

  unless (-e "$library_dir" . "/PsN_$name_safe_version/nonmem.pm" ){
    print "Copying of files to $library_dir" . "/PsN_$name_safe_version/".
	" failed, the following command did not work\n$full_command\n";
    if( $Config{osname} eq 'MSWin32' ){
      print "It is recommended to run the command\n".
	  "ppm install file-copy-recursive\n".
	  "in a command window, and then try to install PsN again, using the same setup script.\n";
    }else{
      print "It is recommended to install File::Copy::Recursive".
	  "and then try to install PsN again, using the same setup script.\n";
    }
    quit('abort');
  }
  
  system( $copy_cmd . " old.conf \"" . $newconf . "\"" ) if $keep_conf;
}

my $confirmed = 0;
foreach my $file ( @utilities ){
  
  #system( $copy_cmd . " " . File::Spec -> catfile( "bin", $file ) . " " . File::Spec -> catfile( $binary_dir, "$file-$version" ) );
  
  unless (open( INST , "<" , File::Spec -> catfile( "bin", $file ) )){
    print "Could not open ".File::Spec -> catfile( "bin", $file ).
	" for reading, unable to install $file: $!\n";
    quit('abort');
  }
  unless (open( UTIL , ">" , File::Spec -> catfile( $binary_dir, "$file-$version" ) )){ 
    print "Could not open ".File::Spec -> catfile( $binary_dir, "$file-$version").
	" for writing, unable to install $file: $!\n";
    quit('abort');
  }
  my $replace_line_found = 0;
  while( <INST> ){
    if( /\# Everything above this line will be replaced \#/ ){
      print UTIL "\#!" , $perl_binary , "\n";
      print UTIL "use lib '$library_dir';\n\n";
      print UTIL "\# Everything above this line was entered by the setup script \#\n";
      $replace_line_found = 1;
    } elsif( /use PsN/ ){
      print UTIL "use PsN_$name_safe_version;\n";
    } elsif( /require PsN/ ){
      print UTIL "require PsN_$name_safe_version;\n";
    } elsif( $replace_line_found ){
      print UTIL $_;
    }
  }
  close UTIL;
  close INST;
  
  chmod( 0755, File::Spec -> catfile( $binary_dir, "$file-$version" ) );
  
  
  my $copy_the_binaries=0;
  if( -e "$binary_dir/$file" ){
    
    if( $Config{osname} ne 'MSWin32' ){
      my $link = readlink( "$binary_dir/execute" );
      if( $old_version eq 'X_X_X' and not($link eq '') ) {
	$link =~ /-(\d+\.\d+\.\d+)/;
	$old_version = $1;
      }
    }
    
    if( $Config{osname} eq 'MSWin32' ){
      my $link = open(IN,"$binary_dir/execute");
      if( $old_version eq 'X_X_X' and $link) {
	while(<IN>){
	  if(/^use PsN/){
	    s/^use PsN\_//;
	    s/\s.*//;
	    s/\_/\./g;
	    s/\;//;
	    $old_version=$_;
	  }
	}
	close(IN);
      }
    }
    my $tmp = $old_version;
    $tmp =~ s/\./\_/g;
    $old_psn_config_file = File::Spec -> catfile( $library_dir, "PsN_$tmp", "psn.conf" );
    $old_psn_config_file = undef unless (-e $old_psn_config_file);
    
    if( $old_version eq $version ){
      
      if( not $confirmed ){
	
	print( "\nThis version ($version) looks like an older installed\n",
	       "version ($old_version) of PsN. Would you like to make\n",
	       "this version ($version) the default? [y/n]" );
	
	$confirmed = 1;
	$overwrite = confirm();
      }
    }
    if( not $confirmed ){
      
      print( "\nAn older version($old_version) of PsN is installed. Would you like to\n",
	     "make this version ($version) the default? [y/n]" );
      
      $confirmed = 1;
      $overwrite = confirm();
    }
    if( $overwrite ){
	unlink( "$binary_dir/$file" );
	$copy_the_binaries=1;
    }
    
    
  } else {
    $copy_the_binaries=1;
  }  
  
  if ($copy_the_binaries){
    if( $Config{osname} eq 'MSWin32' ){
      if ($have_file_copy ){
	unless (fcopy("$binary_dir\\$file-$version","$binary_dir\\$file")){
	  print "Could not copy $binary_dir\\$file-$version to  $binary_dir\\$file : $!\n";
	  quit('abort');
	}
	unless (fcopy("$runperl_binary","$binary_dir\\$file.bat")){
	  print "Could not copy $runperl_binary to $binary_dir\\$file.bat : $!\n";
	  quit('abort');
	}
	if ($add_psn_aliases){
	  unless (fcopy("$binary_dir\\$file-$version","$binary_dir\\psn_$file")){
	    print "Could not copy $binary_dir\\$file-$version to  $binary_dir\\psn_$file : $!\n";
	    quit('abort');
	  }
	  unless (fcopy("$runperl_binary","$binary_dir\\psn_$file.bat")){
	    print "Could not copy $runperl_binary to $binary_dir\\psn_$file.bat : $!\n";
	    quit('abort');
	  }
	}
      }else{
	#win but no file copy
	system( "copy /Y \"$binary_dir\\$file-$version\" \"$binary_dir\\$file\"" );
	system( "copy /Y \"$runperl_binary\" \"$binary_dir\\$file.bat\"" );
	if ($add_psn_aliases){
	  system( "copy /Y \"$binary_dir\\$file-$version\" \"$binary_dir\\psn_$file\"" );
	  system( "copy /Y \"$runperl_binary\" \"$binary_dir\\psn_$file.bat\"" );
	}
      }
    } else {
      #unix
      symlink( "$binary_dir/$file-$version", "$binary_dir/$file" );
      if ($add_psn_aliases){
	symlink( "$binary_dir/$file-$version", "$binary_dir/psn_$file" );
      }
    }
    if ($file eq 'update_inits'){
      if( $Config{osname} eq 'MSWin32' ){
	if ($have_file_copy ){
	  unless (fcopy("$binary_dir\\$file-$version","$binary_dir\\update")){
	    print "Could not copy $binary_dir\\$file-$version to  $binary_dir\\update : $!\n";
	    quit('abort');
	  }
	  unless (fcopy("$runperl_binary","$binary_dir\\update.bat")){
	    print "Could not copy $runperl_binary to $binary_dir\\update.bat : $!\n";
	    quit('abort');
	  }
	  if ($add_psn_aliases){
	    unless (fcopy("$binary_dir\\$file-$version","$binary_dir\\psn_update")){
	      print "Could not copy $binary_dir\\$file-$version to  $binary_dir\\psn_update : $!\n";
	      quit('abort');
	    }
	    unless (fcopy("$runperl_binary","$binary_dir\\psn_update.bat")){
	      print "Could not copy $runperl_binary to $binary_dir\\psn_update.bat : $!\n";
	      quit('abort');
	    }
	  }
	}else{
	  #win but no file copy
	  system( "copy /Y \"$binary_dir\\$file-$version\" \"$binary_dir\\update\"" );
	  system( "copy /Y \"$runperl_binary\" \"$binary_dir\\update.bat\"" );
	  if ($add_psn_aliases){
	    system( "copy /Y \"$binary_dir\\$file-$version\" \"$binary_dir\\psn_update\"" );
	    system( "copy /Y \"$runperl_binary\" \"$binary_dir\\psn_update.bat\"" );
	  }
	}
      } else {
	#unix
#	symlink( "$binary_dir/$file-$version", "$binary_dir/update" );
	if ($add_psn_aliases){
	  symlink( "$binary_dir/$file-$version", "$binary_dir/psn_update" );
	}
      }
    }
  }
}

unless (open( TEMPLATE, "lib/PsN_template.pm" )){
  print "Unable to open PsN_template.pm in lib: $!\n";
  quit('abort');
}
unless (open( PSN, '>', "$library_dir" . "/PsN_$name_safe_version.pm" )){
  print "Unable to install PsN_$name_safe_version.pm in $library_dir: $!\n";
  quit('abort');
}

if( $Config{osname} eq 'MSWin32' ){
  require Win32;
  $library_dir = Win32::GetShortPathName($library_dir);
}

print( PSN "package PsN;\n" );
print( PSN "use lib '$library_dir/PsN_$name_safe_version';\n" );
print( PSN "\$lib_dir = '$library_dir/PsN_$name_safe_version';\n" );
print( PSN "\$config_file = '$library_dir/PsN_$name_safe_version/psn.conf';\n" );
print( PSN "\$version = '$version';\n" );

for ( <TEMPLATE> ) {
  print PSN $_;
}
close( PSN );
close( TEMPLATE );


my $copy_PsNpm=0;

if( -e "$library_dir/PsN.pm" ){
  if( $overwrite ){
    unlink( "$library_dir/PsN.pm" );
    $copy_PsNpm=1;
  }
}else{
  $copy_PsNpm=1;
}
if ($copy_PsNpm){
  if( $Config{osname} eq 'MSWin32' ){
    if ($have_file_copy ){
      unless (fcopy("$library_dir\\PsN_$name_safe_version.pm","$library_dir\\PsN.pm")){
	print "Could not copy $library_dir\\PsN_$name_safe_version.pm to $library_dir\\PsN.pm : $!\n";
	quit('abort');
      }
    }else{
      system("copy \"$library_dir\\PsN_$name_safe_version.pm\" \"$library_dir\\PsN.pm\"" );
    }
  } else {
    symlink( "$library_dir/PsN_$name_safe_version.pm", "$library_dir/PsN.pm" );
  }
}

unless (open( PSN, '<', "$library_dir" . "/PsN_$name_safe_version/nonmem.pm" )){
  print "Unable to install PsN-$name_safe_version/nonmem.pm in $library_dir: $!\n";
  quit('abort');
}

my @nonmem_pm = <PSN>;

close( PSN );

unless (open( PSN, '>', "$library_dir" . "/PsN_$name_safe_version/nonmem.pm" )){
  print "Unable to install PsN_$name_safe_version/nonmem.pm in $library_dir: $!\n";
  quit('abort');
}

for( @nonmem_pm ){
  if( /require PsN/ ){
    print PSN "require PsN_$name_safe_version;\n";
  } else {
    print PSN;
  }
}
close PsN;

my $dir_sep = "/";
my $folder='directory';
if( $Config{osname} eq 'MSWin32' ){
  $dir_sep = "\\";  
  $folder = 'folder';
}

print "\nWould you like to copy the PsN documentation to a file system location of your choice?  [y/n] ";
if (confirm()){
  print "The documentation will be copied to a $folder named PsN_documentation\n";
  my $ok=0;
  my $input;
  my $default;
  if( $Config{osname} eq 'MSWin32' ){
    my $home = $ENV{USERPROFILE};
    if ($is_win7){
      $default = $home.'\Desktop';
    }else{
      $default = $home.'\Start Menu';
    }
    $default = '' unless (defined $home);
  }else{
    my $home = home();
    $default = $home;
  }
  while (not $ok){
    if (length($default)>0){
      print "Please enter the full search path to where do you want to place this $folder,\nor press ENTER to use $default\n : ";
      $input = <STDIN>;
      chomp( $input );
      $input = $default unless (length($input)>0);
    }else{
      print "Please enter the full search path to where do you want to place this $folder\n : ";
      $input = <STDIN>;
      chomp( $input );
    }
    if( $Config{osname} eq 'MSWin32' ){
      if ($input =~ /^[A-Za-z]:\\/){
	$ok=1;
      }elsif ($input =~ /^%USERPROFILE%/){
	my $home = $ENV{USERPROFILE};
	if (defined $home){
	  $input =~ s/^%USERPROFILE%/$home/;
#	  print "$input\n";
	  $ok=1;
	}else{
	  print "$input does not look like a full search path including drive letter. Please try again.\n";
	}
      }else{
	print "$input does not look like a full search path including drive letter. Please try again.\n";
      }
    }else{
      if ($input =~ /^\//){
	$ok=1;
      }elsif ($input =~ /^~/){
	my $home=home();
	$input =~ s/^~/$home/;
	$ok=1;
#	print "$input\n";
      }else{
	print "$input does not look like a full search path (does not begin with /). Please try again.\n";
      }
    }
  }

  if ($is_root){
    if (length($default_user_name)>0){
      print "Please enter the user name for the owner of the documentation copies,\nor press ENTER to use $default_user_name\n : ";
      $use_user_name = <STDIN>;
      chomp( $use_user_name );
      $use_user_name = $default_user_name unless (length($use_user_name)>0);
    }else{
      print "Please enter the user name for the owner of the documentation files copy (it is unpractical if it is root)\n : ";
      $use_user_name = <STDIN>;
      chomp( $use_user_name );
    }
    if (length($use_user_name)>0){
      my ($login,$pass);
      unless (($login,$pass,$uid,$gid) = getpwnam($use_user_name)){
	$ok=0;
	print "$use_user_name not found, please try again\n";
      }
    }
  }

  unless( -d $input ){
    print "$input does not exist. Would you like to create it? [y/n] ";
    if( confirm() ){
      unless( mkpath( $input ) ){ 
	print "Unable to create $input. Cannot copy documentation\n";
	$ok=0;
      }
    } else {
	print "Cannot copy documentation\n";
	$ok=0;
    }
  }
  my $newdir = File::Spec -> catdir($input,"PsN_documentation");
  if ($ok){
    if( mkpath( $newdir ) ){ 
#      print "made $newdir\n";
      unless ( $Config{osname} eq 'MSWin32' ){
	if (length($use_user_name)>0){
	  my @arr = ($newdir);
	  chown $uid, $gid, @arr;
	}
      }
    }else{ 
#      print "did not make  $newdir\n";
      if (-d $newdir){
	unless ( $Config{osname} eq 'MSWin32' ){
	  if (length($use_user_name)>0){
	    my @arr = ($newdir);
	    chown $uid, $gid, @arr;
	  }
	}
      }else{
	print "Unable to create $newdir. Cannot copy documentation\n";
	$ok=0;
      }
    }
  }
  if ($ok){
    if ($have_file_copy){
      my @file_list;
      if ( $Config{osname} eq 'MSWin32' ){
	@file_list = <lib\\doc\\*.scm lib\\doc\\*.pdf lib\\doc\\*.xls>;
      }else{
	@file_list = <lib/doc/*.scm lib/doc/*.pdf lib/doc/*.xls>;
      }
      my $dest = File::Spec -> catfile( "$newdir","." );
      foreach my $ff (@file_list){
	fcopy($ff, $dest);
      }
    }else{
      my $full_command = $copy_recursive_cmd . " " . File::Spec -> catfile( "lib","doc","*.scm" ) . " \"" .
	  $newdir. "\""; 
      system($full_command);
      my $full_command = $copy_recursive_cmd . " " . File::Spec -> catfile( "lib","doc","*.pdf" ) . " \"" .
	  $newdir. "\""; 
      system($full_command);
      my $full_command = $copy_recursive_cmd . " " . File::Spec -> catfile( "lib","doc","*.xls" ) . " \"" .
	  $newdir. "\""; 
      system($full_command);
    }
  }
  if ($ok and (-e "$newdir" . "/bootstrap_userguide.pdf") ){
    unless ( $Config{osname} eq 'MSWin32' ){
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


my $configuration_done=0;
$configuration_done=1 if ($keep_conf);

if (not $keep_conf){
  my $conf_ok = 0;
  print "\nNow you must edit "."$library_dir"."$dir_sep"."PsN_$name_safe_version"."$dir_sep".
      "psn.conf\n"."so that PsN can find your NONMEM installations.\n";

  my $offer_help=1;
  if (defined $old_psn_config_file){
    print "Would you like to copy psn.conf from the previous default version ".
	"to this new installation? [y/n] ";
    my $newf = File::Spec -> catfile( $library_dir,"PsN_$name_safe_version","psn.conf" );

    if (confirm()){
      if ($have_file_copy){
	fcopy($old_psn_config_file,$newf);
      }else{
	system($copy_cmd." \"".$old_psn_config_file."\" \"".$newf. "\"") ;
      }
      if (-e $newf){
	$offer_help=0;
	$conf_ok=1;
	$configuration_done=1;
	print "Copied $old_psn_config_file to $newf.\n";
      }else{
	print "Copying of old psn.conf failed.\n";
      }
    }
  }
  if ($offer_help){
    print "You can get help to create a bare-bones configuration file that will work\n".
	"when running PsN locally. If you are running PsN on a cluster and/or want\n".
	"to set personalized defaults and/or will run PsN with NMQual,\n".
	"you can manually add the relevant options to the file afterwards.\n".
	"Would you like help to create a configuration file? [y/n] ";
    if (confirm()){
      $conf_ok = 
	  create_conf ("$library_dir"."$dir_sep"."PsN_$name_safe_version"."$dir_sep",$perl_binary);
	  $configuration_done=$conf_ok;
    }
  }

}

if ($configuration_done ){
	print "\nInstallation complete.\n";
}else{
	print "\nInstallation partially complete. You still have to create psn.conf before you can run PsN.\n";
    print "A template psn.conf to edit is found in\n";
    print "$library_dir"."$dir_sep"."PsN_$name_safe_version\n"; 
    print "Detailed instructions are found in ";
    print "$library_dir$dir_sep"."PsN_$name_safe_version$dir_sep"."doc$dir_sep"."psn_configuration.pdf"."\n"; 
}



if( -e home() . "/psn.conf" ){
  print "\nPlease note that for you personally the configuration file you\n".
      "already have in ".home()."$dir_sep"."psn.conf\n".
      "will override the settings in\n".
      "$library_dir"."$dir_sep"."PsN_$name_safe_version"."$dir_sep".
      "psn.conf\n";

}elsif( $Config{osname} eq 'MSWin32' ){
  print "\nPlease note that if you have a psn.conf file in your Desktop folder,\n".
      "the settings in that file will override the settings in\n".
      "$library_dir"."$dir_sep"."PsN_$name_safe_version"."$dir_sep".
      "psn.conf\n";
}else{
  print "\nPlease note that if you have a psn.conf file in your home directory,\n".
      "the settings in that file will override the settings in\n".
      "$library_dir"."$dir_sep"."PsN_$name_safe_version"."$dir_sep".
      "psn.conf\n";
}

quit('done');
#

# Uninstall ?
# rm -rf /usr/lib/perl5/site_perl/5.8.4/PsN* /usr/bin/*-2.1.?.pl /usr/bin/execute.pl /usr/bin/llp.pl /usr/bin/cdd.pl /usr/bin/bootstrap.pl /usr/bin/scm.pl /usr/bin/sumo.pl
