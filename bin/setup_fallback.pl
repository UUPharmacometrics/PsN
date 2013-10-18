use strict;
use Config;
use File::Spec;
use File::Path qw(mkpath);
use File::Copy::Recursive qw/fcopy rcopy dircopy fmove rmove dirmove/;

#modified install script to handle recursive copy problems on Windows
my $version = '3.5.10';

my $name_safe_version = $version;
$name_safe_version =~ s/\./_/g;
my @utilities = (
		 'bootstrap', 'cdd', 'execute', 'llp', 'scm', 'sumo', 'sse',
		 'data_stats', 
		 'single_valued_columns',
		 'create_cont_data', 'create_subsets',
		 'se_of_eta',
		 'update_inits',
		 'npc',
		 'vpc',
		 'pind','nonpb','extended_grid','psn','psn_options','psn_clean',
		 'runrecord','mcmp','lasso'
		 );

my @win_modules = ('Math::Random');
my @nix_modules = ('Math::Random','Storable');
my @recommended_modules = ('Statistics::Distributions');

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
   return( $default );
 } else {
   return( $input );
 }

}

$| = 1; # Make sure autoflush is on
$, = ',';
print "\nThis is the PsN installer. I will install version $version of PsN Core,
Toolkit and the following utilities: \n", @utilities, ".
You will be presented with a few questions. Just pressing ENTER means
that you accept the default values, except for YES and NO questions,
in which you must be explicit.\n\n";
$, ='';

if( $Config{osname} eq 'linux' ){
 my $user = `whoami`;
 chomp($user);
 unless( $user =~ /^root$/ ){
   print "Hi $user, you don't look like root. You need root privileges to install PsN systemwide. Would you like to try anyway [y/n] ?\n";
   quit('abort') unless( confirm() );
 }
}else{
  unless ($Config{osname} eq 'MSWin32'){
    print "OS ".$Config{osname}." is not explicitly supported by PsN, but if it is a unix type\n".
	"system everything should work anyway. Problems are expected only if it is a Windows system.\n".
	"Do you want to proceed with the installation [y/n] ?\n";
    quit('abort') unless( confirm() );
  }
}

my $binary_dir;
my $library_dir;
my $perl_binary;

print "PsN Utilities installation directory [$Config{bin}]:";

$binary_dir = get_input( $Config{bin} );

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

print "Path to perl binary used to run Utilities [$Config{perlpath}]:";

$perl_binary = get_input( $Config{perlpath} );

(my $volume,my $directory,my $file ) = File::Spec->splitpath( $perl_binary);
my $runperl_name="runperl.bat";
my $runperl_binary = File::Spec->catpath( $volume, $directory, $runperl_name);

#print "\n $runperl_binary \n";
if(( $Config{osname} eq 'MSWin32' ) and ($perl_binary ne 'C:\Perl\bin\perl.exe')){
  print "Warning: If you are running on Windows and the path to the perl binary \n".
      "set in the previous question was something other than ".'C:\Perl\bin\perl.exe'.
      "\n".
      "you must set the same path in psn.conf. Remove ; from the beginning of the ".
      "line\n".';perl = C:\Perl\bin\perl.exe'."\n".
      "in psn.conf and enter the new path on the right hand side of =\n\n";
}

print "PsN Core and Toolkit installation directory [$Config{sitelib}]:";

$library_dir = get_input( $Config{sitelib} );

unless( -e $library_dir ){
 print "Directory $library_dir does not exist. Would you like to create it? [y/n]\n";
 if( confirm() ){
   unless( mkpath( $library_dir ) ){ print "Unable to create $library_dir.\n"; quit('abort') };
 } else {
   quit('abort');
 }
}

if( $Config{osname} eq 'MSWin32' ){
 @modules = @win_modules;
} else {
 @modules = @nix_modules;
}


print "\n The next step is to check Perl module dependencies.
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




if( $Config{osname} eq 'MSWin32' ){
  if ($keep_conf){
    unless (fcopy($library_dir."\\PsN_$name_safe_version\\psn.conf","old.conf")){
      print "Could not copy $library_dir\\PsN_$name_safe_version\\psn.conf to old.conf : $!\n";
      quit('abort');
    }
  }
  unless (dircopy("lib", $library_dir."\\PsN_$name_safe_version")){
    print " Could not copy contents of lib directory to $library_dir\\PsN_$name_safe_version\\ : $!\n";
    quit('abort');
  }
  if ($keep_conf){
    unless (fcopy("old.conf",$library_dir."\\PsN_$name_safe_version\\psn.conf")){
      print "Could not copy old.conf to $library_dir\\PsN_$name_safe_version\\psn.conf to old.conf : $!\n";
      quit('abort');
    }
  }

}else{
  if ($keep_conf){
    unless (fcopy($library_dir."/PsN_$name_safe_version/psn.conf","old.conf")){
      print "Could not copy $library_dir/PsN_$name_safe_version/psn.conf to old.conf : $!\n";
      quit('abort');
    }
  }
  unless (dircopy("lib", $library_dir."/PsN_$name_safe_version")){
    print " Could not copy contents of lib directory to $library_dir/PsN_$name_safe_version/ : $!\n";
    quit('abort');
  }
  if ($keep_conf){
    unless (fcopy("old.conf",$library_dir."/PsN_$name_safe_version/psn.conf")){
      print "Could not copy old.conf to $library_dir/PsN_$name_safe_version/psn.conf : $!\n";
      quit('abort');
    }
  }
}



my $confirmed = 0;
foreach my $file ( @utilities ){

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
	 if( $Config{osname} eq 'MSWin32' ){
	   unless (fcopy("$binary_dir\\$file-$version","$binary_dir\\$file")){
	     print "Could not copy $binary_dir\\$file-$version to  $binary_dir\\$file : $!\n";
	     quit('abort');
	   }
	   unless (fcopy("$runperl_binary","$binary_dir\\$file.bat")){
	     print "Could not copy $runperl_binary to $binary_dir\\$file.bat : $!\n";
	     quit('abort');
	   }
	 } else {
	     symlink( "$binary_dir/$file-$version", "$binary_dir/$file" );
	 }
     }
     
 } else {
     
     if( $Config{osname} eq 'MSWin32' ){
       unless (fcopy("$binary_dir\\$file-$version","$binary_dir\\$file")){
	 print "Could not copy $binary_dir\\$file-$version to  $binary_dir\\$file : $!\n";
	 quit('abort');
       }
       unless (fcopy("$runperl_binary","$binary_dir\\$file.bat")){
	 print "Could not copy $runperl_binary to $binary_dir\\$file.bat : $!\n";
	 quit('abort');
       }
     } else {
	 symlink( "$binary_dir/$file-$version", "$binary_dir/$file" );
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


if( -e "$library_dir/PsN.pm" ){
 if( $overwrite ){
   unlink( "$library_dir/PsN.pm" );
   if( $Config{osname} eq 'MSWin32' ){
     unless (fcopy("$library_dir\\PsN_$name_safe_version.pm","$library_dir\\PsN.pm")){
       print "Could not copy $library_dir\\PsN_$name_safe_version.pm to $library_dir\\PsN.pm : $!\n";
       quit('abort');
     }
   } else {
     symlink( "$library_dir/PsN_$name_safe_version.pm", "$library_dir/PsN.pm" );
   }
 }
} else {
 if( $Config{osname} eq 'MSWin32' ){
   unless (fcopy("$library_dir\\PsN_$name_safe_version.pm","$library_dir\\PsN.pm")){
     print "Could not copy $library_dir\\PsN_$name_safe_version.pm to $library_dir\\PsN.pm : $!\n";
     quit('abort');
   }
 } else {
   symlink( "$library_dir/PsN_$name_safe_version.pm", "$library_dir/PsN.pm" );
 }
}

unless (open( PSN, '<', "$library_dir" . "/PsN_$name_safe_version/nonmem.pm" )){
  print "Unable to install PsN_$name_safe_version/nonmem.pm in $library_dir: $!\n";
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
if( $Config{osname} eq 'MSWin32' ){
   $dir_sep = "\\";  
}
print "\nInstallation complete.\n";
print "\nNow you should edit psn.conf so that PsN can find your NONMEM installations.\n";
print "A template psn.conf is found in\n";
print "$library_dir"."$dir_sep"."PsN_$name_safe_version\n"; 
print "Detailed instructions are found in ";
print "$library_dir$dir_sep"."PsN_$name_safe_version$dir_sep"."doc$dir_sep"."psn_configuration.pdf"; 
quit('done');

# Uninstall ?
# rm -rf /usr/lib/perl5/site_perl/5.8.4/PsN* /usr/bin/*-2.1.?.pl /usr/bin/execute.pl /usr/bin/llp.pl /usr/bin/cdd.pl /usr/bin/bootstrap.pl /usr/bin/scm.pl /usr/bin/sumo.pl
