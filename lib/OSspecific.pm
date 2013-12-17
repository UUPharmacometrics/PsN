package OSspecific;
use Cwd;
use Config;
use strict;
#use Carp;
use include_modules;
use File::Spec;

## Change the appropriate "system" command below to reflect your
## NONMEM installation.
my $ver5_unix = '/export/home/nmv/util/nmfe5';
my $ver55_unix = '/export/home/nmv/util/nmfe55 -d -f -O';
my $ver6_unix = '/export/home/nmvi/util/nmfe6';
my $ver6_unix_spec = '/export/home/nmvi/util/nmfe -d -f -O';
my $ver5_win32 = 'nmfe5';
my $ver6_win32 = 'nmfe6';

sub unique_path {
    my $path = shift;
    my $start = shift;
    $start = defined $start ? $start : '.';
    unless( defined $path ){
	$path = "autopath";
    }
    my $i = 1;

    opendir DIRHANDLE , $start or die "Unable to list directory $start\n";
    my @dir_list = readdir(DIRHANDLE);
    closedir DIRHANDLE;
    my $max = 0;
    foreach my $dir_or_file ( @dir_list ){
      if( $dir_or_file =~ /^$path(\d+)/ ){
	my $ending = $1;
	$max = $ending if( $ending > $max );
	$i++;
      }
    }
    
    if( $i > $max ){
      $max = $i;
    } else {
      $max++;
    }

    # find( { wanted => sub{ /^$path/ && $i++ }}, $start );
    
    if( -e $start . '/' . $path . $max ){
	die "The directory ${start}${path}${max} is in the way.\n";
    }
    my ( $dir , $file ) = absolute_path($start . '/' . $path . $max, '');
    return $dir;
}

sub absolute_path {
  
  # path finding strategy:
  
  # 1. If path is not given it is assumed to be the current working
  # directory. If it is not absolute, it is assumed to be relative to
  # the current working directory.
  
  # 2. If filename is absolute, let that overide the directory

  # 3. If the filename is relative, assume the path as base.

  my $path = shift;
  my $file = shift;
  
  $file = File::Spec -> canonpath($file);
  
  if( defined $path ){
    $path = File::Spec -> canonpath($path);
    unless( File::Spec -> file_name_is_absolute( $path ) ){
      $path = File::Spec -> rel2abs($path);
    }
  } else {
    $path = getcwd;
  }

  my ($path_volume,$path_directory, $path_file) = File::Spec -> splitpath( $path, 1 );
  my ($file_volume,$file_directory, $file_file) = File::Spec -> splitpath( $file );

  my @path_directory = File::Spec -> splitdir( $path_directory );
  my @file_directory = File::Spec -> splitdir( $file_directory );
  my @directory;

  if( File::Spec -> file_name_is_absolute( $file ) ){
    $path_volume = $file_volume;
    @directory = @file_directory;
  } else {
    @directory = (@path_directory, @file_directory);
  }

  for( my $i = 0; $i < $#directory; $i++ ){
    if( $directory[$i] ne File::Spec -> updir() and $directory[$i+1] eq File::Spec -> updir() ){
      @directory = (@directory[0..$i-1], @directory[$i+2..$#directory]);
      $i = $i-2;
    }
  }

  $path = File::Spec -> catpath( $path_volume, File::Spec -> catfile( @directory,'' ), '' );


  #Append trailing slash if missing
  unless(( $path =~ /\/$/ ) || ( $path =~ /\\$/ )){ 
    $path .= '/';  
  }
  
  return ($path, $file_file);
}

sub NM_command {
  my $version = shift;
  my $prio = shift;
  my $modelfile = shift;
  my $outfile = shift;
  my $silent = shift;
  my $nm_opts = "";

  my $os = $Config{osname};
  my $command;
  my $shell = '';
  $os = 'linux' unless ( $os eq 'MSWin32' );
  
  if( $os eq 'linux' ){
      ## Sometimes it is necessary to specify the shell under which NONMEM
      ## executes. The shell needs to know where nmfe5 is. Example for csh:
      #my $shell ='/bin/csh';
      
      ## Change this line to specify the shell under which you want to 
      ## run NONMEM:
      
      # PP_TODO Should this be a config option?
      $shell ='';
  }

  if ( $version eq '5' and $os eq 'linux' ) {
      $command = $ver5_unix;
  } elsif ( $version eq '55' and $os eq 'linux' ) {
      $command = $ver55_unix;
  } elsif ( $version eq '6' and $os eq 'linux' ) {
      $command = $ver6_unix;
#      $nm_opts = '-d -f -O'
  } elsif ( $version eq '6_nmfe' and $os eq 'linux' ) {
      $command = $ver6_unix_spec;
#      $nm_opts = '-d -f -O'
  } elsif ( $version eq '5' and $os eq 'MSWin32' ) {
      $command = "$ver5_win32 $modelfile $outfile";
  } elsif ( $version eq '6' and $os eq 'MSWin32' ) {
      $command = "$ver6_win32 $modelfile $outfile";
  } else {
      carp("Could not determine platform. Assuming standard UNIX");
      ## You will have to specify the shell under which NONMEM
      ## executes. The shell needs to know where nmfe5 is.
      $shell = '/bin/csh -f';
      $command = $ver5_unix;
  }

  $silent = $silent ? ">$silent" : '';

  $command = "$shell nice -n $prio $command $nm_opts $modelfile $outfile $silent"
      if ( $os eq 'linux' );

  carp("$command");
  return $command;
}

sub directory {
  my $file = shift;
  my @tmp;
  my $directory;
  if ( $Config{osname} eq 'MSWin32' ) {
    if( $file =~ /\\/ ){
      @tmp = split(/\\/, $file );
      $directory = join( "\\", @tmp[0..$#tmp-1] )."\\";
    } else {
      $directory = '.';
    }
  } else { # Assume OS == unix
    if( $file =~ /\// ){
      @tmp = split(/\//, $file );
      $directory = join( '/', @tmp[0..$#tmp-1] ).'/';
    } else {
      $directory = '.';
    }
  }
  return $directory;
}

sub nopath {
  my $file = shift;
  my @tmp;
  my $nopath;
  if ( $Config{osname} eq 'MSWin32' ) {
    if( $file =~ /\\/ ){
      @tmp = split(/\\/, $file );
      $nopath = $tmp[$#tmp];
    } else {
      $nopath = $file;
    }
  } else { # Assume OS == unix
    if( $file =~ /\// ){
      @tmp = split(/\//, $file );
      $nopath = $tmp[$#tmp];
    } else {
      $nopath = $file;
    }
  }
  return $nopath;
}
  

sub slurp_file {
  my $file = shift;
  my @content = ();
  open ( FILE, $file );
  while ( <FILE> ) {
    if ( $Config{osname} eq 'MSWin32' ) {
      chomp;
      $_=$_."\r\n";
    } else {
      s/\r//;
    }
    push( @content, $_ );
  }
  close(FILE);
  return @content;
}
1;
