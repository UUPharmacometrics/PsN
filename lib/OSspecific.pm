package OSspecific;

use Cwd;
use Config;
use strict;
use include_modules;
use File::Spec;

sub unique_path
{
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

    if( -e $start . '/' . $path . $max ) {
	    die "The directory ${start}${path}${max} is in the way.\n";
    }
    my ( $dir , $file ) = absolute_path($start . '/' . $path . $max, '');
    return $dir;
}

sub absolute_path
{
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
	  if ( $Config{osname} eq 'MSWin32' ) {
		  $path .= "\\";
	  }else{
		  $path .= '/';
	  }  
  }
  
  return ($path, $file_file);
}

1;
