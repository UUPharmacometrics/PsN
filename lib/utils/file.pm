package utils::file;

# Package for handling files, filenames and paths in a platform independent way
# Note: Does not handle units on windows paths when path is relative current directory
#       i.e. C:dir\file
 
use strict;
use warnings;
use MooseX::Params::Validate;
use include_modules;

use Config;
use OSspecific;

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ('all' => [ qw(remove_path get_file_stem directory replace_extension slurp_file) ]);
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our $path_separator;
our $newline;

sub _set_windows
{
    $path_separator = "\\";
    $newline = "\r\x0A";
}

sub _set_unix
{
    $path_separator = '/';
    $newline = "\x0A";
}

our $path_separator;
if ($Config{osname} eq 'MSWin32') {
    _set_windows();
} else {
    _set_unix();
}

sub remove_path
{
    # Remove the path from a filename
    my $full_name = shift;

    my @tmp = split /\Q$path_separator\E/, $full_name;
    my $nopath = pop @tmp;

    return $nopath;
}

sub get_file_stem
{
    # Remove the path and extension from a filename
    my $name = shift;

    $name = remove_path($name); 
    $name =~ s/(.*)\..*/\1/;

    return $name;
}

sub replace_extension
{
    # Replace extension if present else add it
    my $filename = shift;
    my $extension = shift;

    if ($filename =~ /(.*)\..*/) {
        $filename = $1 . '.' . $extension;
    } else {
        $filename .= '.' . $extension;
    }

    return $filename;
}

sub directory
{
    # Extract the path from a filename
    my $file = shift;

    my @tmp;
    my $directory;
    if ($file =~ /\Q$path_separator\E/) {
        @tmp = split(/\Q$path_separator\E/, $file);
        $directory = join($path_separator, @tmp[0 .. $#tmp - 1]) . $path_separator;
    } else {
        $directory = ".$path_separator";
    }

    return $directory;
}

sub slurp_file
{
    # Read a file to an array. Remove all extra LFs and CRs but Keep the OS specific end of line
    my $file = shift;

    my @content = ();
    open(FILE, $file);
    while (<FILE>) {
        tr/\n\r//d;
        $_ = $_ . $newline;
        push(@content, $_);
    }
    close(FILE);

    return @content;
}

1;
