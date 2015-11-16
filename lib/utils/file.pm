package utils::file;

# Package for handling files, filenames and paths in a platform independent way
# Note: Does not handle units on windows paths when path is relative current directory
#       i.e. C:dir\file
 
use strict;
use warnings;
use MooseX::Params::Validate;
use File::Spec;
use include_modules;

use Config;
use OSspecific;

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ('all' => [ qw(get_file_stem replace_extension slurp_file) ]);
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our $path_separator;

sub _set_windows
{
    $path_separator = "\\";
}

sub _set_unix
{
    $path_separator = '/';
}

if ($Config{osname} eq 'MSWin32') {
    _set_windows();
} else {
    _set_unix();
}

sub get_file_stem
{
    # Remove the path and extension from a filename
    my $name = shift;

    (undef, undef, $name) = File::Spec->splitpath($name); 
    $name =~ s/(.*)\..*/$1/;

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

sub slurp_file
{
    # Read a file to an array. Remove all extra LFs and CRs but Keep the OS specific end of line
    my $file = shift;

    my @content = ();
    open(FILE, $file);
    while (<FILE>) {
        tr/\r//d;   # Remove extra CRs. We can use Windows formatted files in Unix
        push(@content, $_);
    }
    close(FILE);

    return @content;
}

1;
