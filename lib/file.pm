package file;

use include_modules;
use Config;
use Cwd;
use Mouse;
use MouseX::Params::Validate;

has 'path' => ( is => 'rw', isa => 'Str' );
has 'name' => ( is => 'rw', isa => 'Str' );

sub BUILD
{
    my $self  = shift;

    my $path = $self->path;
    my $name = $self->name;

    $path = $self->merge_path_and_name(path => $path, name => $name);

    $path =~ s!([^\/]*)$!!;
    $name = $1;

    $self->path($path);
    $self->name($name);
}

sub merge_path_and_name
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         path => { isa => 'Str', optional => 0 },
         name => { isa => 'Str', optional => 0 }
    );
    my $path = $parm{'path'};
    my $name = $parm{'name'};
    my $merged_path;

    # path finding strategy:

    # 1. If path is not given it is assumed to be the current working
    # directory. If it is not absolute, it is assumed to be relative to
    # the current working directory.

    # 2. If filename is absolute and a path is given, compare them and
    # warn if they differ.

    # 3. If the filename is relative, merge it with the path.

    # Step 1. Make the pathname absolute.
    if ( defined $path ) {
        if ( $Config{osname} eq 'MSWin32' ) {
            unless ( $path =~ /^\w\:[\\\/]/ ) {
                $path = getcwd() . '/' . $path;
            }
        } else { # Assume os == unix
            unless( $path =~ /^\// ) {
                $path = getcwd() . '/' . $path;
            }
        }
    } else {
        $path = getcwd();
    }

    $path = $self -> clean_path( path => $path );

    $name = $self -> clean_path( path => $name );

    unless ( $path =~ /\/$/ ) { # append trailing slash
        $path .= '/';
    }

    my $tmp = $name;
    $tmp =~ s![^\/]*$!!;

    # Step 1
    if ( $self -> is_absolute( name => $name ) ) {
        $path = $name;
    } else {
        $path .= $name;
    }

    # Step 3
    $merged_path = $self->clean_path(path => $path);

    return $merged_path;
}

sub full_name
{
    my $self = shift;
    my $full_name;

    $full_name = $self->path . $self->name;

    return $full_name;
}

sub is_absolute
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         name => { isa => 'Str', optional => 0 }
    );
    my $name = $parm{'name'};
    my $is_absolute;

    if ( $Config{osname} eq 'MSWin32' ) {
        if ( $name =~ /^\w\:[\\\/]/ ) {
            $is_absolute = 1;
        } else {
            $is_absolute = 0;
        }
    } else { # Assume OS == unix
        if ( $name =~ /^\// ) {
            $is_absolute = 1;
        } else {
          $is_absolute = 0;
        }
    }

    return $is_absolute;
}

sub clean_path
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         path => { isa => 'Str', optional => 0 }
    );
    my $path = $parm{'path'};
    my $clean_path;

    $path =~ s!\\!\/!g; # Flip slashes

    while ( $path =~ /[^\/\.]+\/\.\.\// ) {
        $path =~ s![^\/\.]+\/\.\.\/!!g; # Remove relative dots
    }

    while ( $path =~ m!^\/\.\.\/! ) {
        $path =~ s!^\/\.\.\/!\/!;
    }

    while ( $path =~ m![^\.]\.\/! ) {
        $path =~ s!\.\/!\/!; # remove singel dots
    }

    while ( $path =~ m!\/\/! ) {
        $path =~ s!\/\/!\/!; # remove double slashes
    }

    $clean_path = $path;

    return $clean_path;
}

1;
