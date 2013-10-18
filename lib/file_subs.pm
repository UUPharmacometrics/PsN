start include statements
use Carp;
use Config;
use Cwd;
end include statements

# {{{ new
start new
	my $path = $this -> {'path'};
	my $name = $this -> {'name'};

	$path = $this -> merge_path_and_name( path => $path, name => $name );

	$path =~ s!([^\/]*)$!!;
	$name = $1;

	$this -> {'path'} = $path;
	$this -> {'name'} = $name;
end new
# }}} new

# {{{ is_absolute
start is_absolute
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
end is_absolute
# }}} is_absolute

# {{{ clean_path
start clean_path
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
end clean_path
# }}}

# {{{ merge_path_and_name
start merge_path_and_name
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
		carp("file : No path given, assuming $path");
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
		unless( $path eq $tmp ) {
			carp("file : Path($path) differs from filename($name), using: $tmp");
		}
		$path = $name;
	} else {
		$path .= $name;
	}

	# Step 3
	$merged_path = $self -> clean_path( path => $path );
end merge_path_and_name
# }}}

# {{{ name
start name
	if ( defined $parm ) {
		my $path = $self -> merge_path_and_name( path => $self -> {'path'}, name => $parm );

		$path =~ s!([^\/]*)$!!;
		my $name = $1;
	
		$self -> {'path'} = $path;
		$parm = $name;
	}
end name
# }}} name

# {{{ path
start path
	if ( defined $parm ) {
		my $path = $self -> merge_path_and_name( path => $parm, name => $self -> {'name'} );

		$path =~ s!([^\/]*)$!!;
		my $name = $1;
	
		$self -> {'name'} = $name;
		$parm = $path;
	}
end path
# }}} path

# {{{ full_name
start full_name
	$full_name = $self -> {'path'} . $self -> {'name'};
end full_name
# }}}
