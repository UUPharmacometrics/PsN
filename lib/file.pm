use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package file;
use Carp;
use Config;
use Cwd;
use debug;


sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'path' => 'SCALAR', 'name' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in file->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in file->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in file->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in file->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in file->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}


	bless $this, $class;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 9 "lib/file_subs.pm" 
	my $path = $this -> {'path'};
	my $name = $this -> {'name'};

	$path = $this -> merge_path_and_name( path => $path, name => $name );

	$path =~ s!([^\/]*)$!!;
	$name = $1;

	$this -> {'path'} = $path;
	$this -> {'name'} = $name;
# line 67 libgen/file.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub path {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> path');
# line 135 "lib/file_subs.pm" 
	if ( defined $parm ) {
		my $path = $self -> merge_path_and_name( path => $parm, name => $self -> {'name'} );

		$path =~ s!([^\/]*)$!!;
		my $name = $1;
	
		$self -> {'name'} = $name;
		$parm = $path;
	}
# line 90 libgen/file.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> path');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'path'} = $parm;
	} else {
		return $self -> {'path'};
	}
}

sub name {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> name');
# line 121 "lib/file_subs.pm" 
	if ( defined $parm ) {
		my $path = $self -> merge_path_and_name( path => $self -> {'path'}, name => $parm );

		$path =~ s!([^\/]*)$!!;
		my $name = $1;
	
		$self -> {'path'} = $path;
		$parm = $name;
	}
# line 117 libgen/file.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> name');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'name'} = $parm;
	} else {
		return $self -> {'name'};
	}
}

sub merge_path_and_name {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'path' => 'm_SCALAR', 'name' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in file->merge_path_and_name: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in file->merge_path_and_name: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in file->merge_path_and_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in file->merge_path_and_name: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in file->merge_path_and_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $path = $parm{'path'};
	my $name = $parm{'name'};
	my $merged_path;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> merge_path_and_name');
# line 66 "lib/file_subs.pm" 
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
# line 212 libgen/file.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> merge_path_and_name');
	# End of Non-Dia code #

	return $merged_path;
}

sub full_name {
	my $self = shift;
	my $full_name;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> full_name');
# line 149 "lib/file_subs.pm" 
	$full_name = $self -> {'path'} . $self -> {'name'};
# line 227 libgen/file.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> full_name');
	# End of Non-Dia code #

	return $full_name;
}

sub is_absolute {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'name' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in file->is_absolute: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in file->is_absolute: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in file->is_absolute: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in file->is_absolute: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in file->is_absolute: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $name = $parm{'name'};
	my $is_absolute;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> is_absolute');
# line 24 "lib/file_subs.pm" 
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
# line 280 libgen/file.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> is_absolute');
	# End of Non-Dia code #

	return $is_absolute;
}

sub clean_path {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'path' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in file->clean_path: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in file->clean_path: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in file->clean_path: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in file->clean_path: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in file->clean_path: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $path = $parm{'path'};
	my $clean_path;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> clean_path');
# line 42 "lib/file_subs.pm" 
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
# line 339 libgen/file.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> clean_path');
	# End of Non-Dia code #

	return $clean_path;
}

1;

