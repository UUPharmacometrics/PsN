# Like the Debug class the user_interface (ui) class is a little bit
# special in that it should never be instanciated. An instance (named
# ui) is kept globally which can be accessed by the members of the
# ui class if they are called statically. Calling a member staticaly
# means that you adress them using the perl module name, for example:
#
# ui -> print( 'print this on the screen' );
#
# Notice that there is no $ in front of 'ui'. Here print is called
# "statically". In other words, it means "call a member without an
# instance".
#

# {{{ include
start include statements
use Text::Wrap;

my $the_instance = ui::new();
end include statements
# }}}

# {{{ new
start new
end new
# }}}

# {{{ category

start category
{
	# Usage:
	#
	# ui -> category( 'bootstrap' )
	#
	# If you give a value to category as a single argument, you will
	# set the global category of user interface messages. 
	#
	# my $current_category = ui -> category
	#
	# If you don't give any argument the current category is returned.

	if ( defined($parm) ) {
		$the_instance -> {'category'} = $parm;
	} else {
		return $the_instance -> {'category'};
	}
	return;
}
end category

# }}}

# {{{ package

start package
{
	# Usage:
	#
	# ui -> package( 'output' )
	#
	# If you give a value to package as a single argument, you will
	# set the global package of ui messages. 
	#
	# my $current_package = ui -> package
	#
	# If you don't give any argument the current package is returned.

	if ( defined($parm) ) {
		$the_instance -> {'package'} = $parm;
	} else {
		return $the_instance -> {'package'};
	}
	return;
}
end package

# }}}

# {{{ subroutine

start subroutine
{
	# Usage:
	#
	# ui -> subroutine( 'output' )
	#
	# If you give a value to subroutine as a single argument, you will
	# set the global subroutine of ui messages. 
	#
	# my $current_subroutine = ui -> subroutine
	#
	# If you don't give any argument the current subroutine is returned.

	if ( defined($parm) ) {
		$the_instance -> {'subroutine'} = $parm;
	} else {
		return $the_instance -> {'subroutine'};
	}
	return;
}
end subroutine

# }}}

# {{{ print
start print
{
	# Usage:
	#
	# ui -> print( category => 'bootstrap',
	#              message  => 'This is a message' );
	#
	# ui::print will print out messages corresponding to the category specified as
	# argument. ui::print will look at the category given and the
	# global category to see whether anything should be printed. 
	#
	#
	# NOTICE the lack of "\n" at the end of the message, ui::print
	# will append one "\n". In case there ever is a GUI for PsN
	# ui::print could be used to create a message in the GUI. And
	# in that case, an extra "\n" might be annoying.

	if( $the_instance -> {'category'} eq $category or $category eq 'all' ) {
		my ( $package_junk, $filename, $line, $subroutine, $junk ) = caller(1);
		my @names = split( '::', $subroutine );
		$subroutine = $names[$#names];
		my $package = join( '::', @names[0..$#names-1] );

		if ( ( not defined $the_instance -> {'package'} or
					$the_instance -> {'package'} eq $package ) and
				( not defined $the_instance -> {'subroutine'} or
					$the_instance -> {'subroutine'} eq $subroutine ) ) {
	  
			my $prefix;
			my $text;

			my $nl = ($newline) ? "\n" : ''; 

			if ( $wrap ) {
				$text = wrap('', '', $message . $nl);
			} else {
				$text = $message . $nl;
			}

			if( $the_instance -> {'silent'} ) {
				if (defined $the_instance->{'logfile'}) {
					open( LOG, '>>'.$the_instance->{'logfile'});
					print LOG ( $text );
					close LOG;
				}
			} else {
				print STDERR ( $text );
			}
		}
	}
}
end print
# }}}

# {{{ status_bar
start status_bar
{
	if ( $goal != 0 ) {
		my $part = int(($sofar / $goal) * $width);
		$bar = "\r" . '|' . '.' x $part . ' ' x ($width - $part) . '|';
	}
}
end status_bar
# }}}

# {{{ silent
start silent
{
	if ( defined($parm) ) {
		$the_instance -> {'silent'} = $parm;
	} else {
		return $the_instance -> {'silent'};
	}
	return;
}
end silent
# }}}

# {{{ logfile
start logfile
{
	if ( defined($parm) ) {
		$the_instance -> {'logfile'} = $parm;
	} else {
		return $the_instance -> {'logfile'};
	}
	return;
}
end logfile
# }}}
