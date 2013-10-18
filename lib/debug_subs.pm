# The Debug class is a little bit special in that it should never be
# instanciated. An instance is kept globaly which can be accessed by
# the members of the debug class if they are called staticaly. Calling
# a member staticaly means that you adress them using the perl module
# name, for example:
#
# debug -> level( );
#
# Notice that there is no $ in front of 'debug'. Here level is called
# "staticaly". In other words, it means "call a member without an
# instance".
#
# The reason for this is that debug keeps a "level" variable globaly,
# which indicates how verbose PsN should be, the higher the level, the
# more messages you will see.. The level variable is numerical, but
# each level has a name in order to make its use a bit more
# intuitive. The levels are, starting with the lowest:
#
# "fatal" - only when an error so grave that PsN has to exit, a fatal
#           message may be printed. This is the least amount of messages 
#           you can see.
#
#
# "warning" - When something critical happens, somethings that
#             probably should be examined closer, though not
#             serious enough to exit PsN a warning message may be
#             printed.
#
# "information" - When something out of the ordinary happens and
#                 we think the user should be informed, an 
#                 informational message may be printed.
# 
# "call_trace" - This level is mostly used by developers when
#                debugging PsN. If this level is set a message 
#                will be printed for each method which is called 
#                inside PsN. Needless to say, this will print a
#                lot of text. The only time a user should turn this
#                is when filing a bug report, and only then if a
#                developer thinks it is necessary.
#
# Setting a higher level than "fatal" also means that message of all
# lower levels will be printed.
#
# No PsN class may change the level.

# {{{ include
start include statements
use Carp;
use Carp qw(cluck);
use Text::Wrap;

# These variables are used for mapping level names to numbers.

my $fatal = 0;
my $warning = 1;
my $information = 2;
my $call_trace = 3;

my $the_instance = debug::new();

end include statements
# }}}

# {{{ new
start new
end new
# }}}

# {{{ level

start level
    {
      # Usage:
      #
      # debug -> level( debug::warning )
      #
      # If you give a value to level as a single argument, you will
      # set the global level of debug messages. 
      #
      # my $current_level = debug -> level
      #
      # If you don't give any argument the current level is returned.

      if( defined($parm) ){
	$the_instance -> {'level'} = $parm;
      } else {
	return $the_instance -> {'level'};
      }
      return;
    }
end level

# }}}

# {{{ logfile

start logfile
    {
      if( defined($parm) ){
	$the_instance -> {'logfile'} = $parm;
      } else {
	return $the_instance -> {'logfile'};
      }
      return;
    }
end logfile

# }}}


# {{{ package

start package
    {
      # Usage:
      #
      # debug -> package( 'output' )
      #
      # If you give a value to package as a single argument, you will
      # set the global package of debug messages. 
      #
      # my $current_package = debug -> package
      #
      # If you don't give any argument the current package is returned.

      if( defined($parm) ){
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
      # debug -> subroutine( 'output' )
      #
      # If you give a value to subroutine as a single argument, you will
      # set the global subroutine of debug messages. 
      #
      # my $current_subroutine = debug -> subroutine
      #
      # If you don't give any argument the current subroutine is returned.

      if( defined($parm) ){
	$the_instance -> {'subroutine'} = $parm;
      } else {
	return $the_instance -> {'subroutine'};
      }
      return;
    }
end subroutine

# }}}

# {{{ level_name
start level_name
    {
      # Usage:
      #
      # my $level_name = debug -> level_name( level => 1 )
      
      # level_name will map an integer in the interval 0 to 3 to a
      # string. The string is the name of the level with that number
      # in the order of levels. ( "fatal" is lowest and "call_trace"
      # highest ).

      # By default it returns the name of the current set level.

      # my $current_level_name = debug -> level_name;

      $return_val = $the_instance -> {'level_names'} -> [$level];
    }
end level_name
# }}}

# {{{ warn_with_trace
start warn_with_trace
    {
      # Usage:
      #
      # debug -> warn_with_trace( debug::warning )
      #
      # By default, a trace of function calls is printed when PsN
      # dies. If you like you can set a level for which you like
      # traces to be printed. Notice that all lower level messages
      # will also have a trace printed after them.

      if( defined($parm) ){
	$the_instance -> {'warn_with_trace'} = $parm;
      } else {
	return $the_instance -> {'warn_with_trace'};
      }
      return;
    }
end warn_with_trace
# }}}

# {{{ warn
start warn
    {

      # Usage:
      #
      # debug -> warn( level => debug::warning, message => "This is a warning" );
      #
      # debug::warn will print out warning, informational or
      # call_trace messages corresponding to the level specified as
      # argument. debug::warn will look at the level given and the
      # global level to see whether anything should be printed. 
      #
      #
      # NOTICE the lack of "\n" at the end of the message, debug::warn
      # will append one "\n". In case there ever is a GUI for PsN
      # debug::warn could be used to create a message in the GUI. And
      # in that case, an extra "\n" might be annoying.

      if( $the_instance -> {'level'} >= $level ){

	my ( $package_junk, $filename, $line, $subroutine, $junk ) = caller(1);
	my @names = split('::', $subroutine );
	$subroutine = $names[$#names];
	my $package = join( '::', @names[0..$#names-1] );

	if ( ( not defined $the_instance -> {'package'} or
	       $the_instance -> {'package'} eq $package ) and
	     ( not defined $the_instance -> {'subroutine'} or
	       $the_instance -> {'subroutine'} eq $subroutine ) ) {
	  
	  my $level_name = $self -> level_name(level => $level);
	  my $prefix;
	  my $text;
	  
	  if( $the_instance -> {'level'} >= 3 ){
	    my @longmess = split( /\n/, Carp::longmess );
	    my $arr = scalar(@longmess);
	    $prefix = '  ' x ($arr-2);
	  } 
	  
	  if( $level >= 3 ){
	    $text = $prefix . $level_name . ': ' . $package . '->' .
	      $subroutine. " : " . $message . "\n";
	  } else {
	    my $level_name_length = length( $level_name );
	    my $indent = ' ' x ($level_name_length + 4);
	    $text = wrap($prefix, $prefix . $indent, $level_name . ': ' . $package . '->' .
			 $subroutine. " : " . $message . "\n" );
	  }
	
	  if (defined $the_instance -> {'logfile'}){
	    open( LOG, '>>'.$the_instance->{'logfile'});
	    print LOG ( $text );
	    close LOG;
	  }

	  if( $the_instance -> {'warn_with_trace'} >= $level ){
	    cluck( $text );
	  } else {
	    print STDERR ( $text );
	  }
	}
      }
    }
end warn
# }}}

# {{{ die
start die
    {
      # Usage
      #
      # debug -> die( message => "This message will print, and then PsN will die" );
      #
      #
      # debug::die is what PsN calls instead of "die" in order to get
      # a call trace. The given message is allways printed.
      #
      # NOTICE the lack of "\n" at the end of the message, debug::warn
      # will append one "\n". In case there ever is a GUI for PsN
      # debug::doe could be used to create a message in the GUI. And
      # in that case, an extra "\n" might be annoying.

      if( $the_instance -> {'level'} >= 0 ){
	my $prefix = "";
	if( $the_instance -> {'level'} >= 3 ){
	  my @longmess = split( /\n/, Carp::longmess );
	  my $arr = scalar(@longmess);
	  $prefix = '  ' x ($arr-2);
	}

	if (defined $the_instance -> {'logfile'}){
	  open( LOG, '>>'.$the_instance->{'logfile'});
	  print LOG ( $prefix . $self -> level_name(level => 0) . ': ' . $message . "\n");
	  print LOG Carp::shortmess;
	  print LOG "\n";
	  close LOG;
	}

	if( $the_instance -> {'level'} > 0 ){
	  $! = 1;
	  confess( $prefix . $message );
	} else {
	  $! = 1;
	  croak(  $prefix . $self -> level_name(level => 0) . ': ' . $message . "\n" );
	}
      }
    }
end die
# }}}
