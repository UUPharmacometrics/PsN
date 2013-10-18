use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package debug;
use Carp;
use Carp qw(cluck);
use Text::Wrap;

# These variables are used for mapping level names to numbers.

my $fatal = 0;
my $warning = 1;
my $information = 2;
my $call_trace = 3;

my $the_instance = debug::new();

use debug;


sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'level' => 'SCALAR', 'warn_with_trace' => 'SCALAR',
			'fatal' => 'SCALAR', 'warning' => 'SCALAR',
			'information' => 'SCALAR', 'call_trace' => 'SCALAR',
			'package' => 'SCALAR', 'logfile' => 'SCALAR',
			'subroutine' => 'SCALAR', 'level_names' => 'ARRAY',
			'specific_classes' => 'HASH', 'specific_methods' => 'HASH' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in debug->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in debug->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in debug->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'level'} = defined $parm{'level'} ? $parm{'level'} : 0 unless defined $this -> {'level'};
	$this -> {'warn_with_trace'} = defined $parm{'warn_with_trace'} ? $parm{'warn_with_trace'} : 0 unless defined $this -> {'warn_with_trace'};
	$this -> {'fatal'} = defined $parm{'fatal'} ? $parm{'fatal'} : 0 unless defined $this -> {'fatal'};
	$this -> {'warning'} = defined $parm{'warning'} ? $parm{'warning'} : 1 unless defined $this -> {'warning'};
	$this -> {'information'} = defined $parm{'information'} ? $parm{'information'} : 2 unless defined $this -> {'information'};
	$this -> {'call_trace'} = defined $parm{'call_trace'} ? $parm{'call_trace'} : 3 unless defined $this -> {'call_trace'};
	$this -> {'level_names'} = defined $parm{'level_names'} ? $parm{'level_names'} : ['Fatal Error', 'Warning', 'Information','Call History'] unless defined $this -> {'level_names'};
	$this -> {'specific_classes'} = defined $parm{'specific_classes'} ? $parm{'specific_classes'} : {} unless defined $this -> {'specific_classes'};
	$this -> {'specific_methods'} = defined $parm{'specific_methods'} ? $parm{'specific_methods'} : {} unless defined $this -> {'specific_methods'};

	bless $this, $class;

	# Start of Non-Dia code #
# line 66 "lib/debug_subs.pm" 
# line 80 libgen/debug.pm 
	# End of Non-Dia code #

	return $this;
};

sub level {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
# line 72 "lib/debug_subs.pm" 
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
# line 111 libgen/debug.pm 
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'level'} = $parm;
	} else {
		return $self -> {'level'};
	}
}

sub warn_with_trace {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
# line 186 "lib/debug_subs.pm" 
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
# line 144 libgen/debug.pm 
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'warn_with_trace'} = $parm;
	} else {
		return $self -> {'warn_with_trace'};
	}
}

sub fatal {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'fatal'} = $parm;
	} else {
		return $self -> {'fatal'};
	}
}

sub warning {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'warning'} = $parm;
	} else {
		return $self -> {'warning'};
	}
}

sub information {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'information'} = $parm;
	} else {
		return $self -> {'information'};
	}
}

sub call_trace {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'call_trace'} = $parm;
	} else {
		return $self -> {'call_trace'};
	}
}

sub package {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
# line 114 "lib/debug_subs.pm" 
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
# line 235 libgen/debug.pm 
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'package'} = $parm;
	} else {
		return $self -> {'package'};
	}
}

sub logfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
# line 98 "lib/debug_subs.pm" 
    {
      if( defined($parm) ){
	$the_instance -> {'logfile'} = $parm;
      } else {
	return $the_instance -> {'logfile'};
      }
      return;
    }
# line 259 libgen/debug.pm 
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'logfile'} = $parm;
	} else {
		return $self -> {'logfile'};
	}
}

sub subroutine {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
# line 140 "lib/debug_subs.pm" 
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
# line 294 libgen/debug.pm 
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subroutine'} = $parm;
	} else {
		return $self -> {'subroutine'};
	}
}

sub warn {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR', 'level' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in debug->warn: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in debug->warn: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in debug->warn: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->warn: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->warn: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = defined $parm{'message'} ? $parm{'message'} : 'Default warning message';
	my $level = defined $parm{'level'} ? $parm{'level'} : 1;

	# Start of Non-Dia code #
# line 208 "lib/debug_subs.pm" 
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
# line 399 libgen/debug.pm 
	# End of Non-Dia code #

}

sub die {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in debug->die: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in debug->die: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in debug->die: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->die: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->die: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = defined $parm{'message'} ? $parm{'message'} : 'Default death message';

	# Start of Non-Dia code #
# line 276 "lib/debug_subs.pm" 
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
# line 474 libgen/debug.pm 
	# End of Non-Dia code #

}

sub debug_method {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'method_name' => 'SCALAR', 'debug' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in debug->debug_method: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in debug->debug_method: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in debug->debug_method: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->debug_method: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->debug_method: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $method_name = $parm{'method_name'};
	my $debug = defined $parm{'debug'} ? $parm{'debug'} : 1;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub debug_class {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'class_name' => 'SCALAR', 'debug' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in debug->debug_class: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in debug->debug_class: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in debug->debug_class: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->debug_class: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->debug_class: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $class_name = $parm{'class_name'};
	my $debug = defined $parm{'debug'} ? $parm{'debug'} : 1;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub level_name {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'level' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in debug->level_name: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in debug->level_name: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in debug->level_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->level_name: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in debug->level_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $level = defined $parm{'level'} ? $parm{'level'} : $self -> {'level'};
	my $return_val;

	# Start of Non-Dia code #
# line 165 "lib/debug_subs.pm" 
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
# line 597 libgen/debug.pm 
	# End of Non-Dia code #

	return $return_val;
}

1;

