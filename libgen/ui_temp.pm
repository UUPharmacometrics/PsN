use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package ui;
use Text::Wrap;

my $the_instance = ui::new();
use debug;


sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'category' => 'SCALAR', 'package' => 'SCALAR',
			'subroutine' => 'SCALAR', 'logfile' => 'SCALAR',
			'silent' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in ui->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in ui->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in ui->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in ui->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in ui->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'silent'} = defined $parm{'silent'} ? $parm{'silent'} : 0 unless defined $this -> {'silent'};

	bless $this, $class;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 24 "lib/ui_subs.pm" 
# line 60 libgen/ui.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub category {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> category');
# line 30 "lib/ui_subs.pm" 
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
# line 93 libgen/ui.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> category');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'category'} = $parm;
	} else {
		return $self -> {'category'};
	}
}

sub package {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> package');
# line 56 "lib/ui_subs.pm" 
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
# line 130 libgen/ui.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> package');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'package'} = $parm;
	} else {
		return $self -> {'package'};
	}
}

sub subroutine {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> subroutine');
# line 82 "lib/ui_subs.pm" 
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
# line 167 libgen/ui.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> subroutine');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subroutine'} = $parm;
	} else {
		return $self -> {'subroutine'};
	}
}

sub logfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> logfile');
# line 186 "lib/ui_subs.pm" 
{
	if ( defined($parm) ) {
		$the_instance -> {'logfile'} = $parm;
	} else {
		return $the_instance -> {'logfile'};
	}
	return;
}
# line 193 libgen/ui.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> logfile');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'logfile'} = $parm;
	} else {
		return $self -> {'logfile'};
	}
}

sub silent {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> silent');
# line 173 "lib/ui_subs.pm" 
{
	if ( defined($parm) ) {
		$the_instance -> {'silent'} = $parm;
	} else {
		return $the_instance -> {'silent'};
	}
	return;
}
# line 219 libgen/ui.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> silent');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'silent'} = $parm;
	} else {
		return $self -> {'silent'};
	}
}

sub print {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'message' => 'SCALAR', 'category' => 'm_SCALAR',
			'wrap' => 'SCALAR', 'newline' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in ui->print: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in ui->print: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in ui->print: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in ui->print: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in ui->print: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $message = defined $parm{'message'} ? $parm{'message'} : 'Default warning message';
	my $category = $parm{'category'};
	my $wrap = defined $parm{'wrap'} ? $parm{'wrap'} : 1;
	my $newline = defined $parm{'newline'} ? $parm{'newline'} : 1;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> print');
# line 107 "lib/ui_subs.pm" 
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
# line 316 libgen/ui.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> print');
	# End of Non-Dia code #

}

sub status_bar {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'sofar' => 'SCALAR', 'goal' => 'SCALAR', 'width' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in ui->status_bar: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in ui->status_bar: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in ui->status_bar: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in ui->status_bar: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in ui->status_bar: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $sofar = $parm{'sofar'};
	my $goal = $parm{'goal'};
	my $width = defined $parm{'width'} ? $parm{'width'} : 50;
	my $bar;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> status_bar');
# line 162 "lib/ui_subs.pm" 
{
	if ( $goal != 0 ) {
		my $part = int(($sofar / $goal) * $width);
		$bar = "\r" . '|' . '.' x $part . ' ' x ($width - $part) . '|';
	}
}
# line 363 libgen/ui.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> status_bar');
	# End of Non-Dia code #

	return $bar;
}

1;

