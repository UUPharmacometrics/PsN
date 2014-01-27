package ui;

use strict;
use Text::Wrap;
use MooseX::Params::Validate;

# Class attributes
our $category;
our $package;
our $subroutine;
our $logfile;
our $silent;

sub category
{
	my $self = shift;
	my $parm = shift;

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

	if (defined($parm)) {
		$ui::category = $parm;
	} else {
		return $ui::category;
	}
}

sub package
{
	my $self = shift;
	my $parm = shift;

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
		$ui::package = $parm;
	} else {
		return $ui::package;
	}
}

sub subroutine
{
	my $self = shift;
	my $parm = shift;

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
		$ui::subroutine = $parm;
	} else {
		return $ui::subroutine;
	}
}

sub logfile
{
	my $self = shift;
	my $parm = shift;

	if ( defined($parm) ) {
		$ui::logfile = $parm;
	} else {
		return $ui::logfile;
	}
}

sub silent
{
	my $self = shift;
	my $parm = shift;

	if ( defined($parm) ) {
		$ui::silent = $parm;
	} else {
		return $ui::silent;
	}
}

sub print
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 message => { isa => 'Str', default => 'Default warning message', optional => 1 },
		 category => { isa => 'Str', optional => 0 },
		 wrap => { isa => 'Bool', default => 1, optional => 1 },
		 newline => { isa => 'Bool', default => 1, optional => 1 }
	);
	my $message = $parm{'message'};
	my $category = $parm{'category'};
	my $wrap = $parm{'wrap'};
	my $newline = $parm{'newline'};

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

	if( $ui::category eq $category or $category eq 'all' ) {
		my ( $package_junk, $filename, $line, $subroutine, $junk ) = caller(1);
		my @names = split( '::', $subroutine );
		$subroutine = $names[$#names];
		my $package = join( '::', @names[0..$#names-1] );

		if ( ( not defined $ui::package or
					$ui::package eq $package ) and
				( not defined $ui::subroutine or
					$ui::subroutine eq $subroutine ) ) {
	  
			my $prefix;
			my $text;

			my $nl = ($newline) ? "\n" : ''; 

			if ( $wrap ) {
				$text = wrap('', '', $message . $nl);
			} else {
				$text = $message . $nl;
			}

			if( $ui::silent ) {
				if (defined $ui::logfile) {
					open( LOG, '>>' . $ui::logfile);
					print LOG ( $text );
					close LOG;
				}
			} else {
				print STDERR ( $text );
			}
		}
	}
}

sub status_bar
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 sofar => { isa => 'Num', optional => 1 },
		 goal => { isa => 'Num', optional => 1 },
		 width => { isa => 'Int', default => 50, optional => 1 }
	);
	my $sofar = $parm{'sofar'};
	my $goal = $parm{'goal'};
	my $width = $parm{'width'};
	my $bar;

	if ( $goal != 0 ) {
		my $part = int(($sofar / $goal) * $width);
		$bar = "\r" . '|' . '.' x $part . ' ' x ($width - $part) . '|';
	}

	return $bar;
}

1;
