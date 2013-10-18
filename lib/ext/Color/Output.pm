package ext::Color::Output;

use strict;
use Carp;
use warnings;

our $VERSION = 1.05;

BEGIN {
  use Exporter   ();
  our (@ISA, @EXPORT);

  @ISA = qw(Exporter);
  @EXPORT = qw(&cprint &cprintf &clear);
}


my ($_Mode, $_Console);
my ($_Symbol) = "\x03";

our ($Mode, $Symbol);


sub Init {
  if (defined $_Mode) {
    carp "You already ran Init!";
    return;
  }
  $_Symbol = $Symbol, if (defined $Symbol);
  $_Mode = 0;

  if (defined $Mode && $Mode =~ /^\d+$/) {
    if ($Mode == 1 || $Mode == 2) { $_Mode = $Mode; }
  }

  if ($^O =~ /Win32/) {
    $_Mode = 2, unless (defined $_Mode and $_Mode == 1);
    if ($_Mode == 1) {
      *clear = sub { system("cls"); };
    }
    elsif ($_Mode == 2) {
      *clear = sub {
        $_Console->Cls();
        $_Console->Display();
      };
    }
  }
  else {
    $_Mode = 1, unless (defined $_Mode and $_Mode == 2);
    *clear = sub {
       system("clear");
    };
  }

  if ($_Mode == 1)    { &Init_ANSI();    }
  elsif ($_Mode == 2) { &Init_Console(); }

}


################################################################################
# ANSI-PART                                                                    #
################################################################################
sub Init_ANSI {
  my @List = qw(0m 30m 34m 34;1m 31m 31;1m 32m 32;1m 35m 35;1m 36m 36;1m 33m 33;1m 37m 37;1m);

  *cprint = sub {
    my ($String);

    if (defined $_[0] and ref $_[0]) {
      shift, unless (ref $_[0] eq 'ARRAY' || ref $_[0] eq 'HASH' || ref $_[0] eq 'SCALAR' || ref $_[0] eq 'CODE' || ref $_[0] eq 'GLOB');
    }

    if ($#_ >= 0) { $String = join ("", @_); }
    else { $String = $_; }

    $String =~ s/$_Symbol(\s|\D|$)/${_Symbol}0$1/g;
    $String =~ s/$_Symbol(\d\d?)/\033[$List[$1]/g;
    print $String;
  };

  *cprintf = sub {
    my ($String);
    if ($#_ >= 0) { $String = shift; $String = sprintf ($String, @_); }
    else { $String = $_; }

    $String =~ s/$_Symbol(\s|\D|$)/${_Symbol}0$1/g;
    $String =~ s/$_Symbol(\d\d?)/\033[$List[$1]/g;
    printf $String;
  };
}


END {
  # Reset text-color.

  if (defined $_Mode && $_Mode == 1) {
    printf "\033[0m";
  }
}

################################################################################
# W32::Console-PART                                                            #
################################################################################
sub Init_Console {
  eval {
    require Win32::Console;
  } or croak $@;

  import Win32::Console qw(STD_OUTPUT_HANDLE);
  $_Console = new Win32::Console(&STD_OUTPUT_HANDLE());
#  $_Console = new Win32::Console();
  my @List = qw(7 0 1 9 4 12 2 10 5 13 3 11 6 14 7 15);

  *cprint = sub {
    my ($String);

    if (defined $_[0] and ref $_[0]) {
      shift, unless (ref $_[0] eq 'ARRAY' || ref $_[0] eq 'HASH' || ref $_[0] eq 'SCALAR' || ref $_[0] eq 'CODE' || ref $_[0] eq 'GLOB');
    }

    if ($#_ >= 0) { $String = join ("", @_); }
    else { $String = $_; }

    ccprint($String, 1);
  };

  *cprintf = sub {
    my ($String);
    if ($#_ >= 0) { $String = shift; $String = sprintf ($String, @_); }
    else { $String = $_; }
    ccprint($String, 0);
  };

  *ccprint = sub {
    my ($String) = shift;
    my ($Option) = shift;

    $String =~ s/$_Symbol(\s|\D|$)/${_Symbol}0$1/g;
    my ($First, $Last) = $String =~ /^(.*?)(?:$_Symbol|$)/s;
    $_Console->Write($First), if (defined $First);
    $_Console->Write("\n"), if (defined $First and defined $Last);

    while ($String =~ /$_Symbol(\d\d?)([^$_Symbol]*)/g) {
      $_Console->Attr($List[$1]);
      $_Console->Write($2);
    }

    if ($Option && defined $\) { $_Console->Write($\ . ""); }
    $_Console->Display();
  };
}



1;
__END__

=head1 NAME

Color::Output - Module to give color to the output

=head1 DESCRIPTION

With this module you can color the output. It will color the output on both Windows and on Unix/Linux.
On Windows it uses by default Win32::Console (unless overwritten with some options),
on Unix/Linux it uses ANSI by default.

This module allows you to do:

=over

=item *

Color the output of your program

=item *

Clearing the screen

=back

=head1 Methods

=over

=item Init

Initialise this module, this should be done before doing anything else with this module.

By default it will use ANSI-Colors on Linux/Unix and Win32::Console on windows.

However, you can change this behaviour by changing the value of several variables, more about that in the Vars-Section

=item cprint [Text]

Display the text on the screen. The char to identify a color is: \x03 or \003 or chr(3) (by default)

All params are joined into 1 string.

Examples:

   cprint ("\x033Blue text\x030\n");
   cprint ("\0035Red text\n");
   cprint ("The text is still red, ", chr(3) ,"7and now it is green.\x030\n");

Note:

   The text-color is set to the default one when the program ends.

   If the first param is an objet then this param is ignored (not being outputed).

=item cprintf [Text]

Display the text on the screen. The char to identify a color is: \x03 or \003 or chr(3) (by default)

Examples:

   cprintf ("\x033This is a %s string\x030\n", "blue");
   cprintf ("\0035%s text\n", "red");
   cprintf ("The text is still %s, ". chr(3) ."7and now it is %s.\x030\n", "red", "green");


Note1:

   If you call cprintf, then it runs sprintf and that result is printed with cprint.

   However, I'm not sure wheter or not this is very safe.. so you might want to pay attention when you use it.


=item clear

Clears the screen.

Example:

   clear();

=back

=head1 Variables

=over

=item Mode ($Color::Output::Mode)

With this variable you can change the way this module behaves.

By default ANSI-Colors will be used on Linux/Unix and Win32::Console on Win32.

If you change this var to B<1> then B<ANSI-Colors> will be used,

If you set it to B<2> then B<Win32::Console> will be used.

Examples: examples/ANSI.pl and examples/W32.pl

=item Symbol ($Color::Output::Symbol)

This is the symbol you can use to color your text, by default chr(3) will be used.

Example: examples/symbol.pl

=back


=head1 Demo

  use Color::Output;
  Color::Output::Init;
  for (my($i)=0;$i<16;$i++) {
    cprint("Color=$i". (" " x (15 - length($i))) ."\x03" . $i . "Example $0, color $i\x030\n");
  }

  This code is also added as an example (examples/colors.pl)


=head1 NOTES

=over

=item *

There is a module called Win32::Console::ANSI, but when I tested it had some bugs..
Therefor I decided to rewrite this module and make it public.

=back

=head1 SEE ALSO

L<Term::ANSIColor>, L<Win32::Console>, L<Win32::Console::ANSI>

=head1 BUGS

=over

=item *

While testing Win32::Console i noticed that it sometimes did not display the color or the correct color..
This is a Win32::Console-issue/bug.

=back

=head1 AUTHOR

Animator <Animator@CQ-Empires.com>

=head1 COPYRIGHT

Copyright (c) 2003 Animator. All rights reserved.

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.
