
require 5;
package ext::File::HomeDir;   #Time-stamp: "2004-12-29 19:46:02 AST"
use strict;
use vars qw($HOME @EXPORT $VERSION @ISA %Cache);
$VERSION = '0.06';
use Carp ();
require Exporter;
@ISA = ('Exporter');
@EXPORT = ('home');
 # %~ doesn't need (and won't take) exporting, as it's a magic symbol name
 # that's always looked for in package 'main'.

# POD at end.

sub my_home () {
  return $HOME if defined $HOME;

  # try the obvious
  $HOME = $ENV{'HOME'} || $ENV{'LOGDIR'};
  return $HOME if $HOME;

  # Or try other ways...
  if($MacPerl::Version and $MacPerl::Version
    # avoid the "used only once" warning.
    and defined do {
      local $SIG{"__DIE__"} = "";
      eval
       'use Mac::Files; $HOME = FindFolder(kOnSystemDisk, kDesktopFolderType)'
    }
  ) {
    return $HOME;
  }

  if(defined do {
    # see if there's a W32 registry on this machine, and if so, look in it
    local $SIG{"__DIE__"} = "";
    eval '
      use Win32::TieRegistry;
      my $folders = Win32::TieRegistry->new(
         "HKEY_CURRENT_USER/Software/Microsoft/Windows/CurrentVersion/Explorer/Shell Folders",
         { Delimiter => "/" }
      );
      $HOME = $folders->GetValue("Desktop");
    ' }
  ) {
    return $HOME;
  }

  # Getting desperate now...
  if( defined eval
    {
      local $SIG{'__DIE__'} = '';
      $HOME = (getpwuid($<))[7];
    }
  ) {
    return $HOME;
  }

  # MSWindows sets WINDIR, MS WinNT sets USERPROFILE.

  if($ENV{'USERPROFILE'}) {   # helpfully suggested by crysflame
    if(-e "$ENV{'USERPROFILE'}\\Desktop") {
      $HOME = "$ENV{'USERPROFILE'}\\Desktop";
      return $HOME;
    }
  } elsif($ENV{'WINDIR'}) {
    if(-e "$ENV{'WINDIR'}\\Desktop") {
      $HOME = "$ENV{'WINDIR'}\\Desktop";
      return $HOME;
    }
  }

  # try even harder
  if( -e 'C:/windows/desktop' ) {
    $HOME = 'C:/windows/desktop';
    return $HOME;
  } elsif( -e 'C:/win95/desktop' ) {
    $HOME = 'C:/win95/desktop';
    return $HOME;
  }

  # Ah well, if all else fails, fail...
  die "Can't find ~/";
}


sub home (;$) {
  if(@_ == 0) {
    $HOME || my_home();
  } elsif(!defined $_[0]) {
    Carp::croak "Can't use undef as a username";
  } elsif(!length $_[0]) {
    Carp::croak "Can't use empty-string (\"\") as a username";
  } elsif($_[0] eq '.') {
    $HOME || my_home();
  } else {
    exists( $Cache{$_[0]} )  # Memoization
     ? $Cache{$_[0]}
     : do {
       local $SIG{'__DIE__'} = '';
       $Cache{$_[0]} = eval { (getpwnam($_[0]))[7] };
        # ...so that on machines where getpwnam causes
        # a fatal error when called at all, we avoid outright
        # dying, and just return the undef that we'll get from
        # a failed eval block.
     }
  }
}

# Okay, things below this point are all scary.
#--------------------------------------------------------------------------
{
  # Make the class for the %~ tied hash:

  package File::HomeDir::TIE;
  use vars qw($o);

  # Make the singleton object:
  $o = bless {}; # We don't use the hash for anything, tho.

  sub TIEHASH { $o }
  sub FETCH   {
    #print "Fetching $_[1]\n";
    if(!defined($_[1]))   { &File::HomeDir::home()      }
    elsif(!length($_[1])) { &File::HomeDir::home()      }
    else                  {
      my $x = &File::HomeDir::home($_[1]);
      Carp::croak "No home dir found for user \"$_[1]\"" unless defined $x;
      $x;
    }
  }

  foreach my $m (qw(STORE EXISTS DELETE CLEAR FIRSTKEY NEXTKEY)) {
    no strict 'refs';
    *{$m} = sub { Carp::croak "You can't try ${m}ing with the %~ hash" }
  }

  # For a more generic approach to this sort of thing, see Dominus's
  # class "Interpolation" in CPAN.
}

#--------------------------------------------------------------------------
tie %~, 'File::HomeDir::TIE';  # Finally.
1;

__END__

=head1 NAME

File::HomeDir -- get home directory for self or other users

=head1 SYNOPSIS

  use File::HomeDir;
  print "My dir is ", home(), " and root's is ", home('root'), "\n";
  print "My dir is $~{''} and root's is $~{root}\n";
   # These both print the same thing, something like:
   #  "My dir is /home/user/mojo and root's is /"

=head1 DESCRIPTION

This module provides a function, C<home>, and also ties the
in-all-packages variable C<%~>.

=over

=item home()

Returns a filespec to this user's home directory.

=item home($user)

Returns a filespec to the home directory of the given user, or undef
if no such user.

Note that the argument to this must be a defined value, and mustn't be
a zero-length string, or a fatal error will result.

=item C<$~{$user}>

=item C<$~{username}>

=item C<"...$~{$user}...">

=item C<"...$~{username}...">

This calls C<home($user)> or C<home('username')> -- except that if you
ask for C<$~{some_user}> and there is no such user, a fatal error
results!

Note that this is especially useful in doublequotish strings, like:

     print "Jojo's .newsrc is ", -s "$~{jojo}/.newsrc", "b long!\n";
      # (helpfully dies if there is no user 'jojo')

If you want to avoid the fatal errors, first test the value of
C<home('jojo')>, which will return undef (instead of dying) in case of
there being no such user.

Note, however, that if the hash key is "" or undef (whether thru being
a literal "", or a scalar whose value is empty-string or undef), then
this returns zero-argument C<home()>, i.e., your home directory:

=item C<$~{""}>

=item C<$~{undef}>

=item C<"...$~{''}...">

These all return C<home()>, i.e., your home directory.

=back

If running under an OS that doesn't implement C<getpwid>, this library
tries to provide a sane return value for the no-argument C<home()>.
Under MacOS, for example, it tries returning the pathspec to the
desktop folder.  See source for full details.

Under OSs that don't implement C<getpwnam> (as C<home($user)> calls),
you will always get a failed lookup, just as if you'd tried to look up
the home dir for a nonexistent user on an OS that I<does> support
C<getpwnam>.

=head1 BUGS AND CAVEATS

* One-argument C<home($username)> is memoized.  Read the source if you
need it unmemoized.

* According to the fileio.c in one version of Emacs, MSWindows (NT?)
does have the concept of users having home directories, more or less.
But I don't know if MSWin ports of Perl allow accessing that with
C<getpwnam>.  I hear that it (currently) doesn't.

=cut

#What it says is, and I quote:
#
#|
#|#ifdef  WINDOWSNT
#|          /* DebPrint(("EMACS broken @-"__FILE__ ": %d\n", __LINE__)); */
#|          /*
#|           * Emacs wants to know the user's home directory...  This is set by
#|           * the user-manager, but how do I get that information from the
#|           * system?
#|           *
#|           * After a bit of hunting I discover that the user's home directroy
#|           * is stored at:  "HKEY_LOCAL_MACHINE\\security\\sam\\"
#|           * "domains\\account\\users\\<account-rid>\\v" in the registry...
#|           * Now I could pull it out but this location only contains local
#|           * accounts... so if you're logged on to some non-local domain this
#|           * may run into a security problem... i.e. I may not always be able
#|           * to read this information even for myself...
#|           *
#|           * What's here is a hack to make things work...
#|           */
#|
#|          newdir = (unsigned char *) egetenv ("HOME");
#|#else /* !WINDOWSNT */
#|          pw = (struct passwd *) getpwnam (o + 1);
#|          if (pw)
#|            {
#|              newdir = (unsigned char *) pw -> pw_dir;
#|#ifdef VMS
#|              nm = p + 1;       /* skip the terminator */
#|#else
#|              nm = p;
#|#endif                          /* VMS */
#|            }
#|#endif /* !WINDOWSNT */
#|

=pod

* This documentation gets garbled by some AIX manual formatters.
Consider C<perldoc -t File::HomeDir> instead.

=head1 COPYRIGHT

Copyright (c) 2000 Sean M. Burke. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Sean M. Burke C<sburke@cpan.org>

=cut

