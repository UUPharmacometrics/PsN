package Mouse;
use 5.008_005;

use Mouse::Exporter; # enables strict and warnings

use version; our $VERSION = version->declare('v2.5.10');

use Carp         ();
use Scalar::Util ();

use Mouse::Util ();

use Mouse::Meta::Module;
use Mouse::Meta::Class;
use Mouse::Meta::Role;
use Mouse::Meta::Attribute;
use Mouse::Object;
use Mouse::Util::TypeConstraints ();

Mouse::Exporter->setup_import_methods(
    as_is => [qw(
        extends with
        has
        before after around
        override super
        augment  inner
    ),
        \&Scalar::Util::blessed,
        \&Carp::confess,
   ],
);


sub extends {
    Mouse::Meta::Class->initialize(scalar caller)->superclasses(@_);
    return;
}

sub with {
    Mouse::Util::apply_all_roles(scalar(caller), @_);
    return;
}

sub has {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);
    my $name = shift;

    $meta->throw_error(q{Usage: has 'name' => ( key => value, ... )})
        if @_ % 2; # odd number of arguments

    for my $n(ref($name) ? @{$name} : $name){
        $meta->add_attribute($n => @_);
    }
    return;
}

sub before {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);
    my $code = pop;
    for my $name($meta->_collect_methods(@_)) {
        $meta->add_before_method_modifier($name => $code);
    }
    return;
}

sub after {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);
    my $code = pop;
    for my $name($meta->_collect_methods(@_)) {
        $meta->add_after_method_modifier($name => $code);
    }
    return;
}

sub around {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);
    my $code = pop;
    for my $name($meta->_collect_methods(@_)) {
        $meta->add_around_method_modifier($name => $code);
    }
    return;
}

our $SUPER_PACKAGE;
our $SUPER_BODY;
our @SUPER_ARGS;

sub super {
    # This check avoids a recursion loop - see
    # t/100_bugs/020_super_recursion.t
    return if  defined $SUPER_PACKAGE && $SUPER_PACKAGE ne caller();
    return if !defined $SUPER_BODY;
    $SUPER_BODY->(@SUPER_ARGS);
}

sub override {
    # my($name, $method) = @_;
    Mouse::Meta::Class->initialize(scalar caller)->add_override_method_modifier(@_);
}

our %INNER_BODY;
our %INNER_ARGS;

sub inner {
    my $pkg = caller();
    if ( my $body = $INNER_BODY{$pkg} ) {
        my $args = $INNER_ARGS{$pkg};
        local $INNER_ARGS{$pkg};
        local $INNER_BODY{$pkg};
        return $body->(@{$args});
    }
    else {
        return;
    }
}

sub augment {
    #my($name, $method) = @_;
    Mouse::Meta::Class->initialize(scalar caller)->add_augment_method_modifier(@_);
    return;
}

sub init_meta {
    shift;
    my %args = @_;

    my $class = $args{for_class}
        or confess("Cannot call init_meta without specifying a for_class");

    my $base_class = $args{base_class} || 'Mouse::Object';
    my $metaclass  = $args{metaclass}  || 'Mouse::Meta::Class';

    my $meta = $metaclass->initialize($class);
    my $filename = Mouse::Util::module_notional_filename($meta->name);
    $INC{$filename} = '(set by Mouse)'
        unless exists $INC{$filename};

    $meta->add_method(meta => sub{
        return $metaclass->initialize(ref($_[0]) || $_[0]);
    });

    $meta->superclasses($base_class)
        unless $meta->superclasses;

    # make a class type for each Mouse class
    Mouse::Util::TypeConstraints::class_type($class)
        unless Mouse::Util::TypeConstraints::find_type_constraint($class);

    return $meta;
}

1;
__END__

=for stopwords sartak MooseX MouseX aliasing codebase coderef deinitialization destructor invocant metaclass metaclass metaroles mro reftype reinitialize ro rw unimport wu-lee

=head1 NAME

Mouse - Moose minus the antlers

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SYNOPSIS

    package Point;
    use Mouse; # automatically turns on strict and warnings

    has 'x' => (is => 'rw', isa => 'Int');
    has 'y' => (is => 'rw', isa => 'Int');

    sub clear {
        my($self) = @_;
        $self->x(0);
        $self->y(0);
    }


    __PACKAGE__->meta->make_immutable();

    package Point3D;
    use Mouse;

    extends 'Point';

    has 'z' => (is => 'rw', isa => 'Int');

    after 'clear' => sub {
        my($self) = @_;
        $self->z(0);
    };

    __PACKAGE__->meta->make_immutable();

=head1 DESCRIPTION

L<Moose|Moose> is a postmodern object system for Perl5. Moose is wonderful.

Unfortunately, Moose has a compile-time penalty. Though significant progress
has been made over the years, the compile time penalty is a non-starter for
some very specific applications. If you are writing a command-line application
or CGI script where startup time is essential, you may not be able to use
Moose (we recommend that you instead use persistent Perl executing environments
like C<FastCGI> for the latter, if possible).

Mouse is a Moose compatible object system, which aims to alleviate this penalty
by providing a subset of Moose's functionality.

We're also going as light on dependencies as possible. Mouse currently has
B<no dependencies> except for building/testing modules. Mouse also works
without XS, although it has an XS backend to make it much faster.

=head2 Moose Compatibility

Compatibility with Moose has been the utmost concern. The sugary interface is
highly compatible with Moose. Even the error messages are taken from Moose.
The Mouse code just runs its test suite 4x faster.

The idea is that, if you need the extra power, you should be able to run
C<s/Mouse/Moose/g> on your codebase and have nothing break. To that end,
we have written L<Any::Moose|Any::Moose> which will act as Mouse unless Moose is loaded,
in which case it will act as Moose. Since Mouse is a little sloppier than
Moose, if you run into weird errors, it would be worth running:

    ANY_MOOSE=Moose perl your-script.pl

to see if the bug is caused by Mouse. Moose's diagnostics and validation are
also better.

See also L<Mouse::Spec> for compatibility and incompatibility with Moose.

=head2 Mouse Extentions

Please don't copy MooseX code to MouseX. If you need extensions, you really
should upgrade to Moose. We don't need two parallel sets of extensions!

If you really must write a Mouse extension, please contact the Moose mailing
list or #moose on IRC beforehand.

=head1 KEYWORDS

=head2 C<< $object->meta -> Mouse::Meta::Class >>

Returns this class' metaclass instance.

=head2 C<< extends superclasses >>

Sets this class' superclasses.

=head2 C<< before (method|methods|regexp) => CodeRef >>

Installs a "before" method modifier. See L<Moose/before>.

=head2 C<< after (method|methods|regexp) => CodeRef >>

Installs an "after" method modifier. See L<Moose/after>.

=head2 C<< around (method|methods|regexp) => CodeRef >>

Installs an "around" method modifier. See L<Moose/around>.

=head2 C<< has (name|names) => parameters >>

Adds an attribute (or if passed an arrayref of names, multiple attributes) to
this class. Options:

=over 4

=item C<< is => ro|rw|bare >>

The I<is> option accepts either I<rw> (for read/write), I<ro> (for read
only) or I<bare> (for nothing). These will create either a read/write accessor
or a read-only accessor respectively, using the same name as the C<$name> of
the attribute.

If you need more control over how your accessors are named, you can
use the C<reader>, C<writer> and C<accessor> options, however if you
use those, you won't need the I<is> option.

=item C<< isa => TypeName | ClassName >>

Provides type checking in the constructor and accessor. The following types are
supported. Any unknown type is taken to be a class check
(e.g. C<< isa => 'DateTime' >> would accept only L<DateTime> objects).

    Any Item Bool Undef Defined Value Num Int Str ClassName
    Ref ScalarRef ArrayRef HashRef CodeRef RegexpRef GlobRef
    FileHandle Object

For more documentation on type constraints, see L<Mouse::Util::TypeConstraints>.

=item C<< does => RoleName >>

This will accept the name of a role which the value stored in this attribute
is expected to have consumed.

=item C<< coerce => Bool >>

This will attempt to use coercion with the supplied type constraint to change
the value passed into any accessors or constructors. You B<must> have supplied
a type constraint in order for this to work. See L<Moose::Cookbook::Basics::Recipe5>
for an example.

=item C<< required => Bool >>

Whether this attribute is required to have a value. If the attribute is lazy or
has a builder, then providing a value for the attribute in the constructor is
optional.

=item C<< init_arg => Str | Undef >>

Allows you to use a different key name in the constructor.  If undef, the
attribute can't be passed to the constructor.

=item C<< default => Value | CodeRef >>

Sets the default value of the attribute. If the default is a coderef, it will
be invoked to get the default value. Due to quirks of Perl, any bare reference
is forbidden, you must wrap the reference in a coderef. Otherwise, all
instances will share the same reference.

=item C<< lazy => Bool >>

If specified, the default is calculated on demand instead of in the
constructor.

=item C<< predicate => Str >>

Lets you specify a method name for installing a predicate method, which checks
that the attribute has a value. It will not invoke a lazy default or builder
method.

=item C<< clearer => Str >>

Lets you specify a method name for installing a clearer method, which clears
the attribute's value from the instance. On the next read, lazy or builder will
be invoked.

=item C<< handles => HashRef|ArrayRef|Regexp >>

Lets you specify methods to delegate to the attribute. ArrayRef forwards the
given method names to method calls on the attribute. HashRef maps local method
names to remote method names called on the attribute. Other forms of
L</handles>, such as RoleName and CodeRef, are not yet supported.

=item C<< weak_ref => Bool >>

Lets you automatically weaken any reference stored in the attribute.

Use of this feature requires L<Scalar::Util>!

=item C<< trigger => CodeRef >>

Any time the attribute's value is set (either through the accessor or the constructor), the trigger is called on it. The trigger receives as arguments the instance, and the new value.

=item C<< builder => Str >>

Defines a method name to be called to provide the default value of the
attribute. C<< builder => 'build_foo' >> is mostly equivalent to
C<< default => sub { $_[0]->build_foo } >>.

=item C<< auto_deref => Bool >>

Allows you to automatically dereference ArrayRef and HashRef attributes in list
context. In scalar context, the reference is returned (NOT the list length or
bucket status). You must specify an appropriate type constraint to use
auto_deref.

=item C<< lazy_build => Bool >>

Automatically define the following options:

    has $attr => (
        # ...
        lazy      => 1
        builder   => "_build_$attr",
        clearer   => "clear_$attr",
        predicate => "has_$attr",
    );

=back

=head2 C<< confess(message) -> BOOM >>

L<Carp/confess> for your convenience.

=head2 C<< blessed(value) -> ClassName | undef >>

L<Scalar::Util/blessed> for your convenience.

=head1 MISC

=head2 import

Importing Mouse will default your class' superclass list to L<Mouse::Object>.
You may use L</extends> to replace the superclass list.

=head2 unimport

Please unimport Mouse (C<no Mouse>) so that if someone calls one of the
keywords (such as L</extends>) it will break loudly instead breaking subtly.

=head1 DEVELOPMENT

Here is the repo: L<https://github.com/gfx/p5-Mouse>.

You can build, test, and release it with B<Minilla>.

    cpanm Minilla
    minil build
    minil test
    minil release

Note that F<Build.PL> and F<README.md> are generated by Minilla,
so you should not edit them. Edit F<minil.toml> and F<lib/Mouse.pm> instead.

=head1 SEE ALSO

L<Mouse::Role>

L<Mouse::Spec>

L<Moose>

L<Moose::Manual>

L<Moose::Cookbook>

L<Class::MOP>

L<Moo>

=head1 AUTHORS

Shawn M Moore E<lt>sartak at gmail.comE<gt>

Yuval Kogman E<lt>nothingmuch at woobling.orgE<gt>

tokuhirom

Yappo

wu-lee

Goro Fuji (gfx) E<lt>gfuji@cpan.orgE<gt>

with plenty of code borrowed from L<Class::MOP> and L<Moose>

=head1 BUGS

All complex software has bugs lurking in it, and this module is no exception.
Please report any bugs to L<https://github.com/gfx/p5-Mouse/issues>.

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2008-2010 Infinity Interactive, Inc.

http://www.iinteractive.com/

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

