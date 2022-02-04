package Mouse::Role;
use Mouse::Exporter; # enables strict and warnings

use version; our $VERSION = version->declare('v2.5.10');

use Carp         ();
use Scalar::Util ();

use Mouse ();

Mouse::Exporter->setup_import_methods(
    as_is => [qw(
        extends with
        has
        before after around
        override super
        augment  inner

        requires excludes
    ),
        \&Scalar::Util::blessed,
        \&Carp::confess,
    ],
);


sub extends  {
    Carp::croak "Roles do not support 'extends'";
}

sub with {
    Mouse::Util::apply_all_roles(scalar(caller), @_);
    return;
}

sub has {
    my $meta = Mouse::Meta::Role->initialize(scalar caller);
    my $name = shift;

    $meta->throw_error(q{Usage: has 'name' => ( key => value, ... )})
        if @_ % 2; # odd number of arguments

    for my $n(ref($name) ? @{$name} : $name){
        $meta->add_attribute($n => @_);
    }
    return;
}

sub before {
    my $meta = Mouse::Meta::Role->initialize(scalar caller);
    my $code = pop;
    for my $name($meta->_collect_methods(@_)) {
        $meta->add_before_method_modifier($name => $code);
    }
    return;
}

sub after {
    my $meta = Mouse::Meta::Role->initialize(scalar caller);
    my $code = pop;
    for my $name($meta->_collect_methods(@_)) {
        $meta->add_after_method_modifier($name => $code);
    }
    return;
}

sub around {
    my $meta = Mouse::Meta::Role->initialize(scalar caller);
    my $code = pop;
    for my $name($meta->_collect_methods(@_)) {
        $meta->add_around_method_modifier($name => $code);
    }
    return;
}


sub super {
    return if !defined $Mouse::SUPER_BODY;
    $Mouse::SUPER_BODY->(@Mouse::SUPER_ARGS);
}

sub override {
    # my($name, $code) = @_;
    Mouse::Meta::Role->initialize(scalar caller)->add_override_method_modifier(@_);
    return;
}

# We keep the same errors messages as Moose::Role emits, here.
sub inner {
    Carp::croak "Roles cannot support 'inner'";
}

sub augment {
    Carp::croak "Roles cannot support 'augment'";
}

sub requires {
    my $meta = Mouse::Meta::Role->initialize(scalar caller);
    $meta->throw_error("Must specify at least one method") unless @_;
    $meta->add_required_methods(@_);
    return;
}

sub excludes {
    Mouse::Util::not_supported();
}

sub init_meta{
    shift;
    my %args = @_;

    my $class = $args{for_class}
        or Carp::confess("Cannot call init_meta without specifying a for_class");

    my $metaclass  = $args{metaclass}  || 'Mouse::Meta::Role';

    my $meta = $metaclass->initialize($class);
    my $filename = Mouse::Util::module_notional_filename($meta->name);
    $INC{$filename} = '(set by Mouse)'
        unless exists $INC{$filename};

    $meta->add_method(meta => sub{
        $metaclass->initialize(ref($_[0]) || $_[0]);
    });

    # make a role type for each Mouse role
    Mouse::Util::TypeConstraints::role_type($class)
        unless Mouse::Util::TypeConstraints::find_type_constraint($class);

    return $meta;
}

1;

__END__

=head1 NAME

Mouse::Role - The Mouse Role

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SYNOPSIS

    package Comparable;
    use Mouse::Role; # the package is now a Mouse role

    # Declare methods that are required by this role
    requires qw(compare);

    # Define methods this role provides
    sub equals {
        my($self, $other) = @_;
        return $self->compare($other) == 0;
    }

    # and later
    package MyObject;
    use Mouse;
    with qw(Comparable); # Now MyObject can equals()

    sub compare {
        # ...
    }

    my $foo = MyObject->new();
    my $bar = MyObject->new();
    $obj->equals($bar); # yes, it is comparable

=head1 DESCRIPTION

This module declares the caller class to be a Mouse role.

The concept of roles is documented in L<Moose::Manual::Roles>.
This document serves as API documentation.

=head1 EXPORTED FUNCTIONS

Mouse::Role supports all of the functions that Mouse exports, but
differs slightly in how some items are handled (see L</CAVEATS> below
for details).

Mouse::Role also offers two role-specific keywords:

=head2 C<< requires(@method_names) >>

Roles can require that certain methods are implemented by any class which
C<does> the role.

Note that attribute accessors also count as methods for the purposes of
satisfying the requirements of a role.

=head2 C<< excludes(@role_names) >>

This is exported but not implemented in Mouse.

=head1 IMPORT AND UNIMPORT

=head2 import

Importing Mouse::Role will give you sugar. C<-traits> are also supported.

=head2 unimport

Please unimport (C<< no Mouse::Role >>) so that if someone calls one of the
keywords (such as L</has>) it will break loudly instead breaking subtly.

=head1 CAVEATS

Role support has only a few caveats:

=over

=item *

Roles cannot use the C<extends> keyword; it will throw an exception for now.
The same is true of the C<augment> and C<inner> keywords (not sure those
really make sense for roles). All other Mouse keywords will be I<deferred>
so that they can be applied to the consuming class.

=item *

Role composition does its best to B<not> be order-sensitive when it comes to
conflict resolution and requirements detection. However, it is order-sensitive
when it comes to method modifiers. All before/around/after modifiers are
included whenever a role is composed into a class, and then applied in the order
in which the roles are used. This also means that there is no conflict for
before/around/after modifiers.

In most cases, this will be a non-issue; however, it is something to keep in
mind when using method modifiers in a role. You should never assume any
ordering.

=back

=head1 SEE ALSO

L<Mouse>

L<Moose::Role>

L<Moose::Manual::Roles>

L<Moose::Spec::Role>

=cut

