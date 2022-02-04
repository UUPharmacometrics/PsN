package Mouse::Spec;
use strict;
use warnings;

use version; our $VERSION = version->declare('v2.5.10');

our $MouseVersion = $VERSION;
our $MooseVersion = '1.13';

sub MouseVersion{ $MouseVersion }
sub MooseVersion{ $MooseVersion }

1;
__END__

=for stopwords metaclasses

=head1 NAME

Mouse::Spec - To what extent Mouse is compatible with Moose

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SYNOPSIS

    use Mouse::Spec;

    printf "Mouse/%s is compatible with Moose/%s\n",
        Mouse::Spec->MouseVersion, Mouse::Spec->MooseVersion;

=head1 DESCRIPTION

Mouse is a subset of Moose. This document describes to what extend Mouse is
compatible (and incompatible) with Moose.

=head2 Compatibility with Moose

=head3 Sugary APIs

The sugary APIs are highly compatible with Moose. Methods which have the
same name as Moose's are expected to be compatible with Moose's.

=head3 Meta object protocols

Meta object protocols are a subset of the counterpart of Moose.
Their methods which have the same name as Moose's are expected to be
compatible with Moose's. Feel free to use these methods even if they
are not documented.

However, there are differences between Moose's MOP and Mouse's.
For example, meta object protocols in Mouse have no attributes by default,
so C<< $metaclass->meta->make_immutable() >> will not work as you expect.
B<Don not make metaclasses immutable>.

=head3 Mouse::Meta::Instance

Meta instance mechanism is not implemented, so you cannot change the reftype
of Mouse objects in the same way as Moose.

=head3 Role exclusion

Role exclusion, C<exclude()>, is not implemented.

=head3 -metaclass in Mouse::Exporter

C<< use Mouse -metaclass => ... >> are not implemented.
Use C<< use Mouse -traits => ... >> instead.

=head3 Mouse::Meta::Attribute::Native

Native traits are not supported directly, but C<MouseX::NativeTraits> is
available on CPAN. Once you have installed it, you can use it as the same way
in Moose. That is, native traits are automatically loaded by Mouse.

See L<MouseX::NativeTraits> for details.

=head2 Notes about Moose::Cookbook

Many recipes in L<Moose::Cookbook> fit L<Mouse>, including:

=over 4

=item *

L<Moose::Cookbook::Basics::Recipe1> - The (always classic) B<Point> example

=item *

L<Moose::Cookbook::Basics::Recipe2> - A simple B<BankAccount> example

=item *

L<Moose::Cookbook::Basics::Recipe3> - A lazy B<BinaryTree> example

=item *

L<Moose::Cookbook::Basics::Recipe4> - Subtypes, and modeling a simple B<Company> class hierarchy

=item *

L<Moose::Cookbook::Basics::Recipe5> - More subtypes, coercion in a B<Request> class

=item *

L<Moose::Cookbook::Basics::Recipe6> - The augment/inner example

=item *

L<Moose::Cookbook::Basics::Recipe7> - Making Moose fast with immutable

=item *

L<Moose::Cookbook::Basics::Recipe8> - Builder methods and lazy_build

=item *

L<Moose::Cookbook::Basics::Recipe9> - Operator overloading, subtypes, and coercion

=item *

L<Moose::Cookbook::Basics::Recipe10> - Using BUILDARGS and BUILD to hook into object construction

=item *

L<Moose::Cookbook::Roles::Recipe1> - The Moose::Role example

=item *

L<Moose::Cookbook::Roles::Recipe2> - Advanced Role Composition - method exclusion and aliasing

=item *

L<Moose::Cookbook::Roles::Recipe3> - Applying a role to an object instance

=item *

L<Moose::Cookbook::Meta::Recipe2> - A meta-attribute, attributes with labels

=item *

L<Moose::Cookbook::Meta::Recipe3> - Labels implemented via attribute traits

=item *

L<Moose::Cookbook::Extending::Recipe3> - Providing an alternate base object class

=back

=head1 SEE ALSO

L<Mouse>

L<Moose>

L<Moose::Manual>

L<Moose::Cookbook>

=cut

