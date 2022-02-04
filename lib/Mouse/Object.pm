package Mouse::Object;
use Mouse::Util qw(does dump meta); # enables strict and warnings
# all the stuff are defined in XS or PP

sub DOES {
    my($self, $class_or_role_name) = @_;
    return $self->isa($class_or_role_name) || $self->does($class_or_role_name);
}

1;
__END__

=head1 NAME

Mouse::Object - The base object for Mouse classes

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 METHODS

=head2 C<< $class->new(%args | \%args) -> Object >>

Instantiates a new C<Mouse::Object>. This is obviously intended for subclasses.

=head2 C<< $class->BUILDARGS(@args) -> HashRef >>

Lets you override the arguments that C<new> takes.
It must return a HashRef of parameters.

=head2 C<< $object->BUILDALL(\%args) >>

Calls C<BUILD> on each class in the class hierarchy. This is called at the
end of C<new>.

=head2 C<< $object->BUILD(\%args) >>

You may put any business logic initialization in BUILD methods. You don't
need to redispatch or return any specific value.

=head2 C<< $object->DEMOLISHALL >>

Calls C<DEMOLISH> on each class in the class hierarchy. This is called at
C<DESTROY> time.

=head2 C<< $object->DEMOLISH >>

You may put any business logic deinitialization in DEMOLISH methods. You don't
need to redispatch or return any specific value.

=head2 C<< $object->does($role_name) -> Bool >>

This will check if the invocant's class B<does> a given C<$role_name>.
This is similar to C<isa> for object, but it checks the roles instead.

=head2 C<< $object->dump($maxdepth) -> Str >>

This is a handy utility for dumping an object with Data::Dumper.
By default, the maximum depth is 3, to avoid making a mess.

=head2 C<< $object->meta() -> MetaClass >>

This is a method which provides access to the object's metaclass.

=head1 SEE ALSO

L<Moose::Object>

=cut

