package Mouse::Meta::Attribute;
use Mouse::Util qw(:meta); # enables strict and warnings

use Carp ();

use Mouse::Meta::TypeConstraint;

my %valid_options = map { $_ => undef } (
  'accessor',
  'auto_deref',
  'builder',
  'clearer',
  'coerce',
  'default',
  'documentation',
  'does',
  'handles',
  'init_arg',
  'insertion_order',
  'is',
  'isa',
  'lazy',
  'lazy_build',
  'name',
  'predicate',
  'reader',
  'required',
  'traits',
  'trigger',
  'type_constraint',
  'weak_ref',
  'writer',

  # internally used
  'associated_class',
  'associated_methods',
  '__METACLASS__',

  # Moose defines, but Mouse doesn't
  #'definition_context',
  #'initializer',

  # special case for AttributeHelpers
  'provides',
  'curries',
);

our @CARP_NOT = qw(Mouse::Meta::Class);

sub new {
    my $class = shift;
    my $name  = shift;

    my $args  = $class->Mouse::Object::BUILDARGS(@_);

    $class->_process_options($name, $args);

    $args->{name} = $name;

    # check options
    # (1) known by core
    my @bad = grep{ !exists $valid_options{$_} } keys %{$args};

    # (2) known by subclasses
    if(@bad && $class ne __PACKAGE__){
        my %valid_attrs = (
            map { $_ => undef }
            grep { defined }
            map { $_->init_arg() }
            $class->meta->get_all_attributes()
        );
        @bad = grep{ !exists $valid_attrs{$_} } @bad;
    }

    # (3) bad options found
    if(@bad){
        Carp::carp(
            "Found unknown argument(s) passed to '$name' attribute constructor in '$class': "
            . Mouse::Util::english_list(@bad));
    }

    my $self = bless $args, $class;
    if($class ne __PACKAGE__){
        $class->meta->_initialize_object($self, $args);
    }
    return $self;
}

sub has_read_method   { $_[0]->has_reader || $_[0]->has_accessor }
sub has_write_method  { $_[0]->has_writer || $_[0]->has_accessor }

sub get_read_method   { $_[0]->reader || $_[0]->accessor }
sub get_write_method  { $_[0]->writer || $_[0]->accessor }

sub get_read_method_ref{
    my($self) = @_;
    return $self->{_mouse_cache_read_method_ref}
        ||= $self->_get_accessor_method_ref('get_read_method', '_generate_reader');
}

sub get_write_method_ref{
    my($self) = @_;
    return $self->{_mouse_cache_write_method_ref}
        ||= $self->_get_accessor_method_ref('get_write_method', '_generate_writer');
}

sub interpolate_class{
    my($class, $args) = @_;

    if(my $metaclass = delete $args->{metaclass}){
        $class = Mouse::Util::resolve_metaclass_alias( Attribute => $metaclass );
    }

    my @traits;
    if(my $traits_ref = delete $args->{traits}){

        for (my $i = 0; $i < @{$traits_ref}; $i++) {
            my $trait = Mouse::Util::resolve_metaclass_alias(Attribute => $traits_ref->[$i], trait => 1);

            next if $class->does($trait);

            push @traits, $trait;

            # are there options?
            push @traits, $traits_ref->[++$i]
                if ref($traits_ref->[$i+1]);
        }

        if (@traits) {
            $class = Mouse::Meta::Class->create_anon_class(
                superclasses => [ $class ],
                roles        => \@traits,
                cache        => 1,
            )->name;
        }
    }

    return( $class, @traits );
}

sub verify_against_type_constraint {
    my ($self, $value) = @_;

    my $type_constraint = $self->{type_constraint};
    return 1 if !$type_constraint;
    return 1 if $type_constraint->check($value);

    $self->_throw_type_constraint_error($value, $type_constraint);
}

sub _throw_type_constraint_error {
    my($self, $value, $type) = @_;

    $self->throw_error(
        sprintf q{Attribute (%s) does not pass the type constraint because: %s},
            $self->name,
            $type->get_message($value),
    );
}

sub illegal_options_for_inheritance {
    return qw(reader writer accessor clearer predicate);
}

sub clone_and_inherit_options{
    my $self = shift;
    my $args = $self->Mouse::Object::BUILDARGS(@_);

    foreach my $illegal($self->illegal_options_for_inheritance) {
        if(exists $args->{$illegal} and exists $self->{$illegal}) {
            $self->throw_error("Illegal inherited option: $illegal");
        }
    }

    foreach my $name(keys %{$self}){
        if(!exists $args->{$name}){
            $args->{$name} = $self->{$name}; # inherit from self
        }
    }

    my($attribute_class, @traits) = ref($self)->interpolate_class($args);
    $args->{traits} = \@traits if @traits;

    # remove temporary caches
    foreach my $attr(keys %{$args}){
        if($attr =~ /\A _mouse_cache_/xms){
            delete $args->{$attr};
        }
    }

    # remove default if lazy_build => 1
    if($args->{lazy_build}) {
        delete $args->{default};
    }

    return $attribute_class->new($self->name, $args);
}


sub _get_accessor_method_ref {
    my($self, $type, $generator) = @_;

    my $metaclass = $self->associated_class
        || $self->throw_error('No asocciated class for ' . $self->name);

    my $accessor = $self->$type();
    if($accessor){
        return $metaclass->get_method_body($accessor);
    }
    else{
        return $self->accessor_metaclass->$generator($self, $metaclass);
    }
}

sub set_value {
    my($self, $object, $value) = @_;
    return $self->get_write_method_ref()->($object, $value);
}

sub get_value {
    my($self, $object) = @_;
    return $self->get_read_method_ref()->($object);
}

sub has_value {
    my($self, $object) = @_;
    my $accessor_ref = $self->{_mouse_cache_predicate_ref}
        ||= $self->_get_accessor_method_ref('predicate', '_generate_predicate');

    return $accessor_ref->($object);
}

sub clear_value {
    my($self, $object) = @_;
    my $accessor_ref = $self->{_mouse_cache_crealer_ref}
        ||= $self->_get_accessor_method_ref('clearer', '_generate_clearer');

    return $accessor_ref->($object);
}

sub associate_method{
    #my($attribute, $method_name) = @_;
    my($attribute) = @_;
    $attribute->{associated_methods}++;
    return;
}

sub install_accessors{
    my($attribute) = @_;

    my $metaclass      = $attribute->associated_class;
    my $accessor_class = $attribute->accessor_metaclass;

    foreach my $type(qw(accessor reader writer predicate clearer)){
        if(exists $attribute->{$type}){
            my $generator = '_generate_' . $type;
            my $code      = $accessor_class->$generator($attribute, $metaclass);
            my $name      = $attribute->{$type};
# TODO: do something for compatibility
#            if( $metaclass->name->can($name) ) {
#                my $t = $metaclass->has_method($name) ? 'method' : 'function';
#                Carp::cluck("You are overwriting a locally defined $t"
#                    . " ($name) with an accessor");
#            }
            $metaclass->add_method($name => $code);
            $attribute->associate_method($name);
        }
    }

    # install delegation
    if(exists $attribute->{handles}){
        my %handles = $attribute->_canonicalize_handles();
        while(my($handle, $method_to_call) = each %handles){
            next if Mouse::Object->can($handle);

            if($metaclass->has_method($handle)) {
                $attribute->throw_error("You cannot overwrite a locally defined method ($handle) with a delegation");
            }

            $metaclass->add_method($handle =>
                $attribute->_make_delegation_method(
                    $handle, $method_to_call));

            $attribute->associate_method($handle);
        }
    }

    return;
}

sub delegation_metaclass() { ## no critic
    'Mouse::Meta::Method::Delegation'
}

sub _canonicalize_handles {
    my($self) = @_;
    my $handles = $self->{handles};

    my $handle_type = ref $handles;
    if ($handle_type eq 'HASH') {
        return %$handles;
    }
    elsif ($handle_type eq 'ARRAY') {
        return map { $_ => $_ } @$handles;
    }
    elsif ($handle_type eq 'Regexp') {
        my $meta = $self->_find_delegate_metaclass();
        return map  { $_ => $_ }
               grep { /$handles/ }
                   Mouse::Util::is_a_metarole($meta)
                        ? $meta->get_method_list
                        : $meta->get_all_method_names;
    }
    elsif ($handle_type eq 'CODE') {
        return $handles->( $self, $self->_find_delegate_metaclass() );
    }
    else {
        $self->throw_error("Unable to canonicalize the 'handles' option with $handles");
    }
}

sub _find_delegate_metaclass {
    my($self) = @_;
    my $meta;
    if($self->{isa}) {
        $meta = Mouse::Meta::Class->initialize("$self->{isa}");
    }
    elsif($self->{does}) {
        $meta = Mouse::Util::get_metaclass_by_name("$self->{does}");
    }
    defined($meta) or $self->throw_error(
        "Cannot find delegate metaclass for attribute " . $self->name);
    return $meta;
}


sub _make_delegation_method {
    my($self, $handle, $method_to_call) = @_;
    return Mouse::Util::load_class($self->delegation_metaclass)
        ->_generate_delegation($self, $handle, $method_to_call);
}

1;
__END__

=head1 NAME

Mouse::Meta::Attribute - The Mouse attribute metaclass

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 DESCRIPTION

This is a meta object protocol for Mouse attributes,
which is a subset of Moose::Meta::Attribute.

=head1 METHODS

=head2 C<< new(%options) -> Mouse::Meta::Attribute >>

Instantiates a new Mouse::Meta::Attribute. Does nothing else.

It adds the following options to the constructor:

=over 4

=item C<< is => 'ro', 'rw', 'bare' >>

This provides a shorthand for specifying the C<reader>, C<writer>, or
C<accessor> names. If the attribute is read-only ('ro') then it will
have a C<reader> method with the same attribute as the name.

If it is read-write ('rw') then it will have an C<accessor> method
with the same name. If you provide an explicit C<writer> for a
read-write attribute, then you will have a C<reader> with the same
name as the attribute, and a C<writer> with the name you provided.

Use 'bare' when you are deliberately not installing any methods
(accessor, reader, etc.) associated with this attribute; otherwise,
Moose will issue a deprecation warning when this attribute is added to a
metaclass.

=item C<< isa => Type >>

This option accepts a type. The type can be a string, which should be
a type name. If the type name is unknown, it is assumed to be a class
name.

This option can also accept a L<Moose::Meta::TypeConstraint> object.

If you I<also> provide a C<does> option, then your C<isa> option must
be a class name, and that class must do the role specified with
C<does>.

=item C<< does => Role >>

This is short-hand for saying that the attribute's type must be an
object which does the named role.

B<This option is not yet supported.>

=item C<< coerce => Bool >>

This option is only valid for objects with a type constraint
(C<isa>). If this is true, then coercions will be applied whenever
this attribute is set.

You can make both this and the C<weak_ref> option true.

=item C<< trigger => CodeRef >>

This option accepts a subroutine reference, which will be called after
the attribute is set.

=item C<< required => Bool >>

An attribute which is required must be provided to the constructor. An
attribute which is required can also have a C<default> or C<builder>,
which will satisfy its required-ness.

A required attribute must have a C<default>, C<builder> or a
non-C<undef> C<init_arg>

=item C<< lazy => Bool >>

A lazy attribute must have a C<default> or C<builder>. When an
attribute is lazy, the default value will not be calculated until the
attribute is read.

=item C<< weak_ref => Bool >>

If this is true, the attribute's value will be stored as a weak
reference.

=item C<< auto_deref => Bool >>

If this is true, then the reader will dereference the value when it is
called. The attribute must have a type constraint which defines the
attribute as an array or hash reference.

=item C<< lazy_build => Bool >>

Setting this to true makes the attribute lazy and provides a number of
default methods.

  has 'size' => (
      is         => 'ro',
      lazy_build => 1,
  );

is equivalent to this:

  has 'size' => (
      is        => 'ro',
      lazy      => 1,
      builder   => '_build_size',
      clearer   => 'clear_size',
      predicate => 'has_size',
  );

=back

=head2 C<< associate_method(MethodName) >>

Associates a method with the attribute. Typically, this is called internally
when an attribute generates its accessors.

Currently the argument I<MethodName> is ignored in Mouse.

=head2 C<< verify_against_type_constraint(Item) -> TRUE | ERROR >>

Checks that the given value passes this attribute's type constraint. Returns C<true>
on success, otherwise C<confess>es.

=head2 C<< clone_and_inherit_options(options) -> Mouse::Meta::Attribute >>

Creates a new attribute in the owner class, inheriting options from parent classes.
Accessors and helper methods are installed. Some error checking is done.

=head2 C<< get_read_method_ref >>

=head2 C<< get_write_method_ref >>

Returns the subroutine reference of a method suitable for reading or
writing the attribute's value in the associated class. These methods
always return a subroutine reference, regardless of whether or not the
attribute is read- or write-only.

=head1 SEE ALSO

L<Moose::Meta::Attribute>

L<Class::MOP::Attribute>

=cut

