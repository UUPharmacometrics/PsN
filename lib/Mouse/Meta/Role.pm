package Mouse::Meta::Role;
use Mouse::Util qw(:meta); # enables strict and warnings

use Mouse::Meta::Module;
our @ISA = qw(Mouse::Meta::Module);

sub method_metaclass;

sub _construct_meta {
    my $class = shift;

    my %args  = @_;

    $args{methods}          = {};
    $args{attributes}       = {};
    $args{required_methods} = [];
    $args{roles}            = [];

    my $self = bless \%args, ref($class) || $class;
    if($class ne __PACKAGE__){
        $self->meta->_initialize_object($self, \%args);
    }
    return $self;
}

sub create_anon_role{
    my $self = shift;
    return $self->create(undef, @_);
}

sub is_anon_role;

sub get_roles;

sub calculate_all_roles {
    my $self = shift;
    my %seen;
    return grep { !$seen{ $_->name }++ }
           ($self, map  { $_->calculate_all_roles } @{ $self->get_roles });
}

sub get_required_method_list{
    return @{ $_[0]->{required_methods} };
}

sub add_required_methods {
    my($self, @methods) = @_;
    my %required = map{ $_ => 1 } @{$self->{required_methods}};
    push @{$self->{required_methods}}, grep{ !$required{$_}++ && !$self->has_method($_) } @methods;
    return;
}

sub requires_method {
    my($self, $name) = @_;
    return scalar( grep{ $_ eq $name } @{ $self->{required_methods} } ) != 0;
}

sub add_attribute {
    my $self = shift;
    my $name = shift;

    $self->{attributes}->{$name} = (@_ == 1) ? $_[0] : { @_ };
    return;
}

sub apply {
    my $self     = shift;
    my $consumer = shift;

    require 'Mouse/Meta/Role/Application.pm';
    return Mouse::Meta::Role::Application->new(@_)->apply($self, $consumer);
}

sub combine {
    my($self, @role_specs) = @_;

    require 'Mouse/Meta/Role/Composite.pm';
    return Mouse::Meta::Role::Composite->new(roles => \@role_specs);
}

sub add_before_method_modifier;
sub add_around_method_modifier;
sub add_after_method_modifier;

sub get_before_method_modifiers;
sub get_around_method_modifiers;
sub get_after_method_modifiers;

sub add_override_method_modifier{
    my($self, $method_name, $method) = @_;

    if($self->has_method($method_name)){
        # This error happens in the override keyword or during role composition,
        # so I added a message, "A local method of ...", only for compatibility (gfx)
        $self->throw_error("Cannot add an override of method '$method_name' "
                   . "because there is a local version of '$method_name'"
                   . "(A local method of the same name as been found)");
    }

    $self->{override_method_modifiers}->{$method_name} = $method;
}

sub get_override_method_modifier {
    my ($self, $method_name) = @_;
    return $self->{override_method_modifiers}->{$method_name};
}

sub does_role {
    my ($self, $role_name) = @_;

    (defined $role_name)
        || $self->throw_error("You must supply a role name to look for");

    $role_name = $role_name->name if ref $role_name;

    # if we are it,.. then return true
    return 1 if $role_name eq $self->name;
    # otherwise.. check our children
    for my $role (@{ $self->get_roles }) {
        return 1 if $role->does_role($role_name);
    }
    return 0;
}

1;
__END__

=head1 NAME

Mouse::Meta::Role - The Mouse Role metaclass

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 DESCRIPTION

This class is a meta object protocol for Mouse roles,
which is a subset of Moose::Meta:::Role.

=head1 SEE ALSO

L<Moose::Meta::Role>

=cut
