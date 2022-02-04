package Mouse::Meta::Role::Composite;
use Carp ();
use Mouse::Util; # enables strict and warnings
use Mouse::Meta::Role;
use Mouse::Meta::Role::Application;
our @ISA = qw(Mouse::Meta::Role);

# FIXME: Mouse::Meta::Role::Composite does things in different way from Moose's
# Moose: creates a new class for the consumer, and applies roles to it.
# Mouse: creates a composite role and apply roles to the role,
#        and then applies it to the consumer.

sub new {
    my $class = shift;
    my $args  = $class->Mouse::Object::BUILDARGS(@_);
    my $roles = delete $args->{roles};
    my $self  = $class->create_anon_role(%{$args});
    foreach my $role_spec(@{$roles}) {
        my($role, $args) = ref($role_spec) eq 'ARRAY'
            ? @{$role_spec}
            : ($role_spec, {});
        $role->apply($self, %{$args});
    }
    return $self;
}

sub get_method_list {
    my($self) = @_;
    return grep { ! $self->{conflicting_methods}{$_} }
                                                    keys %{ $self->{methods} };
}

sub add_method {
    my($self, $method_name, $code, $role) = @_;

    if( ($self->{methods}{$method_name} || 0) == $code){
        # This role already has the same method.
        return;
    }

    if($method_name eq 'meta'){
        $self->SUPER::add_method($method_name => $code);
    }
    else{
        # no need to add a subroutine to the stash
        my $roles = $self->{composed_roles_by_method}{$method_name} ||= [];
        push @{$roles}, $role;
        if(@{$roles} > 1){
            $self->{conflicting_methods}{$method_name}++;
        }
        $self->{methods}{$method_name} = $code;
    }
    return;
}

sub get_method_body {
    my($self, $method_name) = @_;
    return $self->{methods}{$method_name};
}

sub has_method {
    # my($self, $method_name) = @_;
    return 0; # to fool apply_methods() in combine()
}

sub has_attribute {
    # my($self, $method_name) = @_;
    return 0; # to fool appply_attributes() in combine()
}

sub has_override_method_modifier {
    # my($self, $method_name) = @_;
    return 0; # to fool apply_modifiers() in combine()
}

sub add_attribute {
    my $self      = shift;
    my $attr_name = shift;
    my $spec      = (@_ == 1 ? $_[0] : {@_});

    my $existing = $self->{attributes}{$attr_name};
    if($existing && $existing != $spec){
        $self->throw_error("We have encountered an attribute conflict with '$attr_name' "
                         . "during composition. This is fatal error and cannot be disambiguated.");
    }
    $self->SUPER::add_attribute($attr_name, $spec);
    return;
}

sub add_override_method_modifier {
    my($self, $method_name, $code) = @_;

    my $existing = $self->{override_method_modifiers}{$method_name};
    if($existing && $existing != $code){
        $self->throw_error( "We have encountered an 'override' method conflict with '$method_name' during "
                          . "composition (Two 'override' methods of the same name encountered). "
                          . "This is fatal error.")
    }
    $self->SUPER::add_override_method_modifier($method_name, $code);
    return;
}

sub apply {
    my $self     = shift;
    my $consumer = shift;

    Mouse::Meta::Role::Application::RoleSummation->new(@_)->apply($self, $consumer);
    return;
}

package Mouse::Meta::Role::Application::RoleSummation;
our @ISA = qw(Mouse::Meta::Role::Application);

sub apply_methods {
    my($self, $role, $consumer, @extra) = @_;

    if(exists $role->{conflicting_methods}){
        my $consumer_class_name = $consumer->name;

        my @conflicting = grep{ !$consumer_class_name->can($_) }
            keys %{ $role->{conflicting_methods} };

        if(@conflicting) {
            my $method_name_conflict = (@conflicting == 1
                ? 'a method name conflict'
                : 'method name conflicts');

            my %seen;
            my $roles = Mouse::Util::quoted_english_list(
                grep{ !$seen{$_}++ } # uniq
                map { $_->name }
                map { @{$_} }
                @{ $role->{composed_roles_by_method} }{@conflicting}
            );

            $self->throw_error(sprintf
                  q{Due to %s in roles %s,}
                . q{ the method%s %s must be implemented or excluded by '%s'},
                    $method_name_conflict,
                    $roles,
                    (@conflicting > 1 ? 's' : ''),
                    Mouse::Util::quoted_english_list(@conflicting),
                    $consumer_class_name);
        }

        my @changed_in_v2_0_0 = grep {
            $consumer_class_name->can($_) && ! $consumer->has_method($_)
        } keys %{ $role->{conflicting_methods} };
        if (@changed_in_v2_0_0) {
            my $method_name_conflict = (@changed_in_v2_0_0 == 1
                ? 'a method name conflict'
                : 'method name conflicts');

            my %seen;
            my $roles = Mouse::Util::quoted_english_list(
                grep{ !$seen{$_}++ } # uniq
                map { $_->name }
                map { @{$_} }
                @{ $role->{composed_roles_by_method} }{@changed_in_v2_0_0}
            );

            Carp::cluck(sprintf
                  q{Due to %s in roles %s,}
                . q{ the behavior of method%s %s might be incompatible with Moose}
                . q{, check out %s},
                    $method_name_conflict,
                    $roles,
                    (@changed_in_v2_0_0 > 1 ? 's' : ''),
                    Mouse::Util::quoted_english_list(@changed_in_v2_0_0),
                    $consumer_class_name);
        }
    }

    $self->SUPER::apply_methods($role, $consumer, @extra);
    return;
}

package Mouse::Meta::Role::Composite;
1;
__END__

=head1 NAME

Mouse::Meta::Role::Composite - An object to represent the set of roles

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SEE ALSO

L<Moose::Meta::Role::Composite>

=cut
