package Mouse::Meta::Role::Application;
use Mouse::Util qw(:meta);

sub new {
    my $class = shift;
    my $args = $class->Mouse::Object::BUILDARGS(@_);

    if(exists $args->{exclude} or exists $args->{alias}) {
        warnings::warnif(deprecated =>
              'The alias and excludes options for role application have been'
            . ' renamed -alias and -exclude');

        if($args->{alias} && !exists $args->{-alias}){
            $args->{-alias} = $args->{alias};
        }
        if($args->{excludes} && !exists $args->{-excludes}){
            $args->{-excludes} = $args->{excludes};
        }
    }

    $args->{aliased_methods} = {};
    if(my $alias = $args->{-alias}){
        @{$args->{aliased_methods}}{ values %{$alias} } = ();
    }

    if(my $excludes = $args->{-excludes}){
        $args->{-excludes} = {}; # replace with a hash ref
        if(ref $excludes){
            %{$args->{-excludes}} = (map{ $_ => undef } @{$excludes});
        }
        else{
            $args->{-excludes}{$excludes} = undef;
        }
    }
    my $self = bless $args, $class;
    if($class ne __PACKAGE__){
        $self->meta->_initialize_object($self, $args);
    }
    return $self;
}

sub apply {
    my($self, $role, $consumer, @extra) = @_;
    my $instance;

    if(Mouse::Util::is_a_metaclass($consumer)) {   # Application::ToClass
        $self->{_to} = 'class';
    }
    elsif(Mouse::Util::is_a_metarole($consumer)) { # Application::ToRole
        $self->{_to} = 'role';
    }
    else {                                         # Appplication::ToInstance
        $self->{_to} = 'instance';
        $instance  = $consumer;

        my $meta = Mouse::Util::class_of($instance);
        $consumer = ($meta || 'Mouse::Meta::Class')
            ->create_anon_class(
                superclasses => [ref $instance],
                roles        => [$role],
                cache        => 0,

                in_application_to_instance => 1, # suppress to apply roles
            );
    }

    #$self->check_role_exclusions($role, $consumer, @extra);
    $self->check_required_methods($role, $consumer, @extra);
    #$self->check_required_attributes($role, $consumer, @extra);

    $self->apply_attributes($role, $consumer, @extra);
    $self->apply_methods($role, $consumer, @extra);
    #$self->apply_override_method_modifiers($role, $consumer, @extra);
    #$self->apply_before_method_modifiers($role, $consumer, @extra);
    #$self->apply_around_method_modifiers($role, $consumer, @extra);
    #$self->apply_after_method_modifiers($role, $consumer, @extra);
    $self->apply_modifiers($role, $consumer, @extra);

    $self->_append_roles($role, $consumer);

    if(defined $instance){ # Application::ToInstance
        # rebless instance
        bless $instance, $consumer->name;
        $consumer->_initialize_object($instance, $instance, 1);
    }

    return;
}

sub check_required_methods {
    my($self, $role, $consumer) = @_;

    if($self->{_to} eq 'role'){
        $consumer->add_required_methods($role->get_required_method_list);
    }
    else{ # to class or instance
        my $consumer_class_name = $consumer->name;

        my @missing;
        foreach my $method_name(@{$role->{required_methods}}){
            next if exists $self->{aliased_methods}{$method_name};
            next if exists $role->{methods}{$method_name};
            next if $consumer_class_name->can($method_name);

            push @missing, $method_name;
        }
        if(@missing){
            $role->throw_error(sprintf "'%s' requires the method%s %s to be implemented by '%s'",
                $role->name,
                (@missing == 1 ? '' : 's'), # method or methods
                Mouse::Util::quoted_english_list(@missing),
                $consumer_class_name);
        }
    }

    return;
}

sub apply_methods {
    my($self, $role, $consumer) = @_;

    my $alias    = $self->{-alias};
    my $excludes = $self->{-excludes};

    foreach my $method_name($role->get_method_list){
        next if $method_name eq 'meta';

        my $code = $role->get_method_body($method_name);

        if(!exists $excludes->{$method_name}){
            if(!$consumer->has_method($method_name)){
                # The third argument $role is used in Role::Composite
                $consumer->add_method($method_name => $code, $role);
            }
        }

        if(exists $alias->{$method_name}){
            my $dstname = $alias->{$method_name};

            my $dstcode = $consumer->get_method_body($dstname);

            if(defined($dstcode) && $dstcode != $code){
                $role->throw_error("Cannot create a method alias if a local method of the same name exists");
            }
            else{
                $consumer->add_method($dstname => $code, $role);
            }
        }
    }

    return;
}

sub apply_attributes {
    my($self, $role, $consumer) = @_;

    for my $attr_name ($role->get_attribute_list) {
        next if $consumer->has_attribute($attr_name);

        $consumer->add_attribute($attr_name
            => $role->get_attribute($attr_name));
    }
    return;
}

sub apply_modifiers {
    my($self, $role, $consumer) = @_;

    if(my $modifiers = $role->{override_method_modifiers}){
        foreach my $method_name (keys %{$modifiers}){
            $consumer->add_override_method_modifier(
                $method_name => $modifiers->{$method_name});
        }
    }

    for my $modifier_type (qw/before around after/) {
        my $table = $role->{"${modifier_type}_method_modifiers"}
            or next;

        my $add_modifier = "add_${modifier_type}_method_modifier";

        while(my($method_name, $modifiers) = each %{$table}){
            foreach my $code(@{ $modifiers }) {
                # skip if the modifier is already applied
                next if $consumer->{"_applied_$modifier_type"}{$method_name, $code}++;
                $consumer->$add_modifier($method_name => $code);
            }
        }
    }
    return;
}

sub _append_roles {
    my($self, $role, $metaclass_or_role) = @_;

    my $roles = $metaclass_or_role->{roles};
    foreach my $r($role, @{$role->get_roles}){
        if(!$metaclass_or_role->does_role($r)){
            push @{$roles}, $r;
        }
    }
    return;
}
1;
__END__

=head1 NAME

Mouse::Meta::Role::Application - The Mouse role application class

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SEE ALSO

L<Moose::Role::Application>

L<Moose::Role::Application::ToClass>

L<Moose::Role::Application::ToRole>

L<Moose::Role::Application::ToInstance>

=cut

