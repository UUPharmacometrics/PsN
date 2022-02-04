package Mouse::Meta::Module;
use Mouse::Util qw/:meta/; # enables strict and warnings

use Carp         ();
use Scalar::Util ();

my %METAS;

if(Mouse::Util::MOUSE_XS){
    # register meta storage for performance
    Mouse::Util::__register_metaclass_storage(\%METAS, 0);

    # ensure thread safety
    *CLONE = sub { Mouse::Util::__register_metaclass_storage(\%METAS, 1) };
}

sub initialize {
    my($class, $package_name, @args) = @_;

    ($package_name && !ref($package_name))
        || $class->throw_error("You must pass a package name and it cannot be blessed");

    return $METAS{$package_name}
        ||= $class->_construct_meta(package => $package_name, @args);
}

sub reinitialize {
    my($class, $package_name, @args) = @_;

    $package_name = $package_name->name if ref $package_name;

    ($package_name && !ref($package_name))
        || $class->throw_error("You must pass a package name and it cannot be blessed");

    if(exists $METAS{$package_name}) {
        unshift @args, %{ $METAS{$package_name} };
    }
    delete $METAS{$package_name};
    return $class->initialize($package_name, @args);
}

sub _class_of{
    my($class_or_instance) = @_;
    return undef unless defined $class_or_instance;
    return $METAS{ ref($class_or_instance) || $class_or_instance };
}

# Means of accessing all the metaclasses that have
# been initialized thus far.
# The public versions are aliased into Mouse::Util::*.
#sub _get_all_metaclasses         {        %METAS         }
sub _get_all_metaclass_instances { values %METAS         }
sub _get_all_metaclass_names     { keys   %METAS         }
sub _get_metaclass_by_name       { $METAS{$_[0]}         }
#sub _store_metaclass_by_name     { $METAS{$_[0]} = $_[1] }
#sub _weaken_metaclass            { weaken($METAS{$_[0]}) }
#sub _does_metaclass_exist        { defined $METAS{$_[0]} }
#sub _remove_metaclass_by_name    { delete $METAS{$_[0]}  }

sub name;

sub namespace;

# add_attribute is an abstract method

sub get_attribute_map { # DEPRECATED
    Carp::cluck('get_attribute_map() has been deprecated. Use get_attribute_list() and get_attribute() instead');
    return $_[0]->{attributes};
}

sub has_attribute     { exists $_[0]->{attributes}->{$_[1]} }
sub get_attribute     {        $_[0]->{attributes}->{$_[1]} }
sub remove_attribute  { delete $_[0]->{attributes}->{$_[1]} }

sub get_attribute_list{ keys   %{$_[0]->{attributes}} }

# XXX: not completely compatible with Moose
my %foreign = map{ $_ => undef } qw(
    Mouse Mouse::Role Mouse::Util Mouse::Util::TypeConstraints
    Carp Scalar::Util List::Util
);
sub _get_method_body {
    my($self, $method_name) = @_;
    my $code = Mouse::Util::get_code_ref($self->{package}, $method_name);
    return $code && !exists $foreign{ Mouse::Util::get_code_package($code) }
        ? $code
        : undef;
}

sub add_method;

sub has_method {
    my($self, $method_name) = @_;
    defined($method_name)
        or $self->throw_error('You must define a method name');

    return defined( $self->{methods}{$method_name} )
        || defined( $self->_get_method_body($method_name) );
}

sub get_method_body {
    my($self, $method_name) = @_;
    defined($method_name)
        or $self->throw_error('You must define a method name');

    return $self->{methods}{$method_name}
        ||= $self->_get_method_body($method_name);
}

sub get_method {
    my($self, $method_name) = @_;

    if(my $code = $self->get_method_body($method_name)){
        return Mouse::Util::load_class($self->method_metaclass)->wrap(
            body                 => $code,
            name                 => $method_name,
            package              => $self->name,
            associated_metaclass => $self,
        );
    }

    return undef;
}

sub get_method_list {
    my($self) = @_;

    return grep { $self->has_method($_) } keys %{ $self->namespace };
}

sub _collect_methods { # Mouse specific, used for method modifiers
    my($meta, @args) = @_;

    my @methods;
    foreach my $arg(@args){
        if(my $type = ref $arg){
            if($type eq 'Regexp'){
                push @methods, grep { $_ =~ $arg } $meta->get_all_method_names;
            }
            elsif($type eq 'ARRAY'){
                push @methods, @{$arg};
            }
            else{
                my $subname = ( caller(1) )[3];
                $meta->throw_error(
                    sprintf(
                        'Methods passed to %s must be provided as a list,'
                        . ' ArrayRef or regular expression, not %s',
                        $subname,
                        $type,
                    )
                );
            }
         }
         else{
            push @methods, $arg;
         }
     }
     return @methods;
}

my $ANON_SERIAL = 0;  # anonymous class/role id
my %IMMORTALS;        # immortal anonymous classes

sub create {
    my($self, $package_name, %options) = @_;

    my $class = ref($self) || $self;
    $self->throw_error('You must pass a package name') if @_ < 2;

    my $superclasses;
    if(exists $options{superclasses}){
        if(Mouse::Util::is_a_metarole($self)){
            delete $options{superclasses};
        }
        else{
            $superclasses = delete $options{superclasses};
            (ref $superclasses eq 'ARRAY')
                || $self->throw_error("You must pass an ARRAY ref of superclasses");
        }
    }

    my $attributes = delete $options{attributes};
    if(defined $attributes){
        (ref $attributes eq 'ARRAY' || ref $attributes eq 'HASH')
            || $self->throw_error("You must pass an ARRAY ref of attributes");
    }
    my $methods = delete $options{methods};
    if(defined $methods){
        (ref $methods eq 'HASH')
            || $self->throw_error("You must pass a HASH ref of methods");
    }
    my $roles = delete $options{roles};
    if(defined $roles){
        (ref $roles eq 'ARRAY')
            || $self->throw_error("You must pass an ARRAY ref of roles");
    }
    my $mortal;
    my $cache_key;

    if(!defined $package_name){ # anonymous
        $mortal = !$options{cache};

        # anonymous but immortal
        if(!$mortal){
                # something like Super::Class|Super::Class::2=Role|Role::1
                $cache_key = join '=' => (
                    join('|',      @{$superclasses || []}),
                    join('|', sort @{$roles        || []}),
                );
                return $IMMORTALS{$cache_key} if exists $IMMORTALS{$cache_key};
        }
        $options{anon_serial_id} = ++$ANON_SERIAL;
        $package_name = $class . '::__ANON__::' . $ANON_SERIAL;
    }


    # instantiate a module
    {
        no strict 'refs';
        ${ $package_name . '::VERSION'   } = delete $options{version}   if exists $options{version};
        ${ $package_name . '::AUTHORITY' } = delete $options{authority} if exists $options{authority};
    }

    my $meta = $self->initialize( $package_name, %options);

    Scalar::Util::weaken($METAS{$package_name})
        if $mortal;

    $meta->add_method(meta => sub {
        $self->initialize(ref($_[0]) || $_[0]);
    });

    $meta->superclasses(@{$superclasses})
        if defined $superclasses;

    # NOTE:
    # process attributes first, so that they can
    # install accessors, but locally defined methods
    # can then overwrite them. It is maybe a little odd, but
    # I think this should be the order of things.
    if (defined $attributes) {
        if(ref($attributes) eq 'ARRAY'){
            # array of Mouse::Meta::Attribute
            foreach my $attr (@{$attributes}) {
                $meta->add_attribute($attr);
            }
        }
        else{
            # hash map of name and attribute spec pairs
            while(my($name, $attr) = each %{$attributes}){
                $meta->add_attribute($name => $attr);
            }
        }
    }
    if (defined $methods) {
        while(my($method_name, $method_body) = each %{$methods}){
            $meta->add_method($method_name, $method_body);
        }
    }
    if (defined $roles and !$options{in_application_to_instance}){
        Mouse::Util::apply_all_roles($package_name, @{$roles});
    }

    if($cache_key){
        $IMMORTALS{$cache_key} = $meta;
    }

    return $meta;
}

sub DESTROY{
    my($self) = @_;

    return if Mouse::Util::in_global_destruction();

    my $serial_id = $self->{anon_serial_id};
    return if !$serial_id;

    # XXX: cleaning stash with threads causes panic/SEGV on legacy perls.
    if(exists $INC{'threads.pm'}) {
        # (caller)[2] indicates the caller's line number,
        # which is zero when the current thread is joining (destroying).
        return if( (caller)[2] == 0);
    }

    # clean up mortal anonymous class stuff

    # @ISA is a magical variable, so we must clear it manually.
    @{$self->{superclasses}} = () if exists $self->{superclasses} && scalar(@{$self->{superclasses}}) > 0;

    # Then, clear the symbol table hash
    %{$self->namespace} = ();

    my $name = $self->name;
    delete $METAS{$name};

    $name =~ s/ $serial_id \z//xms;
    no strict 'refs';
    delete ${$name}{ $serial_id . '::' };
    return;
}


1;
__END__

=head1 NAME

Mouse::Meta::Module - The common base class of Mouse::Meta::Class and Mouse::Meta::Role

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 DESCRIPTION

This class is an abstract base class of meta classes and meta roles.

=head1 SEE ALSO

L<Class::MOP::Class>

L<Class::MOP::Module>

L<Class::MOP::Package>

=cut

