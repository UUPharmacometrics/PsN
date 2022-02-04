package Mouse::Meta::Class;
use Mouse::Util qw/:meta/; # enables strict and warnings

use Scalar::Util ();

use Mouse::Meta::Module;
our @ISA = qw(Mouse::Meta::Module);

our @CARP_NOT = qw(Mouse); # trust Mouse

sub attribute_metaclass;
sub method_metaclass;

sub constructor_class;
sub destructor_class;


sub _construct_meta {
    my($class, %args) = @_;

    $args{attributes} = {};
    $args{methods}    = {};
    $args{roles}      = [];

    $args{superclasses} = do {
        no strict 'refs';
        \@{ $args{package} . '::ISA' };
    };

    my $self = bless \%args, ref($class) || $class;
    if(ref($self) ne __PACKAGE__){
        $self->meta->_initialize_object($self, \%args);
    }
    return $self;
}

sub create_anon_class{
    my $self = shift;
    return $self->create(undef, @_);
}

sub is_anon_class;

sub roles;

sub calculate_all_roles {
    my $self = shift;
    my %seen;
    return grep { !$seen{ $_->name }++ }
           map  { $_->calculate_all_roles } @{ $self->roles };
}

sub superclasses {
    my $self = shift;

    if (@_) {
        foreach my $super(@_){
            Mouse::Util::load_class($super);
            my $meta = Mouse::Util::get_metaclass_by_name($super);
            next if $self->verify_superclass($super, $meta);
            $self->_reconcile_with_superclass_meta($meta);
        }
        return @{ $self->{superclasses} } = @_;
    }

    return @{ $self->{superclasses} };
}

sub verify_superclass {
    my($self, $super, $super_meta) = @_;

    if(defined $super_meta) {
        if(Mouse::Util::is_a_metarole($super_meta)){
            $self->throw_error("You cannot inherit from a Mouse Role ($super)");
        }
    }
    else {
        # The metaclass of $super is not initialized.
        # i.e. it might be Mouse::Object, a mixin package (e.g. Exporter),
        # or a foreign class including Moose classes.
        # See also Mouse::Foreign::Meta::Role::Class.
        my $mm = $super->can('meta');
        if(!($mm && $mm == \&Mouse::Util::meta)) {
            if($super->can('new') or $super->can('DESTROY')) {
                $self->inherit_from_foreign_class($super);
            }
        }
        return 1; # always ok
    }

    return $self->isa(ref $super_meta); # checks metaclass compatibility
}

sub inherit_from_foreign_class {
    my($class, $super) = @_;
    if($ENV{PERL_MOUSE_STRICT}) {
        Carp::carp("You inherit from non-Mouse class ($super),"
            . " but it is unlikely to work correctly."
            . " Please consider using MouseX::Foreign");
    }
    return;
}

my @MetaClassTypes = (
    'attribute',   # Mouse::Meta::Attribute
    'method',      # Mouse::Meta::Method
    'constructor', # Mouse::Meta::Method::Constructor
    'destructor',  # Mouse::Meta::Method::Destructor
);

sub _reconcile_with_superclass_meta {
    my($self, $other) = @_;

    # find incompatible traits
    my %metaroles;
    foreach my $metaclass_type(@MetaClassTypes){
        my $accessor = $self->can($metaclass_type . '_metaclass')
            || $self->can($metaclass_type . '_class');

        my $other_c = $other->$accessor();
        my $self_c  = $self->$accessor();

        if(!$self_c->isa($other_c)){
            $metaroles{$metaclass_type}
                = [ $self_c->meta->_collect_roles($other_c->meta) ];
        }
    }

    $metaroles{class} = [$self->meta->_collect_roles($other->meta)];

    #use Data::Dumper; print Data::Dumper->new([\%metaroles], ['*metaroles'])->Indent(1)->Dump;

    require Mouse::Util::MetaRole;
    $_[0] = Mouse::Util::MetaRole::apply_metaroles(
        for             => $self,
        class_metaroles => \%metaroles,
    );
    return;
}

sub _collect_roles {
    my ($self, $other) = @_;

    # find common ancestor
    my @self_lin_isa  = $self->linearized_isa;
    my @other_lin_isa = $other->linearized_isa;

    my(@self_anon_supers, @other_anon_supers);
    push @self_anon_supers,  shift @self_lin_isa  while $self_lin_isa[0]->meta->is_anon_class;
    push @other_anon_supers, shift @other_lin_isa while $other_lin_isa[0]->meta->is_anon_class;

    my $common_ancestor = $self_lin_isa[0] eq $other_lin_isa[0] && $self_lin_isa[0];

    if(!$common_ancestor){
        $self->throw_error(sprintf '%s cannot have %s as a super class because of their metaclass incompatibility',
            $self->name, $other->name);
    }

    my %seen;
    return sort grep { !$seen{$_}++ } ## no critic
        (map{ $_->name } map{ $_->meta->calculate_all_roles } @self_anon_supers),
        (map{ $_->name } map{ $_->meta->calculate_all_roles } @other_anon_supers),
    ;
}


sub find_method_by_name {
    my($self, $method_name) = @_;
    defined($method_name)
        or $self->throw_error('You must define a method name to find');

    foreach my $class( $self->linearized_isa ){
        my $method = $self->initialize($class)->get_method($method_name);
        return $method if defined $method;
    }
    return undef;
}

sub get_all_methods {
    my($self) = @_;
    return map{ $self->find_method_by_name($_) } $self->get_all_method_names;
}

sub get_all_method_names {
    my $self = shift;
    my %uniq;
    return grep { $uniq{$_}++ == 0 }
            map { Mouse::Meta::Class->initialize($_)->get_method_list() }
            $self->linearized_isa;
}

sub find_attribute_by_name {
    my($self, $name) = @_;
    defined($name)
        or $self->throw_error('You must define an attribute name to find');
    foreach my $attr($self->get_all_attributes) {
        return $attr if $attr->name eq $name;
    }
    return undef;
}

sub add_attribute {
    my $self = shift;

    my($attr, $name);

    if(Scalar::Util::blessed($_[0])){
        $attr = $_[0];

        $attr->isa('Mouse::Meta::Attribute')
            || $self->throw_error("Your attribute must be an instance of Mouse::Meta::Attribute (or a subclass)");

        $name = $attr->name;
    }
    else{
        # _process_attribute
        $name = shift;

        my %args = (@_ == 1) ? %{$_[0]} : @_;

        defined($name)
            or $self->throw_error('You must provide a name for the attribute');

        if ($name =~ s/^\+//) { # inherited attributes
            # Workaround for https://github.com/gfx/p5-Mouse/issues/64
            # Do not use find_attribute_by_name to avoid problems with cached attributes list
            # because we're about to change it anyway
            my $inherited_attr;
            foreach my $i ( @{ $self->_calculate_all_attributes } ) {
                if ( $i->name eq $name ) {
                    $inherited_attr = $i;
                    last;
                }
            }
            $self->throw_error("Could not find an attribute by the name of '$name' to inherit from in ".$self->name)
                unless $inherited_attr;

            $attr = $inherited_attr->clone_and_inherit_options(%args);
        }
        else{
            my($attribute_class, @traits) = $self->attribute_metaclass->interpolate_class(\%args);
            $args{traits} = \@traits if @traits;

            $attr = $attribute_class->new($name, %args);
        }
    }

    Scalar::Util::weaken( $attr->{associated_class} = $self );

    # install accessors first
    $attr->install_accessors();

    # then register the attribute to the metaclass
    $attr->{insertion_order}   = keys %{ $self->{attributes} };
    $self->{attributes}{$name} = $attr;
    $self->_invalidate_metaclass_cache();

    if(!$attr->{associated_methods} && ($attr->{is} || '') ne 'bare'){
        Carp::carp(qq{Attribute ($name) of class }.$self->name
            .qq{ has no associated methods (did you mean to provide an "is" argument?)});
    }
    return $attr;
}

sub _calculate_all_attributes {
    my($self) = @_;
    my %seen;
    my @all_attrs;
    foreach my $class($self->linearized_isa) {
        my $meta  = Mouse::Util::get_metaclass_by_name($class) or next;
        my @attrs = grep { !$seen{$_->name}++ } values %{$meta->{attributes}};
        @attrs = sort {
                $b->{insertion_order} <=> $a->{insertion_order}
            } @attrs;
        push @all_attrs, @attrs;
    }
    return [reverse @all_attrs];
}

sub linearized_isa;

sub new_object;
sub clone_object;

sub immutable_options {
    my ( $self, @args ) = @_;

    return (
        inline_constructor => 1,
        inline_destructor  => 1,
        constructor_name   => 'new',
        @args,
    );
}

sub make_immutable {
    my $self = shift;
    my %args = $self->immutable_options(@_);

    $self->{is_immutable}++;

    if ($args{inline_constructor}) {
        $self->add_method($args{constructor_name} =>
            Mouse::Util::load_class($self->constructor_class)
                ->_generate_constructor($self, \%args));
    }

    if ($args{inline_destructor}) {
        $self->add_method(DESTROY =>
            Mouse::Util::load_class($self->destructor_class)
                ->_generate_destructor($self, \%args));
    }

    # Moose's make_immutable returns true allowing calling code to skip
    # setting an explicit true value at the end of a source file.
    return 1;
}

sub make_mutable {
    my($self) = @_;
    $self->{is_immutable} = 0;
    return;
}

sub is_immutable;
sub is_mutable   { !$_[0]->is_immutable }

sub _install_modifier {
    my( $self, $type, $name, $code ) = @_;
    my $into = $self->name;

    my $original = $into->can($name)
        or $self->throw_error("The method '$name' was not found in the inheritance hierarchy for $into");

    my $modifier_table = $self->{modifiers}{$name};

    if(!$modifier_table){
        my(@before, @after, @around);
        my $cache = $original;
        my $modified = sub {
            if(@before) {
                for my $c (@before) { $c->(@_) }
            }
            unless(@after) {
                return $cache->(@_);
            }

            if(wantarray){ # list context
                my @rval = $cache->(@_);

                for my $c(@after){ $c->(@_) }
                return @rval;
            }
            elsif(defined wantarray){ # scalar context
                my $rval = $cache->(@_);

                for my $c(@after){ $c->(@_) }
                return $rval;
            }
            else{ # void context
                $cache->(@_);

                for my $c(@after){ $c->(@_) }
                return;
            }
        };

        $self->{modifiers}{$name} = $modifier_table = {
            original => $original,

            before   => \@before,
            after    => \@after,
            around   => \@around,

            cache    => \$cache, # cache for around modifiers
        };

        $self->add_method($name => $modified);
    }

    if($type eq 'before'){
        unshift @{$modifier_table->{before}}, $code;
    }
    elsif($type eq 'after'){
        push @{$modifier_table->{after}}, $code;
    }
    else{ # around
        push @{$modifier_table->{around}}, $code;

        my $next = ${ $modifier_table->{cache} };
        ${ $modifier_table->{cache} } = sub{ $code->($next, @_) };
    }

    return;
}

sub add_before_method_modifier {
    my ( $self, $name, $code ) = @_;
    $self->_install_modifier( 'before', $name, $code );
}

sub add_around_method_modifier {
    my ( $self, $name, $code ) = @_;
    $self->_install_modifier( 'around', $name, $code );
}

sub add_after_method_modifier {
    my ( $self, $name, $code ) = @_;
    $self->_install_modifier( 'after', $name, $code );
}

sub add_override_method_modifier {
    my ($self, $name, $code) = @_;

    if($self->has_method($name)){
        $self->throw_error("Cannot add an override method if a local method is already present");
    }

    my $package = $self->name;

    my $super_body = $package->can($name)
        or $self->throw_error("You cannot override '$name' because it has no super method");

    $self->add_method($name => sub {
        local $Mouse::SUPER_PACKAGE = $package;
        local $Mouse::SUPER_BODY    = $super_body;
        local @Mouse::SUPER_ARGS    = @_;
        &{$code};
    });
    return;
}

sub add_augment_method_modifier {
    my ($self, $name, $code) = @_;
    if($self->has_method($name)){
        $self->throw_error("Cannot add an augment method if a local method is already present");
    }

    my $super = $self->find_method_by_name($name)
        or $self->throw_error("You cannot augment '$name' because it has no super method");

    my $super_package = $super->package_name;
    my $super_body    = $super->body;

    $self->add_method($name => sub {
        local $Mouse::INNER_BODY{$super_package} = $code;
        local $Mouse::INNER_ARGS{$super_package} = [@_];
        &{$super_body};
    });
    return;
}

sub does_role {
    my ($self, $role_name) = @_;

    (defined $role_name)
        || $self->throw_error("You must supply a role name to look for");

    $role_name = $role_name->name if ref $role_name;

    for my $class ($self->linearized_isa) {
        my $meta = Mouse::Util::get_metaclass_by_name($class)
            or next;

        for my $role (@{ $meta->roles }) {

            return 1 if $role->does_role($role_name);
        }
    }

    return 0;
}

1;
__END__

=head1 NAME

Mouse::Meta::Class - The Mouse class metaclass

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 DESCRIPTION

This class is a meta object protocol for Mouse classes,
which is a subset of Moose::Meta:::Class.

=head1 METHODS

=head2 C<< initialize(ClassName) -> Mouse::Meta::Class >>

Finds or creates a C<Mouse::Meta::Class> instance for the given ClassName. Only
one instance should exist for a given class.

=head2 C<< name -> ClassName >>

Returns the name of the owner class.

=head2 C<< superclasses -> ClassNames >> C<< superclass(ClassNames) >>

Gets (or sets) the list of superclasses of the owner class.

=head2 C<< add_method(name => CodeRef) >>

Adds a method to the owner class.

=head2 C<< has_method(name) -> Bool >>

Returns whether we have a method with the given name.

=head2 C<< get_method(name) -> Mouse::Meta::Method | undef >>

Returns a L<Mouse::Meta::Method> with the given name.

Note that you can also use C<< $metaclass->name->can($name) >> for a method body.

=head2 C<< get_method_list -> Names >>

Returns a list of method names which are defined in the local class.
If you want a list of all applicable methods for a class, use the
C<get_all_methods> method.

=head2 C<< get_all_methods -> (Mouse::Meta::Method) >>

Return the list of all L<Mouse::Meta::Method> instances associated with
the class and its superclasses.

=head2 C<< add_attribute(name => spec | Mouse::Meta::Attribute) >>

Begins keeping track of the existing L<Mouse::Meta::Attribute> for the owner
class.

=head2 C<< has_attribute(Name) -> Bool >>

Returns whether we have a L<Mouse::Meta::Attribute> with the given name.

=head2 C<< get_attribute Name -> Mouse::Meta::Attribute | undef >>

Returns the L<Mouse::Meta::Attribute> with the given name.

=head2 C<< get_attribute_list -> Names >>

Returns a list of attribute names which are defined in the local
class. If you want a list of all applicable attributes for a class,
use the C<get_all_attributes> method.

=head2 C<< get_all_attributes -> (Mouse::Meta::Attribute) >>

Returns the list of all L<Mouse::Meta::Attribute> instances associated with
this class and its superclasses.

=head2 C<< linearized_isa -> [ClassNames] >>

Returns the list of classes in method dispatch order, with duplicates removed.

=head2 C<< new_object(Parameters) -> Instance >>

Creates a new instance.

=head2 C<< clone_object(Instance, Parameters) -> Instance >>

Clones the given instance which must be an instance governed by this
metaclass.

=head2 C<< throw_error(Message, Parameters) >>

Throws an error with the given message.

=head1 SEE ALSO

L<Mouse::Meta::Module>

L<Moose::Meta::Class>

L<Class::MOP::Class>

=cut

