package Mouse::PurePerl;
# The pure Perl backend for Mouse
package Mouse::Util;
use strict;
use warnings;
use warnings FATAL => 'redefine'; # to avoid to load Mouse::PurePerl twice

use Scalar::Util ();
use B ();

require Mouse::Util;

# taken from Class/MOP.pm
sub is_valid_class_name {
    my $class = shift;

    return 0 if ref($class);
    return 0 unless defined($class);

    return 1 if $class =~ /\A \w+ (?: :: \w+ )* \z/xms;

    return 0;
}

sub is_class_loaded {
    my $class = shift;

    return 0 if ref($class) || !defined($class) || !length($class);

    # walk the symbol table tree to avoid autovififying
    # \*{${main::}{"Foo::"}{"Bar::"}} == \*main::Foo::Bar::

    my $pack = \%::;
    foreach my $part (split('::', $class)) {
        $part .= '::';
        return 0 if !exists $pack->{$part};

        my $entry = \$pack->{$part};
        return 0 if ref($entry) ne 'GLOB';
        $pack = *{$entry}{HASH};
    }

    return 0 if !%{$pack};

    # check for $VERSION or @ISA
    return 1 if exists $pack->{VERSION}
             && defined *{$pack->{VERSION}}{SCALAR} && defined ${ $pack->{VERSION} };
    return 1 if exists $pack->{ISA}
             && defined *{$pack->{ISA}}{ARRAY} && @{ $pack->{ISA} } != 0;

    # check for any method
    foreach my $name( keys %{$pack} ) {
        my $entry = \$pack->{$name};
        return 1 if ref($entry) ne 'GLOB' || defined *{$entry}{CODE};
    }

    # fail
    return 0;
}


# taken from Sub::Identify
sub get_code_info {
    my ($coderef) = @_;
    ref($coderef) or return;

    my $cv = B::svref_2object($coderef);
    $cv->isa('B::CV') or return;

    my $gv = $cv->GV;
    $gv->isa('B::GV') or return;

    return ($gv->STASH->NAME, $gv->NAME);
}

sub get_code_package{
    my($coderef) = @_;

    my $cv = B::svref_2object($coderef);
    $cv->isa('B::CV') or return '';

    my $gv = $cv->GV;
    $gv->isa('B::GV') or return '';

    return $gv->STASH->NAME;
}

sub get_code_ref{
    my($package, $name) = @_;
    no strict 'refs';
    no warnings 'once';
    use warnings FATAL => 'uninitialized';
    return *{$package . '::' . $name}{CODE};
}

sub generate_isa_predicate_for {
    my($for_class, $name) = @_;

    my $predicate = sub{ Scalar::Util::blessed($_[0]) && $_[0]->isa($for_class) };

    if(defined $name){
        Mouse::Util::install_subroutines(scalar caller, $name => $predicate);
        return;
    }

    return $predicate;
}

sub generate_can_predicate_for {
    my($methods_ref, $name) = @_;

    my @methods = @{$methods_ref};

    my $predicate = sub{
        my($instance) = @_;
        if(Scalar::Util::blessed($instance)){
            foreach my $method(@methods){
                if(!$instance->can($method)){
                    return 0;
                }
            }
            return 1;
        }
        return 0;
    };

    if(defined $name){
        Mouse::Util::install_subroutines(scalar caller, $name => $predicate);
        return;
    }

    return $predicate;
}

package Mouse::Util::TypeConstraints;


sub Any        { 1 }
sub Item       { 1 }

sub Bool       { !$_[0] || $_[0] eq '1' }
sub Undef      { !defined($_[0]) }
sub Defined    {  defined($_[0])  }
sub Value      {  defined($_[0]) && !ref($_[0]) }
sub Num        {  Scalar::Util::looks_like_number($_[0]) }
sub Str        {
    # We need to use a copy here to flatten MAGICs, for instance as in
    # Str( substr($_, 0, 42) ).
    my($value) = @_;
    return defined($value) && ref(\$value) eq 'SCALAR';
}
sub Int        {
    # We need to use a copy here to save the original internal SV flags.
    my($value) = @_;
    return defined($value) && $value =~ /\A -? [0-9]+  \z/xms;
}

sub Ref        { ref($_[0]) }
sub ScalarRef  {
    my($value) = @_;
    return ref($value) eq 'SCALAR' || ref($value) eq 'REF';
}
sub ArrayRef   { ref($_[0]) eq 'ARRAY'  }
sub HashRef    { ref($_[0]) eq 'HASH'   }
sub CodeRef    { ref($_[0]) eq 'CODE'   }
sub RegexpRef  { ref($_[0]) eq 'Regexp' }
sub GlobRef    { ref($_[0]) eq 'GLOB'   }

sub FileHandle {
    my($value) = @_;
    return Scalar::Util::openhandle($value)
        || (Scalar::Util::blessed($value) && $value->isa("IO::Handle"))
}

sub Object     { Scalar::Util::blessed($_[0]) && ref($_[0]) ne 'Regexp' }

sub ClassName  { Mouse::Util::is_class_loaded($_[0]) }
sub RoleName   { (Mouse::Util::class_of($_[0]) || return 0)->isa('Mouse::Meta::Role') }

sub _parameterize_ArrayRef_for {
    my($type_parameter) = @_;
    my $check = $type_parameter->_compiled_type_constraint;

    return sub {
        foreach my $value (@{$_}) {
            return undef unless $check->($value);
        }
        return 1;
    }
}

sub _parameterize_HashRef_for {
    my($type_parameter) = @_;
    my $check = $type_parameter->_compiled_type_constraint;

    return sub {
        foreach my $value(values %{$_}){
            return undef unless $check->($value);
        }
        return 1;
    };
}

# 'Maybe' type accepts 'Any', so it requires parameters
sub _parameterize_Maybe_for {
    my($type_parameter) = @_;
    my $check = $type_parameter->_compiled_type_constraint;

    return sub{
        return !defined($_) || $check->($_);
    };
}

package Mouse::Meta::Module;

sub name          { $_[0]->{package} }

sub _method_map   { $_[0]->{methods} }
sub _attribute_map{ $_[0]->{attributes} }

sub namespace{
    my $name = $_[0]->{package};
    no strict 'refs';
    return \%{ $name . '::' };
}

sub add_method {
    my($self, $name, $code) = @_;

    if(!defined $name){
        $self->throw_error('You must pass a defined name');
    }
    if(!defined $code){
        $self->throw_error('You must pass a defined code');
    }

    if(ref($code) ne 'CODE'){
        $code = \&{$code}; # coerce
    }

    $self->{methods}->{$name} = $code; # Moose stores meta object here.

    Mouse::Util::install_subroutines($self->name,
        $name => $code,
    );
    return;
}

my $generate_class_accessor = sub {
    my($name) = @_;
    return sub {
        my $self = shift;
        if(@_) {
            return $self->{$name} = shift;
        }

        foreach my $class($self->linearized_isa) {
            my $meta = Mouse::Util::get_metaclass_by_name($class)
                or next;

            if(exists $meta->{$name}) {
                return $meta->{$name};
            }
        }
        return undef;
    };
};


package Mouse::Meta::Class;

use Mouse::Meta::Method::Constructor;
use Mouse::Meta::Method::Destructor;

sub method_metaclass    { $_[0]->{method_metaclass}    || 'Mouse::Meta::Method'    }
sub attribute_metaclass { $_[0]->{attribute_metaclass} || 'Mouse::Meta::Attribute' }

sub constructor_class { $_[0]->{constructor_class} || 'Mouse::Meta::Method::Constructor' }
sub destructor_class  { $_[0]->{destructor_class}  || 'Mouse::Meta::Method::Destructor'  }

sub is_anon_class{
    return exists $_[0]->{anon_serial_id};
}

sub roles { $_[0]->{roles} }

sub linearized_isa { @{ Mouse::Util::get_linear_isa($_[0]->{package}) } }

sub new_object {
    my $meta = shift;
    my %args = (@_ == 1 ? %{$_[0]} : @_);

    my $object = bless {}, $meta->name;

    $meta->_initialize_object($object, \%args, 0);
    # BUILDALL
    if( $object->can('BUILD') ) {
        for my $class (reverse $meta->linearized_isa) {
            my $build = Mouse::Util::get_code_ref($class, 'BUILD')
                || next;

            $object->$build(\%args);
        }
    }
    return $object;
}

sub clone_object {
    my $class  = shift;
    my $object = shift;
    my $args   = $object->Mouse::Object::BUILDARGS(@_);

    (Scalar::Util::blessed($object) && $object->isa($class->name))
        || $class->throw_error("You must pass an instance of the metaclass (" . $class->name . "), not ($object)");

    my $cloned = bless { %$object }, ref $object;
    $class->_initialize_object($cloned, $args, 1);
    return $cloned;
}

sub _initialize_object{
    my($self, $object, $args, $is_cloning) = @_;
    # The initializer, which is used everywhere, must be clear
    # when an attribute is added. See Mouse::Meta::Class::add_attribute.
    my $initializer = $self->{_mouse_cache}{_initialize_object} ||=
        Mouse::Util::load_class($self->constructor_class)
            ->_generate_initialize_object($self);
    goto &{$initializer};
}

sub get_all_attributes {
    my($self) = @_;
    return @{ $self->{_mouse_cache}{all_attributes}
        ||= $self->_calculate_all_attributes };
}

sub is_immutable {  $_[0]->{is_immutable} }

sub strict_constructor;
*strict_constructor = $generate_class_accessor->('strict_constructor');

sub _invalidate_metaclass_cache {
    my($self) = @_;
    delete $self->{_mouse_cache};
    return;
}

sub _report_unknown_args {
    my($metaclass, $attrs, $args) = @_;

    my @unknowns;
    my %init_args;
    foreach my $attr(@{$attrs}){
        my $init_arg = $attr->init_arg;
        if(defined $init_arg){
            $init_args{$init_arg}++;
        }
    }

    while(my $key = each %{$args}){
        if(!exists $init_args{$key}){
            push @unknowns, $key;
        }
    }

    $metaclass->throw_error( sprintf
        "Unknown attribute passed to the constructor of %s: %s",
        $metaclass->name, Mouse::Util::english_list(@unknowns),
    );
}

package Mouse::Meta::Role;

sub method_metaclass{ $_[0]->{method_metaclass} || 'Mouse::Meta::Role::Method' }

sub is_anon_role{
    return exists $_[0]->{anon_serial_id};
}

sub get_roles { $_[0]->{roles} }

sub add_before_method_modifier {
    my ($self, $method_name, $method) = @_;

    push @{ $self->{before_method_modifiers}{$method_name} ||= [] }, $method;
    return;
}
sub add_around_method_modifier {
    my ($self, $method_name, $method) = @_;

    push @{ $self->{around_method_modifiers}{$method_name} ||= [] }, $method;
    return;
}
sub add_after_method_modifier {
    my ($self, $method_name, $method) = @_;

    push @{ $self->{after_method_modifiers}{$method_name} ||= [] }, $method;
    return;
}

sub get_before_method_modifiers {
    my ($self, $method_name) = @_;
    return @{ $self->{before_method_modifiers}{$method_name} ||= [] }
}
sub get_around_method_modifiers {
    my ($self, $method_name) = @_;
    return @{ $self->{around_method_modifiers}{$method_name} ||= [] }
}
sub get_after_method_modifiers {
    my ($self, $method_name) = @_;
    return @{ $self->{after_method_modifiers}{$method_name} ||= [] }
}

sub add_metaclass_accessor { # for meta roles (a.k.a. traits)
    my($meta, $name) = @_;
    $meta->add_method($name => $generate_class_accessor->($name));
    return;
}

package Mouse::Meta::Attribute;

require Mouse::Meta::Method::Accessor;

sub accessor_metaclass{ $_[0]->{accessor_metaclass} || 'Mouse::Meta::Method::Accessor' }

# readers

sub name                 { $_[0]->{name}                   }
sub associated_class     { $_[0]->{associated_class}       }

sub accessor             { $_[0]->{accessor}               }
sub reader               { $_[0]->{reader}                 }
sub writer               { $_[0]->{writer}                 }
sub predicate            { $_[0]->{predicate}              }
sub clearer              { $_[0]->{clearer}                }
sub handles              { $_[0]->{handles}                }

sub _is_metadata         { $_[0]->{is}                     }
sub is_required          { $_[0]->{required}               }
sub default {
    my($self, $instance) = @_;
    my $value = $self->{default};
    $value = $value->($instance) if defined($instance) and ref($value) eq "CODE";
    return $value;
}
sub is_lazy              { $_[0]->{lazy}                   }
sub is_lazy_build        { $_[0]->{lazy_build}             }
sub is_weak_ref          { $_[0]->{weak_ref}               }
sub init_arg             { $_[0]->{init_arg}               }
sub type_constraint      { $_[0]->{type_constraint}        }

sub trigger              { $_[0]->{trigger}                }
sub builder              { $_[0]->{builder}                }
sub should_auto_deref    { $_[0]->{auto_deref}             }
sub should_coerce        { $_[0]->{coerce}                 }

sub documentation        { $_[0]->{documentation}          }
sub insertion_order      { $_[0]->{insertion_order}        }

# predicates

sub has_accessor         { exists $_[0]->{accessor}        }
sub has_reader           { exists $_[0]->{reader}          }
sub has_writer           { exists $_[0]->{writer}          }
sub has_predicate        { exists $_[0]->{predicate}       }
sub has_clearer          { exists $_[0]->{clearer}         }
sub has_handles          { exists $_[0]->{handles}         }

sub has_default          { exists $_[0]->{default}         }
sub has_type_constraint  { exists $_[0]->{type_constraint} }
sub has_trigger          { exists $_[0]->{trigger}         }
sub has_builder          { exists $_[0]->{builder}         }

sub has_documentation    { exists $_[0]->{documentation}   }

sub _process_options{
    my($class, $name, $args) = @_;

    # taken from Class::MOP::Attribute::new

    defined($name)
        or $class->throw_error('You must provide a name for the attribute');

    if(!exists $args->{init_arg}){
        $args->{init_arg} = $name;
    }

    # 'required' requires either 'init_arg', 'builder', or 'default'
    my $can_be_required = defined( $args->{init_arg} );

    if(exists $args->{builder}){
        # XXX:
        # Moose refuses a CODE ref builder, but Mouse doesn't for backward compatibility
        # This feature will be changed in a future. (gfx)
        $class->throw_error('builder must be a defined scalar value which is a method name')
            #if ref $args->{builder} || !defined $args->{builder};
            if !defined $args->{builder};

        $can_be_required++;
    }
    elsif(exists $args->{default}){
        if(ref $args->{default} && ref($args->{default}) ne 'CODE'){
            $class->throw_error("References are not allowed as default values, you must "
                              . "wrap the default of '$name' in a CODE reference (ex: sub { [] } and not [])");
        }
        $can_be_required++;
    }

    if( $args->{required} && !$can_be_required ) {
        $class->throw_error("You cannot have a required attribute ($name) without a default, builder, or an init_arg");
    }

    # taken from Mouse::Meta::Attribute->new and ->_process_args

    if(exists $args->{is}){
        my $is = $args->{is};

        if($is eq 'ro'){
            $args->{reader} ||= $name;
        }
        elsif($is eq 'rw'){
            if(exists $args->{writer}){
                $args->{reader} ||= $name;
             }
             else{
                $args->{accessor} ||= $name;
             }
        }
        elsif($is eq 'bare'){
            # do nothing, but don't complain (later) about missing methods
        }
        else{
            $is = 'undef' if !defined $is;
            $class->throw_error("I do not understand this option (is => $is) on attribute ($name)");
        }
    }

    my $tc;
    if(exists $args->{isa}){
        $tc = $args->{type_constraint} = Mouse::Util::TypeConstraints::find_or_create_isa_type_constraint($args->{isa});
    }

    if(exists $args->{does}){
        if(defined $tc){ # both isa and does supplied
            my $does_ok = do{
                local $@;
                eval{ "$tc"->does($args->{does}) };
            };
            if(!$does_ok){
                $class->throw_error("Cannot have both an isa option and a does option because '$tc' does not do '$args->{does}' on attribute ($name)");
            }
        }
        else {
            $tc = $args->{type_constraint} = Mouse::Util::TypeConstraints::find_or_create_does_type_constraint($args->{does});
        }
    }

    if($args->{coerce}){
        defined($tc)
            || $class->throw_error("You cannot have coercion without specifying a type constraint on attribute ($name)");

        $args->{weak_ref}
            && $class->throw_error("You cannot have a weak reference to a coerced value on attribute ($name)");
    }

    if ($args->{lazy_build}) {
        exists($args->{default})
            && $class->throw_error("You can not use lazy_build and default for the same attribute ($name)");

        $args->{lazy}      = 1;
        $args->{builder} ||= "_build_${name}";
        if ($name =~ /^_/) {
            $args->{clearer}   ||= "_clear${name}";
            $args->{predicate} ||= "_has${name}";
        }
        else {
            $args->{clearer}   ||= "clear_${name}";
            $args->{predicate} ||= "has_${name}";
        }
    }

    if ($args->{auto_deref}) {
        defined($tc)
            || $class->throw_error("You cannot auto-dereference without specifying a type constraint on attribute ($name)");

        ( $tc->is_a_type_of('ArrayRef') || $tc->is_a_type_of('HashRef') )
            || $class->throw_error("You cannot auto-dereference anything other than a ArrayRef or HashRef on attribute ($name)");
    }

    if (exists $args->{trigger}) {
        ('CODE' eq ref $args->{trigger})
            || $class->throw_error("Trigger must be a CODE ref on attribute ($name)");
    }

    if ($args->{lazy}) {
        (exists $args->{default} || defined $args->{builder})
            || $class->throw_error("You cannot have a lazy attribute ($name) without specifying a default value for it");
    }

    return;
}


package Mouse::Meta::TypeConstraint;

use overload
    '""' => '_as_string',
    '0+' => '_identity',
    '|'  => '_unite',

    fallback => 1;

sub name    { $_[0]->{name}    }
sub parent  { $_[0]->{parent}  }
sub message { $_[0]->{message} }

sub _identity  { Scalar::Util::refaddr($_[0]) } # overload 0+

sub type_parameter           { $_[0]->{type_parameter} }
sub _compiled_type_constraint{ $_[0]->{compiled_type_constraint} }

sub __is_parameterized { exists $_[0]->{type_parameter} }
sub has_coercion {       exists $_[0]->{_compiled_type_coercion} }


sub compile_type_constraint{
    my($self) = @_;

    # add parents first
    my @checks;
    for(my $parent = $self->{parent}; defined $parent; $parent = $parent->{parent}){
         if($parent->{hand_optimized_type_constraint}){
            unshift @checks, $parent->{hand_optimized_type_constraint};
            last; # a hand optimized constraint must include all the parents
        }
        elsif($parent->{constraint}){
            unshift @checks, $parent->{constraint};
        }
    }

    # then add child
    if($self->{constraint}){
        push @checks, $self->{constraint};
    }

    if($self->{type_constraints}){ # Union
        my @types = map{ $_->{compiled_type_constraint} } @{ $self->{type_constraints} };
        push @checks, sub{
            foreach my $c(@types){
                return 1 if $c->($_[0]);
            }
            return 0;
        };
    }

    if(@checks == 0){
        $self->{compiled_type_constraint} = \&Mouse::Util::TypeConstraints::Any;
    }
    else{
        $self->{compiled_type_constraint} =  sub{
          my(@args) = @_;
          for ($args[0]) { # local $_ will cancel tie-ness due to perl's bug
              foreach my $c(@checks){
                  return undef if !$c->(@args);
              }
          }
          return 1;
        };
    }
    return;
}

sub check {
    my $self = shift;
    return $self->_compiled_type_constraint->(@_);
}


package Mouse::Object;

sub BUILDARGS {
    my $class = shift;

    if (scalar @_ == 1) {
        (ref($_[0]) eq 'HASH')
            || $class->meta->throw_error("Single parameters to new() must be a HASH ref");

        return {%{$_[0]}};
    }
    else {
        return {@_};
    }
}

sub new {
    my $class = shift;
    my $args  = $class->BUILDARGS(@_);
    return $class->meta->new_object($args);
}

sub DESTROY {
    my $self = shift;

    return unless $self->can('DEMOLISH'); # short circuit

    my $e = do{
        local $?;
        local $@;
        eval{
            # DEMOLISHALL

            # We cannot count on being able to retrieve a previously made
            # metaclass, _or_ being able to make a new one during global
            # destruction. However, we should still be able to use mro at
            # that time (at least tests suggest so ;)

            foreach my $class (@{ Mouse::Util::get_linear_isa(ref $self) }) {
                my $demolish = Mouse::Util::get_code_ref($class, 'DEMOLISH')
                    || next;

                $self->$demolish(Mouse::Util::in_global_destruction());
            }
        };
        $@;
    };

    no warnings 'misc';
    die $e if $e; # rethrow
}

sub BUILDALL {
    my $self = shift;

    # short circuit
    return unless $self->can('BUILD');

    for my $class (reverse $self->meta->linearized_isa) {
        my $build = Mouse::Util::get_code_ref($class, 'BUILD')
            || next;

        $self->$build(@_);
    }
    return;
}

sub DEMOLISHALL;
*DEMOLISHALL = \&DESTROY;

1;
__END__

=head1 NAME

Mouse::PurePerl - A Mouse guts in pure Perl

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SEE ALSO

L<Mouse::XS>

=cut
