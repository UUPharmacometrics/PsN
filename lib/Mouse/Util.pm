package Mouse::Util;
use Mouse::Exporter; # enables strict and warnings

# Note that those which don't exist here are defined in XS or Mouse::PurePerl

# must be here because it will be referred by other modules loaded
sub get_linear_isa($;$); ## no critic

# must be here because it will called in Mouse::Exporter
sub install_subroutines {
    my $into = shift;

    while(my($name, $code) = splice @_, 0, 2){
        no strict 'refs';
        no warnings 'once', 'redefine';
        use warnings FATAL => 'uninitialized';
        *{$into . '::' . $name} = \&{$code};
    }
    return;
}

BEGIN{
    # This is used in Mouse::PurePerl
    Mouse::Exporter->setup_import_methods(
        as_is => [qw(
            find_meta
            does_role
            resolve_metaclass_alias
            apply_all_roles
            english_list

            load_class
            is_class_loaded

            get_linear_isa
            get_code_info

            get_code_package
            get_code_ref

            not_supported

            does meta throw_error dump
        )],
        groups => {
            default => [], # export no functions by default

            # The ':meta' group is 'use metaclass' for Mouse
            meta    => [qw(does meta dump throw_error)],
        },
    );

    use version; our $VERSION = version->declare('v2.5.10');

    my $xs = !(defined(&is_valid_class_name) || $ENV{MOUSE_PUREPERL} || $ENV{PERL_ONLY});

    # Because Mouse::Util is loaded first in all the Mouse sub-modules,
    # XSLoader must be placed here, not in Mouse.pm.
    if($xs){
        # XXX: XSLoader tries to get the object path from caller's file name
        #      $hack_mouse_file fools its mechanism
        (my $hack_mouse_file = __FILE__) =~ s/.Util//; # .../Mouse/Util.pm -> .../Mouse.pm
        $xs = eval sprintf("#line %d %s\n", __LINE__, $hack_mouse_file) . q{
            local $^W = 0; # workaround 'redefine' warning to &install_subroutines
            no warnings 'redefine';
            require XSLoader;
            XSLoader::load('Mouse', $VERSION);
            Mouse::Util->import({ into => 'Mouse::Meta::Method::Constructor::XS' }, ':meta');
            Mouse::Util->import({ into => 'Mouse::Meta::Method::Destructor::XS'  }, ':meta');
            Mouse::Util->import({ into => 'Mouse::Meta::Method::Accessor::XS'    }, ':meta');
            return 1;
        } || 0;
        warn $@ if $@ && $ENV{MOUSE_XS};
    }

    if(!$xs){
        require 'Mouse/PurePerl.pm'; # we don't want to create its namespace
    }

    {
        my $value = $xs; # avoid "Constants from lexical variables potentially modified elsewhere are deprecated"
        *MOUSE_XS = sub(){ $value };
    }

    # definition of mro::get_linear_isa()
    my $get_linear_isa;
    if ($] >= 5.010_000) {
        require 'mro.pm';
        $get_linear_isa = \&mro::get_linear_isa;
    }
    else {
        # this code is based on MRO::Compat::__get_linear_isa
        my $_get_linear_isa_dfs; # this recurses so it isn't pretty
        $_get_linear_isa_dfs = sub {
            my($classname) = @_;

            my @lin = ($classname);
            my %stored;

            no strict 'refs';
            foreach my $parent (@{"$classname\::ISA"}) {
                foreach  my $p(@{ $_get_linear_isa_dfs->($parent) }) {
                    next if exists $stored{$p};
                    push(@lin, $p);
                    $stored{$p} = 1;
                }
            }
            return \@lin;
        };

        {
            package # hide from PAUSE
                Class::C3;
            our %MRO; # avoid 'once' warnings
        }

        # MRO::Compat::__get_linear_isa has no prototype, so
        # we define a prototyped version for compatibility with core's
        # See also MRO::Compat::__get_linear_isa.
        $get_linear_isa = sub ($;$){
            my($classname, $type) = @_;

            if(!defined $type){
                $type = exists $Class::C3::MRO{$classname} ? 'c3' : 'dfs';
            }
            if($type eq 'c3'){
                require Class::C3;
                return [Class::C3::calculateMRO($classname)];
            }
            else{
                return $_get_linear_isa_dfs->($classname);
            }
        };
    }

    *get_linear_isa = $get_linear_isa;
}

use Carp         ();
use Scalar::Util ();

# aliases as public APIs
# it must be 'require', not 'use', because Mouse::Meta::Module depends on Mouse::Util
require Mouse::Meta::Module; # for the entities of metaclass cache utilities

# aliases
{
    *class_of                    = \&Mouse::Meta::Module::_class_of;
    *get_metaclass_by_name       = \&Mouse::Meta::Module::_get_metaclass_by_name;
    *get_all_metaclass_instances = \&Mouse::Meta::Module::_get_all_metaclass_instances;
    *get_all_metaclass_names     = \&Mouse::Meta::Module::_get_all_metaclass_names;

    *Mouse::load_class           = \&load_class;
    *Mouse::is_class_loaded      = \&is_class_loaded;

    # is-a predicates
    #generate_isa_predicate_for('Mouse::Meta::TypeConstraint' => 'is_a_type_constraint');
    #generate_isa_predicate_for('Mouse::Meta::Class'          => 'is_a_metaclass');
    #generate_isa_predicate_for('Mouse::Meta::Role'           => 'is_a_metarole');

    # duck type predicates
    generate_can_predicate_for(['_compiled_type_constraint']  => 'is_a_type_constraint');
    generate_can_predicate_for(['create_anon_class']          => 'is_a_metaclass');
    generate_can_predicate_for(['create_anon_role']           => 'is_a_metarole');
}

sub in_global_destruction;

if (defined ${^GLOBAL_PHASE}) {
    *in_global_destruction = sub {
        return ${^GLOBAL_PHASE} eq 'DESTRUCT';
    };
}
else {
    my $in_global_destruction = 0;
    END { $in_global_destruction = 1 }
    *in_global_destruction = sub {
        return $in_global_destruction;
    };
}

# Moose::Util compatible utilities

sub find_meta{
    return class_of( $_[0] );
}

sub _does_role_impl {
    my ($class_or_obj, $role_name) = @_;

    my $meta = class_of($class_or_obj);

    (defined $role_name)
        || ($meta || 'Mouse::Meta::Class')->throw_error("You must supply a role name to does()");

    return defined($meta) && $meta->does_role($role_name);
}

sub does_role {
    my($thing, $role_name) = @_;

    if( (Scalar::Util::blessed($thing) || is_class_loaded($thing))
            && $thing->can('does')) {
        return $thing->does($role_name);
    }
    goto &_does_role_impl;
}

# taken from Mouse::Util (0.90)
{
    my %cache;

    sub resolve_metaclass_alias {
        my ( $type, $metaclass_name, %options ) = @_;

        my $cache_key = $type . q{ } . ( $options{trait} ? '-Trait' : '' );

        return $cache{$cache_key}{$metaclass_name} ||= do{

            my $possible_full_name = join '::',
                'Mouse::Meta', $type, 'Custom', ($options{trait} ? 'Trait' : ()), $metaclass_name
            ;

            my $loaded_class = load_first_existing_class(
                $possible_full_name,
                $metaclass_name
            );

            $loaded_class->can('register_implementation')
                ? $loaded_class->register_implementation
                : $loaded_class;
        };
    }
}

# Taken from Module::Runtime
sub module_notional_filename {
    my $class = shift;

    $class =~ s{::}{/}g;

    return $class.'.pm';
}

# Utilities from Class::MOP

sub get_code_info;
sub get_code_package;

sub is_valid_class_name;
sub is_class_loaded;

# taken from Class/MOP.pm
sub load_first_existing_class {
    my @classes = @_
      or return;

    my %exceptions;
    for my $class (@classes) {
        my $e = _try_load_one_class($class);

        if ($e) {
            $exceptions{$class} = $e;
        }
        else {
            return $class;
        }
    }

    # not found
    Carp::confess join(
        "\n",
        map {
            sprintf( "Could not load class (%s) because : %s",
                $_, $exceptions{$_} )
          } @classes
    );
}

# taken from Class/MOP.pm
sub _try_load_one_class {
    my $class = shift;

    unless ( is_valid_class_name($class) ) {
        my $display = defined($class) ? $class : 'undef';
        Carp::confess "Invalid class name ($display)";
    }

    return '' if is_class_loaded($class);

    my $filename = module_notional_filename($class);

    return do {
        local $@;
        eval { require $filename };
        $@;
    };
}


sub load_class {
    my $class = shift;
    my $e = _try_load_one_class($class);
    Carp::confess "Could not load class ($class) because : $e" if $e;

    return $class;
}


sub apply_all_roles {
    my $consumer = Scalar::Util::blessed($_[0])
        ?                                $_[0]   # instance
        : Mouse::Meta::Class->initialize($_[0]); # class or role name

    my @roles;

    # Basis of Data::OptList
    my $max = scalar(@_);
    for (my $i = 1; $i < $max ; $i++) {
        my $role = $_[$i];
        my $role_name;
        if(ref $role) {
            $role_name = $role->name;
        }
        else {
            $role_name = $role;
            load_class($role_name);
            $role = get_metaclass_by_name($role_name);
        }

        if ($i + 1 < $max && ref($_[$i + 1]) eq 'HASH') {
            push @roles, [ $role => $_[++$i] ];
        } else {
            push @roles, [ $role => undef ];
        }
        is_a_metarole($role)
            || $consumer->meta->throw_error("You can only consume roles, $role_name is not a Mouse role");
    }

    if ( scalar @roles == 1 ) {
        my ( $role, $params ) = @{ $roles[0] };
        $role->apply( $consumer, defined $params ? $params : () );
    }
    else {
        Mouse::Meta::Role->combine(@roles)->apply($consumer);
    }
    return;
}

# taken from Moose::Util 0.90
sub english_list {
    return $_[0] if @_ == 1;

    my @items = sort @_;

    return "$items[0] and $items[1]" if @items == 2;

    my $tail = pop @items;

    return join q{, }, @items, "and $tail";
}

sub quoted_english_list {
    return english_list(map { qq{'$_'} } @_);
}

# common utilities

sub not_supported{
    my($feature) = @_;

    $feature ||= ( caller(1) )[3] . '()'; # subroutine name

    local $Carp::CarpLevel = $Carp::CarpLevel + 1;
    Carp::confess("Mouse does not currently support $feature");
}

# general meta() method
sub meta :method{
    return Mouse::Meta::Class->initialize(ref($_[0]) || $_[0]);
}

# general throw_error() method
# $o->throw_error($msg, depth => $leve, longmess => $croak_or_confess)
sub throw_error :method {
    my($self, $message, %args) = @_;

    local $Carp::CarpLevel  = $Carp::CarpLevel + 1 + ($args{depth} || 0);
    local $Carp::MaxArgNums = 20; # default is 8, usually we use named args which gets messier though

    if(exists $args{longmess} && !$args{longmess}) {
        Carp::croak($message);
    }
    else{
        Carp::confess($message);
    }
}

# general dump() method
sub dump :method {
    my($self, $maxdepth) = @_;

    require 'Data/Dumper.pm'; # we don't want to create its namespace
    my $dd = Data::Dumper->new([$self]);
    $dd->Maxdepth(defined($maxdepth) ? $maxdepth : 3);
    $dd->Indent(1);
    $dd->Sortkeys(1);
    $dd->Quotekeys(0);
    return $dd->Dump();
}

# general does() method
sub does :method {
    goto &_does_role_impl;
}

1;
__END__

=head1 NAME

Mouse::Util - Utilities for working with Mouse classes

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SYNOPSIS

    use Mouse::Util; # turns on strict and warnings

=head1 DESCRIPTION

This module provides a set of utility functions. Many of these
functions are intended for use in Mouse itself or MouseX modules, but
some of them may be useful for use in your own code.

=head1 IMPLEMENTATIONS FOR

=head2 Moose::Util functions

The following functions are exportable.

=head3 C<find_meta($class_or_obj)>

The same as C<Mouse::Util::class_of()>.

=head3 C<does_role($class_or_obj, $role_or_obj)>

=head3 C<resolve_metaclass_alias($category, $name, %options)>

=head3 C<apply_all_roles($applicant, @roles)>

=head3 C<english_listi(@items)>

=head2 Class::MOP functions

The following functions are not exportable.

=head3 C<< Mouse::Util::is_class_loaded($classname) -> Bool >>

Returns whether I<$classname> is actually loaded or not.
It uses a heuristic which involves checking for the existence of
C<$VERSION>, C<@ISA>, and any locally-defined method.

=head3 C<< Mouse::Util::load_class($classname) -> ClassName >>

This will load a given I<$classname> (or die if it is not loadable).
This function can be used in place of tricks like
C<eval "use $module ()"> or using C<require>.

=head3 C<< Mouse::Util::class_of($classname_or_object) -> MetaClass >>

=head3 C<< Mouse::Util::get_metaclass_by_name($classname) -> MetaClass >>

=head3 C<< Mouse::Util::get_all_metaclass_instances() -> (MetaClasses) >>

=head3 C<< Mouse::Util::get_all_metaclass_names() -> (ClassNames) >>

=head2 mro (or MRO::Compat)

=head3 C<get_linear_isa>

=head2 Sub::Identify

=head3 C<get_code_info>

=head1 Mouse specific utilities

=head3 C<not_supported>

=head3 C<get_code_package>

=head3 C<get_code_ref>

=head1 SEE ALSO

L<Moose::Util>

L<Class::MOP>

L<Sub::Identify>

L<mro>

L<MRO::Compat>

=cut

