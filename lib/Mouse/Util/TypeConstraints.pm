package Mouse::Util::TypeConstraints;
use Mouse::Util; # enables strict and warnings

use Mouse::Meta::TypeConstraint;
use Mouse::Exporter;

use Carp         ();
use Scalar::Util ();

Mouse::Exporter->setup_import_methods(
    as_is => [qw(
        as where message optimize_as
        from via

        type subtype class_type role_type maybe_type duck_type
        enum
        coerce

        find_type_constraint
        register_type_constraint
    )],
);

our @CARP_NOT = qw(Mouse::Meta::Attribute);

my %TYPE;

# The root type
$TYPE{Any} = Mouse::Meta::TypeConstraint->new(
    name => 'Any',
);

my @builtins = (
    # $name    => $parent,   $code,

    # the base type
    Item       => 'Any',     undef,

    # the maybe[] type
    Maybe      => 'Item',    undef,

    # value types
    Undef      => 'Item',    \&Undef,
    Defined    => 'Item',    \&Defined,
    Bool       => 'Item',    \&Bool,
    Value      => 'Defined', \&Value,
    Str        => 'Value',   \&Str,
    Num        => 'Str',     \&Num,
    Int        => 'Num',     \&Int,

    # ref types
    Ref        => 'Defined', \&Ref,
    ScalarRef  => 'Ref',     \&ScalarRef,
    ArrayRef   => 'Ref',     \&ArrayRef,
    HashRef    => 'Ref',     \&HashRef,
    CodeRef    => 'Ref',     \&CodeRef,
    RegexpRef  => 'Ref',     \&RegexpRef,
    GlobRef    => 'Ref',     \&GlobRef,

    # object types
    FileHandle => 'GlobRef', \&FileHandle,
    Object     => 'Ref',     \&Object,

    # special string types
    ClassName  => 'Str',       \&ClassName,
    RoleName   => 'ClassName', \&RoleName,
);

while (my ($name, $parent, $code) = splice @builtins, 0, 3) {
    $TYPE{$name} = Mouse::Meta::TypeConstraint->new(
        name      => $name,
        parent    => $TYPE{$parent},
        optimized => $code,
    );
}

# parametarizable types
$TYPE{Maybe}   {constraint_generator} = \&_parameterize_Maybe_for;
$TYPE{ArrayRef}{constraint_generator} = \&_parameterize_ArrayRef_for;
$TYPE{HashRef} {constraint_generator} = \&_parameterize_HashRef_for;

# sugars
sub as          ($) { (as          => $_[0]) } ## no critic
sub where       (&) { (where       => $_[0]) } ## no critic
sub message     (&) { (message     => $_[0]) } ## no critic
sub optimize_as (&) { (optimize_as => $_[0]) } ## no critic

sub from    { @_ }
sub via (&) { $_[0] } ## no critic

# type utilities

sub optimized_constraints { # DEPRECATED
    Carp::cluck('optimized_constraints() has been deprecated');
    return \%TYPE;
}

undef @builtins;        # free the allocated memory
@builtins = keys %TYPE; # reuse it
sub list_all_builtin_type_constraints { @builtins }
sub list_all_type_constraints         { keys %TYPE }

sub _define_type {
    my $is_subtype = shift;
    my $name;
    my %args;

    if(@_ == 1 && ref $_[0] ){    # @_ : { name => $name, where => ... }
        %args = %{$_[0]};
    }
    elsif(@_ == 2 && ref $_[1]) { # @_ : $name => { where => ... }
        $name = $_[0];
        %args = %{$_[1]};
    }
    elsif(@_ % 2) {               # @_ : $name => ( where => ... )
        ($name, %args) = @_;
    }
    else{                         # @_ : (name => $name, where => ...)
        %args = @_;
    }

    if(!defined $name){
        $name = $args{name};
    }

    $args{name} = $name;

    my $parent = delete $args{as};
    if($is_subtype && !$parent){
        $parent = delete $args{name};
        $name   = undef;
    }

    if(defined $parent) {
        $args{parent} = find_or_create_isa_type_constraint($parent);
    }

    if(defined $name){
        # set 'package_defined_in' only if it is not a core package
        my $this = $args{package_defined_in};
        if(!$this){
            $this = caller(1);
            if($this !~ /\A Mouse \b/xms){
                $args{package_defined_in} = $this;
            }
        }

        if(defined $TYPE{$name}){
            my $that = $TYPE{$name}->{package_defined_in} || __PACKAGE__;
            if($this ne $that) {
                my $note = '';
                if($that eq __PACKAGE__) {
                    $note = sprintf " ('%s' is %s type constraint)",
                        $name,
                        scalar(grep { $name eq $_ } list_all_builtin_type_constraints())
                            ? 'a builtin'
                            : 'an implicitly created';
                }
                Carp::croak("The type constraint '$name' has already been created in $that"
                          . " and cannot be created again in $this" . $note);
            }
        }
    }

    $args{constraint} = delete $args{where}        if exists $args{where};
    $args{optimized}  = delete $args{optimized_as} if exists $args{optimized_as};

    my $constraint = Mouse::Meta::TypeConstraint->new(%args);

    if(defined $name){
        return $TYPE{$name} = $constraint;
    }
    else{
        return $constraint;
    }
}

sub type {
    return _define_type 0, @_;
}

sub subtype {
    return _define_type 1, @_;
}

sub coerce { # coerce $type, from $from, via { ... }, ...
    my $type_name = shift;
    my $type = find_type_constraint($type_name)
        or Carp::croak("Cannot find type '$type_name', perhaps you forgot to load it");

    $type->_add_type_coercions(@_);
    return;
}

sub class_type {
    my($name, $options) = @_;
    my $class = $options->{class} || $name;

    # ClassType
    return subtype $name => (
        as           => 'Object',
        optimized_as => Mouse::Util::generate_isa_predicate_for($class),
        class        => $class,
    );
}

sub role_type {
    my($name, $options) = @_;
    my $role = $options->{role} || $name;

    # RoleType
    return subtype $name => (
        as           => 'Object',
        optimized_as => sub {
            return Scalar::Util::blessed($_[0])
                && Mouse::Util::does_role($_[0], $role);
        },
        role         => $role,
    );
}

sub maybe_type {
    my $param = shift;
    return _find_or_create_parameterized_type($TYPE{Maybe}, $param);
}

sub duck_type {
    my($name, @methods);

    if(ref($_[0]) ne 'ARRAY'){
        $name = shift;
    }

    @methods = (@_ == 1 && ref($_[0]) eq 'ARRAY') ? @{$_[0]} : @_;

    # DuckType
    return _define_type 1, $name => (
        as           => 'Object',
        optimized_as => Mouse::Util::generate_can_predicate_for(\@methods),
        message      => sub {
            my($object) = @_;
            my @missing = grep { !$object->can($_) } @methods;
            return ref($object)
                . ' is missing methods '
                . Mouse::Util::quoted_english_list(@missing);
        },
        methods      => \@methods,
    );
}

sub enum {
    my($name, %valid);

    if(!(@_ == 1 && ref($_[0]) eq 'ARRAY')){
        $name = shift;
    }

    %valid = map{ $_ => undef }
        (@_ == 1 && ref($_[0]) eq 'ARRAY' ? @{$_[0]} : @_);

    # EnumType
    return _define_type 1, $name => (
        as            => 'Str',
        optimized_as  => sub{
            return defined($_[0]) && !ref($_[0]) && exists $valid{$_[0]};
        },
    );
}

sub _find_or_create_regular_type{
    my($spec, $create)  = @_;

    return $TYPE{$spec} if exists $TYPE{$spec};

    my $meta = Mouse::Util::get_metaclass_by_name($spec);

    if(!defined $meta){
        return $create ? class_type($spec) : undef;
    }

    if(Mouse::Util::is_a_metarole($meta)){
        return role_type($spec);
    }
    else{
        return class_type($spec);
    }
}

sub _find_or_create_parameterized_type{
    my($base, $param) = @_;

    my $name = sprintf '%s[%s]', $base->name, $param->name;

    $TYPE{$name} ||= $base->parameterize($param, $name);
}

sub _find_or_create_union_type{
    return if grep{ not defined } @_; # all things must be defined
    my @types = sort
        map{ $_->{type_constraints} ? @{$_->{type_constraints}} : $_ } @_;

    my $name = join '|', @types;

    # UnionType
    $TYPE{$name} ||= Mouse::Meta::TypeConstraint->new(
        name              => $name,
        type_constraints  => \@types,
    );
}

# The type parser

# param : '[' type ']' | NOTHING
sub _parse_param {
    my($c) = @_;

    if($c->{spec} =~ s/^\[//){
        my $type = _parse_type($c, 1);

        if($c->{spec} =~ s/^\]//){
            return $type;
        }
        Carp::croak("Syntax error in type: missing right square bracket in '$c->{orig}'");
    }

    return undef;
}

# name : [\w.:]+
sub _parse_name {
    my($c, $create) = @_;

    if($c->{spec} =~ s/\A ([\w.:]+) //xms){
        return _find_or_create_regular_type($1, $create);
    }
    Carp::croak("Syntax error in type: expect type name near '$c->{spec}' in '$c->{orig}'");
}

# single_type : name param
sub _parse_single_type {
    my($c, $create) = @_;

    my $type  = _parse_name($c, $create);
    my $param = _parse_param($c);

    if(defined $type){
        if(defined $param){
            return _find_or_create_parameterized_type($type, $param);
        }
        else {
            return $type;
        }
    }
    elsif(defined $param){
        Carp::croak("Undefined type with parameter [$param] in '$c->{orig}'");
    }
    else{
        return undef;
    }
}

# type : single_type  ('|' single_type)*
sub _parse_type {
    my($c, $create) = @_;

    my $type = _parse_single_type($c, $create);
    if($c->{spec}){ # can be an union type
        my @types;
        while($c->{spec} =~ s/^\|//){
            push @types, _parse_single_type($c, $create);
        }
        if(@types){
            return _find_or_create_union_type($type, @types);
        }
    }
    return $type;
}


sub find_type_constraint {
    my($spec) = @_;
    return $spec if Mouse::Util::is_a_type_constraint($spec) or not defined $spec;

    $spec =~ s/\s+//g;
    return $TYPE{$spec};
}

sub register_type_constraint {
    my($constraint) = @_;
    Carp::croak("No type supplied / type is not a valid type constraint")
        unless Mouse::Util::is_a_type_constraint($constraint);
    return $TYPE{$constraint->name} = $constraint;
}

sub find_or_parse_type_constraint {
    my($spec) = @_;
    return $spec if Mouse::Util::is_a_type_constraint($spec) or not defined $spec;

    $spec =~ tr/ \t\r\n//d;

    my $tc = $TYPE{$spec};
    if(defined $tc) {
        return $tc;
    }

    my %context = (
        spec => $spec,
        orig => $spec,
    );
    $tc = _parse_type(\%context);

    if($context{spec}){
        Carp::croak("Syntax error: extra elements '$context{spec}' in '$context{orig}'");
    }

    return $TYPE{$spec} = $tc;
}

sub find_or_create_does_type_constraint{
    # XXX: Moose does not register a new role_type, but Mouse does.
    my $tc = find_or_parse_type_constraint(@_);
    return defined($tc) ? $tc : role_type(@_);
}

sub find_or_create_isa_type_constraint {
    # XXX: Moose does not register a new class_type, but Mouse does.
    my $tc = find_or_parse_type_constraint(@_);
    return defined($tc) ? $tc : class_type(@_);
}

1;
__END__

=head1 NAME

Mouse::Util::TypeConstraints - Type constraint system for Mouse

=head1 VERSION

This document describes Mouse version v2.5.10

=head2 SYNOPSIS

  use Mouse::Util::TypeConstraints;

  subtype 'Natural'
      => as 'Int'
      => where { $_ > 0 };

  subtype 'NaturalLessThanTen'
      => as 'Natural'
      => where { $_ < 10 }
      => message { "This number ($_) is not less than ten!" };

  coerce 'Num'
      => from 'Str'
        => via { 0+$_ };

  enum 'RGBColors' => qw(red green blue);

  no Mouse::Util::TypeConstraints;

=head1 DESCRIPTION

This module provides Mouse with the ability to create custom type
constraints to be used in attribute definition.

=head2 Important Caveat

This is B<NOT> a type system for Perl 5. These are type constraints,
and they are not used by Mouse unless you tell it to. No type
inference is performed, expressions are not typed, etc. etc. etc.

A type constraint is at heart a small "check if a value is valid"
function. A constraint can be associated with an attribute. This
simplifies parameter validation, and makes your code clearer to read,
because you can refer to constraints by name.

=head2 Slightly Less Important Caveat

It is B<always> a good idea to quote your type names.

This prevents Perl from trying to execute the call as an indirect
object call. This can be an issue when you have a subtype with the
same name as a valid class.

For instance:

  subtype DateTime => as Object => where { $_->isa('DateTime') };

will I<just work>, while this:

  use DateTime;
  subtype DateTime => as Object => where { $_->isa('DateTime') };

will fail silently and cause many headaches. The simple way to solve
this, as well as future proof your subtypes from classes which have
yet to have been created, is to quote the type name:

  use DateTime;
  subtype 'DateTime' => as 'Object' => where { $_->isa('DateTime') };

=head2 Default Type Constraints

This module also provides a simple hierarchy for Perl 5 types, here is
that hierarchy represented visually.

 Any
  Item
      Bool
      Maybe[`a]
      Undef
      Defined
          Value
              Str
                  Num
                      Int
                  ClassName
                  RoleName
          Ref
              ScalarRef
              ArrayRef[`a]
              HashRef[`a]
              CodeRef
              RegexpRef
              GlobRef
                  FileHandle
              Object

B<NOTE:> Any type followed by a type parameter C<[`a]> can be
parameterized, this means you can say:

  ArrayRef[Int]    # an array of integers
  HashRef[CodeRef] # a hash of str to CODE ref mappings
  Maybe[Str]       # value may be a string, may be undefined

If Mouse finds a name in brackets that it does not recognize as an
existing type, it assumes that this is a class name, for example
C<ArrayRef[DateTime]>.

B<NOTE:> The C<Undef> type constraint for the most part works
correctly now, but edge cases may still exist, please use it
sparingly.

B<NOTE:> The C<ClassName> type constraint does a complex package
existence check. This means that your class B<must> be loaded for this
type constraint to pass.

B<NOTE:> The C<RoleName> constraint checks a string is a I<package
name> which is a role, like C<'MyApp::Role::Comparable'>. The C<Role>
constraint checks that an I<object does> the named role.

=head2 Type Constraint Naming

Type name declared via this module can only contain alphanumeric
characters, colons (:), and periods (.).

Since the types created by this module are global, it is suggested
that you namespace your types just as you would namespace your
modules. So instead of creating a I<Color> type for your
B<My::Graphics> module, you would call the type
I<My::Graphics::Types::Color> instead.

=head2 Use with Other Constraint Modules

This module can play nicely with other constraint modules with some
slight tweaking. The C<where> clause in types is expected to be a
C<CODE> reference which checks it's first argument and returns a
boolean. Since most constraint modules work in a similar way, it
should be simple to adapt them to work with Mouse.

For instance, this is how you could use it with
L<Declare::Constraints::Simple> to declare a completely new type.

  type 'HashOfArrayOfObjects',
      {
      where => IsHashRef(
          -keys   => HasLength,
          -values => IsArrayRef(IsObject)
      )
  };

Here is an example of using L<Test::Deep> and it's non-test
related C<eq_deeply> function.

  type 'ArrayOfHashOfBarsAndRandomNumbers'
      => where {
          eq_deeply($_,
              array_each(subhashof({
                  bar           => isa('Bar'),
                  random_number => ignore()
              })))
        };

=head1 METHODS

=head2 C<< list_all_builtin_type_constraints -> (Names) >>

Returns the names of builtin type constraints.

=head2 C<< list_all_type_constraints -> (Names) >>

Returns the names of all the type constraints.

=head1 FUNCTIONS

=over 4

=item C<< type $name => where { } ... -> Mouse::Meta::TypeConstraint >>

=item C<< subtype $name => as $parent => where { } ... -> Mouse::Meta::TypeConstraint >>

=item C<< subtype as $parent => where { } ...  -> Mouse::Meta::TypeConstraint >>

=item C<< class_type ($class, ?$options) -> Mouse::Meta::TypeConstraint >>

=item C<< role_type ($role, ?$options) -> Mouse::Meta::TypeConstraint >>

=item C<< duck_type($name, @methods | \@methods) -> Mouse::Meta::TypeConstraint >>

=item C<< duck_type(\@methods) -> Mouse::Meta::TypeConstraint >>

=item C<< enum($name, @values | \@values) -> Mouse::Meta::TypeConstraint >>

=item C<< enum (\@values) -> Mouse::Meta::TypeConstraint >>

=item C<< coerce $type => from $another_type, via { }, ... >>

=back

=over 4

=item C<< find_type_constraint(Type) -> Mouse::Meta::TypeConstraint >>

=back

=head1 THANKS

Much of this documentation was taken from C<Moose::Util::TypeConstraints>

=head1 SEE ALSO

L<Moose::Util::TypeConstraints>

=cut


