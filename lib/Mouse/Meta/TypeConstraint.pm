package Mouse::Meta::TypeConstraint;
use Mouse::Util qw(:meta); # enables strict and warnings

sub new {
    my $class = shift;
    my %args  = @_ == 1 ? %{$_[0]} : @_;

    $args{name} = '__ANON__' if !defined $args{name};

    my $type_parameter;
    if(defined $args{parent}) { # subtyping
        %args = (%{$args{parent}}, %args);

        # a child type must not inherit 'compiled_type_constraint'
        # and 'hand_optimized_type_constraint' from the parent
        delete $args{compiled_type_constraint};       # don't inherit it
        delete $args{hand_optimized_type_constraint}; # don't inherit it

        $type_parameter = $args{type_parameter};
        if(defined(my $parent_tp = $args{parent}{type_parameter})) {
            if($parent_tp != $type_parameter) {
                $type_parameter->is_a_type_of($parent_tp)
                    or $class->throw_error(
                        "$type_parameter is not a subtype of $parent_tp",
                    );
            }
            else {
                $type_parameter = undef;
            }
        }
    }

    my $check;

    if($check = delete $args{optimized}) { # likely to be builtins
        $args{hand_optimized_type_constraint} = $check;
        $args{compiled_type_constraint}       = $check;
    }
    elsif(defined $type_parameter) { # parameterizing
        my $generator = $args{constraint_generator}
            || $class->throw_error(
                  "The $args{name} constraint cannot be used,"
                . " because $type_parameter doesn't subtype"
                . " from a parameterizable type");

        my $parameterized_check = $generator->($type_parameter);
        if(defined(my $my_check = $args{constraint})) {
            $check = sub {
                return $parameterized_check->($_) && $my_check->($_);
            };
        }
        else {
            $check = $parameterized_check;
        }
        $args{constraint} = $check;
    }
    else { # common cases
        $check = $args{constraint};
    }

    if(defined($check) && ref($check) ne 'CODE'){
        $class->throw_error(
            "Constraint for $args{name} is not a CODE reference");
    }

    my $self = bless \%args, $class;
    $self->compile_type_constraint()
        if !$args{hand_optimized_type_constraint};

    if($args{type_constraints}) { # union types
        foreach my $type(@{$self->{type_constraints}}){
            if($type->has_coercion){
                # set undef for has_coercion()
                $self->{_compiled_type_coercion} = undef;
                last;
            }
        }
    }

    return $self;
}

sub create_child_type {
    my $self = shift;
    return ref($self)->new(@_, parent => $self);
}

sub name;
sub parent;
sub message;
sub has_coercion;

sub check;

sub type_parameter;
sub __is_parameterized;

sub _compiled_type_constraint;
sub _compiled_type_coercion;

sub compile_type_constraint;


sub _add_type_coercions { # ($self, @pairs)
    my $self = shift;

    if(exists $self->{type_constraints}){ # union type
        $self->throw_error(
            "Cannot add additional type coercions to Union types '$self'");
    }

    my $coercion_map = ($self->{coercion_map} ||= []);
    my %has          = map{ $_->[0]->name => undef } @{$coercion_map};

    for(my $i = 0; $i < @_; $i++){
        my $from   = $_[  $i];
        my $action = $_[++$i];

        if(exists $has{$from}){
            $self->throw_error("A coercion action already exists for '$from'");
        }

        my $type = Mouse::Util::TypeConstraints::find_or_parse_type_constraint($from)
            or $self->throw_error(
                "Could not find the type constraint ($from) to coerce from");

        push @{$coercion_map}, [ $type => $action ];
    }

    $self->{_compiled_type_coercion} = undef;
    return;
}

sub _compiled_type_coercion {
    my($self) = @_;

    my $coercion = $self->{_compiled_type_coercion};
    return $coercion if defined $coercion;

    if(!$self->{type_constraints}) {
        my @coercions;
        foreach my $pair(@{$self->{coercion_map}}) {
            push @coercions,
                [ $pair->[0]->_compiled_type_constraint, $pair->[1] ];
        }

        $coercion = sub {
           my($thing) = @_;
           foreach my $pair (@coercions) {
                #my ($constraint, $converter) = @$pair;
                if ($pair->[0]->($thing)) {
                    return $pair->[1]->($thing) for $thing; # local $_ will cancel tie-ness due to perl's bug
                }
           }
           return $thing;
        };
    }
    else { # for union type
        my @coercions;
        foreach my $type(@{$self->{type_constraints}}){
            if($type->has_coercion){
                push @coercions, $type;
            }
        }
        if(@coercions){
            $coercion = sub {
                my($thing) = @_;
                foreach my $type(@coercions){
                    my $value = $type->coerce($thing);
                    return $value if $self->check($value);
                }
                return $thing;
            };
        }
    }

    return( $self->{_compiled_type_coercion} = $coercion );
}

sub coerce {
    my $self = shift;
    return $_[0] if $self->check(@_);

    my $coercion = $self->_compiled_type_coercion
        or $self->throw_error("Cannot coerce without a type coercion");
    return  $coercion->(@_);
}

sub get_message {
    my ($self, $value) = @_;
    if ( my $msg = $self->message ) {
        return $msg->($value) for $value; # local $_ will cancel tie-ness due to perl's bug
    }
    else {
        if(not defined $value) {
            $value = 'undef';
        }
        elsif( ref($value) && defined(&overload::StrVal) ) {
            $value = overload::StrVal($value);
        }
        return "Validation failed for '$self' with value $value";
    }
}

sub is_a_type_of {
    my($self, $other) = @_;

    # ->is_a_type_of('__ANON__') is always false
    return 0 if !ref($other) && $other eq '__ANON__';

    (my $other_name = $other) =~ s/\s+//g;

    return 1 if $self->name eq $other_name;

    if(exists $self->{type_constraints}){ # union
        foreach my $type(@{$self->{type_constraints}}) {
            return 1 if $type->name eq $other_name;
        }
    }

    for(my $p = $self->parent; defined $p; $p = $p->parent) {
        return 1 if $p->name eq $other_name;
    }

    return 0;
}

# See also Moose::Meta::TypeConstraint::Parameterizable
sub parameterize {
    my($self, $param, $name) = @_;

    if(!ref $param){
        require Mouse::Util::TypeConstraints;
        $param = Mouse::Util::TypeConstraints::find_or_create_isa_type_constraint($param);
    }

    $name ||= sprintf '%s[%s]', $self->name, $param->name;
    return Mouse::Meta::TypeConstraint->new(
        name           => $name,
        parent         => $self,
        type_parameter => $param,
    );
}

sub assert_valid {
    my ($self, $value) = @_;

    if(!$self->check($value)){
        $self->throw_error($self->get_message($value));
    }
    return 1;
}

# overloading stuff

sub _as_string { $_[0]->name } # overload ""
sub _identity;                 # overload 0+

sub _unite { # overload infix:<|>
    my($lhs, $rhs) = @_;
    require Mouse::Util::TypeConstraints;
    return Mouse::Util::TypeConstraints::_find_or_create_union_type(
        $lhs,
        Mouse::Util::TypeConstraints::find_or_create_isa_type_constraint($rhs),
    );
}

1;
__END__

=head1 NAME

Mouse::Meta::TypeConstraint - The Mouse Type Constraint metaclass

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 DESCRIPTION

This class represents a type constraint, including built-in
type constraints, union type constraints, parameterizable/
parameterized type constraints, as well as custom type
constraints

=head1 METHODS

=over

=item C<< Mouse::Meta::TypeConstraint->new(%options) >>

=item C<< $constraint->name >>

=item C<< $constraint->parent >>

=item C<< $constraint->constraint >>

=item C<< $constraint->has_coercion >>

=item C<< $constraint->message >>

=item C<< $constraint->is_a_type_of($name or $object) >>

=item C<< $constraint->coerce($value) >>

=item C<< $constraint->check($value) >>

=item C<< $constraint->assert_valid($value) >>

=item C<< $constraint->get_message($value) >>

=item C<< $constraint->create_child_type(%options) >>

=back

=head1 SEE ALSO

L<Moose::Meta::TypeConstraint>

=cut

