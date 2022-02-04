package Mouse::Meta::Method::Constructor;
use Mouse::Util qw(:meta); # enables strict and warnings

use constant _MOUSE_DEBUG => $ENV{MOUSE_DEBUG} ? 1 : 0;

sub _inline_slot{
    my(undef, $self_var, $attr_name) = @_;
    return sprintf '%s->{q{%s}}', $self_var, $attr_name;
}

sub _generate_constructor {
    my ($class, $metaclass, $args) = @_;

    my $associated_metaclass_name = $metaclass->name;

    my $buildall      = $class->_generate_BUILDALL($metaclass);
    my $buildargs     = $class->_generate_BUILDARGS($metaclass);
    my $initializer   = $metaclass->{_mouse_cache}{_initialize_object} ||=
       $class->_generate_initialize_object($metaclass);
    my $source = sprintf(<<'EOT', __FILE__, $metaclass->name, $buildargs, $buildall);
#line 1 "%s"
        package %s;
        sub {
            my $class = shift;
            return $class->Mouse::Object::new(@_)
                if $class ne __PACKAGE__;
            # BUILDARGS
            %s;
            my $instance = bless {}, $class;
            $metaclass->$initializer($instance, $args, 0);
            # BUILDALL
            %s;
            return $instance;
        }
EOT
    warn $source if _MOUSE_DEBUG;
    my $body;
    my $e = do{
        local $@;
        $body = eval $source;
        $@;
    };
    die $e if $e;
    return $body;
}

sub _generate_initialize_object {
    my ($method_class, $metaclass) = @_;
    my @attrs  = $metaclass->get_all_attributes;

    my @checks = map { $_ && $_->_compiled_type_constraint }
                 map { $_->type_constraint } @attrs;

    my @res;

    my $has_triggers;
    my $strict = $metaclass->strict_constructor;

    if($strict){
        push @res, 'my $used = 0;';
    }

    for my $index (0 .. @attrs - 1) {
        my $code = '';

        my $attr = $attrs[$index];
        my $key  = $attr->name;

        my $init_arg        = $attr->init_arg;
        my $type_constraint = $attr->type_constraint;
        my $is_weak_ref     = $attr->is_weak_ref;
        my $need_coercion;

        my $instance_slot  = $method_class->_inline_slot('$instance', $key);
        my $attr_var       = "\$attrs[$index]";
        my $constraint_var;

        if(defined $type_constraint){
             $constraint_var = "$attr_var\->{type_constraint}";
             $need_coercion  = ($attr->should_coerce && $type_constraint->has_coercion);
        }

        $code .= "# initialize $key\n";

        my $post_process = '';
        if(defined $type_constraint){
            $post_process .= "\$checks[$index]->($instance_slot)\n";
            $post_process .= "  or $attr_var->_throw_type_constraint_error($instance_slot, $constraint_var);\n";
        }

        # build cde for an attribute
        if (defined $init_arg) {
            my $value = "\$args->{q{$init_arg}}";

            $code .= "if (exists $value) {\n";

            if($need_coercion){
                $value = "$constraint_var->coerce($value)";
            }

            $code .= "$instance_slot = $value;\n";
            $code .= $post_process;

            if ($attr->has_trigger) {
                $has_triggers++;
                $code .= "push \@triggers, [$attr_var\->{trigger}, $instance_slot];\n";
            }

            if ($strict){
                $code .= '++$used;' . "\n";
            }

            $code .= "\n} else {\n"; # $value exists
        }

        if ($attr->has_default || $attr->has_builder) {
            unless ($attr->is_lazy) {
                my $default = $attr->default;
                my $builder = $attr->builder;

                my $value;
                if (defined($builder)) {
                    $value = "\$instance->$builder()";
                }
                elsif (ref($default) eq 'CODE') {
                    $value = "$attr_var\->{default}->(\$instance)";
                }
                elsif (defined($default)) {
                    $value = "$attr_var\->{default}";
                }
                else {
                    $value = 'undef';
                }

                if($need_coercion){
                    $value = "$constraint_var->coerce($value)";
                }

                $code .= "$instance_slot = $value;\n";
                $code .= $post_process;
            }
        }
        elsif ($attr->is_required) {
            $code .= "\$meta->throw_error('Attribute ($key) is required')";
            $code .= "    unless \$is_cloning;\n";
        }

        $code .= "}\n" if defined $init_arg;

        if($is_weak_ref){
            $code .= "Scalar::Util::weaken($instance_slot) "
                   . "if ref $instance_slot and not Scalar::Util::isweak($instance_slot);\n";
        }

        push @res, $code;
    }

    if($strict){
        push @res, q{if($used < keys %{$args})}
            . q{{ $meta->_report_unknown_args(\@attrs, $args) }};
    }

    if($metaclass->is_anon_class){
        push @res, q{$instance->{__METACLASS__} = $meta;};
    }

    if($has_triggers){
        unshift @res, q{my @triggers;};
        push    @res, q{$_->[0]->($instance, $_->[1]) for @triggers;};
    }

    my $source = sprintf <<'EOT', __FILE__, $metaclass->name, join "\n", @res;
#line 1 "%s"
    package %s;
    sub {
        my($meta, $instance, $args, $is_cloning) = @_;
        %s;
        return $instance;
    }
EOT
    warn $source if _MOUSE_DEBUG;
    my $body;
    my $e = do {
        local $@;
        $body = eval $source;
        $@;
    };
    die $e if $e;
    return $body;
}

sub _generate_BUILDARGS {
    my(undef, $metaclass) = @_;

    my $class = $metaclass->name;
    if ( $class->can('BUILDARGS') && $class->can('BUILDARGS') != \&Mouse::Object::BUILDARGS ) {
        return 'my $args = $class->BUILDARGS(@_)';
    }

    return <<'...';
        my $args;
        if ( scalar @_ == 1 ) {
            ( ref( $_[0] ) eq 'HASH' )
                || Carp::confess "Single parameters to new() must be a HASH ref";
            $args = +{ %{ $_[0] } };
        }
        else {
            $args = +{@_};
        }
...
}

sub _generate_BUILDALL {
    my (undef, $metaclass) = @_;

    return '' unless $metaclass->name->can('BUILD');

    my @code;
    for my $class ($metaclass->linearized_isa) {
        if (Mouse::Util::get_code_ref($class, 'BUILD')) {
            unshift  @code, qq{${class}::BUILD(\$instance, \$args);};
        }
    }
    return join "\n", @code;
}

1;
__END__

=head1 NAME

Mouse::Meta::Method::Constructor - A Mouse method generator for constructors

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SEE ALSO

L<Moose::Meta::Method::Constructor>

=cut
