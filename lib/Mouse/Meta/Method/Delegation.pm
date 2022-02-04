package Mouse::Meta::Method::Delegation;
use Mouse::Util qw(:meta); # enables strict and warnings
use Scalar::Util;

sub _generate_delegation{
    my (undef, $attr, $handle_name, $method_to_call) = @_;

    my @curried_args;
    if(ref($method_to_call) eq 'ARRAY'){
        ($method_to_call, @curried_args) = @{$method_to_call};
    }

    # If it has a reader, we must use it to make method modifiers work
    my $reader = $attr->get_read_method() || $attr->get_read_method_ref();

    my $can_be_optimized = $attr->{_mouse_cache_method_delegation_can_be_optimized};

    if(!defined $can_be_optimized){
        my $tc = $attr->type_constraint;
        $attr->{_mouse_cache_method_delegation_can_be_optimized} =
            (defined($tc) && $tc->is_a_type_of('Object'))
            && ($attr->is_required || $attr->has_default || $attr->has_builder)
            && ($attr->is_lazy || !$attr->has_clearer);
    }

    if($can_be_optimized){
        # need not check the attribute value
        return sub {
            return shift()->$reader()->$method_to_call(@curried_args, @_);
        };
    }
    else {
        # need to check the attribute value
        return sub {
            my $instance = shift;
            my $proxy    = $instance->$reader();

            my $error = !defined($proxy)                              ? ' is not defined'
                      : ref($proxy) && !Scalar::Util::blessed($proxy) ? qq{ is not an object (got '$proxy')}
                                                                      : undef;
            if ($error) {
                $instance->meta->throw_error(
                    "Cannot delegate $handle_name to $method_to_call because "
                        . "the value of "
                        . $attr->name
                        . $error
                 );
            }
            $proxy->$method_to_call(@curried_args, @_);
        };
    }
}


1;
__END__

=head1 NAME

Mouse::Meta::Method::Delegation - A Mouse method generator for delegation methods

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SEE ALSO

L<Moose::Meta::Method::Delegation>

=cut
