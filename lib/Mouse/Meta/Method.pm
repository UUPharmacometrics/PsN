package Mouse::Meta::Method;
use Mouse::Util qw(:meta); # enables strict and warnings
use Scalar::Util ();

use overload
    '=='  => '_equal',
    'eq'  => '_equal',
    '&{}' => sub{ $_[0]->body },
    fallback => 1,
;

sub wrap {
    my $class = shift;
    unshift @_, 'body' if @_ % 2 != 0;
    return $class->_new(@_);
}

sub _new{
    my($class, %args) = @_;
    my $self = bless \%args, $class;

    if($class ne __PACKAGE__){
        $self->meta->_initialize_object($self, \%args);
    }
    return $self;
}

sub body                 { $_[0]->{body}    }
sub name                 { $_[0]->{name}    }
sub package_name         { $_[0]->{package} }
sub associated_metaclass { $_[0]->{associated_metaclass} }

sub fully_qualified_name {
    my($self) = @_;
    return $self->package_name . '::' . $self->name;
}

# for Moose compat
sub _equal {
    my($l, $r) = @_;

    return Scalar::Util::blessed($r)
            && $l->body         == $r->body
            && $l->name         eq $r->name
            && $l->package_name eq $r->package_name;
}

1;
__END__

=head1 NAME

Mouse::Meta::Method - A Mouse Method metaclass

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 DESCRIPTION

This class is a meta object protocol for Mouse methods,
which is a subset of Moose::Meta:::Method.

=head1 SEE ALSO

L<Moose::Meta::Method>

L<Class::MOP::Method>

=cut
