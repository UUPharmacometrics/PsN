package Mouse::Meta::Role::Method;
use Mouse::Util; # enables strict and warnings

use Mouse::Meta::Method;
our @ISA = qw(Mouse::Meta::Method);

sub _new{
    my($class, %args) = @_;
    my $self = bless \%args, $class;

    if($class ne __PACKAGE__){
        $self->meta->_initialize_object($self, \%args);
    }
    return $self;
}

1;
__END__

=head1 NAME

Mouse::Meta::Role::Method - A Mouse Method metaclass for Roles

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SEE ALSO

L<Moose::Meta::Role::Method>

=cut

