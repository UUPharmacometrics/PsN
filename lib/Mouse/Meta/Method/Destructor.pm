package Mouse::Meta::Method::Destructor;
use Mouse::Util qw(:meta); # enables strict and warnings

use constant _MOUSE_DEBUG => $ENV{MOUSE_DEBUG} ? 1 : 0;

sub _generate_destructor{
    my (undef, $metaclass) = @_;

    my $demolishall = '';
    for my $class ($metaclass->linearized_isa) {
        if (Mouse::Util::get_code_ref($class, 'DEMOLISH')) {
            $demolishall .= '                ' . $class
                . '::DEMOLISH($self, Mouse::Util::in_global_destruction());'
                . "\n",
        }
    }

    if($demolishall) {
        $demolishall = sprintf <<'EOT', $demolishall;
        my $e = do{
            local $?;
            local $@;
            eval{
                %s;
            };
            $@;
        };
        no warnings 'misc';
        die $e if $e; # rethrow
EOT
    }

    my $name   = $metaclass->name;
    my $source = sprintf(<<'EOT', __FILE__, $name, $demolishall);
#line 1 "%s"
    package %s;
    sub {
        my($self) = @_;
        return $self->Mouse::Object::DESTROY()
            if ref($self) ne __PACKAGE__;
        # DEMOLISHALL
        %s;
        return;
    }
EOT

    warn $source if _MOUSE_DEBUG;

    my $code;
    my $e = do{
        local $@;
        $code = eval $source;
        $@;
    };
    die $e if $e;
    return $code;
}

1;
__END__

=for stopwords destructors

=head1 NAME

Mouse::Meta::Method::Destructor - A Mouse method generator for destructors

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SEE ALSO

L<Moose::Meta::Method::Destructor>

=cut
