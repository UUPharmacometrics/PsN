package MouseX::Params::Validate;

$MouseX::Params::Validate::VERSION = '0.10';
$MouseX::Params::Validate::AUTHOR  = 'cpan:MANWAR';

=head1 NAME

MouseX::Params::Validate - Extension of Params::Validate using Mouse's types.

=head1 VERSION

Version 0.10

=cut

use 5.006;
use strict; use warnings;

use Carp 'confess';
use Devel::Caller 'caller_cv';
use Scalar::Util 'blessed', 'refaddr', 'reftype';

use Mouse::Util::TypeConstraints qw( find_type_constraint class_type role_type );
use Params::Validate;
use Sub::Exporter -setup =>
{
    exports => [qw( validated_hash validated_list pos_validated_list )],
    groups  => {default => [qw( validated_hash validated_list pos_validated_list )]},
};

my %CACHED_SPECS;

=head1 DESCRIPTION

Method parameter validation extension to Mouse.

Borrowed code entirely from L<MooseX::Params::Validate> and stripped Moose footprints.

=head1 EXPORTS

By default, this module exports the following:

=over 3

=item * C<validated_hash>

=item * C<validated_list>

=item * C<pos_validated_list>

=back

=head1 CAVEATS

It  isn't  possible  to introspect the method parameter specs they are created as
needed when the method is called and cached for subsequent calls.

=head1 CACHING

When  a  validation  subroutine  is  called the first time, the parameter spec is
prepared  & cached to avoid unnecessary regeneration. It uses the fully qualified
name of the subroutine (package +subname) as the cache key. In 99.999% of the use
cases  for  this  module  that will be the right thing to do. You can do a couple
things to better control the caching behavior.

=over 2

=item *

Passing in the C<MX_PARAMS_VALIDATE_NO_CACHE> flag in the parameter spec this will
prevent the parameter spec from being cached.

=item *

Passing in C<MX_PARAMS_VALIDATE_CACHE_KEY> with value to be used as the cache key
will bypass the normal cache key generation.

=back

=head1 METHODS

=head2 B<validated_hash(\@_, %parameter_spec)>

This behaves  similarly  to the standard L<Params::Validate> C<validate> function
and returns the captured values in a HASH. The one exception is where if it spots
an instance in the C<@_> ,then it will handle it appropriately.

The  values  in  C<@_> can  either  be a set of name-value pairs or a single hash
reference.

The C<%parameter_spec> accepts the following options:

=over 4

=item I<isa>

The C<isa> option can be either;class name, Mouse type constraint name or an anon
Mouse type constraint.

=item I<does>

The C<does> option can be either; role name or an anon Mouse type constraint.

=item I<default>

This is the default value to be used if the value is not supplied.

=item I<optional>

As with L<Params::Validate>, all options are considered required unless otherwise
specified. This option is passed directly to L<Params::Validate>.

=item I<coerce>

If this is true and the parameter has a type constraint which has coercions, then
the coercion will  be  called for this parameter.If the type does have coercions,
then this parameter is ignored.

=back

    use Mouse;
    use MouseX::Params::Validate;

    sub foo
    {
        my ($self, %params) = validated_hash(
            \@_,
            bar => {isa => 'Str', default => 'Mouse'},
        );
        ...
        ...
    }

=cut

sub validated_hash
{
    my ($args, %spec) = @_;

    my $cache_key = _cache_key(\%spec);
    my $allow_extra = delete $spec{MX_PARAMS_VALIDATE_ALLOW_EXTRA};

    if (exists $CACHED_SPECS{$cache_key})
    {
        (ref($CACHED_SPECS{$cache_key}) eq 'HASH')
        || confess("I was expecting a HASH-ref in the cached $cache_key parameter"
                 . " spec, you are doing something funky, stop it!");
        %spec = %{$CACHED_SPECS{$cache_key}};
    }
    else
    {
        my $should_cache = delete $spec{MX_PARAMS_VALIDATE_NO_CACHE} ? 0 : 1;
        $spec{$_} = _convert_to_param_validate_spec( $spec{$_} )
            foreach keys %spec;
        $CACHED_SPECS{$cache_key} = \%spec if $should_cache;
    }

    my $instance;
    $instance = shift @$args if blessed $args->[0];

    my %args
        = @$args == 1
        && ref $args->[0]
        && reftype($args->[0]) eq 'HASH' ? %{$args->[0]} : @$args;

    $args{$_} = $spec{$_}{constraint}->coerce($args{$_})
        for grep { $spec{$_}{coerce} && exists $args{$_} } keys %spec;

    %args = Params::Validate::validate_with(
        params      => \%args,
        spec        => \%spec,
        allow_extra => $allow_extra,
        called      => _caller_name(),
    );

    return ((defined $instance ? $instance : ()), %args);
}

=head2 B<validated_list(\@_, %parameter_spec)>

The  C<%parameter_spec> accepts  the same options as above but returns the params
as positional values instead of a HASH.

We  capture the order in which you defined the parameters and then return them as
a list  in the same order.If a param is marked optional and not included, then it
will be set to C<undef>.

The  values  in C<@_>  can  either  be a set of name-value pairs or a single hash
reference.

Like  C<validated_hash>, if it spots an object instance as the first parameter of
C<@_> it will handle it appropriately, returning it as the first argument.

    use Mouse;
    use MouseX::Params::Validate;

    sub foo
    {
        my ($self, $foo, $bar) = validated_list(
            \@_,
            foo => {isa => 'Foo'},
            bar => {isa => 'Bar'},
        );
        ...
        ...
    }

=cut

sub validated_list
{
    my ($args, @spec) = @_;

    my %spec = @spec;
    my $cache_key = _cache_key(\%spec);
    my $allow_extra = delete $spec{MX_PARAMS_VALIDATE_ALLOW_EXTRA};

    my @ordered_spec;
    if (exists $CACHED_SPECS{$cache_key})
    {
        (ref($CACHED_SPECS{$cache_key}) eq 'ARRAY')
        || confess("I was expecting a ARRAY-ref in the cached $cache_key parameter"
                 . " spec, you are doing something funky, stop it!");
        %spec         = %{ $CACHED_SPECS{$cache_key}->[0] };
        @ordered_spec = @{ $CACHED_SPECS{$cache_key}->[1] };
    }
    else
    {
        my $should_cache = delete $spec{MX_PARAMS_VALIDATE_NO_CACHE} ? 0 : 1;
        @ordered_spec = grep { exists $spec{$_} } @spec;
        $spec{$_} = _convert_to_param_validate_spec($spec{$_}) foreach keys %spec;
        $CACHED_SPECS{$cache_key} = [\%spec, \@ordered_spec] if $should_cache;
    }

    my $instance;
    $instance = shift @$args if blessed $args->[0];

    my %args
        = @$args == 1
        && ref $args->[0]
        && reftype( $args->[0] ) eq 'HASH' ? %{ $args->[0] } : @$args;

    $args{$_} = $spec{$_}{constraint}->coerce($args{$_})
        for grep { $spec{$_}{coerce} && exists $args{$_} } keys %spec;

    %args = Params::Validate::validate_with(
        params      => \%args,
        spec        => \%spec,
        allow_extra => $allow_extra,
        called      => _caller_name(),
    );

    return (
        (defined $instance ? $instance : ()),
        @args{@ordered_spec}
    );
}

=head2 B<pos_validated_list(\@_, $spec, $spec, ...)>

This function validates a  list  of  positional parameters. Each C<$spec>  should
validate one of the parameters in the list.

Unlike the other functions, this function I<cannot> find C<$self> in the argument
list. Make sure to shift it off yourself before doing validation.

The  values  in C<@_>  must be a list of values. You cannot pass the values as an
array reference,because this cannot be distinguished from passing one value which
itself an array reference.

If  a  parameter  is marked as optional and is not present, it will simply not be
returned.

If  you  want to pass in  any  of  the  cache control parameters described below,
simply pass them after the list of parameter validation specs.

    use Mouse;
    use MouseX::Params::Validate;

    sub foo
    {
        my $self = shift;
        my ($foo, $bar) = pos_validated_list(
            \@_,
            {isa => 'Foo'},
            {isa => 'Bar'},
            MX_PARAMS_VALIDATE_NO_CACHE => 1,
        );
        ...
        ...
    }

=cut

sub pos_validated_list
{
    my $args = shift;

    my @spec;
    push @spec, shift while ref $_[0];
    my %extra = @_;
    my $cache_key = _cache_key( \%extra );
    my $allow_extra = delete $extra{MX_PARAMS_VALIDATE_ALLOW_EXTRA};

    my @pv_spec;
    if (exists $CACHED_SPECS{$cache_key})
    {
        (ref($CACHED_SPECS{$cache_key}) eq 'ARRAY')
        || confess("I was expecting an ARRAY-ref in the cached $cache_key parameter"
                 . " spec, you are doing something funky, stop it!");
        @pv_spec = @{$CACHED_SPECS{$cache_key}};
    }
    else
    {
        my $should_cache = exists $extra{MX_PARAMS_VALIDATE_NO_CACHE} ? 0 : 1;
        @pv_spec = map { _convert_to_param_validate_spec($_) } @spec;
        $CACHED_SPECS{$cache_key} = \@pv_spec if $should_cache;
    }

    my @args = @$args;
    $args[$_] = $pv_spec[$_]{constraint}->coerce($args[$_])
        for grep { $pv_spec[$_] && $pv_spec[$_]{coerce} } 0 .. $#args;

    @args = Params::Validate::validate_with(
        params      => \@args,
        spec        => \@pv_spec,
        allow_extra => $allow_extra,
        called      => _caller_name(),
    );

    return @args;
}

sub _cache_key
{
    my $spec = shift;

    if (exists $spec->{MX_PARAMS_VALIDATE_CACHE_KEY})
    {
        return delete $spec->{MX_PARAMS_VALIDATE_CACHE_KEY};
    }
    else
    {
        return refaddr(caller_cv(2));
    }
}

sub _convert_to_param_validate_spec
{
    my $spec = shift;
    my %pv_spec;

    $pv_spec{optional} = $spec->{optional}
        if exists $spec->{optional};
    $pv_spec{default} = $spec->{default}
        if exists $spec->{default};
    $pv_spec{coerce} = $spec->{coerce}
        if exists $spec->{coerce};
    $pv_spec{depends} = $spec->{depends}
        if exists $spec->{depends};

    my $constraint;
    if (defined $spec->{isa})
    {
        $constraint = _is_tc($spec->{isa})
            || Mouse::Util::TypeConstraints::find_or_parse_type_constraint($spec->{isa})
            || class_type($spec->{isa});
    }
    elsif (defined $spec->{does})
    {
        $constraint = _is_tc($spec->{isa})
            || find_type_constraint($spec->{does})
            || role_type($spec->{does});
    }

    $pv_spec{callbacks} = $spec->{callbacks}
        if exists $spec->{callbacks};

    if ($constraint)
    {
        $pv_spec{constraint} = $constraint;
        $pv_spec{callbacks}{'checking type constraint for ' . $constraint->name}
            = sub { $constraint->check($_[0]) };
    }

    delete $pv_spec{coerce}
        unless $pv_spec{constraint} && $pv_spec{constraint}->has_coercion;

    return \%pv_spec;
}

sub _is_tc
{
    my $maybe_tc = shift;

    return $maybe_tc
        if defined $maybe_tc
            && blessed $maybe_tc
            && $maybe_tc->isa('Mouse::Meta::TypeConstraint');
}

sub _caller_name
{
    my $depth = shift || 0;
    return (caller(2 + $depth))[3];
}

=head1 AUTHOR

Mohammad S Anwar, C<< <mohammad.anwar at yahoo.com> >>

=head1 REPOSITORY

L<https://github.com/manwar/MouseX-Params-Validate>

=head1 CONTRIBUTORS

Hans Staugaard (STAUGAARD)

=head1 BUGS

Please  report  any  bugs  or feature requests to C<bug-mousex-params-validate at
rt.cpan.org>, or through the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=MouseX-Params-Validate>.
I  will be notified and then you'll automatically be notified of progress on your
bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc MouseX::Params::Validate

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=MouseX-Params-Validate>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/MouseX-Params-Validate>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/MouseX-Params-Validate>

=item * Search CPAN

L<http://search.cpan.org/dist/MouseX-Params-Validate/>

=back

=head1 ACKNOWLEDGEMENTS

=over 2

=item * Stevan Little <stevan.little@iinteractive.com> (Author of L<MooseX::Params::Validate>).

=item * Dave Rolsky E<lt>autarch@urth.orgE<gt> (Maintainer of L<MooseX::Params::Validate>).

=back

=head1 LICENSE AND COPYRIGHT

Copyright (C) 2011 - 2016 Mohammad S Anwar.

This  program  is  free software; you can redistribute it and/or modify it under
the  terms  of the the Artistic License (2.0). You may obtain a copy of the full
license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any  use,  modification, and distribution of the Standard or Modified Versions is
governed by this Artistic License.By using, modifying or distributing the Package,
you accept this license. Do not use, modify, or distribute the Package, if you do
not accept this license.

If your Modified Version has been derived from a Modified Version made by someone
other than you,you are nevertheless required to ensure that your Modified Version
 complies with the requirements of this license.

This  license  does  not grant you the right to use any trademark,  service mark,
tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge patent license
to make,  have made, use,  offer to sell, sell, import and otherwise transfer the
Package with respect to any patent claims licensable by the Copyright Holder that
are  necessarily  infringed  by  the  Package. If you institute patent litigation
(including  a  cross-claim  or  counterclaim) against any party alleging that the
Package constitutes direct or contributory patent infringement,then this Artistic
License to you shall terminate on the date that such litigation is filed.

Disclaimer  of  Warranty:  THE  PACKAGE  IS  PROVIDED BY THE COPYRIGHT HOLDER AND
CONTRIBUTORS  "AS IS'  AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES. THE IMPLIED
WARRANTIES    OF   MERCHANTABILITY,   FITNESS   FOR   A   PARTICULAR  PURPOSE, OR
NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY YOUR LOCAL LAW. UNLESS
REQUIRED BY LAW, NO COPYRIGHT HOLDER OR CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL,  OR CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE
OF THE PACKAGE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=cut

1; # End of MouseX::Params::Validate
