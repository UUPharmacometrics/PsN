package Mouse::Exporter;
use strict;
use warnings;
use Carp ();

my %SPEC;

# it must be "require", because Mouse::Util depends on Mouse::Exporter,
# which depends on Mouse::Util::import()
require Mouse::Util;

sub import{
    strict->import;
    warnings->import('all', FATAL => 'recursion');
    return;
}


sub setup_import_methods{
    my($class, %args) = @_;

    my $exporting_package = $args{exporting_package} ||= caller();

    my($import, $unimport) = $class->build_import_methods(%args);

    Mouse::Util::install_subroutines($exporting_package,
        import   => $import,
        unimport => $unimport,

        export_to_level => sub {
            my($package, $level, undef, @args) = @_; # the third argument is redundant
            $package->import({ into_level => $level + 1 }, @args);
        },
        export => sub {
            my($package, $into, @args) = @_;
            $package->import({ into => $into }, @args);
        },
    );
    return;
}

sub build_import_methods{
    my($self, %args) = @_;

    my $exporting_package = $args{exporting_package} ||= caller();

    $SPEC{$exporting_package} = \%args;

    # canonicalize args
    my @export_from;
    if($args{also}){
        my %seen;
        my @stack = ($exporting_package);

        while(my $current = shift @stack){
            push @export_from, $current;

            my $also = $SPEC{$current}{also} or next;
            push @stack, grep{ !$seen{$_}++ } ref($also) ? @{ $also } : $also;
        }
    }
    else{
        @export_from = ($exporting_package);
    }

    my %exports;
    my @removables;
    my @all;

    my @init_meta_methods;

    foreach my $package(@export_from){
        my $spec = $SPEC{$package} or next;

        if(my $as_is = $spec->{as_is}){
            foreach my $thingy (@{$as_is}){
                my($code_package, $code_name, $code);

                if(ref($thingy)){
                    $code = $thingy;
                    ($code_package, $code_name) = Mouse::Util::get_code_info($code);
                }
                else{
                    $code_package = $package;
                    $code_name    = $thingy;
                    no strict 'refs';
                    $code         = \&{ $code_package . '::' . $code_name };
               }

                push @all, $code_name;
                $exports{$code_name} = $code;
                if($code_package eq $package){
                    push @removables, $code_name;
                }
            }
        }

        if(my $init_meta = $package->can('init_meta')){
            if(!grep{ $_ == $init_meta } @init_meta_methods){
                push @init_meta_methods, $init_meta;
            }
        }
    }
    $args{EXPORTS}    = \%exports;
    $args{REMOVABLES} = \@removables;

    $args{groups}{all} ||= \@all;

    if(my $default_list = $args{groups}{default}){
        my %default;
        foreach my $keyword(@{$default_list}){
            $default{$keyword} = $exports{$keyword}
                || Carp::confess(qq{The $exporting_package package does not export "$keyword"});
        }
        $args{DEFAULT} = \%default;
    }
    else{
        $args{groups}{default} ||= \@all;
        $args{DEFAULT}           = $args{EXPORTS};
    }

    if(@init_meta_methods){
        $args{INIT_META} = \@init_meta_methods;
    }

    return (\&do_import, \&do_unimport);
}

# the entity of general import()
sub do_import {
    my($package, @args) = @_;

    my $spec = $SPEC{$package}
        || Carp::confess("The package $package package does not use Mouse::Exporter");

    my $into = _get_caller_package(ref($args[0]) ? shift @args : undef);

    my @exports;
    my @traits;

    while(@args){
        my $arg = shift @args;
        if($arg =~ s/^-//){
            if($arg eq 'traits'){
                push @traits, ref($args[0]) ? @{shift(@args)} : shift(@args);
            }
            else {
                Mouse::Util::not_supported("-$arg");
            }
        }
        elsif($arg =~ s/^://){
            my $group = $spec->{groups}{$arg}
                || Carp::confess(qq{The $package package does not export the group "$arg"});
            push @exports, @{$group};
        }
        else{
            push @exports, $arg;
        }
    }

    strict->import;
    warnings->import('all', FATAL => 'recursion');

    if($spec->{INIT_META}){
        my $meta;
        foreach my $init_meta(@{$spec->{INIT_META}}){
            $meta = $package->$init_meta(for_class => $into);
        }

        if(@traits){
            my $type = (split /::/, ref $meta)[-1]; # e.g. "Class" for "My::Meta::Class"
            @traits = map{
              ref($_)
                ? $_
                : Mouse::Util::resolve_metaclass_alias($type => $_, trait => 1)
            } @traits;

            require Mouse::Util::MetaRole;
            Mouse::Util::MetaRole::apply_metaroles(
                for => $into,
                Mouse::Util::is_a_metarole($into->meta)
                    ? (role_metaroles  => { role  => \@traits })
                    : (class_metaroles => { class => \@traits }),
            );
        }
    }
    elsif(@traits){
        Carp::confess("Cannot provide traits when $package does not have an init_meta() method");
    }

    if(@exports){
        my @export_table;
        foreach my $keyword(@exports){
            push @export_table,
                $keyword => ($spec->{EXPORTS}{$keyword}
                    || Carp::confess(qq{The $package package does not export "$keyword"})
                );
        }
        Mouse::Util::install_subroutines($into, @export_table);
    }
    else{
        Mouse::Util::install_subroutines($into, %{$spec->{DEFAULT}});
    }
    return;
}

# the entity of general unimport()
sub do_unimport {
    my($package, $arg) = @_;

    my $spec = $SPEC{$package}
        || Carp::confess("The package $package does not use Mouse::Exporter");

    my $from = _get_caller_package($arg);

    my $stash = do{
        no strict 'refs';
        \%{$from . '::'}
    };

    for my $keyword (@{ $spec->{REMOVABLES} }) {
        next if !exists $stash->{$keyword};
        my $gv = \$stash->{$keyword};

        # remove what is from us
        if(ref($gv) eq 'GLOB' && *{$gv}{CODE} == $spec->{EXPORTS}{$keyword}){
            delete $stash->{$keyword};
        }
    }
    return;
}

sub _get_caller_package {
    my($arg) = @_;

    # We need one extra level because it's called by import so there's a layer
    # of indirection
    if(ref $arg){
        return defined($arg->{into})       ? $arg->{into}
             : defined($arg->{into_level}) ? scalar caller(1 + $arg->{into_level})
             :                               scalar caller(1);
    }
    else{
        return scalar caller(1);
    }
}

1;
__END__

=head1 NAME

Mouse::Exporter - make an import() and unimport() just like Mouse.pm

=head1 VERSION

This document describes Mouse version v2.5.10

=head1 SYNOPSIS

    package MyApp::Mouse;

    use Mouse ();
    use Mouse::Exporter;

    Mouse::Exporter->setup_import_methods(
      as_is => [ 'has_rw', 'other_sugar', \&Some::Random::thing ],
      also  => 'Mouse',
    );

    sub has_rw {
        my $meta = caller->meta;
        my ( $name, %options ) = @_;
        $meta->add_attribute(
          $name,
          is => 'rw',
          %options,
        );
    }

    # then later ...
    package MyApp::User;

    use MyApp::Mouse;

    has 'name';
    has_rw 'size';
    thing;

    no MyApp::Mouse;

=head1 DESCRIPTION

This module encapsulates the exporting of sugar functions in a
C<Mouse.pm>-like manner. It does this by building custom C<import>,
C<unimport> methods for your module, based on a spec you provide.

Note that C<Mouse::Exporter> does not provide the C<with_meta> option,
but you can easily get the metaclass by C<< caller->meta >> as L</SYNOPSIS> shows.

=head1 METHODS

=head2 C<< setup_import_methods( ARGS ) >>

=head2 C<< build_import_methods( ARGS ) -> (\&import, \&unimport) >>

=head1 SEE ALSO

L<Moose::Exporter>

=cut

