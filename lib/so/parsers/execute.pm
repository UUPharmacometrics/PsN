package so::parsers::execute;

# Package for parsing the results from an execute

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;

has 'rundir' => ( is => 'rw', isa => 'Str' );
has 'so' => ( is => 'rw', isa => 'so' );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'labels_hash' => ( is => 'rw', isa => 'Maybe[HashRef]' );

sub BUILD
{
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
