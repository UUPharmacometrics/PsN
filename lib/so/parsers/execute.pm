package so::parsers::execute;

# Package for parsing the results from an execute

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use include_modules;

has 'rundir' => ( is => 'rw', isa => 'Str' );
has 'so' => ( is => 'rw', isa => 'so' );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'labels_hash' => ( is => 'rw', isa => 'Maybe[HashRef]' );

sub BUILD
{
}

1;
