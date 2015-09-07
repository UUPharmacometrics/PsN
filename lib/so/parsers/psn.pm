package so::parsers::psn;

# Package for handling psn input to nmoutput2so.

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;

sub BUILD
{
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
