package model::problem::aesinitial;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::code_record';

no Moose;
__PACKAGE__->meta->make_immutable;
1;
