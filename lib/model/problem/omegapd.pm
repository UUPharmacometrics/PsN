package model::problem::omegapd;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::theta';

no Moose;
__PACKAGE__->meta->make_immutable;
1;
