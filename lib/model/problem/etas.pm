package model::problem::etas;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

no Moose;
__PACKAGE__->meta->make_immutable;
1;
