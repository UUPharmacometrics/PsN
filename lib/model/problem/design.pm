package model::problem::design;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

no Moose;
__PACKAGE__->meta->make_immutable;
1;
