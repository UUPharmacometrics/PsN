package model::problem::thetapv;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::init_record';

no Moose;
__PACKAGE__->meta->make_immutable;
1;
