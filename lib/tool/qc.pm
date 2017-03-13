package tool::qc;

use strict;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use log;
use utils::file;
use tool::modelfit;
use tool::linearize;

extends 'tool';

has 'model' => ( is => 'rw', isa => 'model' );
has 'groups' => ( is => 'rw', isa => 'Int', default => 4 );       # The number of groups to use for quantiles in the time_varying model 
has 'idv' => ( is => 'rw', isa => 'Str', default => 'TIME' );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'CWRES' );
has 'dvid' => ( is => 'rw', isa => 'Str', default => 'DVID' );
has 'occ' => ( is => 'rw', isa => 'Str', default => 'OCC' );

sub BUILD
{
    my $self = shift;

	my $model = $self->models()->[0]; 
    $self->model($model);
}

sub modelfit_setup
{
	my $self = shift;

    my $linearized_model_name = $self->model->filename;
    $linearized_model_name =~ s/(\.[^.]+)$/_linbase.mod/;

    my $linearize = tool::linearize->new(
        eval($common_options::parameters),
        models => [ $self->model ],
        full_block => 1,
    );

    $linearize->run();
    $linearize->print_results();
    # FIXME: model is garbled here


}

sub modelfit_analyze
{
    my $self = shift;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
