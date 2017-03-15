package tool::qa;

use strict;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use log;
use utils::file;
use tool::modelfit;
use tool::resmod;
use tool::linearize;
use tool::frem;
use tool::cdd;
use tool::simeval;

extends 'tool';

has 'model' => ( is => 'rw', isa => 'model' );
has 'groups' => ( is => 'rw', isa => 'Int', default => 4 );       # The number of groups to use for quantiles in the time_varying model 
has 'idv' => ( is => 'rw', isa => 'Str', default => 'TIME' );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'CWRES' );
has 'dvid' => ( is => 'rw', isa => 'Str', default => 'DVID' );
has 'occ' => ( is => 'rw', isa => 'Str', default => 'OCC' );
has 'covariates' => ( is => 'rw', isa => 'Str' );       # A comma separated list of covariate symbols

sub BUILD
{
    my $self = shift;

	my $model = $self->models()->[0]; 
    $self->model($model);
}

sub modelfit_setup
{
	my $self = shift;

#    print "*** Running resmod ***\n";
#    my $resmod = tool::resmod->new(
#        eval( $common_options::parameters ),
#        models => [ $self->model ],
#        dvid => $self->dvid,
#        idv => $self->idv,
#        dv => $self->dv,
#        occ => $self->occ,
#        groups => $self->groups,
#        iterative => 0,
#    );
#    $resmod->run();

    print "*** Running linearize ***\n";
    my $linearized_model_name = $self->model->filename;
    $linearized_model_name =~ s/(\.[^.]+)$/_linbase.mod/;

    my $linearize = tool::linearize->new(
        eval($common_options::parameters),
        models => [ $self->model ],
        full_block => 1,
        directory => 'linearize_run',
    );

    $linearize->run();
    $linearize->print_results();
    # FIXME: model is garbled here

    my $linearized_model = model->new(
        filename => $linearized_model_name,
    );

    print "*** Running boxcox transformed model ***\n";
    my $boxcox_model = $linearized_model->copy(filename => "boxcox.mod");
    $boxcox_model->boxcox_etas();
    my $modelfit = tool::modelfit->new(
        eval($common_options::parameters),
        models => [ $boxcox_model ],
        directory => 'boxcox_run',
    );
    $modelfit->run();
    unlink("boxcox.mod");

    if (defined $self->covariates) {
        print "*** Running FREM ***\n";
        my $frem_model = model->new(filename => $linearized_model_name);
        my $frem = tool::frem->new(
            eval($common_options::parameters),
            models => [ $frem_model ],
            covariates => [ split(',', $self->covariates) ],
            directory => 'frem_run',
            rescale => 1, 
            run_sir => 1, 
            rplots => 1,
        ); 
        $frem->run();
    }

    print "*** Running cdd ***\n";
    my $cdd_model = model->new(filename => $linearized_model_name);
    my $cdd = tool::cdd->new(
        eval($common_options::parameters),
        models => [ $cdd_model ],
        directory => 'cdd_run',
        rplots => 1,
    );
    $cdd->run();

    print "*** Running simeval ***\n";
    my $simeval_model = model->new(filename => $linearized_model_name);
    my $simeval = tool::simeval->new(
        eval($common_options::parameters),
        models => [ $simeval_model ],
        rplots => 1,
        lst_file => "pheno_linbase.lst",
        n_simulation_models => 5,
        directory => "simeval_dir",
    );
    $simeval->run();
}

sub modelfit_analyze
{
    my $self = shift;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
