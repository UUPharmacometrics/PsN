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

    print "*** Running resmod ***\n";
    my $resmod_idv;
    eval {
        $resmod_idv = tool::resmod->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $self->model ],
            dvid => $self->dvid,
            idv => $self->idv,
            dv => $self->dv,
            occ => $self->occ,
            groups => $self->groups,
            iterative => 0,
            directory => 'resmod_idv',
        );
    };
    if (not $@) {
        eval {
            $resmod_idv->run();
        };
    } else {
        rmdir "resmod_idv";
    }

    my $resmod_tad;
    eval {
        $resmod_tad = tool::resmod->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $self->model ],
            dvid => $self->dvid,
            idv => 'TAD',
            dv => $self->dv,
            occ => $self->occ,
            groups => $self->groups,
            iterative => 0,
            directory => 'resmod_TAD',
        );
    };
    if (not $@) {
        eval {
            $resmod_tad->run();
        };
    } else {
        rmdir 'resmod_TAD';
    }

    my $resmod_pred;
    eval {
        $resmod_pred = tool::resmod->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $self->model ],
            dvid => $self->dvid,
            idv => 'PRED',
            dv => $self->dv,
            occ => $self->occ,
            groups => $self->groups,
            iterative => 0,
            directory => 'resmod_PRED',
        );
    };
    if (not $@) {
        eval {
            $resmod_pred->run();
        };
    } else {
        rmdir 'resmod_PRED';
    }

    my $model_copy = $self->model->copy(filename => $self->model->filename );
    print "*** Running linearize ***\n";
    my $linearized_model_name = $self->model->filename;
    $linearized_model_name =~ s/(\.[^.]+)$/_linbase.mod/;

    ui->category('linearize');
    my $linearize = tool::linearize->new(
        %{common_options::restore_options(@common_options::tool_options)},
        models => [ $model_copy ],
        full_block => 1,
        directory => 'linearize_run',
    );

    $linearize->run();
    $linearize->print_results();
    ui->category('qa');

    my $linearized_model = model->new(
        filename => $linearized_model_name,
    );

    print "*** Running boxcox transformed model ***\n";
    my $boxcox_model = $linearized_model->copy(filename => "boxcox.mod");
    $boxcox_model->boxcox_etas();
    eval {
        my $modelfit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $boxcox_model ],
            directory => 'boxcox_run',
        );
        $modelfit->run();
    };
    unlink("boxcox.mod");

    if (defined $self->covariates) {
        print "\n*** Running FREM ***\n";
        my $frem_model = model->new(filename => $linearized_model_name);
        eval {
            my $frem = tool::frem->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $frem_model ],
                covariates => [ split(',', $self->covariates) ],
                directory => 'frem_run',
                rescale => 1, 
                run_sir => 1, 
                rplots => 1,
            ); 
            $frem->run();
        };

        print "\n*** Running scm ***\n";
        $self->_create_scm_config();
        eval {
            system("scm config.scm");       # FIXME: cheating for now
        };
    }
    print "\n*** Running cdd ***\n";
    my $cdd_model = model->new(filename => $linearized_model_name);
    eval {
        my $cdd = tool::cdd->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $cdd_model ],
            directory => 'cdd_run',
            rplots => 1,
        );
        $cdd->run();
    };

    print "\n*** Running simeval ***\n";
    my $simeval_model = model->new(filename => $linearized_model_name);
    eval {
        my $simeval = tool::simeval->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $simeval_model ],
            rplots => 1,
            n_simulation_models => 5,
            directory => "simeval_run",
        );
        $simeval->run();
    };
}

sub modelfit_analyze
{
    my $self = shift;
}

sub _create_scm_config
{
    my $self = shift;
    open my $fh, '>', 'config.scm';

	my $model_name = $self->model->full_name();
	my $covariates = $self->covariates;

my $content = <<"END";
model=$model_name

directory=scm_run

search_direction=forward
linearize=1
foce=1

p_forward=0.05
max_steps=1
;p_backward=0.01

continuous_covariates=$covariates

do_not_drop=$covariates


[test_relations]
CL=$covariates
V=$covariates

[valid_states]
continuous = 1,4
categorical = 1,2
END
	print $fh $content;
    close $fh;
}

sub create_R_plots_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
        rplot => { isa => 'rplots', optional => 0 }
    );
	my $rplot = $parm{'rplot'};

	$rplot->pdf_title('Quality assurance');
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
