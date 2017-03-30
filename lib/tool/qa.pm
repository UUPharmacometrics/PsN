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
has 'covariates' => ( is => 'rw', isa => 'Str' );       # A comma separated list of continuous covariate symbols
has 'categorical' => ( is => 'rw', isa => 'Str' );       # A comma separated list of categorical covariate symbols
has 'parameters' => ( is => 'rw', isa => 'Str' );       # A comma separated list of parameter symbols

sub BUILD
{
    my $self = shift;

	my $model = $self->models()->[0]; 
    $self->model($model);
}

sub modelfit_setup
{
	my $self = shift;

    my $model_copy = $self->model->copy(filename => $self->model->filename, directory => $self->model->directory, output_same_directory => 1);
    $model_copy->phi_file($self->model->get_phi_file());
    print "*** Running linearize ***\n";
    my $linearized_model_name = $self->model->filename;
    $linearized_model_name =~ s/(\.[^.]+)$/_linbase.mod/;
    ui->category('linearize');
    my $linearize = tool::linearize->new(
        %{common_options::restore_options(@common_options::tool_options)},
        models => [ $model_copy ],
        directory => 'linearize_run',
    );

    $linearize->run();
    $linearize->print_results();
    ui->category('qa');

    my $linearized_model = model->new(
        filename => $linearized_model_name,
    );

    print "*** Running full omega block, add etas and boxcox model ***\n";
    eval {
        my $full_block_model = $linearized_model->copy(filename => "fullblock.mod");
        $full_block_model->full_omega_block();
        my $boxcox_model = $linearized_model->copy(filename => "boxcox.mod");
        $boxcox_model->boxcox_etas();
        my $add_etas_model = $linearized_model->copy(filename => "add_etas.mod");
        $add_etas_model->unfix_omega_0_fix();

        my $modelfit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $full_block_model, $boxcox_model, $add_etas_model ],
            directory => 'modelfit_run',
        );
        $modelfit->run();
    };
    $self->_to_qa_dir();

    if (defined $self->covariates or defined $self->categorical) {
        print "\n*** Running FREM ***\n";
        my $frem_model = model->new(filename => $linearized_model_name);
        eval {
            my @covariates; 
            if (defined $self->covariates) {
                @covariates = split(',', $self->covariates);
            }
            my @categorical;
            if (defined $self->categorical) {
                @categorical = split(',', $self->categorical);
            }
            my $frem = tool::frem->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $frem_model ],
                covariates => [ @covariates, @categorical ],
                directory => 'frem_run',
                rescale => 1, 
                run_sir => 1, 
                rplots => 1,
            ); 
            $frem->run();
        };
        $self->_to_qa_dir();

        if (defined $self->parameters) {
            print "\n*** Running scm ***\n";
            $self->_create_scm_config(model_name => $linearized_model_name);
            eval {
                system("scm config.scm");       # FIXME: cheating for now
            };
            $self->_to_qa_dir();
        }
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
    $self->_to_qa_dir();

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
    $self->_to_qa_dir();

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

    $self->_to_qa_dir();

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
    $self->_to_qa_dir();

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
    $self->_to_qa_dir();

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

    my $covariates = "";
    if (defined $self->covariates) {
        $covariates = "continuous_covariates=" . $self->covariates;
    }

    my $categorical = "";
    if (defined $self->categorical) {
        $categorical = "categorical_covariates=" . $self->categorical;
    }

    my $all = "";
    if (defined $self->categorical and not defined $self->covariates) {
        $all = $self->categorical;
    } elsif (not defined $self->categorical and defined $self->covariates) {
        $all = $self->covariates;
    } else {
        $all = $self->covariates . ',' . $self->categorical;
    }
  
    my $relations;
    for my $param (split(',', $self->parameters)) {
        $relations .= "$param=" . $all . "\n";
    }

my $content = <<"END";
model=$model_name

directory=scm_run

search_direction=forward
linearize=1
foce=1

p_forward=0.05
max_steps=1
;p_backward=0.01

$covariates
$categorical

do_not_drop=$all


[test_relations]
$relations

[valid_states]
continuous = 1,4
categorical = 1,2
END
	print $fh $content;
    close $fh;
}

sub _to_qa_dir
{
    my $self = shift;

    chdir $self->directory;
}

sub create_R_plots_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
        rplot => { isa => 'rplots', optional => 0 }
    );
	my $rplot = $parm{'rplot'};

	$rplot->pdf_title('Quality assurance');

    $rplot->add_preamble(
        code => [
            '# qa specific preamble',
            "idv <- '" . $self->idv . "'",
            "covariates <- " . rplots::create_r_vector(array => [split(/,/, $self->covariates)]),
            "categorical <- " . rplots::create_r_vector(array => [split(/,/, $self->categorical)]),
        ]
    );
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
