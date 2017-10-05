package tool::qa;

use strict;
use Moose;
use MooseX::Params::Validate;
use File::Copy 'cp';
use include_modules;
use log;
use model_transformations;
use utils::file;
use tool::modelfit;
use tool::resmod;
use tool::linearize;
use tool::frem;
use tool::cdd;
use tool::simeval;
use PsN;

extends 'tool';

has 'model' => ( is => 'rw', isa => 'model' );
has 'groups' => ( is => 'rw', isa => 'Int', default => 10 );       # The number of groups to use for quantiles in the time_varying model
has 'idv' => ( is => 'rw', isa => 'Str', default => 'TIME' );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'CWRES' );
has 'dvid' => ( is => 'rw', isa => 'Str', default => 'DVID' );
has 'occ' => ( is => 'rw', isa => 'Str', default => 'OCC' );
has 'covariates' => ( is => 'rw', isa => 'Str' );       # A comma separated list of continuous covariate symbols
has 'categorical' => ( is => 'rw', isa => 'Str' );       # A comma separated list of categorical covariate symbols
has 'parameters' => ( is => 'rw', isa => 'Str' );       # A comma separated list of parameter symbols
has 'fo' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'cmd_line' => ( is => 'rw', isa => 'Str' );         # Used as a work around for calling scm via system
has 'nointer' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nonlinear' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'skip' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'only' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );    # Will be transformed into skip in BUILD
has 'add_etas' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'added_etas' => ( is => 'rw', isa => 'HashRef' );   # What parameters did get added etas and to what etas?

has 'resmod_idv_table' => ( is => 'rw', isa => 'Str' ); # The table used by resmod

sub BUILD
{
    my $self = shift;

	my $model = $self->models()->[0];
    $self->model($model);

    if (scalar(@{$self->skip}) > 0 and scalar(@{$self->only}) > 0) {
        die("Cannot have both skip and only\n");
    }

    if (scalar(@{$self->only}) > 0) {
        my %skip_hash = ( 'scm' => 1, 'frem' => 1, 'cdd' => 1, 'simeval' => 1, 'transform' => 1, 'resmod' => 1 );
        for my $section (@{$self->only}) {
            if (exists $skip_hash{$section}) {
                delete $skip_hash{$section};
            } else {
                die("only: Unknown section $section. Allowed are transform, scm, frem, cdd, simeval and resmod\n");
            }
        }
        $self->skip([keys %skip_hash]);
    }

    for my $skip (@{$self->skip}) {
        if ($skip !~ /^(transform|scm|frem|cdd|simeval|resmod)$/) {
            die("skip: Unknown section $skip. Allowed are transform, scm, frem, cdd, simeval and resmod\n");
        }
    }
}

sub modelfit_setup
{
	my $self = shift;
    my $model_copy = $self->model->copy(filename => $self->model->filename, directory => $self->model->directory, write_copy => 0, output_same_directory => 1);
    $model_copy->_write(filename => $self->directory . $self->model->filename);

    my @covariates;
    if (defined $self->covariates) {
        @covariates = split(',', $self->covariates);
    }
    my @categorical;
    if (defined $self->categorical) {
        @categorical = split(',', $self->categorical);
    }
    my $all_covariates = [ @covariates, @categorical ];

    $model_copy->problems->[0]->undrop_columns(columns => $all_covariates);

    $model_copy->phi_file($self->model->get_phi_file());

	my $vers = $PsN::version;
	my $dev = $PsN::dev;

    $model_copy->set_records(type => 'covariance', record_strings => [ "OMITTED" ]);

    my $base_model_name = $self->model->filename;
    if (not $self->nonlinear) {
        $base_model_name =~ s/(\.[^.]+)$/_linbase.mod/;
    
        print "*** Running linearize ***\n";
        ui->category('linearize');
	
    	my @table_columns = ( 'ID', $self->idv,'CWRES', 'PRED', 'CIPREDI','CPRED' );
	
        if ($model_copy->defined_variable(name => $self->dvid)) {
            push @table_columns, $self->dvid;
        } 
	
        if ($model_copy->defined_variable(name => 'TAD')) {
            push @table_columns, 'TAD';
        }

        my $lst_file;
        if (defined $self->lst_file) {
            $lst_file = '../../../' . $self->lst_file;
        }

        my $old_nm_output = common_options::get_option('nm_output');    # Hack to set clean further down
        common_options::set_option('nm_output', 'phi,ext,cov,cor,coi');
        my $linearize = tool::linearize->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $model_copy ],
            directory => 'linearize_run',
            estimate_fo => $self->fo,
            extra_table_columns => \@table_columns,
            lst_file => $lst_file,
            nointer => $self->nointer,
            keep_covariance => 1,
            nm_output => 'phi,ext,cov,cor,coi',
        );

        $linearize->run();
        $linearize->print_results();
        ui->category('qa');

        common_options::set_option('nm_output', $old_nm_output);
    }

    my $base_model;
    if (not $self->nonlinear) {
        $base_model = model->new(
            filename => $base_model_name,
        );
    } else {
        $base_model = $model_copy;
    }

    #if ($self->fo) {
    #    $base_model->remove_option(record_name => 'estimation', option_name => 'METHOD');
    #    $base_model->_write();
    #}

    if (not $self->_skipped('transform')) {
        print "*** Running full omega block, add etas, boxcox and tdist models ***\n";
        eval {
            mkdir "modelfit_run";
            my @models;
            my $full_block_model = $base_model->copy(directory => "modelfit_run", filename => "fullblock.mod", write_copy => 0);
            my $was_full_block = model_transformations::full_omega_block(model => $full_block_model);
            if (not $was_full_block) {
                $full_block_model->_write();
                push @models, $full_block_model;
            }
            my $boxcox_model = $base_model->copy(directory => "modelfit_run", filename => "boxcox.mod", write_copy => 0);
            my $zero_fix_omegas = model_transformations::find_zero_fix_omegas(model => $boxcox_model);
            my $etas_to_boxcox_tdist = model_transformations::remaining_omegas(model => $boxcox_model, omegas => $zero_fix_omegas);
            if (scalar(@$etas_to_boxcox_tdist) > 0) {
                model_transformations::boxcox_etas(model => $boxcox_model, etas => $etas_to_boxcox_tdist);
                $boxcox_model->_write();
                push @models, $boxcox_model;
            }
            my $tdist_model = $base_model->copy(directory => "modelfit_run", filename => "tdist.mod", write_copy => 0);
            if (scalar(@$etas_to_boxcox_tdist) > 0) {
                model_transformations::tdist_etas(model => $tdist_model, etas => $etas_to_boxcox_tdist);
                $tdist_model->_write();
                push @models, $tdist_model;
            }
            my $iov_etas = model_transformations::find_etas(model => $base_model, type => 'iov');
            if (scalar(@$iov_etas) == 0) {      # We don't have iov previously
                my $add_iov_model = $base_model->copy(directory => "modelfit_run", filename => "iov.mod", write_copy => 0);
                my $error = model_transformations::add_iov(model => $add_iov_model, occ => $self->occ);
                if (not $error) {
                    $add_iov_model->_write();
                    push @models, $add_iov_model;
                }
            }
            for my $model (@models) {       # Set output directory so that .lst file gets saved in the rundir
                $model->outputs->[0]->directory(".");
            }
            chdir "modelfit_run";
            my $modelfit = tool::modelfit->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => \@models,
                directory => "modelfit_dir1",
                top_tool => 1,
                so => 1,
                nm_output => 'ext',
            );
            $modelfit->run();
            chdir "..";
        };
        if ($@) {
            print $@;
        }
        $self->_to_qa_dir();
    }

    if (defined $self->add_etas and not $self->_skipped('transform')) {
        print "\n*** Running add_etas ***\n";
        mkdir "add_etas_run";
        my $add_etas_model = $self->model->copy(
            filename => $self->model->filename,
            directory => $self->model->directory,
            write_copy => 0,
            output_same_directory => 1,
        );
        my $added_etas = model_transformations::add_etas_to_parameters(model => $add_etas_model, parameters => $self->add_etas);
        $self->added_etas($added_etas);
        $add_etas_model->filename("add_etas.mod");
        $add_etas_model->_write(filename => $self->directory . 'add_etas_run/add_etas.mod');

        chdir("add_etas_run");
        ui->category('linearize');
        my $old_nm_output = common_options::get_option('nm_output');    # Hack to set clean further down
        common_options::set_option('nm_output', 'ext');
        eval {
            my $linearize = tool::linearize->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $add_etas_model ],
                directory => 'linearize_run',
                estimate_fo => $self->fo,
                nointer => $self->nointer,
                nm_output => 'ext',
            );
            $linearize->run();
            $linearize->print_results();
        };
        if ($@) {
            print $@;
        }
        ui->category('qa');
        common_options::set_option('nm_output', $old_nm_output);
    }

    $self->_to_qa_dir();

    if (defined $self->covariates or defined $self->categorical) {
        if (not $self->_skipped('frem')) {
            print "\n*** Running FREM ***\n";
            my $frem_model = model->new(filename => $base_model_name);

            my $old_clean = common_options::get_option('clean');    # Hack to set clean further down
            common_options::set_option('clean', 1);
            eval {
                my $frem = tool::frem->new(
                    %{common_options::restore_options(@common_options::tool_options)},
                    models => [ $frem_model ],
                    covariates => $all_covariates,
                    categorical => [ @categorical ],
                    directory => 'frem_run',
                    rescale => 1,
                    run_sir => 0,
                    rplots => 1,
                    top_tool => 1,
                    clean => 1,
                );
                $frem->run();
                $frem->print_options(   # To get skip_omegas over to postfrem
                    toolname => 'frem',
                    local_options => [ 'skip_omegas' ],
                    common_options => \@common_options::tool_options
                );
            };
            if ($@) {
                print $@;
            }
            common_options::set_option('clean', $old_clean);


            $self->_to_qa_dir();
            if (-d "frem_run") {
                print "\n*** Running POSTFREM ***\n";
                eval {
                    if ($dev) {
                        system("postfrem -frem_directory=frem_run -directory=postfrem_run -force_posdef_covmatrix");
                    } else {
                        system("postfrem-".$vers." -force_posdef_covmatrix -frem_directory=frem_run -directory=postfrem_run");
                    }
                };
                if ($@) {
                    print $@;
                }
            }
            $self->_to_qa_dir();
        }

        if (not $self->_skipped('scm') and defined $self->parameters) {
            print "\n*** Running scm ***\n";
            my $scm_model = $self->model->copy(filename => "m1/scm.mod");
            if ($self->model->is_run()) {
                cp($self->model->outputs->[0]->full_name(), 'm1/scm.lst');
            }
            model_transformations::add_tv(model => $scm_model, parameters => [split /,/, $self->parameters]);
            $scm_model->_write();
            $self->_create_scm_config(model_name => "m1/scm.mod");
            my %tool_options = %{common_options::restore_options(@common_options::tool_options)};
            my $scm_options = "";
            for my $cmd (split /\s+/, $self->cmd_line) {
                if ($cmd =~ /^--?/) {
                    my $option = $cmd;
                    $option =~ s/^--?(no-)?(.*)/$2/;
                    $option =~ s/=(.*)//;
                    if (grep { $_ eq $option } @common_options::tool_options) {
                        $scm_options .= " $cmd";
                    }
                }
            }
            my $fo = "";
            if ($self->fo) {
                $fo = "-estimate_fo";
            }
            my $nointer = "";
            if ($self->nointer) {
                $nointer = "-nointer";
            }
            my $nonlinear = "";
            if ($self->nonlinear) {
                $nonlinear = "-no-linearize -no-foce";
            }

            eval {
				if($dev) {
					system("scm config.scm $scm_options $fo $nointer $nonlinear");       # FIXME: system for now
				} else {
					system("scm-".$vers." config.scm $scm_options $fo $nointer $nonlinear");       # FIXME: system for now
				}
            };
            if ($@) {
                print $@;
            }
            $self->_to_qa_dir();
        }
    }

    if (not $self->_skipped('cdd')) {
        print "\n*** Running cdd ***\n";
        my $cdd_model = model->new(filename => $base_model_name);
        eval {
            my $cdd = tool::cdd->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $cdd_model ],
                directory => 'cdd_run',
                rplots => 1,
                etas => 1,
                top_tool => 1,
            );
            $cdd->run();
        };
        if ($@) {
            print $@;
        }
        $self->_to_qa_dir();
    }

    if (not $self->_skipped('simeval')) {
        print "\n*** Running simeval ***\n";
        my $simeval_model = $base_model->copy(filename => "m1/simeval.mod");
        $simeval_model->remove_records(type => 'etas');
        $simeval_model->remove_option(record_name => 'estimation', option_name => 'MCETA');
        $simeval_model->_write();
        eval {
            my $simeval = tool::simeval->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $simeval_model ],
                rplots => 1,
                n_simulation_models => 5,
                directory => "simeval_run",
                top_tool => 1,
            );
            $simeval->run();
        };
        if ($@) {
            print $@;
        }
        $self->_to_qa_dir();
    }

    if (not $self->_skipped('resmod')) {
        print "*** Running resmod ***\n";
        my $resmod_model;
        if (not $self->nonlinear) {
            $resmod_model = model->new(filename => 'linearize_run/scm_dir1/derivatives.mod');
        } else {
            $resmod_model = $model_copy;
        }

        my $resmod_idv;
        eval {
            $resmod_idv = tool::resmod->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $resmod_model ],
                dvid => $self->dvid,
                idv => $self->idv,
                dv => $self->dv,
                occ => $self->occ,
                groups => $self->groups,
                iterative => 0,
                directory => 'resmod_'.$self->idv,
                top_tool => 1,
            );
            $self->resmod_idv_table($resmod_idv->table_file);
        };
        if (not $@) {
            eval {
                $resmod_idv->run();
            };
        } else {
            print $@;
            rmdir "resmod_".$self->idv;
        }

        $self->_to_qa_dir();

        my $resmod_tad;
        eval {
            $resmod_tad = tool::resmod->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $resmod_model ],
                dvid => $self->dvid,
                idv => 'TAD',
                dv => $self->dv,
                occ => $self->occ,
                groups => $self->groups,
                iterative => 0,
                directory => 'resmod_TAD',
                top_tool => 1,
            );
        };
        if (not $@) {
            eval {
                $resmod_tad->run();
            };
        } else {
            print $@;
            rmdir 'resmod_TAD';
        }
        $self->_to_qa_dir();

        my $resmod_pred;
        eval {
            $resmod_pred = tool::resmod->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $resmod_model ],
                dvid => $self->dvid,
                idv => 'PRED',
                dv => $self->dv,
                occ => $self->occ,
                groups => $self->groups,
                iterative => 0,
                directory => 'resmod_PRED',
                top_rool => 1,
                clean => 0,         # Should not be need as top_tool is 1, but top_tool gets reset somehow for this run
            );
        };
        if (not $@) {
            eval {
                $resmod_pred->run();
            };
        } else {
            print $@;
            rmdir 'resmod_PRED';
        }
        $self->_to_qa_dir();
    }
}

sub modelfit_analyze
{
    my $self = shift;
}

sub _skipped
{
    my $self = shift;
    my $skip = shift;

    for my $a (@{$self->skip}) {
        if ($skip eq $a) {
            return 1;
        }
    }
    return 0;
}

sub _all_skipped_for_linearize
{
    my $self = shift;
    return $self->_skipped('scm') && $self->_skipped('frem') && $self->_skipped('cdd') && $self->_skipped('simeval') && $self->_skipped('transform');
}

sub _create_scm_config
{
    my $self = shift;
	my %parm = validated_hash(\@_,
        model_name => { isa => 'Str' }
    );
	my $model_name = $parm{'model_name'};

    open my $fh, '>', 'config.scm';

    #my $model_name = $self->model->full_name();

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

    my @covariates;
    if (defined $self->covariates) {
        @covariates = split(/,/, $self->covariates);
    }
    my @categorical;
    if (defined $self->categorical) {
        @categorical = split(/,/, $self->categorical);
    }
    my @parameters;
    if (defined $self->parameters) {
        @parameters = split(/,/, $self->parameters);
    }
	my $CWRES_table_path = $self->resmod_idv_table;
    if (defined $CWRES_table_path) {
	    $CWRES_table_path =~ s/\\/\//g;
    } else {
        $CWRES_table_path = "";
    }

    my $code =  [
            '# qa specific preamble',
			"groups <- " . $self->groups,
            "idv_name <- '" . $self->idv . "'",
			"dvid_name <- '" . $self->dvid . "'",
            "covariates <- " . rplots::create_r_vector(array => \@covariates),
            "categorical <- " . rplots::create_r_vector(array => \@categorical),
            "parameters <- " . rplots::create_r_vector(array => \@parameters),
            "CWRES_table <- '" . $CWRES_table_path . "'",
			"cdd_dofv_cutoff <- 3.84 ",
			"cdd_max_rows <- 10",
			"type <- 'latex' # set to 'html' if want to create a html file ",
            "skip <- " . rplots::create_r_vector(array => $self->skip),
        ];

    if (defined $self->added_etas) {
        my @content;
        for my $p (keys %{$self->added_etas}) {
            my $value = $self->added_etas->{$p};
            if (not defined $value) {
                $value = 'NULL';
            }
            push @content, "$p=$value";
        }
        my $add = 'added_etas <- list(' . join(', ', @content) . ')';
        push @$code, $add;
    }

    $rplot->add_preamble(code => $code);
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
