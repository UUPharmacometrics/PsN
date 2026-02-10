package tool::qa;

use strict;
use random;
use Mouse;
use MouseX::Params::Validate;
use File::Copy 'copy';
use File::Spec;
use include_modules;
use array;
use model_transformations;
use filter_data;
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
has 'dvid' => ( is => 'rw', isa => 'Str' );
has 'occ' => ( is => 'rw', isa => 'Str' );
has 'continuous' => ( is => 'rw', isa => 'Str' );       # A comma separated list of continuous covariate symbols
has 'categorical' => ( is => 'rw', isa => 'Str' );       # A comma separated list of categorical covariate symbols
has 'parameters' => ( is => 'rw', isa => 'Str' );       # A comma separated list of parameter symbols. Currently private
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'cmd_line' => ( is => 'rw', isa => 'Str' );         # Used as a work around for calling scm via system
has 'nonlinear' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'skip' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );    # Will be transformed into _tools_to run in BUILD
has 'only' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has '_tools_to_run' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has '_tools_to_skip' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'add_etas' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'added_etas' => ( is => 'rw', isa => 'HashRef' );   # What parameters did get added etas and to what etas?
has 'iov_structure' => ( is => 'rw', isa => 'ArrayRef' );   # The occ/iov structure for the r code
has 'orig_max0_model_path' => ( is => 'rw', isa => 'Str' );
has 'base_model_path' => ( is => 'rw', isa => 'Str' );
has 'base_model' => ( is => 'rw', isa => 'model' );
has 'base_dataset_path' => ( is => 'rw', isa => 'Str' );
has 'extra_table_columns' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'nm_parallel' => ( is => 'rw', isa => 'Str' );  # Should a special NONMEM version be used for frem and linearize?
has 'estimation_options' => ( is => 'rw', isa => 'Str' );
has '_special_tool_options' => ( is => 'rw', isa => 'Maybe[HashRef]', default => undef); # All tool options to override command line


sub BUILD
{
    select(STDERR);     # Turn on autoflush to simplify fault-finding
    $| = 1;
    select(STDOUT);
    $| = 1;
    my $self = shift;

    my $model = $self->models()->[0];
    $self->model($model);

    if (not $self->nonlinear) {     # Allow more features for nonlinear exploration
        $self->check_nonsupported_modelfeatures();
    }

    for my $param (@{$self->add_etas}) {
        if (not code_parsing::defined_symbol(model => $model, symbol => $param)) {
            die("Parameter $param in add_etas not defined in model code\n");
        }
    }

    if (scalar(@{$self->skip}) > 0 and scalar(@{$self->only}) > 0) {
        die("Cannot have both skip and only\n");
    }

    my %all_tools = ( 'scm' => 1, 'frem' => 1, 'cdd' => 1, 'simeval' => 1, 'transform' => 1, 'resmod' => 1 );
    for my $skipped (@{$self->skip}) {
        if (not array::string_in($skipped, [keys %all_tools])) {
            die("skip: Unknown section $skipped. Allowed are transform, scm, frem, cdd, simeval and resmod\n");
        }
    }
    for my $only (@{$self->skip}) {
        if (not array::string_in($only, [keys %all_tools])) {
            die("only: Unknown section $only. Allowed are transform, scm, frem, cdd, simeval and resmod\n");
        }
    }

    my %steps_to_run = %all_tools;
    for my $tool (keys %all_tools) {
        if (scalar(@{$self->skip}) > 0) {
            if (array::string_in($tool, $self->skip)) {
                $steps_to_run{$tool} = 0;
            }
        } elsif (scalar(@{$self->only})) {
            if (not array::string_in($tool, $self->only)) {
                $steps_to_run{$tool} = 0;
            }
        }
    }

    # If frem is requested we also would need transform to get the full block OFV
    if ($steps_to_run{'frem'} and not $steps_to_run{'transform'}) {
        print "As frem was requested transform is added to be able to calculate the frem OFV.\n";
        $steps_to_run{'transform'} = 1;
    }

    $self->_tools_to_run(\%steps_to_run);
    my @tools_to_skip;      # Rplots needs to know the skipped tools
    for my $tool (keys %all_tools) {
        if (not $steps_to_run{$tool}) {
            push @tools_to_skip, $tool;
        }
    }
    $self->_tools_to_skip(\@tools_to_skip);

    if (defined $self->nm_parallel) {
        $self->_special_tool_options({});
        my $special_options_section = $PsN::config->{'default_options_' . $self->nm_parallel};
        if (defined $special_options_section) {
            $self->_special_tool_options($special_options_section);
        }
        $self->_special_tool_options->{'clean'} = 1;
        if (defined $self->template_directory_rplots) {
            $self->_special_tool_options->{'template_directory_rplots'} = $self->template_directory_rplots;
        }
        if (defined $self->zip) {
            $self->_special_tool_options->{'zip'} = $self->zip;
        }
        if (defined $self->nmfe) {
            $self->_special_tool_options->{'nmfe'} = $self->nmfe;
        }
        if (defined $self->nm_output) {
            $self->_special_tool_options->{'nm_output'} = $self->nm_output;
        }
        if (defined $self->threads) {
            $self->_special_tool_options->{'nodes'} = $self->threads;
        }
        $self->_special_tool_options->{'threads'} = 1;
        $self->_special_tool_options->{'nm_version'} = $self->nm_parallel;
    }
}

sub modelfit_setup
{
    my $self = shift;

    $self->default_update_inits(lst_file => $self->lst_file, model => $self->model);

    my $model_copy = $self->model->copy(
        filename => $self->model->filename,
        directory => $self->model->directory,
        write_copy => 0,
        copy_output => 1,
        output_same_directory => 1
    );

    my $data = data->new(
        filename => $model_copy->datafiles(absolute_path => 1)->[0],
        ignoresign => defined $model_copy->ignoresigns ? $model_copy->ignoresigns->[0] : undef,
        missing_data_token => $self->missing_data_token,
        idcolumn => $model_copy->idcolumns->[0],
    );

    if (not $data->have_unique_ids()) {
        $data->renumber_ids();
        $data->_write(filename => 'renumbered.dta');
        $model_copy->datafiles(new_names => ['renumbered.dta']);
        print "Warning: The dataset does not have unique ids. Dataset has been renumbered starting from 1\n";
    }

    $model_copy->_write(filename => $self->directory . $self->model->filename);

    my @covariates;
    if (defined $self->continuous) {
        @covariates = split(',', $self->continuous);
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

    my @table_columns = ( 'ID', 'CWRES', 'PRED', 'CIPREDI','CPRED' );

    if ($model_copy->defined_variable(name => 'TIME')) {
        push @table_columns, 'TIME';
    }
    if ($model_copy->defined_variable(name => 'TAD')) {
        push @table_columns, 'TAD';
    }
    if (($self->idv ne 'TIME') and ($self->idv ne 'TAD')) {
        push @table_columns, $self->idv;
    }

    if (defined $self->dvid and $model_copy->defined_variable(name => $self->dvid)) {
        push @table_columns, $self->dvid;
    }

    $self->extra_table_columns(\@table_columns);

    my $base_model_name = $self->model->filename;
    my $eval_model;

    my $derived_covariates = filter_data::derived_covariates_columns(model => $model_copy, columns => $all_covariates);

    if ($self->nonlinear) {
        $eval_model = $self->model->copy(filename => $self->model->filename, directory => $self->directory, write_copy => 0, output_same_directory => 0);
        my @extra_tablestrings = ( @table_columns, 'NOPRINT', 'NOAPPEND', 'ONEHEADER', 'FILE=extra_table' );
        $eval_model->remove_records(type => 'table');
        $eval_model->add_records(type => 'table', record_strings => \@extra_tablestrings);
        $eval_model->_write(filename => $self->directory . $self->model->filename);
        my $eval_model_to_run = $self->model->copy(filename => $eval_model->filename, directory => $eval_model->directory, write_copy => 0, output_same_directory => 0);
        $eval_model_to_run->set_maxeval_zero();
        $eval_model_to_run->outputs->[0]->directory($self->directory);
        my $modelfit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $eval_model_to_run ],
            directory => "eval_run",
            top_tool => 1,
            nm_output => 'ext,phi',
            model_subdir => 0,
        );
        $modelfit->run();

        # Add phi file generated from evaluation run to model
        $eval_model->init_etas();
        $eval_model->_write(filename => $self->directory . $self->model->filename);

        $self->base_model_path($eval_model->directory . $eval_model->filename);
        $self->orig_max0_model_path($self->base_model_path);

        filter_data::filter_dataset(model => $eval_model, force => 1);
    } else {
        $base_model_name =~ s/(\.[^.]+)$/_linbase.mod/;

        print "*** Running linearize ***\n";
        ui->category('linearize');

        my $old_nm_output = common_options::get_option('nm_output');    # Hack to set clean further down
        common_options::set_option('nm_output', 'phi,ext,cov,cor,coi');
        my $linearize = tool::linearize->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $model_copy ],
            directory => 'linearize_run',
            extra_table_columns => \@table_columns,
            keep_covariance => 1,
            nm_output => 'phi,ext,cov,cor,coi',
            extra_data_columns => $derived_covariates,
            estimation_options => $self->estimation_options,
        );

        $linearize->run();
        $linearize->print_results();
        PsN::call_pharmpy("psn linearize linearize_run");      # Generate results.json and results.csv
        ui->category('qa');

        common_options::set_option('nm_output', $old_nm_output);
    }

    my $base_model;
    if (not $self->nonlinear) {
        $base_model = model->new(
            filename => $base_model_name,
        );

        my $outobj = $base_model->outputs->[0];
        my $failed = 0;
        if (not defined $outobj->problems) {
            $failed = 1;
        } else {
            ($failed, undef) = $outobj->nonmem_run_failed;
        }
        if ($failed) {
            print "\nERROR: Linearization failed. Stopping qa.\n";
            exit;
        }

        $self->base_model_path($base_model->directory . $base_model->filename);
        $self->orig_max0_model_path($base_model->directory . 'linearize_run/scm_dir1/derivatives.mod');
        $self->base_dataset_path($base_model->problems->[0]->datas->[0]->get_absolute_filename());
        $base_model->init_etas(full_path => 1);
    } else {
        $base_model = $model_copy;
        $self->base_dataset_path($self->directory . 'preprocess_data_dir/filtered.dta');
    }
    $self->base_model($base_model);

    my $lin_data = data->new(
        filename => $self->base_dataset_path,
        ignoresign => '@',
        idcolumn => $base_model->idcolumns->[0],
    );

    my @categoricals_to_check = @categorical;
    push @categoricals_to_check, $self->occ if defined $self->occ;
    push @categoricals_to_check, $self->dvid if defined $self->dvid;
    for my $cat (@categoricals_to_check) {
        my $factors = $lin_data->factors(column_head => $cat);
        my $n = scalar(keys %$factors);
        if ($n > 10) {
            die "Error: Too many different values ($n) of categorical variable $cat. Exiting\n"
        }
    }

    my $numids = scalar(@{$lin_data->individuals});
    if ($numids < 2 and $self->_tools_to_run->{'cdd'}) {   # Skip cdd if only one individual
        print "Warning: Only one individual in dataset. Will skip cdd\n";
        $self->_tools_to_run->{'cdd'} = 0;
        push @{$self->_tools_to_skip}, 'cdd';
    }

    # Find time_varying covariates (assuming a filtered dataset)
    my @time_varying;
    COV_LOOP: for my $cov (@covariates) {
        my $index = $lin_data->column_head_indices->{$cov} - 1;
        for my $ind (@{$lin_data->individuals}) {
            my @cov_values;
            for my $row (@{$ind->subject_data}) {
                my @a = split ',', $row;
                my $e = $a[$index];
                push @cov_values, $e;
            }
            my $unique = array::unique(\@cov_values);
            if (scalar(@$unique) > 1) {
                push @time_varying, $cov;
                next COV_LOOP;
            }
        }
    }

    if ($self->_tools_to_run->{'transform'}) {
        print "*** Running full omega block, boxcox and tdist models ***\n";
        eval {
            mkdir "modelfit_run";
            my @models;
            my @simulation_models;
            my $full_block_model = $base_model->copy(directory => "modelfit_run", filename => "fullblock.mod", write_copy => 0);
            my $was_full_block = model_transformations::full_omega_block(model => $full_block_model);
            if (not $was_full_block) {
                set_mceta($full_block_model, 10);
                push @simulation_models, simulation_model($full_block_model);
                $full_block_model->_write();
                push @models, $full_block_model;
            }
            my $boxcox_model = $base_model->copy(directory => "modelfit_run", filename => "boxcox.mod", write_copy => 0);
            set_mceta($boxcox_model, 10);
            my $zero_fix_omegas = model_transformations::find_zero_fix_omegas(model => $boxcox_model);
            my $etas_to_boxcox_tdist = model_transformations::remaining_omegas(model => $boxcox_model, omegas => $zero_fix_omegas);
            if (scalar(@$etas_to_boxcox_tdist) > 0) {
                model_transformations::boxcox_etas(model => $boxcox_model, etas => $etas_to_boxcox_tdist);
                model_transformations::set_size(model => $boxcox_model, size => 'DIMNEW', value => -10000);
                push @simulation_models, simulation_model($boxcox_model);
                $boxcox_model->_write();
                push @models, $boxcox_model;
            }
            my $tdist_model = $base_model->copy(directory => "modelfit_run", filename => "tdist.mod", write_copy => 0);
            set_mceta($tdist_model, 10);
            if (scalar(@$etas_to_boxcox_tdist) > 0) {
                model_transformations::tdist_etas(model => $tdist_model, etas => $etas_to_boxcox_tdist);
                model_transformations::set_size(model => $tdist_model, size => 'DIMNEW', value => -10000);
                push @simulation_models, simulation_model($tdist_model);
                $tdist_model->_write();
                push @models, $tdist_model;
            }
            if (defined $self->occ) {
                my $iov_etas = model_transformations::find_etas(model => $base_model, type => 'iov');
                if (scalar(@$iov_etas) == 0) {      # We don't have iov previously
                    my $add_iov_model = $base_model->copy(directory => "modelfit_run", filename => "iov.mod", write_copy => 0);
                    set_mceta($add_iov_model, 10);
                    my $error = model_transformations::add_iov(model => $add_iov_model, occ => $self->occ);
                    if (not $error) {
                        push @simulation_models, simulation_model($add_iov_model);
                        $add_iov_model->_write();
                        push @models, $add_iov_model;
                        my $iov_structure = model_transformations::find_iov_structure(model => $add_iov_model);
                        $self->iov_structure($iov_structure);
                    }
                }
            }
            for my $model (@models) {       # Set output directory so that .lst file gets saved in the rundir
                $model->outputs->[0]->directory('.');
            }
            if (scalar(@models) > 0) {
                chdir "modelfit_run";
                my $modelfit = tool::modelfit->new(
                    %{common_options::restore_options(@common_options::tool_options)},
                    models => \@models,
                    directory => "modelfit_dir1",
                    top_tool => 1,
                    nm_output => 'ext,phi',
                    model_subdir => 0,
                );
                $modelfit->run();
                my $simrun = tool::modelfit->new(
                    %{common_options::restore_options(@common_options::tool_options)},
                    models => \@simulation_models,
                    directory => "simulations_dir1",
                    top_tool => 1,
                    nm_output => 'ext,phi',
                    model_subdir => 0,
                );
                $simrun->run();
                chdir "..";
            }
        };
        if ($@) {
            print $@;
        }
        $self->_to_qa_dir();
    }

    if (defined $self->add_etas and scalar(@{$self->add_etas}) > 0 and $self->_tools_to_run->{'transform'}) {
        print "\n*** Running add_etas ***\n";
        mkdir "add_etas_run";
        my $add_etas_model = $self->model->copy(
            filename => $self->model->filename,
            directory => $self->model->directory,
            write_copy => 0,
            output_same_directory => 1,
        );

        $add_etas_model->set_records(type => 'covariance', record_strings => [ "OMITTED" ]);

        if ($add_etas_model->is_run()) {
            $add_etas_model->update_inits(from_output => $add_etas_model->outputs->[0]);
            my $phi_file = $add_etas_model->get_phi_file();
            $add_etas_model->phi_file($phi_file);
        }
        if ($self->nonlinear) {
            $add_etas_model->outputs->[0]->filename_root('add_etas');
            $add_etas_model->outputs->[0]->filename('add_etas.lst');
            $add_etas_model->outputs->[0]->directory($self->directory . '/add_etas_run');
            $add_etas_model->directory($self->directory . '/add_etas_run');
        } else {
            $add_etas_model->outputs(undef);
        }
        my $added_etas = model_transformations::add_etas_to_parameters(model => $add_etas_model, parameters => $self->add_etas);
        $self->added_etas($added_etas);
        $add_etas_model->filename("add_etas.mod");
        $add_etas_model->_write(filename => $self->directory . 'add_etas_run/add_etas.mod');
        chdir("add_etas_run");
        my $old_nm_output = common_options::get_option('nm_output');    # Hack to set clean further down
        common_options::set_option('nm_output', 'ext');
        if ($self->nonlinear) {
            eval {
                my $modelfit = tool::modelfit->new(
                    %{common_options::restore_options(@common_options::tool_options)},
                    models => [ $add_etas_model ],
                    directory => "modelfit_dir1",
                    top_tool => 1,
                    nm_output => 'ext,phi',
                    model_subdir => 0,
                );
                $modelfit->run();
            };
            if ($@) {
                print $@;
            }
        } else {
            ui->category('linearize');
            eval {
                my $linearize = tool::linearize->new(
                    %{common_options::restore_options(@common_options::tool_options)},
                    models => [ $add_etas_model ],
                    directory => 'linearize_run',
                    nm_output => 'ext,phi',
                );
                $linearize->run();
                $linearize->print_results();
            };
            if ($@) {
                print $@;
            }
            ui->category('qa');
        }
        common_options::set_option('nm_output', $old_nm_output);
    }

    $self->_to_qa_dir();

    if (defined $self->continuous or defined $self->categorical) {
        if ($self->_tools_to_run->{'frem'}) {
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
                    rplots => 0,
                    top_tool => 1,
                    clean => 1,
                    tool_options => $self->_special_tool_options,
                    derivatives => 1,
                    bipp => 1,
                    force_posdef_covmatrix => 1,
                );
                $frem->print_options(
                    toolname => 'frem',
                    local_options => [ 'skip_omegas', 'rescale' ],
                    common_options => \@common_options::tool_options
                );
                $frem->run();
                my $err = $frem->prepare_results();
                if ($err) {
                    print("Frem result generation err (no file could be generated):\n");
                    print("$err\n");
                } else {
                    $frem->top_tool(0); # To avoid creating Rmd
                    $frem->print_results();
                }
            };
            if ($@) {
                print $@;
            }
            common_options::set_option('clean', $old_clean);

            $self->_to_qa_dir();
        }

        if ($self->_tools_to_run->{'scm'} and (defined $self->continuous or defined $self->categorical)) {
            print "\n*** Running scm ***\n";
            if (scalar(@time_varying) > 0) {
                if ($self->model->nomegas->[0] > 9) {
                    print "Warning: found time varying covariates, but cannot use them as time varying\n";
                    print "         because model has more than 9 etas\n";
                    @time_varying = ();
                } else {
                    print "Found time varying continuous covariates: " . join(", ", @time_varying) . "\n";
                }
            }
            my $scm_model = $base_model->copy(directory => "m1", filename => "scm.mod", write_copy => 0);
            if ($base_model->is_run()) {
                my $lst_path = $base_model->outputs->[0]->full_name();
                copy($lst_path, 'm1/scm.lst');
                my $ext_path = utils::file::replace_extension($lst_path, 'ext');
                copy($ext_path, 'm1/scm.ext');
                my $phi_file = $base_model->get_phi_file;
                if (defined $phi_file and -e $phi_file) {
                    copy($phi_file, 'm1/scm.phi');
                }
            }
            $scm_model->set_records(type => 'covariance', record_strings => [ "OMITTED" ]);
            my $iov_omegas = model_transformations::find_etas(model => $scm_model, type => 'iov');
            my %keep_etas;
            my $nomegas = $scm_model->problems->[0]->nomegas;
            for (my $i = 1; $i <= $nomegas; $i++) {
                $keep_etas{$i} = 1;
            }
            for my $eta (@$iov_omegas) {
                if ($eta <= $scm_model->problems->[0]->nomegas) {
                    delete $keep_etas{$eta};
                }
            }

            model_transformations::rename_etas(model => $scm_model, etas => [keys %keep_etas], prefix => 'ET');
            my @scm_parameters;
            my @scm_code;
            for my $i (keys %keep_etas) {
                push @scm_code, "ET$i = ETA($i)";
                push @scm_parameters, "ET$i";
            }
            $self->parameters(join ',', @scm_parameters);
            model_transformations::prepend_code(model => $scm_model, code => \@scm_code);
            model_transformations::add_tv(model => $scm_model, parameters => \@scm_parameters, type => 'additive');
            set_mceta($scm_model, 100);

            $scm_model->_write();
            $self->_create_scm_config(model_name => "m1/scm.mod", parameters => \@scm_parameters, time_varying => \@time_varying);
            my %tool_options = %{common_options::restore_options(@common_options::tool_options)};
            my $scm_options = "";
            for my $cmd (split /\s+/, $self->cmd_line) {
                next if $cmd =~ /^--?dir/;
                next if $cmd =~ /^--?rpl/;
                if ($cmd =~ /^--?/) {
                    my $option = $cmd;
                    $option =~ s/^--?(no-)?(.*)/$2/;
                    $option =~ s/=(.*)//;
                    if (grep { $_ eq $option } @common_options::tool_options) {
                        $scm_options .= " $cmd";
                    }
                }
            }

            my $nonlinear = "";
            if ($self->nonlinear) {
                $nonlinear = "-no-linearize -no-foce";
            }

            eval {
                if($dev) {
                    system("scm config.scm -force_binarize -categorical_mean_offset $scm_options $nonlinear");       # FIXME: system for now
                } else {
                    system("scm-".$vers." -force_binarize -categorical_mean_offset config.scm $scm_options $nonlinear");       # FIXME: system for now
                }
            };
            if ($@) {
                print $@;
            }
            $self->_to_qa_dir();
        }
    }

    if ($self->_tools_to_run->{'cdd'}) {
        print "\n*** Running cdd ***\n";
        my $cdd_model = model->new(filename => $self->base_model_path);

        my $cdd_ignore = 1;
        if ($self->nonlinear) {
            $cdd_ignore = 0;
            $cdd_model->set_records(type => 'covariance', record_strings => [ "UNCONDITIONAL" ]);
        }
        eval {
            my $cdd = tool::cdd->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $cdd_model ],
                directory => 'cdd_run',
                rplots => 1,
                etas => 1,
                top_tool => 1,
                ignore => $cdd_ignore,   # Use IGNORE instead of generating new datasets for regular qa. Fallback to no-ignore for nonlinear (too long paths problems)
            );
            $cdd->print_options(
                toolname => 'CDD',
                local_options => [],
                common_options => \@common_options::tool_options
            );
            $cdd->run();
            $cdd->prepare_results();
            PsN::call_pharmpy("psn cdd cdd_run");      # Generate results.json and results.csv
       };
        if ($@) {
            print $@;
        }
        $self->_to_qa_dir();
    }

    if ($self->_tools_to_run->{'simeval'}) {
        print "\n*** Running simeval ***\n";
        eval {
            my $simeval = tool::simeval->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $base_model ],
                rplots => 1,
                n_simulation_models => 5,
                directory => "simeval_run",
                top_tool => 1,
            );
            $simeval->run();
            PsN::call_pharmpy("psn simeval simeval_run");      # Generate results.json and results.csv
        };
        if ($@) {
            print $@;
        }
        $self->_to_qa_dir();
    }

    if ($self->_tools_to_run->{'resmod'}) {
        print "\n*** Running resmod ***\n";
        my $resmod_model;
        if (not $self->nonlinear) {
            $resmod_model = model->new(filename => 'linearize_run/scm_dir1/derivatives.mod');
        } else {
            $resmod_model = $eval_model;
        }

        if($resmod_model->defined_variable(name => 'TIME')) {
            my $resmod_time;
            eval {
                $resmod_time = tool::resmod->new(
                    %{common_options::restore_options(@common_options::tool_options)},
                    models => [ $resmod_model ],
                    dvid => $self->dvid,
                    idv => 'TIME',
                    occ => $self->occ,
                    groups => $self->groups,
                    iterative => 0,
                    directory => 'resmod_TIME',
                    top_tool => 1,
                    clean => 2,
                );
            };
            if (not $@) {
                eval {
                    $resmod_time->run();
                    PsN::call_pharmpy("psn ruvsearch resmod_TIME");      # Generate results.json and results.csv
                };
            } else {
                print $@;
                rmdir 'resmod_TIME';
            }
            $self->_to_qa_dir();
        }

        if($resmod_model->defined_variable(name => 'TAD')) {
            my $resmod_tad;
            eval {
                $resmod_tad = tool::resmod->new(
                    %{common_options::restore_options(@common_options::tool_options)},
                    models => [ $resmod_model ],
                    dvid => $self->dvid,
                    idv => 'TAD',
                    occ => $self->occ,
                    groups => $self->groups,
                    iterative => 0,
                    directory => 'resmod_TAD',
                    top_tool => 1,
                    clean => 2,
                );
            };
            if (not $@) {
                eval {
                    $resmod_tad->run();
                    PsN::call_pharmpy("psn ruvsearch resmod_TAD");      # Generate results.json and results.csv
                };
            } else {
                print $@;
                rmdir 'resmod_TAD';
            }
            $self->_to_qa_dir();
        }


        my $resmod_pred;
        eval {
            $resmod_pred = tool::resmod->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => [ $resmod_model ],
                dvid => $self->dvid,
                idv => 'PRED',
                occ => $self->occ,
                groups => $self->groups,
                iterative => 0,
                directory => 'resmod_PRED',
                top_rool => 1,
                clean => 2,
            );
        };
        if (not $@) {
            eval {
                $resmod_pred->run();
                PsN::call_pharmpy("psn ruvsearch resmod_PRED");      # Generate results.json and results.csv
            };
        } else {
            print $@;
            rmdir 'resmod_PRED';
        }
        $self->_to_qa_dir();

        if(($self->idv ne "TIME") and ($self->idv ne "TAD")) {
            my $resmod_idv;
            eval {
                $resmod_idv = tool::resmod->new(
                    %{common_options::restore_options(@common_options::tool_options)},
                    models => [ $resmod_model ],
                    dvid => $self->dvid,
                    idv => $self->idv,
                    occ => $self->occ,
                    groups => $self->groups,
                    iterative => 0,
                    directory => 'resmod_'.$self->idv,
                    top_tool => 1,
                    clean => 2,
                );
            };
            if (not $@) {
                eval {
                    $resmod_idv->run();
                    PsN::call_pharmpy("psn ruvsearch " . 'resmod_'.$self->idv);      # Generate results.json and results.csv
                };
            } else {
                print $@;
                rmdir "resmod_".$self->idv;
            }
            $self->_to_qa_dir();
        }
    }
}

sub modelfit_analyze
{
    my $self = shift;
}

sub _create_scm_config
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_name => { isa => 'Str' },
        parameters => { isa => 'ArrayRef' },
        time_varying => { isa => 'ArrayRef' },
    );
    my $model_name = $parm{'model_name'};
    my $parameters = $parm{'parameters'};
    my $time_varying = $parm{'time_varying'};

    open my $fh, '>', 'config.scm';

    my $covariates = "";
    if (defined $self->continuous) {
        $covariates = "continuous_covariates=" . $self->continuous;
    }

    my $categorical = "";
    if (defined $self->categorical) {
        $categorical = "categorical_covariates=" . $self->categorical;
    }

    my $time_varying_covariates = "";
    if (scalar(@$time_varying) > 0) {
        $time_varying_covariates = "time_varying=" . join(",", @$time_varying);
    }

    my $all = "";
    if (defined $self->categorical and not defined $self->continuous) {
        $all = $self->categorical;
    } elsif (not defined $self->categorical and defined $self->continuous) {
        $all = $self->continuous;
    } else {
        $all = $self->continuous . ',' . $self->categorical;
    }

    my $relations;
    for my $param (@$parameters) {
        $relations .= "$param=" . $all . "\n";
    }
    my $logit = $self->parameters;

my $content = <<"END";
model=$model_name

directory=scm_run

search_direction=forward

p_forward=0.05
max_steps=1

$covariates
$categorical
$time_varying_covariates
logit=$logit

do_not_drop=$all

[test_relations]
$relations

[valid_states]
continuous = 1,2
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

sub check_nonsupported_modelfeatures
{
    my $self = shift;

    my $model = $self->model;

    if ($model->defined_variable(name => "F_FLAG")) {
        die("Error: Models with F_FLAG are not supported by qa as LAPLACE is not supported.\n");
    }

    if ($model->defined_variable(name => 'FREMTYPE')) {
        die("Error: FREM models are currently not supported by qa.\n");
    }

    if (defined $model->problems->[0]->mixs) {
        die("Error: Mixture models are not directly supported by qa. Please see the user guide for an idea on how to run them.\n");
    }

    if ($model->is_option_set(record => 'estimation', name => 'HYBRID', record_number => -1, fuzzy_match => 1) or
            $model->is_option_set(record => 'estimation', name => 'LAPLACIAN', fuzzy_match => 1) or
            $model->is_option_set(record => 'estimation', name => 'LAPLACE', fuzzy_match => 1)) {
        die("Error: options HYBRID and LAPLACE to \$ESTIMATION are not supported by qa.\n");
    }

    my $methods = $model->get_option_value(record_name => 'estimation', option_name => 'METHOD', record_index => 'all', fuzzy_match => 1);
    my $final_method = pop @$methods;

    if (not defined $final_method or $final_method =~ /^(IMP|IMPMAP|SAEM|BAYES|NUTS)/ or $final_method eq '0') {
        die("Error: Estimation with FO, IMP, IMPMAP, SAEM, BAYES or NUTS in the final \$EST of the model is not supported by qa.\n");
    }
}

sub get_scm_categorical
{
    my $self = shift;

    my @categorical;

    return \@categorical if not defined $self->categorical;

    # Warning: nasty code ahead.
    # Figure out which catcovs that were actually used in scm i.e. in case of splitting of categoricals with more than two levels.
    my $covariate_statistics_filename = $self->directory . 'scm_run/covariate_statistics.txt';

    if (not -e $covariate_statistics_filename) {
        if (defined $self->categorical) {
            @categorical = split /,/, $self->categorical;
        }
    } else {
        open my $fh, '<', $covariate_statistics_filename;
        my $covcode = do { local $/ = undef; <$fh> };
        my $VAR1;       # Gets filled by eval
        eval $covcode;
        my @scm_covs = keys %$VAR1;
        for my $cat (split ',', $self->categorical) {
            for my $scmcov (@scm_covs) {
                if ($cat eq $scmcov) {
                    push @categorical, $cat;
                    last;
                } elsif ($scmcov =~ /^${cat}_\d+$/) {
                    push @categorical, $scmcov;
                }
            }
        }
        close $fh;
    }

    return \@categorical;
}

sub write_captured_output
{
    # Write the string that was captured by bin/qa to a file for later use by the R-plots code
    my $self = shift;
    my $capture = shift;

    open my $fh, '>', $self->directory . 'captured_output.txt';
    print $fh $capture;
    close $fh;
}

sub create_R_plots_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        rplot => { isa => 'rplots', optional => 0 }
    );
    my $rplot = $parm{'rplot'};

    my @continuous;
    if (defined $self->continuous) {
        @continuous = split(/,/, $self->continuous);
    }
    my @categorical;
    if (defined $self->categorical) {
        @categorical = split(/,/, $self->categorical);
    }
    my @parameters;
    if (defined $self->parameters) {
        @parameters = split(/,/, $self->parameters);
    }
    my $extra_table_path = 'linearize_run/scm_dir1/extra_table';
    if ($self->nonlinear) {
        $extra_table_path = 'extra_table';
    }

    # Synonym in _linbase $TABLE carries over to extra_tables and need to be replaced.
    my %synonyms = ( 'CIPREDI' => 'OPRED' );
    my $time_synonym = $self->model->problems->[0]->find_data_synonym(name => 'TIME');
    if (defined $time_synonym) {
        $synonyms{'TIME'} = $time_synonym;
    }
    nmtablefile::rename_column_names(filename => $self->directory . $extra_table_path, replacements => \%synonyms);
    $extra_table_path =~ s/\\/\//g;


    my $orig_max0_model_path = File::Spec->abs2rel($self->orig_max0_model_path, $self->directory);
    $orig_max0_model_path =~ s/\\/\//g;
    my $base_model_path = File::Spec->abs2rel($self->base_model_path, $self->directory);
    $base_model_path =~ s/\\/\//g;
    my $base_dataset_path = File::Spec->abs2rel($self->base_dataset_path, $self->directory);
    $base_dataset_path =~ s/\\/\//g;

    my $nonlinear_run;
    if($self->nonlinear) {
        $nonlinear_run = "TRUE";
    } else {
        $nonlinear_run = "FALSE";
    }

    my $scm_categorical = $self->get_scm_categorical();
    # FIXME: Could for some reason get undef
    if (not defined $scm_categorical) {
        $scm_categorical = [];
    }

    my $code = [
            '# qa specific preamble',
            "groups <- " . $self->groups,
            "idv_name <- '" . $self->idv . "'",
            "continuous <- " . rplots::create_r_vector(array => \@continuous),
            "categorical <- " . rplots::create_r_vector(array => \@categorical),
            "scm_categorical <- " . rplots::create_r_vector(array => $scm_categorical),
            "parameters <- " . rplots::create_r_vector(array => \@parameters),
            "extra_table <- '" . $extra_table_path . "'",
            "cdd_dofv_cutoff <- 3.84 ",
            "cdd_max_rows <- 10",
            "skip <- " . rplots::create_r_vector(array => $self->_tools_to_skip ),
            "nonlinear <- " . $nonlinear_run,
            "original_max0_model <- '" . $orig_max0_model_path . "'",
            "base_model <- '" . $base_model_path . "'",
            "base_dataset <- '" . $base_dataset_path . "'",
        ];
    my $dvid_line = "dvid_name <- ''";
    if (defined $self->dvid) {
        $dvid_line = "dvid_name <- '" . $self->dvid . "'";
    }
    push @$code, $dvid_line;

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

    if (defined $self->iov_structure) {
        my @content;
        for (my $i = 0; $i < scalar(@{$self->iov_structure}); $i++) {
            my $occ = 'occ' . ($i + 1) . '=' . rplots::create_r_vector(array => $self->iov_structure->[$i], quoted => 0);
            push @content, $occ;
        }
        my $line = 'iov_etas <- list(' . join(', ', @content). ')';
        push @$code, $line;
    }


    $rplot->add_preamble(code => $code);
}

sub set_mceta
{
    my $model = shift;
    my $mceta = shift;
    
    $model->remove_option(
        record_name => 'estimation',
        option_name => 'MCETA',
    );
    $model->add_option(
        record_name => 'estimation',
        option_name => 'MCETA',
        option_value => $mceta,
    );
}

sub simulation_model
{
    # Create a new simulation model from an estimation model.
    # A separate $PROBLEM was tried, but gave errors for the simulation

    my $model = shift;

    my $name = utils::file::get_file_stem($model->filename);
    my $sim = $model->copy(directory => "modelfit_run", filename => "${name}_sim.mod", write_copy => 0);

    $sim->remove_records(type => 'estimation');
    $sim->remove_records(type => 'covariance');

    my $seed = random_uniform_integer(1,1,99999999);

    $sim->set_records(type => 'simulation', record_strings => ["($seed) NSUB=300 ONLYSIM"] );
	$sim->set_records(type => 'table', record_strings => ["ID DV MDV TIME NOPRINT NOAPPEND ONEHEADER FILE=$name.sim"]);
    $sim->_write();
    return $sim;
}

1;
