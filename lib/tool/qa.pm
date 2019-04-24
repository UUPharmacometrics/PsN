package tool::qa;

use strict;
use Moose;
use MooseX::Params::Validate;
use File::Copy 'cp';
use include_modules;
use log;
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
has 'dv' => ( is => 'rw', isa => 'Str', default => 'CWRES' );
has 'dvid' => ( is => 'rw', isa => 'Str' );
has 'occ' => ( is => 'rw', isa => 'Str' );
has 'continuous' => ( is => 'rw', isa => 'Str' );       # A comma separated list of continuous covariate symbols
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
has 'iov_structure' => ( is => 'rw', isa => 'ArrayRef' );   # The occ/iov structure for the r code
has 'orig_max0_model_path' => ( is => 'rw', isa => 'Str' );
has 'base_model_path' => ( is => 'rw', isa => 'Str' );
has 'base_dataset_path' => ( is => 'rw', isa => 'Str' );
has 'extra_table_columns' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'nm_parallel' => ( is => 'rw', isa => 'Str' );  # Should a special NONMEM version be used for frem and linearize?
has '_parallel_cores' => ( is => 'rw', isa => 'Maybe[Int]', default => undef );     # The number of -node to use for frem and linearize


sub BUILD
{	
	select(STDERR);     # Turn on autoflush to simplify fault-finding
	$| = 1;
	select(STDOUT);
	$| = 1;
    my $self = shift;

	my $model = $self->models()->[0];
    $self->model($model);

    $self->check_nonsupported_modelfeatures();

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

    if (defined $self->nm_parallel) {
        if (defined $self->threads) {
            $self->_parallel_cores($self->threads);
        }
    } else {
        if (defined $self->nm_version) {
            $self->nm_parallel($self->nm_version);
        } else {
            $self->nm_parallel('default');
        }
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
    if ($self->nonlinear) {
        $eval_model = $self->model->copy(filename => $self->model->filename, directory => $self->directory, write_copy => 0, output_same_directory => 0);
        my @extra_tablestrings = ( @table_columns, 'NOPRINT', 'NOAPPEND', 'ONEHEADER', 'FILE=extra_table' );
        $eval_model->remove_records(type => 'table');
        $eval_model->add_records(type => 'table', record_strings => \@extra_tablestrings);
        $eval_model->_write(filename => $self->directory . $self->model->filename);
        $eval_model->set_maxeval_zero();
        $eval_model->outputs->[0]->directory($self->directory);
        my $modelfit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            models => [ $eval_model ],
            directory => "eval_run",
            top_tool => 1,
            nm_output => 'ext,phi',
            model_subdir => 0,
            nm_version => $self->nm_parallel,
            nodes => $self->_parallel_cores,
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
            estimate_fo => $self->fo,
            extra_table_columns => \@table_columns,
            nointer => $self->nointer,
            keep_covariance => 1,
            nm_output => 'phi,ext,cov,cor,coi',
            nm_version => $self->nm_parallel,
            nodes => $self->_parallel_cores,
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
    } else {
        $base_model = $model_copy;
        $self->base_dataset_path($self->directory . 'preprocess_data_dir/filtered.dta');
    }
 
    my $data = data->new(
        filename => $self->base_dataset_path,
        ignoresign => defined $base_model->ignoresigns ? $base_model->ignoresigns->[0] : undef,
        idcolumn => $base_model->idcolumns->[0],
    );

    my $numids = scalar(@{$data->individuals});
    if ($numids < 2 and not $self->_skipped('cdd')) {   # Skip cdd if only one individual
        print "Warning: Only one individual in dataset. Will skip cdd\n";
        push @{$self->skip}, 'cdd';
    }

    #if ($self->fo) {
    #    $base_model->remove_option(record_name => 'estimation', option_name => 'METHOD');
    #    $base_model->_write();
    #}


    if (not $self->_skipped('transform')) {
        print "*** Running full omega block, boxcox and tdist models ***\n";
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
                model_transformations::set_size(model => $boxcox_model, size => 'DIMNEW', value => -10000);
                $boxcox_model->_write();
                push @models, $boxcox_model;
            }
            my $tdist_model = $base_model->copy(directory => "modelfit_run", filename => "tdist.mod", write_copy => 0);
            if (scalar(@$etas_to_boxcox_tdist) > 0) {
                model_transformations::tdist_etas(model => $tdist_model, etas => $etas_to_boxcox_tdist);
                model_transformations::set_size(model => $tdist_model, size => 'DIMNEW', value => -10000);
                $tdist_model->_write();
                push @models, $tdist_model;
            }
            if (defined $self->occ) {
                my $iov_etas = model_transformations::find_etas(model => $base_model, type => 'iov');
                if (scalar(@$iov_etas) == 0) {      # We don't have iov previously
                    my $add_iov_model = $base_model->copy(directory => "modelfit_run", filename => "iov.mod", write_copy => 0);
                    my $error = model_transformations::add_iov(model => $add_iov_model, occ => $self->occ);
                    if (not $error) {
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
            chdir "modelfit_run";
            my $modelfit = tool::modelfit->new(
                %{common_options::restore_options(@common_options::tool_options)},
                models => \@models,
                directory => "modelfit_dir1",
                top_tool => 1,
                so => 1,
                nm_output => 'ext,phi',
                model_subdir => 0,
            );
            $modelfit->run();
            chdir "..";
        };
        if ($@) {
            print $@;
        }
        $self->_to_qa_dir();
    }

    if (defined $self->add_etas and scalar(@{$self->add_etas}) > 0 and not $self->_skipped('transform')) {
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
                    estimate_fo => $self->fo,
                    nointer => $self->nointer,
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
                    nm_version => $self->nm_parallel,
                    nodes => $self->_parallel_cores,
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
                my $phi_file = $self->model->get_phi_file;
                if (defined $phi_file and -e $phi_file) {
                    cp($phi_file, 'm1/scm.phi');
                }
            }
            model_transformations::add_tv(model => $scm_model, parameters => [split /,/, $self->parameters]);
            $scm_model->set_records(type => 'covariance', record_strings => [ "OMITTED" ]);
            $scm_model->_write();
            $self->_create_scm_config(model_name => "m1/scm.mod");
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
        my $cdd_model = model->new(filename => $self->base_model_path);
        my $cdd_ignore = 1;
        if ($self->nonlinear) {
            $cdd_ignore = 0;
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
					dv => $self->dv,
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
					dv => $self->dv,
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
                dv => $self->dv,
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
					dv => $self->dv,
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

    my $covariates = "";
    if (defined $self->continuous) {
        $covariates = "continuous_covariates=" . $self->continuous;
    }

    my $categorical = "";
    if (defined $self->categorical) {
        $categorical = "categorical_covariates=" . $self->categorical;
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

sub check_nonsupported_modelfeatures
{
    my $self = shift;

    my $model = $self->model;

    if (defined $model->problems->[0]->mixs) {
        die("Error: Mixture models are not supported by qa.\n");
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

sub mend_extra_table_names
{
    my $self = shift;
    my $colnames = shift; 

    my $directory = $self->directory . 'linearize_run/scm_dir1/';

    open my $fh, '<', $directory . 'extra_table' or return;
    open my $dh, '>', $directory . 'extra_table.temp' or return;

    my $table_line = <$fh>;
    print $dh $table_line;
    <$fh>;
    print $dh ' ' . join('      ', @$colnames) . "\n";
    while (my $line = <$fh>) {
        print $dh $line;
    }

    close $dh;
    close $fh;

    unlink "${directory}extra_table";
    rename "${directory}extra_table.temp", "${directory}/extra_table";
}


sub create_R_plots_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
        rplot => { isa => 'rplots', optional => 0 }
    );
	my $rplot = $parm{'rplot'};

	$rplot->pdf_title('Quality assurance');

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
	my $extra_table_path = $self->directory . 'linearize_run/scm_dir1/extra_table';
    if ($self->nonlinear) {
        $extra_table_path = $self->directory . 'extra_table';
    }
    $extra_table_path =~ s/\\/\//g;
	
	my $orig_max0_model_path = $self->orig_max0_model_path;
	$orig_max0_model_path =~ s/\\/\//g;
	my $base_model_path = $self->base_model_path;
	$base_model_path =~ s/\\/\//g;
	my $base_dataset_path = $self->base_dataset_path;
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

    my @extra_table_columns = (@{$self->extra_table_columns}, 'MDV');
    $self->mend_extra_table_names(\@extra_table_columns);

    my $code =  [
            '# qa specific preamble',
			"groups <- " . $self->groups,
            "idv_name <- '" . $self->idv . "'",
            "continuous <- " . rplots::create_r_vector(array => \@continuous),
            "categorical <- " . rplots::create_r_vector(array => \@categorical),
            "scm_categorical <- " . rplots::create_r_vector(array => $scm_categorical),
            "parameters <- " . rplots::create_r_vector(array => \@parameters),
            "extra_table <- '" . $extra_table_path . "'",
            "extra_table_columns <- " . rplots::create_r_vector(array => \@extra_table_columns),
			"cdd_dofv_cutoff <- 3.84 ",
			"cdd_max_rows <- 10",
			"type <- 'latex' # set to 'html' if want to create a html file ",
            "skip <- " . rplots::create_r_vector(array => $self->skip),
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


no Moose;
__PACKAGE__->meta->make_immutable;
1;
