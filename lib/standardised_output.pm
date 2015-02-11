package standardised_output;

# Package for creation of a DDMoRe standardised output XML file

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use output;
use data;
use array;
use IO::File;
use math;
use utils::file;
use standardised_output::xml;
use PsN;

has 'lst_files' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'bootstrap_results' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'so_filename' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'precision' => ( is => 'rw', isa => 'Int', default => 10 );
has 'use_tables' => ( is => 'rw', isa => 'Bool', default => 1 );    # Set to zero if sdtab and patab should not be used
has 'exclude_elements' => ( is => 'rw', isa => 'Maybe[ArrayRef]' );
has 'only_include_elements' => ( is => 'rw', isa => 'Maybe[ArrayRef]' );
has 'message' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'toolname' => ( is => 'rw', isa => 'Str', default => 'NONMEM' );
has 'max_replicates' => ( is => 'rw', isa => 'Maybe[Int]' );        # Maximum number of simulation replicates to add
has 'pretty' => ( is => 'rw', isa => 'Bool', default => 0 );        # Should the xml be indented or not
has 'pharmml' => ( is => 'rw', isa => 'Maybe[Str]' );               # Name of pharmml file
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'external_tables' => ( is => 'rw', isa => 'Bool', default => 0 );   # For now a bool to specify if external tables should be created
has '_document' => ( is => 'rw', isa => 'Ref' );                    # The XML document 
has '_duplicate_blocknames' => ( is => 'rw', isa => 'HashRef' );    # Contains those blocknames which will have duplicates with next number for block
has '_first_block' => ( is => 'rw', isa => 'Str' );                 # Name of the first SOBlock
has '_so_path' => ( is => 'rw', isa => 'Str' );                     # The path of the output SO file
has '_xml' => ( is => 'rw', isa => 'standardised_output::xml' );    # Module to create SO xml
has '_used_files' => ( is => 'rw', isa => 'HashRef' );              # files to add as RawResults. Empty before starting a new _parse_lst_file 

sub BUILD
{
    my $self = shift;

    if (defined $self->max_replicates) {
        if ($self->max_replicates < 1) {
            croak("max_replicates must be a positive number");
        }
    }

    my $so_filename;

    if (not defined $self->so_filename) {   # User specified filename
        if (defined $self->lst_files and defined $self->lst_files->[0]) {   # Infer filename from name of first .lst file
            $so_filename = utils::file::replace_extension($self->lst_files->[0], 'SO.xml');
        } else {
            $so_filename = 'bootstrap.SO.xml';
        }
        $so_filename = utils::file::remove_path($so_filename);
        $self->so_filename($so_filename);
        $self->_so_path('./');
    } else {
        my $path = utils::file::directory($self->so_filename);
        $self->_so_path($path);
    }

    my $so_xml = standardised_output::xml->new(precision => $self->precision, verbose => $self->verbose);
    $self->_xml($so_xml);
    $self->_document($self->_xml->_document);
}

sub create_block
{
    # Create a new SOBlock and set the id 
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
    );
    my $name = $parm{'name'};
    
    my $doc = $self->_document;
    my %duplicates;
    if (defined $self->_duplicate_blocknames) {
        %duplicates = %{$self->_duplicate_blocknames};
    }

    my $block = $doc->createElement("SOBlock");
    if (not exists $duplicates{$name}) {
        $block->setAttribute(blkId => $name);
    } else {
        print "$duplicates{$name}";
        $block->setAttribute(blkId => $name . $duplicates{$name});
        $self->_duplicate_blocknames->{$name}++;
    }

    return $block;
}

sub match_elements
{
    my $path1 = shift;
    my $path2 = shift;

    my @elements1 = split m|/|, $path1;
    my @elements2 = split m|/|, $path2;

    my $shortest = scalar(@elements1);
    if (scalar(@elements2) < $shortest) {
        $shortest = scalar(@elements2);
    }
    for (my $i = 0; $i < $shortest; $i++) {
        if ($elements1[$i] ne $elements2[$i]) {
            return 0;
        }
    }
    return 1;
}

sub check_include
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        element => { isa => 'Str' },
    );
    my $element = $parm{'element'};

    if (defined $self->exclude_elements) {
        foreach my $e (@{$self->exclude_elements}) {
            if ($e eq $element) {
                return 0;
            }
        }
        return 1;
    } elsif (defined $self->only_include_elements) {
        foreach my $e (@{$self->only_include_elements}) {
            if (match_elements($e, $element)) {
                return 1;
            }
        }
        return 0;
    }
    return 1;
}

sub parse
{
    my $self = shift;

    my $doc = $self->_document;

    my $SO = $doc->createElement("SO");
    $SO->setAttribute('xmlns' => "http://www.pharmml.org/so/0.1/StandardisedOutput");
    $SO->setAttribute('xmlns:xsi' => "http://www.w3.org/2001/XMLSchema-instance");
    $SO->setAttribute('xmlns:ds' => "http://www.pharmml.org/pharmml/0.6/Dataset");
    $SO->setAttribute('xmlns:ct' => "http://www.pharmml.org/pharmml/0.6/CommonTypes");
    $SO->setAttribute('xsi:schemaLocation' => "http://www.pharmml.org/so/0.1/StandardisedOutput");
    $SO->setAttribute('implementedBy' => "MJS");
    $SO->setAttribute('writtenVersion' => "0.1");
    $SO->setAttribute('id' => "i1");

    if (defined $self->pharmml) {
        my $pharmmlref = $self->_xml->create_pharmml_ref(name => $self->pharmml);
        $SO->appendChild($pharmmlref);
    }

    # Check for duplicate lst_file names
    my %duplicates;
    if (defined $self->lst_files) {
        foreach my $file (@{$self->lst_files}) {
            my $stem = utils::file::get_file_stem($file);
            $duplicates{$stem}++;
        }
        foreach my $name (keys %duplicates) {
            if ($duplicates{$name} == 1) {
                delete $duplicates{$name};
            } else {
                $duplicates{$name} = 1;
            }
        }
    }
    $self->_duplicate_blocknames(\%duplicates);
    
    # Handle lst files
    if (defined $self->lst_files) {
        foreach my $file (@{$self->lst_files}) {
            my $block = $self->_parse_lst_file(lst_file => $file);
            $SO->appendChild($block);
        }
    }

    # Handle bootstrap_results
    if (defined $self->bootstrap_results) {
        # Find or create xml structure
        my $bootstrap_block = $self->_first_block;
        (my $block) = $SO->findnodes("SOBlock[\@blkId='$bootstrap_block']");
        if (not defined $block) {
            $bootstrap_block = "Bootstrap";
            $block = $self->create_block(name => $bootstrap_block);
            $SO->appendChild($block);
        }
        if ($self->verbose) {
            print "Adding bootstrap results from file ", $self->bootstrap_results, " to SOBlock \"$bootstrap_block\"\n";
        }
        my $estimation = $self->_xml->find_or_create_node(root_node => $block, node_name => "Estimation");

        my $bootstrap_message;

        # Create Bootstrap element
        if (-e $self->bootstrap_results) {
            my $ppi = $self->_xml->find_or_create_node(root_node => $estimation, node_name => "PrecisionPopulationEstimates");
            (my $bootstrap, $bootstrap_message) = $self->_create_bootstrap();
            if (defined $bootstrap) {
                $ppi->appendChild($bootstrap);
            }
        } else {
            $bootstrap_message = {
                type => "ERROR",
                toolname => $self->toolname,
                name => "File error",
                content => "Bootstrap results file \"" . $self->bootstrap_results . "\" does not exist",
                severity => 10,
            };
        }

        # Create Bootstrap messages
        if (defined $bootstrap_message) {
            if ($block->exists("TaskInformation")) {
                (my $ti) = $block->findnodes("TaskInformation");
                my $message = $self->_xml->create_message(message => $bootstrap_message);
                my $first_child = $ti->firstChild();
                $ti->insertBefore($message, $first_child);
            } else {
                my $ti = $self->_create_task_information(messages => [ $bootstrap_message ]);
                $block->appendChild($ti)
            }
        }
    }

    $doc->setDocumentElement($SO);
    $doc->toFile($self->so_filename, $self->pretty);
}

sub _parse_lst_file
{
    # Parse one lst-file and put it into an SOBlock
    my $self = shift;
    my %parm = validated_hash(\@_,
        lst_file => { isa => 'Str' },
    );
    my $lst_file = $parm{'lst_file'};

    if ($self->verbose) {
        print "Adding $lst_file\n";
    }

    my $path = utils::file::directory($lst_file);

    my $elapsed_time = 0;
    my @messages;
    my @on_sd_scale;
    my $doc = $self->_document;

    my $file_stem = utils::file::get_file_stem($lst_file);

    my $block = $self->create_block(name => $file_stem);
    my $estimation;
    my $simulation;

    # Check that the output file exist before trying to read it.
    if (not -e $lst_file) {
        push @messages, {
            type => "ERROR",
            toolname => $self->toolname,
            name => "File error",
            content => "The file: \"" . $lst_file . "\" does not exist",
            severity => 10,
        };
    } else {
        $self->_used_files({$lst_file => "NONMEM results file"});       # Start a new used files hash

        my $outobj = output->new(filename => $lst_file);
        if (not $outobj->parsed_successfully) {
            push @messages, {
                type => "ERROR",
                toolname => $self->toolname,
                name => "Parsing error", 
                content => "Outputfile not parsed successfully, error message: " . $outobj->parsing_error_message,
                severity => 10,
            };
        } else {
            my $model = $outobj->lst_model;

            my $eta_shrinkage = $outobj->shrinkage_eta();
            my $eps_shrinkage = $outobj->shrinkage_eps();
            my $observation_records = $outobj->nobs();
            my $individuals = $outobj->nind();

            my $problems = 0; #TODO check if first $PROB is prior, then should be =1 here, as in e.g. sse script
            my $sub_problems = 0;  #always 0 since we do not have workflow simulation + estimation?

            my @etashrinkage = defined $eta_shrinkage -> [$problems][$sub_problems] ? @{$eta_shrinkage -> [$problems][$sub_problems]} : ();
            my @epsshrinkage = defined $eps_shrinkage -> [$problems][$sub_problems] ? @{$eps_shrinkage -> [$problems][$sub_problems]} : ();

            ## Termination
            my $ofv = $outobj -> get_single_value(attribute => 'ofv',
                problem_index => $problems,
                subproblem_index => $sub_problems);

            my $min_success = $outobj -> get_single_value(attribute => 'minimization_successful',
                problem_index =>$problems,
                subproblem_index => $sub_problems);

            my $covariance_step_successful = 0;
            if ($outobj->covariance_step_run->[$problems]) {
                if ($outobj->covariance_step_successful->[$problems][$sub_problems] ne '0') {
                    $covariance_step_successful = 1;
                }
            }

            my $simulation_step_run = $outobj->get_single_value(attribute => 'simulationstep', problem_index => $problems);
            my $estimation_step_run = $outobj->get_single_value(attribute => 'estimation_step_run', problem_index => $problems);

            my @est_values = @{$outobj->get_filtered_values(
                problem_index => $problems,
                subproblem_index => $sub_problems,
                parameter => 'all',
                category => 'estimate',
                allow_sdcorrform => 1,
            )};

            my @se_values = @{$outobj->get_filtered_values(
                problem_index => $problems,
                subproblem_index => $sub_problems,
                parameter => 'all',
                category => 'se',
                allow_sdcorrform => 1,
            )};

            my @filtered_labels = @{$model->problems->[$problems]->get_estimated_attributes(parameter => 'all', attribute => 'labels')};

            my @all_labels = @filtered_labels; #will add fix with label here

            # Handling parameters that are FIX but have a label
            my @records;
            if (defined $model->problems->[$problems]->thetas) {
                push @records, @{$model->problems->[$problems]->thetas};
            }
            if (defined $model->problems->[$problems]->omegas) {
                push @records, @{$model->problems->[$problems]->omegas};
            }
            if (defined $model->problems->[$problems]->sigmas) {
                push @records, @{$model->problems->[$problems]->sigmas};
            }
            my @options;
            foreach my $record (@records) {
                if (defined $record->options) {
                    push @options, @{$record->options};
                }
            }

            foreach my $option (@options) {
                if ($option->fix and defined $option->label) {
                    push @est_values, $option->init;
                    push @all_labels, $option->label;
                }
                if ($option->sd or $option->corr) {     # Save labels for parameters on sd/corr scale
                    if (grep { $_ eq $option->label } @all_labels) {
                        push @on_sd_scale, $option->label;
                    }
                }
            }

            if ($estimation_step_run) {
                $estimation = $doc->createElement("Estimation");

                foreach my $label (@all_labels) {
                    if (not $self->_xml->match_symbol_idtype($label)) {
                        my $old_label = $label;
                        $label = $self->_xml->mangle_symbol_idtype($label);
                        push @messages, {
                            type => "WARNING",
                            toolname => "nmoutput2so",
                            name => "Name change",
                            content => "Parameter label \"$old_label\" not specified or not a legal symbolIdType. Setting/changing it to: $label",
                            severity => 1,
                        };
                    }
                }
				#repeat same procedure for filtered_labels, duplication, do not add message again
                foreach my $label (@filtered_labels) {
                    if (not $self->_xml->match_symbol_idtype($label)) {
                        my $old_label = $label;
                        $label = $self->_xml->mangle_symbol_idtype($label);
                    }
                }

                #Calculate relative standard errors, only for estimated values that have se
                my @rel_se = ();

                if ($covariance_step_successful) {
                    for (my $i = 0; $i < scalar(@se_values); $i++) {
                        if ($est_values[$i] == 0) {
                            push @rel_se, undef;
                        } else { 
                            push @rel_se, $se_values[$i] / abs($est_values[$i]);
                        }
                    }
                }

                my $minimization_message = $outobj -> get_single_value(attribute => 'minimization_message',
                    problem_index => $problems,
                    subproblem_index => $sub_problems);

                my $undefs = grep { not defined $_ } @est_values;
                if ($undefs != scalar(@est_values)) {   # Check that not all in list are undef. Should possibly have been done earlier
                    if ($self->check_include(element => 'Estimation/PopulationEstimates')) {
                        my $pe = $self->_create_population_estimates(labels => \@all_labels, values => \@est_values);
                        $estimation->appendChild($pe);
                    }
                }

                if ($covariance_step_successful) {
                    my $correlation_matrix = linear_algebra::triangular_symmetric_to_full($outobj->correlation_matrix->[$problems]->[$sub_problems]);
                    my $covariance_matrix = linear_algebra::triangular_symmetric_to_full($outobj->covariance_matrix->[$problems]->[$sub_problems]);
                    my $additional = $outobj->problems->[$problems]->subproblems->[$sub_problems]->NM7_parsed_additional;
                    if (defined $additional) {
                        if ($additional->{'cov'}) {
                            $self->_add_raw_results_file("$file_stem.cov", "NONMEM Covariance matrix");
                        }
                        if ($additional->{'cor'}) {
                            $self->_add_raw_results_file("$file_stem.cor", "NONMEM Correlation matrix");
                        }
                    }
                    if ($self->check_include(element => 'Estimation/PrecisionPopulationEstimates')) {
                        my $ppe = $self->_create_precision_population_estimates(
                            labels => \@filtered_labels,
                            standard_errors => \@se_values,
                            relative_standard_errors => \@rel_se,
                            correlation_matrix => $correlation_matrix,
                            covariance_matrix => $covariance_matrix,
                        );
                        $estimation->appendChild($ppe);
                    }
                }

                # Loop through the different tables
                if ($self->use_tables) {
                    my ($table_name_ref, $dummy) = $model->problems->[$problems]->_option_val_pos(record_name => 'table', name => 'FILE');
                    if (defined $table_name_ref and scalar @{$table_name_ref} >= 0) {
                        foreach my $table (@$table_name_ref) {
                            if ($table =~ /^(sdtab|patab)/ and not -e ($path . $table)) {
                                push @messages, {
                                    type => "WARNING",
                                    toolname => $self->toolname,
                                    name => "File error",
                                    content => "Could not find table $path$table. Results from this table could not be added.",
                                    severity => 1,
                                };
                                next;
                            }
                            if ($table =~ /^sdtab/) {
                                my $sdtab = data->new(
                                    directory => $path,
                                    filename => $table,
                                    ignoresign => '@',
                                    parse_header => 1,
                                );
                                if ($self->check_include(element => 'Estimation/Residuals')) {
                                    my $residuals = $self->_create_residuals(sdtab => $sdtab);
                                    if (defined $residuals) {
                                        $estimation->appendChild($residuals);
                                    }
                                }
                                if ($self->check_include(element => 'Estimation/Predictions')) {
                                    my $predictions = $self->_create_predictions(sdtab => $sdtab);
                                    if (defined $predictions) {
                                        $estimation->appendChild($predictions);
                                    }
                                }

                            }
                            if ($table =~ /^patab/) {
                                my $patab = data->new(
                                    directory => $path,
                                    filename => $table,
                                    ignoresign => '@',
                                    parse_header => 1,
                                );
                                if ($self->check_include(element => 'Estimation/IndividualEstimates')) {
                                    my $individual_estimates = $self->_create_individual_estimates(
                                        patab => $patab,
                                        model => $model,
                                        model_labels => \@all_labels
                                    );
                                    if (defined $individual_estimates) {
                                        $estimation->appendChild($individual_estimates);
                                    }
                                }
                            }
                        }
                    }
                }

                if ($self->check_include(element => 'Estimation/Likelihood')) {
                    my $likelihood = $self->_create_likelihood(ofv => $ofv);
                    if (defined $likelihood) {
                        $estimation->appendChild($likelihood);
                    }
                }

                if (not defined $ofv) {
                    push @messages, {
                        type => 'ERROR',
                        toolname => $self->toolname,
                        name => "Minimzation error",
                        content => join('', @{$minimization_message}),
                        severity => 5,
                    };
                }
            }
            $outobj->runtime =~ m/(\d+):(\d+):(\d+)/;
            $elapsed_time = $1 + $2 / 60 + $3 / 3600;

            if ($simulation_step_run and $self->use_tables) {
                $simulation = $self->_create_simulation(
                    model => $model,
                    problem => $model->problems->[$problems],
                    path => $path,
                    table_file => $file_stem
                );
            }

        }
    }

    push @messages, {
        type => "INFORMATION",
        toolname => "nmoutput2so",
        name => "nmoutput2so_version",
        content => "This SOBlock was created with nmoutput2so version " . $PsN::version,
        severity => 0,
    };

    if (not defined $self->_first_block) {
        $self->_first_block($file_stem);
        if (defined $self->message) {
            push @messages, {
                type => "INFORMATION",
                toolname => $self->toolname,
                name => "User specified message",
                content => $self->message,
                severity => 0,
            };
        }
        if (defined $self->bootstrap_results and scalar(@on_sd_scale) > 0) {
            my $msg;
            if (scalar(@on_sd_scale) == 1) {
                $msg = "The parameter " . $on_sd_scale[0] . " is on sd/corr scale in the model but the bootstrap percentiles for this parameter will be on var/cov scale";
            } elsif (scalar(@on_sd_scale) == 2) {
                $msg = "The parameters " . $on_sd_scale[0] . " and " . $on_sd_scale[1] . " are on sd/corr scale in the model but the bootstrap percentiles for these parameters will be on var/cov scale";
            } else {
                $msg = "The parameters " . join(',', @on_sd_scale[0 .. $#on_sd_scale - 1]) . " and " . $on_sd_scale[-1] . " are on sd/corr scale in the model but the bootstrap percentiles for these parameters will be on the var/cov scale";
            }
            push @messages, {
                type => "WARNING",
                toolname => "PsN",
                name => "Bootstrap",
                content => $msg, 
                severity => 8,
            };
        }
    }

    my $task_information = $self->_create_task_information(messages => \@messages, run_time => $elapsed_time);
    my $raw_results = $self->_create_raw_results();

    if (defined $raw_results) {
        $block->appendChild($raw_results);
    }
    if (defined $task_information) {
        $block->appendChild($task_information);
    }
    if (defined $estimation) {
        $block->appendChild($estimation);
    }
    if (defined $simulation) {
        $block->appendChild($simulation);
    }

    return $block;
}

sub _get_included_columns
{
    # Get which columns from an array are included in a table header hash

    # static no self
    my %parm = validated_hash(\@_,
        header => { isa => 'HashRef' },
        columns => { isa => 'ArrayRef' },
    );
    my %header = %{$parm{'header'}};
    my @columns = @{$parm{'columns'}};

    my @included = ();

    for my $col (@columns) {
        if (exists $header{$col}) {
            push @included, $col;
        }
    }

    return \@included;
}

sub _get_remaining_columns
{
    # Get the columns in a table header hash that are not included in an array

    # static no self
    my %parm = validated_hash(\@_,
        header => { isa => 'HashRef' },
        columns => { isa => 'ArrayRef' },
    );
    my %header = %{$parm{'header'}};
    my @columns = @{$parm{'columns'}};

    for my $col (@columns) {
        delete $header{$col};
    }

    my @remaining = keys %header;
    @remaining = sort { $header{$a} <=> $header{$b} } @remaining;

    return \@remaining;
}

sub _create_population_estimates
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        labels => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my @labels = @{$parm{'labels'}};
    my @values = @{$parm{'values'}};

    my $doc = $self->_document;

    my $pe = $doc->createElement("PopulationEstimates");

    my $table = $self->_xml->create_single_row_table(table_name => 'MLE', labels => \@labels, values => \@values);
    $pe->appendChild($table);

    return $pe;
}

sub _create_precision_population_estimates
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        labels => { isa => 'ArrayRef', optional => 1 },
        standard_errors => { isa => 'ArrayRef', optional => 1 },
        relative_standard_errors => { isa => 'ArrayRef', optional => 1 },
        correlation_matrix => { isa => 'ArrayRef[ArrayRef]', optional => 1 },
        covariance_matrix => { isa => 'ArrayRef[ArrayRef]', optional => 1 },
    );
    my @labels = defined $parm{'labels'} ? @{$parm{'labels'}} : ();
    my @standard_errors = defined $parm{'standard_errors'} ? @{$parm{'standard_errors'}}: ();
    my @relative_standard_errors = defined $parm{'relative_standard_errors'} ? @{$parm{'relative_standard_errors'}} : ();
    my $correlation_matrix = $parm{'correlation_matrix'};
    my $covariance_matrix = $parm{'covariance_matrix'};

    my $doc = $self->_document;

    my $ppe = $doc->createElement("PrecisionPopulationEstimates");

    if (scalar(@labels) > 0) {
        my $mle = $doc->createElement("MLE");
        $ppe->appendChild($mle);
        if ($self->check_include(element => 'Estimation/PrecisionPopulationEstimates/MLE/StandardError')) {
            my $table = $self->_xml->create_parameter_table(table_name => 'StandardError', name => 'SE', labels => \@labels, values => \@standard_errors);
            $mle->appendChild($table);
        }
        if ($self->check_include(element => 'Estimation/PrecisionPopulationEstimates/MLE/RelativeStandardError')) {
            my $table = $self->_xml->create_parameter_table(table_name => 'RelativeStandardError', name => 'RSE', labels => \@labels,
                values => \@relative_standard_errors);
            $mle->appendChild($table);
        }

        if ($self->check_include(element => 'Estimation/PrecisionPopulationEstimates/MLE/CorrelationMatrix')) {
            my $cor = $doc->createElement("CorrelationMatrix");
            $mle->appendChild($cor);
            my $matrix = $self->_xml->create_matrix(rownames => \@labels, colnames => \@labels, matrix => $correlation_matrix);
            $cor->appendChild($matrix);
        }

        if ($self->check_include(element => 'Estimation/PrecisionPopulationEstimates/MLE/CovarianceMatrix')) {
            my $cov = $doc->createElement("CovarianceMatrix");
            $mle->appendChild($cov);
            my $matrix = $self->_xml->create_matrix(rownames => \@labels, colnames => \@labels, matrix => $covariance_matrix);
            $cov->appendChild($matrix);
        }
    }

    return $ppe;
}

sub _create_bootstrap
{
    my $self = shift;

    my $doc = $self->_document;

    open my $fh, '<', $self->bootstrap_results;

    my @parameters;
    my @percentiles;
    my @column;

    while (<$fh>) {
        if (/^percentile.confidence.intervals$/) {
            my $header = <$fh>;
            my @a = split /","/, $header;
            shift @a;
            shift @a;
            foreach my $param (@a) {
                $param =~ s/\s*//;      # Remove spaces
                if ($param !~ /^se/) {
                    push @parameters, $self->_xml->mangle_symbol_idtype($param);   # FIXME: warning?
                } else {
                    last;
                }
            }

            # Loop through percentiles
            for (my $i = 0; $i < 7; $i++) {
                my $row = <$fh>;
                my @a = split /,/, $row;
                my $percentile = shift @a;
                $percentile =~ s/^"\s*(.*)%"/\1/;
                shift @a;
                my $value;
                for (my $col = 0; $col < scalar(@parameters); $col++) {
                    $value = shift @a;
                    $value =~ s/^\s*(.*)/\1/;
                    if ($value ne 'NA') {
                        push @{$column[$col]}, $value;
                    }
                }
                if ($value ne 'NA') {
                    push @percentiles, $percentile;
                }
            }
        }
    }

    close $fh;

    # Warning if no percentiles
    my $message;
    my $bootstrap;
    if (scalar(@percentiles) == 0) {
        $message = {
            type => "WARNING",
            toolname => "PsN",
            name => "Bootstrap",
            content => "No bootstrap percentiles in " . $self->bootstrap_results . ". No Bootstrap results added.",
            severity => 2,
        };
    } else {
        my $table = $self->_xml->create_table(
            table_name => 'Percentiles',
            column_ids => [ "Percentile", @parameters ],
            column_types => [ ('undefined') x (scalar(@parameters) + 1) ],
            column_valuetypes => [ ('real') x (scalar(@parameters) + 1) ],
            values => [ \@percentiles, @column ],
        );
        $bootstrap = $doc->createElement("Bootstrap");
        $bootstrap->appendChild($table);
    }

    return ($bootstrap, $message);
}

sub _create_task_information
{
    # Create a list of xml messages from an array of hashes of the following form:
    #   type ('ERROR', 'WARNING' etc)
    #   toolname
    #   name
    #   content
    #   severity

    my $self = shift;
    my %parm = validated_hash(\@_,
        messages => { isa => 'ArrayRef[HashRef]', optional => 1 },
        run_time => { isa => 'Num', optional => 1 },
    );
    my @messages = defined $parm{'messages'} ? @{$parm{'messages'}} : ();
    my $run_time = $parm{'run_time'};

    my $doc = $self->_document;

    my $task_information = $doc->createElement("TaskInformation");

    foreach my $message (@messages) {
        my $element = $self->_xml->create_message(message => $message);
        $task_information->appendChild($element);
    }

    if (defined $run_time) {
        my $xml_run_time = $doc->createElement("RunTime");
        $task_information->appendChild($xml_run_time);
        $xml_run_time->appendChild($self->_xml->create_typed_element(type => 'Real', content => $run_time));
    }

    return $task_information;
}

sub _create_raw_results
{
    my $self = shift;

    return if (not defined $self->_used_files);
    return if (scalar(keys %{$self->_used_files}) == 0);

    my $doc = $self->_document;

    my $rr = $doc->createElement("RawResults");

    my $no = 1;
    foreach my $file (keys %{$self->_used_files}) {
        my $datafile = $doc->createElement("DataFile");
        $datafile->setAttribute('oid', "d$no");
        my $description = $doc->createElement("ct:Description");
        $description->appendTextNode($self->_used_files->{$file});
        my $path = $doc->createElement("ds:path");
        $path->appendTextNode(utils::file::remove_path($file));
        $datafile->appendChild($description);
        $datafile->appendChild($path);
        $rr->appendChild($datafile);
        $no++;
    }

    return $rr;
}

sub _add_raw_results_file
{
    # Add a file to the RawResults for this SOBlock
    my $self = shift;
    my $name = shift;
    my $description = shift;

    $self->_used_files({}) if (not defined $self->_used_files);

    $self->_used_files->{$name} = $description;
}

sub _create_likelihood
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        ofv => { isa => 'Maybe[Num]' },
    );
    my $ofv = $parm{'ofv'};

    my $doc = $self->_document;
    my $likelihood;

    if (defined $ofv) {
        $likelihood = $doc->createElement("Likelihood");
        my $deviance = $doc->createElement("Deviance");
        $deviance->appendTextNode($ofv);
        $likelihood->appendChild($deviance);
    }

    return $likelihood;
}

sub _create_predictions
{
     my $self = shift;
     my %parm = validated_hash(\@_,
        sdtab => { isa => 'data' },
    );
    my $sdtab = $parm{'sdtab'};

    if (not (exists $sdtab->column_head_indices->{'ID'} and exists $sdtab->column_head_indices->{'TIME'}
        and exists $sdtab->column_head_indices->{'PRED'} and exists $sdtab->column_head_indices->{'IPRED'})) {
        return;
    }

    $self->_add_raw_results_file($sdtab->filename, "sdtab");

    my $doc = $self->_document;

    my $id = $sdtab->column_to_array(column => "ID");
    my $time = $sdtab->column_to_array(column => "TIME");
    my $pred = $sdtab->column_to_array(column => "PRED");
    my $ipred = $sdtab->column_to_array(column => "IPRED");

    my $predictions = $self->_xml->create_table(
        table_name => "Predictions",
        column_ids => [ "ID", "TIME", "PRED", "IPRED" ],
        column_types => [ "id", "undefined", "undefined", "undefined" ],
        column_valuetypes =>  [ "string", "real", "real", "real" ],
        values => [ $id, $time, $pred, $ipred ],
    );

    return $predictions;
}

sub _create_residuals
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        sdtab => { isa => 'data' },
    );
    my $sdtab = $parm{'sdtab'};

    if (not (exists $sdtab->column_head_indices->{'ID'} and exists $sdtab->column_head_indices->{'TIME'})) {
        return;
    }

    my $doc = $self->_document;

    my $id = $sdtab->column_to_array(column => "ID");
    my $time = $sdtab->column_to_array(column => "TIME");

    my @values = ( $id, $time );
    my @ids = ( "ID", "TIME" );

    if (exists $sdtab->column_head_indices->{'RES'}) {
        my $res = $sdtab->column_to_array(column => "RES");
        push @values, $res;
        push @ids, "RES";
    }

    if (exists $sdtab->column_head_indices->{'IRES'}) {
        my $ires = $sdtab->column_to_array(column => "IRES");
        push @values, $ires;
        push @ids, "IRES";
    }

    if (exists $sdtab->column_head_indices->{'WRES'}) {
        my $wres = $sdtab->column_to_array(column => "WRES");
        push @values, $wres;
        push @ids, "WRES";
    }
    
    if (exists $sdtab->column_head_indices->{'IWRES'}) {
        my $wres = $sdtab->column_to_array(column => "IWRES");
        push @values, $wres;
        push @ids, "IWRES";
    }

    if (scalar(@values) == 2) { # No columns were added
        return;
    }


    $self->_add_raw_results_file($sdtab->filename, "sdtab");

    my $table = $self->_xml->create_table(
        table_name => "Residuals",
        column_ids => \@ids,
        column_types => [ "id", ("undefined") x (scalar(@ids) - 1) ],
        column_valuetypes =>  [ "string", ("real") x (scalar(@ids) - 1) ],
        values => \@values,
    );

    return $table;
}

sub _individual_statistics
{
    # Calculate the median of each individual and return in individual order 

    my %parm = validated_hash(\@_,
        id => { isa => 'ArrayRef' },
        parameter => { isa => 'ArrayRef' },
    );
    my $id = $parm{'id'};
    my $parameter = $parm{'parameter'};

    my @medians = ();
    my @means = ();

    my $current_id = $id->[0];
    my $row = 0;
    my @a = ();

    while (defined $current_id) {
        while ($current_id == $id->[$row]) {
            push @a, $parameter->[$row];
            $row++;
        }
        my $median = array::median(\@a);
        my $mean = array::mean(\@a);
        push @medians, $median;
        push @means, $mean;
        $current_id = $id->[$row];
        @a = ();
    }

    return (\@medians, \@means);
}

sub _create_individual_estimates
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        patab => { isa => 'data' },
        model => { isa => 'model' },
        model_labels => { isa => 'ArrayRef' },
    );
    my $patab = $parm{'patab'};
    my $model = $parm{'model'};
    my $model_labels = $parm{'model_labels'};

    my $eta_names = $model->get_eta_names();
    if (not exists $patab->column_head_indices->{'ID'}) {
        return;
    }

    my $doc = $self->_document;
    my $id = $patab->column_to_array(column => "ID");

    my @labels = @{_get_remaining_columns(header => $patab->column_head_indices, columns => [ 'ID', 'TIME', @$eta_names, @$model_labels ])};

    return if scalar(@labels) == 0;

    my @parameters;
    my @medians;
    my @means;
    for (my $i = 0; $i < scalar(@labels); $i++) {
        $parameters[$i] = $patab->column_to_array(column => $labels[$i]);
        ($medians[$i], $means[$i]) = _individual_statistics(id => $id, parameter => $parameters[$i]);
    }
    my $unique_ids = array::unique($id);

    my $individual_estimates = $doc->createElement("IndividualEstimates");
    if ($self->check_include(element => 'Estimation/IndividualEstimates/Estimates')) {
        my $estimates = $doc->createElement("Estimates");
        $individual_estimates->appendChild($estimates);
        if ($self->check_include(element => 'Estimation/IndividualEstimates/Estimates/Median')) {
            my $table = $self->_xml->create_table(
                table_name => "Median",
                column_ids => [ "ID", @labels ],
                column_types => [ "id", ("undefined") x scalar(@labels) ],
                column_valuetypes => [ "string", ("real") x scalar(@labels) ],
                values => [ $unique_ids, @medians ],
            );
            $estimates->appendChild($table);
        }
        if ($self->check_include(element => 'Estimation/IndividualEstimates/Estimates/Mean')) {
            my $table = $self->_xml->create_table(
                table_name => "Mean",
                column_ids => [ "ID", @labels ],
                column_types => [ "id", ("undefined") x scalar(@labels) ],
                column_valuetypes => [ "string", ("real") x scalar(@labels) ],
                values => [ $unique_ids, @means ],
            );
            $estimates->appendChild($table);
        }
    }
 
    if (scalar(@$eta_names) > 0) {
        # Filter out etas that does not exist in the patab
        $eta_names = _get_included_columns(header => $patab->column_head_indices, columns => $eta_names);

        my @eta_medians = ();
        my @eta_means = ();
        foreach my $eta (@$eta_names) {
            my $column = $patab->column_to_array(column => $eta);
            (my $median, my $mean) = _individual_statistics(id => $id, parameter => $column);
            push @eta_medians, $median;
            push @eta_means, $mean;
        }

        if ($self->check_include(element => 'Estimation/IndividualEstimates/RandomEffects')) {
            my $random_effects = $doc->createElement("RandomEffects");
            $individual_estimates->appendChild($random_effects);

            if ($self->check_include(element => 'Estimation/IndividualEstimates/RandomEffects/EffectMedian')) {
                my $table = $self->_xml->create_table(
                    table_name => "EffectMedian",
                    column_ids => [ "ID", @$eta_names ],
                    column_types => [ "id", ("undefined") x scalar(@$eta_names) ],
                    column_valuetypes => [ "string", ("real") x scalar(@$eta_names) ],
                    values => [ $unique_ids, @eta_medians ],
                );
                $random_effects->appendChild($table);
            }

            if ($self->check_include(element => 'Estimation/IndividualEstimates/RandomEffects/EffectMean')) {
                my $table = $self->_xml->create_table(
                    table_name => "EffectMean",
                    column_ids => [ "ID", @$eta_names ],
                    column_types => [ "id", ("undefined") x scalar(@$eta_names) ],
                    column_valuetypes => [ "string", ("real") x scalar(@$eta_names) ],
                    values => [ $unique_ids, @eta_means ],
                );
                $random_effects->appendChild($table);
            }
        }
    }

    $self->_add_raw_results_file($patab->filename, "patab");

    return $individual_estimates;
}

sub _create_simulation
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        path => { isa => 'Str' },
        table_file => { isa => 'Str', optional => 1 },
        model => { isa => 'model' },
        problem => { isa => 'model::problem' },
    );
    my $path = $parm{'path'};
    my $table_file = $parm{'table_file'};
    my $model = $parm{'model'};
    my $problem = $parm{'problem'};

    my $profiles_table_name = $problem->find_table(columns => [ 'ID', 'TIME', 'DV' ]);
    my $indiv_table_name = $problem->find_table_with_name(name => '^patab', path => $path);
    my $covariates_table_name = $problem->find_table_with_name(name => '^cotab', path => $path);
    #my $population_table_name = $indiv_table_name;

    unless ($profiles_table_name or $indiv_table_name) {
        return undef;
    }

    my $doc = $self->_document;
    my $simulation = $doc->createElement("Simulation");

    open my $profiles_table_fh, '<', $path . $profiles_table_name;
    open my $indiv_table_fh, '<', $path . $indiv_table_name;
    open my $covariates_table_fh, '<', $path . $covariates_table_name;
    #open my $population_table_fh, '<', $path. $population_table_name;

    my $replicate_no = 1;

    for (;;) {      # Loop through simulation replicates aka simulation blocks
        my $sim_block = $doc->createElement("SimulationBlock");
        $sim_block->setAttribute("replicate", $replicate_no);
        my $external_table_name = $self->external_tables ? $table_file . "_$replicate_no" : undef;
        my $simulated_profiles = $self->_create_simulated_profiles(
            file => $profiles_table_fh,
            table_file => $external_table_name
        );
        my $indiv_parameters = $self->_create_indiv_parameters(
            file => $indiv_table_fh,
            table_file => $external_table_name,
            model => $model,
            problem => $problem
        );
        my $covariates = $self->_create_covariates(
            file => $covariates_table_fh,
            table_file => $external_table_name
        );
        #my $population_parameters = $self->_create_population_parameters(
        #    file => $population_table_fh,
        #   table_file => $external_table_name,
        #    problem => $problem,
        #);
        #
        if (defined $simulated_profiles) {
            $sim_block->appendChild($simulated_profiles);
            $self->_add_raw_results_file($profiles_table_name, "simulated profiles");
        }
        if (defined $indiv_parameters) {
            $sim_block->appendChild($indiv_parameters);
            $self->_add_raw_results_file($indiv_table_name, "patab");
        }
        if (defined $covariates) {
            $sim_block->appendChild($covariates);
            $self->_add_raw_results_file($covariates_table_name, "cotab");
        }
        #if (defined $population_parameters) {
        #    $sim_block->appendChild($population_parameters);
        #}
        if (defined $simulated_profiles or defined $indiv_parameters or defined $covariates) {
            $simulation->appendChild($sim_block);
        } else {
            last;
        }
        #last unless (defined $simulated_profiles or defined $indiv_parameters or defined $covariates);# or defined $population_parameters);
        $replicate_no++;
        last if (defined $self->max_replicates and $replicate_no >= $self->max_replicates); 
    }

    #close $population_table_fh;
    close $covariates_table_fh;
    close $indiv_table_fh;
    close $profiles_table_fh;

    return $simulation;
}

sub _read_header
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        file => { isa => 'Ref' },
    );
    my $file = $parm{'file'};

    <$file>;
    my $header = <$file>;
    if (not defined $header) {  #EOF
        return;
    }
    $header =~ s/^\s+//;
    my @columns = split /\s+/, $header; 
    my %colnos;
    for (my $i = 0; $i < scalar(@columns); $i++) {
        $colnos{$columns[$i]} = $i;
    }

    return \%colnos;
}

sub _create_simulated_profiles
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        file => { isa => 'Ref' },
        table_file => { isa => 'Maybe[Str]' },
    );
    my $file = $parm{'file'};
    my $table_file = $parm{'table_file'};

    my $colnosref = $self->_read_header(file => $file);
    return if not defined $colnosref;
    my %colnos = %{$colnosref};

    my @id;
    my @time;
    my @dvid;
    my @dv;

    for (;;) {
        my $row = <$file>;
        last if not defined $row;   # EOF
        if ($row =~ /^TABLE NO/) {
            seek($file, -length($row), 1);      # Unread the line for next iteration
            last;
        }
        chomp($row);
        $row =~ s/^\s+//;
        my @columns = split /\s+/, $row;
        push @id, $columns[$colnos{'ID'}];
        push @time, $columns[$colnos{'TIME'}];
        push @dv, $columns[$colnos{'DV'}];
    }
    @dvid = ((1) x scalar(@id));        # Don't support multiple DVs for now

    my $simulated_profiles = $self->_xml->create_table(
        table_name => "SimulatedProfiles",
        column_ids => [ "ID", "DVID", "TIME", "Observation" ],
        column_types => [ "id", "dvid", "time", "dv" ],
        column_valuetypes => [ "string", "int", "real", "real" ],
        values => [ \@id, \@dvid, \@time, \@dv ],
        table_file => $table_file,
    ); 

    return $simulated_profiles;
}

sub _create_indiv_parameters
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        file => { isa => 'Ref' },
        table_file => { isa => 'Maybe[Str]' },
        model => { isa => 'model' },
        problem => { isa => 'model::problem' },
    );
    my $file = $parm{'file'};
    my $table_file = $parm{'table_file'};
    my $model = $parm{'model'};
    my $problem = $parm{'problem'};

    my @all_labels = @{$problem->get_estimated_attributes(parameter => 'all', attribute => 'labels')};

    my $colnosref = $self->_read_header(file => $file);
    return if not defined $colnosref;
    my %colnos = %{$colnosref};

    my $labels = _get_remaining_columns(header => \%colnos, columns => [ 'ID', 'TIME', @all_labels ]); 

    my $indiv_parameters = $self->_create_occasion_table(
        file => $file,
        labels => $labels,
        table_file => $table_file,
        table_name => 'IndivParameters',
        colnos => \%colnos,
    );

    return $indiv_parameters;
}

sub _create_covariates
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        file => { isa => 'Ref' },
        table_file => { isa => 'Maybe[Str]' },
    );
    my $file = $parm{'file'};
    my $table_file = $parm{'table_file'};

    my $colnosref = $self->_read_header(file => $file);
    return if not defined $colnosref;
    my %colnos = %{$colnosref};

    my $labels = _get_remaining_columns(header => \%colnos, columns => [ 'ID', 'TIME']);

    my $covariates = $self->_create_occasion_table(
        file => $file,
        labels => $labels,
        table_file => $table_file,
        table_name => 'Covariates',
        colnos => \%colnos,
    );

    return $covariates;
}
=cut
sub _create_population_parameters
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        file => { isa => 'Ref' },
        table_file => { isa => 'Maybe[Str]' },
        problem => { isa => 'model::problem' },
    );
    my $file = $parm{'file'};
    my $table_file = $parm{'table_file'};
    my $problem = $parm{'problem'};

    my $theta_labels = $problem->get_estimated_attributes(parameter => 'theta', attribute => 'labels');

    my $colnosref = $self->_read_header(file => $file);
    return if not defined $colnosref;
    my %colnos = %{$colnosref};

    my $labels = _get_included_columns(header => \%colnos, columns => $theta_labels);
    return if (scalar(@$labels) == 0);
    
    my $population_parameters = $self->_create_occasion_table(
        file => $file,
        labels => $labels,
        colnos => \%colnos,
    );

    my @rows;

    my $running_id;
    my $running_occ_start;
    my @running_dv;
    my $prev_time;
    my $max_time;

    for (;;) {
        my $row = <$file>;
        if ($row =~ /^TABLE NO/) {
            seek($file, -length($row), 1);      # Unread the line for next iteration
            $row = undef;
        }
        if (not defined $row) {
            push @rows, [ $running_occ_start, $prev_time, @running_dv ];
            last;
        }
        chomp($row);
        $row =~ s/^\s+//;
        my @columns = split /\s+/, $row;

        my $id = $columns[$colnos{'ID'}];
        my $time = $columns[$colnos{'TIME'}];
        $max_time = $time if $time > $max_time;
        my @dv;
        for my $col (@labels) {
            push @dv, $columns[$colnos{$col}];
        }
        # Check if first id
        if (not defined $running_id) {
            $running_id = $id;
            $running_occ_start = $time;
            @running_dv = @dv;
        }
        # Check if something has changed
        if ($running_id != $id or not array::is_equal(\@dv, \@running_dv)) {
            push @rows, [ $running_id, $running_occ_start, $prev_time, @running_dv ];
            $running_id = $id;
            $running_occ_start = $time;
            @running_dv = @dv;
        }
        $prev_time = $time;
    }

    my $table = $self->_xml->create_table(
        table_name => "PopulationParameters",
        column_ids => [ "ID", "OccasionStart", "OccationEnd", @labels ],
        column_types => [ "id", "time", "time", ("undefined") x scalar(@labels) ],
        column_valuetypes => [ "string", "real", "real", ("real") x scalar(@labels) ],
        values => \@rows,
        row_major => 1,
        table_file => $table_file,
    );


    return $population_parameters;
}
=cut
sub _create_occasion_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        file => { isa => 'Ref' },
        labels => { isa => 'ArrayRef' },
        table_file => { isa => 'Maybe[Str]' },
        table_name => { isa => 'Str' },
        colnos => { isa => 'HashRef' },
    );
    my $file = $parm{'file'};
    my @labels = @{$parm{'labels'}};
    my $table_file = $parm{'table_file'};
    my $table_name = $parm{'table_name'};
    my %colnos = %{$parm{'colnos'}};

    my @rows;

    my $running_id;
    my $running_occ_start;
    my @running_dv;
    my $prev_time;

    for (;;) {
        my $row = <$file>;
        if ($row =~ /^TABLE NO/) {
            seek($file, -length($row), 1);      # Unread the line for next iteration
            $row = undef;
        }
        if (not defined $row) {
            push @rows, [ $running_id, $running_occ_start, $prev_time, @running_dv ];
            last;
        }
        chomp($row);
        $row =~ s/^\s+//;
        my @columns = split /\s+/, $row;

        my $id = $columns[$colnos{'ID'}];
        my $time = $columns[$colnos{'TIME'}];
        my @dv;
        for my $col (@labels) {
            push @dv, $columns[$colnos{$col}];
        }
        # Check if first id
        if (not defined $running_id) {
            $running_id = $id;
            $running_occ_start = $time;
            @running_dv = @dv;
        }
        # Check if something has changed
        if ($running_id != $id or not array::is_equal(\@dv, \@running_dv)) {
            push @rows, [ $running_id, $running_occ_start, $prev_time, @running_dv ];
            $running_id = $id;
            $running_occ_start = $time;
            @running_dv = @dv;
        }
        $prev_time = $time;
    }

    my $table = $self->_xml->create_table(
        table_name => $table_name,
        column_ids => [ "ID", "OccasionStart", "OccationEnd", @labels ],
        column_types => [ "id", "time", "time", ("undefined") x scalar(@labels) ],
        column_valuetypes => [ "string", "real", "real", ("real") x scalar(@labels) ],
        values => \@rows,
        row_major => 1,
        table_file => $table_file,
    );

    return $table;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
