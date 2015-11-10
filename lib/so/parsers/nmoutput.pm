package so::parsers::nmoutput;

# Package for parsing NONMEM output into an so object

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
use linear_algebra;
use utils::file;
use PsN;
use so;
use so::soblock;
use so::soblock::simulation::simulationblock;

has 'so' => ( is => 'rw', isa => 'so' );
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has '_so_block' => ( is => 'rw', isa => 'so::soblock' );
has 'use_tables' => ( is => 'rw', isa => 'Bool', default => 1 );    # Set to zero if sdtab and patab should not be used
has 'toolname' => ( is => 'rw', isa => 'Str', default => 'NONMEM' );
has 'max_replicates' => ( is => 'rw', isa => 'Maybe[Int]' );        # Maximum number of simulation replicates to add
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'external_tables' => ( is => 'rw', isa => 'Bool', default => 0 );   # For now a bool to specify if external tables should be created
has 'labels_hash' => ( is => 'rw', isa => 'HashRef' );  # List of labels on sd/corr scale etc. Intended for external use i.e. bootstrap
has 'extra_output' => ( is => 'rw', isa => 'Maybe[ArrayRef]' );
has '_document' => ( is => 'rw', isa => 'Ref' );                    # The XML document 
has '_duplicate_blocknames' => ( is => 'rw', isa => 'HashRef' );    # Contains those blocknames which will have duplicates with next number for block
has '_so_path' => ( is => 'rw', isa => 'Str' );                     # The path of the output SO file

sub BUILD
{
    my $self = shift;

    if (defined $self->max_replicates) {
        if ($self->max_replicates < 1) {
            croak("max_replicates must be a positive number");
        }
    }

    my $file_stem = utils::file::get_file_stem($self->lst_file);
    my $so_block = $self->so->create_block(name => $file_stem);

    $self->_so_block($so_block);

    $self->_parse_lst_file();
}

sub _parse_lst_file
{
    my $self = shift;

    # Parse one lst-file and put it into an SOBlock
    my $lst_file = $self->lst_file;

    if ($self->verbose) {
        print "Adding $lst_file\n";
    }

    my $path = utils::file::directory($lst_file);

    my $elapsed_time = 0;
    my @on_sd_scale = ();

    my $file_stem = utils::file::get_file_stem($lst_file);

    my $block = $self->_so_block;
 
    my $estimation;
    my $simulation;

    # Check that the output file exist before trying to read it.
    if (not -e $lst_file) {
        $self->_so_block->TaskInformation->add_message(
            type => "ERROR",
            toolname => $self->toolname,
            name => "File error",
            content => "The file: \"" . $lst_file . "\" does not exist",
            severity => 10,
        );
    } else {
        $self->_so_block->RawResults->add_datafile(name => $lst_file, description => "NONMEM results file");

        my $outobj = output->new(filename => $lst_file);
        if (not $outobj->parsed_successfully) {
            $self->_so_block->TaskInformation->add_message(
                type => "ERROR",
                toolname => $self->toolname,
                name => "Parsing error", 
                content => "Outputfile not parsed successfully, error message: " . $outobj->parsing_error_message,
                severity => 10,
            );
        } else {
            my $problems = 0; #TODO check if first $PROB is prior, then should be =1 here, as in e.g. sse script
            my $sub_problems = 0;  #always 0 since we do not have workflow simulation + estimation?

            if ($outobj->problems->[$problems]->subproblems->[$sub_problems]->NM7_parsed_raw) {
                $self->_so_block->RawResults->add_datafile(name => "$file_stem.ext", description => "NONMEM Raw output file");
            }

            my $condition_number = $outobj->problems->[$problems]->subproblems->[$sub_problems]->condition_number;
            if (defined $condition_number) {
                $self->_so_block->Estimation->PrecisionPopulationEstimates->MLE->ConditionNumber($condition_number);
            }

            my $model = $outobj->lst_model;

            my $eta_shrinkage = $outobj->shrinkage_eta();
            my $eps_shrinkage = $outobj->shrinkage_eps();
            my $observation_records = $outobj->nobs();
            my $individuals = $outobj->nind();

            my @etashrinkage = defined $eta_shrinkage -> [$problems][$sub_problems] ? @{$eta_shrinkage -> [$problems][$sub_problems]} : ();
            my @epsshrinkage = defined $eps_shrinkage -> [$problems][$sub_problems] ? @{$eps_shrinkage -> [$problems][$sub_problems]} : ();

            ## Termination
            my $ofv = $outobj -> get_single_value(attribute => 'ofv',
                problem_index => $problems,
                subproblem_index => $sub_problems);

            my $covariance_step_run = $outobj->covariance_step_run->[$problems];

            my $covariance_step_successful = 0;
            if ($covariance_step_run) {
                if ($outobj->covariance_step_successful->[$problems][$sub_problems] ne '0') {
                    $covariance_step_successful = 1;
                }
            }

            $self->_add_status_messages(output => $outobj, problem => $problems, subproblem => $sub_problems);

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
            my @filtered_inits = @{$model->problems->[$problems]->get_estimated_attributes(parameter => 'all', attribute => 'inits')};

            my @all_labels = @filtered_labels; #will add fix with label here
            my @all_inits = @filtered_inits;

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
                    push @all_inits, $option->init;
                }
                if ($option->sd or $option->corr) {     # Save labels for parameters on sd/corr scale
                    if (grep { $_ eq $option->label } @all_labels) {
                        push @on_sd_scale, $option->label;
                    }
                }
            }

            $self->labels_hash({ labels => \@all_labels, on_sd_scale => \@on_sd_scale });

            if ($estimation_step_run or $simulation_step_run) {
                foreach my $label (@all_labels, @on_sd_scale) {
                    if (not so::xml::match_symbol_idtype($label)) {
                        my $old_label = $label;
                        $label = so::xml::mangle_symbol_idtype($label);
                        $self->_so_block->TaskInformation->add_message(
                            type => "WARNING",
                            toolname => "nmoutput2so",
                            name => "Name change",
                            content => "Parameter label \"$old_label\" not specified or not a legal symbolIdType. Setting/changing it to: $label",
                            severity => 1,
                        );
                    }
                }
				#repeat same procedure for filtered_labels, duplication, do not add message again
                foreach my $label (@filtered_labels) {
                    if (not so::xml::match_symbol_idtype($label)) {
                        my $old_label = $label;
                        $label = so::xml::mangle_symbol_idtype($label);
                    }
                }
            }
            
            if ($estimation_step_run) {
                #Calculate relative standard errors, only for estimated values that have se
                my @rel_se = ();

                if ($covariance_step_successful) {
                    for (my $i = 0; $i < scalar(@se_values); $i++) {
                        if ($est_values[$i] == 0) {
                            push @rel_se, undef;
                        } else { 
                            push @rel_se, 100 * $se_values[$i] / abs($est_values[$i]);
                        }
                    }
                }

                my $minimization_message = $outobj -> get_single_value(attribute => 'minimization_message',
                    problem_index => $problems,
                    subproblem_index => $sub_problems);

                my $undefs = grep { not defined $_ } @est_values;
                if ($undefs != scalar(@est_values)) {   # Check that not all in list are undef. Should possibly have been done earlier
                    $self->_so_block->Estimation->PopulationEstimates->create_MLE(labels => \@all_labels, values => \@est_values);
                }

                if ($covariance_step_successful) {
                    my $correlation_matrix = linear_algebra::triangular_symmetric_to_full($outobj->correlation_matrix->[$problems]->[$sub_problems]);
                    my $covariance_matrix = linear_algebra::triangular_symmetric_to_full($outobj->covariance_matrix->[$problems]->[$sub_problems]);
                    my $additional = $outobj->problems->[$problems]->subproblems->[$sub_problems]->NM7_parsed_additional;
                    if (defined $additional) {
                        if ($additional->{'cov'}) {
                            $self->_so_block->RawResults->add_datafile(name => "$file_stem.cov", description => "NONMEM Covariance matrix");
                        }
                        if ($additional->{'cor'}) {
                            $self->_so_block->RawResults->add_datafile(name => "$file_stem.cor", description => "NONMEM Correlation matrix");
                        }
                    }
                    $self->_so_block->Estimation->PrecisionPopulationEstimates->MLE->create(
                        labels => \@filtered_labels,
                        standard_errors => \@se_values,
                        relative_standard_errors => \@rel_se,
                        correlation_matrix => $correlation_matrix,
                        covariance_matrix => $covariance_matrix,
                    );
                }

                # Loop through the different tables
                if ($self->use_tables) {
                    my ($table_name_ref, $dummy) = $model->problems->[$problems]->_option_val_pos(record_name => 'table', name => 'FILE');
                    if (defined $table_name_ref and scalar @{$table_name_ref} >= 0) {
                        foreach my $table (@$table_name_ref) {
                            if ($table =~ /^(sdtab|patab)/ and not -e ($path . $table)) {
                                $self->_so_block->TaskInformation->add_message(
                                    type => "WARNING",
                                    toolname => $self->toolname,
                                    name => "File error",
                                    content => "Could not find table $path$table. Results from this table could not be added.",
                                    severity => 1,
                                );
                                next;
                            }
                            if ($table =~ /^sdtab/) {
                                my $sdtab = data->new(
                                    directory => $path,
                                    filename => $table,
                                    ignoresign => '@',
                                    parse_header => 1,
                                );
                                $self->_create_residuals(sdtab => $sdtab);
                                $self->_create_predictions(sdtab => $sdtab);
                            }
                            if ($table =~ /^patab/) {
                                my $patab = data->new(
                                    directory => $path,
                                    filename => $table,
                                    ignoresign => '@',
                                    parse_header => 1,
                                );
                                $self->_create_individual_estimates(
                                    patab => $patab,
                                    model => $model,
                                    model_labels => \@all_labels
                                );
                            }
                        }
                    }
                }

                $self->_so_block->Estimation->Likelihood->Deviance($ofv);

                if (not defined $ofv) {
                    $self->_so_block->TaskInformation->add_message(
                        type => 'ERROR',
                        toolname => $self->toolname,
                        name => "Minimzation error",
                        content => join('', @{$minimization_message}),
                        severity => 5,
                    );
                }
            }

            if (defined $outobj->runtime) {
                $outobj->runtime =~ m/(\d+):(\d+):(\d+)/;
                $elapsed_time = $1 + $2 / 60 + $3 / 3600;
                $self->_so_block->TaskInformation->RunTime->Real($elapsed_time);
                $self->_so_block->TaskInformation->RunTime->Description("Run time in hours");
            }

            if ($simulation_step_run and $self->use_tables) {
                $self->_create_simulation(
                    model => $model,
                    problem => $model->problems->[$problems],
                    path => $path,
                    table_file => $file_stem,
                    labels => \@all_labels,
                    inits => \@all_inits,
                );
            }

        }
    }

    $self->_so_block->TaskInformation->add_message(
        type => "INFORMATION",
        toolname => "nmoutput2so",
        name => "nmoutput2so_version",
        content => "This SOBlock was created with nmoutput2so version " . $PsN::version,
        severity => 0,
    );
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

    $self->_so_block->RawResults->add_datafile(name => $sdtab->filename, description => "sdtab");

    my $doc = $self->_document;

    my $id = $sdtab->column_to_array(column => "ID");
    my $id = [ map { int($_) } @$id ];
    my $time = $sdtab->column_to_array(column => "TIME");
    my $pred = $sdtab->column_to_array(column => "PRED");
    my $ipred = $sdtab->column_to_array(column => "IPRED");

    my $column_id = [ "ID", "TIME", "PRED", "IPRED" ];
    my $column_type = [ "id", "undefined", "undefined", "undefined" ];
    my $value_type = [ "string", "real", "real", "real" ];
    my $columns = [ $id, $time, $pred, $ipred ];

    if (exists $sdtab->column_head_indices->{'DVID'}) {
        push @$column_id, "DVID";
        push @$column_type, "undefined";
        push @$value_type, "int";
        push @$columns, $sdtab->column_to_array(column => "DVID");
    }

    my $predictions = so::table->new(
        name => "Predictions",
        columnId => $column_id,
        columnType => $column_type,
        valueType =>  $value_type,
        columns => $columns,
    );

    $self->_so_block->Estimation->Predictions($predictions);
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
    my $id = [ map { int($_) } @$id ];
    my $time = $sdtab->column_to_array(column => "TIME");

    my @values = ( $id, $time );
    my @ids = ( "ID", "TIME" );
    my @value_type = ( "string", "real" );

    if (exists $sdtab->column_head_indices->{'RES'}) {
        my $res = $sdtab->column_to_array(column => "RES");
        push @values, $res;
        push @ids, "RES";
        push @value_type, "real";
    }

    if (exists $sdtab->column_head_indices->{'IRES'}) {
        my $ires = $sdtab->column_to_array(column => "IRES");
        push @values, $ires;
        push @ids, "IRES";
        push @value_type, "real";
    }

    if (exists $sdtab->column_head_indices->{'WRES'}) {
        my $wres = $sdtab->column_to_array(column => "WRES");
        push @values, $wres;
        push @ids, "WRES";
        push @value_type, "real";
    }
    
    if (exists $sdtab->column_head_indices->{'IWRES'}) {
        my $iwres = $sdtab->column_to_array(column => "IWRES");
        push @values, $iwres;
        push @ids, "IWRES";
        push @value_type, "real";
    }

    if (exists $sdtab->column_head_indices->{'CWRES'}) {
        my $cwres = $sdtab->column_to_array(column => "CWRES");
        push @values, $cwres;
        push @ids, "CWRES";
        push @value_type, "real";
    }

    if (scalar(@values) == 2) { # No columns were added
        return;
    }

    if (exists $sdtab->column_head_indices->{'DVID'}) {
        my $dvid = $sdtab->column_to_array(column => "DVID");
        push @values, $dvid;
        push @ids, "DVID";
        push @value_type, "int";
    }

    # Remove MDV rows
    if (exists $sdtab->column_head_indices->{'MDV'}) {
        my $mdv = $sdtab->column_to_array(column => "MDV");
        foreach my $column (@values) {
            my @new_col = map { int($mdv->[$_]) ? () : $column->[$_] } 0 .. scalar(@$mdv) - 1;
            $column = \@new_col;
        }
    }

    $self->_so_block->RawResults->add_datafile(name => $sdtab->filename, description => "sdtab");

    my $table = so::table->new(
        name => "ResidualTable",
        columnId => \@ids,
        columnType => [ "id", ("undefined") x (scalar(@ids) - 1) ],
        valueType =>  \@value_type,
        columns => \@values,
    );
    $self->_so_block->Estimation->Residuals->ResidualTable($table);
}

sub _create_eta_table
{
    my %parm = validated_hash(\@_,
        id => { isa => 'ArrayRef' },
        occ => { isa => 'Maybe[ArrayRef]', optional => 1 },
        etas => { isa => 'ArrayRef' },
    );
    my $id = $parm{'id'};
    my $occ = $parm{'occ'};
    my $etas = $parm{'etas'};

    my $numcols = 1 + scalar(@$etas);
    if (defined $occ) {
        $numcols++;
    }

    my @results;
    for (my $i = 0; $i < $numcols; $i++) {
        push @results, [];
    }

    my $prev_id, my $prev_occ;
    for (my $i = 0; $i < scalar(@$id); $i++) {
        if ($id->[$i] != $prev_id or (defined $occ and $occ->[$i] != $prev_occ)) {
            $prev_id = $id->[$i];
            if (defined $occ) {
                $prev_occ = $occ->[$i];
            }
            push @{$results[0]}, $id->[$i];
            my $k = 1;
            if (defined $occ) {
                push @{$results[1]}, $occ->[$i];
                $k++;
            }
            for (my $j = 0; $j < scalar(@$etas); $j++) {
                push @{$results[$k++]}, $etas->[$j]->[$i];
            }
        }
    }

    return \@results;
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

    (my $occasion, my $iov_etas) = $self->_get_iov_etas(model => $model);

    if (not exists $patab->column_head_indices->{'ID'}) {
        return;
    }

    my $id = $patab->column_to_array(column => "ID");
    $id = [ map { int($_) } @$id ];

    my @columns_to_remove = ( 'ID', 'TIME', @$eta_names, @$model_labels);
    if (defined $occasion) {
        push @columns_to_remove, $occasion;
        push @columns_to_remove, @$iov_etas;
    }

    my @labels = @{_get_remaining_columns(header => $patab->column_head_indices, columns => \@columns_to_remove)};

    return if scalar(@labels) == 0;

    my @parameters;
    my @medians;
    my @means;
    for (my $i = 0; $i < scalar(@labels); $i++) {
        $parameters[$i] = $patab->column_to_array(column => $labels[$i]);
        ($medians[$i], $means[$i]) = _individual_statistics(id => $id, parameter => $parameters[$i]);
    }
    my $array_of_ids = array::remove_adjacent_duplicates($id);

    my $table = so::table->new(
        name => "Median",
        columnId => [ "ID", @labels ],
        columnType => [ "id", ("undefined") x scalar(@labels) ],
        valueType => [ "string", ("real") x scalar(@labels) ],
        columns => [ $array_of_ids, @medians ],
    );
    $self->_so_block->Estimation->IndividualEstimates->Estimates->Median($table);

    my $table = so::table->new(
        name => "Mean",
        columnId => [ "ID", @labels ],
        columnType => [ "id", ("undefined") x scalar(@labels) ],
        valueType => [ "string", ("real") x scalar(@labels) ],
        columns => [ $array_of_ids, @means ],
    );
    $self->_so_block->Estimation->IndividualEstimates->Estimates->Mean($table);

    if (defined $occasion) {
        push @$eta_names, @$iov_etas;
    }

    if (scalar(@$eta_names) > 0) {
        
        # Filter out etas that does not exist in the patab
        $eta_names = _get_included_columns(header => $patab->column_head_indices, columns => $eta_names);
        if (scalar(@$eta_names) > 0) {

            my @eta_medians = ();
            my @eta_means = ();
            my @etas;
            foreach my $eta (@$eta_names) {
                my $column = $patab->column_to_array(column => $eta);
                push @etas, $column;
            }

            my $occasion_data;
            if (defined $occasion) {
                $occasion_data = $patab->column_to_array(column => $occasion);
            }
            my $eta_table = _create_eta_table(id => $id, occ => $occasion_data, etas => \@etas); 

            my @columnId = ( "ID" );
            if (defined $occasion) {
                push @columnId, $occasion;
            }
            push @columnId, @$eta_names;

            my @columnType = ( "id" );
            if (defined $occasion) {
                push @columnType, "occasion";
            }
            push @columnType, ("undefined") x scalar(@$eta_names);

            my @valueType = ( "string" );
            if (defined $occasion) {
                push @valueType, "real";
            }
            push @valueType, ( "real" ) x scalar(@$eta_names);

            my $table = so::table->new(
                name => "EffectMedian",
                columnId => \@columnId,
                columnType => \@columnType,
                valueType => \@valueType,
                columns => $eta_table,
            );
            $self->_so_block->Estimation->IndividualEstimates->RandomEffects->EffectMedian($table);

            my $table = so::table->new(
                name => "EffectMean",
                columnId => \@columnId,
                columnType => \@columnType,
                valueType => \@valueType,
                columns => $eta_table,
            );
            $self->_so_block->Estimation->IndividualEstimates->RandomEffects->EffectMean($table);
        }
    }

    $self->_so_block->RawResults->add_datafile(name => $patab->filename, description => "patab");
}

sub _get_iov_etas
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my @code;
	if ($model->has_code(record => 'pk')) {
		@code = @{$model->get_code(record => 'pk')};
	} else {
		@code = @{$model->get_code(record => 'pred')};
	}

    my @names;
    my $occasion;

    foreach my $line (@code) {
        if ($line =~ /^\s*(\w+)\s*=\s*ETA\((\w+)_\1\)/) {
            push @names, $1;
            $occasion = $2;
        }
    }

    return ($occasion, \@names); 
}

sub _create_simulation
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        path => { isa => 'Str' },
        table_file => { isa => 'Str', optional => 1 },
        model => { isa => 'model' },
        problem => { isa => 'model::problem' },
        labels => { isa => 'ArrayRef' },
        inits => { isa => 'ArrayRef' },
    );
    my $path = $parm{'path'};
    my $table_file = $parm{'table_file'};
    my $model = $parm{'model'};
    my $problem = $parm{'problem'};
    my $labels = $parm{'labels'};
    my $inits = $parm{'inits'};

    my $dosing = $self->_create_dosing(model => $model, problem => $problem);
    my $data_columns = $problem->inputs->[0]->get_nonskipped_columns;
    my $have_AMT = grep { $_ eq 'AMT' } @$data_columns;
    my $have_PK = $model->has_code(record => 'pk');

    my $profiles_table_name = $problem->find_table(columns => [ 'ID', 'TIME', 'DV' ]);
    my $indiv_table_name = $problem->find_table_with_name(name => '^patab', path => $path);
    my $random_effects_table_name = $problem->find_table_with_name(name => '^patab', path => $path);
    my $covariates_table_name = $problem->find_table_with_name(name => '^cotab', path => $path);
    my $population_table_name = $problem->find_table(columns => [ 'TIME' ]);

    my $extra_output_table_name;
    if (defined $self->extra_output) {
        $extra_output_table_name = $problem->find_table(columns => [ 'ID', 'TIME', @{$self->extra_output} ]);
    }

    unless ($profiles_table_name or $indiv_table_name or $covariates_table_name) {
        return;
    }

    open my $profiles_table_fh, '<', $path . $profiles_table_name;
    open my $indiv_table_fh, '<', $path . $indiv_table_name;
    open my $random_effects_table_fh, '<', $path . $random_effects_table_name;
    open my $covariates_table_fh, '<', $path . $covariates_table_name;
    open my $population_table_fh, '<', $path . $population_table_name;
    my $extra_output_table_fh;
    if (defined $extra_output_table_name) {
        open $extra_output_table_fh, '<', $path . $extra_output_table_name;
    }

    my $replicate_no = 1;

    for (;;) {      # Loop through simulation replicates aka simulation blocks
        my $sim_block = so::soblock::simulation::simulationblock->new(replicate => $replicate_no);
        my $external_table_name = $self->external_tables ? $table_file . "_$replicate_no" : undef;
        my $simulated_profiles = $self->_create_simulated_profiles(
            file => $profiles_table_fh,
            table_file => $external_table_name,
        );
        my $extra_output_simulated_profiles;
        if (defined $extra_output_table_fh) {
            $extra_output_simulated_profiles = $self->_create_simulated_profiles(
                file => $extra_output_table_fh,
                table_file => $external_table_name,
                name => "append_columns",
                dv_columns => $self->extra_output,
            ); 
        }
        my $indiv_parameters = $self->_create_indiv_parameters(
            file => $indiv_table_fh,
            table_file => $external_table_name,
            model => $model,
            problem => $problem
        );
        my $random_effects = $self->_create_random_effects(
            file => $random_effects_table_fh,
            table_file => $external_table_name,
            model => $model,
            problem => $problem
        );
        my $covariates = $self->_create_covariates(
            file => $covariates_table_fh,
            table_file => $external_table_name
        );
        my $population_parameters = $self->_create_population_parameters(
            file => $population_table_fh,
            table_file => $external_table_name,
            labels => $labels,
            inits => $inits,
        );

        if (defined $simulated_profiles) {
            push @{$sim_block->SimulatedProfiles}, $simulated_profiles;
            $self->_so_block->RawResults->add_datafile(name => $profiles_table_name, description => "simulated profiles");
        }
        if (defined $extra_output_simulated_profiles) {
           push @{$sim_block->SimulatedProfiles}, $extra_output_simulated_profiles;
           $self->_so_block->RawResults->add_datafile(name => $extra_output_table_name, description => "simulated profiles");
        }
        if (defined $indiv_parameters) {
            $sim_block->IndivParameters($indiv_parameters);
            $self->_so_block->RawResults->add_datafile(name => $indiv_table_name, description => "patab");
        }
        if (defined $random_effects) {
            $sim_block->RandomEffects($random_effects);
            $self->_so_block->RawResults->add_datafile(name => $random_effects_table_name, description => "patab");
        }
        if (defined $covariates) {
            $sim_block->Covariates($covariates);
            $self->_so_block->RawResults->add_datafile(name => $covariates_table_name, description => "cotab");
        }
        if (defined $population_parameters) {
            $sim_block->PopulationParameters($population_parameters);
        }
        if (defined $dosing) {      # The same for all replicates
            $sim_block->Dosing($dosing);
        }
        if (defined $simulated_profiles or defined $indiv_parameters or defined $random_effects or defined $covariates or defined $population_parameters) {
            $self->_so_block->Simulation([]) if not defined $self->_so_block->Simulation;
            push @{$self->_so_block->Simulation->SimulationBlock}, $sim_block;
        } else {
            last;
        }
        last unless (defined $simulated_profiles or defined $indiv_parameters or defined $random_effects or defined $covariates or defined $population_parameters);
        $replicate_no++;
        last if (defined $self->max_replicates and $replicate_no >= $self->max_replicates); 
    }

    close $covariates_table_fh;
    close $indiv_table_fh;
    close $random_effects_table_fh;
    close $profiles_table_fh;
    if (defined $extra_output_table_fh) {
        close $extra_output_table_fh;
    }
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
        name => { isa => 'Str', optional => 1 },
        dv_columns => { isa => 'ArrayRef', optional => 1 },
    );
    my $file = $parm{'file'};
    my $table_file = $parm{'table_file'};
    my $name = $parm{'name'};
    my $dv_columns = $parm{'dv_columns'};

    my $colnosref = $self->_read_header(file => $file);
    return if not defined $colnosref;
    my %colnos = %{$colnosref};

    my @id;
    my @time;
    my @dvid;
    my @dv;

    my @extra;

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
        if (not exists $colnos{'MDV'} or $columns[$colnos{'MDV'}] == 0) {
            push @id, int($columns[$colnos{'ID'}]);
            push @time, $columns[$colnos{'TIME'}];
            if (defined $dv_columns) {
                for (my $i = 0; $i < scalar(@$dv_columns); $i++) {
                    push @{$extra[$i]}, $columns[$colnos{$dv_columns->[$i]}];
                }
            } else {
                push @dv, $columns[$colnos{'DV'}];
                if (exists $colnos{'DVID'}) {
                    push @dvid, int($columns[$colnos{'DVID'}]);
                } else {
                    push @dvid, 1;
                }
            }
        }
    }

    my $simulated_profiles;
    if (defined $dv_columns) {    # This duplicated code is due to possible future usage of real output names from PharmML instead of DVID
        $simulated_profiles = so::soblock::simulation::simulationblock::simulationtable->new(
            name => "SimulatedProfiles",
            columnId => [ "ID", "TIME", @$dv_columns ],
            columnType => [ "id", "time", ("dv") x scalar(@$dv_columns) ],
            valueType => [ "string", "real", ("real") x scalar(@$dv_columns) ],
            columns => [ \@id, \@time, @extra ],
            table_file => $table_file,
        ); 
    } else {
        $simulated_profiles = so::soblock::simulation::simulationblock::simulationtable->new(
            name => "SimulatedProfiles",
            columnId => [ "ID", "DVID", "TIME", "Observation" ],
            columnType => [ "id", "dvid", "time", "dv" ],
            valueType => [ "string", "int", "real", "real" ],
            columns => [ \@id, \@dvid, \@time, \@dv ],
            table_file => $table_file,
        );
    }

    if (defined $name) {
        $simulated_profiles->attr_name($name);
    }

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

    my $eta_names = $model->get_eta_names();

    (my $occasion, my $iov_etas) = $self->_get_iov_etas(model => $model);
    my @columns_for_removal = ( 'ID', 'TIME', @all_labels, @$eta_names );
    if (defined $occasion) {
        push @columns_for_removal, @$iov_etas, $occasion;
    }

    my $labels = _get_remaining_columns(header => \%colnos, columns => \@columns_for_removal); 

    my $indiv_parameters = $self->_create_occasion_table(
        file => $file,
        labels => $labels,
        table_file => $table_file,
        table_name => 'IndivParameters',
        colnos => \%colnos,
    );

    return $indiv_parameters;
}

sub _create_random_effects
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

    my $colnosref = $self->_read_header(file => $file);
    return if not defined $colnosref;

    my $eta_names = $model->get_eta_names();

    (my $occasion, my $iov_etas) = $self->_get_iov_etas(model => $model);
    if (defined $occasion) {
        push @$eta_names, @$iov_etas;
    }

    my $random_effects = $self->_create_occasion_table(
        file => $file,
        labels => $eta_names,
        table_file => $table_file,
        table_name => 'RandomEffects',
        colnos => $colnosref,
        occasion => $occasion,
    );

    return $random_effects;
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

sub _create_population_parameters
{
    # Assume that the population parameters are constant and find the largest time period of any simulation replicate

    my $self = shift;
    my %parm = validated_hash(\@_,
        file => { isa => 'Ref' },
        table_file => { isa => 'Maybe[Str]' },
        labels => { isa => 'ArrayRef' },
        inits => { isa => 'ArrayRef' },
    );
    my $file = $parm{'file'};
    my $table_file = $parm{'table_file'};
    my $labels = $parm{'labels'};
    my @inits = @{$parm{'inits'}};
 
    my $colnosref = $self->_read_header(file => $file);
    return if not defined $colnosref;
    my %colnos = %{$colnosref};

    # Find minimum and maximum of TIME
    my @time;

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
        push @time, $columns[$colnos{'TIME'}];
    }

    my @columns;
    foreach my $e (@inits) {
        push @columns, [ $e ];
    }

    my $population_parameters = so::table->new(
        name => "PopulationParameters",
        columnId => [ "occasionStart", "occasionEnd", @{$labels} ],
        columnType => [ "time", "time", ("undefined") x scalar(@inits) ],
        valueType => [ "real", "real", ("real") x scalar(@inits) ],
        columns => [ [ scalar(array::min(\@time)) ], [ scalar(array::max(\@time)) ],  @columns ],
        table_file => $table_file,
    ); 

    return $population_parameters;
}

sub _create_occasion_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        file => { isa => 'Ref' },
        labels => { isa => 'ArrayRef' },
        table_file => { isa => 'Maybe[Str]' },
        table_name => { isa => 'Str' },
        colnos => { isa => 'HashRef' },
        occasion => { isa => 'Maybe[Str]', optional => 1 },
    );
    my $file = $parm{'file'};
    my @labels = @{$parm{'labels'}};
    my $table_file = $parm{'table_file'};
    my $table_name = $parm{'table_name'};
    my %colnos = %{$parm{'colnos'}};
    my $occasion = $parm{'occasion'};

    my @rows;

    my $running_id;
    my $running_occ_start;
    my $running_occ;
    my @running_dv;
    my $prev_time;

    for (;;) {
        my $row = <$file>;
        if ($row =~ /^TABLE NO/) {
            seek($file, -length($row), 1);      # Unread the line for next iteration
            $row = undef;
        }
        if (not defined $row) {
            my @new_row = ( int($running_id), $running_occ_start, $prev_time, @running_dv );
            if (defined $occasion) {
                push @new_row, int($running_occ);
            }
            push @rows, \@new_row;
            last;
        }
        chomp($row);
        $row =~ s/^\s+//;
        my @columns = split /\s+/, $row;

        my $id = $columns[$colnos{'ID'}];
        my $time = $columns[$colnos{'TIME'}];
        my $occ;
        if (defined $occasion) {
            $occ = $columns[$colnos{$occasion}];
        }
        my @dv;
         for my $col (@labels) {
            push @dv, $columns[$colnos{$col}];
        }
        # Check if first id
        if (not defined $running_id) {
            $running_id = $id;
            $running_occ_start = $time;
            $running_occ = $occ;
            @running_dv = @dv;
        }
        # Check if something has changed
        if ($running_id != $id or not array::is_equal(\@dv, \@running_dv) or (defined $occasion and $running_occ != $occ)) {
            my @new_row = ( int($running_id), $running_occ_start, $prev_time, @running_dv );
            if (defined $occasion) {
                push @new_row, int($running_occ);
            }
            push @rows, \@new_row;
            $running_id = $id;
            $running_occ_start = $time;
            $running_occ = $occ;
            @running_dv = @dv;
        }
        $prev_time = $time;
    }

    linear_algebra::transpose(\@rows);

    my @columnId = ( "ID", "OccasionStart", "OccasionEnd", @labels );
    my @columnType = ( "id", "time", "time", ("undefined") x scalar(@labels) );
    my @valueType = ( "string", "real", "real", ("real") x scalar(@labels) );
    if (defined $occasion) {
        push @columnId, $occasion;
        push @columnType, "occasion";
        push @valueType, "real";
    }
    my $table = so::table->new(
        name => $table_name,
        columnId => \@columnId,
        columnType => \@columnType,
        valueType => \@valueType,
        columns => \@rows,
        table_file => $table_file,
    );

    return $table;
}

sub _create_dosing
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        problem => { isa => 'model::problem' },
    );
    my $model = $parm{'model'};
    my $problem = $parm{'problem'};

    # Only support $PK
    if (not $model->has_code(record => 'pk')) {
        return;
    }

    my $data_columns = $problem->inputs->[0]->get_nonskipped_columns;
    my $ID_col;
    my $TIME_col;
    my $AMT_col;
    my $RATE_col;
    my $SS_col;
    my $II_col;

    for (my $col = 0; $col < scalar(@{$problem->inputs->[0]->options}); $col++) {
        my $option = $problem->inputs->[0]->options->[$col];
        next if ($option->value eq "DROP" or $option->value eq "SKIP" or $option->name eq "DROP" or $option->name eq "SKIP");
        my $name = $option->name;
        $ID_col = $col if ($name eq "ID");
        $TIME_col = $col if ($name eq "TIME");
        $AMT_col = $col if ($name eq "AMT");
        $RATE_col = $col if ($name eq "RATE");
        $SS_col = $col if ($name eq "SS");
        $II_col = $col if ($name eq "II");
    }

    if (not defined $ID_col or not defined $TIME_col) {
        return;
    }

    if (not -e $problem->datas->[0]->get_filename) {
        return;
    }

    my $data = data->new(filename => $problem->datas->[0]->get_filename, idcolumn => $ID_col, ignoresign => $problem->datas->[0]->ignoresign, parse_header => 0);

    my $id = $data->column_to_array(column => $ID_col);
    $id = [ map { int($_) } @$id ];
    my $time = $data->column_to_array(column => $TIME_col);

    my @column_id = ( "ID", "TIME" );
    my @column_type = ( "id", "time" );
    my @value_type = ( "string", "real" );
    my @columns = ( $id, $time );

    # have only AMT
    if (defined $AMT_col and not defined $RATE_col and not defined $SS_col and not defined $II_col) {
        my $amt = $data->column_to_array(column => $AMT_col);
        push @column_id, "AMT";
        push @column_type, "dose";
        push @value_type, "real";
        push @columns, $amt;
        foreach my $column (@columns) {     # Filter out rows with AMT = 0
            my @new_col = map { $amt->[$_] == 0 ? () : $column->[$_] } 0 .. scalar(@$amt) - 1;
            $column = \@new_col;
        }
        # filter out AMT = 0
    } else {
        return;
    }

    # Was everything filtered out?
    if (scalar(@{$columns[0]}) == 0) {
        return;
    }

    my $table = so::table->new(
        name => "Dosing",
        columnId => \@column_id,
        columnType => \@column_type,
        valueType => \@value_type,
        columns => \@columns,
        #table_file => $table_file,
    );

    return $table;
}

sub _add_status_messages
{
    # Add the PsN raw_results status as messages
    my $self = shift;
    my %parm = validated_hash(\@_,
        output => { isa => 'output' },
        problem => { isa => 'Int' },
        subproblem => { isa => 'Int' },
    );
    my $output = $parm{'output'};
    my $problem = $parm{'problem'};
    my $subproblem = $parm{'subproblem'};

    my $estimation_step_run = $output->problems->[$problem]->subproblems->[$subproblem]->estimation_step_run;

    if ($estimation_step_run) {
        my $minimization_successful = $output->minimization_successful->[$problem][$subproblem];
        $self->_so_block->TaskInformation->add_message(
            type => $minimization_successful ? "INFORMATION" : "WARNING",
            toolname => "NONMEM",
            name => "minimization_successful",
            content => $minimization_successful,
            severity => 1,
        );
    }

    my $covariance_step_run = $output->covariance_step_run->[$problem];

    $self->_so_block->TaskInformation->add_message(
        type => "INFORMATION",
        toolname => "NONMEM",
        name => "covariance_step_run",
        content => $covariance_step_run,
        severity => 1,
    );

    if ($covariance_step_run) {
        my $covariance_step_successful = 0;
        if ($output->covariance_step_successful->[$problem][$subproblem] ne '0') {
            $covariance_step_successful = 1;
        }

        $self->_so_block->TaskInformation->add_message(
            type => $covariance_step_successful ? "INFORMATION" : "WARNING",
            toolname => "NONMEM",
            name => "covariance_step_successful",
            content => $covariance_step_successful,
            severity => 1,
        );


        my $covariance_step_warnings = 0;
        if ($output->covariance_step_warnings->[$problem][$subproblem] ne '0') {
            $covariance_step_warnings = 1;
        }
        $self->_so_block->TaskInformation->add_message(
            type => $covariance_step_warnings ? "WARNING" : "INFORMATION",
            toolname => "NONMEM",
            name => "covariance_step_warnings",
            content => $covariance_step_warnings,
            severity => 1,
        );
    }

    my $rounding_errors = $output->rounding_errors->[$problem][$subproblem] eq '0' ? 0 : 1;
    $self->_so_block->TaskInformation->add_message(
        type => $rounding_errors ? "WARNING" : "INFORMATION",
        toolname => "NONMEM",
        name => "rounding_errors",
        content => $rounding_errors,
        severity => 1,
    );

    my $hessian_reset = $output->hessian_reset->[$problem][$subproblem];
    if (defined $hessian_reset) {
        $self->_so_block->TaskInformation->add_message(
            type => $hessian_reset eq '0' ? "INFORMATION" : "WARNING",
            toolname => "NONMEM",
            name => "hessian_reset",
            content => $hessian_reset,
            severity => 1,
        );
    }


    my $zero_gradients = $output->zero_gradients->[$problem][$subproblem];
    if (defined $zero_gradients) {
         $self->_so_block->TaskInformation->add_message(
            type => $zero_gradients eq '0' ? "INFORMATION" : "WARNING",
            toolname => "NONMEM",
            name => "zero_gradients",
            content => $zero_gradients,
            severity => 1,
        );
    }

    my $final_zero_gradients = $output->final_zero_gradients->[$problem][$subproblem];
    if (defined $final_zero_gradients) {
        $self->_so_block->TaskInformation->add_message(
            type => $final_zero_gradients eq '0' ? "INFORMATION" : "WARNING",
            toolname => "NONMEM",
            name => "final_zero_gradients",
            content => $final_zero_gradients,
            severity => 1,
        );
    }

    my $estimate_near_boundary = $output->estimate_near_boundary->[$problem][$subproblem];
    $self->_so_block->TaskInformation->add_message(
        type => $estimate_near_boundary ? "WARNING" : "INFORMATION",
        toolname => "NONMEM",
        name => "estimate_near_boundary",
        content => $estimate_near_boundary,
        severity => 1,
    );


    my $s_matrix_singular = $output->s_matrix_singular->[$problem][$subproblem];
    $self->_so_block->TaskInformation->add_message(
        type => $s_matrix_singular ? "WARNING" : "INFORMATION",
        toolname => "NONMEM",
        name => "s_matrix_singular",
        content => $s_matrix_singular,
        severity => 1,
    );

    my $significant_digits = $output->significant_digits->[$problem][$subproblem];
    if (defined $significant_digits) {
        $self->_so_block->TaskInformation->add_message(
            type => "INFORMATION",
            toolname => "NONMEM",
            name => "significant_digits",
            content => $significant_digits,
            severity => 1,
        );
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
