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
use OSspecific;

has 'lst_files' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'bootstrap_results' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'so_filename' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'precision' => ( is => 'rw', isa => 'Int', default => 10 );
has 'use_tables' => ( is => 'rw', isa => 'Bool', default => 1 );    # Set to zero if sdtab and patab should not be used
has 'exclude_elements' => ( is => 'rw', isa => 'Maybe[ArrayRef]' );
has 'only_include_elements' => ( is => 'rw', isa => 'Maybe[ArrayRef]' );
has '_document' => ( is => 'rw', isa => 'Ref' );    # The XML document 
has '_duplicate_blocknames' => ( is => 'rw', isa => 'HashRef' );    # Contains those blocknames which will have duplicates with next number for block
has '_first_block' => ( is => 'rw', isa => 'Str' );

sub BUILD
{
    my $self = shift;

    my $so_filename;

    if (not defined $self->so_filename) {   # User specified filename
        if (defined $self->lst_files and defined $self->lst_files->[0]) {   # Infer filename from name of first .lst file
            $so_filename = $self->lst_files->[0];

            if ($so_filename =~ /(.*)\..*/) {
                $so_filename = $1 . '.SO.xml';
            } else {
                $so_filename .= '.SO.xml';
            }
        } else {
            $so_filename = 'bootstrap.SO.xml';
        }
        $self->so_filename($so_filename);
    }

    my $doc = XML::LibXML::Document->new('1.0', 'utf-8');
    $self->_document($doc);
}

# Plain XML helper methods
# Uses $self->_writer and $self->precision

sub _get_printable_number
{
    # Return a number with the correct precision as a string for output
    my $self = shift;
    my $number = shift;

    my $form = '%.' . $self->precision . 'g';
    my $str = sprintf($form, $number);

    return $str;
}

sub create_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        table_name => { isa => 'Str' },
        column_ids => { isa => 'ArrayRef' },
        column_types => { isa => 'ArrayRef' },
        column_valuetypes => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
        row_major => { isa => 'Bool', default => 0 },
    );
    my $table_name = $parm{'table_name'};
    my $column_ids = $parm{'column_ids'};
    my $column_types = $parm{'column_types'};
    my $column_valuetypes = $parm{'column_valuetypes'};
    my $values = $parm{'values'};
    my $row_major = $parm{'row_major'};

    my $doc = $self->_document;

    my $table = $doc->createElement($table_name);

    my $def = $doc->createElement("ds:Definition");
    $table->appendChild($def);
    for (my $col = 0; $col < scalar(@$column_ids); $col++) {
        my $column = $doc->createElement("ds:Column");
        $column->setAttribute(columnId => $column_ids->[$col]);
        $column->setAttribute(columnType => $column_types->[$col]);
        $column->setAttribute(valueType => $column_valuetypes->[$col]);
        $column->setAttribute(columnNum => $col + 1);
        $def->appendChild($column);
    }

    my $tab = $doc->createElement("ds:Table");
    $table->appendChild($tab);
    my $numcols = scalar(@$column_ids);
    my $numrows;
    if ($row_major) {
        $numrows = scalar(@$values);
    } else {
        $numrows = scalar(@{$values->[0]});
    }
    for (my $row = 0; $row < $numrows; $row++) {
        my $row_xml = $doc->createElement("ds:Row");
        $tab->appendChild($row_xml);
        for (my $col = 0; $col < $numcols; $col++) {
            my $value_type = uc(substr($column_valuetypes->[$col], 0, 1)) . substr($column_valuetypes->[$col], 1);
            my $column_type = $column_types->[$col];
            my $element;
            if ($row_major) {
                $element = $values->[$row]->[$col];
            } else {
                $element = $values->[$col]->[$row];
            }
            my $value = $doc->createElement("ct:" . $value_type);
            if ($value_type eq 'String' and $column_type ne 'id') {
                $value->appendTextNode($element);
            } else {
                $value->appendTextNode($self->_get_printable_number($element));
            }
            $row_xml->appendChild($value);
        }
    }

    return $table;
}

sub create_single_row_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        table_name => { isa => 'Str' },
        labels => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my $table_name = $parm{'table_name'};
    my @labels = @{$parm{'labels'}};
    my @values = @{$parm{'values'}};

    my $table = $self->create_table(
        table_name => $table_name,
        column_ids => \@labels,
        column_types => [ ("undefined") x scalar(@labels) ],
        column_valuetypes => [ ("real") x scalar(@labels) ],
        values => [ \@values ],
        row_major => 1,
    );

    return $table;
}

sub create_parameter_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        table_name => { isa => 'Str' },
        name => { isa => 'Str' },
        labels => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my $table_name = $parm{'table_name'};
    my $name = $parm{'name'};
    my $labels = $parm{'labels'};
    my $values = $parm{'values'};

    my $table = $self->create_table(
        table_name => $table_name,
        column_ids => [ "parameter", $name ],
        column_types => [ ("undefined") x 2 ],
        column_valuetypes => [ "string", "real" ],
        values => [ $labels, $values ], 
    );

    return $table;
}

sub create_matrix
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        rownames => { isa => 'ArrayRef' },
        colnames => { isa => 'ArrayRef' },
        matrix => { isa => 'ArrayRef[ArrayRef]' },
    );
    my $rownames = $parm{'rownames'};
    my $colnames = $parm{'colnames'};
    my $matrix = $parm{'matrix'};

    my $doc = $self->_document;

    my $matrix_xml = $doc->createElement("ct:Matrix");
    $matrix_xml->setAttribute(matrixType => "Any");

    my $rownames_xml = $doc->createElement("ct:RowNames");
    $matrix_xml->appendChild($rownames_xml);
    foreach my $name (@$rownames) {
        my $string = $doc->createElement("ct:String");
        $string->appendTextNode($name);
        $rownames_xml->appendChild($string);
    }

    my $colnames_xml = $doc->createElement("ct:ColumnNames");
    $matrix_xml->appendChild($colnames_xml);
    foreach my $name (@$colnames) {
        my $string = $doc->createElement("ct:String");
        $string->appendTextNode($name);
        $colnames_xml->appendChild($string);
    }

    for my $row (@$matrix) {
        my $matrix_row = $doc->createElement("ct:MatrixRow");
        $matrix_xml->appendChild($matrix_row);
        for my $element (@$row) {
            my $real = $doc->createElement("ct:Real");
            $real->appendTextNode($self->_get_printable_number($element));
            $matrix_row->appendChild($real);
        }
    }

    return $matrix_xml;
}

sub find_or_create_node
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        root_node => { isa => 'Ref' },
        node_name => { isa => 'Str' },      # Can be a one level XPath
    );
    my $root_node = $parm{'root_node'};
    my $node_name = $parm{'node_name'};

    my $doc = $self->_document;
    my $node;
    if (not $root_node->exists($node_name)) {
        $node = $doc->createElement($node_name);
        $root_node->appendChild($node);
    } else {
        ($node) = $root_node->findnodes($node_name);
    }

    return $node;
}

sub append_array
{
    my $self = shift;
    my $node = shift;
    my $array = shift;

    if (defined $array) {
        foreach my $a (@$array) {
            $node->appendChild($a);
        }
    }
}

sub IsStarting_character
{
    # Character class for the XML character class [\i-[:]] (first character in NCName)
    # Adapted from http://www.regular-expressions.info/shorthand.html#xml
    return <<END;
0041 005A
005F
0061 007A
00C0 00D6
00D8 00F6
00F8 02FF
0370 037D
037F 1FFF
200C 200D
2070 218F
2C00 2FEF
3001 D7FF
F900 FDCF
FDF0 FFFD
END
}

sub IsContinuing_character
{
    # Character class for the XML character class [\c-[:]] (following characters in NCName)
    # Adapted from http://www.regular-expressions.info/shorthand.html#xml
    return <<END;
002D
002E
0030 0039
0041 005A
005F
0061 007A
00B7
00C0 00D6
00D8 00F6
00F8 037D
037F 1FFF
200C 200D
203F
2040
2070 218F
2C00 2FEF
3001 D7FF
F900 FDCF
FDF0 FFFD
END
}

sub match_symbol_idtype
{
    # Check if a string is a legal SymbolIdType
    my $symbol = shift;
    
    if ($symbol =~ /^\p{IsStarting_character}\p{IsContinuing_character}*$/) {
        return 1;
    } else {
        return 0;
    }
}

sub mangle_symbol_idtype
{
    # Mangle a SymbolIdType by replacing all illegal characters with underscore
    my $symbol = shift;

    $symbol =~ s/^\P{IsStarting_character}/_/;
    $symbol =~ s/\P{IsContinuing_character}/_/g;

    return $symbol;
}

# End of XML helper methods

sub get_file_stem
{
    my $name = shift;

    $name = OSspecific::nopath($name); 
    $name =~ s/(.*)\..*/\1/;

    return $name;
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
    $SO->setAttribute('xmlns' => "http://www.pharmml.org/2013/03/StandardisedOutput");
    $SO->setAttribute('xmlns:xsi' => "http://www.w3.org/2001/XMLSchema-instance");
    $SO->setAttribute('xmlns:ds' => "http://www.pharmml.org/2013/08/Dataset");
    $SO->setAttribute('xmlns:ct' => "http://www.pharmml.org/2013/03/CommonTypes");
    $SO->setAttribute('xsi:schemaLocation' => "http://www.pharmml.org/2013/03/StandardisedOutput");
    $SO->setAttribute('implementedBy' => "MJS");
    $SO->setAttribute('writtenVersion' => "0.1");
    $SO->setAttribute('id' => "i1");

    # Check for duplicate lst_file names
    my %duplicates;
    if (defined $self->lst_files) {
        foreach my $file (@{$self->lst_files}) {
            my $stem = get_file_stem($file);
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
        my $first_block = $self->_first_block;
        (my $block) = $SO->findnodes("SOBlock[\@blkId='$first_block']");
        if (not defined $block) {
            $block = $self->create_block(name => "Bootstrap");
            $SO->appendChild($block);
        }
        my $estimation = $self->find_or_create_node(root_node => $block, node_name => "Estimation");

        # Create Bootstrap element
        if (-e $self->bootstrap_results) {
            my $ppi = $self->find_or_create_node(root_node => $estimation, node_name => "PrecisionPopulationEstimates");
            my $bootstrap = $self->_create_bootstrap();
            $ppi->appendChild($bootstrap);
        } else {
            my $bootstrap_error = {
                type => "ERROR",
                toolname => "nmoutput2so",
                name => "File error",
                content => "Bootstrap results file \"" . $self->bootstrap_results . "\" does not exist",
                severity => 10,
            };
            my $messages = $self->_create_messages(messages => [ $bootstrap_error ]);
            $self->append_array($block, $messages);
        }
    }

    $doc->setDocumentElement($SO);
    $doc->toFile($self->so_filename, 1);
}

sub _parse_lst_file
{
    # Parse one lst-file and put it into an SOBlock
    my $self = shift;
    my %parm = validated_hash(\@_,
        lst_file => { isa => 'Str' },
    );
    my $lst_file = $parm{'lst_file'};

    my $elapsed_time = 0;
    my @messages;
    my $doc = $self->_document;

    my $file_stem = get_file_stem($lst_file);
    if (not defined $self->_first_block) {
        $self->_first_block($file_stem);
    }

    my $block = $self->create_block(name => $file_stem);
    my $estimation = $doc->createElement("Estimation");
    $block->appendChild($estimation);

    # Check that the output file exist before trying to read it.
    if (not -e $lst_file) {
        push @messages, {
            type => "ERROR",
            toolname => "nmoutput2so",
            name => "File error",
            content => "The file: \"" . $lst_file . "\" does not exist",
            severity => 10,
        };
    } else {
        my $outobj = output->new(filename => $lst_file);
        if (not $outobj->parsed_successfully) {
            push @messages, {
                type => "ERROR",
                toolname => "NONMEM",
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

            my @est_values = @{$outobj->get_filtered_values(
                problem_index => $problems,
                subproblem_index => $sub_problems,
                parameter => 'all',
                category => 'estimate'
            )};

            my @se_values = @{$outobj->get_filtered_values(
                problem_index => $problems,
                subproblem_index => $sub_problems,
                parameter => 'all',
                category => 'se'
            )};

            my @all_labels = @{$model->problems->[$problems]->get_estimated_attributes(parameter => 'all', attribute => 'labels')};

            foreach my $label (@all_labels) {
                if (not match_symbol_idtype($label)) {
                    my $old_label = $label;
                    $label = mangle_symbol_idtype($label);
                    push @messages, {
                        type => "WARNING",
                        toolname => "nmoutput2so",
                        name => "Name change",
                        content => "Parameter label \"$old_label\" not specified or not a legal symbolIdType. Setting/changing it to: $label",
                        severity => 1,
                    };
                }
            }

            #Calculate relative standard errors, only if have se values
            my @rel_se = ();
            if ($covariance_step_successful) {
                for (my $i = 0; $i < scalar(@se_values); $i++) {
                    if ($est_values[$i] == 0) {
                        push @rel_se, undef;
                    } else {
                        push @rel_se, $se_values[$i] / $est_values[$i];
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
                if ($self->check_include(element => 'Estimation/PrecisionPopulationEstimates')) {
                    my $ppe = $self->_create_precision_population_estimates(
                        labels => \@all_labels,
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
                        if ($table =~ /^(sdtab|patab)/ and not -e $table) {
                            push @messages, {
                                type => "WARNING",
                                toolname => "nmoutput2so",
                                name => "File error",
                                content => "Could not find table $table. Results from this table could not be added.",
                                severity => 1,
                            };
                            next;
                        }
                        if ($table =~ /^sdtab/) {
                            my $sdtab = data->new(
                                directory => $outobj->directory,
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
                                directory => $outobj->directory,
                                filename => $table,
                                ignoresign => '@',
                                parse_header => 1,
                            );
                            if ($self->check_include(element => 'Estimation/IndividualEstimates')) {
                                my $individual_estimates = $self->_create_individual_estimates(patab => $patab, model => $model);
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
                    toolname => "NONMEM",
                    name => "Minimzation error",
                    content => join('', @{$minimization_message}),
                    severity => 5,
                };
            }

            $outobj->runtime =~ m/(\d+):(\d+):(\d+)/;
            $elapsed_time = $1 * 3600 + $2 * 60 + $3;
        }
    }

    my $xml_messages = $self->_create_messages(messages => \@messages);
    $self->append_array($block, $xml_messages);

    return $block;
}

sub _get_eta_names
{
    # Gets the names of the ETAs from a list in the code
    # ETA_CL = ETA(1)

    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my @names;
   	my @code;
	if ($model->has_code(record => 'pk')) {
		@code = @{$model->get_code(record => 'pk')};
	} else {
		@code = @{$model->get_code(record => 'pred')};
	}

    foreach my $line (@code) {
        if ($line =~ /^\s*(\w+)\s*=\s*ETA\(\d+\)/) {
            push @names, $1;
        }
    }

    return \@names;
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

    my $table = $self->create_single_row_table(table_name => 'MLE', labels => \@labels, values => \@values);
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
            my $table = $self->create_parameter_table(table_name => 'StandardError', name => 'SE', labels => \@labels, values => \@standard_errors);
            $mle->appendChild($table);
        }
        if ($self->check_include(element => 'Estimation/PrecisionPopulationEstimates/MLE/RelativeStandardError')) {
            my $table = $self->create_parameter_table(table_name => 'RelativeStandardError', name => 'RSE', labels => \@labels,
                values => \@relative_standard_errors);
            $mle->appendChild($table);
        }

        if ($self->check_include(element => 'Estimation/PrecisionPopulationEstimates/MLE/CorrelationMatrix')) {
            my $cor = $doc->createElement("CorrelationMatrix");
            $mle->appendChild($cor);
            my $matrix = $self->create_matrix(rownames => \@labels, colnames => \@labels, matrix => $correlation_matrix);
            $cor->appendChild($matrix);
        }

        if ($self->check_include(element => 'Estimation/PrecisionPopulationEstimates/MLE/CovarianceMatrix')) {
            my $cov = $doc->createElement("CovarianceMatrix");
            $mle->appendChild($cov);
            my $matrix = $self->create_matrix(rownames => \@labels, colnames => \@labels, matrix => $covariance_matrix);
            $cov->appendChild($matrix);
        }
    }

    return $ppe;
}

sub _create_bootstrap
{
    my $self = shift;

    my $doc = $self->_document;

    my $bootstrap = $doc->createElement("Bootstrap");

    open my $fh, '<', $self->bootstrap_results;

    my @parameters;
    my @percentiles;
    my @column;

    while (<$fh>) {
        if (/^percentile.confidence.intervals$/) {
            my $header = <$fh>;
            my @a = split /,/, $header;
            shift @a;
            shift @a;
            foreach my $param (@a) {
                $param =~ s/"\s*(.*)"/\1/;      # Remove "" and spaces
                if ($param !~ /^se/) {
                    push @parameters, $param;
                } else {
                    last;
                }
            }
            # Add parameter names to table
            for (my $col = 0; $col < scalar(@parameters); $col++) {
                push @{$column[$col]}, $parameters[$col];
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

    # Create column Ids
    foreach my $p (@percentiles) {
        $p = "Pctl_${p}th";
    }
    unshift @percentiles, 'parameter';

    my $table = $self->create_table(
        table_name => 'PercentilesCI',
        column_ids => \@percentiles,
        column_types => [ ('undefined') x scalar(@percentiles) ],
        column_valuetypes => [ 'string', ('real') x (scalar(@percentiles) - 1) ],
        values => \@column,
        row_major => 1,
    );
    $bootstrap->appendChild($table);

    return $bootstrap;
}

sub _create_messages
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
    );
    my @messages = defined $parm{'messages'} ? @{$parm{'messages'}} : ();

    my $doc = $self->_document;

    my @xml_messages;

    foreach my $message (@messages) {
        my $node = $doc->createElement("Message");
        $node->setAttribute('type', $message->{'type'});
        my $toolname = $doc->createElement('toolname');
        $toolname->appendTextNode($message->{'toolname'});
        $node->appendChild($toolname);
        my $name = $doc->createElement('name');
        $name->appendTextNode($message->{'name'});
        $node->appendChild($name);
        my $content = $doc->createElement('content');
        $content->appendTextNode($message->{'content'});
        $node->appendChild($content);
        my $severity = $doc->createElement('severity');
        $severity->appendTextNode($message->{'severity'});
        $node->appendChild($severity);
        push @xml_messages, $node;
    }

    return \@xml_messages;
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

    my $doc = $self->_document;

    my $id = $sdtab->column_to_array(column => "ID");
    my $time = $sdtab->column_to_array(column => "TIME");
    my $pred = $sdtab->column_to_array(column => "PRED");
    my $ipred = $sdtab->column_to_array(column => "IPRED");

    my $predictions = $self->create_table(
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

    my $residuals = $doc->createElement("Residuals");

    if ($self->check_include(element => 'Estimation/Residuals/RES')) {
        if (exists $sdtab->column_head_indices->{'RES'}) {
            my $res = $sdtab->column_to_array(column => "RES");
            my $table = $self->create_table(
                table_name => "RES",
                column_ids => [ "ID", "TIME", "RES" ],
                column_types => [ "id", "undefined", "undefined" ],
                column_valuetypes =>  [ "string", "real", "real" ],
                values => [ $id, $time, $res ],
            );
            $residuals->appendChild($table);
        }
    }

    if ($self->check_include(element => 'Estimation/Residuals/IRES')) {
        if (exists $sdtab->column_head_indices->{'IRES'}) {
            my $ires = $sdtab->column_to_array(column => "IRES");
            my $table = $self->create_table(
                table_name => "IRES",
                column_ids => [ "ID", "TIME", "IRES" ],
                column_types => [ "id", "undefined", "undefined" ],
                column_valuetypes =>  [ "string", "real", "real" ],
                values => [ $id, $time, $ires ],
            );
            $residuals->appendChild($table);
        }
    }

    if ($self->check_include(element => 'Estimation/Residuals/WRES')) {
        if (exists $sdtab->column_head_indices->{'WRES'}) {
            my $wres = $sdtab->column_to_array(column => "WRES");
            my $table = $self->create_table(
                table_name => "WRES",
                column_ids => [ "ID", "TIME", "WRES" ],
                column_types => [ "id", "undefined", "undefined" ],
                column_valuetypes =>  [ "string", "real", "real" ],
                values => [ $id, $time, $wres ],
            );
            $residuals->appendChild($table);
        }
    }

    if ($self->check_include(element => 'Estimation/Residuals/IWRES')) {
        if (exists $sdtab->column_head_indices->{'IWRES'}) {
            my $iwres = $sdtab->column_to_array(column => "IWRES");
            my $table = $self->create_table(
                table_name => "IWRES",
                column_ids => [ "ID", "TIME", "IWRES" ],
                column_types => [ "id", "undefined", "undefined" ],
                column_valuetypes =>  [ "string", "real", "real" ],
                values => [ $id, $time, $iwres ],
            );
            $residuals->appendChild($table);
        }
    }

    return $residuals;
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
    );
    my $patab = $parm{'patab'};
    my $model = $parm{'model'};

    my $eta_names = $self->_get_eta_names(model => $model);
    if (not exists $patab->column_head_indices->{'ID'}) {
        return;
    }

    my $doc = $self->_document;
    my $id = $patab->column_to_array(column => "ID");

    my @labels = ();
    foreach my $index (keys %{$patab->column_head_indices}) {
        my $is_eta = grep(/^$index$/, @$eta_names);
        if ($index ne 'ID' and not $is_eta) {
            $labels[$patab->column_head_indices->{$index} - 2] = $index;
        }
    }

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
            my $table = $self->create_table(
                table_name => "Median",
                column_ids => [ "ID", @labels ],
                column_types => [ "id", ("undefined") x scalar(@labels) ],
                column_valuetypes => [ "string", ("real") x scalar(@labels) ],
                values => [ $unique_ids, @medians ],
            );
            $estimates->appendChild($table);
        }
        if ($self->check_include(element => 'Estimation/IndividualEstimates/Estimates/Mean')) {
            my $table = $self->create_table(
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
        my $temp_etas = [];
        foreach my $eta (@$eta_names) {
            if (exists $patab->column_head_indices->{$eta}) {
                push @$temp_etas, $eta;
            }
        }
        $eta_names = $temp_etas;

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
                my $table = $self->create_table(
                    table_name => "EffectMedian",
                    column_ids => [ "ID", @$eta_names ],
                    column_types => [ "id", ("undefined") x scalar(@$eta_names) ],
                    column_valuetypes => [ "string", ("real") x scalar(@$eta_names) ],
                    values => [ $unique_ids, @eta_medians ],
                );
                $random_effects->appendChild($table);
            }

            if ($self->check_include(element => 'Estimation/IndividualEstimates/RandomEffects/EffectMean')) {
                my $table = $self->create_table(
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

    return $individual_estimates;
}

1;
