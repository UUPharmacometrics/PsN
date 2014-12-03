package standardised_output;

# Package for creation of a DDMoRe standardised output XML file

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::Writer;
use output;
use data;
use array;
use IO::File;

has 'lst_files' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'bootstrap_results' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'so_filename' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'precision' => ( is => 'rw', isa => 'Int', default => 4 );
has '_output' => ( is => 'rw', isa => 'output' );
has '_model' => ( is => 'rw', isa => 'model' );
has '_writer' => ( is => 'rw', isa => 'Ref' ); 
has '_block_number' => ( is => 'rw', isa => 'Int', default => 1 );

sub BUILD
{
    my $self = shift;

    my $so_filename;

    if (defined $self->so_filename) {   # User specified filename
        $so_filename = $self->so_filename;
    } else {
        if (defined $self->lst_files->[0]) {   # Infer filename from name of first .lst file
            $so_filename = $self->lst_files->[0];

            if ($so_filename =~ /(.*)\..*/) {
                $so_filename = $1 . '.SO.xml';
            } else {
                $so_filename .= '.SO.xml';
            }
        } else {
            $so_filename = 'bootstrap.SO.xml';
        }
    }

    my $output_file = IO::File->new(">" . $so_filename);

    my $writer = new XML::Writer(OUTPUT => $output_file, DATA_MODE => 1, DATA_INDENT => 2);
    $writer->xmlDecl("UTF-8");

    $self->_writer($writer);
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

sub add_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        column_ids => { isa => 'ArrayRef' },
        column_types => { isa => 'ArrayRef' },
        column_valuetypes => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
        row_major => { isa => 'Bool', default => 0 },
    );
    my $column_ids = $parm{'column_ids'};
    my $column_types = $parm{'column_types'};
    my $column_valuetypes = $parm{'column_valuetypes'};
    my $values = $parm{'values'};
    my $row_major = $parm{'row_major'};

    my $writer = $self->_writer;

    $writer->startTag("ds:Definition");
    for (my $col = 0; $col < scalar(@$column_ids); $col++) {
        $writer->emptyTag("ds:Column",
            columnId => $column_ids->[$col],
            columnType => $column_types->[$col],
            valueType => $column_valuetypes->[$col],
            columnNum => $col + 1,
        );
    }
    $writer->endTag("ds:Definition");

    $writer->startTag("ds:Table");
    my $numcols = scalar(@$column_ids);
    my $numrows;
    if ($row_major) {
        $numrows = scalar(@$values);
    } else {
        $numrows = scalar(@{$values->[0]});
    }
    for (my $row = 0; $row < $numrows; $row++) {
        $writer->startTag("ds:Row");
        for (my $col = 0; $col < $numcols; $col++) {
            my $value_type = uc(substr($column_valuetypes->[$col], 0, 1)) . substr($column_valuetypes->[$col], 1);
            my $element;
            if ($row_major) {
                $element = $values->[$row]->[$col];
            } else {
                $element = $values->[$col]->[$row];
            }
            if ($value_type eq 'String') {
                $writer->dataElement("ct:" . $value_type, $element);
            } else {
                $writer->dataElement("ct:" . $value_type, $self->_get_printable_number($element));
            }
        }
        $writer->endTag("ds:Row");
    }
    $writer->endTag("ds:Table");
}

sub add_single_row_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        labels => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my @labels = @{$parm{'labels'}};
    my @values = @{$parm{'values'}};

    $self->add_table(
        column_ids => \@labels,
        column_types => [ ("undefined") x scalar(@labels) ],
        column_valuetypes => [ ("real") x scalar(@labels) ],
        values => [ \@values ],
        row_major => 1,
    );
}

sub add_parameter_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
        labels => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my $name = $parm{'name'};
    my $labels = $parm{'labels'};
    my $values = $parm{'values'};

    $self->add_table(
        column_ids => [ "parameter", $name ],
        column_types => [ ("undefined") x 2 ],
        column_valuetypes => [ "string", "real" ],
        values => [ $labels, $values ], 
    );
}

sub add_matrix
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

    my $writer = $self->_writer;

    $writer->startTag("ct:Matrix", matrixType => "Any");

    $writer->startTag("ct:RowNames");
    foreach my $name (@$rownames) {
        $writer->startTag("ct:String");
        $writer->characters($name);
        $writer->endTag("ct:String");
    }
    $writer->endTag("ct:RowNames");

    $writer->startTag("ct:ColumnNames");
    foreach my $name (@$colnames) {
        $writer->startTag("ct:String");
        $writer->characters($name);
        $writer->endTag("ct:String");
    }
    $writer->endTag("ct:ColumnNames");

    for my $row (@$matrix) {
        $writer->startTag("ct:MatrixRow");
        for my $element (@$row) {
            $writer->dataElement("ct:Real", $self->_get_printable_number($element));
        }
        $writer->endTag("ct:MatrixRow");
    }
    $writer->endTag("ct:Matrix");
}

# End of XML helper methods

sub _start_block
{
    # Start a new SOBlock and set the id with a number
    my $self = shift;
    my $writer = $self->_writer;

    $writer->startTag("SOBlock", blkId => "SO" . $self->_block_number);
    $self->_block_number($self->_block_number + 1);
}

sub _end_block
{
    # End an SOBlock
    my $self = shift;
    my $writer = $self->_writer;
    $writer->endTag("SOBlock");
}

sub parse
{
    my $self = shift;

    my $writer = $self->_writer;

    $writer->startTag("SO",
        'xmlns' => "http://www.pharmml.org/2013/03/StandardisedOutput",
        'xmlns:xsi' => "http://www.w3.org/2001/XMLSchema-instance",
        'xmlns:ds' => "http://www.pharmml.org/2013/08/Dataset",
        'xmlns:ct' => "http://www.pharmml.org/2013/03/CommonTypes",
        'xsi:schemaLocation' => "http://www.pharmml.org/2013/03/StandardisedOutput",
        'implementedBy' => "MJS",
        'writtenVersion' => "0.1",
        'id' => "i1",
    );

    if (scalar(@{$self->lst_files}) > 0) {
        foreach my $file (@{$self->lst_files}) {
            $self->_parse_lst(filename => $file);
        }
    } else {
        $self->_start_block;
        $writer->startTag("Estimate");
        $self->_add_precision_population_estimates();  # For bootstrap
        $writer->endTag("Estimate");
        $self->_end_block;
    }

    $writer->endTag("SO");
    $writer->end();
}

sub _parse_lst
{
    # Parse one lst-file and put it into an SOBlock
    my $self = shift;
    my %parm = validated_hash(\@_,
        filename => { isa => 'Str' },
    );
    my $filename = $parm{'filename'};

    my $writer = $self->_writer;

    $self->_start_block();
    $writer->startTag("Estimation");

    # Check that the output file exist before trying to read it.
    if (not -e $filename) {
        $self->_add_target_tool_messages(error => "The file: " . $filename . " does not exist");
        $writer->endTag("Estimation");
        $self->_end_block();
        return;
    }

    my $outobj = output->new(filename => $filename);
    $self->_output($outobj);
    if (not $self->_output->parsed_successfully) {
        $self->_add_target_tool_messages(error => "Unable to read everything from outputfile, parser error message: " . $outobj->parsing_error_message);
        $writer->endTag("Estimation");
        $self->_end_block();
        return;
    }

    my $model = model->new(
        problems => $outobj->control_stream_problems,
        filename => 'dummy',
        is_dummy => 1,
        ignore_missing_data => 1,
        ignore_missing_files => 1,
        ignore_missing_output => 1
    );
    $self->_model($model);

	my $eta_shrinkage = $outobj->shrinkage_eta();
	my $eps_shrinkage = $outobj->shrinkage_eps();
	my $observation_records = $outobj->nobs();
	my $individuals = $outobj->nind();

	#arrays (over problems) of arrays (over subproblems) of arrays of values. Only non-zero are stored
	my $thetaref = $model -> get_values_to_labels(category => 'theta',
												  output_object => $outobj);
	my $omegaref = $model -> get_values_to_labels(category => 'omega',
												  output_object => $outobj);
	my $sigmaref = $model -> get_values_to_labels(category => 'sigma',
												  output_object => $outobj);

	#arrays (over problems) of arrays of names. 
	my $thetanamesref = $model->labels(parameter_type =>'theta', generic => 0);
	my $omeganamesref = $model->labels(parameter_type =>'omega', generic => 0);
	my $sigmanamesref = $model->labels(parameter_type =>'sigma', generic => 0);

	my $omegasameref = $model->same(parameter_type =>'omega');
	my $sigmasameref = $model->same(parameter_type =>'sigma');

	#arrays (over problems) of arrays (over subproblems) of arrays of values, one per name. Values may be undef
	my $sethetaref = $model -> get_values_to_labels(category => 'setheta',
													output_object => $outobj);
	my $seomegaref = $model -> get_values_to_labels(category => 'seomega',
													output_object => $outobj);
	my $sesigmaref = $model -> get_values_to_labels(category => 'sesigma',
													output_object => $outobj);

    my $problems = 0; #TODO check if first $PROB is prior, then should be =1 here, as in e.g. sse script
    my $sub_problems = 0;  #always 0 since we do not have workflow simulation + estimation?

	## Thetas

	my @thetas = defined $thetaref-> [$problems][$sub_problems] ? @{$thetaref -> [$problems][$sub_problems]} : ();
	my @thnam  = defined $thetanamesref -> [$problems] ? @{$thetanamesref -> [$problems]}  : ();
	my @sethet = defined $sethetaref -> [$problems][$sub_problems] ? @{$sethetaref -> [$problems][$sub_problems]} : ();

	my @etashrinkage = defined $eta_shrinkage -> [$problems][$sub_problems] ? @{$eta_shrinkage -> [$problems][$sub_problems]} : ();
	my @epsshrinkage = defined $eps_shrinkage -> [$problems][$sub_problems] ? @{$eps_shrinkage -> [$problems][$sub_problems]} : ();

	## Omegas
	my @omegas    = defined $omegaref -> [$problems][$sub_problems] ? @{$omegaref -> [$problems][$sub_problems]} : ();
	my @omnam     = defined $omeganamesref -> [$problems]? @{$omeganamesref -> [$problems]}  : ();
	my @seomeg    = defined $seomegaref -> [$problems][$sub_problems] ? @{$seomegaref -> [$problems][$sub_problems]} : ();
	my @omegasame = defined $omegasameref->[$problems] ? @{$omegasameref->[$problems]} : ();

	## Sigmas
	my @sigmas  = defined $sigmaref -> [$problems][$sub_problems] ? @{$sigmaref -> [$problems][$sub_problems]} : ();
	my @signam  = defined $sigmanamesref -> [$problems] ? @{$sigmanamesref -> [$problems]}                : ();
	my @sesigm  = defined $sesigmaref -> [$problems][$sub_problems] ? @{$sesigmaref -> [$problems][$sub_problems]} : ();
	my @sigmasame = defined $sigmasameref->[$problems] ? @{$sigmasameref->[$problems]} : ();

	## Termination
	my $ofv = $outobj -> get_single_value(attribute => 'ofv',
			       						  problem_index => $problems,
										  subproblem_index => $sub_problems);

    my $min_success = $outobj -> get_single_value(attribute => 'minimization_successful',
        problem_index =>$problems,
        subproblem_index => $sub_problems);

    my $have_ses = 0;
	if ($outobj->covariance_step_run->[$problems]) {
		if ($outobj->covariance_step_successful->[$problems][$sub_problems] ne '0') {
			$have_ses = 1;
		}
	}

    my @all_labels = (@thnam);
	my @est_values = (@thetas);

	my @se_values = ();
	my @rel_se = ();

	@se_values = (@sethet) if $have_ses;

	for (my $i = 0; $i < scalar(@omnam); $i++){
		unless ($omegasame[$i]) {
			push(@all_labels,$omnam[$i]);
			push(@est_values,$omegas[$i]);
			push(@se_values,$seomeg[$i]) if $have_ses;
		}
	}
	for (my $i = 0; $i < scalar(@signam); $i++){
		unless ($sigmasame[$i]) {
			push(@all_labels,$signam[$i]);
			push(@est_values,$sigmas[$i]);
			push(@se_values,$sesigm[$i]) if $have_ses;
		}
	}

	#Calculate relative standard errors, only if have se values
	for (my $i = 0; $i < scalar(@se_values); $i++) {
		if ($est_values[$i] == 0) {
			push @rel_se, undef;
		} else {
			push @rel_se, $se_values[$i] / $est_values[$i];
		}
	}

	my $minimization_message = $outobj -> get_single_value(attribute => 'minimization_message',
			       						  problem_index => $problems,
										  subproblem_index => $sub_problems);

    my $undefs = grep { not defined $_ } @est_values;
    if ($undefs != scalar(@est_values)) {   # Check that not all in list are undef. Should possibly have been done earlier
        $self->_add_population_estimates(labels => \@all_labels, values => \@est_values);
    }

	if (scalar(@se_values) > 0) {
        my $correlation_matrix = linear_algebra::triangular_symmetric_to_full($outobj->correlation_matrix->[$problems]->[$sub_problems]);
        my $covariance_matrix = linear_algebra::triangular_symmetric_to_full($outobj->covariance_matrix->[$problems]->[$sub_problems]);
		$self->_add_precision_population_estimates(
            labels => \@all_labels,
            standard_errors => \@se_values,
            relative_standard_errors => \@rel_se,
            correlation_matrix => $correlation_matrix,
            covariance_matrix => $covariance_matrix,
        );
	}

    # Loop through the different tables
	my ($table_name_ref, $dummy) = $self->_model->problems->[$problems]->_option_val_pos(record_name => 'table', name => 'FILE');
	if (defined $table_name_ref and scalar @{$table_name_ref} >= 0) {
        foreach my $table (@$table_name_ref) {
            if ($table =~ /^sdtab/) {
                my $sdtab = data->new(
                    directory => $self->_model->directory,
                    filename => $table,
                    ignoresign => '@',
                    parse_header => 1,
                );
                $self->_add_predictions(sdtab => $sdtab);
                $self->_add_residuals(sdtab => $sdtab);
            }
            if ($table =~ /^patab/) {
                my $patab = data->new(
                    directory => $self->_model->directory,
                    filename => $table,
                    ignoresign => '@',
                    parse_header => 1,
                );
                $self->_add_individual_estimates(patab => $patab);
            }
        }
    }

    $self->_add_likelihood(ofv => $ofv);

    if (defined $ofv) {
        $self->_add_target_tool_messages;
    } else {
        $self->_add_target_tool_messages(error => join('', @{$minimization_message}));
    }

    $writer->endTag("Estimation");
    $self->_end_block;
}

sub _get_eta_names
{
    # Gets the names of the ETAs from a list in the code
    # ETA_CL = ETA(1)

    my $self = shift;
    my @names;

   	my @code;
	if ($self->_model->has_code(record => 'pk')) {
		@code = @{$self->_model->get_code(record => 'pk')};
	} else {
		@code = @{$self->_model->get_code(record => 'pred')};
	}

    foreach my $line (@code) {
        if ($line =~ /^\s*(\w+)\s*=\s*ETA\(\d+\)/) {
            push @names, $1;
        }
    }

    return \@names;
}

sub _add_population_estimates
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        labels => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my @labels = @{$parm{'labels'}};
    my @values = @{$parm{'values'}};

    my $writer = $self->_writer;

    $writer->startTag("PopulationEstimates");
    $writer->startTag("MLE");

    $self->add_single_row_table(labels => \@labels, values => \@values);

    $writer->endTag("MLE");
    $writer->endTag("PopulationEstimates");
}

sub _add_precision_population_estimates
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

    my $writer = $self->_writer;

    $writer->startTag("PrecisionPopulationEstimates");

    if (scalar(@{$self->lst_files}) > 0) {
        $writer->startTag("MLE");
        $writer->startTag("StandardError");
        $self->add_parameter_table(name => 'SE', labels => \@labels, values => \@standard_errors);
        $writer->endTag("StandardError");

        $writer->startTag("RelativeStandardError");
        $self->add_parameter_table(name => 'RSE', labels => \@labels, values => \@relative_standard_errors);
        $writer->endTag("RelativeStandardError");

        $writer->startTag("CorrelationMatrix");
        $self->add_matrix(rownames => \@labels, colnames => \@labels, matrix => $correlation_matrix);
        $writer->endTag("CorrelationMatrix");

        $writer->startTag("CovarianceMatrix");
        $self->add_matrix(rownames => \@labels, colnames => \@labels, matrix => $covariance_matrix);
        $writer->endTag("CovarianceMatrix");

        $writer->endTag("MLE");
    }

    if (defined $self->bootstrap_results) {
        $self->_add_bootstrap();
    }

    $writer->endTag("PrecisionPopulationEstimates");
}

sub _add_bootstrap
{
    my $self = shift;

    my $writer = $self->_writer;

    $writer->startTag("Bootstrap");
    $writer->startTag("PercentilesCI");

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

    $self->add_table(
        column_ids => \@percentiles,
        column_types => [ ('undefined') x scalar(@percentiles) ],
        column_valuetypes => [ 'string', ('real') x (scalar(@percentiles) - 1) ],
        values => \@column,
        row_major => 1,
    );

    $writer->endTag("PercentilesCI");    
    $writer->endTag("Bootstrap");
}

sub _add_target_tool_messages
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        error => { isa => 'Str', optional => 1 },
    );
    my $error = $parm{'error'};

    my $writer = $self->_writer;

    $writer->startTag("TargetToolMessages");

    if (defined $error) {
        $writer->dataElement("Errors", $error);
    } else {
        #$writer->dataElement("Termination", '');
        #$writer->dataElement("Warnings", '');
        #$writer->dataElement("Errors", '');

        $self->_output->runtime =~ m/(\d+):(\d+):(\d+)/;
        my $seconds = $1 * 3600 + $2 * 60 + $3;
        $writer->dataElement("ElapsedTime", $seconds);
    }

    $writer->endTag("TargetToolMessages");
}

sub _add_likelihood
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        ofv => { isa => 'Maybe[Num]' },
    );
    my $ofv = $parm{'ofv'};

    my $writer = $self->_writer;

    if (defined $ofv) {
        $writer->startTag("Likelihood");
        $writer->dataElement("Deviance", $ofv);
        $writer->endTag("Likelihood");
    }
}

sub _add_predictions
{
     my $self = shift;
     my %parm = validated_hash(\@_,
        sdtab => { isa => 'data' },
    );
    my $sdtab = $parm{'sdtab'};

    my $writer = $self->_writer;

    if (not (exists $sdtab->column_head_indices->{'ID'} and exists $sdtab->column_head_indices->{'TIME'}
        and exists $sdtab->column_head_indices->{'PRED'} and exists $sdtab->column_head_indices->{'IPRED'})) {
        return;
    }

    my $id = $sdtab->column_to_array(column => "ID");
    my $time = $sdtab->column_to_array(column => "TIME");
    my $pred = $sdtab->column_to_array(column => "PRED");
    my $ipred = $sdtab->column_to_array(column => "IPRED");

    $writer->startTag("Predictions");

    $self->add_table(
        column_ids => [ "ID", "TIME", "PRED", "IPRED" ],
        column_types => [ "id", "undefined", "undefined", "undefined" ],
        column_valuetypes =>  [ "id", "real", "real", "real" ],
        values => [ $id, $time, $pred, $ipred ],
    );

    $writer->endTag("Predictions");
}

sub _add_residuals
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        sdtab => { isa => 'data' },
    );
    my $sdtab = $parm{'sdtab'};

    my $writer = $self->_writer;

    if (not (exists $sdtab->column_head_indices->{'ID'} and exists $sdtab->column_head_indices->{'TIME'})) {
        return;
    }

    my $id = $sdtab->column_to_array(column => "ID");
    my $time = $sdtab->column_to_array(column => "TIME");

    $writer->startTag("Residuals");

    if (exists $sdtab->column_head_indices->{'RES'}) {
        my $res = $sdtab->column_to_array(column => "RES");
        $writer->startTag("RES");
        $self->add_table(
            column_ids => [ "ID", "TIME", "RES" ],
            column_types => [ "id", "undefined", "undefined" ],
            column_valuetypes =>  [ "id", "real", "real" ],
            values => [ $id, $time, $res ],
        );
        $writer->endTag("RES");
    }

    if (exists $sdtab->column_head_indices->{'IRES'}) {
        my $ires = $sdtab->column_to_array(column => "IRES");
        $writer->startTag("IRES");
        $self->add_table(
            column_ids => [ "ID", "TIME", "IRES" ],
            column_types => [ "id", "undefined", "undefined" ],
            column_valuetypes =>  [ "id", "real", "real" ],
            values => [ $id, $time, $ires ],
        );
        $writer->endTag("IRES");
    }

    if (exists $sdtab->column_head_indices->{'WRES'}) {
        my $wres = $sdtab->column_to_array(column => "WRES");
        $writer->startTag("WRES");
        $self->add_table(
            column_ids => [ "ID", "TIME", "WRES" ],
            column_types => [ "id", "undefined", "undefined" ],
            column_valuetypes =>  [ "id", "real", "real" ],
            values => [ $id, $time, $wres ],
        );
        $writer->endTag("WRES");
    }

    if (exists $sdtab->column_head_indices->{'IWRES'}) {
        my $iwres = $sdtab->column_to_array(column => "IWRES");
        $writer->startTag("IWRES");
        $self->add_table(
            column_ids => [ "ID", "TIME", "IWRES" ],
            column_types => [ "id", "undefined", "undefined" ],
            column_valuetypes =>  [ "id", "real", "real" ],
            values => [ $id, $time, $iwres ],
        );
        $writer->endTag("IWRES");
    }

    $writer->endTag("Residuals");
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

sub _add_individual_estimates
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        patab => { isa => 'data' },
    );
    my $patab = $parm{'patab'};

    my $writer = $self->_writer;
    my $eta_names = $self->_get_eta_names;
    if (not exists $patab->column_head_indices->{'ID'}) {
        return;
    }
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

    $writer->startTag("IndividualEstimates");
    $writer->startTag("Estimates");

    $writer->startTag("Median");
    $self->add_table(
        column_ids => [ "ID", @labels ],
        column_types => [ "id", ("undefined") x scalar(@labels) ],
        column_valuetypes => [ "id", ("real") x scalar(@labels) ],
        values => [ $unique_ids, @medians ],
    );
    $writer->endTag("Median");

    $writer->startTag("Mean");
    $self->add_table(
        column_ids => [ "ID", @labels ],
        column_types => [ "id", ("undefined") x scalar(@labels) ],
        column_valuetypes => [ "id", ("real") x scalar(@labels) ],
        values => [ $unique_ids, @means ],
    );
    $writer->endTag("Mean");

    $writer->endTag("Estimates");

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

        $writer->startTag("RandomEffects");

        $writer->startTag("EffectMedian");
        $self->add_table(
            column_ids => [ "ID", @$eta_names ],
            column_types => [ "id", ("undefined") x scalar(@$eta_names) ],
            column_valuetypes => [ "id", ("real") x scalar(@$eta_names) ],
            values => [ $unique_ids, @eta_medians ],
        );
        $writer->endTag("EffectMedian");

        $writer->startTag("EffectMean");
        $self->add_table(
            column_ids => [ "ID", @$eta_names ],
            column_types => [ "id", ("undefined") x scalar(@$eta_names) ],
            column_valuetypes => [ "id", ("real") x scalar(@$eta_names) ],
            values => [ $unique_ids, @eta_means ],
        );
        $writer->endTag("EffectMean");

        $writer->endTag("RandomEffects");
    }
    $writer->endTag("IndividualEstimates");
}

1;
