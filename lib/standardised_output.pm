package standardised_output;

# Package for creation of a DDMoRe standardised output XML file

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::Writer;
use output;

has 'output' => ( is => 'rw', isa => 'output' );
has 'model' => ( is => 'rw', isa => 'model' );
has '_writer' => ( is => 'rw', isa => 'Ref' ); 

sub BUILD
{
    my $self = shift;

    my $writer = new XML::Writer(DATA_MODE => 1, DATA_INDENT => 2);  # will write to stdout
    $writer->xmlDecl("UTF-8");


    $self->_writer($writer);
}

# Plain XML helper methods
# Should only use $self->_writer

sub add_single_row_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        labels => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my @labels = @{$parm{'labels'}};
    my @values = @{$parm{'values'}};

    my $writer = $self->_writer;

    $writer->startTag("Definition", "xmlns" => "http://www.pharmml.org/2013/08/Dataset");

    for (my $col = 0; $col < scalar(@labels); $col++) {
        $writer->emptyTag("Column", columnId => $labels[$col], columnType => "undefined", valueType => "real", columnNum => $col + 1);
    }

    $writer->endTag("Definition");

    $writer->startTag("Table", xmlns => "http://www.pharmml.org/2013/08/Dataset");
    $writer->startTag("Row");

    for (my $col = 0; $col < scalar(@values); $col++) {
        $writer->startTag("ct:Real");
        $writer->characters($values[$col]);
        $writer->endTag("ct:Real");
    }

    $writer->endTag("Row");
    $writer->endTag("Table"); 
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
    my @labels = @{$parm{'labels'}};
    my @values = @{$parm{'values'}};

    my $writer = $self->_writer;

    $writer->startTag("ds:Definition");
    $writer->emptyTag("ds:Column", columnId => "parameter", columnType => "undefined", valueType => "id", columnNum => 1);
    $writer->emptyTag("ds:Column", columnId => $name, columnType => "undefined", valueType => "id", columnNum => 2);
    $writer->endTag("ds:Definition");
    $writer->startTag("ds:Table");

    for (my $col = 0; $col < scalar(@labels); $col++) {
        $writer->startTag("ds:Row");
        $writer->startTag("ct:String");
        $writer->characters($labels[$col]);
        $writer->endTag("ct:String");
        $writer->startTag("ct:Real");
        $writer->characters($values[$col]);
        $writer->endTag("ct:Real");
        $writer->endTag("ds:Row"); 
    }

    $writer->endTag("ds:Table");
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
            $writer->dataElement("ct:Real", $element);
        }
        $writer->endTag("ct:MatrixRow");
    }
    $writer->endTag("ct:Matrix");
}

# End of XML helper methods

sub parse
{
    my $self = shift;
    my $writer = $self->_writer;
    my $outobj = $self->output;
	my $model = $self->model;

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
	my $ofv    = $outobj -> get_single_value(attribute => 'ofv',
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

	for (my $i=0; $i<scalar(@omnam); $i++){
		unless ($omegasame[$i]){
			push(@all_labels,$omnam[$i]);
			push(@est_values,$omegas[$i]);
			push(@se_values,$seomeg[$i]) if $have_ses;
		}
	}
	for (my $i=0; $i<scalar(@signam); $i++){
		unless ($sigmasame[$i]){
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


    $writer->startTag("SOBlock", "blkId" => "SO1");

    $writer->startTag("Estimation");

    $self->_add_population_estimates(labels => \@all_labels, values => \@est_values);

	if (scalar(@se_values) > 0) {
        my $correlation_matrix = linear_algebra::triangular_symmetric_to_full($outobj->correlation_matrix->[$problems]->[$sub_problems]);
		$self->_add_precision_population_estimates(
            labels => \@all_labels,
            standard_errors => \@se_values,
            relative_standard_errors => \@rel_se,
            correlation_matrix => $correlation_matrix,
        );
	}
    $self->_add_target_tool_messages;

    $writer->endTag("Estimation");

    $writer->endTag("SOBlock");
    $writer->endTag("SO");
    $writer->end();
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
        labels => { isa => 'ArrayRef' },
        standard_errors => { isa => 'ArrayRef' },
        relative_standard_errors => { isa => 'ArrayRef' },
        correlation_matrix => { isa => 'ArrayRef[ArrayRef]' },
    );
    my @labels = @{$parm{'labels'}};
    my @standard_errors = @{$parm{'standard_errors'}};
    my @relative_standard_errors = @{$parm{'relative_standard_errors'}};
    my $correlation_matrix = $parm{'correlation_matrix'};

    my $writer = $self->_writer;

    $writer->startTag("PrecisionPopulationEstimates");
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

    $writer->endTag("MLE");
    $writer->endTag("PrecisionPopulationEstimates");
}

sub _add_target_tool_messages
{
    my $self = shift;

    my $writer = $self->_writer;

    $writer->startTag("TargetToolMessages");

    $writer->dataElement("Termination", '');
    $writer->dataElement("Warnings", '');
    $writer->dataElement("Errors", '');

    $self->output->runtime =~ m/(\d+):(\d+):(\d+)/;
    my $seconds = $1 * 3600 + $2 * 60 + $3;
    $writer->dataElement("ElapsedTime", $seconds);

    $writer->endTag("TargetToolMessages");
}

1;
