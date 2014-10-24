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
    );
    my $rownames = $parm{'rownames'};
    my $colnames = $parm{'colnames'};

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
    $writer->endTag("ct:Matrix");
}

# End of XML helper methods

sub parse
{
    my $self = shift;
    my $writer = $self->_writer;
    my $outobj = $self->output;

    my $theta_labels = @{$self->model->labels(parameter_type => 'theta')}[0];
    my $omega_labels = @{$self->model->labels(parameter_type => 'omega')}[0];
    my $sigma_labels = @{$self->model->labels(parameter_type => 'sigma')}[0];
    my @all_labels = (@$theta_labels, @$omega_labels, @$sigma_labels);

    #Use accessors to retrieve data
    my $problems = 0;
    my $sub_problems = 0;

    #arrays (over problems) of arrays (over subproblems) of arrays of values. Only non-zero are stored
    my $thetaref = $outobj -> thetas();
    my $omegaref = $outobj -> omegas();
    my $sigmaref = $outobj -> sigmas();

    #arrays (over problems) of arrays (over subproblems) of arrays of names. 
    #One name per non-zero value theta/omega/sigma 
    my $thetanamesref = $outobj -> thetanames();
    my $omeganamesref = $outobj -> omeganames();
    my $sigmanamesref = $outobj -> sigmanames();

    #arrays (over problems) of arrays (over subproblems) of arrays of values, one per name. Values may be undef
    my $sethetaref = $outobj -> sethetas();
    my $cvsethetaref = $outobj -> cvsethetas();
    my $seomegaref = $outobj -> seomegas();
    my $cvseomegaref = $outobj -> cvseomegas();
    my $sesigmaref = $outobj -> sesigmas();
    my $cvsesigmaref = $outobj -> cvsesigmas();
    my $comegasref = $outobj -> comegas();
    my $csigmasref = $outobj -> csigmas();
    my $nmversion = $outobj -> nonmem_version();


    my ( %nam, %est, %cest, %ses );
    if (defined $thetaref-> [$problems][$sub_problems]){
    }else{
    }
    my @thetas = defined $thetaref-> [$problems][$sub_problems] ? @{$thetaref -> [$problems][$sub_problems]} : ();
    my @thnam  = defined $thetanamesref -> [$problems][$sub_problems] ? @{$thetanamesref -> [$problems][$sub_problems]} : ();
    my @sethet = defined $sethetaref -> [$problems][$sub_problems] ? @{$sethetaref -> [$problems][$sub_problems]} : ();

    $nam{'theta'} = \@thnam;
    $est{'theta'} = \@thetas;
    $ses{'theta'} = \@sethet;

    ## Omegas
    my @omegas    = defined $omegaref -> [$problems][$sub_problems] ? @{$omegaref -> [$problems][$sub_problems]} : ();
    my @comegas   = defined $comegasref -> [$problems][$sub_problems] ? @{$comegasref -> [$problems][$sub_problems]} : ();
    my @omnam     = defined $omeganamesref -> [$problems][$sub_problems]? @{$omeganamesref -> [$problems][$sub_problems]}                : ();
    my @seomeg    = defined $seomegaref -> [$problems][$sub_problems] ? @{$seomegaref -> [$problems][$sub_problems]} : ();

    $nam{'omega'}  = \@omnam;
    $est{'omega'}  = \@omegas;
    $cest{'omega'} = \@comegas;
    $ses{'omega'}  = \@seomeg;

    ## Sigmas
    my @sigmas  = defined $sigmaref -> [$problems][$sub_problems] ? @{$sigmaref -> [$problems][$sub_problems]} : ();
    my @csigmas = defined $csigmasref -> [$problems][$sub_problems] ? @{$csigmasref -> [$problems][$sub_problems]} : ();
    my @signam  = defined $sigmanamesref -> [$problems][$sub_problems] ? @{$sigmanamesref -> [$problems][$sub_problems]}                : ();
    my @sesigm  = defined $sesigmaref -> [$problems][$sub_problems] ? @{$sesigmaref -> [$problems][$sub_problems]} : ();

    $nam{'sigma'}  = \@signam;
    $est{'sigma'}  = \@sigmas;
    $cest{'sigma'} = \@csigmas;
    $ses{'sigma'}  = \@sesigm;

    my @allparams = (@thetas);
    my @allses = (@sethet);

    for (my $i = 0; $i < scalar(@omegas); $i++) {
        next if (not defined $seomeg[$i]);      # This paramater was not estimated
        push @allparams, $omegas[$i];
        push @allses, $seomeg[$i];
    }

    for (my $i = 0; $i < scalar(@sigmas); $i++) {
        next if (not defined $sesigm[$i]);      # This paramater was not estimated
        push @allparams, $sigmas[$i];
        push @allses, $sesigm[$i];
    }

    #Calculate relative standard errors
    my @relses = ();
    for (my $i = 0; $i < scalar(@allparams); $i++) {
        push @relses, $allses[$i] / $allparams[$i];
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

    $self->_add_population_estimates(labels => \@all_labels, values => \@allparams);
    $self->_add_precision_population_estimates(labels => \@all_labels, standard_errors => \@allses, relative_standard_errors => \@relses);
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
    );
    my @labels = @{$parm{'labels'}};
    my @standard_errors = @{$parm{'standard_errors'}};
    my @relative_standard_errors = @{$parm{'relative_standard_errors'}};

    my $writer = $self->_writer;

    $writer->startTag("PrecisionPopulationEstimates");
    $writer->startTag("MLE");

    $writer->startTag("StandardError");
    $self->add_parameter_table(name => 'SE', labels => \@labels, values => \@standard_errors);
    $writer->endTag("StandardError");
   
    $writer->startTag("RelativeStandardError");
    $self->add_parameter_table(name => 'RSE', labels => \@labels, values => \@relative_standard_errors);
    $writer->endTag("RelativeStandardError");

    # FIXME: Check status of covariance step first
    $writer->startTag("CorrelationMatrix");
    $self->add_matrix(rownames => \@labels, colnames => \@labels);  #FIXME: No content added here
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
