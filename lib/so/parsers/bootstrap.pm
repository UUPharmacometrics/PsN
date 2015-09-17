package so::parsers::bootstrap;

# Package for parsing a PsN bootstrap_results file into an so object

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;

use utils::file;
use so::soblock;

has 'bootstrap_results' => ( is => 'rw', isa => 'Str' );
has 'rundir' => ( is => 'rw', isa => 'Str' );
has 'so' => ( is => 'rw', isa => 'so' );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'labels_hash' => ( is => 'rw', isa => 'Maybe[HashRef]' );
has '_so_block' => ( is => 'rw', isa => 'so::soblock' );
has '_bootstrap' => ( is => 'rw', isa => 'so::soblock::estimation::populationestimates::bootstrap' );
has '_precision_bootstrap' => ( is => 'rw', isa => 'so::soblock::estimation::precisionpopulationestimates::bootstrap' );

sub BUILD
{
    my $self = shift;

    if (defined $self->rundir) {
        $self->bootstrap_results($self->rundir . "/bootstrap_results.csv");
    }

    my $so_block = $self->so->SOBlock->[0];

    if (not defined $so_block) {
        $so_block = $self->so->create_block(name => "bootstrap");
    }

    if ($self->verbose) {
        print "Adding bootstrap results from file ", $self->bootstrap_results, " to SOBlock \"" . $so_block->blkId . "\"\n";
    }

    $self->_so_block($so_block);
    $self->_bootstrap($so_block->Estimation->PopulationEstimates->Bootstrap);
    $self->_precision_bootstrap($so_block->Estimation->PrecisionPopulationEstimates->Bootstrap);

    $self->_create_bootstrap();
}

sub _create_bootstrap
{
    my $self = shift;

    if (not -e $self->bootstrap_results) {
        $self->_so_block->TaskInformation->add_message(
            type => "ERROR",
            toolname => "PsN",
            name => "File error",
            content => "Bootstrap results file \"" . $self->bootstrap_results . "\" does not exist",
            severity => 10,
        );
        return;
    }

    open my $fh, '<', $self->bootstrap_results;
    my @parameters;
    my @parameters_original_name;
    my @percentiles;
    my @column;
    my $means;
    my $medians;
    my $seci_25;
    my $seci_975;
    my $ses;
    while (<$fh>) {
        if (/^means$/) {
            my $header = <$fh>;         # Get the header only once. means comes first in the file so do it here
            my @a = split /","/, $header;
            shift @a;
            shift @a;
            foreach my $param (@a) {
                $param =~ s/\s*//; # Remove spaces
                if ($param !~ /^se/) {
                    push @parameters, so::xml::mangle_symbol_idtype($param);
                    push @parameters_original_name, $param;
                } else {
                    last;
                }
            }
            $means = _read_line(fh => $fh, parameters => \@parameters);

        } elsif (/^medians$/) {
            <$fh>;
            $medians = _read_line(fh => $fh, parameters => \@parameters);

        } elsif (/^standard.error.confidence.intervals$/) {
            <$fh>;
            <$fh>;
            <$fh>;
            $seci_25 = _read_line(fh => $fh, parameters => \@parameters);
            <$fh>;
            <$fh>;
            $seci_975 = _read_line(fh => $fh, parameters => \@parameters);
            <$fh>;
            <$fh>;

        } elsif (/^standard.errors$/) {
            <$fh>;
            $ses = _read_line(fh => $fh, parameters => \@parameters);

        } elsif (/^percentile.confidence.intervals$/) {
            # Loop through percentiles
            <$fh>;
            for (my $i = 0; $i < 8; $i++) {
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
        $self->_so_block->TaskInformation->add_message(
            type => "WARNING",
            toolname => "PsN",
            name => "Bootstrap",
            content => "No bootstrap percentiles in " . $self->bootstrap_results . ". No Bootstrap percentiles added.",
            severity => 2,
        );
    } else {
        (my $used_parameters, my $filtered_column) = $self->filter(parameters => \@parameters, values => \@column);
        my $table = so::table->new(
            name => 'Percentiles',
            columnId => [ "Percentile", @$used_parameters ],
            columnType => [ ('undefined') x (scalar(@$used_parameters) + 1) ],
            valueType => [ ('real') x (scalar(@$used_parameters) + 1) ],
            columns => [ \@percentiles, @$filtered_column ],
        );
        $self->_precision_bootstrap->Percentiles($table);
    }

    (my $used_parameters, my $filtered_seci25) = $self->filter(parameters => \@parameters, values => $seci_25);
    (undef, my $filtered_seci975) = $self->filter(parameters => \@parameters, values => $seci_975);
    (undef, my $filtered_ses) = $self->filter(parameters => \@parameters, values => $ses);

    my $precision_estimates = so::table->new(
        name => "PrecisionEstimates",
        columnId => [ "Parameter", "StandardError", "LowerCI", "UpperCI", "Alpha" ],
        columnType => [ ('undefined') x 5 ],
        valueType => [ "string", ('real') x 4 ],
        columns => [ $used_parameters, $filtered_ses, $filtered_seci25, $filtered_seci975, [ (0.05) x scalar(@$used_parameters) ] ],
    );
    $self->_precision_bootstrap->PrecisionEstimates($precision_estimates);

    (my $used_parameters, my $adjusted_means) = $self->filter(parameters => \@parameters, values => $means);
    my $mean_table = so::table->new(name => "Mean", columnId => $used_parameters);
    $mean_table->single_row(values => $adjusted_means);
    $self->_bootstrap->Mean($mean_table);

    (my $used_parameters, my $adjusted_medians) = $self->filter(parameters => \@parameters, values => $medians);
    my $median_table = so::table->new(name => "Median", columnId => $used_parameters);
    $median_table->single_row(values => $adjusted_medians);
    $self->_bootstrap->Median($median_table);

    # warn if any parameter on sd/corr scale
    if (defined $self->labels_hash) {
        my @on_sd_corr;
        for (my $i = 0; $i < scalar(@$used_parameters); $i++) {
            if (grep { $_ eq $used_parameters->[$i] } @{$self->labels_hash->{'on_sd_scale'}}) {
                push @on_sd_corr, $used_parameters->[$i];
            }
        }
        if (scalar(@on_sd_corr) > 0) {
            my $warning_text;
            if (scalar(@on_sd_corr) == 1) {
                $warning_text = "The parameter " . $on_sd_corr[0];
            } else {
                $warning_text = "The parameters " . join(", ", @on_sd_corr[0 .. $#on_sd_corr - 1]) . " and " . $on_sd_corr[-1];
            }
            $warning_text .= " were requested on the sd/corr scale but are given on the var/cov scale in all bootstrap results.";

            $self->_so_block->TaskInformation->add_message(
                type => "WARNING",
                toolname => "PsN",
                name => "bootstrap_parameter_scale",
                content => $warning_text,
                severity => 2,
            );
        }
    }

    # add rawresults
    $self->_so_block->RawResults->add_datafile(name => $self->bootstrap_results, description => "PsN Bootstrap results file", oid => 'PsN_bootstrap_results'); 
    $self->_so_block->RawResults->add_datafile(name => 'included_individuals1.csv', description => "PsN Bootstrap included individuals", oid => 'PsN_bootstrap_included_individuals'); 

    my $dir = utils::file::directory($self->bootstrap_results);
    (my $raw_results) = glob("$dir/raw_results_*.csv");
    $raw_results = utils::file::remove_path($raw_results);

    $self->_so_block->RawResults->add_datafile(name => $raw_results, description => "PsN Bootstrap raw results", oid => 'PsN_bootstrap_raw_results'); 
} 

sub _read_line
{
    # Read one line of values from the bootstrap results file
    my %parm = validated_hash(\@_,
        fh => { isa => 'Ref' },
        parameters => { isa => 'ArrayRef' },
    );
    my $fh = $parm{'fh'};
    my @parameters = @{$parm{'parameters'}};

    my $row = <$fh>;
    my @a = split /,/, $row;
    shift @a;
    shift @a;

    my $value;
    my @data_row;
    for (my $col = 0; $col < scalar(@parameters); $col++) {
        $value = shift @a;
        $value =~ s/^\s*(.*)/\1/;
        if ($value ne 'NA') {
            push @data_row, $value;
        }
    }

    return \@data_row;
}

sub filter
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        parameters => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my $parameters = $parm{'parameters'};
    my $values = $parm{'values'};

    if (not defined $self->labels_hash) {
        return ($parameters, $values);
    }

    # Filter out parameters not intended for inclusion (without label and with FIX)
    my @used_parameters;
    my @used_values;
    for (my $i = 0; $i < scalar(@$parameters); $i++) {
       if (grep { $_ eq $parameters->[$i] } @{$self->labels_hash->{'labels'}}) {
            push @used_parameters, $parameters->[$i];
            push @used_values, $values->[$i];
       }
    }

    return (\@used_parameters, \@used_values);
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
