package so::parsers::bootstrap;

# Package for parsing a PsN bootstrap_results file into an so object

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;

use so::soblock;

has 'bootstrap_results' => ( is => 'rw', isa => 'Str' );
has 'so' => ( is => 'rw', isa => 'so' );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has '_so_block' => ( is => 'rw', isa => 'so::soblock' );
has '_bootstrap' => ( is => 'rw', isa => 'so::soblock::estimation::populationestimates::bootstrap' );
has '_precision_bootstrap' => ( is => 'rw', isa => 'so::soblock::estimation::precisionpopulationestimates::bootstrap' );

sub BUILD
{
    my $self = shift;

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
    my @percentiles;
    my @column;
    my $means;
    my $medians;
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
                } else {
                    last;
                }
            }
            $means = _read_line(fh => $fh, parameters => \@parameters);

        } elsif (/^medians$/) {
            <$fh>;
            $medians = _read_line(fh => $fh, parameters => \@parameters);

        } elsif (/^percentile.confidence.intervals$/) {
            # Loop through percentiles
            <$fh>;
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
        $self->_so_block->TaskInformation->add_message(
            type => "WARNING",
            toolname => "PsN",
            name => "Bootstrap",
            content => "No bootstrap percentiles in " . $self->bootstrap_results . ". No Bootstrap percentiles added.",
            severity => 2,
        );
    } else {
        my $table = so::table->new(
            name => 'Percentiles',
            columnId => [ "Percentile", @parameters ],
            columnType => [ ('undefined') x (scalar(@parameters) + 1) ],
            valueType => [ ('real') x (scalar(@parameters) + 1) ],
            columns => [ \@percentiles, @column ],
        );
        $self->_precision_bootstrap->Percentiles($table);
    }

    my $mean_table = so::table->new(name => "Mean", columnId => \@parameters);
    $mean_table->single_row(values => $means);
    $self->_bootstrap->Mean($mean_table);

    my $median_table = so::table->new(name => "Median", columnId => \@parameters);
    $median_table->single_row(values => $medians);
    $self->_bootstrap->Median($median_table);
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

no Moose;
__PACKAGE__->meta->make_immutable;
1;
