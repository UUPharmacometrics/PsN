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
    while (<$fh>) {
        if (/^percentile.confidence.intervals$/) {
            my $header = <$fh>;
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
        $self->_so_block->TaskInformation->add_message(
            type => "WARNING",
            toolname => "PsN",
            name => "Bootstrap",
            content => "No bootstrap percentiles in " . $self->bootstrap_results . ". No Bootstrap results added.",
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
} 

no Moose;
__PACKAGE__->meta->make_immutable;
1;
