package so::parsers::sse;

# Package for parsing the results from an sse

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use File::Spec;
use include_modules;

use so::soblock;

has 'rundir' => ( is => 'rw', isa => 'Str' );
has 'so' => ( is => 'rw', isa => 'so' );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'labels_hash' => ( is => 'rw', isa => 'Maybe[HashRef]' );

sub BUILD
{
    my $self = shift;

    my $so_block = $self->so->SOBlock->[0];

    if (not defined $so_block) {
        $so_block = $self->so->create_block(name => "sse");
    }

    # add rawresults
    if (-e $self->rundir . "/sse_results.csv") {
        $so_block->RawResults->add_datafile(
            name => "sse_results.csv",
            description => "PsN SSE results file",
            oid => 'PsN_SSE_results'
        );
    }

    (my $raw_results) = glob($self->rundir . "/raw_results_*.csv");
    if (defined $raw_results) {
        (undef, undef, $raw_results) = File::Spec->splitpath($raw_results);
        $so_block->RawResults->add_datafile(name => $raw_results, description => "PsN SSE raw results", oid => 'PsN_SSE_raw_results');
    }
}

1;
