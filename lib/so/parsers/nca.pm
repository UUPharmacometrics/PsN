package so::parsers::nca;

# Package for parsing the results from an nca 

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
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
        $so_block = $self->so->create_block(name => "nca");
    }

    # add rawresults
    if (-e $self->rundir . "npctab.dta") {
        $so_block->RawResults->add_datafile(
            name => "npctab.dta",
            description => "PsN simulation file",
            oid => 'PsN_NCA_results'
        );
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
