package so::parsers::vpc;

# Package for parsing a PsN vpc_results file into an so object

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;

use so::soblock;
use utils::file;

has 'vpc_results' => ( is => 'rw', isa => 'Str' );
has 'so' => ( is => 'rw', isa => 'so' );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has '_so_block' => ( is => 'rw', isa => 'so::soblock' );

sub BUILD
{
    my $self = shift;

    my $so_block = $self->so->SOBlock->[0];

    if (not defined $so_block) {
        $so_block = $self->so->create_block(name => "vpc");
    }

    if ($self->verbose) {
        print "Adding vpc results from file ", $self->vpc_results, " to SOBlock \"" . $so_block->blkId . "\"\n";
    }

    $self->_so_block($so_block);

    $self->_create_vpc();
}

sub _create_vpc
{
    my $self = shift;

    if (not -e $self->vpc_results) {
        $self->_so_block->TaskInformation->add_message(
            type => "ERROR",
            toolname => "PsN",
            name => "File error",
            content => "vpc results file \"" . $self->vpc_results . "\" does not exist",
            severity => 10,
        );
        return;
    }

    # add rawresults
    $self->_so_block->RawResults->add_datafile(name => $self->vpc_results, description => "PsN vpc results file"); 

    # find vpctab
    my $vpcdir = utils::file::directory($self->vpc_results);
    print "YYY ", $self->vpc_results, "\n";
    print "QQQ $vpcdir\n";
    (my $vpctab) = glob "$vpcdir/vpctab*";

    $self->_so_block->RawResults->add_datafile(name => $vpctab, description => "PsN vpctab"); 
} 

no Moose;
__PACKAGE__->meta->make_immutable;
1;
