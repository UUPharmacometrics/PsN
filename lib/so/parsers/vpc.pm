package so::parsers::vpc;

# Package for parsing a PsN vpc_results file into an so object

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use File::Basename;
use include_modules;
use nmtablefile;

use so::soblock;

has 'rundir' => ( is => 'rw', isa => 'Str' );
has 'vpc_results' => ( is => 'rw', isa => 'Str' );
has 'so' => ( is => 'rw', isa => 'so' );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'labels_hash' => ( is => 'rw', isa => 'Maybe[HashRef]' );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'DV' );
has '_so_block' => ( is => 'rw', isa => 'so::soblock' );

sub BUILD
{
    my $self = shift;

    if (defined $self->rundir) {
        $self->vpc_results(File::Spec->catfile($self->rundir, "/vpc_results.csv"));
    }

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

    # add rawresults
    (my $vpc_results, my $vpcdir) = fileparse($self->vpc_results);
    $self->_so_block->RawResults->add_datafile(name => $vpc_results, description => "PsN vpc results file", oid => "PsN_VPC_results");

    # find vpctab
    (my $vpctab) = glob($vpcdir . "vpctab*");
    if (defined $vpctab and -e $vpctab) {
        $vpctab = fileparse($vpctab);
        $self->_so_block->RawResults->add_datafile(name => $vpctab, description => "PsN vpctab", oid => "PsN_VPC_vpctab");
    }

    $self->_add_original_table();
    $self->_add_simulation_table();
}

sub _add_simulation_table
{
    my $self = shift;

    my $simulation_table = $self->rundir . '/m1/vpc_simulation.1.npctab.dta';

    my $nmtables = nmtablefile->new(filename => $simulation_table);

    my $replicate_no = 1;
    for my $nmtable (@{$nmtables->tables}) {    # Loop over the replicates
        my $idcol = $nmtable->header->{'ID'};
        my $timecol = $nmtable->header->{'TIME'};
        my $dvcol = $nmtable->header->{$self->dv};
        my $mdvcol = $nmtable->header->{'MDV'};

        my @columns;
        if (not defined $mdvcol) {
            @columns = ($nmtable->columns->[$idcol], $nmtable->columns->[$timecol], $nmtable->columns->[$dvcol]);
        } else {
            my @id;
            my @time;
            my @dv;
            for (my $i = 0; $i < scalar(@{$nmtable->columns->[$idcol]}); $i++) {
                if ($nmtable->columns->[$mdvcol]->[$i] == 0) {
                    push @id, $nmtable->columns->[$idcol]->[$i];
                    push @time, $nmtable->columns->[$timecol]->[$i];
                    push @dv, $nmtable->columns->[$dvcol]->[$i];
                }
            }
            @columns = (\@id, \@time, \@dv);
        }

        my $sim_block = so::soblock::simulation::simulationblock->new(replicate => $replicate_no);

        my $simulated_profiles = so::soblock::simulation::simulationblock::simulationtable->new(
            name => "SimulatedProfiles",
            columnId => [ "ID", "TIME", "DV" ],
            columnType => [ "id", "idv", "dv" ],
            valueType => [ "string", "real", "real" ],
            columns => \@columns,
        );

        push @{$sim_block->SimulatedProfiles}, $simulated_profiles;
        push @{$self->_so_block->Simulation->SimulationBlock}, $sim_block;

        $replicate_no++;
    }
}

sub _add_original_table
{
    my $self = shift;

    my $original_table = $self->rundir . '/m1/vpc_original.npctab.dta';

    my $nmtables = nmtablefile->new(filename => $original_table);
    my $nmtable = $nmtables->tables->[0];

    my $idcol = $nmtable->header->{'ID'};
    my $timecol = $nmtable->header->{'TIME'};
    my $dvcol = $nmtable->header->{$self->dv};
    my $mdvcol = $nmtable->header->{'MDV'};

    my @columns;
    if (not defined $mdvcol) {
        @columns = ($nmtable->columns->[$idcol], $nmtable->columns->[$timecol], $nmtable->columns->[$dvcol]);
    } else {
        my @id;
        my @time;
        my @dv;
        for (my $i = 0; $i < scalar(@{$nmtable->columns->[$idcol]}); $i++) {
            if ($nmtable->columns->[$mdvcol]->[$i] == 0) {
                push @id, $nmtable->columns->[$idcol]->[$i];
                push @time, $nmtable->columns->[$timecol]->[$i];
                push @dv, $nmtable->columns->[$dvcol]->[$i];
            }
        }
        @columns = (\@id, \@time, \@dv);
    }

    my $table = so::table->new(
        name => "IndivObservationPrediction",
        columnId => [ "ID", "TIME", "OBSERVATION" ],
        columnType => [ "id", "idv", "dv" ],
        valueType => [ "string", "real", "real" ],
        columns => \@columns,
    );

    $self->_so_block->ModelDiagnostic->DiagnosticStructuralModel->IndivObservationPrediction($table);
}

1;
