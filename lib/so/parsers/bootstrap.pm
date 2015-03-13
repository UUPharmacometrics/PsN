package so::parsers::bootstrap;

# Package for parsing NONMEM output into an so object

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;

    # Handle bootstrap_results
    if (defined $self->bootstrap_results) {
        # Find or create xml structure
        my $bootstrap_block = $self->_first_block;
        (my $block) = $SO->findnodes("SOBlock[\@blkId='$bootstrap_block']");
        if (not defined $block) {
            $bootstrap_block = "Bootstrap";
            $block = $self->create_block(name => $bootstrap_block);
            $SO->appendChild($block);
        }
        if ($self->verbose) {
            print "Adding bootstrap results from file ", $self->bootstrap_results, " to SOBlock \"$bootstrap_block\"\n";
        }
        my $estimation = $self->_xml->find_or_create_node(root_node => $block, node_name => "Estimation");

        my $bootstrap_message;

        # Create Bootstrap element
        if (-e $self->bootstrap_results) {
            my $ppi = $self->_xml->find_or_create_node(root_node => $estimation, node_name => "PrecisionPopulationEstimates");
            my $bootstrap = $self->_create_bootstrap();
            if (defined $bootstrap) {
                $ppi->appendChild($bootstrap);
            }
        } else {
            $bootstrap_message = {
                type => "ERROR",
                toolname => $self->toolname,
                name => "File error",
                content => "Bootstrap results file \"" . $self->bootstrap_results . "\" does not exist",
                severity => 10,
            };
        }

        # Create Bootstrap messages
        if (defined $bootstrap_message) {
            if ($block->exists("TaskInformation")) {
                (my $ti) = $block->findnodes("TaskInformation");
                my $message = $self->_xml->create_message(message => $bootstrap_message);
                my $first_child = $ti->firstChild();
                $ti->insertBefore($message, $first_child);
            } else {
                my $ti = $self->_create_task_information(messages => [ $bootstrap_message ]);
                $block->appendChild($ti)
            }
        }
    }

no Moose;
__PACKAGE__->meta->make_immutable;
1;
