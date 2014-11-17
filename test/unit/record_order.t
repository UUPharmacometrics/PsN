#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use model::problem::record_order;

my $record_order = model::problem::record_order->new(order =>
    [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ]
);

# _get_pos
is ($record_order->_get_pos('input'), 1, "_get_pos existing 1");
is ($record_order->_get_pos('problem'), 0, "_get_pos first");
is ($record_order->_get_pos('table'), 11, "_get_pos last");
is ($record_order->_get_pos('sim'), undef, "_get_pos does not exist");

# _find_records
is ($record_order->_find_records(records => ['input', 'data']), 1, "_find_first_record 1");
is ($record_order->_find_records(records => ['problem', 'pk']), 0, "_find_first_record 2");
is ($record_order->_find_records(records => ['subroutine', 'pk', 'table']), 3, "_find_first_record 3");
is ($record_order->_find_records(records => ['boule', 'croquet']), undef, "_find_first_record 4");

#insert

# what to insert, starting, result
my @test_orders = (
    [
        [ 'sizes' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'sizes', 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ]
    ], [
        [ 'table' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ]
    ], [
        [ 'simulation' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'simulation', 'estimation', 'covariance', 'table' ],
    ], [
        [ 'simulation' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'covariance', 'table' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'covariance', 'table', 'simulation' ],
    ], [
        [ 'msfi' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data', 'msfi', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
    ], [
        [ 'prior' ],,
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
       [ 'problem', 'input', 'data', 'prior', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
    ], [
       [ 'bind' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'bind', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
    ], [
        [ 'bind', 'prior' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'bind', 'data', 'prior', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
    ],
);

foreach my $test (@test_orders) {
    my $record_order = model::problem::record_order->new(order => $test->[1]);
    foreach my $record (@{$test->[0]}) {
        $record_order->insert($record);
    }
    is_deeply ($record_order->order, $test->[2], "insert adding " . join(', ', @{$test->[0]}));
}


done_testing;
