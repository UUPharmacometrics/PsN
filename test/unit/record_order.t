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
    ],[
        [ 'simulation' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'table' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'simulation', 'table' ],
    ], [
        [ 'simulation' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'covariance', 'table' ],
        [ 'problem', 'input', 'data', 'subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'simulation','covariance', 'table' ],
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
	[
        [ 'pred' ],
        [ 'problem', 'input', 'data', 'abbreviated', 'omega', 'sigma', 'estimation' ],
        [ 'problem', 'input', 'data', 'abbreviated', 'pred','omega', 'sigma', 'estimation' ],
    ],
	[
        [ 'pk','error' ],
        [ 'problem', 'input', 'data', 'abbreviated', 'omega', 'sigma', 'estimation' ],
        [ 'problem', 'input', 'data', 'abbreviated', 'pk','error','omega', 'sigma', 'estimation' ],
    ],
	[
        [ 'omega' ],
        [ 'problem', 'input', 'data', 'pred', 'estimation' ],
        [ 'problem', 'input', 'data', 'pred','omega', 'estimation' ],
    ],
	[
        [ 'omega' ],
        [ 'problem', 'input', 'data', 'pred','theta', 'estimation' ],
        [ 'problem', 'input', 'data', 'pred','theta','omega', 'estimation' ],
    ],
	[
        [ 'infn' ],
        [ 'problem', 'input', 'data', 'pred','theta', 'estimation' ],
        [ 'problem', 'input', 'data','infn', 'pred','theta', 'estimation' ],
    ],
	[
        [ 'abbreviated' ],
        [ 'problem', 'input', 'data', 'pk','error','omega', 'sigma', 'estimation' ],
        [ 'problem', 'input', 'data', 'abbreviated', 'pk','error','omega', 'sigma', 'estimation' ],
    ],
	[
        [ 'subroutine' ],
        [ 'problem', 'input', 'data', 'des','aes','omega', 'sigma', 'estimation' ],
        [ 'problem', 'input', 'data', 'subroutine','des','aes','omega', 'sigma', 'estimation' ],
    ],
	[
        [ 'contr' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data', 'contr','subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
    ],
	[
        [ 'nonparametric' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'nonparametric','table' ],
    ],
	[
        [ 'estimation' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance','table' ],
    ],
	[
        [ 'covariance' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance','table' ],
    ],
	[
        [ 'theta' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
    ],
	[
        [ 'sigma' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'sigma', 'estimation', 'covariance', 'table' ],
    ],
	[
        [ 'thetap' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','thetap', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
    ],
	[
        [ 'thetapv' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','thetapv', 'omega', 'sigma', 'estimation', 'covariance', 'table' ],
    ],
	[
        [ 'omegap' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'omegap','sigma', 'estimation', 'covariance', 'table' ],
    ],
	[
        [ 'omegapd' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'omegap', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'omegap', 'omegapd','sigma', 'estimation', 'covariance', 'table' ],
    ],
	[
        [ 'sigmap' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'sigma','sigmap', 'estimation', 'covariance', 'table' ],
    ],
	[
        [ 'sigmapd' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'sigma', 'estimation', 'covariance', 'table' ],
        [ 'problem', 'input', 'data','subroutine', 'pk', 'error', 'theta','omega', 'sigma','sigmapd', 'estimation', 'covariance', 'table' ],
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
