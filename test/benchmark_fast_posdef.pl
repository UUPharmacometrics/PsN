#!/usr/bin/perl

use strict;
use warnings;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use Math::Random;
use tool::sir;
use Time::HiRes 'gettimeofday', 'tv_interval';

print "Benchmarking fast_posdef_check option (cholesky check) to tool::sir::check_blocks_posdef()\n";
foreach my $adjust_blocks (0,1) {
    my $adj_str = $adjust_blocks ? "adjust" : "no adjust";

    # settings: dimensionality and number of blocks to pass to check_blocks_posdef()
    # (larger matrix sizes give more relative speedup)
    my @dims    = $adjust_blocks ? (5, 10, 20, 50) : (5, 10, 20, 50);
    my $nblocks = $adjust_blocks ? 100 : 1;

    my $xvecs = [];
    my $hash_arrs = [];
    foreach my $dim (@dims) {
        my $nelements = ($dim*$dim - $dim)/2 + $dim;
        my $av = 0;
        my $sd = 10;
        my @xvec = random_normal($nblocks*$nelements, $av, $sd);

        my $hash_arr = [];
        for (my $i=0; $i<$nblocks; $i++) {
            my @indices = (($i*$nelements)..($i*$nelements + $nelements - 1));

            my $hash_ref = {'size' => $dim, 'indices' => \@indices};
            push @{$hash_arr}, $hash_ref;
        }

        push @{$xvecs}, \@xvec;
        push @{$hash_arrs}, $hash_arr;
    }

    for (my $i=0; $i<scalar(@dims); $i++) {
        my $dim = $dims[$i];
        my @xvec = @{$xvecs->[$i]};
        my $hash_arr = $hash_arrs->[$i];

        my @elapsed;
        foreach my $fast_posdef (0,1) {
            my $posdef_str = $fast_posdef ? "fast" : "slow";
            printf "(%9s; %4s) n=%4u %2ux%2u random matrices", $adj_str, $posdef_str, $nblocks, $dim, $dim;

            my $start = [ gettimeofday() ];
            my ($acc,$adj) = tool::sir::check_blocks_posdef(xvec => \@xvec,
                                                            hash_array => $hash_arr,
                                                            adjust_blocks => $adjust_blocks,
                                                            fast_posdef_check => $fast_posdef);
            my $elapsed = tv_interval($start);
            print ", elapsed time: $elapsed s\n";
            $elapsed[$fast_posdef] = $elapsed;
        }
        my $speedup = $elapsed[0]/$elapsed[1];
        printf "(speedup: %.2f)\n", $speedup;
    }
}
