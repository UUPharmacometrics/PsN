#!/usr/bin/perl

use Test::Harness;

chdir "unit";

my @test_files = <*.t>;

runtests @test_files;
