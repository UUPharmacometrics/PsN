#!/usr/bin/perl

use Test::Harness;

chdir "system";

my @test_files = <*.t>;

runtests @test_files;
