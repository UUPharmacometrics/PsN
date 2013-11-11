#!/usr/bin/perl

use Test::Harness;

chdir "courses/decision_making";

my @test_files = <*.t>;

runtests @test_files;
