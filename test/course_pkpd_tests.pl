#!/usr/bin/perl

use Test::Harness;

chdir "courses/pkpd";

my @test_files = <*.t>;

runtests @test_files;
