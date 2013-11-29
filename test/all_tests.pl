#!/usr/bin/perl

use Test::Harness;

chdir "unit";

my @test_files = <*.t>;

runtests @test_files;

chdir "../system";

my @test_files = <*.t>;

runtests @test_files;

chdir "../courses/pkpd";

my @test_files = <*.t>;

runtests @test_files;

chdir "../decision_making";

my @test_files = <*.t>;

runtests @test_files;
