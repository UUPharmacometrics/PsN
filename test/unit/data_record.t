#!/usr/bin/perl

# Unit tests for the record data.pm

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use model::problem::data;

is (model::problem::data::_is_ignore_accept("IGNORE(VISI>2)"), 1, "_is_ignore_accept 1");
is (model::problem::data::_is_ignore_accept("IGNORE", "(VISI>2)"), 1, "_is_ignore_accept 2");
is (model::problem::data::_is_ignore_accept("IGN", "'%'"), 0, "_is_ignore_accept 3");
is (model::problem::data::_is_ignore_accept("IGNO", "#"), 0, "_is_ignore_accept 4");

done_testing;
