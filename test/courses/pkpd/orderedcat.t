#!/etc/bin/perl

use strict;
use warnings;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

do_course_tests($Bin, 'orderedcat');
