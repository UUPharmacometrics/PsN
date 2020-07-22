#!/etc/bin/perl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition

ok (have_pharmpy(), "Is Pharmpy available?");

done_testing();
