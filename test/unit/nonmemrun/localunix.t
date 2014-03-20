#!/etc/bin/perl

use strict;
use warnings;
use Test::More tests => 1;
use lib "../.."; #location of includes.pm
use includes; #file with paths to PsN packages

use nonmemrun::localunix;

my $exec_call_string;

BEGIN
{
	*CORE::GLOBAL::exec = sub { $exec_call_string = $_[0] };
}

#my $nonmemrun = nonmemrun::localunix->new(nm_version => 'default');

#$nonmemrun->submit;

is($exec_call_string, undef, "localunix call string");


done_testing();
