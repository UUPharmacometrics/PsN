#!/etc/bin/perl


use strict;
use warnings;
use FindBin qw($Bin);
use lib "$Bin/../..";
use includes; #file with paths to PsN packages
use Test::More;
use Test::Exception;
use scmplus;


is_deeply(scmplus::parsed_scope_reduction_steps('1,3,5'),{'scope_reduction_steps' => [1,3,5]},"scope reduction steps 1");
is_deeply(scmplus::parsed_scope_reduction_steps('all'),{'scope_reduction_steps' => ['all']},"scope reduction steps 2");
is_deeply(scmplus::parsed_scope_reduction_steps('none'),{'scope_reduction_steps' => []},"scope reduction steps 3");
is_deeply(scmplus::parsed_scope_reduction_steps(undef),{},"scope reduction steps 4");
dies_ok { scmplus::parsed_scope_reduction_steps('1,1,2')} "scope reduction steps not increasing";

is_deeply(scmplus::get_internal_logit('original_logit' => ['BASE','KA'], 'test_relations' => {'CL'=> [1,2],'V'=>[3]}),
		  [],"internal logit 1");
is_deeply(scmplus::get_internal_logit('original_logit' => ['CL','BASE'], 'test_relations' => {'CL'=> [1,2],'BASE'=>[3]}),
		  ['CL','BASE'],"internal logit 2");
is_deeply(scmplus::get_internal_logit('original_logit' => ['CL','BASE'], 'test_relations' => {'CL'=> [1,2],'V'=>[3]}),
		  ['CL'],"internal logit 3");

my %options = ('some' => 'value');

scmplus::set_common_defaults(options => \%options);

is_deeply(\%options, {'some' => 'value', handle_crashes => 0, crash_restarts => 1, clean => 2}, 'handle crashes 1');

%options = ('some' => 'value','crash_restarts' =>2);
is_deeply(\%options,{'some' => 'value',crash_restarts => 2},'handle crashes 2');

%options = ('some' => 'value','handle_crashes' =>0);
is_deeply(\%options,{'some' => 'value',handle_crashes => 0},'handle crashes 3');

$options{'handle_crashes'} =1;

done_testing();
