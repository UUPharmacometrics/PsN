#!/etc/bin/perl

use strict;
use warnings;
use FindBin qw($Bin);
use lib "$Bin/../.."; #location of includes.pm
use includes;
use Test::More;
use Test::Exception;
use monitor;


is(monitor::ext_file('NM_run2/psn.mod'),'NM_run2/psn.ext','ext-file psn.mod');
is(monitor::ext_file('NM_run2/psn.ctl'),'NM_run2/psn.ext','ext-file psn.ctl');
is(monitor::ext_file('m1/CLAPGR2.mod'),'m1/CLAPGR2.ext','ext-file m1');

is(monitor::lst_file('NM_run2/psn.mod'),'NM_run2/psn.lst','lst-file psn.mod');
is(monitor::lst_file('NM_run2/psn.ctl'),'NM_run2/psn.lst','lst-file psn.ctl');
is(monitor::lst_file('m1/CLAPGR2.mod'),'m1/CLAPGR2.lst','lst-file m1');

is_deeply(monitor::get_model_NMrun_translation($includes::testfiledir.'/scmplus/monitor/modelfit_running/'),
		  {'CLACE2.mod' => 'NM_run1',
		   'CLCRCL5.mod' => 'NM_run2',
		   'CLDIG2.mod' => 'NM_run3',
		   'CLDIU2.mod' => 'NM_run4',
		   'CLNYHA2.mod' =>  'NM_run5',
		   'CLSEX2.mod' => 'NM_run6',
		   'CLWT5.mod' =>  'NM_run7',
		   'VACE2.mod' =>  'NM_run8',
		   'VCRCL5.mod' => 'NM_run9',
		   'VDIG2.mod' =>  'NM_run10',
		   'VDIU2.mod' =>  'NM_run11',
		   'VNYHA2.mod' => 'NM_run12',
		   'VSEX2.mod' =>  'NM_run13',
		   'VWT5.mod' =>   'NM_run14'},'get model nmrun translation');

is_deeply(monitor::get_candidate_models($includes::testfiledir.'/scmplus/monitor/scm_dir1/m1'),
		  {
			  'CLAPGR2'=>{'parameter'=>'CL','covariate'=>'APGR','continuous'=>0,'oldstate'=>1,'state'=> 2},
			  'CLCV12'=>{'parameter'=>'CL','covariate'=>'CV1','continuous'=>1,'oldstate'=>1,'state'=> 2},
			  'CLWGT2'=>{'parameter'=>'CL','covariate'=>'WGT','continuous'=>1,'oldstate'=>1,'state'=> 2},
			  'VCV22'=>{'parameter'=>'V','covariate'=>'CV2','continuous'=>1,'oldstate'=>1,'state'=> 2},
			  'VCVD12'=>{'parameter'=>'V','covariate'=>'CVD1','continuous'=>0,'oldstate'=>1,'state'=> 2},
		  },'get candidate models in m1');

is(monitor::maxevals_exceeded($includes::testfiledir . '/scmplus/monitor/maxeval_exceeded.lst'),1,'maxeval exceeded');
is(monitor::maxevals_exceeded($includes::testfiledir . '/scmplus/pheno_with_cov.lst'),0,'maxeval not exceeded');

cmp_ok(monitor::get_final_ofv($includes::testfiledir . '/scmplus/monitor/finished.ext'),'==',-639.01802895383548,'final ofv');

my ($feval,$ofv) = monitor::get_current_feval_ofv($includes::testfiledir . '/scmplus/monitor/finished.ext');

cmp_ok($feval,'==',48,'monitor feval');
cmp_ok($ofv,'==',-639.01802895383548,'monitor ofv');

($feval,$ofv) = monitor::get_current_feval_ofv($includes::testfiledir . '/scmplus/monitor/modelfit_running/NM_run3/psn.ext');

cmp_ok($feval,'==',0,'monitor feval');
cmp_ok($ofv,'==',-634.64433830581231,'monitor ofv');

($feval,$ofv) = monitor::get_current_feval_ofv($includes::testfiledir . '/scmplus/monitor/modelfit_running/NM_run3/psn.hej');

is($feval,undef,'monitor feval');
is($ofv,undef,'monitor ofv');

($feval,$ofv) = monitor::get_current_feval_ofv($includes::testfiledir . '/scmplus/empty.ext');

is($feval,undef,'monitor feval empty');
is($ofv,undef,'monitor ofv empty');

is(monitor::get_scriptname($includes::testfiledir . '/scmplus/monitor/'), 'scmplus', 'get toolname scmplus');
is(monitor::get_scriptname($includes::testfiledir . '/scmplus/monitor/scm_dir1'), 'scm', 'get toolname scm');


my %need_options=('logfile' => undef, 'search_direction' => undef);
is(monitor::get_options(directory =>$includes::testfiledir . '/scmplus/notthere/',
						options => \%need_options),0,'no logfilename');

is(monitor::get_options(directory => $includes::testfiledir . '/scmplus/monitor/',
						options => \%need_options),2,'find 2 options');

is_deeply(\%need_options,{'logfile' =>'/home/kajsa/sandbox/demoPsNplus/scmplus_dir9/scmlog.txt',
						  'search_direction' => 'forward'}, 'found options');

my ($active,$newforward) = monitor::get_active_scmdir(
	basedir => '/PMX/Projects/Pharmetheus/PsNplus/perl/test/test_files/monitor',
	is_asrscm => 0,
	last_logged_modeldir => undef,
	last_direction_forward => 0);
#is($active,'/PMX/Projects/Pharmetheus/PsNplus/perl/test/test_files/monitor','active scmdir');
is($newforward,0,'new forward 1');

($active,$newforward) = monitor::get_active_scmdir(
	basedir => '/PMX/Projects/Pharmetheus/PsNplus/perl/test/test_files/monitor',
	last_logged_modeldir => '/PMX/Projects/Pharmetheus/PsNplus/perl/test/test_files/monitor/m1',
	is_asrscm => 0,
	last_direction_forward => 1);
#is($active,'/PMX/Projects/Pharmetheus/PsNplus/perl/test/test_files/monitor/scm_dir1','active scmdir 2');
is($newforward,1,'new forward 2');


is(monitor::strip_m1('/home/kajsa/sandbox/demoPsNplus/scmplus_dir9/m1/'),'/home/kajsa/sandbox/demoPsNplus/scmplus_dir9','strip m1 1');
is(monitor::strip_m1('/home/kajsa/sandbox/demoPsNplus/scmplus_dir9/m1'),'/home/kajsa/sandbox/demoPsNplus/scmplus_dir9','strip m1 2');
is(monitor::strip_m1('/home/kajsa/sandbox/demoPsNplus/scmplus_dir9/'),'/home/kajsa/sandbox/demoPsNplus/scmplus_dir9','strip m1 3');

my %active = (
	'CLAPGR2'=>{'finished' => 0,'ofv' => '610.69468871903427','feval' => 32,'maxeval_exceeded' => undef,
				'parameter'=>'CL','covariate'=>'APGR','oldstate'=>1,'state'=> 2,'jobid' => undef,'rounding_errors' => undef},
	'CLCV12'=>{'finished' => 1,'ofv' => '625.34728590348573','maxeval_exceeded' => 0,
			   'parameter'=>'CL','covariate'=>'CV1','oldstate'=>1,'state'=> 2,'jobid' => undef,'rounding_errors' => undef},
	'CLWGT2'=>{'finished' => 1,'ofv' => '590.06015647499623','maxeval_exceeded' => 0,
			   'parameter'=>'CL','covariate'=>'WGT','oldstate'=>1,'state'=> 2,'jobid' => undef,'rounding_errors' => undef},
	'VCV22'=>{'finished' => 1,'ofv' => '626.61182522009597','maxeval_exceeded' => 0,
			  'parameter'=>'V','covariate'=>'CV2','oldstate'=>1,'state'=> 2,'jobid' => undef,'rounding_errors' => undef},
	'VCVD12'=>{'finished' => 1,'ofv' => '625.89604505154568','maxeval_exceeded' => 0,
			   'parameter'=>'V','covariate'=>'CVD1','oldstate'=>1,'state'=> 2,'jobid' => undef,'rounding_errors' => undef});

is_deeply(monitor::get_scm_progress(tooldir=>$includes::testfiledir . '/scmplus/monitor/active_scm_dir1'),\%active,'get scm progress');

#is(monitor::get_jobid($includes::testfiledir),'338052','get jobid');
done_testing();
