package includes;

use strict;
use File::Spec;
use Cwd;

# Setup an include path to the lib directory
# First get the path of this module and split out the directory part
# Then make it into an absolute path
BEGIN
{
	my ($volume, $directory, $file) = File::Spec->splitpath(__FILE__);
	my $libpath = Cwd::abs_path($volume . $directory . '../lib');
	unshift @INC, $libpath;
}

# Get an absolute path to the scripts
my ($volume, $directory, $file) = File::Spec->splitpath(__FILE__);
our $path = Cwd::abs_path($volume . $directory . '../bin');

our $testfiledir = Cwd::abs_path($volume . $directory . 'test_files');

use PsN;

our $version = '';

#n41 cluster 
#use lib "/opt/local64/PsN/PsN_3_7_6";
#use lib "/opt/local64/PsN";
#use PsN_3_7_6;
#our $path = "/opt/local64/PsN/bin/";
#our $version = '-3.7.6';

#doris
#use lib "/opt/PsN/PsN_3_7_6";
#use lib "/opt/PsN";
#use PsN_3_7_6;
#our $path = "/opt/PsN/bin/";
#our $version = '-3.7.6';

#MAC
#use lib "/opt/PsN/PsN_3_7_6";
#use lib "/opt/PsN";
#use PsN_3_7_6;
#our $path = "/opt/PsN/bin/";
#our $version = '-3.7.6';

#XP under MAC
#use lib "C:\Perl\site\lib\PsN_3_7_6";
#use lib "C:\Perl\site\lib";
#use PsN_3_7_6;
#our $path = "C:\Perl\bin";
#our $version = ''; #can't have numbers on windows

our $boot_scm = $path.'boot_scm'.$version;
our $bootstrap = $path.'bootstrap'.$version;
our $cdd = $path.'cdd'.$version;
our $crossval = $path.'crossval'.$version;
our $data_stats = $path.'data_stats'.$version;
our $ebe_npde = $path.'ebe_npde'.$version;
our $execute = $path.'execute'.$version;
our $extended_grid = $path.'extended_grid'.$version;
our $frem = $path.'frem'.$version;
our $gls = $path.'gls'.$version;
our $lasso = $path.'lasso'.$version;
our $linearize = $path.'linearize'.$version;
our $llp = $path.'llp'.$version;
our $mcmp = $path.'mcmp'.$version;
our $mimp = $path.'mimp'.$version;
our $nonpb = $path.'nonpb'.$version;
our $npc = $path.'npc'.$version;
our $parallel_retries = $path.'parallel_retries'.$version;
our $pind = $path.'pind'.$version;
our $randtest = $path.'randtest'.$version;
our $runrecord = $path.'runrecord'.$version;
our $scm = $path.'scm'.$version;
our $sse = $path.'sse'.$version;
our $sumo = $path.'sumo'.$version;
our $update_inits = $path.'update_inits'.$version;
our $vpc = $path.'vpc'.$version;
our $xv_scm = $path.'xv_scm'.$version;
our $sir = $path.'sir'.$version;


#uncomment for MAC, doris, n41, (5.10.0, 5.8.8, 5.10.1)
#sub done_testing{
#	1;
#}

1;
