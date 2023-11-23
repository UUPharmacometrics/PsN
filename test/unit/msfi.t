#!/usr/bin/perl

# Unit tests for data.pm
#

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;
use model::problem::msfi;
use model::problem;
use model;
use File::Copy 'cp';
use Cwd;
use OSspecific;
use File::Spec qw(catfile);

my $tempdir = create_test_dir('unit_msfi');
#chdir($tempdir);
#need OSspecific to make tests platform independend
my $msfirec = model::problem::msfi->new(record_arr => ['$MSFI "'.$tempdir.'subdir space/file.csv" ']);
my ($dir,$file)=OSspecific::absolute_path($tempdir.'subdir space','file');
is($msfirec ->get_directory,$dir,'msfi record dir double quotes space');
is($msfirec ->get_filename,'file.csv','msfi record filename double quotes space');

my $dirsep='/';
if( $dir =~ /\\$/ ){ 
	#windows
	$dirsep="\\";
}

$msfirec = model::problem::msfi->new(record_arr => ['$MSFI "'.$tempdir.'subdir space'.$dirsep.'sub space'.$dirsep.'..'.$dirsep.'file.csv" ']);
($dir,$file)=OSspecific::absolute_path($tempdir.'subdir space','file');
is($msfirec ->get_directory,$dir,'msfi record dir double quotes space up down');
is($msfirec ->get_filename,'file.csv','msfi record filename double quotes space');

is($msfirec->format_filename(write_directory=>$tempdir.'other space',
							 relative_msfi_path=>1),'"..'.$dirsep.'subdir space'.$dirsep.'file.csv"','format_filname space relative up then down');
is($msfirec->format_filename(write_directory=>$tempdir,
							 relative_msfi_path=>1),'"subdir space'.$dirsep.'file.csv"','format_filname relative down');

$msfirec = model::problem::msfi->new(record_arr => ['$MSFI "file.csv" '], model_directory => $tempdir);
is($msfirec->format_filename(write_directory=>$tempdir.'subdir',
							 relative_msfi_path=>1),'..'.$dirsep.'file.csv','format_filname '.$msfirec->format_filename(write_directory=>$tempdir.'subdir',
																														relative_msfi_path=>1));


$msfirec = model::problem::msfi->new(record_arr => ["\$MSFI '".$tempdir."subdir space".$dirsep."file.csv' "]);
($dir,$file)=OSspecific::absolute_path($tempdir.'subdir space','file');
is($msfirec ->get_directory,$dir,'msfi record dir single quotes space');
is($msfirec ->get_filename,'file.csv','msfi record filename single quotes space');

$msfirec = model::problem::msfi->new(record_arr => [" '".$tempdir."subdir space".$dirsep."file.csv' "]);
($dir,$file)=OSspecific::absolute_path($tempdir.'subdir space','file');
is($msfirec ->get_directory,$dir,'msfi record dir single quotes space no recname');
is($msfirec ->get_filename,'file.csv','msfi record filename single quotes space no recname');

$msfirec = model::problem::msfi->new(record_arr => [$tempdir.'subdir/file.csv ']);
($dir,$file)=OSspecific::absolute_path($tempdir.'subdir','file');
is($msfirec ->get_directory,$dir,'msfi record dir');
is($msfirec ->get_filename,'file.csv','msfi record filename');

is($msfirec->format_filename(write_directory=>$tempdir.'subdir',
							 relative_msfi_path=>1),'file.csv','format_filname relative local');
is($msfirec->format_filename(write_directory=>$tempdir,
							 relative_msfi_path=>1),File::Spec->catfile('subdir','file.csv'),'format_filname relative down');
is($msfirec->format_filename(write_directory=>$tempdir.'subdir/subdir2',
							 relative_msfi_path=>1),File::Spec->catfile('..','file.csv'),'format_filname relative up');
is($msfirec->format_filename(write_directory=>$tempdir.'subdir3',
							 relative_msfi_path=>1),File::Spec->catfile('..','subdir','file.csv'),'format_filname relative up then down');

{
    local $SIG{__WARN__} = sub {
        # disallow warnings to ignore the potential "too long path" warnings
    };
is($msfirec->format_filename(write_directory=>$tempdir.'subdir',
							 relative_msfi_path=>0),File::Spec->catfile($tempdir.'subdir','file.csv'),'format_filname abs local');
is($msfirec->format_filename(write_directory=>$tempdir,
							 relative_msfi_path=>0),File::Spec->catfile($tempdir.'subdir','file.csv'),'format_filname abs up');
is($msfirec->format_filename(write_directory=>$tempdir.'subdir/subdir2',
							 relative_msfi_path=>0),File::Spec->catfile($tempdir.'subdir','file.csv'),'format_filname abs down');
is($msfirec->format_filename(write_directory=>$tempdir.'subdir3',
							 relative_msfi_path=>0),File::Spec->catfile($tempdir.'subdir','file.csv'),'format_filname abs up-down');
}

my ($homedir,$dirt) = OSspecific::absolute_path(getcwd(),'file');
$msfirec->set_filename(filename=> 'new.csv');
is(File::Spec->canonpath($msfirec->get_directory), File::Spec->canonpath($homedir), 'msfi record dir after change');
is($msfirec ->get_filename,'new.csv','msfi record filename after change');
($dir,$dirt) = OSspecific::absolute_path($homedir.'sub','file');
$msfirec->set_filename(filename=> $homedir.'sub/new2.csv');
is($msfirec ->get_directory,$dir,'msfi record dir after change 2');
is($msfirec ->get_filename,'new2.csv','msfi record filename after change 2');
($dir,$dirt) = OSspecific::absolute_path($homedir.'other','file');
$msfirec->set_filename(filename=> $homedir.'sub/../other/new3.csv');
is($msfirec ->get_directory,$dir,'msfi record dir after change 3');
is($msfirec ->get_filename,'new3.csv','msfi record filename after change 3');

$msfirec = model::problem::msfi->new(record_arr => ['$MSFI "file.csv" '], model_directory => $tempdir);
is($msfirec ->get_msfo_from_problem_number,0,'msfi msfo probnum 0');
is($msfirec ->format_filename(write_directory => $tempdir.$dirsep.'subdir'),'..'.$dirsep.'file.csv','msfi msfo format_filename path');
#internal not empty
$msfirec = model::problem::msfi->new(record_arr => ['$MSFI "file.csv" '], model_directory => $tempdir,internal_msfo_files=>{'file.csv' => 1});
is($msfirec ->get_msfo_from_problem_number,1,'msfi msfo probnum 1');
is($msfirec ->format_filename(write_directory => $tempdir.$dirsep.'subdir'),'file.csv','msfi msfo format_filename internal');

my $modeldir = $includes::testfiledir;
my $model = model->new(filename => "$modeldir/twoprobmsf_match.mod", ignore_missing_data =>1);
is_deeply($model->problems->[0]->get_msfo_filenames,['msfb1'],'MSFO filenames 1 a');
is_deeply($model->problems->[1]->get_msfo_filenames,[],'MSFO filenames 1b');
is($model->problems->[1]->msfis->[0]->format_filename(write_directory => $modeldir.$dirsep.'subdir'),'msfb1','format_filename msfo match');

$model = model->new(filename => "$modeldir/twoprobmsf_mismatch.mod", ignore_missing_data =>1);
is_deeply($model->problems->[0]->get_msfo_filenames,['msfb1'],'MSFO filenames 2 a');
is_deeply($model->problems->[1]->get_msfo_filenames,[],'MSFO filenames 2b');
is($model->problems->[1]->msfis->[0]->format_filename(write_directory => $modeldir.$dirsep.'subdir'),'..'.$dirsep.'msfb2','format_filename msfo mismatch');

is_deeply(model::problem::msfi::get_additional_msfo_files(msfname => 'pheno.run1.msf'),
		  ['pheno.run1_ETAS.msf','pheno.run1_RMAT.msf','pheno.run1_SMAT.msf'],'get additional msfo files new nonmem two dots');

is_deeply(model::problem::msfi::get_additional_msfo_files(msfname => 'pheno.msf'),
		  ['pheno_ETAS.msf','pheno_RMAT.msf','pheno_SMAT.msf'],'get additional msfo files new nonmem one dot');

is_deeply(model::problem::msfi::get_additional_msfo_files(msfname => 'phenomsf'),
		  ['phenomsf_ETAS','phenomsf_RMAT','phenomsf_SMAT'],'get additional msfo files new nonmem no dot');

my ($base,$type,$extension) = model::problem::msfi::get_basename_msftype_extension(filename => 'phenomsf');
is_deeply([$base,$type,$extension],['phenomsf','',''],'get_basename_msftype_extension 1');

($base,$type,$extension) = model::problem::msfi::get_basename_msftype_extension(filename => 'pheno.run1_RMAT.msf');
is_deeply([$base,$type,$extension],['pheno.run1','_RMAT','.msf'],'get_basename_msftype_extension 2');

($base,$type,$extension) = model::problem::msfi::get_basename_msftype_extension(filename => 'phenomsf_SMAT');
is_deeply([$base,$type,$extension],['phenomsf','_SMAT',''],'get_basename_msftype_extension 3');

($base,$type,$extension) = model::problem::msfi::get_basename_msftype_extension(filename => 'pheno_ETAS.msf');
is_deeply([$base,$type,$extension],['pheno','_ETAS','.msf'],'get_basename_msftype_extension 4');

($base,$type,$extension) = model::problem::msfi::get_basename_msftype_extension(filename => 'run1.mod');
is_deeply([$base,$type,$extension],['run1','','.mod'],'get_basename_msftype_extension 5');


remove_test_dir($tempdir);
done_testing();
