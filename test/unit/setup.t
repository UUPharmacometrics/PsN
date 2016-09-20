#!/usr/bin/perl
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use PsN (); #as in setup.pl
use Config;


my $dirsep = '/';
my $internal_libdir = $PsN::lib_dir;
my $is_windows=0;
if ($Config{osname} eq 'MSWin32'){
	$dirsep = "\\";
	$internal_libdir =~s/\//\\/g;
	$is_windows=1;
}

my $default_installation = PsN::get_default_psn_installation_info();


is($default_installation->{'config_file'},$PsN::config_file,'default version config file '.$PsN::config_file); 
is((-d $default_installation->{'lib_dir'}),1,'default version lib dir exists: '.$default_installation->{'lib_dir'});
is($default_installation->{'lib_dir'},$PsN::lib_dir,'default version lib dir '.$PsN::lib_dir);
is((defined $default_installation->{'base_lib_dir'}),1,'default version base_lib_dir is defined: '.$default_installation->{'base_lib_dir'});
is((length($default_installation->{'base_lib_dir'})>0),1,'default version base_lib_dir positive length: '.$default_installation->{'base_lib_dir'});
is((-d $default_installation->{'base_lib_dir'}),1,'default version base_lib_dir exists: '.$default_installation->{'base_lib_dir'});
is((index($internal_libdir,$default_installation->{'base_lib_dir'})==0 ),1,'default version base_lib_dir: '.$default_installation->{'base_lib_dir'}.' is substring of: '.$internal_libdir);



is((defined $default_installation->{'bin_dir'} and (length($default_installation->{'bin_dir'})>0)),1,'default version bin dir '.$default_installation->{'bin_dir'});
is((-d $default_installation->{'bin_dir'}),1,'default version bin_dir exists: '.$default_installation->{'base_lib_dir'});
is($default_installation->{'version'},$PsN::version,'default version number '.$PsN::version);

my $new_defaults = PsN::get_new_installation_defaults('10.2.4',$default_installation);
is($new_defaults->{'old_config_file'},$PsN::config_file,'get_new_installation_defaults: old config file is '.$PsN::config_file); 
is($new_defaults->{'lib_dir'},$default_installation->{'base_lib_dir'}.$dirsep.'PsN_10_2_4','get_new_installation_defaults: new lib dir '.$new_defaults->{'lib_dir'}); 
is($new_defaults->{'base_lib_dir'},$default_installation->{'base_lib_dir'},'get_new_installation_defaults: new base lib dir '.$new_defaults->{'base_lib_dir'});
is($new_defaults->{'bin_dir'},$default_installation->{'bin_dir'},'get_new_installation_defaults: new bin dir '.$new_defaults->{'bin_dir'});
is($new_defaults->{'old_default_version'},$PsN::version,'get_new_installation_defaults: old default version number '.$PsN::version);

my $undefined;
my $older_install = {'config_file'=> $undefined,'lib_dir' => $undefined, 'base_lib_dir' => $undefined,
					 'bin_dir' =>$undefined, 'version' =>$undefined};
$new_defaults = PsN::get_new_installation_defaults('10.2.4',$older_install);
is($new_defaults->{'old_config_file'},undef,'old config file is undef');
is($new_defaults->{'lib_dir'},$Config{sitelib}.$dirsep.'PsN_10_2_4','novel lib dir '.$new_defaults->{'lib_dir'});
is($new_defaults->{'base_lib_dir'},$Config{sitelib},'novel base lib dir '.$new_defaults->{'base_lib_dir'});

if ($is_windows){
	is($new_defaults->{'bin_dir'},$Config{bin},'novel bin dir '.$new_defaults->{'bin_dir'});
}else{
	is($new_defaults->{'bin_dir'},'/usr/local/bin','novel bin dir '.$new_defaults->{'bin_dir'});
}
is($new_defaults->{'old_default_version'},undef,'old default version number undef');

done_testing();
