use strict;
use Config;
use File::Spec;
use File::Copy qw(cp);
use File::Path qw(mkpath);
use File::Glob;
use lib 'lib';
use Config::Tiny;
use File::HomeDir;
use PsN (); #pass empty list so that import, which reads config file, is not called
use File::Copy::Recursive qw(fcopy rcopy dircopy fmove rmove dirmove);


my $version = $PsN::version;

my $default_user_name;
my $default_sitelib;
my $default_bin;
my $default_perlpath;
my $use_user_name;
my $is_root = 0;
my $uid;
my $gid;
my $directory_separator;
my $is_win7 = 0;
my $is_Vista = 0;
my $binary_dir;
my $library_dir;
my $perl_binary;
my $old_psn_config_file;
my $old_default_psn_config_file;
my $copy_cmd;
my $copy_recursive_cmd;
my $relative_lib_path=0;
my $auto = 0;


if (scalar(@ARGV)>0){
    if ($ARGV[0] eq 'relative'){
        $relative_lib_path=1;
        print "\nUsing relative library path, suitable for portable installations\n";
    }
    for my $arg (@ARGV) {
        if ($arg eq '-auto') {
            $auto = 1;
        }
    }
}

setup_globals();

if (running_on_windows()) {
    get_windows_version();
}

#NEW
my $default_installation = PsN::get_default_psn_installation_info();
my $new_defaults = PsN::get_new_installation_defaults($version,$default_installation);

$default_sitelib = $new_defaults->{'base_lib_dir'};
$default_bin = $new_defaults->{'bin_dir'};
$default_perlpath = $Config{perlpath};
my $old_default_psn_config_file = $new_defaults->{'old_config_file'};
my $old_default_psn_version = $new_defaults->{'version'};

if (running_on_windows()) {
    #"trying to find the perl binary via system command\n";
    my $wherebin;
    if ($is_win7 or $is_Vista) {
        $wherebin = `where perl`;
    }else{
        my $command = 'for %i in (perl.exe) do @echo. %~$PATH:i';
        $wherebin=`$command`;
    }
    chomp $wherebin;
    my $local_perlpath = $wherebin;
    my $local_sitelib = $wherebin;
    $local_sitelib =~ s/bin\\perl\.exe$/site\\lib/;
    my $local_bin = $wherebin;
    $local_bin =~ s/perl\.exe$//;

    if ((defined $default_sitelib) and (-e $default_sitelib) and (-d $default_sitelib)) {
        #default-sitelib ok
        1;
    }elsif(length($local_sitelib) > 0 and -d $local_sitelib) {
        $default_sitelib = $local_sitelib;
    } else {
        $default_sitelib = undef;
    }
    if ((defined $default_bin) and (-e $default_bin) and (-d $default_bin)) {
        #default  ok
        1;
    }elsif(length($local_bin)>0 and -d $local_bin) {
        $default_bin = $local_bin;
    }else{
        $default_bin = undef;
    }
    if ((defined $default_perlpath) and (-e $default_perlpath) and (-x $default_perlpath) and (not -d $default_perlpath)) {
        #default ok
        1;
    }elsif(length($local_perlpath)>0 and -x $local_perlpath and (not -d $local_perlpath)) {
        $default_perlpath = $local_perlpath;
    }else{
        $default_perlpath = undef;
    }
}
unless (defined $default_perlpath and defined $default_bin and defined $default_sitelib) {
    print "\nWarning: There is something unusual with your Perl installation and configuration.\n".
        "Will try to install anyway, but there may be problems.\n";
}

my $name_safe_version = $version;
$name_safe_version =~ s/\./_/g;
my @utilities = (
    'bootstrap', 'cdd', 'execute', 'llp', 'scm', 'sumo', 'sse',
    'update_inits', 'update', 'npc', 'vpc',
    'pind','nonpb','extended_grid','psn','psn_options','psn_clean',
    'runrecord','mcmp','lasso','mimp','xv_scm','parallel_retries',
    'boot_scm', 'gls', 'simeval', 'frem', 'randtest', 'linearize', 'crossval', 'pvar', 'nca', 'proseval', 'sir', 'rawresults',
    'precond', 'covmat', 'nmoutput2so', 'benchmark', 'npfit', 'resmod', 'cddsimeval', 'qa', 'transform', 'boot_randtest',
    'monitor', 'scmplus', 'scmreport',
    'm1find', 'pack'
    );

my @win_modules = ('File::Copy::Recursive', 'Capture::Tiny', 'Config::Tiny', 'Math::Random', 'Math::MatrixReal', 'Mouse', 'MouseX::Params::Validate', 'YAML');
my @nix_modules = ('File::Copy::Recursive', 'Capture::Tiny', 'Config::Tiny', 'Math::Random', 'Math::MatrixReal', 'Mouse', 'MouseX::Params::Validate', 'YAML');
my @recommended_modules = ('Archive::Zip');

my @modules;

sub get_windows_version
{
    if (eval('require Win32')) {
        my $winver = Win32::GetOSName();
        $is_win7 = 1 if ($winver eq 'Win7');
        $is_Vista = 1 if ($winver eq 'WinVista');
    }
}

sub running_on_windows
{
    return $Config{osname} eq 'MSWin32';
}

sub setup_globals
{
    $| = 1; # Turn on autoflush

    if (running_on_windows()) {
        $directory_separator = "\\";
    } else {
        $directory_separator = "/";
    }
}

sub confirm
{
    while (1) {
        my $input = <STDIN>;
        if ($input =~ /^\s*[yY]\s*$/) {
            return 1;
        } elsif ($input =~ /^\s*[nN]\s*$/) {
            return 0;
        } else {
            print "Please answer y for yes or n for no: ";
        }
    }
}

sub numeric_input
{
    my $max = shift;

    while (1) {
        my $input = <STDIN>;
        if ($input =~ /^\s*(\d+)\s*$/) {
            if (1 <= $1 and $1 <= $max) {
                return $1;
            }
        }
        print "Please answer a number from 1 to $max: ";
    }
}

sub get_default_nm_versions
{
    my %versionhash;
    if (running_on_windows()) {
        my $home = $ENV{USERPROFILE};
        my @dirs = <C:/nmvi* C:/NONMEM/nmvi* $home/nmvi* $home/NONMEM/nmvi*>;
        foreach my $dir (@dirs){
            $versionhash{$dir}=6 if ((-d $dir) and (
                                         ((-d $dir.'\run') and (-x $dir.'\run\nmfe6.bat')) or
                                         ((-d $dir.'\util') and (-x $dir.'\util\nmfe6.bat'))
                                     ));
        }
        @dirs = <C:/nm7* C:/nm_7* C:/NONMEM/nm7* C:/NONMEM/nm_7*>;
        foreach my $dir (@dirs){
            if ((-d $dir) and ((-d $dir.'\run') or (-d $dir.'\util'))){
                my $version;
                if ((-x $dir.'\run\nmfe72.bat') or (-x $dir.'\util\nmfe72.bat')){
                    $version='7.2';
                }elsif ((-x $dir.'\run\nmfe73.bat') or (-x $dir.'\util\nmfe73.bat')){
                    $version='7.3';
                }elsif ((-x $dir.'\run\nmfe74.bat') or (-x $dir.'\util\nmfe74.bat')){
                    $version='7.4';
                }elsif ((-x $dir.'\run\nmfe75.bat') or (-x $dir.'\util\nmfe75.bat')){
                    $version='7.5';
                }elsif ((-x $dir.'\run\nmfe76.bat') or (-x $dir.'\util\nmfe76.bat')){
                    $version='7.6';
                }elsif ((-x $dir.'\run\nmfe7.bat') or (-x $dir.'\util\nmfe7.bat')){
                    $version='7.1';
                }
                $versionhash{$dir}=$version if (defined $version);
            }
        }
    }else{
        my $home = home();
        my @dirs = </opt/nmvi* /opt/NONMEM/nmvi* $home/nmvi* $home/NONMEM/nmvi*>;
        foreach my $dir (@dirs){
            $versionhash{$dir}=6 if ((-d $dir) and (
                                         ((-d $dir.'/run') and (-x $dir.'/run/nmfe6')) or
                                         ((-d $dir.'/util') and (-x $dir.'/util/nmfe6'))
                                     ));
        }
        @dirs = </opt/nm7* /opt/nm_7* /opt/NONMEM/nm7* /opt/NONMEM/nm_7* /opt/nonmem/nm7* /opt/nonmem/nm_7*>;
        push(@dirs,<$home/nm7* $home/nm_7* $home/NONMEM/nm7* $home/NONMEM/nm_7* $home/nonmem/nm7* $home/nonmem/nm_7*>);
        foreach my $dir (@dirs){
            if ((-d $dir) and ((-d $dir.'/run') or (-d $dir.'/util'))){
                my $version;
                if ((-x $dir.'/run/nmfe72') or (-x $dir.'/util/nmfe72')){
                    $version='7.2';
                }elsif ((-x $dir.'/run/nmfe73') or (-x $dir.'/util/nmfe73')){
                    $version='7.3';
                }elsif ((-x $dir.'/run/nmfe74') or (-x $dir.'/util/nmfe74')){
                    $version='7.4';
                }elsif ((-x $dir.'/run/nmfe75') or (-x $dir.'/util/nmfe75')){
                    $version='7.5';
                }elsif ((-x $dir.'/run/nmfe76') or (-x $dir.'/util/nmfe76')){
                    $version='7.6';
                }elsif ((-x $dir.'/run/nmfe7') or (-x $dir.'/util/nmfe7')){
                    $version='7.1';
                }
                $versionhash{$dir}=$version if (defined $version);
            }
        }
    }

    return \%versionhash;
}

sub get_nm_version
{
    print "Enter the *complete* path of the NM-installation directory: ";
    my $dir = <STDIN>;
    chomp ($dir);
    unless( -e $dir ){
        print "Directory $dir does not exist. Please try again\n";
        return '0','0';
    }
    if (running_on_windows()) {
        if ($dir =~ /^[A-Za-z]:\\/){
            1;
        }elsif ($dir =~ /^%USERPROFILE%/){
            my $home = $ENV{USERPROFILE};
            if (defined $home){
                $dir =~ s/^%USERPROFILE%/$home/;
            }else{
                print "$dir does not look like a full search path including drive letter. Please try again.\n";
            }
        }else{
            print "$dir does not look like a full search path including drive letter. Please try again.\n";
            return '0','0';
        }
    }else{
        if ($dir =~ /^\//){
            1;
        }elsif ($dir =~ /^~/){
            my $home=home();
            $dir =~ s/^~/$home/;
        }else{
            print "$dir does not look like a full search path (does not begin with /). Please try again.\n";
            return '0','0';
        }
    }

    my $version;
    if (running_on_windows()) {
        $version=6 if ((-d $dir) and (
                           ((-d $dir.'\run') and (-x $dir.'\run\nmfe6.bat')) or
                           ((-d $dir.'\util') and (-x $dir.'\util\nmfe6.bat'))
                       ));
        if ((-d $dir) and ((-d $dir.'\run') or (-d $dir.'\util'))){
            if ((-x $dir.'\run\nmfe72.bat') or (-x $dir.'\util\nmfe72.bat')){
                $version='7.2';
            }elsif ((-x $dir.'\run\nmfe73.bat') or (-x $dir.'\util\nmfe73.bat')){
                $version='7.3';
            }elsif ((-x $dir.'\run\nmfe74.bat') or (-x $dir.'\util\nmfe74.bat')){
                $version='7.4';
            }elsif ((-x $dir.'\run\nmfe75.bat') or (-x $dir.'\util\nmfe75.bat')){
                $version='7.5';
            }elsif ((-x $dir.'\run\nmfe76.bat') or (-x $dir.'\util\nmfe76.bat')){
                $version='7.6';
            }elsif ((-x $dir.'\run\nmfe7.bat') or (-x $dir.'\util\nmfe7.bat')){
                $version='7.1';
            }
        }
    }else{
        $version=6 if ((-d $dir) and (
                           ((-d $dir.'/run') and (-x $dir.'/run/nmfe6')) or
                           ((-d $dir.'/util') and (-x $dir.'/util/nmfe6'))
                       ));
        if ((-d $dir) and ((-d $dir.'/run') or (-d $dir.'/util'))){
            if ((-x $dir.'/run/nmfe72') or (-x $dir.'/util/nmfe72')){
                $version='7.2';
            }elsif ((-x $dir.'/run/nmfe73') or (-x $dir.'/util/nmfe73')){
                $version='7.3';
            }elsif ((-x $dir.'/run/nmfe74') or (-x $dir.'/util/nmfe74')){
                $version='7.4';
            }elsif ((-x $dir.'/run/nmfe75') or (-x $dir.'/util/nmfe75')){
                $version='7.5';
            }elsif ((-x $dir.'/run/nmfe76') or (-x $dir.'/util/nmfe76')){
                $version='7.6';
            }elsif ((-x $dir.'/run/nmfe7') or (-x $dir.'/util/nmfe7')){
                $version='7.1';
            }
        }
    }

    unless (defined $version) {
        print "Did not find an nmfe script in the util or run or . subdirectory of $dir. Please try again\n";
        return '0','0';
    }
    return $dir,$version;
}

sub get_psnname
{
    my $path = shift;
    my $default = shift;
    print "Enter the name you want to use for the NM-version in\n$path\n".
        "with PsN option -nm_version,\nor press ENTER to use the name $default : ";
    my $psnname = <STDIN>;
    chomp ($psnname);
    unless (length($psnname)>0){
        $psnname=$default;
    }
    return $psnname;
}

sub copy_and_modify_bin_files
{
    my $file = shift;
    my $binary_dir = shift;

    my $includepath;
    my @includelines=();
    if ($relative_lib_path){
        #from binary_dir to library
        my $path = File::Spec->abs2rel($library_dir,$binary_dir);
        #must have forward slash also on windows, otherwise slash will disappear
        #when quoting path to get value of $Bin
        if ($path eq '.'){
            $includepath = "PsN_$name_safe_version";
        }else{

            $includepath = File::Spec->catfile($path,"PsN_$name_safe_version");
        }
        @includelines=('use FindBin qw($Bin);','use lib "$Bin/".'."'$includepath';");
    }else{
        $includepath = File::Spec->catfile($library_dir,"PsN_$name_safe_version");
        #need empty line to keep correct number
        @includelines=("use lib '$includepath';",'');
    }

    unless (open(INST, "<", File::Spec->catfile("bin", $file))) {
        abort("Could not open " . File::Spec->catfile("bin", $file) . " for reading, unable to install $file: $!\n");
    }
    unless (open(UTIL, ">", File::Spec->catfile($binary_dir, "$file-$version"))) {
        abort("Could not open " . File::Spec->catfile($binary_dir, "$file-$version") . " for writing, unable to install $file: $!\n");
    }
    my $replace_line_found = 0;
    while (<INST>) {
        if (/\# Everything above this line will be replaced \#/) {
            print UTIL "\#!", $perl_binary, "\n";
            print UTIL join("\n",@includelines)."\n";
            print UTIL "\# Everything above this line was entered by the setup script \#\n";
            $replace_line_found = 1;
        } elsif ($replace_line_found) {
            print UTIL $_;
        }
    }
    close UTIL;
    close INST;

    chmod(0755, File::Spec->catfile($binary_dir, "$file-$version"));
}

sub create_conf
{
    my $confpath = shift;
    my $perlbin = shift;
    my $template = $confpath.'psn.conf_template';
    my $newconf = $confpath.'psn.conf';

    unless (-e $template){
        print "Error: Could not find $template.\n"."No config file created\n";
        return 0;
    }
    my $config = Config::Tiny -> read( $template );
    ## this is not used anywhere $config -> {'_'} -> {'perl'} = $perlbin;


    #fix R
    my @paths;
    my $file;
    if (running_on_windows()) {
        my $path = $ENV{PATH};
        @paths= split(';',$path);
        $path = $ENV{Path};
        push(@paths,split(';',$path));
        push(@paths,'C:\R\bin\R');
        $file='\R.exe';
    }else{
        my $path = $ENV{PATH};
        @paths= split(':',$path);
        $file='/R';
    }
    foreach my $alt (@paths){
        if (-e $alt.$file){
            if (running_on_windows()) {
                $alt =~ s/Program Files/Progra~1/;
            }
            $config -> {'_'} -> {'R'} = $alt.$file;
            last;
        }
    }

    #fix nm

    my %nm_versions;

    print "\nSearching for NM versions on your file system...\n";
    my $ref = get_default_nm_versions();
    if ( defined $ref ) {
        %nm_versions=%{$ref};
    }

    while (1){
        if (%nm_versions){
            print "These are the loaded NM versions:\n";
            foreach my $key (keys %nm_versions){
                print "$key\n";
            }
            print"Would you like to add another one [y/n] ?\n";
            last unless( confirm() );
        }else{
            print "No NM versions have been found. You need to add at least one.\n";
        }

        my ($path,$version_label) = get_nm_version();
        unless ($path eq '0'){
            $nm_versions{$path}=$version_label;
        }
    }
    if (%nm_versions){
        my %psn_names;
        foreach my $version (keys %nm_versions){
            my @dirarr = File::Spec->splitdir( $version);
            my $default= pop(@dirarr);
            while (defined $psn_names{$default}){
                $default = pop(@dirarr).'_'.$default;
            }
            my $name = get_psnname($version,$default);
            $psn_names{$name}=$version;
        }

        if (scalar(keys %psn_names)==1){
            my @keys = keys %psn_names;
            $psn_names{'default'} = $psn_names{$keys[0]};
        }

        while (not defined $psn_names{'default'}){
            print "These are the defined psn-names:\n";
            print (join('  ',(keys %psn_names))."\n");
            print "Enter the psn-name of the NM version that should be the default : ";
            my $name = <STDIN>;
            chomp ($name);
            if (defined $psn_names{$name}){
                $psn_names{'default'} = $psn_names{$name};
            }else{
                print "No NM version with psn-name $name is defined.\n";
            }
        }
        foreach my $name (keys %psn_names){
            my $version_label = $nm_versions{$psn_names{$name}};
            $config -> {'nm_versions'} -> { $name } = $psn_names{$name}.','.$version_label;
        }

    } else {
        print "\nError: No NM versions defined. You must add at least one manually to $newconf, otherwise PsN will not run.";
    }

    $config->write($newconf);

    print "\n\nA new configuration file has been created:\n$newconf\n";
    print "If you want to add cluster options or personalized defaults or\n".
        "NMqual settings you can do this manually by editing\n"."$newconf\n".
        "in a regular text editor.\n";
    return 1;
}

sub abort
{
    my $message = shift;

    print $message;
    print "\n\nThe installation process has been aborted, PsN was not installed.";
    print "\n\nPress ENTER to exit the installation program.\n";
    my $dirt = <STDIN>;
    exit;
}

sub get_input
{
    my $default = shift;
    if ($auto) {
        return $default;
    }
    my $input = <STDIN>;
    chomp($input);
    if ($input =~ /^\s*$/) {
        if (defined $default) {
            return($default);
        } else {
            abort("No input given and no default available, must exit.\n");
        }
    } else {
        return($input);
    }
}

sub run_r
{
    my $line = shift;
    system("Rscript -e \"$line\"");
}

sub create_directory
{
    my $directory_name = shift;

    unless (-e $directory_name) {
        print "Directory $directory_name does not exist. Would you like to create it?[y/n]\n";
        if ($auto or confirm()) {
            if (not mkpath($directory_name)) {
                abort("Unable to create $directory_name.\n");
            }
        } else {
            abort();
        }
    }
}

sub test_perl_modules
{
    print "\nTesting required modules:\n";
    foreach my $module (@modules) {
        my $ok = 0;
        $ok = 1 if eval("require " . $module);
        if ($ok) {
            print "Module $module ok\n";
        } else {
            print "Perl module $module is missing, you must install it before running PsN.\n";
        }
    }
    print "\nDone testing required modules.\n";
    print "\nTesting recommended but not required modules...\n";
    foreach my $module (@recommended_modules){
        my $ok = 0;
        $ok = 1 if eval("require " . $module);
        if ($ok) {
            print "Module $module ok\n";
        } else {
            print "Perl module $module is missing, you can run PsN without it but some features will be disabled.\n";
        }
    }

    print "Tests done.\n\n";
    print "Continue installing PsN (installing is possible even if modules are missing)[y/n]?";
    abort() unless (confirm());
}

sub warn_about_local_configuration
{
    if (-e home() . "/psn.conf") {
        print "\nPlease note that for you personally the configuration file you\n".
        "already have in ".home()."$directory_separator"."psn.conf\n".
        "will override the settings in\n".
        "$library_dir"."$directory_separator"."PsN_$name_safe_version"."$directory_separator".
        "psn.conf\n";
    } elsif (running_on_windows()) {
        print "\nPlease note that if you have a psn.conf file in your Desktop folder,\n".
        "the settings in that file will override the settings in\n".
        "$library_dir"."$directory_separator"."PsN_$name_safe_version"."$directory_separator".
        "psn.conf\n";
    } else {
        print "\nPlease note that if you have a psn.conf file in your home directory,\n".
        "the settings in that file will override the settings in\n".
        "$library_dir" . "$directory_separator" . "PsN_$name_safe_version" . "$directory_separator" . "psn.conf\n";
    }
}

sub copy_file
{
    my $source = shift;
    my $dest = shift;

    unless (fcopy($source, $dest)) {
        abort("Could not copy $source to $dest : $!\n");
    }
}

sub create_bat_file
{
    my $name = shift;

    my $bat_file;

    unless (open $bat_file, ">", $name) {
        abort("Could not open $name: $!\n");
    }

    print $bat_file '@echo off' . "\n" .'perl %~dp0%~n0.pl %*' . "\n";

    close $bat_file;
}

sub get_psn_version
{
    my $version = shift;

    $version =~ /(\d+)\.(\d+)\.(\d+)/;
    return ($1, $2, $3);
}

# Update from the old comma separated format of nmfe_options
sub update_conf_file
{
    my $conf_file = shift;
    my $old_version = shift;

    (my $major, my $minor, my $maint) = get_psn_version($old_version);
    unless ($major > 4 or ($major == 4 and $minor > 1) or ($major == 4 and $minor == 1 and $maint > 3)) {
        open my $old_conf, "<", $conf_file;
        open my $new_conf, ">", "$conf_file.new";
        while (<$old_conf>) {
            if (/nmfe_options=(.*)/) {
                my @arr = split /,/, $1;
                my $str = "nmfe_options=";
                foreach my $opt (@arr) {
                    $str .= "-$opt ";
                }
                $str .= "\n";
                print $new_conf $str;
            } else {
                print $new_conf $_;
            }
        }
        close $old_conf;
        close $new_conf;
        unlink $conf_file;
        rename "$conf_file.new", $conf_file;
    }
}

print "\nThis is the PsN installer. I will install PsN version $version.\n".
    "You need to answer a few questions. If a default value is presented\n".
    "you may accept it by pressing ENTER.\n\n";

if ($Config{osname} eq 'linux' or $Config{osname} eq 'darwin') {
    my $user = `whoami`;
    chomp($user);
    if ($user =~ /^root$/) {
        $is_root = 1;
        my $home = $ENV{HOME};
        if ($home =~ /\/([^\/]*)$/) {
            $default_user_name = $1;
        }
    } else {
        print "Hi $user, you don't look like root. Please note that you need root privileges to install PsN systemwide.\n";
    }
} else {
    if (not running_on_windows()) {
        print "OS ".$Config{osname}." is not explicitly supported by PsN, but if it is a unix/Mac type\n".
            "system everything should work anyway. Problems are expected only if it is a Windows system.\n".
            "Do you want to proceed with the installation [y/n] ?\n";
        abort() unless(confirm());
    }
}

print "PsN Utilities installation directory [$default_bin]:";
$binary_dir = get_input($default_bin);
create_directory($binary_dir);

print "Path to perl binary used to run Utilities [$default_perlpath]:";
$perl_binary = get_input($default_perlpath);

(my $volume, my $directory, my $file) = File::Spec->splitpath($perl_binary);
my $runperl_name = "runperl.bat";
my $runperl_binary = File::Spec->catpath( $volume, $directory, $runperl_name);


print "PsN Core and Toolkit installation directory [$default_sitelib]:";
$library_dir = get_input($default_sitelib);
create_directory($library_dir);
my $psn_lib_path = File::Spec->catfile($library_dir, "PsN_$name_safe_version");

if (running_on_windows()) {
    $copy_cmd = "copy /Y";
    $copy_recursive_cmd = "xcopy /Y /E";
    @modules = @win_modules;
} else {
    @modules = @nix_modules;
    $copy_cmd = "cp";
    $copy_recursive_cmd = "cp -r";
}

print "\nThe next step is to check Perl module dependencies.
If a module is missing, you must install it, e.g. from CPAN,
http://www.cpan.org/modules/index.html
before PsN can be run.

Would you like this script to check Perl modules [y/n]?";

if (not $auto and confirm()) {
    test_perl_modules();
}

my $overwrite = 0;
my $old_version = 'X_X_X';
if (defined $old_default_psn_version){
    $old_version = $old_default_psn_version;
    $old_version =~ s/\./_/g;
}
my $keep_conf = 0;

if (-d "$library_dir/PsN_$name_safe_version"){
    print "Directory $library_dir/PsN_$name_safe_version already exists.\n";
    print "PsN $version is already (partially) installed. Would you like to continue anyway [y/n] ?";
    abort() unless ($auto or confirm());
}else{
    unless (mkpath("$library_dir/PsN_$name_safe_version")) {
        print "Failed to create $library_dir/PsN_$name_safe_version: $!\n";
        print "Would you like to continue anyway [y/n] ?";
        abort() unless (confirm());
    }
}

if (-e "$library_dir/PsN_$name_safe_version/psn.conf") {
    print "An older version of psn.conf exists in $library_dir/PsN_$name_safe_version.\n";
    print "Keep the old version [y/n] ?";
    if (not $auto) {
        $keep_conf = confirm();
    } else {
        $keep_conf = 1;
    }
}

my $newconf = File::Spec->catfile( $library_dir, "PsN_$name_safe_version", "psn.conf" );
my $thelibdir = File::Spec->catdir($library_dir,"PsN_$name_safe_version");

if ($keep_conf) {
    unless (fcopy($newconf, "old.conf")) {
        abort("Could not copy $newconf to old.conf : $!\n");
    }
}
unless (dircopy("lib", $thelibdir)) {
    abort(" Could not copy contents of lib directory to $thelibdir : $!\n");
}
if ($keep_conf){
    unless (fcopy("old.conf",$newconf)) {
        abort("Could not copy old.conf to $newconf : $!\n");
    }
}

my $confirmed = 0;
foreach my $file (@utilities) {
    copy_and_modify_bin_files($file, $binary_dir);

    my $copy_the_binaries = 0;
    if (-e "$binary_dir/$file") {

        if (not running_on_windows()) {
            my $link = readlink("$binary_dir/execute");
            if ($old_version eq 'X_X_X' and not($link eq '')) {
                $link =~ /-(\d+\.\d+\.\d+)/;
                $old_version = $1;
            }
        } elsif (running_on_windows()) {
            my $link = open(IN, "$binary_dir/execute");
            if ($old_version eq 'X_X_X' and $link) {
                while (<IN>) {
                    if (/^use lib '.*PsN_(.*)';/) {
                        $old_version = $1;
                        $old_version =~ tr/_/./;
                        last;
                    } elsif (/^use PsN_\d+_\d+_\d+/) {        # The old system
                        s/^use PsN\_//;
                        s/\s.*//;
                        s/\_/\./g;
                        s/\;//;
                        $old_version = $_;
                        last;
                    }
                }
                close(IN);
            }
        }

        my $tmp = $old_version;
        $tmp =~ s/\./\_/g;
        if (defined $old_default_psn_config_file and -e $old_default_psn_config_file){
            $old_psn_config_file = $old_default_psn_config_file;
        }else{
            $old_psn_config_file = File::Spec->catfile($library_dir, "PsN_$tmp", "psn.conf");
            unless (-e $old_psn_config_file){
                $old_psn_config_file = undef ;
            }
        }
        if ($old_version eq $version) {
            if (not $confirmed) {
                print("\nThis version ($version) looks like the same or an older installed\n",
                       "version ($old_version) of PsN. Would you like to make\n",
                       "this version ($version) the default? [y/n]");
                $confirmed = 1;
                if (not $auto) {
                    $overwrite = confirm();
                } else {
                    $overwrite = 1;
                }
            }
        }
        if (not $confirmed) {
            print("\nAn older version($old_version) of PsN is installed. Would you like to\n",
                   "make this version ($version) the default? [y/n]");
            $confirmed = 1;
            if (not $auto) {
                $overwrite = confirm();
            } else {
                $overwrite = 1;
            }
        }
        if ($overwrite) {
            unlink("$binary_dir/$file");
            $copy_the_binaries = 1;
        }

    } else {
        $copy_the_binaries = 1;
    }

    if ($copy_the_binaries) {
        if (running_on_windows()) {
            copy_file("$binary_dir\\$file-$version", "$binary_dir\\$file");
            copy_file($runperl_binary, "$binary_dir\\$file.bat");
        } else {
            symlink("$binary_dir/$file-$version", "$binary_dir/$file");
        }
    }

    # Make the versioned script directly executable
    if (running_on_windows()) {
        rename("$binary_dir\\$file-$version", "$binary_dir\\$file-$version.pl");
        create_bat_file("$binary_dir\\$file-$version.bat");

        # Conversion of previously installed scripts
        foreach my $name (glob "$binary_dir\\$file-*") {
            next if $name =~ /(.bat|.pl)$/;
            rename($name, "$name.pl");
            create_bat_file("$name.bat");
        }
    }
}

if (running_on_windows()) {
    require Win32;
    $library_dir = Win32::GetShortPathName($library_dir);
}

my $set_python_lib_path = 0;
my $python_lib_path;
my $set_rlib_path = 0;
my $rlib_path;
my $venv_python;

if (not running_on_windows()) {
    $rlib_path = File::Spec->catfile($psn_lib_path, "Rlib");
    print "\n";
    print "The R package PsNR and its dependencies are needed for the rplots functionality and the qa tool in PsN.\n";
    print "The PsN installer can automatically install these using renv to make sure that all versions\n";
    print "of R packages have been tested together. A separate R library will be created inside the PsN\n";
    print "installation directory. You need to have R installed for this installation.\n";
    print "\n";
    print "\nWould you like to install the PsNR R package? [y/n] ";

    if ($auto or confirm()) {
        if (not -e $rlib_path) {
            if (not mkpath($rlib_path)) {
                print "Failed to create $rlib_path: $!\n";
                abort();
            }
        }

        my $repos = 'https://cloud.r-project.org';
        my $rsafe_path = $rlib_path;
        $rsafe_path =~ s/\\/\\\\/g;
        $ENV{'R_LIBS_SITE'} = $rlib_path;
        $ENV{'R_LIBS_USER'} = $rlib_path;
        run_r("install.packages(c('renv', 'remotes'), lib='$rsafe_path', repos='$repos')");
        run_r("options(renv.consent=TRUE); renv::settings" . '\$' . "use.cache(FALSE); renv::restore(library='$rsafe_path', lockfile='PsNR/renv.lock')");
        run_r("remotes::install_local('PsNR', lib='$rsafe_path', repos=NULL, dependencies=F)");
        $set_rlib_path = 1;
    }

    $python_lib_path = File::Spec->catfile($psn_lib_path, "pyvenv");
    $set_python_lib_path = 0;
    print "\n";
    print "The Python package 'pharmpy' is needed by PsN and you would need to have python installed on your system\n";
    print "If you let the installer install pharmpy it will be installed in a virtual environment together with its dependencies inside the PsN installation\n";
    print "You would need to have python installed for this installation\n";
    print "\n";
    print "\nWould you like to install the pharmpy python package? [y/n] ";

    if ($auto or confirm()) {
        my $py_response = readpipe('python -c "import sys;print(sys.version_info[0])"');
        my $python;
        chomp($py_response);
        if ($py_response eq "3") {
            $python = 'python';
        } else {
            my $py3_response = readpipe('python3 -c "import sys;print(sys.version_info[0])"');
            chomp($py3_response);
            if ($py3_response eq "3") {
                $python = 'python3';
            } else {
                die "No python interpreter in PATH\n";
            }
        }
        system("$python -m venv $python_lib_path");
        if (running_on_windows()) {
            $venv_python = "$python_lib_path\\Scripts\\python";
        } else {
            $venv_python = "$python_lib_path/bin/python";
        }
        my $pharmpy_file = (glob("pharmpy-core*.zip"))[0];
        system("$venv_python -m pip install wheel");
        system("$venv_python -m pip install -r requirements.txt");
        system("$venv_python -m pip install $pharmpy_file --upgrade --no-deps --force-reinstall");
        $set_python_lib_path = 1;
    }
}

my $default_test = $library_dir;
my $test_library_dir;
print "\nWould you like to install the PsN test library? [y/n] ";
if ($auto or confirm()) {
    print "PsN test library installation directory [$default_test]:";
    $test_library_dir = get_input($default_test);
    $test_library_dir .= "/PsN_test_$name_safe_version";

    unless (dircopy("test", $test_library_dir)) {
        abort("Could not copy contents of test directory to $test_library_dir : $!\n");
    }

    # Put library and bin paths into includes.pm
    my $includes_file = "$test_library_dir/includes.pm";
    rename($includes_file, "$includes_file.temp");
    open(my $sh, "<", "$test_library_dir/includes.pm.temp") or abort("Error when copying includes.pm: $!");
    open(my $dh, ">", "$test_library_dir/includes.pm") or abort("Error when copying includes.pm: $!");

    while (<$sh>) {
        if (/^(\s*my \$libpath\s*=\s*)'';/) {
            print $dh "$1'$library_dir/PsN_$name_safe_version';\n";
        } elsif (/^(\s*our \$path\s*=\s*)'';/) {
            print $dh "$1'$binary_dir/';\n";
        } else {
            print $dh $_;
        }
    }

    close $sh;
    close $dh;
    unlink("$includes_file.temp");

    #change perl binary in runsystem
    open(my $sh, "+< $test_library_dir/runsystem")                 or die "Opening: $!";
    my @ARRAY = <$sh>;
    $ARRAY[0] = "\#!".$perl_binary."\n";
    seek($sh,0,0)                        or die "Seeking: $!";
    print $sh @ARRAY                     or die "Printing: $!";
    truncate($sh,tell($sh))               or die "Truncating: $!";
    close($sh)                           or die "Closing: $!";


    print "PsN test library installed successfully in [$test_library_dir].\n";
    print "Please read the 'testing' chapter of the developers_guide.pdf for information on how to run the tests\n\n";
}


my $configuration_done = 0;
$configuration_done = 1 if ($keep_conf);

if (not $keep_conf) {
    my $conf_ok = 0;
    print "\nNow you must edit " . "$library_dir" . "$directory_separator" . "PsN_$name_safe_version" . "$directory_separator".
        "psn.conf\n"."so that PsN can find your NONMEM installations.\n";

    my $offer_help = 1;
    if (defined $old_psn_config_file){
        print "Would you like to copy psn.conf from the previous default version ".
            "to this new installation? [y/n] ";
        my $newf = File::Spec->catfile($library_dir, "PsN_$name_safe_version", "psn.conf");

        if (not $auto and confirm()) {
            fcopy($old_psn_config_file, $newf);
            if (-e $newf) {
                $offer_help = 0;
                $conf_ok = 1;
                $configuration_done = 1;
                print "Copied $old_psn_config_file to $newf.\n";
                update_conf_file($newf, $old_version);
            } else {
                print "Copying of old psn.conf failed.\n";
            }
        }
    }
    if ($offer_help) {
        print "You can get help to create a bare-bones configuration file that will work\n".
            "when running PsN locally. If you are running PsN on a cluster and/or want\n".
            "to set personalized defaults and/or will run PsN with NMQual,\n".
            "you can manually add the relevant options to the file afterwards.\n".
            "Would you like help to create a configuration file? [y/n] ";
        if (not $auto and confirm()) {
            $conf_ok = create_conf ("$library_dir"."$directory_separator"."PsN_$name_safe_version"."$directory_separator",$perl_binary);
            $configuration_done = $conf_ok;
        }
    }
}

warn_about_local_configuration();

if ($configuration_done) {
    print "\nInstallation complete.\n";
} else {
    my $path = "$library_dir" . "$directory_separator" . "PsN_$name_safe_version";
    cp($path . '/psn.conf_template', $path . '/psn.conf');
    print "\nInstallation partially complete. You still have to add NONMEM settings to psn.conf before you can run PsN.\n";
    print "A psn.conf to edit is found in\n";
    print "$path\n";
    print "Detailed instructions are found in psn_configuration.pdf";
}

if (not running_on_windows()) {
    # Set R_LIB_PATH if created
    my $set_python = ($set_python_lib_path and -e $python_lib_path);
    my $set_r = ($set_rlib_path and -e $rlib_path);
    if ($set_r or $set_python) {
        my $tempconf = File::Spec->catfile($psn_lib_path, 'tempconf');
        my $confpath = File::Spec->catfile($psn_lib_path, 'psn.conf');
        open my $sh, '<', $confpath;
        open my $dh, '>', $tempconf;
        if ($set_r) {
            if ($relative_lib_path) {
                $rlib_path = File::Spec->abs2rel($rlib_path, $library_dir);
            }
            print $dh "R_LIB_PATH=$rlib_path\n";
        }
        if ($set_python) {
            if ($relative_lib_path) {
                $python_lib_path = "pyvenv";
            }
            print $dh "PYTHON_PATH=$python_lib_path\n";
        }
        while (<$sh>) {
            if (not (/^R_LIB_PATH=/ and $set_r) and not (/^PYTHON_PATH=/ and $set_python)) {
                print $dh $_;
            }
        }
        close $dh;
        close $sh;
        cp $tempconf, $confpath;
        unlink $tempconf;
    }
}

if (not $auto) {
    print "\n\nPress ENTER to exit the installation program.\n";
    my $dirt = <STDIN>;
}
