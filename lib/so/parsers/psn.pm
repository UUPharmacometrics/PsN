package so::parsers::psn;

# Package for handling psn input to nmoutput2so.

use strict;
use warnings;
use Mouse;
use MouseX::Params::Validate;
use include_modules;
use File::Copy qw/copy mv/;
use Cwd;
use OSspecific;

#<#assign outputFile = (job.executionFileName)?replace("\\.[A-Za-z0-9]{1,3}$", ".lst", "r")>
#<#assign psnlogFile = (job.executionFileName)?replace("\\.[A-Za-z0-9]{1,3}$", ".psn.log", "r")>

sub _get_stem
{
    my %parm = validated_hash(\@_,
        pharmml => { isa => 'Str', optional => 0 },
    );

    my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

    my $stem = $pharmml;
    $stem =~ s/\.[A-Za-z0-9]{1,3}$//;

    return $stem;
}

sub _get_logfile
{
    my %parm = validated_hash(\@_,
        pharmml => { isa => 'Str', optional => 0 },
    );

    my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

    my $stem = _get_stem(pharmml => $pharmml);
    return $stem.'.psn.log';
}

sub _get_lstfile
{
    my %parm = validated_hash(\@_,
        pharmml => { isa => 'Str', optional => 0 },
    );

    my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

    my $stem = _get_stem(pharmml => $pharmml);
    return $stem.'.lst';
}

sub _get_sofile
{
    my %parm = validated_hash(\@_,
        pharmml => { isa => 'Str', optional => 0 },
    );

    my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

    my $stem = _get_stem(pharmml => $pharmml);
    return $stem.'.SO.xml';
}

sub _connector_get_files
{
    my %parm = validated_hash(\@_,
        directory => { isa => 'Str', optional => 0 },
        pharmml => { isa => 'Str', optional => 0 },
    );

    my $directory = $parm{'directory'};
    my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

    my $logfile = _get_logfile(pharmml => $pharmml);
    my $lstfile = _get_lstfile(pharmml => $pharmml);
    my $sofilename = _get_sofile(pharmml => $pharmml);
    my $errorstring;

    my @files=();
    my $tool= _get_toolname(directory => $directory); #can be undef
    my $errorfile='errorMessages';
    my $append_columns;

    if ( (not (-d $directory))  or
         ( (not defined $tool) and (not -e $lstfile)) or
         (defined $tool and ($tool ne 'execute') and ($tool ne 'nca') and (not -e $directory.'/'.$tool.'_results.csv')) or
         ($tool eq 'nca' and (not -e $directory . '/' . 'nca_simulation.1.npctab.dta'))
        ) {
        #generic failure
        #$errorstring = 'run failure';
        copy($logfile,$errorfile);  #skip copying, change design
        @files = ($logfile); #treat error messages as lstfile
    }elsif (($tool eq 'execute') and (not -e $lstfile)) {
        #execute failure
        if (-e $directory.'/NM_run1/psn.lst') {
            copy($directory.'/NM_run1/psn.lst',$lstfile);
            @files = ($lstfile);
        }elsif(-e $directory.'/NM_run1/psn-1.lst'){
            copy($directory.'/NM_run1/psn-1.lst',$lstfile);
            @files = ($lstfile);
        }elsif(-e $directory.'/NM_run1/nmtran_error.txt'){
            copy($directory.'/NM_run1/nmtran_error.txt',$errorfile);
            @files = ($errorfile);
        }elsif( (-e $directory.'/NM_run1/FMSG') and ( not -e $directory.'/NM_run1/FREPORT')) {
            copy($directory.'/NM_run1/FMSG',$errorfile);
            @files = ($errorfile);
        }else{
            copy($logfile,$errorfile);
            @files = ($logfile);
        }
    }else {
        #success
        my @copyfiles = <$directory/*.csv>;
        push(@copyfiles,$directory.'/version_and_option_info.txt') if (-e $directory.'/version_and_option_info.txt');
        foreach my $f (@copyfiles){
            copy ($f,'.'); #FIXME ok on windows?
        }

        #we have already checked existence of tool_results.csv and copied the csv up here
        if ($tool eq 'bootstrap'){
            @files = ($lstfile);
        }elsif ($tool eq 'vpc'){
            copy($directory.'/m1/vpc_simulation.1.lst','.');
            copy($directory.'/m1/vpc_simulation.1.npctab.dta','npctab.dta');
            @files = ('vpc_simulation.1.lst');
            my @tab = <$directory/vpctab*>;
            copy($tab[0],'.');
        } elsif ($tool eq 'nca') {
            copy($directory.'/m1/nca_simulation.1.lst', '.');
            copy($directory.'/nca_simulation.1.npctab.dta', 'npctab.dta');
            @files = ('nca_simulation.1.lst');
            open my $fh, '<', 'npctab.dta';
            <$fh>;
            my $row = <$fh>;
            $row =~ s/\s+//;
            my @columns = split /\s+/, $row;
            for my $e (@columns) {
                if ($e ne 'ID' and $e ne 'TIME' and $e ne 'MDV') {
                    $append_columns .= "$e,"
                }
            }
            chop $append_columns;   # Remove final ,
            close $fh;
        } elsif ($tool eq 'sse') {
            my $keep_tables;
            my $curdir = getcwd();
            chdir($directory);
            open my $fh, '<', "version_and_option_info.txt";
            while (<$fh>) {
                if (/^-append_columns=(.*)$/) {
                    if ($1 ne "") {
                        $append_columns = $1;
                    }
                } elsif (/^-keep_tables=(.*)$/) {
                    $keep_tables = $1;
                }
            }
            close $fh;
            chdir($curdir);
            chdir($directory . '/m1');
            if (not $keep_tables) {
                @files = <*.lst>;
                my @ssedata =  <mc-sim-*.dat>;
                chdir($curdir);
                foreach my $fl (@files) {
                    copy ($directory . '/m1/' . $fl, $fl);
                }
                foreach my $fl (@ssedata) {
                    copy ($directory . '/m1/' . $fl, $fl);
                }
            } else {
                _merge_simulated_tables(destination => $curdir);
                chdir($curdir);
                copy($directory . '/m1/mc-1.lst', '.');
                @files = ( "mc-1.lst" );
            }
        } else {
            #execute or successful other tool
            @files = ($lstfile);
        }

    }

    return (\@files, $sofilename, $errorstring, $append_columns);
}

sub _get_toolname
{
    # Figure out which tool was run in this directory
    #for now will return undef for most tools
    my %parm = validated_hash(\@_,
        directory => { isa => 'Str',optional=>0 },
    );

    my $directory = $parm{'directory'};

    my $tool;

    my $command = "";
    if (-e $directory . '/version_and_option_info.txt') {
        open(COM, $directory . '/version_and_option_info.txt') or return $tool;
        while (<COM>) {
            if (/^Actual values optional (\w+)/) {
                $command = lc($1);
                close(COM);
                last;
            }
        }
        close(COM);
    }

    if ($command =~ /^bootstrap/) {
        $tool = 'bootstrap';
    } elsif ($command =~ /^vpc/) {
        $tool = 'vpc';
    } elsif ($command =~ /^nca/) {
        $tool = 'nca';
    } elsif ($command =~ /^sse/) {
        $tool = 'sse';
    } elsif ($command =~ /^execute/) {
        $tool = 'execute';
    }

    return $tool; #can be undef
}

sub _merge_simulated_tables
{
    my %parm = validated_hash(\@_,
        destination => { isa => 'Str' },
    );
    my $destination = $parm{'destination'};

    my @tables = glob "mc-sim-*";
    my $numsamples = scalar(@tables);

    open my $dest_cotab, '>', "$destination/cotab-sim-1";
    open my $dest_sdtab, '>', "$destination/sdtab-sim-1";
    open my $dest_patab, '>', "$destination/patab-sim-1";
    open my $dest_mc, '>', "$destination/mc-sim-1.dat";

    for (my $i = 1; $i <= $numsamples; $i++) {
        open my $cotab, '<', "cotab-sim-$i";
        while (my $line = <$cotab>) {
            print $dest_cotab $line;
        }
        close $cotab;

        open my $sdtab, '<', "sdtab-sim-$i";
        while (my $line = <$sdtab>) {
            print $dest_sdtab $line;
        }
        close $sdtab;

        open my $patab, '<', "patab-sim-$i";
        while (my $line = <$patab>) {
            print $dest_patab $line;
        }
        close $patab;

        open my $mc, '<', "mc-sim-$i.dat";
        while (my $line = <$mc>) {
            print $dest_mc $line;
        }
        close $mc;
    }

    close $dest_mc;
    close $dest_patab;
    close $dest_sdtab;
    close $dest_cotab;
}

1;
