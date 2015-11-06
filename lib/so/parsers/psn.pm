package so::parsers::psn;

# Package for handling psn input to nmoutput2so.

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use File::Copy qw/cp mv/;
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
		 (defined $tool and ($tool ne 'execute') and (not -e $directory.'/'.$tool.'_results.csv'))
		){
		#generic failure
        #$errorstring = 'run failure';
        cp($logfile,$errorfile);  #skip copying, change design
        @files = ($logfile); #treat error messages as lstfile
	}elsif (($tool eq 'execute') and (not -e $lstfile)) {
		#execute failure
		if (-e $directory.'/NM_run1/psn.lst') {
			cp($directory.'/NM_run1/psn.lst',$lstfile);
			@files = ($lstfile);
		}elsif(-e $directory.'/NM_run1/psn-1.lst'){
			cp($directory.'/NM_run1/psn-1.lst',$lstfile);
			@files = ($lstfile);
		}elsif(-e $directory.'/NM_run1/nmtran_error.txt'){
			cp($directory.'/NM_run1/nmtran_error.txt',$errorfile);
			@files = ($errorfile);
		}elsif( (-e $directory.'/NM_run1/FMSG') and ( not -e $directory.'/NM_run1/FREPORT')) {
			cp($directory.'/NM_run1/FMSG',$errorfile);
			@files = ($errorfile);
		}else{
			cp($logfile,$errorfile); 
			@files = ($logfile); 
		}
    }else {
		#success
        my @copyfiles = <$directory/*.csv>;
        push(@copyfiles,$directory.'/version_and_option_info.txt') if (-e $directory.'/version_and_option_info.txt');
        foreach my $f (@copyfiles){
            cp ($f,'.'); #FIXME ok on windows?
        }

        #we have already checked existence of tool_results.csv and copied the csv up here
        if ($tool eq 'bootstrap'){
            @files = ($lstfile);
        }elsif ($tool eq 'vpc'){
            cp($directory.'/m1/vpc_simulation.1.lst','.');
            cp($directory.'/m1/vpc_simulation.1.npctab.dta','npctab.dta'); 
            @files = ('vpc_simulation.1.lst');
            my @tab = <$directory/vpctab*>;
            cp($tab[0],'.');
        } elsif ($tool eq 'sse') {
            my $curdir = getcwd();
            chdir($directory);
            open my $fh, '<', "version_and_option_info.txt";
            while (<$fh>) {
                if (/^-append_columns=(.*)$/) {
                    if ($1 ne "") {
                        $append_columns = $1;
                        last;
                    }
                }
            }
            close $fh;
            chdir($curdir);
            chdir($directory.'/m1');
            @files = <*.lst>;
            my @ssedata =  <mc-sim-*.dat>; 
            chdir($curdir);
            foreach my $fl (@files){
                cp ($directory.'/m1/'.$fl,$fl); 
            }
            foreach my $fl (@ssedata){
                cp ($directory.'/m1/'.$fl,$fl); 
            }
        }else{
            #execute or successful other tool
            @files = ($lstfile);
        }

    }

    return (\@files, $sofilename, $errorstring, $append_columns);
}

sub _get_toolname
{
    # Figure out which tool was run in this directory
    #for now will return undef for all but bootstrap,vpc or sse
    my %parm = validated_hash(\@_,
        directory => { isa => 'Str',optional=>0 },
    );

    my $directory = $parm{'directory'};

    my $tool;

	my $command;
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
	} elsif ($command =~ /^sse/) {
		$tool = 'sse';
	} elsif ($command =~ /^execute/) {
		$tool = 'execute';
	}

    return $tool; #can be undef
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
