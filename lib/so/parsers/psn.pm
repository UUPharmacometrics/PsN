package so::parsers::psn;

# Package for handling psn input to nmoutput2so.

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use File::Copy qw/cp mv/;
use Cwd;

sub BUILD
{
}

#<#assign outputFile = (job.executionFileName)?replace("\\.[A-Za-z0-9]{1,3}$", ".lst", "r")>
#<#assign psnlogFile = (job.executionFileName)?replace("\\.[A-Za-z0-9]{1,3}$", ".psn.log", "r")>


sub _get_stem{
    my %parm = validated_hash(\@_,
							  pharmml => { isa => 'Str', optional => 0 },
		);
	
	my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

	my $stem = $pharmml;
	$stem =~ s/\.[A-Za-z0-9]{1,3}$//;

	return $stem;
}

sub _get_logfile{
    my %parm = validated_hash(\@_,
							  pharmml => { isa => 'Str', optional => 0 },
		);
	
	my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

	my $stem = _get_stem(pharmml => $pharmml);
	return $stem.'.psn.log';
}
sub _get_lstfile{
    my %parm = validated_hash(\@_,
							  pharmml => { isa => 'Str', optional => 0 },
		);
	
	my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

	my $stem = _get_stem(pharmml => $pharmml);
	return $stem.'.lst';
}

sub _get_sofile{
    my %parm = validated_hash(\@_,
							  pharmml => { isa => 'Str', optional => 0 },
		);
	
	my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

	my $stem = _get_stem(pharmml => $pharmml);
	return $stem.'.SO.xml';
}

sub _connector_get_files{
    my %parm = validated_hash(\@_,
							  directory => { isa => 'Str', optional => 0 },
							  pharmml => { isa => 'Str', optional => 0 },
		);
	
	my $directory = $parm{'directory'};
	my $pharmml = $parm{'pharmml'}; #could also be .mod or .ctl if mdl-trick

	my $logfile = _get_logfile(pharmml => $pharmml);
	my $lstfile = _get_lstfile(pharmml => $pharmml);
	

    my @files=();
    my $bootstrap_results;
    my $tool= _get_toolname(directory => $directory); #can be undef

    if ( (not (-d $directory))  or 
		 ( (not defined $tool) and (not -e $lstfile)) or
		 (defined $tool and (not -e $directory.'/'.$tool.'_results.csv'))
		){
		#$options{'message'} = 'run failure';
		#cp($logfile,'errorMessages');  #skip copying, change design
		@files = ($logfile); #treat error messages as lstfile
    }else {
		my @copyfiles = <$directory/*.csv>;
		push(@copyfiles,$directory.'/version_and_option_info.txt') if (-e $directory.'/version_and_option_info.txt');
		foreach my $f (@copyfiles){
			cp ($f,'.'); #FIXME ok on windows?
		}

		#we have already checked existence of tool_results.csv and copied the csv up here
		if ($tool eq 'bootstrap'){
			$bootstrap_results = 'bootstrap_results.csv';
			@files = ($lstfile);
		}elsif ($tool eq 'vpc'){
			cp($directory.'/m1/vpc_simulation.1.lst','.');
			cp($directory.'/m1/vpc_simulation.1.npctab.dta','npctab.dta'); 
			@files = ('vpc_simulation.1.lst');
			my @tab = <$directory/vpctab*>;
			cp($tab[0],'.');
		}elsif ($tool eq 'sse'){
			my $curdir=getcwd();
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

    return (\@files,$bootstrap_results);
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

    if ( (-e $directory.'/bootstrap_results.csv') or
		 (-e $directory.'/sample_keys1.csv') or
		 (-e $directory.'/included_keys1.csv') or
		 (-e $directory.'/bootstraplog.csv') 
		){
		$tool = 'bootstrap';
    }elsif ((-e $directory.'/vpc_results.csv') or
			(-e $directory.'/vpc_bins.txt') or
			(-e $directory.'/m1/vpc_original.mod') or
			(-e $directory.'/m1/vpc_simulation.1.mod') 
		){
		$tool = 'vpc';
    }elsif ((-e $directory.'/sse_results.csv') or
			(-e $directory.'/simulation_initial_values') or
			(-e $directory.'/m1/mc-1.sim')
		){
		$tool = 'sse';
    }

    return $tool; #can be undef

}
no Moose;
__PACKAGE__->meta->make_immutable;
1;
