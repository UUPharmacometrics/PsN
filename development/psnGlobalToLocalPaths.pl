# before running this script make sure the $binpath variable points to the Perl/bin folder in
# the portable PKPD folder structure. the script should be run directly after install
# of a new PsN version with version number >= 4.1.6
# The PsN bin scripts must be in the Perl/bin folder of the PKPD env. and the PsN modules 
# must be installed under Perl/site/lib of the PKPD environment
# It is okay to run script multiple times on same files
# The PsN.pm modifications in setup.pl for the PKPD environment must NOT be made for
# PsN versions >= 4.1.6, for them
# setup should only change the psn.conf file and the perl PATH, nothing else regarding PsN.


use File::Glob;

my $binpath = 'Perl/bin/';
chdir($binpath);


my $psn_default="PsN_4_1_8";
my $dotversion = $psn_default;
$dotversion =~ s/^PsN_(\d+)_(\d+)_(\d+)/$1.$2.$3/;

my @versionscripts = <*-$dotversion.pl>;
my @defaultscripts=();
foreach my $ver (@versionscripts){
	my ($scr,$dirt) = split('-',$ver,2);
	push(@defaultscripts,$scr);
}


foreach my $script (@defaultscripts,@versionscripts){
	print "modifying $script\n";
	open( FILE, $script );
	my @lines = <FILE>;
	close (FILE);

	$lines[0] = '#!perl.exe'."\n";
	$lines[1] = 'use FindBin qw($Bin);'."\n";
 	$lines[2] = 'use lib '.'"$Bin/../site/lib/'.$psn_default.'";'."\n";
	$lines[3] = '# the above was entered by psnGlobalToLocalPaths'."\n";	

	open(FILE , '>'.$script);
	foreach my $line (@lines){
		print FILE $line;
	}
	close(FILE);

}

