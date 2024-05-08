#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use Config;

my %programs=(
boot_scm => ['methodA','samples'],
bootstrap => ['samples','stratify_on'],
cdd => ['xv','case_column'],
covmat => ['comma','header'],
crossval => ['groups'],
simeval => ['estimate_input'],
execute => ['tbs'],
extended_grid => ['id_column'],
frem => ['cholesky','check'],
gls => ['samples'],
lasso => ['relations'],
linearize => ['error'],
llp => ['omegas'],
mcmp => ['curve'],
mimp => ['samples'],	
nca => ['samples'],
nmoutput2so => ['verbose'],
nonpb => ['samples'],
npc => ['samples'],
npfit => ['npsupp'],
parallel_retries => ['timestamp'],
pind => ['njd'],
precond => ['nodec'],
psn => ['nm_versions'],
psn_clean => ['level'],
psn_options => ['tbs'],
pvar => ['parameters'],
randtest => ['samples'],
rawresults => ['path'],
runrecord => ['from'],
scm => ['linearize'],
sir => ['samples'],
sse => ['samples'],
sumo => ['ci'],
update => ['output_model'],
update_inits => ['output_model'],
vpc => ['samples'],
xv_scm => ['groups']
);

our $tempdir = create_test_dir('system_help');

chdir($tempdir);
foreach my $prog (sort {lc($a) cmp lc($b)} keys %programs){
	if (($prog eq 'nmoutput2so') and (not eval('require XML::LibXML'))){
		next;
	}

	my $command = get_command_without_args($prog).' -help > text.txt';
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	unless ($Config{osname} eq 'MSWin32'){
		my $string = readpipe('wc -l text.txt');
		if ($string =~ /^\s*(\d+)/){
			ok ($1 > 0, "help text is not empty");
		}
		system("grep 'No help available' text.txt");
		ok($? != 0,'No help text missing for '.$prog);
	}
	unlink('text.txt'); 

	$command = get_command_without_args($prog).' -h > text.txt';
	print "Running $command\n";
	$rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	unless ($Config{osname} eq 'MSWin32'){
		my $string = readpipe('wc -l text.txt');
		if ($string =~ /^\s*(\d+)/){
			ok ($1 > 0, "help text is not empty");
		}
	}
	unlink('text.txt'); 

	foreach my $opt (@{$programs{$prog}}){
		$command = get_command_without_args($prog).' -h '.$opt.' > text.txt';
		print "Running $command\n";
		my $rc = system($command);
		$rc = $rc >> 8;
		ok ($rc == 0, "$command, should run ok");
		unless ($Config{osname} eq 'MSWin32'){
			my $string = readpipe('wc -l text.txt');
			if ($string =~ /^\s*(\d+)/){
				ok ($1 > 0, "help text is not empty");
			}
		}
		unlink('text.txt'); 
	}
}

remove_test_dir($tempdir);


done_testing();
