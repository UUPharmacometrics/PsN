#!/etc/bin/perl


use strict;
use warnings;
use Test::More tests=>5;
use Test::Deep;
use Test::Exception;
use File::Path 'rmtree';
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

our $tempdir = create_test_dir('unit_covmat');
our $outfile = "cov.csv"; 
our $file_dir = $includes::testfiledir;


sub get_cov
{
	open( STAT, '<'."$outfile" );
	my @lines;
	for ( <STAT> ) {
		my @tmp = split(/,/,$_);
		push(@lines,\@tmp);
	}
	close( STAT );
	return \@lines;
}

my $command = get_command('covmat') . " -rawres_input=$file_dir/rawres_for_get_rawres_params.csv -raw_results_structure=$file_dir/rawres_for_get_rawres_params_structure -no-header -no-require -comma > $outfile";
chdir($tempdir);
print "Running $command\n";
my $rc = system($command);
$rc = $rc >> 8;
ok ($rc == 0, "$command, should run ok");
my $ref = get_cov();

#foreach my $line (@{$ref}){
#	print join(' ',@{$line})."\n";
#}
cmp_deeply($ref->[0]->[0], fnum(1.46167231816964),"cov 1,1");
cmp_ok($ref->[0]->[1],'==',$ref->[1]->[0],"cov 1,2 2,1");
cmp_ok($ref->[2]->[3],'==',$ref->[3]->[2],"cov 3,4 4,3");
cmp_ok($ref->[3]->[4],'==',1.43639675,"cov 4,5");

#TODO add some more crash tests here


remove_test_dir($tempdir);

done_testing();
