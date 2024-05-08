#!/etc/bin/perl

use strict;
use warnings;
use File::Path 'rmtree';
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages and $path variable definition
use data;
#black box testing of rawres_input functionality

our $tempdir = create_test_dir('system_data_parsing');

our $modeldir = ''; #local
copy_test_files($tempdir,['data/test_99_separators_99.dta','data/test_99_separators_dot.dta',
						  'data/test_dot_separators_99.dta','data/test_dot_separators_dot.dta',
						  'data/test_leading_separators.dta',
						  'data/run_99_separators_99.mod','data/run_99_separators_dot.mod',
						  'data/run_dot_separators_99.mod','data/run_dot_separators_dot.mod',
						  'data/run_leading_separators.mod']);
chdir($tempdir);


my @sep = ('<none>','tab','space','comma');
my @edge_text=('99','dot');

my $count=117;
for (my $si=0; $si<scalar(@edge_text); $si++){
	for (my $ei=0; $ei<scalar(@edge_text); $ei++){
		my $base = '_'.$edge_text[$si].'_separators_'.$edge_text[$ei];
		my $model = 'run'.$base.'.mod'; #to generate nonmemtable
		my $input = 'test'.$base.'.dta';
		my $nonmemtable = 'answer'.$base.'.dta'; #basic space separated, 

		my $command = get_command('execute')." $model -model_dir_name -clean=3 -nm_output=cov";
		print "Running $command\n";
		my $rc = system($command);
		$rc = $rc >> 8;
		ok ($rc == 0, "$command, should run ok");
		ok ((-e $nonmemtable),"$nonmemtable exists");
		next unless (-e $nonmemtable);
		#NONMEMs interpretation of TEST SEP1 SEP2 SEP3 SEP4 LIM1 LIM2 C0
		#where C0 is the count of empty values inserted
		#SEP1-4 are indexes in the @separators array, i.e. 0 is none, 1 is tab, etc
		open( RRES, $modeldir.$nonmemtable) or die "could not open $modeldir$nonmemtable";
		my @original;
		while  (<RRES>){
			chomp;
			my @val = split;
			die "bug in test, length is ".scalar(@val) unless (scalar(@val)==8);
			push(@original,\@val);
		}
		is(scalar(@original),$count,"rows answer $nonmemtable");
		close( RRES );
		my $data = data->new( 
			idcolumn             => 6,
			filename             => $input,
			directory            => $modeldir,
			ignoresign => '@',
			ignore_missing_files => 0);
		is(scalar(@{$data->individuals()}),$count,"individuals PsN-parsed $input");
		my $psncol = $data->column_to_array(column => 18);

		for (my $index=0; $index< $count; $index++){
			my $i = $original[$index]->[1];
			my $j = $original[$index]->[2];
			my $k = $original[$index]->[3];
			my $l = $original[$index]->[4];
			my $testlabel = 'insert count edge-'.$edge_text[$si].' '.$sep[$i].' '.$sep[$j].' '.$sep[$k].' '.$sep[$l].' edge-'.$edge_text[$ei];
			cmp_ok($psncol->[$index],'==',$original[$index]->[7],$testlabel);
		}
	}


	#leading separators
	my $base = '_leading_separators';
	my $model = 'run'.$base.'.mod'; #to generate nonmemtable
	my $input = 'test'.$base.'.dta';
	my $nonmemtable = 'answer'.$base.'.dta'; #basic space separated, 

	my $command = get_command('execute')." $model -model_dir_name -clean=3 -nm_output=cov";
	print "Running $command\n";
	my $rc = system($command);
	$rc = $rc >> 8;
	ok ($rc == 0, "$command, should run ok");
	ok ((-e $nonmemtable),"$nonmemtable exists");
	next unless (-e $nonmemtable);

	#NONMEMs interpretation of 
	#$INPUT      C6 C5 C4 C3 C2 C1 DV SEP1 SEP2 SEP3 SEP4 SEP5 ID1 ID2 ID3 ID4 ID5
	#$TABLE ID5 SEP5 DV
	#where DV is the count of empty values inserted, SEP5 is the index string regardless of shift, 
	#and ID5 is the id number regardless of shift
	#SEP1-5 are combined single string indexes in the @separators array, e.g. 2210 , where 0 is none, 1 is tab, etc
	open( RRES, $modeldir.$nonmemtable) or die "could not open $modeldir$nonmemtable";
	my @original;
	while  (<RRES>){
		chomp;
		my @val = split;
		die "bug in test, length is ".scalar(@val) unless (scalar(@val)==3);
		#substr expression offset length
		push(@original,[$val[0],substr($val[1],0,1),substr($val[1],1,1),substr($val[1],2,1),substr($val[1],3,1),$val[2]]);
	}
	is(scalar(@original),$count,"rows answer $nonmemtable");
	close( RRES );
	#idcolumn count starts at 1
	my $data = data->new( 
		idcolumn             => 17, 
		filename             => $input,
		directory            => $modeldir,
		ignoresign => '@',
		ignore_missing_files => 0);
	is(scalar(@{$data->individuals()}),$count,"individuals PsN-parsed $input");
	my $psncol = $data->column_to_array(column => 6); #here indexing starts at 0
	for (my $index=0; $index< $count; $index++){
		my $i = $original[$index]->[1];
		my $j = $original[$index]->[2];
		my $k = $original[$index]->[3];
		my $l = $original[$index]->[4];
		my $testlabel = 'insert count leading '.$sep[$i].' '.$sep[$j].' '.$sep[$k].' '.$sep[$l];
		cmp_ok($psncol->[$index],'==',$original[$index]->[5],$testlabel);
	}

}


remove_test_dir($tempdir);

done_testing();
