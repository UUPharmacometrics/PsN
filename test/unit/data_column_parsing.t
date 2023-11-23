#!/usr/bin/perl

# Unit tests for data.pm column parsing

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use data;

my $modeldir = $includes::testfiledir.'/data/';


my @separators=('',"\t",' ',',');
my @sep = ('<none>','tab','space','comma');
my @edge_values=('99','.');

my @edge_text=('99','dot');

my $count=117;
for (my $si=0; $si<scalar(@edge_text); $si++){
	for (my $ei=0; $ei<scalar(@edge_text); $ei++){
		my $filename = '_'.$edge_text[$si].'_separators_'.$edge_text[$ei].'.dta';
		my $input = 'test'.$filename;
		my $nonmemtable = 'answer'.$filename; #basic space separated, 
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
	my $filename = '_leading_separators.dta';
	my $input = 'test'.$filename;
	my $nonmemtable = 'answer'.$filename; #basic space separated, 
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


done_testing;

exit;
# This was the input data generating code
my $filename = 'original.dat';
open( RRES, $filename) or die "could not open $filename";
my @original;
while  (<RRES>){
	chomp;
	push(@original,$_);
}
close( RRES );

my $tail = ",6,5,4,3,2,1,0\n";
for (my $si=0; $si<scalar(@edge_values); $si++){
	for (my $ei=0; $ei<scalar(@edge_values); $ei++){
		my $filename = 'test_';
		if ($edge_values[$si] eq '.'){
			$filename .= 'dot';
		}else{
			$filename .= $edge_values[$si];
		}
		$filename .= '_separators_';
		if ($edge_values[$ei] eq '.'){
			$filename .= 'dot';
		}else{
			$filename .= $edge_values[$ei];
		}
		$filename .= '.dta';
		unlink ($filename);

		open my $fh, '>', $filename;

		my $oi=0; # original index
		#two separators
		my $k=0;
		my $l=0;
		for (my $i=1; $i<scalar(@separators); $i++){
			for (my $j=1; $j<scalar(@separators); $j++){
				print $fh $original[$oi].','.($oi+1).",$i,$j,$k,$l,".
					$edge_values[$si].
					$separators[$i].$separators[$j].
					$edge_values[$ei].$tail;
				$oi++;
			}
		}

		#three separators
		for (my $i=1; $i<scalar(@separators); $i++){
			for (my $j=1; $j<scalar(@separators); $j++){
				for ($k=1; $k<scalar(@separators); $k++){
					print $fh $original[$oi].','.($oi+1).",$i,$j,$k,$l,".
						$edge_values[$si].
						$separators[$i].$separators[$j].$separators[$k].
						$edge_values[$ei].$tail;
					$oi++;
				}
			}
		}


		#four separators
		for (my $i=1; $i<scalar(@separators); $i++){
			for (my $j=1; $j<scalar(@separators); $j++){
				for ($k=1; $k<scalar(@separators); $k++){
					for ($l=1; $l<scalar(@separators); $l++){
						print $fh $original[$oi].','.($oi+1).",$i,$j,$k,$l,".
							$edge_values[$si].
							$separators[$i].$separators[$j].$separators[$k].$separators[$l].
							$edge_values[$ei].$tail;
						$oi++;
					}
				}
			}
		}


		close($fh);
	}
}

exit;

#data generating code for leading separators
$tail = "6,5,4,3,2,1,0,";

$filename = 'test_leading_separators.dta';
unlink ($filename);
open my $fh, '>', $filename;

#two separators
my $counter=1;
my $k=0;
my $l=0;
for (my $i=1; $i<scalar(@separators); $i++){
	for (my $j=1; $j<scalar(@separators); $j++){
		print $fh $separators[$i].$separators[$j].
			$tail."$i$j$k$l,$i$j$k$l,$i$j$k$l,$i$j$k$l,$i$j$k$l,$counter,$counter,$counter,$counter,$counter\n";
		$counter++;
	}
}

#three separators
for (my $i=1; $i<scalar(@separators); $i++){
	for (my $j=1; $j<scalar(@separators); $j++){
		for ($k=1; $k<scalar(@separators); $k++){
			print $fh $separators[$i].$separators[$j].$separators[$k].
				$tail."$i$j$k$l,$i$j$k$l,$i$j$k$l,$i$j$k$l,$i$j$k$l,$counter,$counter,$counter,$counter,$counter\n";
			$counter++;
		}
	}
}


#four separators
for (my $i=1; $i<scalar(@separators); $i++){
	for (my $j=1; $j<scalar(@separators); $j++){
		for ($k=1; $k<scalar(@separators); $k++){
			for ($l=1; $l<scalar(@separators); $l++){
				print $fh $separators[$i].$separators[$j].$separators[$k].$separators[$l].
					$tail."$i$j$k$l,$i$j$k$l,$i$j$k$l,$i$j$k$l,$i$j$k$l,$counter,$counter,$counter,$counter,$counter\n";
				$counter++;
			}
		}
	}
}


close($fh);
exit;
