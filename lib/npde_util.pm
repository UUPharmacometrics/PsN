package npde_util;

use FindBin qw($Bin);
use lib "$Bin/../lib";
use strict;
use include_modules;
use MooseX::Params::Validate;
use nmtablefile;
use array qw(any_nonzero);

our $missing=-99;
#	$ret = npde_util::read_table_files(\@found_files,\@eta_headers,$est_matrix,$mean_matrix,1);
# add id matrix, always return that
sub get_nmtablefiles{
	my %parm = validated_hash(\@_,
							  files => { isa => 'ArrayRef', optional => 0 },
		);
	my $files = $parm{'files'};

	my @nmtablefiles = ();
	foreach my $file (@{$files}){
		push(@nmtablefiles,nmtablefile->new(filename => $file));
	}
	return \@nmtablefiles;
}

sub get_columns_ids_samples{
	my %parm = validated_hash(\@_,
							  nmtablefiles => { isa => 'ArrayRef', optional => 0 },
							  header_strings => { isa => 'ArrayRef', optional => 0 },
							  values_matrix => { isa => 'ArrayRef', optional => 0 },
							  mean_matrix => { isa => 'ArrayRef', optional => 1 },
							  filter_all_zero => { isa => 'Bool', optional => 0 },
		);
	my $nmtablefiles = $parm{'nmtablefiles'};
	my $header_strings = $parm{'header_strings'};
	my $values_matrix = $parm{'values_matrix'};
	my $mean_matrix = $parm{'mean_matrix'};
	my $filter_all_zero = $parm{'filter_all_zero'};
	
	my $get_mean = 0;
	$get_mean=1 if (defined $mean_matrix);

	#assume first file is original

    my $input_error = 2;
    my $file_read_error = 1;

	my @column_indices=();
	return $input_error unless (scalar(@{$nmtablefiles})>0);
	return $input_error unless (scalar(@{$header_strings})>0);


	if ($filter_all_zero){
		my @filtered_headers =();
		foreach my $header (@{$header_strings}){
			if (any_nonzero($nmtablefiles->[0]->tables->[0]->get_column(name=> $header))){
				push(@filtered_headers,$header);
			}else{
				ui->print(category=>'ebe_npde',
						  message => "\nWarning: Removed ".$header.
						  " from npde calculation because no non-zero values found first table\n");

			}
		}
		$header_strings = \@filtered_headers;
	}

	my $first_table=1;
	my $sample_count=0;
	#FIXME empty files, empty tables
	foreach my $tablefile (@{$nmtablefiles}){
		foreach my $nmtable (@{$tablefile->tables}){
			if ($first_table){
				for (my $hi=0; $hi < scalar(@{$header_strings}); $hi++){
					my $col = $nmtable->get_column(name=> $header_strings->[$hi]);
					for (my $ind = 0; $ind < scalar(@{$col}); $ind++){
						$values_matrix->[$hi][$ind]=[$col->[$ind]];
						$mean_matrix->[$hi][$ind] = 0 if ($get_mean); #first table is original, do not count it
					}
				}
				$first_table = 0;
			}else{
				for (my $hi=0; $hi < scalar(@{$header_strings}); $hi++){
					my $col = $nmtable->get_column(name=> $header_strings->[$hi]);
					for (my $ind = 0; $ind < scalar(@{$col}); $ind++){
						push(@{$values_matrix->[$hi][$ind]},$col->[$ind]);
						$mean_matrix->[$hi][$ind] += ($col->[$ind]) if ($get_mean);
					}
				}
				$sample_count++;
			}
		}
	}
	if ($get_mean){
		return $file_read_error unless ($sample_count > 0);
		for (my $j=0;$j<scalar(@{$header_strings});$j++){
			for (my $i=0;$i<scalar(@{$mean_matrix->[$j]});$i++){
				#loop over individuals
				$mean_matrix->[$j][$i] = $mean_matrix->[$j][$i]/$sample_count;
			} 
		}
	}

	return 0;


}


sub read_table_files { 
	#in array of file names
	#in string array of headers to read, will be modified if all values in a column are 0 for all inds, cleaned
	#in reference to empty array to put results [over columns][over individuals][over samples/files]
	#in reference to empty array to put mean [over columns][over individuals], skipping original
	#in boolean 1 or 0 whether to get mean or not
	#out integer 0 if ok, 2 if input error, 1 if file read error 
	my $file_array = shift;
	my $header_strings = shift;
	my $estimate_matrix = shift;
	my $mean_matrix = shift;
	my $get_mean = shift;
#	my $expected_individuals=shift;
	# expected number of individuals

    my $input_error = 2;
    my $file_read_error = 1;

	my @column_indices=();
	return $input_error unless (scalar(@{$file_array})>0);
	return $input_error unless (scalar(@{$header_strings})>0);
#	return $input_error unless ($expected_individuals>0);

	my @found_nonzero = (0) x scalar(@{$header_strings});

	#cleaning, store header inidices but nothing else
	my $file = $file_array->[0];
	open( FILE, $file ) or croak("Could not open $file");
	my $id_index = 0;
	while (my $row = <FILE>){
		chomp $row;
		next if ($row =~ /TABLE NO/);
		$row =~ s/^\s*//;
		my @values=split(/\s+/,$row);
		if ($row =~ /^[A-Za-z]/){
			#found a header
			if (scalar(@column_indices)>0){
				print ("\nfound multiple headers in $file\n");
				return $file_read_error;
			}
			foreach my $hstring (@{$header_strings}){
				my $found = 0;
				for (my $j=0; $j< scalar(@values); $j++){
					if ($hstring eq $values[$j]){
						$found=1;
						push(@column_indices,$j);
						last;
					}
				}
				croak ("could not find header $hstring in $row") unless $found;
			}
			next;
		}
		#first file, check nonzero
		my $any_zero=0;
		for (my $j=0;$j<scalar(@column_indices);$j++){
			unless ($found_nonzero[$j]){
				if ($values[$column_indices[$j]] == 0){
					$any_zero = 1;
				}else{
					$found_nonzero[$j] =1;
				}
			}
		}
		last unless ($any_zero);
		$id_index++;
	}
	close(FILE);

	if ($get_mean){
		my @temp_header=();
		my @temp_indices=();
		for (my $j=0;$j<scalar(@column_indices);$j++){
			if ($found_nonzero[$j]){
				push(@temp_header,$header_strings->[$j]);
				push(@temp_indices,$column_indices[$j]);
			}else{
				print "\nWarning: Removed ".$header_strings->[$j].
					" from npde calculation because no non-zero values found in\n$file\n";
			}
		}
		$header_strings = \@temp_header;
		@column_indices = @temp_indices;
	}	

#	print "indices ". join(" ",@column_indices)."\n";
	my $not_first_file=0;
	my $sample_count=0;
	foreach my $file (@{$file_array}){
		unless (-e $file){
			print "\nWarning: Could not find file\n";
			next;
		}
		open( FILE, $file ) or croak("Could not open $file");
		my $id_index = 0;
		while (my $row = <FILE>){
			chomp $row;
			$row =~ s/^\s*//;
			next if ($row =~ /^[A-Za-z]/);
			next if ($row =~ /^$/);
			my @values=split(/\s+/,$row);
			if ($not_first_file){
				$sample_count++ if ($id_index == 0);
				#add to arrays
				for (my $j=0;$j<scalar(@column_indices);$j++){
					push(@{$estimate_matrix->[$j][$id_index]},$values[$column_indices[$j]]);
					$mean_matrix->[$j][$id_index] += $values[$column_indices[$j]];
				}
			}else{
				#first file, initiate arrays
				for (my $j=0;$j<scalar(@column_indices);$j++){
					$estimate_matrix->[$j][$id_index]=[$values[$column_indices[$j]]];
					$mean_matrix->[$j][$id_index] = 0; #do not count original in mean, original is first file
				}
			}
			$id_index++;
		}
		close(FILE);
		$not_first_file=1;
	}


	if ($get_mean){
		return $file_read_error unless ($sample_count > 0);
		for (my $j=0;$j<scalar(@column_indices);$j++){
			for (my $i=0;$i<scalar(@{$mean_matrix->[$j]});$i++){
				$mean_matrix->[$j][$i] = $mean_matrix->[$j][$i]/$sample_count;
			} 
		}
	}

	
	return 0;

}

sub decorrelation{
	#has unit tests
	my ($estimate_matrix,$mean_matrix,$decorrelated_estmatrix,$stdev_arr) = pos_validated_list(\@_,
																							   { isa => 'ArrayRef' },
																							   { isa => 'ArrayRef' },
																							   { isa => 'ArrayRef' },
																							   { isa => 'ArrayRef' },
		);
	#$estimate_matrix
	# reference to array  [over columns][over individuals][over samples/files]
	# $mean_matrix 
	#reference to mean array  [over columns][over individuals]

	# reference to empty array to put decorrelated results [over columns][over individuals][over samples]
	# $decorrelated_estmatrix 
	# $stdev_arr
	# $standardized
	#\frac{(iOFV_{observed} - mean (iOFV_{simulated}))^2}{variance(iOFV_{simulated})}

    my $input_error = 2;
    my $numerical_error = 1;

	my $nparm = scalar(@{$estimate_matrix});
	return $input_error unless ($nparm>0);
	my $individuals = scalar(@{$estimate_matrix->[0]});
	return $input_error unless ($individuals>0);
	my $samples = scalar(@{$estimate_matrix->[0]->[0]})-1; #-1 for original
	return $input_error unless ($samples > 1);

	if ($nparm == 1){
		for (my $i=0;$i<$individuals;$i++){
			my $mean=$mean_matrix->[0]->[$i];
			my $sum_errors_pow2=0;
			my $origval = $estimate_matrix->[0]->[$i]->[0];
			#loop over simulations, start at 1 since 0 is original
			for (my $k = 1; $k < scalar(@{$estimate_matrix->[0]->[$i]}); $k++){ 
				$sum_errors_pow2 += (($estimate_matrix->[0]->[$i]->[$k]) - $mean)**2;
			}
			my $stdev=0;
			unless( $sum_errors_pow2 <= 0 ){
				$stdev= sqrt ($sum_errors_pow2/($samples-1)); #root of variance
			}

			push(@{$stdev_arr},$stdev);

#			if (($stdev > 0) and (not ($origval==0))) {
			if ($stdev > 0) {
				my $original = ($origval-$mean)/$stdev;
				$decorrelated_estmatrix->[0]->[$i]=[$original];
				#loop over simulations, start at 1 since 0 is original
				for (my $k = 1; $k < scalar(@{$estimate_matrix->[0]->[$i]}); $k++){ 
					my $transf= (($estimate_matrix->[0]->[$i]->[$k]) -$mean)/$stdev;
					push (@{$decorrelated_estmatrix->[0]->[$i]},$transf);
				}
			}else{
				$decorrelated_estmatrix->[0]->[$i]=[$missing];
				#loop over simulations, start at 1 since 0 is original
				for (my $k = 1; $k < scalar(@{$estimate_matrix->[0]->[$i]}); $k++){ 
					push (@{$decorrelated_estmatrix->[0]->[$i]},$missing);
				}
			}
		}
		#return here when nparm==1???????
	}else{
	
		my $sqrt=sqrt($samples-1);
		for (my $i=0;$i<$individuals;$i++){
			my @Amat;
			my @original;
			my @meanvec;
			for (my $j=0;$j<$nparm;$j++){
				my $mean = $mean_matrix->[$j][$i];
				push (@meanvec,$mean);
				my $origval = $estimate_matrix->[$j]->[$i]->[0];
				push(@original,($origval-$mean));
				#loop over simulations, start at 1 since 0 is original
				for (my $k = 1; $k < scalar(@{$estimate_matrix->[$j]->[$i]}); $k++){ 
					push(@{$Amat[$j]},($estimate_matrix->[$j]->[$i]->[$k])-$mean);
				}
			}

			my $Rmat = [];
			my $numerr = linear_algebra::QR_factorize(\@Amat,$Rmat);
			#TODO handle numerr

			#want to multiply with inverse 'square root' (in a loose sense) of 
			#empirical variance-covariance matrix of A'A
			#i.e. we want to multiply with inv(R*diag(1/sqrt(N-1))
			#i.e. we want to solve orig=(R*diag(1/sqrt(N-1))*transf

			#Rmat->[$col][$row]

			#have already subtracted $mean from original

			my $ncol = scalar(@{$Rmat});

			#solve diag(1/sqrt(N-1))*bvec=yvec
			for (my $j=0;$j<$ncol;$j++){
				$original[$j]=$original[$j]*$sqrt;
			}
			
			#solve R'* transf=bvec
			#Golub p 88
			
			my $numerr = linear_algebra::upper_triangular_transpose_solve($Rmat,\@original);
			#TODO handle numerr
			
			for (my $j=0;$j<$nparm;$j++){
#				if ( ($estimate_matrix->[$j]->[$i]->[0]) == 0){
#					$decorrelated_estmatrix->[$j]->[$i]=[0];
#				}else{
					$decorrelated_estmatrix->[$j]->[$i]=[$original[$j]];
#				}
			}

			#transform estmatrix for each simulation of id $i
			
			for (my $k = 1; $k < ($samples+1); $k++){ 
				my @simvec;
				for (my $j=0;$j<$nparm;$j++){
					#must subtract mean here also
					push(@simvec,(($estimate_matrix->[$j]->[$i]->[$k]) - $meanvec[$j]));
				}
				
				#solve R'*x=simvec
				
				my $numerr = linear_algebra::upper_triangular_transpose_solve($Rmat,\@simvec);
				#TODO handle numerr
				
				#solve diag(1/sqrt(N-1))*transf=x
				for (my $j=0;$j<$ncol;$j++){
					$simvec[$j]=$simvec[$j]*$sqrt;
				}

				for (my $j=0;$j<$nparm;$j++){
#					if ( ($decorrelated_estmatrix->[$j]->[$i]->[0])==0){
#						push(@{$decorrelated_estmatrix->[$j][$i]},0);
#					}else{
						push(@{$decorrelated_estmatrix->[$j][$i]},$simvec[$j]);
#					}
				}
			}
			
		} #end loop over id
	}
	return 0;
	
}

sub pde
{
	#has unit tests
	my ($vector) = pos_validated_list(\@_,
									  { isa => 'ArrayRef' },
		);
	
	my $pde;
	my $original = $vector->[0];
	my $samples = scalar(@{$vector})-1; #-1 for original
	if ($original == $missing){
		$pde = $missing;
	}else{
		my $count=0;
		for (my $k = 1; $k <= $samples; $k++){ 
			$count++ if (($vector->[$k])< $original);
		}
		if ($count == 0){
			$pde = (1/$samples);
		}elsif ($count == $samples){
			$pde = 1-(1/$samples);
		}else{
			$pde =$count/$samples;
		}
	}

	return $pde;
}

sub npde_comp{
	#has unit tests
	#in matrix over params -> inds -> samples
	my ($decorrelated,$pde_matrix,$npde_matrix) = pos_validated_list(\@_,
									  { isa => 'ArrayRef' },
									  { isa => 'ArrayRef' },
									  { isa => 'ArrayRef' },
		);
	#decorrelated matrix, empty matrix, empty matrix

	my $have_CDF=0;
	$have_CDF=1 if eval('require Statistics::Distributions'); #enough, now loaded

	#TODO input untransformed and compute normalized pd in addition to normalized pde
    my $input_error = 2;
    my $numerical_error = 1;

	return $input_error unless (scalar(@{$decorrelated}) >0);
	return $input_error unless (scalar(@{$decorrelated->[0]})>0);
	return $input_error unless (scalar(@{$decorrelated->[0]->[0]}) > 2);


	for (my $j=0;$j<scalar(@{$decorrelated});$j++){
		push(@{$pde_matrix},[]);
	}
	for (my $i=0;$i<scalar(@{$decorrelated->[0]});$i++){
		for (my $j=0;$j<scalar(@{$decorrelated}) ;$j++){
			$pde_matrix->[$j]->[$i] = pde($decorrelated->[$j]->[$i]);
			if ($have_CDF and ($pde_matrix->[$j]->[$i] != $missing)){
				$npde_matrix->[$j]->[$i] = -(Statistics::Distributions::udistr($pde_matrix->[$j]->[$i]));
			}else{
				$npde_matrix->[$j]->[$i] =$missing;
			}
		}
	}
	return 0;

}


1;
